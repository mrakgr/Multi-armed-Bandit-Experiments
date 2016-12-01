#load "SpiralV5CudaCodeGen.fsx"

open SpiralV5

open ManagedCuda
open ManagedCuda.BasicTypes
open ManagedCuda.VectorTypes
open ManagedCuda.CudaBlas
open ManagedCuda.CudaRand
open ManagedCuda.NVRTC
open ManagedCuda.CudaDNN

open System
open System.Diagnostics
open System.IO
open System.Collections.Generic
open System.Runtime.InteropServices

type unit_to_unit_delegate = delegate of unit -> unit
let add_callback_to_stream (str : CudaStream) (callback : unit -> unit) =
    let callb (str : CUstream) (res : CUResult) (p : nativeint) =
        let t : unit_to_unit_delegate = Runtime.InteropServices.Marshal.GetDelegateForFunctionPointer(p)
        t.Invoke()

    let aux = new unit_to_unit_delegate (callback)
    let ptr_to_aux = Marshal.GetFunctionPointerForDelegate aux

    let cuda_callback = CUstreamCallback(callb)
    str.AddCallback(cuda_callback,ptr_to_aux,CUStreamAddCallbackFlags.None)

//     Some formal rules for memory allocation
//    - On first allocation the memory is initialized to zero
//    - On the forward pass the primals of the objects in the ObjectPool are set to zero inside the functions during usage
//    - On the forward pass the adjoints of the objects in the ObjectPool are set to zero inside the ObjectPool
//    - During the optimization phase, the optimizers are responsible for setting the adjoints of the weight nodes to zero.

// Helper functions
let inline dispose (v: #IDisposable) = v.Dispose()

/// Copies a host array to device.
let inline to_dev (host_ar: 't []) =
    let d_a = new CudaDeviceVariable<'t>(SizeT host_ar.Length)    
    d_a.CopyToDevice(host_ar)
    d_a

/// Copies a device array to host.
let inline to_host (dev_ar: CudaDeviceVariable<'t>) =
    let h_a = Array.zeroCreate<'t> (int dev_ar.Size)
    dev_ar.CopyToHost(h_a)
    cuda_context.Synchronize()
    h_a

/// Copies the device array to host. Extends the CudaDeviceVariable class.
type CudaDeviceVariable<'t when 't: struct and 't: (new: unit -> 't) and 't:> System.ValueType> with
    member inline this.Gather() =
        to_host this

/// The float scalar type
type Df = 
    {
    P : Lazy<float32> ref // primal
    A : float32 ref // adjoint
    }

    /// Shorthand for t.P.Value.Value
    member t.PrimalValue = t.P.Value.Value

    static member inline create P =
        {P=ref (lazy P);A=ref 0.0f}

type DeadFlagType =
| Undefined
| Dead
| Alive

type AutoDiffType =
    | PrimalOnly of CudaDeviceVariable<float32> // Does no AD
    | PrimalAndAdjoint of CudaDeviceVariable<float32> * CudaDeviceVariable<float32> // Does first order AD

    static member CreatePrimalOnly(size: int) =
        new CudaDeviceVariable<float32>(SizeT size)
        |> fun x -> x.Memset(0u); x // Zeroes the variable (saves me from having to zero out the adjoint on the first run)
        |> PrimalOnly

    static member CreatePA(size: int) =
        // Zeroes the variables (saves me from having to zero out the adjoint on the first run)
        let a = new CudaDeviceVariable<float32>(SizeT size) |> fun x -> x.Memset(0u); x
        let b = new CudaDeviceVariable<float32>(SizeT size) |> fun x -> x.Memset(0u); x
        PrimalAndAdjoint(a,b)

    static member private resize (size: int) (v: CudaDeviceVariable<float32>) = 
        if size <= int v.Size then v
        else
            v.Dispose()
            new CudaDeviceVariable<float32>(SizeT size)

    member t.ResizePrimalOnly(size: int) =
        match t with
        | PrimalOnly v -> PrimalOnly(AutoDiffType.resize size v)
        | PrimalAndAdjoint(a,b) -> PrimalAndAdjoint(AutoDiffType.resize size a, AutoDiffType.resize size b) // Does not resize downwards.

    member t.ResizePrimalAndAdjoint(size: int) =
        match t with
        | PrimalOnly v -> PrimalAndAdjoint(AutoDiffType.resize size v, new CudaDeviceVariable<float32>(SizeT size))
        | PrimalAndAdjoint(a,b) -> PrimalAndAdjoint(AutoDiffType.resize size a, AutoDiffType.resize size b)

    member t.Resize(size: int) =
        match t with
        | PrimalOnly v -> PrimalOnly(AutoDiffType.resize size v)
        | PrimalAndAdjoint(a,b) -> PrimalAndAdjoint(AutoDiffType.resize size a, AutoDiffType.resize size b)
        
    member t.P =
        match t with
        | PrimalOnly v -> v
        | PrimalAndAdjoint(a,b) -> a

    member t.A =
        match t with
        | PrimalOnly v -> failwith "No adjoint!"
        | PrimalAndAdjoint(a,b) -> b

    member t.PA = t.P, t.A

    member t.CopyToPrimal(host_ar: float32[]) =
        let x = t.P
        if int x.Size <> host_ar.Length then failwithf "int x.Size(%i) <> host_ar.Length(%i)" (int x.Size) (host_ar.Length)
        x.CopyToDevice(host_ar)

    member t.HasAdjoint =
        match t with
        | PrimalOnly v -> false
        | _ -> true

    interface IDisposable with
        member t.Dispose() =
            match t with
            | PrimalOnly v -> v.Dispose()
            | PrimalAndAdjoint(a,b) -> a.Dispose(); b.Dispose()

let defaultLayout = cudnnTensorFormat.NCHW
let defaultType = cudnnDataType.Float
let defaultMaxPoolingNanOption = cudnnNanPropagation.PropagateNan
let defaultReluNanOption = cudnnNanPropagation.PropagateNan

type TensorDescriptor with
    /// Extended method that works according to the bound defaultLayout and defaultType variables.
    member inline t.SetTensor4dDescriptor(n,c,h,w) = t.SetTensor4dDescriptor(defaultLayout,defaultType,n,c,h,w)

type FilterDescriptor with
    /// Extended method that works according to the bound defaultType variable.
    member inline t.SetFilter4dDescriptor(n,c,h,w) = t.SetFilter4dDescriptor(defaultType,defaultLayout,n,c,h,w)

type ConvolutionParameters = {
    pad_h : int
    pad_w : int
    stride_h : int
    stride_w : int
    upscale_h : int
    upscale_w : int
    mode : cudnnConvolutionMode
    }

type PoolingParameters =
    {
    mode : cudnnPoolingMode
    windowHeight : int
    windowWidth : int
    verticalPadding : int
    horizontalPadding : int
    verticalStride : int
    horizontalStride : int
    }

type PoolingDescriptor with
    member inline t.SetPooling2dDescriptor (p : PoolingParameters) =
        t.SetPooling2dDescriptor(p.mode,defaultMaxPoolingNanOption,p.windowHeight,p.windowWidth,p.verticalPadding,p.horizontalPadding,p.verticalStride,p.horizontalStride)

    member inline t.GetPooling2dForwardOutputDim s =
        let mutable n,c,h,w = 0,0,0,0
        t.GetPooling2dForwardOutputDim(s,&n,&c,&h,&w)
        n,c,h,w

let defaultConvPar = 
    {
    pad_h = 0
    pad_w = 0
    stride_h = 1
    stride_w = 1
    upscale_h = 1
    upscale_w = 1
    mode = cudnnConvolutionMode.Convolution
    }

type ConvolutionDescriptor with
    member inline t.SetConvolution2dDescriptor (p : ConvolutionParameters) =
        t.SetConvolution2dDescriptor(p.pad_h,p.pad_w,p.stride_h,p.stride_w,p.upscale_h,p.upscale_w,p.mode, defaultType)
    member inline t.GetConvolution2dForwardOutputDim (s,f) =
        let mutable n,c,h,w = 0,0,0,0
        t.GetConvolution2dForwardOutputDim(s,f,&n,&c,&h,&w)
        n,c,h,w

type Workspace() = 
    let mutable workspace: CudaDeviceVariable<byte> = CudaDeviceVariable.Null

    /// Resizes the workspace if it is less than size and returns it. The size is in 'a.
    member t.ResizeIf<'a when 'a: (new: unit -> 'a) and 'a: struct and 'a :> ValueType>(size: int) =
        let toGeneric(workspace: CudaDeviceVariable<byte>) = new CudaDeviceVariable<'a>(workspace.DevicePointer,false,SizeT (size * sizeof<'a>))
        if size < int workspace.Size then toGeneric workspace
        else
            workspace.Dispose()
            workspace <- new CudaDeviceVariable<byte>(SizeT (size * sizeof<'a>))
            toGeneric workspace

    /// Resizes the workspace if it is less than size and returns it as a d2M.
    member t.ResizeIfd2M((r,c as rc): int*int) =
        let x = t.ResizeIf(r*c)
        {
        rc = rc
        diff = PrimalOnly x
        is_dead = Undefined
        }

    interface IDisposable with
        member t.Dispose() = workspace.Dispose()

/// The wrapper type for the d2M and d4M. It is really unbelievable how much it simplified the code for the combinators.
/// Discriminated unions are bad for storing data, but they are excellent for making code generic by emulating calling by name
/// and also for doing access control. This type is for the later.
and DM =
    | D2M of d2M
    | D4M of d4M

    interface IDisposable with
        member t.Dispose() =
            match t with
            | D2M(x) -> x |> dispose
            | D4M(x) -> x |> dispose

and d2M =
    {
    mutable rc : int * int
    mutable diff: AutoDiffType
    mutable is_dead : DeadFlagType // flag to skip backprop
    }

    static member private size_rc (row,col) = row*col

    /// Add the rows and column of the 2d matrix.
    member t.AddDims = t.rc |> fun (r,c) -> r+c
    
    static member create' (size : (int * int), is_constant) =
        let diff = 
            let s = d2M.size_rc size
            match is_constant with
            | true -> AutoDiffType.CreatePrimalOnly s
            | false -> AutoDiffType.CreatePA s
        {rc = size; diff=diff; is_dead=Undefined}

    static member create' (size : (int * int), host_data : float32[], is_constant) =
        let t = d2M.create' (size, is_constant)
        t.diff.CopyToPrimal(host_data)
        t

    // Constructors for the singular d2M records.
    static member inline create (ar : int * int) = d2M.create'(ar, false)
    static member inline create (row : int, col : int) = d2M.create'((row, col), false)
    static member inline create (row : int, col : int, ar_data : float32[]) = d2M.create'((row,col),ar_data, false)
    static member inline create (size : int * int, ar_data : float32[]) = d2M.create'(size,ar_data, false)
    static member inline createConstant (size : int * int) = d2M.create'(size, true)
    static member inline createConstant (row : int, col : int, ar_data : float32[]) = d2M.create'((row,col),ar_data, true)
    static member inline createConstant (size : int * int, ar_data : float32[]) = d2M.create'(size,ar_data, true)

    /// Number of rows
    member t.Rows = t.rc |> fst
    /// Number of columns
    member t.Columns = t.rc |> snd  
    /// Returns whether the function has an adjoint
    member t.HasAdjoint = t.diff.HasAdjoint
  
    /// Returns the size of matrix
    member t.Size = d2M.size_rc t.rc

    /// Returns matrix of the same dimensions at the current one without copying the values.
    member t.GhostCopy is_constant = d2M.create'(t.rc,is_constant)

    /// Returns matrix of the same inner dimension as the current one without copying the values.
    member t.GhostCopyBias is_constant = d2M.create'((t.Rows,1),is_constant)

    /// Get Adjoint Pointer
    member t.GAP = t.diff.A.DevicePointer

    /// Get Primal Pointer
    member t.GPP = t.diff.P.DevicePointer

    /// Get Adjoint Variable
    member t.GAV = t.diff.A

    /// Get Primal Variable
    member t.GPV = t.diff.P

    /// Get row and column
    member t.RC = t.rc
    
    /// Get the deadness flag
    member t.IsDead = t.is_dead

    /// Update the deadness flag
    member t.DeadIs v = t.is_dead <- v

    /// CUDNN has a bug where it is ridicously slow if the dimensions are not set up right.
    /// So this function is to get nchw of the matrix for the tensor_add function.
    member t.NCHW =
        (t.Columns,1,t.Rows,1)
        // (t.Columns,t.Rows,1,1) is 10x slower than the above
        // There are other fast configurations, but it is unfortunate that I picked the
        // Wrong one for SpiralV3. Now that I know duck typing, writing generic code will be
        // much easier.

    /// Returns the nchw (for the buggy tensor_add function)
    /// The stupid cuDNN function will throw an exception if I put in the parameters for the fast version.
    member t.NCHWBiasAdd = (t.Columns,t.Rows,1,1)

    /// Throws an exception if the sizes are not all equal
    static member GuardSizes(x:d2M, o: d2M) =
        if x.rc <> o.rc then failwithf "x.rc(%A) <> o.rc(%A)" x.rc o.rc

    /// Throws an exception if the sizes are not all equal
    static member GuardSizes(x:d2M, y:d2M, o: d2M) =
        if x.rc <> y.rc then failwithf "x.rc(%A) <> y.rc(%A)" x.rc y.rc
        if y.rc <> o.rc then failwithf "y.rc(%A) <> o.rc(%A)" y.rc o.rc

    /// Throws an exception if the sizes are not all equal
    static member GuardSizes(x:d2M, y:d2M, z: d2M, o: d2M) =
        if x.rc <> y.rc then failwithf "x.rc(%A) <> y.rc(%A)" x.rc y.rc
        if y.rc <> z.rc then failwithf "y.rc(%A) <> z.rc(%A)" y.rc z.rc
        if z.rc <> o.rc then failwithf "z.rc(%A) <> o.rc(%A)" z.rc o.rc

    /// Resizes the object. Does not free memory when resizing downwards.
    member t.ReplaceIf (ar : int * int, is_constant) =
        t.rc <- ar
        let new_size = d2M.size_rc ar
        match is_constant with
        | true -> t.diff <- t.diff.ResizePrimalOnly new_size
        | false -> t.diff <- t.diff.ResizePrimalAndAdjoint new_size

    /// Gets an object the same size as self from the object pool
    member inline t.GetFromObjectPool(obj_pool: ObjectPool, is_constant, is_inference_only, str: CudaStream) =
        obj_pool.Getd2M(is_constant,t.rc,is_inference_only,str)

    /// Copies the object by using the memory from the object pool.
    member inline t.CopyUsingObjectPool(obj_pool: ObjectPool, is_constant, is_inference_only, str: CudaStream) =
        let x = obj_pool.Getd2M(is_constant,t.rc,is_inference_only,str)
        x.GPV.AsyncCopyToDevice(t.GPV,str.Stream)
        x

    /// Sets the adjoint to a value.
    member inline t.SetAdjoint(x: float32, str: CudaStream) = 
        let v = BitConverter.ToUInt32(BitConverter.GetBytes(x),0)
        t.diff.A.MemsetAsync(v,str.Stream)

    /// Set the matrix to a value.
    member inline t.SetPrimal (x: float32, str: CudaStream) = 
        let v = BitConverter.ToUInt32(BitConverter.GetBytes(x),0)
        t.diff.P.MemsetAsync(v,str.Stream)

    /// For temporary immediate use only.
    member t.ConvertdMLikeFromWorkspace(w: Workspace) =
        let v = w.ResizeIf<float32> t.Size
        {rc = t.rc; diff = PrimalOnly v; is_dead = Undefined}

    /// Cast to DM.
    member t.CastToDM = D2M t

    /// Returns the contents of the primal in a 1D array to host.
    member t.GatherPrimal =
        t.GPV.Gather()

    /// Returns the contents of the primal in a 2D array to host.
    member t.ToFloat32Array2D =
        let x = t.GatherPrimal
        Array2D.init t.Rows t.Columns <| fun r c -> x.[c*t.Rows+r]

    interface IDisposable with
        member t.Dispose() = t.diff |> dispose

and d4M =
    {
    mutable nchw : int * int * int * int
    mutable diff : AutoDiffType
    mutable is_dead : DeadFlagType // flag to skip backprop
    }

    /// Adds the image,channel, width and height dimensions of the 4d tensor.
    member t.AddDims = t.nchw |> fun (n,c,h,w) -> n+c+h+w

    static member private size_nchw (n:int,c,h,w) = n*c*h*w

    static member create' (size : (int * int * int * int), is_constant) =
        let diff = 
            let s = d4M.size_nchw size
            match is_constant with
            | true -> AutoDiffType.CreatePrimalOnly s
            | false -> AutoDiffType.CreatePA s
        {nchw = size; diff=diff ; is_dead=Undefined}

    static member create' (size : int * int * int * int, host_data : float32[], is_constant) =
        let t = d4M.create' (size, is_constant)
        t.diff.CopyToPrimal(host_data)
        t

    // Constructors for the singular d4M records.
    static member inline create (ar: int * int * int * int) = d4M.create'(ar, false)
    static member inline create (ar: (int * int * int * int), data: float32[]) = d4M.create'(ar, data, false)
    static member inline createConstant (ar : int * int * int * int) = d4M.create'(ar, true)
    static member inline createConstant (ar: (int * int * int * int), data: float32[]) = d4M.create'(ar, data, true)

    /// Number of rows (concatenates along the c,h,w dimensions)
    member t.Rows = t.nchw |> fun (_,c,h,w) -> c*h*w
    /// Number of columns (return the outer n dimension)
    member t.Columns = t.nchw |> fun (n,_,_,_) -> n
    /// Returns whether the function has an adjoint
    member t.HasAdjoint = t.diff.HasAdjoint

    /// Returns the size of matrix
    member t.Size = d4M.size_nchw t.nchw

    /// Returns matrix of the same dimensions at the current one without copying the values.
    member t.GhostCopy is_constant = d4M.create'(t.nchw,is_constant)
        
    /// Returns matrix of the same inner dimensions as the current one without copying the values.
    member t.GhostCopyBias is_constant = 
        t.nchw |> fun (_,c,h,w) ->
            d4M.create'((1,c,h,w),is_constant)

    /// Get Adjoint Pointer
    member t.GAP = t.diff.A.DevicePointer

    /// Get Primal Pointer
    member t.GPP = t.diff.P.DevicePointer

    /// Get Adjoint Variable
    member t.GAV = t.diff.A

    /// Get Primal Variable
    member t.GPV = t.diff.P

    /// Returns the tensor's dimensions projected 
    /// to a 2D space according to the following formula:
    /// row = c*h*w
    /// column = n
    member t.RC = t.Rows, t.Columns
    
    /// Get the deadness flag
    member t.IsDead = t.is_dead

    /// Update the deadness flag
    member t.DeadIs v = t.is_dead <- v

    /// Returns the nchw
    member t.NCHW = t.nchw

    /// Returns the nchw (for the buggy tensor_add function)
    member t.NCHWBiasAdd = t.nchw

    /// Throws an exception if the sizes are not all equal
    static member inline GuardSizes(x:d4M, o: d4M) =
        if x.nchw <> o.nchw then failwithf "x.rc(%A) <> o.rc(%A)" x.nchw o.nchw

    /// Throws an exception if the sizes are not all equal
    static member inline GuardSizes(x:d4M, y:d4M, o: d4M) =
        if x.nchw <> y.nchw then failwithf "x.rc(%A) <> y.rc(%A)" x.nchw y.nchw
        if y.nchw <> o.nchw then failwithf "y.rc(%A) <> o.rc(%A)" y.nchw o.nchw

    /// Throws an exception if the sizes are not all equal
    static member inline GuardSizes(x:d4M, y:d4M, z: d4M, o: d4M) =
        if x.nchw <> y.nchw then failwithf "x.rc(%A) <> y.rc(%A)" x.nchw y.nchw
        if y.nchw <> z.nchw then failwithf "y.rc(%A) <> z.rc(%A)" y.nchw z.nchw
        if z.nchw <> o.nchw then failwithf "z.rc(%A) <> o.rc(%A)" z.nchw o.nchw

    /// Resizes the object. Does not free memory when resizing downwards.
    member t.ReplaceIf (ar : (int * int * int * int), is_constant) =
        t.nchw <- ar
        let new_size = d4M.size_nchw ar
        match is_constant with
        | true -> t.diff <- t.diff.ResizePrimalOnly new_size
        | false -> t.diff <- t.diff.ResizePrimalAndAdjoint new_size

    /// Gets an object the same size as it from the object pool
    member inline t.GetFromObjectPool(obj_pool: ObjectPool, is_constant, is_inference_only, str: CudaStream) =
        obj_pool.Getd4M(is_constant,t.nchw,is_inference_only,str)

    /// Copies the object by using the memory from the object pool.
    member inline t.CopyUsingObjectPool(obj_pool: ObjectPool, is_constant, is_inference_only, str: CudaStream) =
        let x = obj_pool.Getd4M(is_constant,t.nchw,is_inference_only,str)
        x.GPV.AsyncCopyToDevice(t.GPV,str.Stream)
        x

    /// Sets the adjoint to a value.
    member inline t.SetAdjoint(x: float32, str: CudaStream) = 
        let v = BitConverter.ToUInt32(BitConverter.GetBytes(x),0)
        t.diff.A.MemsetAsync(v,str.Stream)

    /// Set the matrix to a value.
    member inline t.SetPrimal (x: float32, str: CudaStream) = 
        let v = BitConverter.ToUInt32(BitConverter.GetBytes(x),0)
        t.diff.P.MemsetAsync(v,str.Stream)

    /// For temporary immediate use only.
    member t.ConvertdMLikeFromWorkspace(w: Workspace) =
        let v = w.ResizeIf<float32> t.Size
        {nchw = t.nchw; diff = PrimalOnly v; is_dead = Undefined}

    /// Cast to DM.
    member t.CastToDM = D4M t

    interface IDisposable with
        member t.Dispose() = t.diff |> dispose

/// The new object pool. Zeroes out the adjoints concurrently on the forward phase.
and ObjectPool() =
    let d2MPool = ResizeArray()
    let d2Mp = ref 0
    let d4MPool = ResizeArray()
    let d4Mp = ref 0

    let tensorDescriptorPool = Dictionary(HashIdentity.Structural)
    let filterDescriptorPool = Dictionary(HashIdentity.Structural)
    let convolutionDescriptorPool = Dictionary(HashIdentity.Structural)
    let poolingDescriptorPool = Dictionary(HashIdentity.Structural)
    let activationDescriptorPool = Dictionary(HashIdentity.Structural)
    let BNDescriptorPool = Dictionary(HashIdentity.Structural)

    static member inline private GetFromPool (pool : ResizeArray<_>) (pointer_to_pool : int ref) (creation_function) =
        if pool.Count > !pointer_to_pool then
            let t = pool.[!pointer_to_pool]
            pointer_to_pool := !pointer_to_pool+1
            t
        else
            let t = creation_function()
            pool.Add(t)
            pointer_to_pool := !pointer_to_pool+1
            t

    static member inline private GetFromDict (pool : Dictionary<_,_>) k creation_function set_function =
        match pool.TryGetValue k with
        | true, v -> v
        | false, _ ->
            let t = creation_function()
            set_function t k
            pool.Add(k, t)
            t

    member t.Getd2M (is_constant, (rc : (int*int)), is_inference_only, str: CudaStream): d2M =
        let t' = ObjectPool.GetFromPool d2MPool d2Mp (fun _ -> d2M.create'(rc,is_constant))
        t'.ReplaceIf(rc,is_constant)
        t'.is_dead <- Undefined
        if is_inference_only = false && t'.HasAdjoint then t'.diff.A.MemsetAsync(0u,str.Stream)
        t'

    member t.Getd4M (is_constant, (nchw : (int*int*int*int)), is_inference_only, str: CudaStream): d4M =
        let t' = ObjectPool.GetFromPool d4MPool d4Mp (fun _ -> d4M.create'(nchw,is_constant))

        t'.ReplaceIf(nchw,is_constant)
        t'.is_dead <- Undefined
        if is_inference_only = false && t'.HasAdjoint then t'.diff.A.MemsetAsync(0u,str.Stream)
        t'

    member t.GetTensorDescriptor (nchw : int*int*int*int) = 
        ObjectPool.GetFromDict tensorDescriptorPool nchw (fun _ -> new TensorDescriptor()) (fun (t: TensorDescriptor) x -> x |> t.SetTensor4dDescriptor)
    member t.GetFilterDescriptor (nchw : int*int*int*int) = 
        ObjectPool.GetFromDict filterDescriptorPool nchw (fun _ -> new FilterDescriptor()) (fun (t: FilterDescriptor) x -> x |> t.SetFilter4dDescriptor)
    member t.GetConvolutionDescriptor (convPars : ConvolutionParameters) = 
        ObjectPool.GetFromDict convolutionDescriptorPool convPars (fun _ -> new ConvolutionDescriptor()) (fun (t: ConvolutionDescriptor) x -> x |> t.SetConvolution2dDescriptor)
    member t.GetPoolingDescriptor (p : PoolingParameters) = 
        ObjectPool.GetFromDict poolingDescriptorPool p (fun _ -> new PoolingDescriptor()) (fun (t: PoolingDescriptor) x -> x |> t.SetPooling2dDescriptor)
    member t.GetActivationDescriptor (mode : cudnnActivationMode, nanopt : cudnnNanPropagation, reluCeiling as p) = 
        ObjectPool.GetFromDict activationDescriptorPool p (fun _ -> new ActivationDescriptor()) (fun (t: ActivationDescriptor) x -> x |> t.SetActivationDescriptor)
    member t.GetBNDescriptor ((nchw : int*int*int*int, mode : cudnnBatchNormMode, srcDesc : TensorDescriptor) as p) = 
        ObjectPool.GetFromDict BNDescriptorPool p 
            (fun _ -> new TensorDescriptor()) 
            (fun (t: TensorDescriptor) (nchw, mode, srcDesc) -> cudnn.DeriveBNTensorDescriptor(t,srcDesc,mode))

    // Resets the pointer in the object pool
    member t.Reset() =
        d2Mp := 0
        d4Mp := 0

    interface IDisposable with
        member __.Dispose() =
            let inline dis' ex pool = 
                // The disposer helper. Uses an extractor for the dictionary values.
                // This succintly demonstrates how to get around the lack of higher kinded types in F#.
                let pool = ex pool
                for x in pool do dispose x
            let inline dis x = dis' id x
            dis d2MPool
            dis d4MPool

            let inline dis x = 
                dis' (fun v -> (^a: (member Values: ^b) v)) x
            dis tensorDescriptorPool // ...It would have been faster to just copy paste .Value everywhere.
            dis filterDescriptorPool
            dis convolutionDescriptorPool
            dis poolingDescriptorPool
            dis activationDescriptorPool
            dis BNDescriptorPool

let T = Operation.Transpose
let nT = Operation.NonTranspose

let inline extract_primal x = (^a : (member GPP: CUdeviceptr) x)
let inline extract_adjoint x = (^a : (member GAP: CUdeviceptr) x)
let inline extract_primal' x = (^a: (member GPV: CudaDeviceVariable<float32>) x)
let inline extract_adjoint' x = (^a: (member GAV: CudaDeviceVariable<float32>) x)
let inline rc x = (^a: (member RC: int * int) x)

let inline GuardSizes2(x,y) =
    (^a: (static member GuardSizes: ^a * ^a -> unit) (x,y))
let inline GuardSizes3(x,y,z) =
    (^a: (static member GuardSizes: ^a * ^a * ^a -> unit) (x,y,z))
let inline GuardSizes4(x,y,z,o) =
    (^a: (static member GuardSizes: ^a * ^a * ^a * ^a -> unit) (x,y,z,o))
let inline Size x =
    (^a : (member Size: int) x)

let inline P x: (^a -> CUdeviceptr) * ^a = (extract_primal, x)
let inline A x: (^a -> CUdeviceptr) * ^a = (extract_adjoint, x)
let inline P' x: (^a -> CudaDeviceVariable<float32>) * ^a = (extract_primal', x)
let inline A' x: (^a -> CudaDeviceVariable<float32>) * ^a = (extract_adjoint', x)

let inline rows x = (^a : (member Rows: int) x)
let inline cols x = (^a : (member Columns: int) x)
let inline size x = (^a : (member Size: int) x)

let inline setPrimal x (v,str) = (^a : (member SetPrimal: float32 * CudaStream -> unit) x, v, str)
let inline setAdjoint x (v,str) = (^a : (member SetAdjoint: float32 * CudaStream -> unit) x, v, str)

let inline isDead x = (^a : (member IsDead: DeadFlagType) x)
let inline deadIs x v = (^a : (member DeadIs: DeadFlagType -> unit) x, v)

let inline hasAdjoint x = (^a : (member HasAdjoint: bool) x)

let inline nchw x =
    (^a: (member NCHW: int * int * int * int) x)

/// Helper for the buggy add_tensor function.
let inline nchwBiasAdd x =
    (^a: (member NCHWBiasAdd: int * int * int * int) x)

let inline convertdMLikeFromWorkspace (x: ^a) (w: Workspace) =
    (^a: (member ConvertdMLikeFromWorkspace: Workspace -> ^a) x, w)

let inline addDims x =
    (^a: (member AddDims: int) x)

