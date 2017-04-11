#load "SpiralV5CudaInit.fsx"
open SpiralV5CudaInit

open System

open ManagedCuda
open ManagedCuda.BasicTypes
open ManagedCuda.VectorTypes
open ManagedCuda.CudaBlas
open ManagedCuda.CudaRand
open ManagedCuda.NVRTC
open ManagedCuda.CudaDNN

open System.Runtime.InteropServices

let inline dispose (v: #IDisposable) = v.Dispose()

/// The dynamic device variable type.
type SpiralDeviceVarType =
| UInt8T
| UInt16T
| UInt32T
| UInt64T
| Int8T
| Int16T
| Int32T
| Int64T
| Float32T
| Float64T
| BoolT

let spiral_sizeof = function
    | UInt8T -> 1UL
    | UInt16T -> 2UL
    | UInt32T -> 4UL
    | UInt64T -> 8UL
    | Int8T -> 1UL
    | Int16T -> 2UL
    | Int32T -> 4UL
    | Int64T -> 8UL
    | Float32T -> 4UL
    | Float64T -> 8UL
    | BoolT -> 4UL

let inline guard res = if res <> CUResult.Success then raise <| new CudaException(res)
let inline is_null_ptr x = x = CUdeviceptr()
let inline array_byte_length (x: 'a[]) = uint64 (int64 sizeof<'a> * x.LongLength)
let inline array_byte_length' x = array_byte_length x |> SizeT

[<Struct>]
/// The basic Spiral pointer type made to be simpler than ManagedCuda's CudaDeviceVariable.
type SpiralDeviceVar =
    val SizeInBytes: uint64
    val mutable DevicePtr: CUdeviceptr

    /// Allocates memory of the following size. The memory is uninitialized.
    new (size_in_bytes: uint64) =
        let data = new CUdeviceptr()
        DriverAPINativeMethods.MemoryManagement.cuMemAlloc_v2(ref data, SizeT size_in_bytes) |> guard
        {SizeInBytes=size_in_bytes;DevicePtr=data}

    member inline t.IsDisposed = is_null_ptr t.DevicePtr

    interface IDisposable with
        member t.Dispose() =
            if t.IsDisposed = false then
                // Ignore if failing.
                DriverAPINativeMethods.MemoryManagement.cuMemFree_v2(t.DevicePtr) |> ignore
                // Set the pointer to null.
                t.DevicePtr <- CUdeviceptr()

let inline prim_copy_template f (device_ptr: CUdeviceptr) (x: 'a[]) = 
    let handle = GCHandle.Alloc(x, GCHandleType.Pinned)
    try 
        let host_ptr = handle.AddrOfPinnedObject()
        f host_ptr
    finally
        handle.Free()
    |> guard

let prim_copy_from_host (device_ptr: CUdeviceptr) (host: 'a[]) = 
    prim_copy_template 
        (fun host_ptr -> 
            DriverAPINativeMethods.SynchronousMemcpy_v2.cuMemcpyHtoD_v2(device_ptr, host_ptr, array_byte_length' host)) 
        device_ptr host

let prim_copy_to_host (host: 'a[]) (device_ptr: CUdeviceptr) =
    prim_copy_template 
        (fun host_ptr -> 
            DriverAPINativeMethods.SynchronousMemcpy_v2.cuMemcpyDtoH_v2(host_ptr, device_ptr, array_byte_length' host)) 
        device_ptr host

[<Struct>]
/// Implements various pointer primitives with resizable semantics. The resizing can only grow the object for the sake
/// speed.
type SpiralDeviceUpwardResizableVar =
    val mutable ViewSizeInBytes: uint64
    val mutable Ptr: SpiralDeviceVar

    /// Allocates memory of the following size. The memory is uninitialized.
    new (size_in_bytes) =
        {ViewSizeInBytes=size_in_bytes
         Ptr=new SpiralDeviceVar(size_in_bytes)}

    /// Checks whether it has been disposed first.
    member t.GetDevicePtr = 
        if t.Ptr.IsDisposed = false then t.Ptr.DevicePtr 
        else failwith "Trying to get a disposed pointer."

    /// Synchronous memcopy from host to device. Adjust the pointer and allocates space if necessary.
    member t.CopyFromHost (source: 'a[]) =
        source.LongLength |> uint64 |> t.ResizeIf
        prim_copy_from_host t.Ptr.DevicePtr source

    member t.CopyToHost (dest: 'a[]) =
        if t.ViewSizeInBytes = array_byte_length dest then
            prim_copy_to_host dest t.GetDevicePtr
        else
            failwith "The sizes do not match."

    member inline private t.Memset(f, x: 'a) =
        let s = sizeof<'a>
        if t.ViewSizeInBytes % uint64 s = 0UL then
            f (t.GetDevicePtr, x, SizeT t.ViewSizeInBytes / s) |> guard
        else
            failwithf "The array size is not divisible by %i." s

    member t.Memset (x: byte) = t.Memset(DriverAPINativeMethods.Memset.cuMemsetD8_v2, x)
    member t.Memset (x: uint16) = t.Memset(DriverAPINativeMethods.Memset.cuMemsetD16_v2, x)
    member t.Memset (x: uint32) = t.Memset(DriverAPINativeMethods.Memset.cuMemsetD32_v2, x)

    member inline private t.MemsetAsync(f, x: 'a, str: CudaStream) =
        let s = sizeof<'a>
        if t.ViewSizeInBytes % uint64 s = 0UL then
            f (t.GetDevicePtr, x, SizeT t.ViewSizeInBytes / s, str.Stream) |> guard
        else
            failwithf "The array size is not divisible by %i." s

    member t.MemsetAsync (x: byte, str) = t.MemsetAsync(DriverAPINativeMethods.MemsetAsync.cuMemsetD8Async,x,str)
    member t.MemsetAsync (x: uint16, str) = t.MemsetAsync(DriverAPINativeMethods.MemsetAsync.cuMemsetD16Async,x,str)
    member t.MemsetAsync (x: uint32, str) = t.MemsetAsync(DriverAPINativeMethods.MemsetAsync.cuMemsetD32Async,x,str)

    member t.ResizeIf size_in_bytes =
        if size_in_bytes > t.Ptr.SizeInBytes || t.Ptr.IsDisposed then
            dispose t.Ptr
            t.Ptr <- new SpiralDeviceVar(size_in_bytes)
        t.ViewSizeInBytes <- size_in_bytes

    interface IDisposable with
        member t.Dispose() = dispose t.Ptr

type DM = 
    {
    mutable Size: uint64 []
    mutable Type: SpiralDeviceVarType
    Data: ResizeArray<SpiralDeviceUpwardResizableVar>
    }
    
    member t.NumVars = t.Data.Count

    interface IDisposable with
        member t.Dispose() = for var in t.Data do dispose var

let size_to_total_size x = Array.fold ((*)) 1UL x
let total_size (x: DM) = size_to_total_size x.Size

let primal (x: DM) = let i=0 in if i < x.NumVars then x.Data.[i] else failwith "DM does not have a primal."
let adjoint (x: DM) = let i=1 in if i < x.NumVars then x.Data.[i] else failwith "DM does not have an adjoint."
let has_adjoint (x: DM) = let i=1 in i < x.NumVars
let aux1 (x: DM) = let i=2 in if i < x.NumVars then x.Data.[i] else failwith "DM does not have an aux1."
let aux2 (x: DM) = let i=3 in if i < x.NumVars then x.Data.[i] else failwith "DM does not have an aux2."

let new_var total_size typ = new SpiralDeviceUpwardResizableVar(total_size * spiral_sizeof typ)
let dm_create size typ (num_vars: int) =
    let total_size = size_to_total_size size
    {Size=size;Type=typ;Data=Array.init num_vars (fun _ -> new_var total_size typ) |> ResizeArray}

type DM with 
    member x.P = primal x
    member x.A = adjoint x
    member x.P' f = f x.Size, primal x
    member x.A' f = f x.Size, adjoint x
    member x.HasAdjoint = has_adjoint x
    member x.NumAuxes = max (x.NumVars - 2) 0

    member t.Resize(size,typ,num_vars) =
        let total_size_in_byte = size_to_total_size size * spiral_sizeof typ
        for i=0 to num_vars-1 do
            if i < t.NumVars then t.Data.[i].ResizeIf total_size_in_byte
            else t.Data.Add <| new_var total_size_in_byte typ
        t.Size <- size
        t.Type <- typ

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

open System.Collections.Generic
type SpiralEnv<'user_state> =
    {
    // Memory (mutable)
    Str : CudaStream
    Mem : ObjectPool
    Tape : Stack<unit -> unit>
    Weights : Stack<DM>
    // State (immutable)
    IsInferenceOnly : bool
    }

    member t.PushTape x = t.Tape.Push x

    static member create =
        {
        Str = new CudaStream()
        Mem = new ObjectPool()
        Tape = new Stack<_>()
        Weights = new Stack<_>()
        IsInferenceOnly = false
        }

and ObjectPool() =
    let dMPool = ResizeArray<DM>()
    let mutable dMp = 0

    let workspace = dm_create [|128UL;128UL|] UInt8T 1

    let tensorDescriptorPool = Dictionary(HashIdentity.Structural)
    let filterDescriptorPool = Dictionary(HashIdentity.Structural)
    let convolutionDescriptorPool = Dictionary(HashIdentity.Structural)
    let poolingDescriptorPool = Dictionary(HashIdentity.Structural)
    let activationDescriptorPool = Dictionary(HashIdentity.Structural)
    let BNDescriptorPool = Dictionary(HashIdentity.Structural)

    static member inline private GetFromDict (pool : Dictionary<_,_>) k creation_function set_function =
        match pool.TryGetValue k with
        | true, v -> v
        | false, _ ->
            let t = creation_function()
            set_function t k
            pool.Add(k, t)
            t

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

    /// For getting the DM on the forward pass. Zeroes out the adjoint.
    member inline t.GetDM(size, typ, num_vars, env: SpiralEnv<_>) =
        let x = 
            if dMp < dMPool.Count then
                let x = dMPool.[dMp]
                x.Resize(size,typ,num_vars)
                x
            else
                let x = dm_create size typ num_vars
                dMPool.Add x
                x
                
        dMp <- dMp+1

        // The optimizers can only zero out the adjoints in the base nodes.
        // The object pool has to take up the slack for the rest.
        // The second variable is always the adjoint and here it is set to zero.
        if env.IsInferenceOnly = false && x.NumVars > 1 then x.A.MemsetAsync(0uy,env.Str) 

        x

    member inline t.GetWorkspace(size, typ, num_vars, env: SpiralEnv<_>) =
        workspace.Resize(size,typ,num_vars)
        workspace

    member t.Reset() = dMp <- 0

    interface IDisposable with
        member __.Dispose() =
            let dis pool = Seq.iter dispose pool
            dis dMPool
            dispose workspace

            dis tensorDescriptorPool.Values
            dis filterDescriptorPool.Values
            dis convolutionDescriptorPool.Values
            dis poolingDescriptorPool.Values
            dis activationDescriptorPool.Values
            dis BNDescriptorPool.Values
