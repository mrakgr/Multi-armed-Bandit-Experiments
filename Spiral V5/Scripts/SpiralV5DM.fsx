#load "SpiralV5CudaCodeGen.fsx"

open SpiralV5
open SpiralV5CudaCodeGen

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
    P : Lazy<float32> // primal
    A : float32 ref // adjoint
    }

    static member inline create P =
        {P=P;A=ref 0.0f}

type Scalar = Scalar
type TotalSizeToken = TotalSizeToken with
    static member TotalSize(_: TotalSizeToken, (a,b,c,d,e)): int = a*b*c*d*e
    static member TotalSize(_: TotalSizeToken, (a,b,c,d)): int = a*b*c*d
    static member TotalSize(_: TotalSizeToken, (a,b,c)): int = a*b*c
    static member TotalSize(_: TotalSizeToken, (a,b)): int = a*b
    static member TotalSize(_: TotalSizeToken, x: int): int = x
    static member TotalSize(_: TotalSizeToken, x: Scalar): int = 1

let inline size_to_total_size x = 
    let call (t:^T) = ((^s or ^T) : (static member TotalSize: TotalSizeToken * ^s -> int) t, x)
    call TotalSizeToken

type DM<'s,'t when 't: struct 
               and 't: (new: unit -> 't) and 't:> System.ValueType
               and 's: equality>
        (size: 's, data: ResizeArray<CudaDeviceVariable<'t>>) =
    member t.Size = size
    member t.Data = data

    member t.NumVars = t.Data.Count

    interface IDisposable with
        member t.Dispose() = for var in t.Data do var.Dispose()

/// Zeroes out the fields on the first allocation.
let new_var (total_size: int) =
    let x = new CudaDeviceVariable<_>(SizeT total_size)
    x.Memset(0u)
    x

let inline createDM (size: 's) (num_vars: int) =
    let total_size = size_to_total_size size
    new DM<'s,_>(size, Array.init num_vars (fun _ -> new_var total_size) |> ResizeArray)

let copyToDevice (host_ar: 'a[]) (device_var: CudaDeviceVariable<'a>) =
    if int device_var.Size <> host_ar.Length then failwithf "int device_var.Size(%i) <> host_ar.Length(%i)" (int device_var.Size) (host_ar.Length)
    device_var.CopyToDevice(host_ar)

let primal (x: DM<_,_>) = let i=0 in if i < x.NumVars then x.Data.[i] else failwith "DM does not have a primal."
let adjoint (x: DM<_,_>) = let i=1 in if i < x.NumVars then x.Data.[i] else failwith "DM does not have an adjoint."
let has_adjoint (x: DM<_,_>) = let i=1 in i < x.NumVars
let aux1 (x: DM<_,_>) = let i=2 in if i < x.NumVars then x.Data.[i] else failwith "DM does not have an aux1."
let aux2 (x: DM<_,_>) = let i=3 in if i < x.NumVars then x.Data.[i] else failwith "DM does not have an aux2."

let total_size_2d (c,r) = c*r
let add_dims_2d (c,r) = c+r

type DM with 
    member x.P = primal x
    member x.A = adjoint x
    member x.P' = x.Size, primal x
    member x.A' = x.Size, adjoint x
    member x.HasAdjoint = has_adjoint x
    member x.NumAuxes = max (x.NumVars - 2) 0
    /// Allocates new variables if the Auxes are missing.
    /// The auxes are intended to be used for things like Nesterov's Momentum so it will also allocate the adjoint if it is missing.
//    member x.GetAuxes num_auxes = 
//        while x.NumAuxes < num_auxes do
//            x.Data.Add <| new_var x.TotalSizeInElems
//        List.init num_auxes (fun i -> x.Data.[2+i])

    static member inline create (c,r) num_vars (x: float32[]) =
        let d = createDM (c,r) num_vars
        copyToDevice x d.P
        d

let inline get_args (x: DM<_,_>) (num_args: int): CudaDeviceVariable<_> list = 
    let total_size = size_to_total_size x.Size
    while x.NumVars < num_args do 
        x.Data.Add <| new_var total_size
    List.init num_args (fun i -> x.Data.[i])


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

/// Disposes the old CudaDeviceVariable and returns the new in an efficient fashion.
let resizeIf (total_size_in_elems: int) (x: CudaDeviceVariable<_>): CudaDeviceVariable<'t> = 
    let total_size = sizeof<'t> * total_size_in_elems
    let new_size_is_bigger = total_size > int x.SizeInBytes

    if new_size_is_bigger then
        x.Dispose()
        new CudaDeviceVariable<_>(SizeT total_size_in_elems)
    else
        new CudaDeviceVariable<_>(x.DevicePointer,true,x.SizeInBytes)

type GenericPoolOperationType =
| PoolRegular
| PoolWorkspace

open System.Collections.Generic
type SpiralEnv<'user_state> =
    {
    // Memory (mutable)
    Str : CudaStream
    Mem : ObjectPool
    Tape : Stack<unit -> unit>
    // State (immutable)
    IsInferenceOnly : bool
    }

    member t.PushTape x = t.Tape.Push x

    static member create =
        {
        Str = new CudaStream()
        Mem = new ObjectPool()
        Tape = new Stack<_>()
        IsInferenceOnly = false
        }

and ObjectPool() =
    let dMPool = ResizeArray<obj>()
    let mutable dMp = 0

    let workspace = ResizeArray<obj>()

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

    member inline t.Get(size: 's, num_vars: int, env: SpiralEnv<_>, pool: ResizeArray<obj>, post_process): DM<'s,'t> =
        let total_size_in_elems = size_to_total_size size
        let get_var i =
            let t = 
                if pool.Count > dMp then resizeIf total_size_in_elems (pool.[dMp+i] :?> _)
                else resizeIf total_size_in_elems CudaDeviceVariable<byte>.Null
            pool.[dMp+i] <- t
            t

        let vars = Array.init num_vars (fun i -> get_var i) |> ResizeArray
        post_process vars // Increments the pointer and zeroes out the adjoint of vars if working on the regular pool.

        new DM<_,_>(size,vars)

    member inline t.GetDM(size, num_vars, env) =
        let post_process (vars: ResizeArray<CudaDeviceVariable<_>>) =
            dMp <- dMp + num_vars

            // The optimizers can only zero out the adjoints in the base nodes.
            // The object pool has to take up the slack for the rest.
            // The second variable is always the adjoint and here it is set to zero.
            if env.IsInferenceOnly = false && vars.Count > 1 then vars.[1].MemsetAsync(0u,env.Str.Stream) 
        t.Get(size,num_vars,env,dMPool,post_process)

    member inline t.GetWorkspace(size,total_size_in_elems, num_vars, env) =
        let post_process _ = ()
        t.Get(size,num_vars,env,workspace,post_process)

    interface IDisposable with
        member __.Dispose() =
            let dis pool = Seq.iter (fun (x: obj) -> dispose (x :?> IDisposable)) pool
            dis dMPool
            dis workspace

            let dis pool = Seq.iter dispose pool
            dis tensorDescriptorPool.Values
            dis filterDescriptorPool.Values
            dis convolutionDescriptorPool.Values
            dis poolingDescriptorPool.Values
            dis activationDescriptorPool.Values
            dis BNDescriptorPool.Values
