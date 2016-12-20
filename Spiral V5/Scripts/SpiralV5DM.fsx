﻿#load "SpiralV5CudaCodeGen.fsx"

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

let total_size_of size = Array.reduce (*) size

/// The float scalar type
type Df = 
    {
    P : Lazy<float32> // primal
    A : float32 ref // adjoint
    }

    static member inline create P =
        {P=P;A=ref 0.0f}

type DM<'s,'t when 't: struct 
               and 't: (new: unit -> 't) and 't:> System.ValueType
               and 's: equality>
        (size: 's, total_size_in_elems: int, data: CudaDeviceVariable<'t>[]) =
    member t.Size = size
    member t.Data = data

    member t.TotalSizeInElems = total_size_in_elems
    member t.NumVars = t.Data.Length

    interface IDisposable with
        member t.Dispose() = for var in t.Data do var.Dispose()

/// Zeroes out the fields on the first allocation.
let new_var (total_size: int) =
    let x = new CudaDeviceVariable<_>(SizeT total_size)
    x.Memset(0u)
    x
    
let makeDMf32(size: 's, total_size, num_vars: int) =
    new DM<'s,float32>(size, total_size, Array.init num_vars <| fun _ -> new_var total_size)

let primal (x: DM<_,_>) = let i=0 in if i < x.Data.Length then x.Data.[i] else failwith "DM does not have a primal."
let adjoint (x: DM<_,_>) = let i=1 in if i < x.Data.Length then x.Data.[i] else failwith "DM does not have an adjoint."
let has_adjoint (x: DM<_,_>) = let i=1 in i < x.Data.Length
let aux1 (x: DM<_,_>) = let i=2 in if i < x.Data.Length then x.Data.[i] else failwith "DM does not have an aux1."
let aux2 (x: DM<_,_>) = let i=3 in if i < x.Data.Length then x.Data.[i] else failwith "DM does not have an aux2."

type DM with 
    member x.P = primal x
    member x.A = adjoint x
    member x.P' = x.Size, x.TotalSizeInElems, primal x
    member x.A' = x.Size, x.TotalSizeInElems, adjoint x
    member x.HasAdjoint = has_adjoint x
    member x.Aux1 = aux1 x
    member x.Aux2 = aux2 x

    /// Resizes the DM.
    /// Does the least amount of work possible.
//    member x.ResizeIf (dims: 's, total_size, num_vars: int) = 
//        let new_size_is_bigger = total_size > x.TotalSize
//
//        if x.Data.Length < num_vars then
//            x.Data <- Array.init num_vars <| fun i ->
//                if i < x.Data.Length then
//                    if new_size_is_bigger then dispose x.Data.[i]; new_var total_size
//                    else x.Data.[i]
//                else new_var total_size
//        elif new_size_is_bigger then
//            let l = min x.Data.Length num_vars
//            for i=0 to l-1 do
//                dispose x.Data.[i]; x.Data.[i] <- new_var total_size
//
//        // This is to help the GC a little.
//        if x.Size.Length = dims.Length then Array.Copy(dims,x.Size,dims.Length)
//        else x.Size <- dims

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
type SpiralEnv =
    {
    // Memory (mutable)
    Str : CudaStream
    Mem : ObjectPool
    Tape : Stack<unit -> unit>
    Nodes : Dictionary<int,obj>
    // State (immutable)
    IsInferenceOnly : bool
    }

    member t.PushTape x = t.Tape.Push x

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

    member inline private t.Get(size: 's, total_size_in_elems: int, num_vars: int, env: SpiralEnv) pool_type: DM<'s,'t> =
        let pool =
            match pool_type with
            | PoolRegular -> dMPool
            | PoolWorkspace -> workspace

        let get_var i =
            let t = 
                if pool.Count > dMp then resizeIf total_size_in_elems (pool.[dMp+i] :?> _)
                else resizeIf total_size_in_elems CudaDeviceVariable<byte>.Null
            pool.[dMp+i] <- t
            t

        let vars = [| for i=0 to num_vars-1 do yield get_var i |]

        match pool_type with
        | PoolRegular -> 
            dMp <- dMp + num_vars

            // The optimizers can only zero out the adjoints in the base nodes.
            // The object pool has to take up the slack for the rest.
            // The second variable is always the adjoint and here it is set to zero.
            if env.IsInferenceOnly = false && vars.Length > 1 then vars.[1].MemsetAsync(0u,env.Str.Stream) 
        | PoolWorkspace -> ()

        new DM<_,_>(size,total_size_in_elems,vars)

    member t.GetDM(size, total_size_in_elems, num_vars, env) =
        t.Get(size,total_size_in_elems,num_vars,env) PoolRegular

    member t.GetWorkspace(size,total_size_in_elems, num_vars, env) =
        t.Get(size,total_size_in_elems,num_vars,env) PoolWorkspace

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
