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

type DMType =
    | DMFloat
    | DMByte

    member t.Size =
        match t with
        | DMFloat -> 4
        | DMByte -> 1

/// To be honest this does not feel good at all to me.
/// I'll deal with it tomorrow. Today I was busy with other things.
type DM =
    {
    mutable size : int[]
    mutable type_ : DMType // The type is tracked dynamically.
    mutable vars : ResizeArray<CudaDeviceVariable<byte>>
    }

    static member create(size: int[], type_: DMType) =
        {size=size; type_=type_; vars=ResizeArray(2)}

    member t.ResizeIf(size: int[], type_: DMType) =
        let total_size = type_.Size * Array.reduce (*) size
        t.size <- size
        t.type_ <- type_
        for i=0 to t.vars.Count do
            if total_size > int t.vars.[i].SizeInBytes then 
                dispose t.vars.[i]
                t.vars.[i] <- new CudaDeviceVariable<_>(SizeT total_size)

    member t.Get(i: int) =
        if i < t.vars.Count then
            t.vars.[i].DevicePointer
        else
            while t.vars.Count <= i do
                t.vars.Add(CudaDeviceVariable<_>.Null)

            t.ResizeIf(t.size,t.type_)
            t.vars.[i].DevicePointer

    interface IDisposable with
        member t.Dispose() = for var in t.vars do var.Dispose()


// TODO: Continue this when done testing Racket.