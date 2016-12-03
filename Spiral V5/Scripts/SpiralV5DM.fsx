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

/// The wrapper type for the d2M and d4M. It is really unbelievable how much it simplified the code for the combinators.
type dM =
    {
    size : int[]
    }

// TODO: Continue this when done testing Racket.