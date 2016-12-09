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

type VarF32 = CudaDeviceVariable<float32>
type VarByte = CudaDeviceVariable<byte>

type DM(size: int[], data: VarF32[]) =
    member val Size = size with get, set
    member val Data = data with get, set

    interface IDisposable with
        member t.Dispose() = for var in t.Data do var.Dispose()

let float32_total_size_of size = Array.reduce (*) size

/// Zeroes out the fields on the first allocation.
let new_var_float32 (total_size: int) =
    let x = new CudaDeviceVariable<float32>(SizeT total_size)
    x.Memset(0u)
    x
    
let makeDM(size: int[], num_vars: int) =
    let total_size = float32_total_size_of size
    new DM(size, Array.init num_vars <| fun _ -> new_var_float32 total_size)

let primal (x: DM) = let i=0 in if i < x.Data.Length then x.Data.[i] else failwith "DM does not have a primal."
let adjoint (x: DM) = let i=1 in if i < x.Data.Length then x.Data.[i] else failwith "DM does not have an adjoint."
let aux1 (x: DM) = let i=2 in if i < x.Data.Length then x.Data.[i] else failwith "DM does not have an aux1."
let aux2 (x: DM) = let i=3 in if i < x.Data.Length then x.Data.[i] else failwith "DM does not have an aux2."

type SpiralObjectPool() =
    let dMPool = ResizeArray()
    let mutable dMp = 0

    member t.GetDM(size: int[], num_vars: int) =
        let resizeIf (x: DM) = 
            let total_size = float32_total_size_of size
            let total_size' = float32_total_size_of x.Size
            if x.Data.Length <> num_vars then
                x.Data <- Array.init num_vars <| fun i ->
                    if i < x.Data.Length then
                        if total_size > total_size' then dispose x.Data.[i]; new_var_float32 total_size
                        else x.Data.[i]
                    else new_var_float32 total_size
            elif total_size > total_size' then
                for i=0 to x.Data.Length do
                    dispose x.Data.[i]; x.Data.[i] <- new_var_float32 total_size

        if dMPool.Count > dMp then
            let t = dMPool.[dMp]
            dMp <- dMp+1
            t
        else
            let t = makeDM(size,num_vars)
            dMPool.Add(t)
            dMp <- dMp+1
            t