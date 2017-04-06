//#load "SpiralV5.fsx"
//
//open SpiralV5

#load "load-project-release.fsx"

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
| UInt32T
| UInt64T
| Int32T
| Int64T
| Float32T
| Float64T
| BoolT

let spiral_sizeof = function
    | UInt32T -> 4u
    | UInt64T -> 8u
    | Int32T -> 4u
    | Int64T -> 8u
    | Float32T -> 4u
    | Float64T -> 8u
    | BoolT -> 4u

let inline guard res = if res <> CUResult.Success then raise <| new CudaException(res)
let inline is_null_ptr x = x = CUdeviceptr()
let inline array_byte_length (x: 'a[]) = uint64 (int64 sizeof<'a> * x.LongLength)
let inline array_byte_length' x = array_byte_length x |> SizeT

[<Struct>]
/// The basic Spiral pointer type made to be simpler than ManagedCuda's CudaDeviceVariable.
type SpiralDevicePtr =
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
type SpiralDeviceUpwardResizablePtr =
    val mutable ViewSizeInBytes: uint64
    val mutable Ptr: SpiralDevicePtr

    new size_in_bytes =
        {ViewSizeInBytes=size_in_bytes
         Ptr=new SpiralDevicePtr(size_in_bytes)}

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

    member t.ResizeIf size_in_bytes =
        if size_in_bytes > t.Ptr.SizeInBytes || t.Ptr.IsDisposed then
            dispose t.Ptr
            t.Ptr <- new SpiralDevicePtr(size_in_bytes)
        t.ViewSizeInBytes <- size_in_bytes

    interface IDisposable with
        member t.Dispose() = dispose t.Ptr

type DM = 
    {
    Size: int []
    Type: SpiralDeviceVarType
    Data: ResizeArray<SpiralDeviceUpwardResizablePtr>
    }
    
    member t.NumVars = t.Data.Count

    interface IDisposable with
        member t.Dispose() = for var in t.Data do dispose var

let size_to_total_size x = Array.fold ((*)) 1 x
let total_size (x: DM) = size_to_total_size x.Size

let primal (x: DM) = let i=0 in if i < x.NumVars then x.Data.[i] else failwith "DM does not have a primal."
let adjoint (x: DM) = let i=1 in if i < x.NumVars then x.Data.[i] else failwith "DM does not have an adjoint."
let has_adjoint (x: DM) = let i=1 in i < x.NumVars
let aux1 (x: DM) = let i=2 in if i < x.NumVars then x.Data.[i] else failwith "DM does not have an aux1."
let aux2 (x: DM) = let i=3 in if i < x.NumVars then x.Data.[i] else failwith "DM does not have an aux2."

type DM with 
    member x.P = primal x
    member x.A = adjoint x
    member x.P' = x.Size, primal x
    member x.A' = x.Size, adjoint x
    member x.HasAdjoint = has_adjoint x
    member x.NumAuxes = max (x.NumVars - 2) 0