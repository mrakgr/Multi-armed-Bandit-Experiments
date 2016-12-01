#load "SpiralV5CodeGen.fsx"

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

type SpiralInitializer2D =
| Init2DV1 of float32[,]
| Init2DV2 of float32[][]

type SpiralInitializer3D =
| Init3DV1 of float32[,,]
| Init3DV2 of float32[][][]

type SpiralInitializer =
| InitNo of dims: int []
| InitRandom of dims: int []
| InitHost1D of float32[]
| InitHost2D of SpiralInitializer2D
| InitHost3D of SpiralInitializer3D

type SpiralExpr =
| BaseNode of id: int

type DM = 
    {mutable dims: int[]; var: CudaDeviceVariable<float32>}

type SpiralContext =
    {
    nodes: Dictionary<int,DM>
    }

let size dims = Array.reduce (*) dims
let make_var<'a when 'a: (new: unit -> 'a) and 'a: struct and 'a :> ValueType> (size: int) = new CudaDeviceVariable<'a>(SizeT size)

/// Copies a host array to device.
let to_dev<'a when 'a: (new: unit -> 'a) and 'a: struct and 'a :> ValueType> (host_ar: Array): CudaDeviceVariable<'a> =
    let d_a = make_var host_ar.Length
    d_a.CopyToDevice(host_ar)
    d_a

let init (ctx: SpiralContext) = 
    function
    | InitNo dims -> {dims = dims; var=make_var (size dims)}
    | InitRandom dims -> {dims = dims; var=make_var (size dims)} // TODO: Make this work properly.
    | InitHost1D data -> {dims = [|data.Length|]; var=to_dev data}
    | InitHost2D (Init2DV1 data) -> {dims = [|Array2D.length1 data; Array2D.length2 data|]; var=to_dev data}
    | InitHost2D (Init2DV2 data) -> 
        (data.[0].Length, data) ||>
        Array.fold (fun s x -> if s = x.Length then s else failwith "Regularity check failed.") 
        |> ignore
        {dims = [|data.Length; data.[0].Length|]; var=to_dev data}

    | InitHost3D (Init3DV1 data) -> {dims = [|Array3D.length1 data; Array3D.length2 data; Array3D.length3 data|]; var=to_dev data}
    | InitHost3D (Init3DV2 data) -> 
        (data.[0].Length, data) ||>
        Array.fold (fun s x -> 
            (x.[0].Length, x) ||>
            Array.fold (fun s x -> if s = x.Length then s else failwith "Regularity check failed.") 
            |> ignore            
            if s = x.Length then s else failwith "Regularity check failed.") 
        |> ignore

        {dims = [|data.Length; data.[0].Length|]; var=to_dev data}