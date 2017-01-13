#r @"C:\Users\Marko\Documents\Visual Studio 2015\Projects\Multi-armed Bandit Experiments\packages\TypeShape.2.8\lib\net40\TypeShape.dll"
open TypeShape

type CudaInt = CudaInt
type CudaFloat = CudaFloat

module CudaShape =
    let private SomeU = Some() // avoid allocating all the time
    let inline private test<'T> (s : TypeShape) =
        match s with
        | :? TypeShape<'T> -> SomeU
        | _ -> None

    let (|CudaInt|_|) s = test<CudaInt> s
    let (|CudaFloat|_|) s = test<CudaFloat> s

let mkCudaType<'T>() : 'T =
    match shapeof<'T> with
    | CudaShape.CudaInt -> unbox<'T> CudaInt
    | CudaShape.CudaFloat -> unbox<'T> CudaFloat
    | _ -> failwith "Unsupported type."

let x: CudaFloat = mkCudaType()