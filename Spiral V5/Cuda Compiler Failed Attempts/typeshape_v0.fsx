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

type MapLambda<'ins,'consts,'outs> = 'ins -> 'consts -> 'outs

let mkCudaLambda<'ins, 'consts,'outs>(f: 'ins -> 'consts -> 'outs) : 'ins * 'consts * 'outs =
    let rec mkElem (field : IShapeMember<'DeclaringType>) =
        field.Accept {
            new IMemberVisitor<'DeclaringType, ('DeclaringType -> _)> with
                member __.Visit(field : ShapeMember<'DeclaringType, 'Field>) =
                    field.Project
        }

    let ins =
        match shapeof<'ins> with
        | CudaShape.CudaInt -> unbox<'T> CudaInt
        | CudaShape.CudaFloat -> unbox<'T> CudaFloat
        | Shape.Tuple (:? ShapeTuple<'T> as shape) ->
            let elems = shape.Elements |> Array.map mkElem
        | _ -> failwith "Unsupported type."
