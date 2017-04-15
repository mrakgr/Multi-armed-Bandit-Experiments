// No, I give up. Discriminated unions are pretty much tailor made for simply typed interpreters.
// As far as I am concerned, the Cuda compiler is perfect.

// The problems I am having in the evaluators are specifically related to types.

#load "SpiralV5.fsx"
open SpiralV5

let map_launcher_block_size = 256
let map_redocol_map_launcher_block_size = 128
let map_redo_map_launcher_block_size = 256

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

//let map_module' (InputArgs in_group) (OutputArgs out_group) name f =
//    let args = [in_group; out_group] |> List.concat
//    cuda_codegen <|
//        [
//        include_ "thrust/tuple.h"
//        include_ "cub/cub.cuh"
//        externCBlock [
//            method_ CudaGlobal CudaVoid name args [
//                for_ [CudaVar("i",CudaInt),Value "blockIdx.x*blockDim.x + threadIdx.x"] (Var "i" .< Var "n") [Var "i" += Value "gridDim.x*blockDim.x"]
//                    (f (InputFArgs (cudavars_to_cudaexps out_group [Var "i"])) (OutputFArgs (cudavars_to_cudaexps in_group [Var "i"])))
//                ]
//            ]
//        ]
//    |> fun code -> code, get_unfolded_signature args

type CudaEnvironment =
    {
    indentation: int
    code : Text.StringBuilder
    }

    member t.PP(s: string) = t.code.Append s |> ignore

let tab_size = 4
let code = Text.StringBuilder()
let pp (s: string) = code.Append s |> ignore
let ind ind = code.Append (String.replicate ind " ") |> ignore
let pp' (s: string) i = ind i; pp s
let ppln (s: string) = code.AppendLine s |> ignore
let ppln' (s: string) i = ind i; code.AppendLine s |> ignore

type CudaTypeInterpreter = CudaTypeInterpreter with
    static member CudaConst(in_: CudaTypeInterpreter, subtype) =
        pp "const "; subtype in_
    static member CudaShared(in_: CudaTypeInterpreter, subtype) =
        pp "__shared__ "; subtype in_
    static member CudaVoid(_: CudaTypeInterpreter) = pp "void "
    static member CudaFloat(_: CudaTypeInterpreter) = "float "
    static member CudaInt(_: CudaTypeInterpreter) = pp "int "
    static member CudaAuto(_: CudaTypeInterpreter) = pp "auto "
    static member CudaThrustTuple(in_: CudaTypeInterpreter, subtypes) =
        pp "thrust::tuple<"; Array.fold (fun prefix x -> pp prefix; x in_; ", ") "" subtypes |> ignore; pp "> "

let inline cuda_const x in_ =
    ((^i) : (static member CudaConst: ^i * (^i -> unit) -> unit) in_, x)
let inline cuda_shared x in_ =
    ((^i) : (static member CudaShared: ^i * (^i -> unit) -> unit) in_, x)
let inline cuda_void x in_ =
    ((^i) : (static member CudaVoid: ^i -> unit) in_)
let inline cuda_float x in_ =
    ((^i) : (static member CudaVoid: ^i -> unit) in_)
let inline cuda_int x in_ =
    ((^i) : (static member CudaVoid: ^i -> unit) in_)
let inline cuda_auto x in_ =
    ((^i) : (static member CudaVoid: ^i -> unit) in_)
let inline cuda_thrust_tuple x in_ =
    ((^i) : (static member CudaThrustTuple: ^i * (^i -> unit) [] -> unit) in_, x)

let include_ s i = pp' "#include " i; pp <| quote s
let externCBlock_ l i =
    ppln """extern "C" {"""
    Array.iter (fun x -> x (i+tab_size)) l
    ind i; ppln "}"
let method annotation return_type name args =
    pp annotation
//    print_type return_type
//    pp name; pp "("
//    let env = print_arguments args env ""
//    ppln ") {"
//    print_seq body env.PlusIndent
//    ind(); ppln "}"

code.ToString()

