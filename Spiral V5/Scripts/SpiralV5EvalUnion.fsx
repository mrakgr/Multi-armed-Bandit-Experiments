#load "SpiralV5Prims.fsx"
open SpiralV5
open SpiralV5DM
open SpiralV5CudaCodeGen
open SpiralV5Prims

type SpiralVar =
| DMF32 of DM<float32>
| DMInt of DM<int>
| DF of Df
| DFSeq of Df []
| ConstFloat32 of float32

type SpiralExp =
| BaseNode of SpiralVar
| Matmult of SpiralExp * SpiralExp
| Seqmatmult of (SpiralExp * SpiralExp) list
| Add of float32 * SpiralExp * float32 * SpiralExp
| Badd of float32 * SpiralExp * float32 * SpiralExp
| Hadmult of SpiralExp * SpiralExp
| Seqhadmult of (SpiralExp * SpiralExp) list
// Map operations. TODO: Replace all of them with a single Map expression operation.
| Relu of SpiralExp
| Tanh of SpiralExp
| Sigmoid of SpiralExp
| ClippedSigmoid of SpiralExp * min: float32 * max: float32
| Clip of SpiralExp * min: float32 * max: float32
| Square of SpiralExp
| Sum of SpiralExp
| Log of SpiralExp
| ScalarMatrixAdd of SpiralExp * coef: float32 * scalar: float32
// Df operations - These are done on host.
| Scale of const_: SpiralExp * SpiralExp
| SumScalars of SpiralExp
| Reshape of SpiralExp * conv: (int[] -> int[])
| NumExamples of SpiralExp * conv: (int[] -> float32)

let rec eval e env =
    let match_constfloat32 x =
        match eval x env with
        | ConstFloat32 x -> x
        | _ -> failwithf "Not ConstFloat32(%A)." x
    let match_dfseq x =
        match eval x env with
        | DFSeq x -> x
        | _ -> failwithf "Not DFSeq(%A)." x
    let match_df x =
        match eval x env with
        | DF x -> x
        | _ -> failwithf "Not DF(%A)." x
    let match_dmf32 x = 
        match eval x env with
        | DMF32 x -> x 
        | _ -> failwithf "Not DMF32(%A)." x
    let ex_2d x = 
        match x with
        | [|a;b|] -> a,b
        | _ -> failwithf "Not 2d(%A)." x
    let eval' x = eval x env
    match e with
    | BaseNode x -> x
    | Matmult(a,b) ->
        Primitives.matmult ex_2d ex_2d ex_2d (match_dmf32 a) (match_dmf32 b) env |> DMF32
    | Seqmatmult abs ->
        let l = List.map (fun (a,b) -> match_dmf32 a, match_dmf32 b) abs 
        Primitives.seqmatmult ex_2d ex_2d ex_2d l env |> DMF32
    | Add (alpha, a, beta, b) ->
        Primitives.standard_add alpha (match_dmf32 a) beta (match_dmf32 b) env |> DMF32
    | Badd(alpha, a, beta, b) ->
        match eval' a,eval' b with
        | DMF32 a, DMF32 b ->
            match a.Size, b.Size with
            | [|_;_|], [|_;_|] ->
                let s_to_4d [|c;r|] = (c,1,r,1)
                let s_to_4d_backwards [|c;r|] = (c,r,1,1) // A hack to make the backwards step 10x faster
                Primitives.routed_add s_to_4d s_to_4d_backwards true alpha a beta b env |> DMF32
            | [|_;_;_;_|], [|_;_;_;_|] ->
                let id [|n;c;h;w|] = n,c,h,w
                Primitives.routed_add id id true alpha a beta b env |> DMF32
        | _,_ -> failwith "Invalid type."
    | Hadmult(a,b) ->
        match eval' a, eval' b with
        | DMF32 a, DMF32 b -> Primitives.hadmult (a, b) env |> DMF32
        | _,_ -> failwith "Invalid type."
    | Seqhadmult abs ->
        let l = List.map (fun (x,y) -> (eval' x, eval' y) |> fun (DMF32 x, DMF32 y) -> x,y) abs
        Primitives.seqhadmult l env |> DMF32
    // Map operations. TODO: Replace all of them with a single Map expression operation.
    | Relu x -> Primitives.map_operation [match_dmf32 x] [] relu relu_backward env |> DMF32
    | Tanh x -> Primitives.map_operation [match_dmf32 x] [] tanh tanh_backward env |> DMF32
    | Sigmoid x -> Primitives.map_operation [match_dmf32 x] [] sigmoid sigmoid_backward env |> DMF32
    | ClippedSigmoid(x,min,max) -> Primitives.map_operation [match_dmf32 x] [min;max] clipped_sigmoid clipped_sigmoid_backward env |> DMF32
    | Clip(x,min,max) -> Primitives.map_operation [match_dmf32 x] [min;max] clip clip_backward env |> DMF32
    | Square x -> Primitives.map_operation [match_dmf32 x] [] square square_backward env |> DMF32
    | Sum x -> Primitives.map_redo_map_operation [match_dmf32 x] [] sum sum_backward env |> DF
    | Log x -> Primitives.map_operation [match_dmf32 x] [] log_ log_backward env |> DMF32
    | ScalarMatrixAdd(x,coef,scalar) -> Primitives.map_operation [match_dmf32 x] [coef;scalar] scalar_matrix_add scalar_matrix_add_backward env |> DMF32
    // Df operations - These are done on host.
    | Scale(alpha,x) -> Primitives.scale (match_constfloat32 alpha) (match_df x) env |> DF
    | SumScalars(xs) -> Primitives.sum_scalars (match_dfseq xs) env |> DF
    | Reshape(x, conv) -> Primitives.reshape (match_dmf32 x) conv env |> DMF32
    | NumExamples(x, conv) -> (match_dmf32 x).Size |> conv |> ConstFloat32 // TODO: Make x CudaVar.

/// Rather than use it directly pass it into cost_function as the cost_f argument
let cross_entropy_cost num_examples_of target input =
    let lt = target
    let li = Log input
    let rt = ScalarMatrixAdd(target, -1.0f, 1.0f)
    let ri = ScalarMatrixAdd(input, -1.0f, 1.0f) |> Log
    Seqhadmult [lt, li; rt, ri] 
    |> Sum
    |> fun x -> Scale(NumExamples(target,num_examples_of), x) // -1.0f / num_examples

/// The generalized cost function.
/// dim_extrator gets the number of examples (usually the outermost dimension) from the target expression. It evaluates it first.
let cost_function cfx cost_f target input (env: SpiralEnv<_>): Df =
    cost_f cfx target input env

//let grad_checking (node : DM<_,_>) (env: SpiralEnv<_>) =
//    () // Does nothing. This is here so the adjoints do not get zeroed out.
//let sgd learning_rate (node : DM<_,_>) (env: SpiralEnv<_>) =
//    Primitives.mutable_map_operation 2 node [-learning_rate] clipped_sgd env
//let clipped_sgd learning_rate clipping_threshold (node : DM<_,_>) (env: SpiralEnv<_>) =
//    Primitives.mutable_map_operation 2 node [-learning_rate;clipping_threshold] clipped_sgd env
//
///// Fills primal of a matrix by sampling from a random uniform distribution in <-1.0f,1.0f]. Is inplace and mutable.
//let fillRandomUniformMatrix (scaling_factor: float32) (location: float32) (env: SpiralEnv<_>) (x: DM<_,float32>) =
//    cudaRandom.SetStream env.Str.Stream
//    cudaRandom.GenerateUniform(x.P)
//
//    // 2.0f*scaling_factor ensures that it is rescaled in the [-1.0f;1.0f] range if the scaling_factor is 1.0f.
//    Primitives.mutable_map_operation 1 x [2.0f * scaling_factor; location] random_normalization env
//    x
//
//let feedforward_layer size total_size env =
//    let W = createDM size total_size 2 |> fillRandomUniformMatrix 1.0f 0.0f env
//    ()
    // Hmmm...