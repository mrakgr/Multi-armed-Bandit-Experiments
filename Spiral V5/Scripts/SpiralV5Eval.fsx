#load "SpiralV5Prims.fsx"
open SpiralV5
open SpiralV5DM
open SpiralV5CudaCodeGen
open SpiralV5Prims

let base_node x (env: SpiralEnv<_>) = x
let matmult a b (env: SpiralEnv<_>) = Primitives.matmult (a env) (b env) env
let seq_matmult abs (env: SpiralEnv<_>) = 
    let l = List.map (fun (a,b) -> a env, b env) abs
    Primitives.seqmatmult l env
/// Standard addition (no broadcasting.)
let add alpha a beta b (env: SpiralEnv<_>) = 
    Primitives.standard_add alpha (a env) beta (b env) env

/// 4d Extractor for badd. Just the id.
let ba4 = id,id
/// 2d Extractor for badd. Does some complicated shuffling.
let ba2 = 
    let s_to_4d (c,r) = (c,1,r,1)
    let s_to_4d_backwards (c,r) = (c,r,1,1) // A hack to make the backwards step 10x faster
    s_to_4d, s_to_4d_backwards

/// Broadcast addition. Does it mutably on the left argument unlike the rest of the operations which are immutable.
let badd bax alpha a beta b (env: SpiralEnv<_>) = 
    let s_to_4d,s_to_4d_backwards = bax
    Primitives.routed_add s_to_4d s_to_4d_backwards true alpha (a env) beta (b env) env

let hadmult a b (env: SpiralEnv<_>) = 
    Primitives.hadmult (a env, b env) env
let seqhadmult abs (env: SpiralEnv<_>) = 
    let l = List.map (fun (a,b) -> a env, b env) abs
    Primitives.seqhadmult l env
let relu x (env: SpiralEnv<_>) = 
    Primitives.map_operation [x env] [] relu relu_backward env
let tanh x (env: SpiralEnv<_>) = 
    Primitives.map_operation [x env] [] tanh tanh_backward env
let sigmoid x (env: SpiralEnv<_>) = 
    Primitives.map_operation [x env] [] sigmoid sigmoid_backward env
let clipped_sigmoid x min max (env: SpiralEnv<_>) = 
    Primitives.map_operation [x env] [min;max] clipped_sigmoid clipped_sigmoid_backward env
let clip x min max (env: SpiralEnv<_>) = 
    Primitives.map_operation [x env] [min;max] clip clip_backward env
let square x (env: SpiralEnv<_>) = 
    Primitives.map_operation [x env] [] square square_backward env
let sum x (env: SpiralEnv<_>) =
    Primitives.map_redo_map_operation [x env] [] sum sum_backward env
let log x (env: SpiralEnv<_>) = 
    Primitives.map_operation [x env] [] log_ log_backward env
let scalar_matrix_add x coef scalar (env: SpiralEnv<_>) =
    Primitives.map_operation [x env] [coef;scalar] scalar_matrix_add scalar_matrix_add_backward env
let scale alpha x (env: SpiralEnv<_>) =
    Primitives.scale alpha (x env) env
let sum_scalars x (env: SpiralEnv<_>) = /// TODO: Not sure about the type of this one. Adjust when making recurrent nets.
    Primitives.sum_scalars x env
let reshape x conv (env: SpiralEnv<_>) =
    Primitives.reshape (x env) conv env

/// Rather than use it directly pass it into cost_function as the cost_f argument
let squared_error num_examples_of target input =
    add 1.0f target -1.0f input
    |> square
    |> sum
    |> scale (0.5f / float32 (num_examples_of target))

/// Rather than use it directly pass it into cost_function as the cost_f argument
let cross_entropy_cost num_examples_of target input =
    let lt = target
    let li = log input
    let rt = scalar_matrix_add target 1.0f -1.0f
    let ri = scalar_matrix_add input 1.0f -1.0f |> log
    seqhadmult [lt, li; rt, ri] 
    |> sum
    |> scale (-1.0f / float32 (num_examples_of target))

// Dim extractors.
/// Pass it as the dim_extractor argument to cost_function. Will evaluate target so make sure it is either a base_node or memoized.
let cf4 (x: SpiralEnv<_> -> DM<int*int*int*int,_>) env = (x env).Size |> fun (n,_,_,_) -> n
/// Pass it as the dim_extractor argument to cost_function. Will evaluate target so make sure it is either a base_node or memoized.
let cf3 (x: SpiralEnv<_> -> DM<int*int*int,_>) env = (x env).Size |> fun (n,_,_) -> n
/// Pass it as the dim_extractor argument to cost_function. Will evaluate target so make sure it is either a base_node or memoized.
let cf2 (x: SpiralEnv<_> -> DM<int*int,_>) env = (x env).Size |> fun (c,_) -> c
/// Pass it as the dim_extractor argument to cost_function. Will evaluate target so make sure it is either a base_node or memoized.
let cf1 (x: SpiralEnv<_> -> DM<int,_>) _ = 1

/// The generalized cost function.
/// dim_extrator gets the number of examples (usually the outermost dimension) from the target expression. It evaluates it first.
let cost_function cfx cost_f target input (env: SpiralEnv<_>): Df =
    cost_f cfx target input env

let grad_checking (node : DM<_,_>) (env: SpiralEnv<_>) =
    () // Does nothing. This is here so the adjoints do not get zeroed out.
let sgd learning_rate (node : DM<_,_>) (env: SpiralEnv<_>) =
    Primitives.mutable_map_operation 2 node [-learning_rate] clipped_sgd env
let clipped_sgd learning_rate clipping_threshold (node : DM<_,_>) (env: SpiralEnv<_>) =
    Primitives.mutable_map_operation 2 node [-learning_rate;clipping_threshold] clipped_sgd env

/// Fills primal of a matrix by sampling from a random uniform distribution in <-1.0f,1.0f]. Is inplace and mutable.
let fillRandomUniformMatrix (scaling_factor: float32) (location: float32) (env: SpiralEnv<_>) (x: DM<_,float32>) =
    cudaRandom.SetStream env.Str.Stream
    cudaRandom.GenerateUniform(x.P)

    // 2.0f*scaling_factor ensures that it is rescaled in the [-1.0f;1.0f] range if the scaling_factor is 1.0f.
    Primitives.mutable_map_operation 1 x [2.0f * scaling_factor; location] random_normalization env
    x

let feedforward_layer size total_size env =
    let W = createDM size total_size 2 |> fillRandomUniformMatrix 1.0f 0.0f env
    ()
