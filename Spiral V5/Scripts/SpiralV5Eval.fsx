#load "SpiralV5Prims.fsx"
open SpiralV5
open SpiralV5DM
open SpiralV5CudaCodeGen
open SpiralV5Prims

let base_node x (env: SpiralEnv<_>) = x
let inline matmult a b (env: SpiralEnv<_>) = Primitives.matmult (a env) (b env) env
let inline seq_matmult abs (env: SpiralEnv<_>) = 
    let l = List.map (fun (a,b) -> a env, b env) abs
    Primitives.seqmatmult l env
/// Standard addition (no broadcasting.)
let inline add alpha a beta b (env: SpiralEnv<_>) = 
    Primitives.standard_add alpha (a env) beta (b env) env

/// 4d Extractor for badd. Just the id.
let ba4 = id,id
/// 2d Extractor for badd. Does some complicated shuffling.
let ba2 = 
    let s_to_4d (c,r) = (c,1,r,1)
    let s_to_4d_backwards (c,r) = (c,r,1,1) // A hack to make the backwards step 10x faster
    s_to_4d, s_to_4d_backwards

/// Broadcast addition. Does it mutably on the left argument unlike the rest of the operations which are immutable.
let inline badd bax alpha a beta b (env: SpiralEnv<_>) = 
    let s_to_4d,s_to_4d_backwards = bax
    Primitives.routed_add (s_to_4d, s_to_4d_backwards) true alpha (a env) beta (b env) env

let inline hadmult a b (env: SpiralEnv<_>) = 
    Primitives.hadmult (a env, b env) env
let inline seqhadmult abs (env: SpiralEnv<_>) = 
    let l = List.map (fun (a,b) -> a env, b env) abs
    Primitives.seqhadmult l env
let inline relu x (env: SpiralEnv<_>) = 
    Primitives.relu (x env) env
let inline tanh x (env: SpiralEnv<_>) = 
    Primitives.tanh (x env) env
let inline sigmoid x (env: SpiralEnv<_>) = 
    Primitives.sigmoid (x env) env
let inline clipped_sigmoid x min max (env: SpiralEnv<_>) = 
    Primitives.clipped_sigmoid (x env) min max env
let inline clip x min max (env: SpiralEnv<_>) = 
    Primitives.clip (x env) min max env
let inline square x (env: SpiralEnv<_>) = 
    Primitives.square (x env) env
let inline sum x (env: SpiralEnv<_>) =
    Primitives.sum (x env) env
let inline log x (env: SpiralEnv<_>) = 
    Primitives.log (x env) env
let inline scalar_matrix_add x coef scalar (env: SpiralEnv<_>) =
    Primitives.scalar_matrix_add (x env) coef scalar env
let inline scale alpha x (env: SpiralEnv<_>) =
    Primitives.scale alpha (x env) env
let inline sum_scalars x (env: SpiralEnv<_>) = /// TODO: Not sure about the type of this one. Adjust when making recurrent nets.
    Primitives.sum_scalars x env
let inline reshape x conv (env: SpiralEnv<_>) =
    Primitives.reshape (x env) conv env

let inline private cross_entropy_cost_template log scalar_matrix_add seqhadmult sum scale num_examples_of target input =
    let lt = target
    let li = log input
    let rt = scalar_matrix_add target 1.0f -1.0f
    let ri = scalar_matrix_add input 1.0f -1.0f |> log 
    seqhadmult [lt, li; rt, ri] 
    |> sum 
    |> scale (-1.0f / float32 (num_examples_of target))

// This part is a bit ugly. The missing language feature to remove this bit of boiler plate are either Scala's implicits or
// algebraic effect handlers (which Ocaml will get.)
// Also Lisp's macros could generate this boilerplate automatically.

// But there is no doubt that the templated form is how the function should be written.
// I actually prever how the above came out to what it was in the previous library.
let inline cross_entropy_cost num_examples_of target input env =
    let log x = Primitives.log x env
    let scalar_matrix_add x coef scalar = Primitives.scalar_matrix_add x coef scalar env
    let seqhadmult x = Primitives.seqhadmult x env
    let sum x = Primitives.sum x env
    let scale alpha x = Primitives.scale alpha x env
    cross_entropy_cost_template log scalar_matrix_add seqhadmult sum scale num_examples_of (target env) (input env)

let inline private squared_error_template add square sum scale num_examples_of target input =
    add 1.0f target -1.0f input
    |> square 
    |> sum 
    |> scale (0.5f / float32 (num_examples_of target))

let inline squared_error_cost num_examples_of target input env =
    let add alpha a beta b = Primitives.standard_add alpha a beta b env
    let square x = Primitives.square x env
    let sum x = Primitives.sum x env
    let scale alpha x = Primitives.scale alpha x env
    squared_error_template add square sum scale num_examples_of (target env) (input env)

// Dim extractors.
/// Pass it as the dim_extractor argument to cost_function. Will evaluate target so make sure it is either a base_node or memoized.
let cf4 (x: DM<int*int*int*int,_>) = x.Size |> fun (n,_,_,_) -> n
/// Pass it as the dim_extractor argument to cost_function. Will evaluate target so make sure it is either a base_node or memoized.
let cf3 (x: DM<int*int*int,_>) = x.Size |> fun (n,_,_) -> n
/// Pass it as the dim_extractor argument to cost_function. Will evaluate target so make sure it is either a base_node or memoized.
let cf2 (x: DM<int*int,_>) = x.Size |> fun (c,_) -> c
/// Pass it as the dim_extractor argument to cost_function. Will evaluate target so make sure it is either a base_node or memoized.
let cf1 (x: DM<int,_>) = 1

let grad_checking (node : DM<_,_>) (env: SpiralEnv<_>) =
    () // Does nothing. This is here so the adjoints do not get zeroed out.
let inline sgd  learning_rate (node : DM<_,_>) (env: SpiralEnv<_>) =
    Primitives.mutable_map_operation 2 node [-learning_rate] clipped_sgd env
let inline clipped_sgd  learning_rate clipping_threshold (node : DM<_,_>) (env: SpiralEnv<_>) =
    Primitives.mutable_map_operation 2 node [-learning_rate;clipping_threshold] clipped_sgd env

/// Fills primal of a matrix by sampling from a random uniform distribution in <-1.0f,1.0f]. Is inplace and mutable.
let fill_random_uniform_matrix (scaling_factor: float32) (location: float32) (env: SpiralEnv<_>) (x: DM<_,float32>) =
    cudaRandom.SetStream env.Str.Stream
    cudaRandom.GenerateUniform(x.P)

    // 2.0f*scaling_factor ensures that it is rescaled in the [-1.0f;1.0f] range if the scaling_factor is 1.0f.
    Primitives.mutable_map_operation 1 x [2.0f * scaling_factor; location] random_normalization env
    x

let relu_initializer add_dims (env: SpiralEnv<_>) (a: DM<_,_>) =
    let t = add_dims a.Size
    let scale = (1.0f / sqrt(float32 t))
    fill_random_uniform_matrix scale 0.0f env a

let create_ff_sublayer_template create_weight create_bias matmult add_bias deal_with_weights activation input (env: SpiralEnv<_>) =
    let input: DM<_,_> = input env
    let W: DM<_,_> = create_weight input env
    let b: DM<_,_> = create_bias env

    (fun x -> matmult W x |> add_bias b |> activation), // run function
    deal_with_weights W b

let create_2d_ff_sublayer desired_size activation input env =
    let create_weight (input: DM<int*int,_>) env = 
        let c,r = input.Size
        createDM (r,desired_size) 2 |> relu_initializer add_dims_2d env
    let create_bias env = 
        createDM (1,desired_size) 2 |> relu_initializer add_dims_2d env
    let matmult W x env = Primitives.matmult W (x env) env
    let add_bias b a env = Primitives.routed_add ba2 true 1.0f (a env) 1.0f b env
    let deal_with_weights W b = [|W;b|]

    let f, weights = create_ff_sublayer_template create_weight create_bias matmult add_bias deal_with_weights activation input env
    f//, weights // TODO: Should I store the weights in the environment or do something more refined with them?

let create_layer runner (create_layer: ('input -> 'env -> ('input -> 'output))) =
    // Note: It should not be assumed that a layer with id-1 is in fact one layer below the current one.
    // That is not how tags are intended to be used. Instead they are to be used only to memoize their current results.
    
    // Residual connection are to be done using specialized layer tying functions.
    let id = tag() 
    let mutable node = None
    fun (input: 'input) (env: 'env) ->
        match node with
        | Some sub_layer -> runner sub_layer id input env : 'true_output
        | None ->
            let sub_layer = create_layer input env
            node <- Some sub_layer
            runner sub_layer id input env : 'true_output

let layer_cost_function runner (cost_f: ('input -> 'output)) =
    let id = tag()
    fun (input: 'input) (env: 'env) ->
        runner cost_f id input env : 'true_output

let ff_runner sub_layer id input env =
    sub_layer input env

let layer_2d_ff desire_size activation = create_2d_ff_sublayer desire_size activation |> create_layer ff_runner

let combine_layer a b env =
    b (a env)

let (++) a b = combine_layer a b

let ce target input = cross_entropy_cost cf2 target input

let layers target = 
    [|
    layer_2d_ff 128 relu
    layer_2d_ff 128 relu
    layer_2d_ff 10 sigmoid
    |] |> Array.reduce (++)
    |> fun x -> x ++ layer_cost_function ff_runner (cross_entropy_cost cf2 target)
