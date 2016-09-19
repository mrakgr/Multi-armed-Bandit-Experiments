[<AutoOpen>]
module SpiralV4.Combinators

open System
open System.Collections.Generic

// For wavefront iteration.
// From https://docs.microsoft.com/en-us/dotnet/articles/fsharp/language-reference/computation-expressions
/// Computations that can be run step by step.
type Eventually<'T> =
    | Done of 'T
    | NotYetDone of (unit -> Eventually<'T>)

    static member doBind (expr: Eventually<'a>,func: 'a -> Eventually<'b>) =
        match expr with
        | Done value -> NotYetDone (fun () -> func value)
        | NotYetDone work -> NotYetDone (fun () -> Eventually<_>.doBind(work(), func))

    static member doReturn expr = Done expr

    // The stepping action for the computations.
    static member step expr =
        match expr with
        | Done _ -> expr
        | NotYetDone func -> func ()

/// The type for combining layers. Can be used to combine RNN with feedfoward layers.
type LayerWrapper<'input,'output,'state when 'state :> StanState > =
    {
    RunLayer: 'input -> 'state -> 'output * 'state
    RunDelayedLayer: 'input * 'state -> Eventually<'output * 'state>
    WrappedNodes: Lazy<DM[]> list
    }

    member inline t.Update(optimizer,state) =
        t.WrappedNodes |> List.iter (fun x -> x.Value |> Array.iter (optimize(optimizer,state)))

    interface IDisposable with
        member t.Dispose() =
            t.WrappedNodes |> List.iter (fun x -> x.Value |> Array.iter dispose)

/// Kleisli arrow for the state passing functions
let inline (>=>) a b = fun x -> a x >>= b
/// Generic right assiciative Kleisli arrow for classes implementing doBind
let inline (^<) a b = fun x -> Eventually<_>.doBind(a x, b)

let inline runLayer layer =
    (^layer_type: (member RunLayer: (^input -> #StanState -> ^output * #StanState)) layer)
let inline runDelayedLayer layer =
    (^layer_type: (member RunDelayedLayer: (^input * #StanState -> Eventually< ^output * #StanState>)) layer)

let inline runLayerWithCost
        (cost: ^target -> ^target -> #StanState -> ^result * #StanState) (layer: ^layer) target
        : ^input -> #StanState -> ^result * #StanState =
    runLayer layer >=> cost target
let inline private trans (f: ^a -> ^s -> ^b * ^s) = fun (a,b) -> Done(f a b)
let inline runDelayedLayerWithCost 
        (cost: ^target -> ^target -> #StanState -> ^result * #StanState) (layer: ^layer) target
        : ^input * #StanState -> Eventually< ^result * #StanState> =
    runDelayedLayer layer ^< trans (cost target)

/// Generic function for feedforward net training and inference.
let inline private run 
        (network: LayerWrapper< ^input,^output,#StanState>) 
        (cost_func: ^output -> ^output -> #StanState -> (Lazy<int> * Df) * #StanState)
        (optimizer: Optimizer)
        (set : (^output * ^input)[]) 
        (state: #StanState) 
        test_delayed 
        test_accuracy =
    let mutable accumulated_cost = 0.0f
    let mutable accuracy = 0
    let mutable max_accuracy = 0

    let inline run'
            (extractor: ^m -> (Lazy<int> * Df) * #StanState) 
            (cost: ^output -> ^input -> #StanState -> ^m) =
        for target,input in set do
            (mem state).Reset()
            let (hits,r),_ = cost target input state |> extractor
            accumulated_cost <- accumulated_cost + r.P.Value.Value

            if is_inference_only state = true then
                if (tape state).Count > 0 then
                    failwith "Forgot to use the is_inference_only flag in a library function somewhere"
                if test_accuracy then 
                    accuracy <- accuracy + hits.Value
                    max_accuracy <- max_accuracy + (size target)
            else
                r.A := 1.0f
                let tape = tape state
                while tape.Count > 0 do
                    tape.Pop()
                    |> fun (name,func) -> func()
                update (optimizer, state) network
        (accuracy, max_accuracy), accumulated_cost / float32 set.Length
    if test_delayed then 
        /// Generic extractor used by train' and infer' functions.
        let rec extract_eventually =
            function
            | Done x -> x
            | NotYetDone x -> x() |> extract_eventually
        fun target input state -> runDelayedLayerWithCost cost_func network target (input,state)
        |> run' extract_eventually
    else runLayerWithCost cost_func network |> run' id
    
/// Trains the network in the depth direction.
let inline train network cost_func optimizer set state = run network cost_func optimizer set (with_is_inference_only state false) false true |> snd
/// Runs the network without doing backprop.
let inline infer network cost_func optimizer set state = run network cost_func optimizer set (with_is_inference_only state true) false true 
/// Trains the delayed network in the depth direction. As it runs it all the way to the end, this function is intended for testing purposes only.
let inline train' network cost_func optimizer set state = run network cost_func optimizer set (with_is_inference_only state false) true true  |> snd
/// Runs the delayed network without doing backprop. As it runs it all the way to the end, this function is intended for testing purposes only.
let inline infer' network cost_func optimizer set state = run network cost_func optimizer set (with_is_inference_only state true) true true 
/// Runs the network without doing backprop and without getting accuracy.
let inline inferCostOnly network cost_func optimizer set state = run network cost_func optimizer set (with_is_inference_only state true) false false |> snd
    
/// Creates the LayerWrapper type from the Layer. LayerWrapper type is the actual layer type.
let inline wrap l1 = 
    let network  = runLayer l1
    let network' = runLayer l1 |> trans 
    let nodes = [toArrayLazy l1]
    {RunLayer = network; RunDelayedLayer = network'; WrappedNodes = nodes}

/// Combines two LayerWrappers.
let inline combine (l1: LayerWrapper<_,_,_>) (l2: LayerWrapper<_,_,_>) =
    let network  = runLayer l1 >=> runLayer l2
    let network' = runDelayedLayer l1 ^< runDelayedLayer l2
    let nodes = l1.WrappedNodes @ l2.WrappedNodes
    {RunLayer = network; RunDelayedLayer = network'; WrappedNodes = nodes}

/// Combines two LayerWrappers.
let inline (^-) l1 l2 = combine l1 l2

/// Creates a feedforward layer with the specified hidden size and activation function. Has an optional flag whether bias should be created.
let inline FFLayer' has_bias hidden_size activation = createLayer hidden_size (createFFRec has_bias activation) |> wrap
/// Creates a 1D recurrent layer with the specified hidden size and activation function. Has an optional flag whether bias should be created.
let inline RNN1DLayer' has_bias hidden_size activation = 
    create1DRNNLayer hidden_size (createStdRNNRec has_bias activation) |> wrap

/// Creates a feedforward layer with the specified hidden size and activation function. Creates bias by default.
let inline FFLayer hidden_size activation = FFLayer' true hidden_size activation
/// Creates a 1D recurrent layer with the specified hidden size and activation function. Creates bias by default.
let inline RNN1DLayer hidden_size activation = RNN1DLayer' true hidden_size activation

/// Generic function for training and inference for 1D RNNs.
let inline private run1DRNN 
        (network: LayerWrapper< ^input,^output,RNN1DState>) 
        (cost_func: ^output -> ^output -> RNN1DState -> (Lazy<int> * Df) * RNN1DState)
        (optimizer: Optimizer)
        (set : (^input * ^output)[][]) // Note the extra array compared to the feedforward case. Here we are dealing with sequences. 
        (state: RNN1DState) 
        test_accuracy =
    let mutable accumulated_cost = 0.0f
    let mutable accuracy = 0
    let mutable max_accuracy = 0

    let cost = network |> runLayerWithCost cost_func

    for sequence in set do
        (mem state).Reset()
        let r =
            [|
            for iter=0 to sequence.Length-1 do
                setTimestep state iter
                let input, target = fst sequence.[iter], snd sequence.[iter]
                let (hits,r),_ = cost target input state
                accumulated_cost <- accumulated_cost + r.P.Value.Value

                if test_accuracy then 
                    accuracy <- accuracy + hits.Value
                    max_accuracy <- max_accuracy + (size target)

                yield r|]
            |> fun ar -> sum_scalars ar state |> fst

        if is_inference_only state = true then
            if (tape state).Count > 0 then
                failwith "Forgot to use the is_inference_only flag in a library function somewhere"
        else
            r.A := 1.0f
            let tape = tape state
            while tape.Count > 0 do
                tape.Pop()
                |> fun (name,func) -> func()
            update (optimizer, state) network
    (accuracy, max_accuracy), accumulated_cost / float32 set.Length
        
/// Trains the network in the depth direction.
let inline train1DRNN network cost_func optimizer set state = run1DRNN network cost_func optimizer set (with_is_inference_only state false) false |> snd
/// Runs the network without doing backprop.
let inline infer1DRNN network cost_func optimizer set state get_accuracy = run1DRNN network cost_func optimizer set (with_is_inference_only state true) get_accuracy 

/// Casts to DM
let inline castToDM x = (^a:(member CastToDM: DM) x)

// Auxiliary layers
let inline createBNRec _ (input: ^input) (state: ^state) =
    let Scale = input |> ghostCopyBias false |> setPrimal' (0.1f,state) // Initial scale value based on the Recurrent Batch Normalization paper by Cooijmans et al.
    let Bias = input |> ghostCopyBias false
    let RunningMean = input |> ghostCopyBias true
    let RunningVariance = input |> ghostCopyBias true
    //let mutable factor = 0.0
    {
    Run =
        fun (input: ^input) (state: ^state) ->
            let bnMode = ManagedCuda.CudaDNN.cudnnBatchNormMode.BatchNormPerActivation
            //factor <- factor + 1.0
            //(1.0/factor)
            batch_normalization_forward bnMode Scale Bias RunningMean RunningVariance 0.05 input state
    ToArray =
        [|Scale;Bias;RunningMean;RunningVariance|] |> Array.map castToDM
    }

/// Creates a feedforward layer with the specified hidden size and activation function. Has an optional flag whether bias should be created.
/// The empty parameter does nothing. It is just there because inline functions have to have a parameter in the current version of F#.
let inline BNLayer _ = createLayer 1 createBNRec |> wrap

/// An auxiliary function that takes in data in the form of [dataset][sequence][input * target (tupled)] where the first tuple array 
/// is the input and the other the target output, groups the sequences by their length, merges 
/// the sequences by the inner dimension, chunks them by minibatch_size and loads them to GPU.
/// The key in the resulting collection is the sequence length and the value fields are [minibatches_of_sequence_length_k][minibatch][sequence][input*target (tupled)]
let group_data_by_seq_length_and_load_to_gpu minibatch_size (data: (float32[] * float32[])[][]) =
    /// Assumes the array of array of arrays is of regular dimensions.
    let rotate_and_concat_3darray_and_load_to_gpu (dataset: float32[][][]) =
        // [dataset][sequence][sample] -> [sequence][dataset][sample] -> 
        // [sequence][minibatch][dataset,sample (merged)] -> [minibatch][sequence][dataset,sample (merged)]
        let dataset_length = dataset.Length
        let sequence_length = dataset.[0].Length
        let sample_length = dataset.[0].[0].Length

        // Checks the assumption.
        for sequence in dataset do
            if sequence.Length <> sequence_length then failwithf "sequence.Length(%i) <> dataset.[0].Length(%i)\nFailed the regularity check." sequence.Length sequence_length
            for sample in sequence do
                if sample.Length <> sample_length then failwithf "sample.Length(%i) <> dataset.[0].[0].Length(%i)\nFailed the regularity check." sample.Length sample_length

        Array.init sequence_length <| fun sequence_dim ->
            Array.init dataset_length <| fun dataset_dim ->
                dataset.[dataset_dim].[sequence_dim]
            |> Array.chunkBySize minibatch_size
            |> Array.map ( fun minibatch ->
                let input = Array.concat minibatch
                d2M.createConstant(sample_length,minibatch.Length,input))
        // [sequence][minibatch][dataset,sample (merged)] -> [minibatch][sequence][dataset,sample (merged)]
        |> fun sequences ->
            let number_of_minibatches = sequences.[0].Length
            Array.init number_of_minibatches <| fun minibatch_dim ->
                Array.init sequence_length <| fun sequence_dim ->
                    sequences.[sequence_dim].[minibatch_dim]
    
    data 
    |> Array.groupBy (fun x -> x.Length)
    |> Array.map (fun (k,v) -> 
        let inp = Array.map <| Array.map fst
        let lab = Array.map <| Array.map snd
        let f ex = rotate_and_concat_3darray_and_load_to_gpu << ex
        let v =
            Array.zip (f inp v) (f lab v)
            |> Array.map (fun (a,b) -> Array.zip a b)
        v)
