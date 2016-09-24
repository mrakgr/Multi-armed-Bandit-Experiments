[<AutoOpen>]
module SpiralV4.Combinators

open System

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

/// The type for combining layers. Can be used to combine RNN with feedfoward layers.
type LayerWrapper<'input,'output,'state when 'state :> StanState > =
    {
    RunLayer: 'input -> 'state -> 'output * 'state
    WrappedNodes: Lazy<DM[]> list
    }

    member inline t.Update(optimizer,state) =
        t.WrappedNodes |> List.iter (fun x -> x.Value |> Array.iter (optimize(optimizer,state)))

    interface IDisposable with
        member t.Dispose() =
            t.WrappedNodes |> List.iter (fun x -> x.Value |> Array.iter dispose)

/// Creates the LayerWrapper type from the Layer. LayerWrapper type is the actual layer type.
let inline wrap (l1: Layer<_,_,_>) = 
    {RunLayer = (fun input state -> l1.RunLayer input state); WrappedNodes = [toArrayLazy l1]}

/// Creates a feedforward layer with the specified hidden size and activation function. Has an optional flag whether bias should be created.
let inline FFLayer' has_bias hidden_size activation = createLayer hidden_size (createFFRec has_bias activation) |> wrap
/// Creates a 1D recurrent layer with the specified hidden size and activation function. Has an optional flag whether bias should be created.
let inline RNN1DLayer' has_bias hidden_size activation = 
    create1DRNNLayer hidden_size (createStdRNNRec has_bias activation) |> wrap

/// Creates a feedforward layer with the specified hidden size and activation function. Creates bias by default.
let inline FFLayer hidden_size activation = FFLayer' true hidden_size activation
/// Creates a 1D recurrent layer with the specified hidden size and activation function. Creates bias by default.
let inline RNN1DLayer hidden_size activation = RNN1DLayer' true hidden_size activation
        
/// Casts to DM
let inline castToDM x = (^a:(member CastToDM: DM) x)

// Auxiliary layers
let inline createBNRec _ (input: ^input) (state: ^state) =
    let Scale = input |> ghostCopyBias false |> setPrimal' (0.1f,state) // Initial scale value based on the Recurrent Batch Normalization paper by Cooijmans et al.
    let Bias = input |> ghostCopyBias false
    let RunningMean = input |> ghostCopyBias true
    let RunningVariance = input |> ghostCopyBias true
//    let mutable factor = 0.0
    {
    Run =
        fun (input: ^input) (state: ^state) ->
            let bnMode = ManagedCuda.CudaDNN.cudnnBatchNormMode.BatchNormPerActivation
//            factor <- factor + 1.0
//            (1.0/factor)
            batch_normalization_forward bnMode Scale Bias RunningMean RunningVariance 0.01 input state
    ToArray =
        [|Scale;Bias;RunningMean;RunningVariance|] |> Array.map castToDM
    }

/// Creates a feedforward layer with the specified hidden size and activation function. Has an optional flag whether bias should be created.
/// The empty parameter does nothing. It is just there because inline functions have to have a parameter in the current version of F#.
let inline BNLayer() = createLayer 1 createBNRec |> wrap

/// Kleisli arrow for the state passing functions
let inline (>=>) a b = fun x -> a x >>= b

/// Combines two LayerWrappers.
let inline combine (l1: LayerWrapper<'a,'b,'state>) (l2: LayerWrapper<'b,'c,'state>) =
    {RunLayer=l1.RunLayer >=> l2.RunLayer; WrappedNodes=l1.WrappedNodes @ l2.WrappedNodes}

/// Combines two LayerWrappers.
let inline (^-) l1 l2 = combine l1 l2

/// Combines the two LayerWrappers while accumulating the intermediate results.
let stitch (l1: LayerWrapper<'a,'b,'state>) (l2: LayerWrapper<'b,'c,'state>): LayerWrapper<'a,'b*'c,'state> =
    let runLayer input state =
        let a,s = l1.RunLayer input state
        let b,s = l2.RunLayer a s
        (a,b),s
    {RunLayer=runLayer; WrappedNodes=l1.WrappedNodes @ l2.WrappedNodes}

/// Combines the two LayerWrappers while accumulating the intermediate results.
let inline (^+) l1 l2 = stitch l1 l2

/// (accuracy * max_accuracy) * cost
type Cost = (Lazy<int> * int) * Df 

/// A type use to apply the cost function of the same name. It is used as a substitute for higher order functions
/// which are insufficiently generic. It is the same as the Optimizer type in purpose.
type CostFunction =
    | SquaredError
    | CrossEntropy

    member inline t.ApplyCostFunction targets activations state =
        match t with
        | SquaredError -> squared_error_cost' targets activations state
        | CrossEntropy -> cross_entropy_cost' targets activations state

// To make the following function generic I would need a polymorphic map over tuples from FsControl.
/// Wraps a layer with two targets in the cross entropy cost.
let inline wrapStitchedCost (cost_func: CostFunction) (network: LayerWrapper<'a,'b*'c,'state>) =
    let addDf (a: Df) (b: Df) =
        {P= ref (lazy (a.PrimalValue + b.PrimalValue)); A= ref 0.0f}
    let addLazy (a: Lazy<int>) (b: Lazy<int>) =
        lazy (a.Value + b.Value)

    let runLayer (input, (target1, target2)) state =
        let (output1,output2),state = network.RunLayer input state
        let ((l1,mac1),r1),_ = cost_func.ApplyCostFunction target1 output1 state
        let ((l2,mac2),r2),state = cost_func.ApplyCostFunction target2 output2 state
        ((addLazy l1 l2, mac1+mac2), addDf r1 r1), state
    
    {RunLayer=runLayer; WrappedNodes=network.WrappedNodes}

/// Generic function for net training and inference.
let inline private run
        (network: LayerWrapper< ^input, Cost, ^state>) 
        (optimizer: Optimizer)
        (set : ^input []) 
        (state: #StanState) 
        test_accuracy =
    Array.fold <| fun (accuracy,max_accuracy,accumulated_cost) example ->
        (mem state).Reset()
        let ((hits,max_hits),r),_ = network.RunLayer example <| state

        let accuracy, max_accuracy =
            if test_accuracy then
                accuracy + hits.Value, max_accuracy + max_hits
            else
                accuracy, max_accuracy

        let accumulated_cost = accumulated_cost + r.PrimalValue

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

        accuracy, max_accuracy, accumulated_cost
    <| (0,0,0.0f) <| set
    |> fun (accuracy, max_accuracy, accumulated_cost) ->
        ((accuracy, max_accuracy), accumulated_cost / float32 set.Length)

/// Trains the network in the depth direction.
let inline train network optimizer set state test_accuracy = 
    run network optimizer set (with_is_inference_only state false) test_accuracy
/// Runs the network without doing backprop.
let inline infer network optimizer set state test_accuracy = 
    run network optimizer set (with_is_inference_only state true) test_accuracy

/// Takes a standard net and wraps it so it takes in a recurrent sequence.
let inline recurrentRepeat
        (network: LayerWrapper< ^input  , 'output  , ^state>)
                : LayerWrapper< ^input[], 'output[], ^state> =
    let runLayer (sequence: ^input[]) state =
        let len = sequence.Length   
        (Array.zeroCreate len,0,state) 
        |> Array.fold ( fun (output_ar,iter,state) example ->
            setTimestep state iter
            let output,state = network.RunLayer example state
            output_ar.[iter] <- output
            (output_ar,iter+1,state)
            ) <| sequence 
        |> fun (output_ar,iter,state) ->
            output_ar,state
    {RunLayer=runLayer; WrappedNodes=network.WrappedNodes}

/// Feds the selected output to itself for n times in a recurrent fashion. Accumulates interemediate results in an array.
let inline recurrentFeedback
        (len: int)
        (selector: ^output -> ^input)
        (network: LayerWrapper< ^input , ^output  , ^state>)
                : LayerWrapper< ^input , ^output[], ^state> =
    let runLayer (sequence: ^input) state =
        let rec loop (output_ar: ^output[],iter,state,example) =
            if iter < len then
                setTimestep state iter
                let output,state = network.RunLayer example state
                output_ar.[iter] <- output
                loop (output_ar,iter+1,state,selector output)
            else output_ar, state
        loop (Array.zeroCreate len,0,state,sequence)
    {RunLayer=runLayer; WrappedNodes=network.WrappedNodes}

/// Maps the outputs to targets.
let inline wrapMap
        (map: ^output -> ^target)
        (network: LayerWrapper< ^input, ^output[], ^state>)
                : LayerWrapper< ^input, ^target[], ^state> =
    let runLayer (input: ^input) state =
        let output,state = network.RunLayer input state
        output |> Array.map map, state
    {RunLayer=runLayer; WrappedNodes=network.WrappedNodes}

/// Folds the output into the target.
let inline wrapFold
        (fold: ^output -> ^state -> ^target * ^state)
        (network: LayerWrapper< ^input  , ^output  , ^state>)
                : LayerWrapper< ^input  , ^target  , ^state> =
    let runLayer (input: ^input) state =
        let output,state = network.RunLayer input >>= fold <| state
        output, state
    {RunLayer=runLayer; WrappedNodes=network.WrappedNodes}

let inline wrapCostFunc
        (network: LayerWrapper< ^input, ^output, ^state>) 
        (cost_func: ^output -> ^output -> ^state -> Cost * ^state) =
    let runLayer (input, target) state =
        let output,state = network.RunLayer input state
        cost_func target output state 
    { RunLayer = runLayer; WrappedNodes = network.WrappedNodes}

let inline (==)
        (network: LayerWrapper< ^input, ^output, ^state>) 
        (cost_func: ^output -> ^output -> ^state -> Cost * ^state) =
    wrapCostFunc network cost_func

/// Takes a standard net and wraps it so it takes in a recurrent sequence.
let inline recurrectSequence
        (network: LayerWrapper< ^input  , Cost, ^state>)
                : LayerWrapper< ^input[], Cost, ^state> =
    let runLayer (sequence: ^input[]) state =
        let len = sequence.Length
        (Array.zeroCreate len,0,Array.zeroCreate len,0) 
        |> Array.fold ( fun (accuracy_ar,max_accuracy,cost_ar,iter) example ->
            setTimestep state iter
            let ((hits,max_hits),cost),_ = network.RunLayer example <| state
            cost_ar.[iter] <- cost
            accuracy_ar.[iter] <- hits
            let max_accuracy = max_accuracy + max_hits
            (accuracy_ar,max_accuracy,cost_ar,iter+1)
            ) <| sequence
        |> fun (accuracy_ar,max_accuracy,l,iter) ->
            let accumulated_cost = sum_scalars l state |> fst
            let accuracy = lazy (accuracy_ar |> Seq.fold (fun s x -> s+x.Value) 0)
            ((accuracy,max_accuracy),accumulated_cost), state
    { RunLayer = runLayer; WrappedNodes = network.WrappedNodes}