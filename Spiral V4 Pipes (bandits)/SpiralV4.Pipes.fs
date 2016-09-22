[<AutoOpen>]
module SpiralV4.Pipes

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
type LayerWrapper<'output,'state when 'state :> StanState > =
    {
    RunLayer: 'output * 'state
    WrappedNodes: Lazy<DM[]> list
    }

    member inline t.Update(optimizer,state) =
        t.WrappedNodes |> List.iter (fun x -> x.Value |> Array.iter (optimize(optimizer,state)))

    interface IDisposable with
        member t.Dispose() =
            t.WrappedNodes |> List.iter (fun x -> x.Value |> Array.iter dispose)

/// Creates the LayerWrapper type from the Layer. LayerWrapper type is the actual layer type.
let inline wrap (l1: Layer<_,_,_>) input state = 
    {RunLayer = l1.RunLayer input state; WrappedNodes = [toArrayLazy l1]}

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
let inline BNLayer() = createLayer 1 createBNRec |> wrap

/// The left associative bind for LayerWrappers.
let inline (--) (a: 'state -> LayerWrapper<'a,'state>) (b: 'a -> 'state -> LayerWrapper<'b,'state>) s =
    let a = a s
    let a',s = a.RunLayer
    let b = b a' s
    {RunLayer=b.RunLayer
     WrappedNodes=a.WrappedNodes @ b.WrappedNodes}

/// Combines two LayerWrappers.
let inline combine (l1: 'a -> 'state ->LayerWrapper<'b,'state>) (l2: 'b -> 'state ->LayerWrapper<'c,'state>) a s =
    let l1 = l1 a s
    let a,s = l1.RunLayer
    let l2 = l2 a s
    {RunLayer=l2.RunLayer; WrappedNodes=l1.WrappedNodes @ l2.WrappedNodes}

/// Combines two LayerWrappers.
let inline (^-) l1 l2 = combine l1 l2

/// The monadic return for the LayerWrapper.
let inline layerWrapperReturn a s =
    {
    RunLayer = a,s
    WrappedNodes = []
    }

type LayerWrapperBuilder() =
    member inline __.Return(v) = layerWrapperReturn v
    member inline __.ReturnFrom(v) = v
    member inline __.Bind(a,f) = a -- f

let layers = LayerWrapperBuilder()

/// Layer that returns lazy accuracy and cost.
let inline CrossEntropyLayer target input state =
    {RunLayer=cross_entropy_cost' target input state; WrappedNodes=[]}
/// Layer that returns lazy accuracy and cost.
let inline SquaredErrorLayer target input state =
    {RunLayer=squared_error_cost' target input state; WrappedNodes=[]}

/// Generic function for feedforward net training and inference.
let inline private run
        (network: ^target -> ^input -> ^state -> LayerWrapper< (Lazy<int> * Df),^state>) 
        (optimizer: Optimizer)
        (set : (^input * ^target)[]) 
        (state: #StanState) 
        test_accuracy =
    Array.fold <| fun (accuracy,max_accuracy,accumulated_cost,num_cols,_) (input,target) ->
        (mem state).Reset()
        let fin = network target input state
        let (hits,r),_ = fin.RunLayer

        let accuracy, max_accuracy =
            if test_accuracy then
                accuracy + hits.Value, max_accuracy + (size target)
            else
                accuracy, max_accuracy

        let num_cols = num_cols + cols target // Cost rescaling so the division at the end gives the correct average.
        let accumulated_cost = accumulated_cost + r.P.Value.Value * (cols target |> float32)

        if is_inference_only state = true then
            if (tape state).Count > 0 then
                failwith "Forgot to use the is_inference_only flag in a library function somewhere"
        else
            r.A := 1.0f
            let tape = tape state
            while tape.Count > 0 do
                tape.Pop()
                |> fun (name,func) -> func()
            update (optimizer, state) fin
        accuracy, max_accuracy, accumulated_cost, num_cols , Some fin
    <| (0,0,0.0f,0,None) <| set
    |> fun (accuracy, max_accuracy, accumulated_cost, num_cols, fin) ->
        ((accuracy,max_accuracy),accumulated_cost / float32 num_cols,fin)

/// Trains the network in the depth direction.
let inline train (network: ^target -> ^input -> ^state -> _) optimizer set state test_accuracy = 
    run network optimizer set (with_is_inference_only state false) test_accuracy
/// Runs the network without doing backprop.
let inline infer (network: ^target -> ^input -> ^state -> _) optimizer set state test_accuracy = 
    run network optimizer set (with_is_inference_only state true) test_accuracy

/// Generic function for training and inference for 1D RNNs.
let inline private runRNN 
        (network: ^target -> ^input -> ^state -> LayerWrapper< (Lazy<int> * Df),^state>) 
        (optimizer: Optimizer)
        (set : (^input * ^target)[][]) 
        (state: ^state) 
        test_accuracy =
    Array.fold <| fun (accuracy,max_accuracy,accumulated_cost,num_cols,_) sequence ->
        Array.fold <| fun (accuracy,max_accuracy,accumulated_cost,num_cols,_,l,iter) (input,target) ->
            setTimestep state iter
            let fin = network target input state
            let (hits,r),_ = fin.RunLayer

            let accuracy, max_accuracy =
                if test_accuracy then
                    accuracy + hits.Value, max_accuracy + (size target)
                else
                    accuracy, max_accuracy

            (accuracy,max_accuracy,accumulated_cost,num_cols,Some fin,r::l,iter+1)
        <| (accuracy,max_accuracy,accumulated_cost,num_cols,None,[],0) <| sequence
        |> fun (accuracy,max_accuracy,accumulated_cost,num_cols,fin,l,iter) ->
            let r = sum_scalars l state |> fst

            let num_cols = num_cols + (sequence.[0] |> snd |> cols) // Cost rescaling so the division at the end gives the correct average.
            // The fact that I added the costs in the upper layer is probably the reason why I did not see any gain from concurrency in Spiral V3. Damn.
            // But it just goes to show how hard testing for it is. If it was not that, it would be something else.
            // More than anything, to make proper use of concurrency, a proper foundation is necessary.
            let accumulated_cost = accumulated_cost + r.P.Value.Value * (sequence.[0] |> snd |> cols |> float32)

            if is_inference_only state = true then
                if (tape state).Count > 0 then
                    failwith "Forgot to use the is_inference_only flag in a library function somewhere"
            else
                r.A := 1.0f
                let tape = tape state
                while tape.Count > 0 do
                    tape.Pop()
                    |> fun (name,func) -> func()
                update (optimizer, state) fin.Value
            (accuracy,max_accuracy,accumulated_cost,num_cols,fin)
    <| (0,0,0.0f,0,None) <| set
    |> fun (accuracy, max_accuracy, accumulated_cost, num_cols, fin) ->
        ((accuracy,max_accuracy),accumulated_cost / float32 num_cols,fin)

/// Trains the network in the depth direction.
let inline train1DRNN (network: ^target -> ^input -> ^state -> _) optimizer set state test_accuracy = 
    runRNN network optimizer set (with_is_inference_only state false) test_accuracy
/// Runs the network without doing backprop.
let inline infer1DRNN (network: ^target -> ^input -> ^state -> _) optimizer set state test_accuracy = 
    runRNN network optimizer set (with_is_inference_only state true) test_accuracy