﻿[<AutoOpen>]
module SpiralV4.Combinators

open System
open ManagedCuda
open ManagedCuda.BasicTypes
open ManagedCuda.VectorTypes
open System.Collections.Generic

/// Creates an array of float32 zeroes with only index x set to 1.0f
let scalar_decoder min_bound max_bound (x: int option) = // .NET arrays can be created with min and max bounds specified.
    let size = max_bound - min_bound + 1
    if size <= 0 then failwith "size <= 0"
    let ar = Array.zeroCreate size
    try
        match x with
        | Some x -> ar.[x-min_bound] <- 1.0f
        | None -> ()
    with
        :? IndexOutOfRangeException -> 
            failwithf 
                "Index out of range exception in scalar_decoder.\nmin_bound=%i max_bound=%i size=%i x=%i"
                min_bound max_bound size x.Value
    ar

/// Creates an array of float32 zeroes with only index x set to 1.0f
let scalar_decoder' min_bound max_bound (x: int) = scalar_decoder min_bound max_bound (Some x)

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

/// Creates a feedforward layer with the specified hidden size and activation function. Has an optional flag whether bias should be created.
let inline FFLayer' has_bias hidden_size activation = createLayer hidden_size (createFFSubLayer has_bias activation)
/// Creates a 1D recurrent layer with the specified hidden size and activation function. Has an optional flag whether bias should be created.
let inline RNN1DLayer' has_bias hidden_size activation = 
    create1DRNNLayer hidden_size (createStdRNNSubLayer has_bias activation)

/// Creates a feedforward layer with the specified hidden size and activation function. Creates bias by default.
let inline FFLayer hidden_size activation = FFLayer' true hidden_size activation
/// Creates a 1D recurrent layer with the specified hidden size and activation function. Creates bias by default.
let inline RNN1DLayer hidden_size activation = RNN1DLayer' true hidden_size activation
        
/// Casts to DM
let inline castToDM x = (^a:(member CastToDM: DM) x)

type BNState =
    {
    BNDict : Dictionary<int, float>
    }

    member t.GetBNState = t
    
    member t.GetBNFactor(tag) = t.BNDict.[tag]
    member t.SetBNFactor(tag, v) = t.BNDict.[tag] <- v

    static member create = { BNDict = Dictionary() }

let inline getBNState (context: Context<_>) = (^userstate : (member GetBNState: BNState) context.UserState)
let inline getBNFactor (userstate: BNState) tag = userstate.GetBNFactor(tag)
let inline setBNFactor (userstate: BNState) tag v = userstate.SetBNFactor(tag,v)

/// It is trivial to combine BN and RNN1D States.
/// This is just an example. I do not indent to use it in the BN layer for the time being.
type BN_RNNState<'timestep> =
    {
    GetBNState : BNState
    GetRNNState : RNNState<'timestep>
    }

    static member create x =
        {
        GetBNState = BNState.create
        GetRNNState = RNNState<_>.create x
        }

// Auxiliary layers
let inline createBNSubLayer factor _ (input: ^input) (context: Context<_>) =
    let Scale = input |> ghostCopyBias false |> setPrimal' (0.1f,context) // Initial scale value based on the Recurrent Batch Normalization paper by Cooijmans et al.
    let Bias = input |> ghostCopyBias false
    let RunningMean = input |> ghostCopyBias true
    let RunningVariance = input |> ghostCopyBias true
//    let mutable factor = 0.0
    fun (input: ^input) (context: Context<_>) ->
        let bnMode = ManagedCuda.CudaDNN.cudnnBatchNormMode.BatchNormPerActivation
//            factor <- factor + 1.0
//            (1.0/factor)
        batch_normalization_forward bnMode Scale Bias RunningMean RunningVariance factor input context

/// Creates a BN layer using a constant factor. The running mean and variance are initialized to zero by default, as is bias.
/// The scale is set to 0.1 as per the Recurrent Batch Normalization paper by Cooijmans et al.
/// Factor values in the range of [0.01,0.1] work well for Mnist.
let inline BNLayer factor = createLayer 1 (createBNSubLayer factor)

/// A type use to apply the cost function of the same name. It is used as a substitute for higher order functions
/// which are insufficiently generic. It is the same as the Optimizer type in purpose.
type CostFunction =
    | SquaredError
    | CrossEntropy

    member inline t.ApplyCostFunction targets activations context =
        match t with
        | SquaredError -> squared_error_cost' targets activations context
        | CrossEntropy -> cross_entropy_cost' targets activations context

/// Generic function for net training and inference.
let inline private run
        (network: ^input -> Context<_> -> Cost) 
        (optimizer: Optimizer)
        (set : ^input []) 
        (context: Context<_>) 
        test_accuracy =
    Array.fold <| fun (accuracy,max_accuracy,accumulated_cost,iter) example ->
        (mem context).Reset()

        let hits,max_hits,r = network example context |> costAsTuple

        let accuracy, max_accuracy =
            if test_accuracy then
                accuracy + hits.Value, max_accuracy + max_hits
            else
                accuracy, max_accuracy

        let accumulated_cost = accumulated_cost + r.PrimalValue

        if is_inference_only context = true then
            if (tape context).Value.Length > 0 then
                failwith "Forgot to use the is_inference_only flag in a library function somewhere"
        else
            r.A := 1.0f
            
            let t = !(tape context)
            for name,func in t do
                func()

            (tape context) := []

            Seq.iter (Array.iter <| fun x -> optimizer.Optimize(x,context)) (nodes context)

        accuracy, max_accuracy, accumulated_cost, iter+1
    <| (0,0,0.0f,0) <| set
    |> fun (accuracy, max_accuracy, accumulated_cost, iter) ->
        ((accuracy, max_accuracy), accumulated_cost / float32 set.Length)

/// Trains the network in the depth direction.
let inline train network optimizer set context test_accuracy = 
    run network optimizer set (with_is_inference_only context false) test_accuracy
/// Runs the network without doing backprop.
let inline infer network optimizer set context test_accuracy = 
    run network optimizer set (with_is_inference_only context true) test_accuracy

/// Takes a standard net and wraps it so it takes in a recurrent sequence.
let inline recurrentRepeat
        (network: ^input -> Context<_> -> ^output)
        (sequence: ^input[])
        (context: Context<_>)
        : ^output[] =
    let len = sequence.Length
    let context = with_userstate 0 context
    (Array.zeroCreate len,0)
    |> Array.fold ( fun (output_ar,iter) example ->
        context.UserState <- iter // Sets the timestep.
        let output = network example context
        output_ar.[iter] <- output
        (output_ar, iter+1)
        ) <| sequence
    |> fst

/// Feds the selected output to itself for n times in a recurrent fashion. Accumulates interemediate results in an array.
let inline recurrentFeedback
        (len: int)
        (selector: ^output -> ^input)
        (network: ^input -> Context<_> -> ^output)
        (sequence: ^input)
        (context: Context<_>)
        : ^output[] =
    let context = with_userstate (BN_RNNState<_>.create 0) context
    let rec loop (output_ar: ^output[],iter,example) =
        if iter <= len-1 then
            set_timestep (getRNNState context) iter // Sets the timestep.
            let output = network example context
            output_ar.[iter] <- output
            if iter = len-1 then output_ar
            else loop (output_ar,iter+1,selector output)
        else output_ar
    loop (Array.zeroCreate len,0,sequence)

/// Maps the outputs to targets.
let inline wrapMap
        (map: ^output -> ^target)
        (network: ^input -> Context<_> -> ^output[])
        (input: ^input)
        (context: Context<_>)
        : ^target[] =
    network input context |> Array.map map

/// Maps the outputs to targets.
let inline wrapChoose
        (map: ^output -> ^target option)
        (network: ^input -> Context<_> -> ^output[])
        (input: ^input)
        (context: Context<_>)
        : ^target[] =
    network input context |> Array.choose map

/// Map for single outputs.
let inline wrapMap'
        (map: ^output -> ^target)
        (network: ^input -> Context<_> -> ^output)
        (input: ^input)
        (context: Context<_>)
        : ^target =
    network input context |> map

let inline wrapCostFunc
        (network: ^input -> Context<_> -> ^output)
        (cost_func: ^output -> ^output -> Context<_> -> Cost) 
        (input, target) context =
    network input context |> cost_func target <| context

let inline (==)
        (network: ^input -> Context<_> -> ^output)
        (cost_func: ^output -> ^output -> Context<_> -> Cost) =
    wrapCostFunc network cost_func

type CostAccumulator =
    {
    accuracy_ar : Lazy<int> []
    mutable max_accuracy : int
    cost_ar : Df []
    mutable iter : int
    }

    member inline t.SetTimeStepToIter(context: Context<_>) =
        set_timestep (getRNNState context) t.iter

    member t.AddCostAndIncrementIter(cost: Cost) =
        let hits,max_hits,cost = costAsTuple cost
        t.cost_ar.[t.iter] <- cost
        t.accuracy_ar.[t.iter] <- hits
        t.max_accuracy <- t.max_accuracy + max_hits
        t.iter <- t.iter+1

    static member create len =
        {accuracy_ar=Array.zeroCreate len;max_accuracy=0;cost_ar=Array.zeroCreate len;iter=0}

let inline add_cost_and_increment_iter (x: CostAccumulator) cost = x.AddCostAndIncrementIter cost; x

let sum_accumulated_costs context (x: CostAccumulator) =
    let accumulated_cost = sum_scalars x.cost_ar context
    let accuracy = lazy (x.accuracy_ar |> Array.fold (fun s x -> s+x.Value) 0)
    toCost' accuracy x.max_accuracy accumulated_cost

/// Takes a standard net and wraps it so it takes in a recurrent sequence.
let inline recurrectSequence
        (network: ^input -> Context<_> -> Cost)
        (sequence: ^input[])
        (context: Context<_>)
        : Cost =
    let context = with_userstate (BN_RNNState<_>.create 0) context
    (CostAccumulator.create sequence.Length)
    |> Array.fold ( fun accumulator example ->
        accumulator.SetTimeStepToIter context
        let output = network example context
        accumulator.AddCostAndIncrementIter(output)
        accumulator
        ) <| sequence
    |> sum_accumulated_costs context

/// Sums an array of costs in the output.
let inline recurrectCostSum
        (network: ^input -> Context<_> -> Cost[])
        (sequence: ^input)
        (context: Context<_>)
        : Cost =
    let costs = network sequence context
    Array.fold add_cost_and_increment_iter (CostAccumulator.create costs.Length) costs
    |> sum_accumulated_costs context

// The following modules are for RL so they've been placed here.

/// o <- max_col_index(x)
/// Gets the maximum indices of each column.
type DeviceMaxColumnIndexModule() = 
    let kernel_name = "MaxColumnIndexKernel"
    let kernel_code = 
        [|"
        //Kernel code:
        extern \"C\" {
            typedef float floatType;
            #define INIT __int_as_float(0xff800000) // The constant init for the reduce operations. This is float negative infinity.
            // The max reduce version.
            __device__ inline floatType warpReduce(floatType value){
                #pragma unroll
	            for (int i=1; i<32; i*=2) {
                    floatType tmp = __shfl_xor(value, i);
                    value = (tmp > value) ? tmp : value;
                    }
	            return value;
            }
              
            // Device code
            __global__ void ";kernel_name;"(const floatType* A, int* O, const int num_rows, const int num_cols)
            {
                int row = threadIdx.x;
                const int col = blockIdx.x;
                int col_idx = blockIdx.x*num_rows; 
                floatType max = INIT; // This is the negative infinity for floats.
                int index = -1;
                while (row < num_rows)
                {
                   if (A[row+col_idx] > max) {
                        max = A[row+col_idx];
                        index = row;
                        }
                    row += blockDim.x;
                }
                
                __shared__ floatType max_index;
                if (max == warpReduce(max)) max_index = index;
                __syncthreads();
                O[col] = max_index;
            }
        }

        "|] |> String.concat ""

    member val Kernel = load_kernel kernel_code kernel_name
    member inline t.A
            (str: CudaStream,
                (ext_a: ^a -> CUdeviceptr, a: ^a),
                o: CudaDeviceVariable<int>) = 
        let r,c = rc a
        let r' = int o.Size
        if c <> r' then failwithf "c(%i) <> r'(%i)" c r'
        max_column_launcher(str, t.Kernel, r, c, [|ext_a a; o.DevicePointer; r; c|])

/// o <- gather(indices,x)
/// Gathers the columns from x given the indices to o.
type DeviceGatherIndexModule() = 
    let kernel_name = "GatherIndexKernel"
    let kernel_code = 
        [|"//Kernel code:
        extern \"C\" {
            typedef float floatType;
              
            // Device code
            __global__ void ";kernel_name;"(const int* Indices, const floatType* A, floatType* O, const int num_rows, const int num_cols)
            {
                //int* er = NULL; // For triggering an error on illegal access.
                int row = threadIdx.x;
                const int col = blockIdx.x;
                const int col_idx = col*num_rows; 
                while (row < num_rows)
                {
                    const int index = Indices[col];
                    //if (index < 0 || index >= num_cols) *er = 555; // Triggers a illegal instruction error on an out of bounds access.
                    const int A_idx = index*num_rows;
                    O[row+col_idx] = A[row+A_idx];
                    row += blockDim.x;
                }
                
            }
        }

        "|] |> String.concat ""

    member val Kernel = load_kernel kernel_code kernel_name
    member inline t.A
            (str: CudaStream,
                indices: CudaDeviceVariable<int>,
                (ext_a: ^a -> CUdeviceptr, a: ^a),
                (ext_o: ^a -> CUdeviceptr, o: ^a)) = 
        let size_indices = int indices.Size
        let r,c = rc a
        let r',c' = rc o
        if c' <> size_indices then failwithf "c'(%i) <> size_indices(%i)" c' size_indices
        if r <> r' then failwithf "r(%i) <> r'(%i)" r r'
        max_column_launcher(str, t.Kernel, r, size_indices, [|indices.DevicePointer; ext_a a; ext_o o; r; c|])

type d3M =
    {
    mutable rcn : int * int*int
    mutable diff: AutoDiffType
    }

    static member private size_rcn (row,col,img) = row*col*img

    static member create' (size : (int * int * int)) =
        let is_constant = false
        let diff = 
            let s = d3M.size_rcn size
            match is_constant with
            | true -> AutoDiffType.CreatePrimalOnly s
            | false -> AutoDiffType.CreatePA s
        {rcn = size; diff = diff}

    static member create' (size : (int * int * int), host_data : float32[]) =
        let t = d3M.create'(size)
        t.diff.CopyToPrimal(host_data)
        t

    /// Number of rows
    member t.Rows = t.rcn |> fun (r,c,n) -> r
    /// Number of columns
    member t.Columns = t.rcn |> fun (r,c,n) -> c
    /// Number of images
    member t.Images = t.rcn |> fun (r,c,n) -> n
    /// Returns whether the function has an adjoint
    member t.HasAdjoint = t.diff.HasAdjoint
    /// Returns the size of matrix
    member t.Size = d3M.size_rcn t.rcn
    /// Get Adjoint Pointer
    member t.GAP = t.diff.A.DevicePointer
    /// Get Primal Pointer
    member t.GPP = t.diff.P.DevicePointer
    /// Get Adjoint Variable
    member t.GAV = t.diff.A
    /// Get Primal Variable
    member t.GPV = t.diff.P

    /// Throws an exception if the sizes are not all equal
    static member GuardSizes(x:d3M, o: d3M) =
        if x.rcn <> o.rcn then failwithf "x.rcn(%A) <> o.rcn(%A)" x.rcn o.rcn

    /// Throws an exception if the sizes are not all equal
    static member GuardSizes(x:d3M, y:d3M, o: d3M) =
        if x.rcn <> y.rcn then failwithf "x.rcn(%A) <> y.rcn(%A)" x.rcn y.rcn
        if y.rcn <> o.rcn then failwithf "y.rcn(%A) <> o.rcn(%A)" y.rcn o.rcn

    /// Throws an exception if the sizes are not all equal
    static member GuardSizes(x:d3M, y:d3M, z: d3M, o: d3M) =
        if x.rcn <> y.rcn then failwithf "x.rcn(%A) <> y.rcn(%A)" x.rcn y.rcn
        if y.rcn <> z.rcn then failwithf "y.rcn(%A) <> z.rcn(%A)" y.rcn z.rcn
        if z.rcn <> o.rcn then failwithf "z.rcn(%A) <> o.rcn(%A)" z.rcn o.rcn

    interface IDisposable with
        member t.Dispose() = t.diff |> dispose

/// o <- gather(indices,x)
/// Gathers the columns from x given the indices to o.
type DeviceGatherIndex3DModule() = 
    let kernel_name = "GatherIndex3DKernel"
    let kernel_code = 
        [|"//Kernel code:
        extern \"C\" {
            typedef float floatType;
              
            // Device code
            __global__ void ";kernel_name;"(const int* Indices, const floatType* A, floatType* O, const int num_rows, const int num_cols)
            {
                int row = threadIdx.x;
                const int col = blockIdx.x;
                const int col_idx = col*num_rows; 
                while (row < num_rows)
                {
                    const int index = Indices[col];
                    const int A_col_idx = index*num_rows;
                    const int A_img_idx = col*num_rows*num_cols;
                    O[row+col_idx] = A[row+A_col_idx+A_img_idx];
                    row += blockDim.x;
                }
                
            }
        }

        "|] |> String.concat ""

    member val Kernel = load_kernel kernel_code kernel_name
    member inline t.A
            (str: CudaStream,
                indices: CudaDeviceVariable<int>,
                (ext_a: d3M -> CUdeviceptr, a: d3M),
                (ext_o: ^a -> CUdeviceptr, o: ^a)) = 
        let size_indices = int indices.Size
        let r,c,_ = a.rcn
        let r',c' = rc o
        if c' <> size_indices then failwithf "c'(%i) <> size_indices(%i)" c' size_indices
        if r <> r' then failwithf "r(%i) <> r'(%i)" r r'
        max_column_launcher(str, t.Kernel, r, size_indices, [|indices.DevicePointer; ext_a a; ext_o o; r; c|])


let maxColumnIndexModule = lazy DeviceMaxColumnIndexModule()
let gatherIndexModule = lazy DeviceGatherIndexModule()
let gatherIndex3DModule = lazy DeviceGatherIndex3DModule()

/// Given an input matrix and an index mapping matrix, it gets the max row indices of the input array and then maps 
/// them to the columns of the index mapping matrix.
let map_indices (input: d2M) (map: d2M) (context: Context<_>) =
    if rows input <> cols map then failwithf "rows input(%i) <> cols map(%i)" (rows input) (cols map)
    let indices = (workspace context).ResizeIf<int>(cols input)
    maxColumnIndexModule.Value.A(str context, P input, indices)
    let output = getd2M true (rows map, cols input) context
    gatherIndexModule.Value.A(str context, indices, P map, P output)
    output

let inline images x = (^a : (member Images: int) x)

/// Given an input matrix and a 3D index mapping matrix, it gets the max row indices of the input array and then maps 
/// them to the columns and images of the 3D index mapping matrix.
let map_indices_3d (input: d2M) (map: d3M) (context: Context<_>) =
    if rows input <> cols map then failwithf "rows input(%i) <> cols map(%i)" (rows input) (cols map)
    if cols input <> images map then failwithf "cols input(%i) <> images map(%i)" (cols input) (images map)
    let indices = (workspace context).ResizeIf<int>(cols input)
    maxColumnIndexModule.Value.A(str context, P input, indices)
    let output = getd2M true (rows map, cols input) context
    gatherIndex3DModule.Value.A(str context, indices, P map, P output)
    output

/// Y[slice] <- beta * Y[slice] + alpha * X[slice]
type DeviceAddSliceModule() = 
    let block_size = 256
    
    let kernel_name = "AddSliceKernel"
    let kernel_code = 
        [|"//Kernel code:
        extern \"C\" {
            typedef float floatType;
            __global__ void ";kernel_name;"(
                    const int start_row_x, const int start_col_x, const int num_rows_x, const int num_cols_x, floatType alpha, const floatType* X,
                    const int start_row_y, const int start_col_y, const int num_rows_y, const int num_cols_y, floatType beta, floatType* Y,
                    const int row_stride, const int col_stride){
                const int stride = blockDim.x * gridDim.x;
                int i = threadIdx.x+blockIdx.x*blockDim.x;
                while (1) {
                    const int row_i = i % row_stride;
                    const int col_i = i / row_stride;
                        
                    const int row_x = start_row_x+row_i;
                    const int col_x = start_col_x+col_i;
                    const int idx_x = row_x+col_x*num_rows_x;

                    const int row_y = start_row_y+row_i;
                    const int col_y = start_col_y+col_i;
                    const int idx_y = row_y+col_y*num_rows_y;

                    if (row_i < row_stride && col_i < col_stride) {
                        Y[idx_y] = beta * Y[idx_y] + alpha * X[idx_x];
                        i += stride;
                    } else return;
                }
            }
        }

        "|] |> String.concat ""

    member val Kernel = load_kernel kernel_code kernel_name

    /// Zero based indexing.
    member t.A(str: CudaStream, 
                start_row_x, start_col_x, alpha: float32, (ext_x: d2M -> CUdeviceptr, x: d2M), 
                start_row_y, start_col_y, beta: float32, (ext_y: d2M -> CUdeviceptr, y: d2M), 
                row_stride, col_stride) =
        if start_row_x < 0 || start_col_x < 0 then failwithf "start_row_x(%i) < 0 || start_col_x(%i) < 0" start_row_x start_col_x
        if start_row_y < 0 || start_col_y < 0 then failwithf "start_row_y(%i) < 0 || start_col_y(%i) < 0" start_row_y start_col_y

        let end_row_x = start_row_x+row_stride-1
        let end_col_x = start_col_x+col_stride-1
        let end_row_y = start_row_y+row_stride-1
        let end_col_y = start_col_y+col_stride-1

        if end_row_x >= x.Rows || end_col_x >= x.Columns then 
            failwithf "end_row(%i) >= x.Rows(%i) || end_col_x(%i) >= x.Columns(%i)" end_row_x x.Rows end_col_x x.Columns
        if end_row_y >= y.Rows || end_col_y >= y.Columns then 
            failwithf "end_row_y(%i) >= y.Rows(%i) || end_col_y(%i) >= y.Columns(%i)" end_row_y y.Rows end_col_y y.Columns
        
        let n = row_stride*col_stride
        let gridSize = divup n block_size
        t.Kernel.GridDimensions <- dim3(gridSize)
        t.Kernel.BlockDimensions <- dim3(block_size)
        t.Kernel.RunAsync(str.Stream, start_row_x, start_col_x, x.Rows, x.Columns, alpha, ext_x x, start_row_y, start_col_y, y.Rows, y.Columns, beta, ext_y y, row_stride, col_stride)


// The Item and GetSlice operators. Column major
let addSliceModule = lazy DeviceAddSliceModule()

let add_slice_backward_name = "add_slice_backward"

/// Y[rowStartY..rowStartY+row_stride-1,colStartY..colStartY+col_stride-1] 
/// += alpha * 
/// X[rowStartX..rowStartX+row_stride-1,colStartX..colStartX+col_stride-1]
let add_slice (rowStartX: int) (colStartX: int) (alpha: float32) (X: d2M)
              (rowStartY: int) (colStartY: int) (beta: float32) (Y: d2M)
              (row_stride: int) (col_stride: int) (context: Context<_>) =
    addSliceModule.Value.A(str context,rowStartX,colStartX,alpha,P X,rowStartY,colStartY,beta,P Y,row_stride,col_stride)

    if (is_inference_only context) = false then
        if hasAdjoint X && hasAdjoint Y then
            let add_slice_backward () = 
                deadness_check Y X
                <| fun _ -> 
                    addSliceModule.Value.A(str context,rowStartY,colStartY,alpha,A Y,
                                                     rowStartX,colStartX,1.0f,A X,
                                                     row_stride,col_stride)
            push_tape context (add_slice_backward_name,add_slice_backward)

/// Stacks A and B along the row dimension.
let stack_vertical (A: d2M) (B: d2M) (context: Context<_>) =
    if cols A <> cols B then failwithf "cols A(%i) <> cols B(%i)" (cols A) (cols B)
    let cols = cols A
    let C = d2M.create(rows A + rows B, cols)
    add_slice 0 0 1.0f A 0 0 0.0f C (rows  A) cols context
    add_slice 0 0 1.0f B (rows A) 0 0.0f C (rows B) cols context
    C

let stack_vertical_lazy (A: d2M) (B: d2M) (context: Context<_>) = 
    lazy stack_vertical A B context

/// Adds the two input arguments together.
let inline ResidualLayer (a, b) context =
    match a with
    | Some a -> add 1.0f a 1.0f b context
    | None -> b

let inline force (x: Lazy<_>) = x.Value

let inline createGRUSublayer has_bias activation desired_hidden_size (input: d2M) (context: Context<_>) =
    let [|W_u;W_r;W_n|] as ar1 = Array.init 3 (fun _ -> d2M.create(desired_hidden_size,input.Rows) |> reluInitializer context)
    let [|U_u;U_r;U_n|] as ar2 = Array.init 3 (fun _ -> d2M.create(desired_hidden_size,desired_hidden_size) |> reluInitializer context)
    let [|b_u;b_r;b_n|] as ar3 = Array.init 3 (fun _ -> if has_bias then d2M.create(desired_hidden_size,1) |> reluInitializer context |> Some else None)

    [| ar1; ar2; Array.choose id ar3 |]
    |> Array.collect (Array.map D2M)
    |> add_nodes context

    fun (y,x) (context: Context<_>) -> 
        match y with
        | Some y ->
            let update_gate = linear_layer_matmult [|W_u,x;U_u,y|] b_u >>= sigmoid <| context
            let reset_gate = linear_layer_matmult [|W_r,x;U_r,y|] b_r >>= sigmoid <| context
            let potential_new_state = linear_layer_matmult [|W_n,x;U_n, (hadmult reset_gate y context)|] b_n >>= activation <| context
            linear_layer_hadmult [|update_gate,y;(scalar_matrix_add 1.0f -1.0f update_gate context),potential_new_state|] <| context
        | None ->
            let update_gate = linear_layer_matmult [|W_u,x|] b_u >>= sigmoid <| context
            let reset_gate = linear_layer_matmult [|W_r,x|] b_r >>= sigmoid <| context
            let potential_new_state = linear_layer_matmult [|W_n,x|] b_n >>= activation <| context
            linear_layer_hadmult [|scalar_matrix_add 1.0f -1.0f update_gate context, potential_new_state|] <| context
        
let inline createLSTMSublayer has_bias activation_for_block_input activation_for_block_output desired_hidden_size (input: d2M) (context: Context<_>) =
    let [|W_z;W_i;W_f;W_o|] as ar1 = Array.init 4 (fun _ -> d2M.create(desired_hidden_size,input.Rows) |> reluInitializer context)
    let [|U_z;U_i;U_f;U_o|] as ar2 = Array.init 4 (fun _ -> d2M.create(desired_hidden_size,desired_hidden_size) |> reluInitializer context)
    let [|P_i;P_f;P_o|] as ar3 = Array.init 4 (fun _ -> d2M.create(desired_hidden_size,desired_hidden_size) |> reluInitializer context)
    let [|b_z;b_i;b_f;b_o|] as ar4 = 
        let relu_ini = reluInitializer context
        // The forget gate bias gets initialized to 1.0f for easier gradient propagation.
        // It might be worth trying the same thing with the input gate to the GRU.
        let forget_bias_ini (x: d2M) = x.SetPrimal(1.0f, str context); x
        [|relu_ini;relu_ini;forget_bias_ini;relu_ini|] 
        |> Array.map (fun ini -> if has_bias then d2M.create(desired_hidden_size,1) |> ini |> Some else None)

    [| ar1; ar2; ar3; Array.choose id ar4 |]
    |> Array.collect (Array.map D2M)
    |> add_nodes context

    fun (y,x) (context: Context<_>) -> 
        match y with
        | Some (y, c) ->
            let block_input = linear_layer_matmult [|W_z,x;U_z,y|] b_z >>= activation_for_block_input <| context
            let input_gate = linear_layer_matmult [|W_i,x;U_i,y;P_i,c|] b_i >>= sigmoid <| context
            let forget_gate = linear_layer_matmult [|W_f,x;U_f,y;P_f,c|] b_f >>= sigmoid <| context
            let c' = linear_layer_hadmult [|block_input,input_gate;c,forget_gate|] context
            let output_gate = linear_layer_matmult [|W_o,x;U_o,y;P_o,c'|] b_o >>= sigmoid <| context
            hadmult (activation_for_block_output c' context) output_gate context, c'
        | None ->
            let block_input = linear_layer_matmult [|W_z,x|] b_z >>= activation_for_block_input <| context
            let input_gate = linear_layer_matmult [|W_i,x|] b_i >>= sigmoid <| context
            // In nearly forgot that forgetting to remove the forget gate would trigger the deadness check.
            let c' = linear_layer_hadmult [|block_input,input_gate|] context
            let output_gate = linear_layer_matmult [|W_o,x;P_o,c'|] b_o >>= sigmoid <| context
            hadmult (activation_for_block_output c' context) output_gate context, c'