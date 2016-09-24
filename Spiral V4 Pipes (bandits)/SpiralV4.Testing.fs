﻿namespace SpiralV4

module Embel =
    let reset_random_seed_on_each_runTestCase = true

    open System
    open System.IO
    open System.Diagnostics

    type Result =
        | Success
        | Failure of string

    type Tally =
        {
        mutable succeeded : int
        mutable failed : int
        mutable ignored : int
        mutable am_ignoring : bool
        mutable label : string
        time_elapsed : Stopwatch
        output : StreamWriter
        }

        static member create(output: StreamWriter) =
            {succeeded=0; failed=0; ignored=0; am_ignoring=false; label=""; time_elapsed=Stopwatch.StartNew(); output=output}
        static member create() =
            let x = Console.OpenStandardError() |> fun x -> new StreamWriter(x)
            x.AutoFlush <- true
            Tally.create x

        override t.ToString() =
            [|
            sprintf "Test tally: Succeeded: %i" t.succeeded
            sprintf "            Failed: %i" t.failed
            sprintf "            Ignored: %i" t.ignored
            sprintf "            Time Elapsed: %A" t.time_elapsed.Elapsed
            |] |> String.concat "\n"

        interface IDisposable with
            member t.Dispose() = t.output.Dispose()

    /// The internal function for running tests.
    let inline runTestCase (x: ^a) (name: string, code: ^a -> Result) (state: Tally) =
        if reset_random_seed_on_each_runTestCase then
            cudaRandom.SetOffset(0UL)
            cudaRandom.SetPseudoRandomGeneratorSeed(42UL)

        if state.am_ignoring then
            state.ignored <- state.ignored+1
            state
        else
            match code x with
            | Success -> state.succeeded <- state.succeeded+1
            | Failure mes -> 
                let sep = if String.IsNullOrEmpty state.label then null else "/"
                let mes = if String.IsNullOrEmpty mes = false then ": " + mes else null
                state.output.WriteLine(sprintf "Test %s%s%s failed%s" state.label sep name mes)
                state.failed <- state.failed+1
            state

    /// Applies a label to a test. Used internally by the other functions.
    let testLabel (label: string) (test: Tally -> Tally) (state: Tally) =
        let backup = state.label
        state.label <- 
            let x = if String.IsNullOrEmpty backup then "" else "/"
            String.concat null [|backup; x; label|]
        test state |> fun state -> state.label <- backup; state

    /// Tests a single test case
    let inline testCase label (setup: unit -> ^a) (test: (string * (^a -> Result))) =
        fun (state: Tally) -> 
            use x = setup () 
            runTestCase x test state
        |> testLabel label

    /// Tests an array of cases with the data from the setup function shared amongst the functions.
    let inline testCases (label: string) (setup: unit -> ^a) (tests: (string * (^a -> Result))[]) =
        fun (state: Tally) ->
            use x = setup ()
            Array.fold(fun state test -> 
                runTestCase x test state) state tests
        |> testLabel label

    /// Tests an array of cases with the data from the setup function instantiated separately for each function.
    let inline testCases' (label: string) (setup: unit -> ^a) (tests: (string * (^a -> Result))[]) =
        fun (state: Tally) -> Array.fold(fun state test -> use x = setup () in runTestCase x test state) state tests
        |> testLabel label

    /// Sequentially runs the array of tests
    let testArray (label: string) (tests: (Tally -> Tally)[]) =
        fun (state: Tally) -> Array.fold(fun state test -> test state) state tests
        |> testLabel label

    /// Ignores the selected tests.
    let testIgnore (test: (Tally -> Tally)) (state: Tally) =
        let backup = state.am_ignoring
        state.am_ignoring <- true
        test state |> fun state -> state.am_ignoring <- backup; state

    /// Turns the result of a boolean into the Result type.
    let assertTest (label: string) =
        function
        | true -> Success
        | false -> Failure label

    /// The main run function
    let run (tree: Tally -> Tally) =
        use tally = Tally.create() |> tree
        tally.ToString() |> tally.output.WriteLine // The final output

module private Testing =
    open System
    open System.IO
    open System.Collections.Generic

    open Embel
    open ManagedCuda
    open ManagedCuda.BasicTypes

    let size_gradcheck = 8

    type GradientCheckingData =
        {
        inputd2M: d2M
        outputd2M: d2M
        state: StanState
        }

        static member create() =
            let state = StanState()
            let inputd2M = d2M.create(size_gradcheck,2) |> fun x -> fillRandomUniformMatrix state.Str x 1.0f 0.0f; x
            let outputd2M = d2M.create(size_gradcheck,2) // Gets initialized to zero.
            {inputd2M = inputd2M; state = state; outputd2M=outputd2M}

        interface IDisposable with
            member t.Dispose() =
                t.inputd2M |> dispose
                t.state |> dispose

    type MnistData =
        {
        training_set : (d2M * d2M)[]
        test_set : (d2M * d2M)[]
        state : StanState
        }

        static member create minibatch_size mnist_path () =
            let load_mnist filename =
                use f = File.Open(filename, FileMode.Open, FileAccess.Read, FileShare.Read)
                use d = new BinaryReader(f)

                let magicnumber = d.ReadInt32() |> Net.IPAddress.NetworkToHostOrder
                match magicnumber with
                | 2049 -> // Labels
                    let n = d.ReadInt32() |> Net.IPAddress.NetworkToHostOrder
                    d.ReadBytes n
                    |> Array.collect (
                        fun x -> 
                            let t = Array.zeroCreate 10
                            t.[int x] <- 1.0f
                            t)
                    |> Array.chunkBySize (minibatch_size*10)
                    |> Array.map (fun x -> (10,x.Length/10,x) |> d2M.createConstant )
                | 2051 -> // Images
                    let n = d.ReadInt32() |> Net.IPAddress.NetworkToHostOrder
                    let rows = d.ReadInt32() |> Net.IPAddress.NetworkToHostOrder
                    let cols = d.ReadInt32() |> Net.IPAddress.NetworkToHostOrder
                    d.ReadBytes(n * rows * cols)
                    |> Array.map (fun x -> float32 x / 255.0f)
                    |> Array.chunkBySize (minibatch_size*rows*cols)
                    |> Array.map (fun x ->  (rows*cols,x.Length/(rows*cols),x) |> d2M.createConstant)
                | _ -> failwith "Given file is not in the MNIST format."

            let [|test_images;test_labels;train_images;train_labels|] = 
                [|"t10k-images.idx3-ubyte";"t10k-labels.idx1-ubyte";"train-images.idx3-ubyte";"train-labels.idx1-ubyte"|]
                |> Array.map (fun x -> Path.Combine(mnist_path,x) |> load_mnist)

            {
            training_set = Array.zip train_images train_labels
            test_set = Array.zip test_images test_labels
            state = StanState()
            }

        interface IDisposable with
            member t.Dispose() =
                for x,y in t.training_set do x |> dispose; y |> dispose
                for x,y in t.test_set do x |> dispose; y |> dispose
                t.state |> dispose

    type ReberData =
        {
        state : RNN1DState
        data : Dictionary<int, (d2M*d2M)[][]>
        }

        static member create() =
            let rng = System.Random()

            let b_string = [|1.0f;0.0f;0.0f;0.0f;0.0f;0.0f;0.0f|]
            let t_string = [|0.0f;1.0f;0.0f;0.0f;0.0f;0.0f;0.0f|]
            let p_string = [|0.0f;0.0f;1.0f;0.0f;0.0f;0.0f;0.0f|]
            let s_string = [|0.0f;0.0f;0.0f;1.0f;0.0f;0.0f;0.0f|]
            let x_string = [|0.0f;0.0f;0.0f;0.0f;1.0f;0.0f;0.0f|]
            let v_string = [|0.0f;0.0f;0.0f;0.0f;0.0f;1.0f;0.0f|]
            let e_string = [|0.0f;0.0f;0.0f;0.0f;0.0f;0.0f;1.0f|]

            let t_p_string = [|0.0f;1.0f;1.0f;0.0f;0.0f;0.0f;0.0f|]
            let t_v_string = [|0.0f;1.0f;0.0f;0.0f;0.0f;1.0f;0.0f|]
            let s_x_string = [|0.0f;0.0f;0.0f;1.0f;1.0f;0.0f;0.0f|]
            let p_v_string = [|0.0f;0.0f;1.0f;0.0f;0.0f;1.0f;0.0f|]

            let make_random_reber_string str path prediction =
                let rec nodeF _ _ _ = // Only outputs B. First node.
                    nodeS "B" [b_string] [b_string]
                and nodeS str path prediction = // Can only receive B. Outputs T or P.
                    let p = rng.NextDouble()
                    if p > 0.5 then node0a (str+"T") (t_string::path) (t_p_string::prediction) else node0b (str+"P") (p_string::path) (t_p_string::prediction)
                and node0a str path prediction = // Can only receive T. Outputs B.
                    node1a (str+"B") (b_string::path) (b_string::prediction)
                and node1a str path prediction = // Can only receive B. Outputs T or P.
                    let p = rng.NextDouble()
                    if p > 0.5 then node2a (str+"T") (t_string::path) (t_p_string::prediction) else node3a (str+"P") (p_string::path) (t_p_string::prediction)
                and node2a str path prediction = // Can receive T or S. Outputs S or X.
                    let p = rng.NextDouble()
                    if p > 0.5 then node2a (str+"S") (s_string::path) (s_x_string::prediction) else node4a (str+"X") (x_string::path) (s_x_string::prediction)
                and node3a str path prediction = // Can receive P or T. Outputs V or T.
                    let p = rng.NextDouble()
                    if p > 0.5 then node3a (str+"T") (t_string::path) (t_v_string::prediction) else node5a (str+"V") (v_string::path) (t_v_string::prediction)
                and node4a str path prediction = // Can receive X or P. Outputs X or S.
                    let p = rng.NextDouble()
                    if p > 0.5 then node3a (str+"X") (x_string::path) (s_x_string::prediction) else node6a (str+"S") (s_string::path) (s_x_string::prediction)
                and node5a str path prediction = // Can only receive V. Outputs P or V.
                    let p = rng.NextDouble()
                    if p > 0.5 then node4a (str+"P") (p_string::path) (p_v_string::prediction) else node6a (str+"V") (v_string::path) (p_v_string::prediction)
                and node6a str path prediction = // Can receive S or V. Outputs E.
                    node7a (str+"E") (e_string::path) (e_string::prediction)
                and node7a str path prediction = // Can only receive E. Outputs T.
                    node8 (str+"T") (t_string::path) (t_string::prediction)
                and node0b str path prediction = // Can only receive P. Outputs B.
                    node1b (str+"B") (b_string::path) (b_string::prediction)
                and node1b str path prediction = // Can only receive B. Outputs T or P.
                    let p = rng.NextDouble()
                    if p > 0.5 then node2b (str+"T") (t_string::path) (t_p_string::prediction) else node3b (str+"P") (p_string::path) (t_p_string::prediction)
                and node2b str path prediction = // Can receive T or S. Outputs S or X.
                    let p = rng.NextDouble()
                    if p > 0.5 then node2b (str+"S") (s_string::path) (s_x_string::prediction) else node4b (str+"X") (x_string::path) (s_x_string::prediction)
                and node3b str path prediction = // Can receive P or T. Outputs V or T.
                    let p = rng.NextDouble()
                    if p > 0.5 then node3b (str+"T") (t_string::path) (t_v_string::prediction) else node5b (str+"V") (v_string::path) (t_v_string::prediction)
                and node4b str path prediction = // Can receive X or P. Outputs X or S.
                    let p = rng.NextDouble()
                    if p > 0.5 then node3b (str+"X") (x_string::path) (s_x_string::prediction) else node6b (str+"S") (s_string::path) (s_x_string::prediction)
                and node5b str path prediction = // Can only receive V. Outputs P or V.
                    let p = rng.NextDouble()
                    if p > 0.5 then node4b (str+"P") (p_string::path) (p_v_string::prediction)  else node6b (str+"V") (v_string::path) (p_v_string::prediction) 
                and node6b str path prediction = // Can receive S or V. Outputs E.
                    node7b (str+"E") (e_string::path) (e_string::prediction)
                and node7b str path prediction = // Can only receive E. Outputs P.
                    node8 (str+"P") (p_string::path) (p_string::prediction)
                and node8 str path prediction = // Can receive T or P. Outputs E. Final node.
                    (str+"E"), ((e_string::path) |> List.rev |> List.toArray), ((e_string::prediction) |> List.rev |> List.toArray)
                nodeF str path prediction
            
            let make_reber_set num_examples =
                let mutable c = 0
                let reber_set = new HashSet<(float32 [] * float32 []) []>(HashIdentity.Structural)

                while c < num_examples do
                    let _,inp,lab = make_random_reber_string "" [] []
                    if reber_set.Add (Array.zip inp lab) then c <- c+1
                reber_set |> Seq.toArray

            let ex = 
                group_data_by_seq_length_and_load_to_gpu 512 (make_reber_set 3000)
                |> Array.concat
                |> Array.groupBy (fun x -> x.Length)
                |> dict |> Dictionary

            {state=RNN1DState(); data = ex}

        interface IDisposable with
            member t.Dispose() = 
                t.state |> dispose
                t.data.Values |> Seq.iter (Seq.iter <| Seq.iter (fun (inp,lab) -> dispose inp; dispose lab))

    let run_all_tests() =
        let gradientCheckingTests =
            let epsilon = 0.001f
            let boundary = 0.001f
            // Functional programming strikes again. This function can check any kind of layer for correctness.
            let inline checkLayer layer_number layer (data: GradientCheckingData) =
                let input = data.inputd2M
                let target = data.outputd2M
                let state = data.state
                let _ = train layer GradChecking [|(target,input)|] state false
                let getNodes extract_node = 
                    [|for dm in layer.WrappedNodes.[layer_number].Value do 
                        match extract_node dm with
                        | Some v -> yield v
                        | None -> ()|]
                let true_gradients =
                    let nodes = getNodes (function D2M x -> 
                        if x.HasAdjoint then Some x.GAV else None)
                    [|for x in nodes do yield x.Gather()|]
                let approx_gradients =
                    let nodes = getNodes (function D2M x -> 
                        if x.HasAdjoint then Some x.GPV else None)
                    let grad_check (ar: CudaDeviceVariable<float32>) =
                        [|
                        for i=0 to int ar.Size-1 do
                            let i = SizeT i
                            let orig = ar.[i]
                            let cost() = infer layer GradChecking [|(target,input)|] state false |> fun (_,x) -> x
                            ar.[i] <- orig + epsilon
                            let cost_plus_epsilon = cost()
                            ar.[i] <- orig - epsilon
                            let cost_minus_epsilon = cost()
                            ar.[i] <- orig // Restore the original
                            let approx_gradient = (cost_plus_epsilon - cost_minus_epsilon) / (2.0f * epsilon)
                            yield approx_gradient
                            |]
                    Array.map grad_check nodes
    //            let difference_between_true_and_approx_gradients =
    //                Array.map2 (Array.map2 (fun x y -> abs <| x-y)) true_gradients approx_gradients
    //            let max_difference =
    //                Array.fold (Array.fold max) Single.NegativeInfinity difference_between_true_and_approx_gradients
                let max_difference =
                    Array.fold2 (Array.fold2(fun m x y -> x-y |> abs |> max m)) Single.NegativeInfinity true_gradients approx_gradients
    //            printfn "difference_between_true_and_approx_gradients=%A" difference_between_true_and_approx_gradients
    //            printfn "max_difference=%f" max_difference
                max_difference <= boundary |> assertTest "max_difference <= boundary"
            let ``feedforward layer test`` = 
                checkLayer 0 (FFLayer size_gradcheck relu == squared_error_cost')
//            let ``batch normalization test`` = 
//                let main = BNLayer()
//                checkLayer 0 (fun target -> main ^- squaredErrorLayer target)
            [|
            "feedforward layer test", ``feedforward layer test``
            //"batch normalization test", ``batch normalization test`` // Note: read the warning in batch_normalization_forward before proceeding with this one.
            |]
       
        let mnistTests =
            let inline layerTest network optimizer num_iters (data: MnistData) =
                let training_set = data.training_set
                let test_set = data.test_set
                let state = data.state
                printfn "Testing the feedforward net with %A..." optimizer
                let rec loop iter =
                    let _,training_cost = train network optimizer training_set state false
                    printfn "Done with training."
                    let (test_accuracy, max_accuracy),test_cost = infer network optimizer test_set state true
                    printfn "Training cost is %f at iteration %i" training_cost iter
                    printfn "Test accuracy and cost are (%i/%i, %f) at iteration %i" test_accuracy max_accuracy test_cost iter
                    if iter >= num_iters then 
                        test_accuracy >= 9750 |> assertTest "test_accuracy >= 9750"
                    else
                        loop <| iter+1
                loop 1

            let ``n layer feedforward net test`` =
                layerTest 
                    (FFLayer 256 relu ^-
                     FFLayer 256 relu ^-
                     FFLayer 10 clipped_sigmoid ==
                     cross_entropy_cost')
        
            let ``n layer feedforward net test (with BN)`` =
                layerTest
                    (FFLayer' false 256 relu ^- BNLayer() ^- 
                     FFLayer' false 256 relu ^- BNLayer() ^- 
                     FFLayer 10 clipped_sigmoid ==
                     cross_entropy_cost')

            [|
            "n layer feedforward net test", ``n layer feedforward net test`` (Sgd(1.0f)) 20
            "n layer feedforward net test (with BN)", ``n layer feedforward net test (with BN)`` (Sgd(2.0f)) 20
            |]

        let reberTests =
            let ``standard 1 layer RNN test`` num_iters (optimizer: Optimizer) (data: ReberData) =
                let state = data.state
                let training_set = data.data.[20] // As these two are full batch I need to put them in an array explicitly.
                let test_set = data.data.[30]
                let network = RNN1DLayer 128 tanh_ ^- FFLayer 7 clipped_sigmoid == cross_entropy_cost' |> recurrectSequence
                printfn "Testing the standard 1 layer RNN for %i iterations and optimizer %A..." num_iters optimizer
                let stopwatch = Diagnostics.Stopwatch.StartNew()
                let rec loop iter =
                    let _,training_cost = train network optimizer training_set state false
                    let _, test_cost = infer network optimizer test_set state false
                    printfn "Training cost is %f at iteration %i" training_cost iter
                    printfn "Test cost is %f at iteration %i" test_cost iter
                    let boundary = 0.5f
                    if iter >= num_iters then 
                        printfn "Time elapse for Reber test is %A" stopwatch.Elapsed
                        test_cost <= boundary |> assertTest (sprintf "test_cost <= boundary(%f/%f)" test_cost boundary)
                    else
                        loop <| iter+1
                loop 1
        
            [|
            "standard 1 layer RNN test", ``standard 1 layer RNN test`` 100 <| ClippedSgd(0.1f, 0.1f)
            |]

        ctx.Synchronize() // Inits the library.
        let tree =
            let mnist_path = 
                // If you are anybody other than me, change this to where the Mnist dataset is.
                @"C:\Users\Marko\Documents\Visual Studio 2015\Projects\SpiralQ\SpiralQ\Tests" 
            testArray null
                [|
//                testCases "Mnist Tests" (MnistData.create 256 mnist_path) mnistTests
//                testCases "Gradient Checking Tests" GradientCheckingData.create gradientCheckingTests
                testCases "Reber Grammar RNN Tests" ReberData.create reberTests
                |]
        run tree
    run_all_tests()
