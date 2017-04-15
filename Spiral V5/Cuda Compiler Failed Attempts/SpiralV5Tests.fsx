#load "SpiralV5Eval.fsx"
open SpiralV5
open SpiralV5DM
open SpiralV5Eval

module Embel =
    let random_seed_system_random = Some 42
    let random_seed_GPU = Some 42UL

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
        match random_seed_GPU with
        | Some v ->
            cudaRandom.SetOffset(0UL)
            cudaRandom.SetPseudoRandomGeneratorSeed(v)
        | None -> ()

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
    open System.Windows

    type MnistData =
        {
        training_set : (DM<int*int,float32> * DM<int*int,float32>)[]
        test_set : (DM<int*int,float32> * DM<int*int,float32>)[]
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
                    |> Array.map (fun x -> DM<_,_>.create (10,x.Length/10) 2 x)
                | 2051 -> // Images
                    let n = d.ReadInt32() |> Net.IPAddress.NetworkToHostOrder
                    let rows = d.ReadInt32() |> Net.IPAddress.NetworkToHostOrder
                    let cols = d.ReadInt32() |> Net.IPAddress.NetworkToHostOrder
                    d.ReadBytes(n * rows * cols)
                    |> Array.map (fun x -> float32 x / 255.0f)
                    |> Array.chunkBySize (minibatch_size*rows*cols)
                    |> Array.map (fun x -> DM<_,_>.create (rows*cols,x.Length/(rows*cols)) 1 x)
                | _ -> failwith "Given file is not in the MNIST format."

            let [|test_images;test_labels;train_images;train_labels|] = 
                [|"t10k-images.idx3-ubyte";"t10k-labels.idx1-ubyte";"train-images.idx3-ubyte";"train-labels.idx1-ubyte"|]
                |> Array.map (fun x -> Path.Combine(mnist_path,x) |> load_mnist)

            {
            training_set = Array.zip train_images train_labels
            test_set = Array.zip test_images test_labels
            }

        interface IDisposable with
            member t.Dispose() =
                for x,y in t.training_set do x |> dispose; y |> dispose
                for x,y in t.test_set do x |> dispose; y |> dispose

    let mnistTests =
//        let inline layerTest network optimizer num_iters (data: MnistData) =
//            let training_set = data.training_set
//            let test_set = data.test_set
//            let ctx = SpiralEnv<_>.create
//            printfn "Testing the feedforward net with %A..." optimizer
//            let rec loop iter =
//                let _,training_cost = train network optimizer training_set ctx false
//                printfn "Done with training."
//                let (test_accuracy, max_accuracy),test_cost = infer network optimizer test_set ctx true
//                printfn "Training cost is %f at iteration %i" training_cost iter
//                printfn "Test accuracy and cost are (%i/%i, %f) at iteration %i" test_accuracy max_accuracy test_cost iter
//                if iter >= num_iters then 
//                    test_accuracy >= 9750 |> assertTest "test_accuracy >= 9750"
//                else
//                    loop <| iter+1
//            loop 1
        ()
