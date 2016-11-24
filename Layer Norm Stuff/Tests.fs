open System
open SpiralV4
open SpiralV4.Flame
open ManagedCuda
open ManagedCuda.BasicTypes
open ManagedCuda.VectorTypes

cuda_context.Synchronize()
let ctx = Context<_>.create

let getd2M (x: d2M) =
    let t = x.GPV.Gather()
    Array2D.init x.Rows x.Columns (fun row col -> t.[col*x.Rows + row])

let d2MtoCudaArray (x: d2M): CudaGlobalArray<float32> =
    let p = nativeint x.GPP.Pointer
    CudaGlobalArray<_>.create(x.Size,p)

let d2MtoCuda2dArray (x: d2M): CudaGlobal2dArray<float32> =
    let p = nativeint x.GPP.Pointer
    CudaGlobal2dArray<_>.create(x.Columns,x.Rows,p)

let inline test_launcher(str: CudaStream, kernel: CudaKernel, x, o: Ar) =
    let inline f x = (^a: (member Conv: obj) x)
    kernel.GridDimensions <- dim3(24)
    kernel.BlockDimensions <- dim3(128)
    let args: obj [] = [|f x; f o|]
    kernel.RunAsync(str.Stream, args)

let map_test() =
    let simple_map_kernel =
        <@
        let kernel_main (x: Ar, o: Ar) =
            let mutable i = BlockIdx.x*BlockDim.x + ThreadIdx.x
            while i < x.length do
                o.[i] <- x.[i]*2.0f
                i <- i + BlockDim.x
        ()
        @>

    let kernel_main_name = "MapTest" // kernel_main will be renamed to this.
    let prog = compile kernel_main_name simple_map_kernel
    printfn "%s" prog

    let cuda_kernel = compile_kernel_nvrtc prog kernel_main_name

    let cols, rows = 10, 1
    let a = d2M.create((rows,cols)) 
            |> fun x -> fillRandomUniformMatrix ctx.Str x 1.0f 0.0f; x 
    let a' = d2MtoCudaArray a

    printfn "%A" (getd2M a)

    let o = d2M.create((rows,cols))
    let o' = d2MtoCudaArray o

    test_launcher(ctx.Str,cuda_kernel,a',o')
    cuda_context.Synchronize()

    printfn "%A" (getd2M o)

let mapredocolmap_test() =
    let mapredocolmap_kernel map_load neutral_element shuffle redo map_out =
        <@
        let map_load: 'a -> 'b = %map_load
        let shuffle: 'b * int -> 'b = %shuffle
        let redo: 'b * 'b -> 'b = %redo
        let map_out: 'b -> 'c = %map_out

        let warp_reduce(value: 'b) =
            let mutable v = value
            _unroll()
            for i=0 to 3 do
                v <- redo(shuffle(v, 1 <<< i), v)
            v

        let block_reduce(v: 'b) =
            let mutable temp = CudaSharedArray(32)
            if ThreadIdx.x < 32 then
                temp.[ThreadIdx.x] <- %neutral_element
            let out_partial = warp_reduce v
            _syncthreads()
            if ThreadIdx.x % 32 = 0 then
                temp.[ThreadIdx.x / 32] <- out_partial
            _syncthreads()
            warp_reduce temp.[ThreadIdx.x % 32]

        let kernel_main (x: CudaGlobal2dArray<'a>, o: CudaGlobalArray<'c>) =
            let mutable col = BlockIdx.x
            while col < x.num_cols do
                let mutable row = ThreadIdx.x
                let mutable v = %neutral_element
                while row < x.num_rows do
                    v <- redo(map_load x.[col,row], v)
                    row <- row + BlockDim.x
                let final_result = map_out(block_reduce v)
                if ThreadIdx.x = 0 then o.[col] <- final_result
                col <- col + GridDim.x
        ()
        @>

    let kernel_main_name = "MapRedocolTest" // kernel_main will be renamed to this.
    let k = mapredocolmap_kernel 
                <@ fun x -> x @> // map_load
                <@ 0.0f @> // neutral_elem
                <@ fun (v,i) -> Shuffle.Xor(v,i) @> // shuffle
                <@ fun (a,b) -> a+b @> // redo
                <@ fun x -> x @> // map_out

    let prog = compile kernel_main_name k
    printfn "%s" prog

    let cuda_kernel = compile_kernel_nvrtc prog kernel_main_name

    let cols, rows = 128, 128
    let a = d2M.create((rows,cols)) 
            |> fun x -> fillRandomUniformMatrix ctx.Str x 1.0f 0.0f; x 
    let a' = d2MtoCuda2dArray a

    //printfn "%A" (getd2M a)

    let o = d2M.create((1,cols))
    let o' = d2MtoCudaArray o

    let watch = Diagnostics.Stopwatch.StartNew()
    for i=1 to 10000 do
        test_launcher(ctx.Str,cuda_kernel,a',o')
        cuda_context.Synchronize()
    printfn "Time elapsed: %A" watch.Elapsed

    //printfn "%A" (o.GPV.Gather())

//mapredocolmap_test()

open System
open System.Diagnostics
open System.IO

let compile_kernel_using_nvcc_command_prompt kernel_code kernel_name =
    // command_shell is supposed to be outside the function, but I do not want to start a command prompt
    // process every time I go through this file, so I've put it here for the time being.
    
    // At any rate, I've decided to use a variant of this function that runs a bat file instead of
    // rerouting the command prompt.
    use command_shell = 
        let procStartInfo = 
            ProcessStartInfo(
                RedirectStandardOutput = true,
                RedirectStandardError = true,
                RedirectStandardInput = true,
                UseShellExecute = false,
                FileName = "cmd")

        let outputHandler f (_sender:obj) (args:DataReceivedEventArgs) = f args.Data
        let p = new Process(StartInfo = procStartInfo)
        let print_to_standard_output = outputHandler <| fun x -> printfn "%s" x
        p.OutputDataReceived.AddHandler(DataReceivedEventHandler (print_to_standard_output))
        p.ErrorDataReceived.AddHandler(DataReceivedEventHandler (print_to_standard_output))
        p.Start()
        p.BeginOutputReadLine()
        p.BeginErrorReadLine()
        p

    let quote x = sprintf "\"%s\"" x
    let quoted_vs_path_to_vcvars = Path.Combine(visual_studio_path, @"VC\bin\x86_amd64\vcvarsx86_amd64.bat") |> quote
    let quoted_vs_path_to_cl = Path.Combine(visual_studio_path, @"VC\bin\x86_amd64") |> quote
    let quoted_cuda_toolkit_path_to_include = Path.Combine(cuda_toolkit_path,"include") |> quote
    let quoted_kernels_dir = kernels_dir |> quote
    let target_path = Path.Combine(kernels_dir,kernel_name+".ptx")
    let quoted_target_path = target_path |> quote
    let input_path = Path.Combine(kernels_dir,"_temp.cu")
    let quoted_input_path = input_path |> quote

    if File.Exists target_path then File.Delete target_path
    File.WriteAllText(input_path,kernel_code)

    command_shell.StandardInput.WriteLine quoted_vs_path_to_vcvars
    command_shell.StandardInput.WriteLine(
        sprintf 
            """nvcc -gencode=arch=compute_30,code=\"sm_30,compute_30\" --use-local-env --cl-version 2015 -ccbin %s  -I%s --keep-dir %s -maxrregcount=0  --machine 64 -ptx -cudart static  -o %s %s"""
            quoted_vs_path_to_cl quoted_cuda_toolkit_path_to_include quoted_kernels_dir quoted_target_path quoted_input_path)

    use watcher = new FileSystemWatcher(kernels_dir)
    watcher.EnableRaisingEvents <- true // This line actually activates the watcher.

    let time_limit = 5000
    let rec f _ = 
        (timer: Threading.Timer).Dispose()
        failwithf "NVCC compilation failed. Time limit of %i milliseconds has passed." time_limit
    and timer = new Threading.Timer(f,null,time_limit,0)
        
    let x = Async.AwaitEvent watcher.Created |> Async.RunSynchronously
    timer.Dispose()

    if x.FullPath <> target_path then failwith "Unexpected file created!"

    command_shell.WaitForExit()
    File.ReadAllText(target_path)

let code = 
    """
    #include "thrust/tuple.h"

    extern "C" {
	    __global__ void CPP11Test(int *c, const int *a, const int *b)
	    {
		    int i = threadIdx.x;
		    auto lamb = [](int x) {return x + 1; }; // Works.
		    auto t = thrust::make_tuple(1, 2, 3);
		    c[i] = a[i] + b[i];
	    }
    }
    """

let k = compile_kernel_using_nvcc_bat_router code "CPP11Test"


