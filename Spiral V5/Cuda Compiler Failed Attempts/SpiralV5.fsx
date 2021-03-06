﻿//[<AutoOpen>]
//module SpiralV5.Main

#load "load-project-release.fsx"

// Open up the namespaces.
open ManagedCuda
open ManagedCuda.BasicTypes
open ManagedCuda.VectorTypes
open ManagedCuda.CudaBlas
open ManagedCuda.CudaRand
open ManagedCuda.NVRTC
open ManagedCuda.CudaDNN

open System
open System.Diagnostics
open System.IO
open System.Collections.Generic
open System.Runtime.InteropServices

let default_num_vars = 2

// Initialize the context. Analogous to a CPU process. Cuda tries to offload as much as possible during context creation so there aren't
// any unexpected delays later.
let cuda_context = new CudaContext(false)
let numSm = cuda_context.GetDeviceInfo().MultiProcessorCount // The number of streaming multiprocessors on the device.

// Set the Cuda libraries handles to the above stream.
let cublas = CudaBlas(PointerMode.Host,AtomicsMode.Allowed) // Better performance for some solver functions with atomics allowed. The Spiral library does not use them though.
let cudnn = new CudaDNNContext()
let cudaRandom = new CudaRand.CudaRandDevice(GeneratorType.PseudoDefault)

/// Integer division with rounding up. (a+b-1)/b is another variant on this.
let inline divup a b = (a-1)/b+1 

let visual_studio_path = //"C:/Program Files (x86)/Microsoft Visual Studio 14.0" usually.
    let x = Environment.GetEnvironmentVariable("VS140COMNTOOLS")
    if x <> null then IO.Directory.GetParent(x).Parent.Parent.FullName
    else failwith "VS140COMNTOOLS environment variable not found. Make sure VS2015 is installed."

let cuda_toolkit_path = 
    let x = System.Environment.GetEnvironmentVariable("CUDA_PATH_V8_0")
    if x <> null then x
    else failwith "CUDA_PATH_V8_0 environment variable not found. Make sure Cuda 8.0 SDK is installed."

let cub_path = // The path for the Cuda Unbound library.
    let x = System.Environment.GetEnvironmentVariable("CUB_PATH")
    if x <> null then x
    else 
        failwith 
            """If you are getting this exception then that means that CUB_PATH environment variable is not defined.

Go to: https://nvlabs.github.io/cub/index.html#sec6
...and download the latest version of the library, extract it somewhere like, 
eg. : C:\cub-1.6.3
and add that directory to the global enviroment by creating the CUB_PATH variable with a pointer to it.

Unlike with VS and Cuda SDK, this will have to be done manually."""

let kernels_dir = IO.Path.Combine(__SOURCE_DIRECTORY__,"Cuda Kernels")
IO.Directory.CreateDirectory(kernels_dir) |> ignore // Creates the Cuda Kernels directory if it does not exist. WriteAllBytes would otherwise throw an exception.

let compile_kernel_nvrtc kernel_name kernel_code = 
    let kernel_path = IO.Path.Combine(kernels_dir,kernel_name)
    let k = new ManagedCuda.NVRTC.CudaRuntimeCompiler(kernel_code,kernel_name)
    try k.Compile(
            [|
            "-arch=compute_30"
            "--std=c++11" // Surprisingly, NVRTC does support C++11 which means lambdas. Unfortunately, it does not support Thrust, so no tuples.
                          // ...So it is still pretty useless all things considered compared to NVCC.
            |])
    with 
    | :? NVRTCException as x -> 
        printfn "%s" (k.GetLogAsString())
        reraise()
    let ptx = k.GetPTX()
    IO.File.WriteAllBytes(kernel_path,ptx)
    cuda_context.LoadKernelPTX(ptx,kernel_name)

/// Puts quotes around the string.
let quote x = sprintf "\"%s\"" x

let inline compile_kernel_using_nvcc_bat_router kernel_name kernel_code =
    let nvcc_router_path = Path.Combine(kernels_dir,"nvcc_router.bat")
    use p = 
        let procStartInfo = 
            ProcessStartInfo(
                RedirectStandardOutput = true,
                RedirectStandardError = true,
                UseShellExecute = false,
                FileName = nvcc_router_path)

        let outputHandler f (_sender:obj) (args:DataReceivedEventArgs) = f args.Data
        let p = new Process(StartInfo = procStartInfo)
        let print_to_standard_output = outputHandler <| fun x -> printfn "%s" x
        //p.OutputDataReceived.AddHandler(DataReceivedEventHandler (print_to_standard_output))
        p.ErrorDataReceived.AddHandler(DataReceivedEventHandler (print_to_standard_output))
        p

    let call x = sprintf "call %s" x
    let quoted_vs_path_to_vcvars = Path.Combine(visual_studio_path, @"VC\bin\x86_amd64\vcvarsx86_amd64.bat") |> quote
    let quoted_vs_path_to_cl = Path.Combine(visual_studio_path, @"VC\bin\x86_amd64") |> quote
    let quoted_cuda_toolkit_path_to_include = Path.Combine(cuda_toolkit_path,"include") |> quote
    let quoted_cub_path_to_include = cub_path |> quote
    let quoted_kernels_dir = kernels_dir |> quote
    let target_path = Path.Combine(kernels_dir,kernel_name+".ptx")
    let quoted_target_path = target_path |> quote
    let input_path = Path.Combine(kernels_dir,"_temp.cu")
    let quoted_input_path = input_path |> quote
    
    if File.Exists input_path then File.Delete input_path
    File.WriteAllText(input_path,kernel_code)

    let _ = 
        if File.Exists nvcc_router_path then File.Delete nvcc_router_path
        use nvcc_router_file = File.OpenWrite(nvcc_router_path)
        use nvcc_router_stream = new StreamWriter(nvcc_router_file)

        nvcc_router_stream.WriteLine(call quoted_vs_path_to_vcvars)
        nvcc_router_stream.WriteLine(
            sprintf 
                """nvcc -gencode=arch=compute_30,code=\"sm_30,compute_30\" --use-local-env --cl-version 2015 -ccbin %s  -I%s -I%s --keep-dir %s -maxrregcount=0  --machine 64 -ptx -cudart static  -o %s %s"""
                quoted_vs_path_to_cl quoted_cuda_toolkit_path_to_include quoted_cub_path_to_include quoted_kernels_dir quoted_target_path quoted_input_path)

    if p.Start() = false then failwith "NVCC failed to run."
    p.BeginOutputReadLine()
    p.BeginErrorReadLine()
    p.WaitForExit()

    if p.ExitCode <> 0 then failwithf "NVCC failed compilation with code %i" p.ExitCode

    cuda_context.LoadKernelPTX(target_path,kernel_name)

let load_kernel compile_kernel kernel_name kernel_code = 
    let kernel_path = IO.Path.Combine(kernels_dir,kernel_name+".ptx")
        
    if IO.File.Exists(kernel_path) 
    then cuda_context.LoadKernelPTX(kernels_dir,kernel_name+".ptx") // For all the modules, it takes roughly 0.35s to compile them. Loading them from drive takes less than a millisecond.
    else compile_kernel kernel_name kernel_code

let inline load_kernel_nvrtc kernel_name kernel_code = load_kernel compile_kernel_nvrtc kernel_name kernel_code
let inline load_kernel_nvcc kernel_name kernel_code = load_kernel compile_kernel_using_nvcc_bat_router kernel_name kernel_code

let memoize f =
    let cache = Dictionary(HashIdentity.Structural)
    fun x ->
        match cache.TryGetValue x with
        | true, v -> v
        | false, _ ->
            let res = f x
            cache.[x] <- res
            res

let tag =
    let mutable i = 0
    fun () -> i <- i+1; i