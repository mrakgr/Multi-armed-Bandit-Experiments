// A backup of the unused compile_kernel function.

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

