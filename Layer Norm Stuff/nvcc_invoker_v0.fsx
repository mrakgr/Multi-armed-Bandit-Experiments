open System
open System.Diagnostics

let procStartInfo = 
    ProcessStartInfo(
        RedirectStandardOutput = true,
        RedirectStandardError = true,
        RedirectStandardInput = true,
        UseShellExecute = false,
        FileName = "cmd"
    )

let outputHandler f (_sender:obj) (args:DataReceivedEventArgs) = f args.Data
let p = new Process(StartInfo = procStartInfo)
let print_to_standard_output = outputHandler <| fun x -> printfn "%s" x
p.OutputDataReceived.AddHandler(DataReceivedEventHandler (print_to_standard_output))
p.ErrorDataReceived.AddHandler(DataReceivedEventHandler (print_to_standard_output))
p.Start()
p.BeginOutputReadLine()
p.BeginErrorReadLine()
p.StandardInput.WriteLine """ "C:\Program Files (x86)\Microsoft Visual Studio 14.0\VC\bin\x86_amd64\vcvarsx86_amd64.bat" """
p.StandardInput.WriteLine """ "C:\Program Files\NVIDIA GPU Computing Toolkit\CUDA\v8.0\bin\nvcc.exe" -gencode=arch=compute_30,code=\"sm_30,compute_30\" --use-local-env --cl-version 2015 -ccbin "C:\Program Files (x86)\Microsoft Visual Studio 14.0\VC\bin\x86_amd64"  -I"C:\Program Files\NVIDIA GPU Computing Toolkit\CUDA\v8.0\include" -I"C:\Program Files\NVIDIA GPU Computing Toolkit\CUDA\v8.0\include"     --keep-dir "C:\Users\Marko\Documents\Visual Studio 2015\Projects\Multi-armed Bandit Experiments\NVCC Experiments\x64\Release" -maxrregcount=0  --machine 64 -ptx -cudart static  -o "C:\Users\Marko\Documents\Visual Studio 2015\Projects\Multi-armed Bandit Experiments\NVCC Experiments\x64\Release\kernel.cu.obj" "C:\Users\Marko\Documents\Visual Studio 2015\Projects\Multi-armed Bandit Experiments\NVCC Experiments\kernel.cu" """
p.StandardInput.WriteLine """ ECHO "Done." """
