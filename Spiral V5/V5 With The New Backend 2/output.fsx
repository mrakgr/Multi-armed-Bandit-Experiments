let rec method_0 ((var_0: System.Object)): (System.Diagnostics.DataReceivedEventArgs -> unit) =
    method_1((var_0: System.Object))
and method_1 ((var_1: System.Object)) ((var_0: System.Diagnostics.DataReceivedEventArgs)): unit =
    let (var_2: string) = var_0.get_Data()
    System.Console.WriteLine(var_2)
let (var_0: string) = System.Environment.GetEnvironmentVariable("CUDA_PATH_V8_0")
let (var_1: bool) = Microsoft.FSharp.Core.Operators.isNull(var_0)
if var_1 then
    (failwith "CUDA_PATH_V8_0 environment variable not found. Make sure Cuda 8.0 SDK is installed.")
else
    ()
let (var_2: string) = System.Environment.GetEnvironmentVariable("VS140COMNTOOLS")
let (var_3: bool) = Microsoft.FSharp.Core.Operators.isNull(var_2)
if var_3 then
    (failwith "VS140COMNTOOLS environment variable not found. Make sure VS2015 is installed.")
else
    ()
let (var_4: System.IO.DirectoryInfo) = System.IO.Directory.GetParent(var_2)
let (var_5: System.IO.DirectoryInfo) = var_4.get_Parent()
let (var_6: System.IO.DirectoryInfo) = var_5.get_Parent()
let (var_7: string) = var_6.get_FullName()
let (var_8: string) = System.Environment.GetEnvironmentVariable("CUB_PATH")
let (var_9: bool) = Microsoft.FSharp.Core.Operators.isNull(var_8)
if var_9 then
    (failwith "If you are getting this exception then that means that CUB_PATH environment variable is not defined.

Go to: https://nvlabs.github.io/cub/index.html#sec6
...and download the latest version of the library, extract it somewhere like, 
eg. : C:\\cub-1.6.3
and add that directory to the global enviroment by creating the CUB_PATH variable with a pointer to it.")
else
    ()
let (var_10: ManagedCuda.CudaContext) = ManagedCuda.CudaContext(false)
let (var_11: System.Diagnostics.ProcessStartInfo) = System.Diagnostics.ProcessStartInfo()
var_11.set_RedirectStandardOutput(true)
var_11.set_RedirectStandardError(true)
var_11.set_UseShellExecute(false)
var_11.set_FileName("nvcc_router.bat")
let (var_12: System.Diagnostics.Process) = System.Diagnostics.Process()
var_12.set_StartInfo(var_11)
let (var_14: (System.Object -> (System.Diagnostics.DataReceivedEventArgs -> unit))) = method_0
let (var_15: System.Diagnostics.DataReceivedEventHandler) = System.Diagnostics.DataReceivedEventHandler(var_14)
let (var_16: string) = System.IO.Path.Combine(var_7, "VC\\bin\\x86_amd64\\vcvarsx86_amd64.bat")
let (var_17: System.Text.StringBuilder) = System.Text.StringBuilder(64)
let (var_18: System.Text.StringBuilder) = var_17.Append("\"")
let (var_19: System.Text.StringBuilder) = var_17.Append(var_16)
let (var_20: System.Text.StringBuilder) = var_17.Append("\"")
let (var_21: string) = var_17.ToString()
let (var_22: string) = System.IO.Path.Combine(var_7, "VC\\bin\\x86_amd64")
let (var_23: System.Text.StringBuilder) = System.Text.StringBuilder(64)
let (var_24: System.Text.StringBuilder) = var_23.Append("\"")
let (var_25: System.Text.StringBuilder) = var_23.Append(var_22)
let (var_26: System.Text.StringBuilder) = var_23.Append("\"")
let (var_27: string) = var_23.ToString()
let (var_28: string) = System.IO.Path.Combine(var_0, "include")
let (var_29: System.Text.StringBuilder) = System.Text.StringBuilder(64)
let (var_30: System.Text.StringBuilder) = var_29.Append("\"")
let (var_31: System.Text.StringBuilder) = var_29.Append(var_28)
let (var_32: System.Text.StringBuilder) = var_29.Append("\"")
let (var_33: string) = var_29.ToString()
let (var_34: System.Text.StringBuilder) = System.Text.StringBuilder(64)
let (var_35: System.Text.StringBuilder) = var_34.Append("\"")
let (var_36: System.Text.StringBuilder) = var_34.Append(var_8)
let (var_37: System.Text.StringBuilder) = var_34.Append("\"")
let (var_38: string) = var_34.ToString()
let (var_39: System.Text.StringBuilder) = System.Text.StringBuilder(64)
let (var_40: System.Text.StringBuilder) = var_39.Append("\"")
let (var_41: System.Text.StringBuilder) = var_39.Append("C:\\Temp\\")
let (var_42: System.Text.StringBuilder) = var_39.Append("\"")
let (var_43: string) = var_39.ToString()
let (var_44: string) = System.IO.Path.Combine("C:\\Temp\\", "cuda_kernels.ptx")
let (var_45: System.Text.StringBuilder) = System.Text.StringBuilder(64)
let (var_46: System.Text.StringBuilder) = var_45.Append("\"")
let (var_47: System.Text.StringBuilder) = var_45.Append(var_44)
let (var_48: System.Text.StringBuilder) = var_45.Append("\"")
let (var_49: string) = var_45.ToString()
let (var_50: string) = System.IO.Path.Combine("C:\\Temp\\", "cuda_kernels.cu")
let (var_51: System.Text.StringBuilder) = System.Text.StringBuilder(64)
let (var_52: System.Text.StringBuilder) = var_51.Append("\"")
let (var_53: System.Text.StringBuilder) = var_51.Append(var_50)
let (var_54: System.Text.StringBuilder) = var_51.Append("\"")
let (var_55: string) = var_51.ToString()
let (var_56: bool) = System.IO.File.Exists("nvcc_router.bat")
if var_56 then
    System.IO.File.Delete("nvcc_router.bat")
else
    ()
let (var_57: System.IO.FileStream) = System.IO.File.OpenWrite("nvcc_router.bat")
let (var_58: System.IO.StreamWriter) = System.IO.StreamWriter(var_57)
let (var_59: System.Text.StringBuilder) = System.Text.StringBuilder(64)
let (var_60: System.Text.StringBuilder) = var_59.Append("call ")
let (var_61: System.Text.StringBuilder) = var_59.Append(var_21)
let (var_62: string) = var_59.ToString()
var_58.WriteLine(var_62)
let (var_63: System.Text.StringBuilder) = System.Text.StringBuilder(64)
let (var_64: System.Text.StringBuilder) = var_63.Append("nvcc -gencode=arch=compute_30,code=\"sm_30,compute_30\" --use-local-env --cl-version 2015 -ccbin ")
let (var_65: System.Text.StringBuilder) = var_63.Append(var_27)
let (var_66: System.Text.StringBuilder) = var_63.Append("  -I")
let (var_67: System.Text.StringBuilder) = var_63.Append(var_33)
let (var_68: System.Text.StringBuilder) = var_63.Append(" -I")
let (var_69: System.Text.StringBuilder) = var_63.Append(var_38)
let (var_70: System.Text.StringBuilder) = var_63.Append(" --keep-dir ")
let (var_71: System.Text.StringBuilder) = var_63.Append(var_43)
let (var_72: System.Text.StringBuilder) = var_63.Append(" -maxrregcount=0  --machine 64 -ptx -cudart static  -o ")
let (var_73: System.Text.StringBuilder) = var_63.Append(var_49)
let (var_74: System.Text.StringBuilder) = var_63.Append(" ")
let (var_75: System.Text.StringBuilder) = var_63.Append(var_55)
let (var_76: string) = var_63.ToString()
var_58.WriteLine(var_76)
var_57.Dispose()
var_58.Dispose()
let (var_77: bool) = var_12.Start()
if (var_77 = false) then
    (failwith "NVCC failed to run.")
else
    ()
var_12.BeginOutputReadLine()
var_12.BeginErrorReadLine()
var_12.WaitForExit()
let (var_78: int32) = var_12.get_ExitCode()
if (var_78 <> 0) then
    let (var_79: System.Text.StringBuilder) = System.Text.StringBuilder(64)
    let (var_80: System.Text.StringBuilder) = var_79.Append("NVCC failed compilation with code ")
    let (var_81: System.Text.StringBuilder) = var_79.Append(var_78)
    let (var_82: string) = var_79.ToString()
    (failwith var_82)
else
    ()
var_12.Dispose()
var_10.LoadModulePTX(var_44)
