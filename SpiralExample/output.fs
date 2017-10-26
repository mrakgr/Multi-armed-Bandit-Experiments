module SpiralExample
let cuda_kernels = """
extern "C" {
    struct EnvStack0 {
        long long int mem_0;
    };
    __device__ __forceinline__ EnvStack0 make_EnvStack0(long long int mem_0){
        EnvStack0 tmp;
        tmp.mem_0 = mem_0;
        return tmp;
    }
    __global__ void method_2() {
        long long int var_0 = threadIdx.x;
        long long int var_1 = threadIdx.y;
        long long int var_2 = threadIdx.z;
        long long int var_3 = blockIdx.x;
        long long int var_4 = blockIdx.y;
        long long int var_5 = blockIdx.z;
        long long int var_6 = 64;
        long long int var_7 = 1;
        long long int var_8 = (var_6 + var_7);
        EnvStack0 var_9 = make_EnvStack0(var_8);
    }
}
"""

let rec method_0 ((var_0: System.Object)): (System.Diagnostics.DataReceivedEventArgs -> unit) =
    method_1((var_0: System.Object))
and method_1 ((var_1: System.Object)) ((var_0: System.Diagnostics.DataReceivedEventArgs)): unit =
    let (var_2: string) = var_0.get_Data()
    System.Console.WriteLine(var_2)
let (var_0: string) = cuda_kernels
let (var_1: string) = System.Environment.GetEnvironmentVariable("CUDA_PATH_V8_0")
let (var_2: bool) = Microsoft.FSharp.Core.Operators.isNull(var_1)
if var_2 then
    (failwith "CUDA_PATH_V8_0 environment variable not found. Make sure Cuda 8.0 SDK is installed.")
else
    ()
let (var_3: string) = System.Environment.GetEnvironmentVariable("VS140COMNTOOLS")
let (var_4: bool) = Microsoft.FSharp.Core.Operators.isNull(var_3)
if var_4 then
    (failwith "VS140COMNTOOLS environment variable not found. Make sure VS2015 is installed.")
else
    ()
let (var_5: System.IO.DirectoryInfo) = System.IO.Directory.GetParent(var_3)
let (var_6: System.IO.DirectoryInfo) = var_5.get_Parent()
let (var_7: System.IO.DirectoryInfo) = var_6.get_Parent()
let (var_8: string) = var_7.get_FullName()
let (var_9: string) = System.Environment.GetEnvironmentVariable("CUB_PATH")
let (var_10: bool) = Microsoft.FSharp.Core.Operators.isNull(var_9)
if var_10 then
    (failwith "If you are getting this exception then that means that CUB_PATH environment variable is not defined.

Go to: https://nvlabs.github.io/cub/index.html#sec6
...and download the latest version of the library, extract it somewhere like, 
eg. : C:\\cub-1.6.3
and add that directory to the global enviroment by creating the CUB_PATH variable with a pointer to it.")
else
    ()
let (var_11: ManagedCuda.CudaContext) = ManagedCuda.CudaContext(false)
let (var_12: string) = System.Environment.get_CurrentDirectory()
let (var_13: string) = System.IO.Path.Combine(var_12, "nvcc_router.bat")
let (var_14: System.Diagnostics.ProcessStartInfo) = System.Diagnostics.ProcessStartInfo()
var_14.set_RedirectStandardOutput(true)
var_14.set_RedirectStandardError(true)
var_14.set_UseShellExecute(false)
var_14.set_FileName(var_13)
let (var_15: System.Diagnostics.Process) = System.Diagnostics.Process()
var_15.set_StartInfo(var_14)
let (var_17: (System.Object -> (System.Diagnostics.DataReceivedEventArgs -> unit))) = method_0
let (var_18: System.Diagnostics.DataReceivedEventHandler) = System.Diagnostics.DataReceivedEventHandler(var_17)
let (var_19: string) = System.IO.Path.Combine(var_8, "VC\\bin\\x86_amd64\\vcvarsx86_amd64.bat")
let (var_20: int64) = (int64 var_19.Length)
let (var_21: int64) = (7L + var_20)
let (var_22: int32) = (int32 var_21)
let (var_23: System.Text.StringBuilder) = System.Text.StringBuilder(var_22)
let (var_24: System.Text.StringBuilder) = var_23.Append('"')
let (var_25: System.Text.StringBuilder) = var_24.Append(var_19)
let (var_26: System.Text.StringBuilder) = var_25.Append("'")
let (var_27: string) = var_26.ToString()
let (var_28: string) = System.IO.Path.Combine(var_8, "VC\\bin\\x86_amd64")
let (var_29: int64) = (int64 var_28.Length)
let (var_30: int64) = (7L + var_29)
let (var_31: int32) = (int32 var_30)
let (var_32: System.Text.StringBuilder) = System.Text.StringBuilder(var_31)
let (var_33: System.Text.StringBuilder) = var_32.Append('"')
let (var_34: System.Text.StringBuilder) = var_33.Append(var_28)
let (var_35: System.Text.StringBuilder) = var_34.Append("'")
let (var_36: string) = var_35.ToString()
let (var_37: string) = System.IO.Path.Combine(var_1, "include")
let (var_38: int64) = (int64 var_37.Length)
let (var_39: int64) = (7L + var_38)
let (var_40: int32) = (int32 var_39)
let (var_41: System.Text.StringBuilder) = System.Text.StringBuilder(var_40)
let (var_42: System.Text.StringBuilder) = var_41.Append('"')
let (var_43: System.Text.StringBuilder) = var_42.Append(var_37)
let (var_44: System.Text.StringBuilder) = var_43.Append("'")
let (var_45: string) = var_44.ToString()
let (var_46: int64) = (int64 var_9.Length)
let (var_47: int64) = (7L + var_46)
let (var_48: int32) = (int32 var_47)
let (var_49: System.Text.StringBuilder) = System.Text.StringBuilder(var_48)
let (var_50: System.Text.StringBuilder) = var_49.Append('"')
let (var_51: System.Text.StringBuilder) = var_50.Append(var_9)
let (var_52: System.Text.StringBuilder) = var_51.Append("'")
let (var_53: string) = var_52.ToString()
let (var_54: int64) = (int64 var_12.Length)
let (var_55: int64) = (7L + var_54)
let (var_56: int32) = (int32 var_55)
let (var_57: System.Text.StringBuilder) = System.Text.StringBuilder(var_56)
let (var_58: System.Text.StringBuilder) = var_57.Append('"')
let (var_59: System.Text.StringBuilder) = var_58.Append(var_12)
let (var_60: System.Text.StringBuilder) = var_59.Append("'")
let (var_61: string) = var_60.ToString()
let (var_62: string) = System.IO.Path.Combine(var_12, "cuda_kernels.ptx")
let (var_63: int64) = (int64 var_62.Length)
let (var_64: int64) = (7L + var_63)
let (var_65: int32) = (int32 var_64)
let (var_66: System.Text.StringBuilder) = System.Text.StringBuilder(var_65)
let (var_67: System.Text.StringBuilder) = var_66.Append('"')
let (var_68: System.Text.StringBuilder) = var_67.Append(var_62)
let (var_69: System.Text.StringBuilder) = var_68.Append("'")
let (var_70: string) = var_69.ToString()
let (var_71: string) = System.IO.Path.Combine(var_12, "cuda_kernels.cu")
let (var_72: int64) = (int64 var_71.Length)
let (var_73: int64) = (7L + var_72)
let (var_74: int32) = (int32 var_73)
let (var_75: System.Text.StringBuilder) = System.Text.StringBuilder(var_74)
let (var_76: System.Text.StringBuilder) = var_75.Append('"')
let (var_77: System.Text.StringBuilder) = var_76.Append(var_71)
let (var_78: System.Text.StringBuilder) = var_77.Append("'")
let (var_79: string) = var_78.ToString()
let (var_80: bool) = System.IO.File.Exists(var_71)
if var_80 then
    System.IO.File.Delete(var_71)
else
    ()
System.IO.File.WriteAllText(var_71, var_0)
let (var_81: bool) = System.IO.File.Exists(var_13)
if var_81 then
    System.IO.File.Delete(var_13)
else
    ()
let (var_82: System.IO.FileStream) = System.IO.File.OpenWrite(var_13)
let (var_83: System.IO.StreamWriter) = System.IO.StreamWriter(var_82)
let (var_84: int64) = (int64 var_27.Length)
let (var_85: int64) = (5L + var_84)
let (var_86: int32) = (int32 var_85)
let (var_87: System.Text.StringBuilder) = System.Text.StringBuilder(var_86)
let (var_88: System.Text.StringBuilder) = var_87.Append("call ")
let (var_89: System.Text.StringBuilder) = var_88.Append(var_27)
let (var_90: string) = var_89.ToString()
var_83.WriteLine(var_90)
let (var_91: int64) = (int64 var_36.Length)
let (var_92: int64) = (int64 var_45.Length)
let (var_93: int64) = (var_91 + var_92)
let (var_94: int64) = (int64 var_53.Length)
let (var_95: int64) = (var_93 + var_94)
let (var_96: int64) = (int64 var_61.Length)
let (var_97: int64) = (var_95 + var_96)
let (var_98: int64) = (int64 var_70.Length)
let (var_99: int64) = (var_97 + var_98)
let (var_100: int64) = (int64 var_79.Length)
let (var_101: int64) = (var_99 + var_100)
let (var_102: int64) = (175L + var_101)
let (var_103: int32) = (int32 var_102)
let (var_104: System.Text.StringBuilder) = System.Text.StringBuilder(var_103)
let (var_105: System.Text.StringBuilder) = var_104.Append("nvcc -gencode=arch=compute_30,code=\"sm_30,compute_30\" --use-local-env --cl-version 2015 -ccbin ")
let (var_106: System.Text.StringBuilder) = var_105.Append(var_36)
let (var_107: System.Text.StringBuilder) = var_106.Append("  -I")
let (var_108: System.Text.StringBuilder) = var_107.Append(var_45)
let (var_109: System.Text.StringBuilder) = var_108.Append(" -I")
let (var_110: System.Text.StringBuilder) = var_109.Append(var_53)
let (var_111: System.Text.StringBuilder) = var_110.Append(" --keep-dir ")
let (var_112: System.Text.StringBuilder) = var_111.Append(var_61)
let (var_113: System.Text.StringBuilder) = var_112.Append(" -maxrregcount=0  --machine 64 -ptx -cudart static  -o ")
let (var_114: System.Text.StringBuilder) = var_113.Append(var_70)
let (var_115: System.Text.StringBuilder) = var_114.Append(' ')
let (var_116: System.Text.StringBuilder) = var_115.Append(var_79)
let (var_117: string) = var_116.ToString()
var_83.WriteLine(var_117)
var_82.Dispose()
var_83.Dispose()
let (var_118: bool) = var_15.Start()
if (var_118 = false) then
    (failwith "NVCC failed to run.")
else
    ()
var_15.BeginOutputReadLine()
var_15.BeginErrorReadLine()
var_15.WaitForExit()
let (var_119: int32) = var_15.get_ExitCode()
if (var_119 <> 0) then
    let (var_120: System.Text.StringBuilder) = System.Text.StringBuilder(40)
    let (var_121: System.Text.StringBuilder) = var_120.Append("NVCC failed compilation with code ")
    let (var_122: System.Text.StringBuilder) = var_121.Append(var_119)
    let (var_123: string) = var_122.ToString()
    (failwith var_123)
else
    ()
var_15.Dispose()
let (var_124: ManagedCuda.BasicTypes.CUmodule) = var_11.LoadModulePTX(var_62)
let (var_125: int64) = (51L + var_54)
let (var_126: int32) = (int32 var_125)
let (var_127: System.Text.StringBuilder) = System.Text.StringBuilder(var_126)
let (var_128: System.Text.StringBuilder) = var_127.Append("Compiled the kernels into the following directory: ")
let (var_129: System.Text.StringBuilder) = var_128.Append(var_12)
let (var_130: string) = var_129.ToString()
System.Console.WriteLine(var_130)
let (var_131: ManagedCuda.CudaStream) = ManagedCuda.CudaStream()
// Cuda method call
// method_2()
let (var_132: (System.Object [])) = Array.zeroCreate<System.Object> (System.Convert.ToInt32(0L))
let (var_133: ManagedCuda.CudaKernel) = ManagedCuda.CudaKernel("method_2", var_124, var_11)
let (var_134: ManagedCuda.VectorTypes.dim3) = ManagedCuda.VectorTypes.dim3(32u, 1u, 1u)
var_133.set_GridDimensions(var_134)
let (var_135: ManagedCuda.VectorTypes.dim3) = ManagedCuda.VectorTypes.dim3(64u, 1u, 1u)
var_133.set_BlockDimensions(var_135)
let (var_136: ManagedCuda.BasicTypes.CUstream) = var_131.get_Stream()
var_133.RunAsync(var_136, var_132)
