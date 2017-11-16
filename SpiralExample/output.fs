module SpiralExample.Main
let cuda_kernels = """
extern "C" {
    __global__ void method_3(long long int * var_0, long long int * var_1);
    __device__ void method_4(long long int * var_0, long long int * var_1, long long int var_2);
    
    __global__ void method_3(long long int * var_0, long long int * var_1) {
        long long int var_2 = threadIdx.x;
        long long int var_3 = threadIdx.y;
        long long int var_4 = threadIdx.z;
        long long int var_5 = blockIdx.x;
        long long int var_6 = blockIdx.y;
        long long int var_7 = blockIdx.z;
        long long int var_8 = (var_5 * 128);
        long long int var_9 = (var_8 + var_2);
        method_4(var_0, var_1, var_9);
    }
    __device__ void method_4(long long int * var_0, long long int * var_1, long long int var_2) {
        if ((var_2 < 8)) {
            long long int var_3 = var_1[var_2];
            long long int var_4 = (var_3 * 2);
            var_0[var_2] = var_4;
            long long int var_5 = (var_2 + 4096);
            method_4(var_0, var_1, var_5);
        } else {
        }
    }
}
"""

type EnvStack0 =
    struct
    val mem_0: ManagedCuda.CudaContext
    new(arg_mem_0) = {mem_0 = arg_mem_0}
    end
and Env1 =
    struct
    val mem_0: ManagedCuda.BasicTypes.CUdeviceptr
    val mem_1: int64
    new(arg_mem_0, arg_mem_1) = {mem_0 = arg_mem_0; mem_1 = arg_mem_1}
    end
and EnvStack2 =
    struct
    val mem_0: ManagedCuda.BasicTypes.CUdeviceptr
    val mem_1: int64
    val mem_2: System.Collections.Generic.Stack<Env1>
    val mem_3: ManagedCuda.CudaContext
    new(arg_mem_0, arg_mem_1, arg_mem_2, arg_mem_3) = {mem_0 = arg_mem_0; mem_1 = arg_mem_1; mem_2 = arg_mem_2; mem_3 = arg_mem_3}
    end
and EnvHeap3 =
    {
    mem_0: (int64 [])
    }
and Env4 =
    struct
    val mem_0: ManagedCuda.BasicTypes.CUdeviceptr
    new(arg_mem_0) = {mem_0 = arg_mem_0}
    end
let rec method_0 ((var_0: System.Diagnostics.DataReceivedEventArgs)): unit =
    let (var_1: string) = var_0.get_Data()
    System.Console.WriteLine(var_1)
and method_1((var_0: (int64 [])), (var_1: int64), (var_2: int64)): int64 =
    if (var_1 <= 7L) then
        var_0.[int32 var_2] <- var_1
        let (var_3: int64) = (var_2 + 1L)
        let (var_4: int64) = (var_1 + 1L)
        method_1((var_0: (int64 [])), (var_4: int64), (var_3: int64))
    else
        var_2
and method_2((var_0: ManagedCuda.BasicTypes.CUdeviceptr), (var_1: int64), (var_2: int64), (var_3: System.Collections.Generic.Stack<Env1>)): Env4 =
    let (var_4: int32) = var_3.get_Count()
    let (var_6: Env1) =
        if (var_4 > 0) then
            var_3.Peek()
        else
            (Env1(var_0, 0L))
    let (var_7: ManagedCuda.BasicTypes.CUdeviceptr) = var_6.mem_0
    let (var_8: int64) = var_6.mem_1
    let (var_9: ManagedCuda.BasicTypes.SizeT) = var_7.Pointer
    let (var_10: uint64) = uint64(var_9)
    let (var_11: uint64) = uint64(var_8)
    let (var_12: ManagedCuda.BasicTypes.SizeT) = var_0.Pointer
    let (var_13: uint64) = uint64(var_12)
    let (var_14: uint64) = uint64(var_1)
    let (var_15: uint64) = (var_10 - var_13)
    let (var_16: uint64) = (var_15 + var_11)
    let (var_17: uint64) = uint64(var_2)
    let (var_18: uint64) = (var_17 + var_16)
    let (var_19: bool) = (var_18 <= var_14)
    if var_19 then
        ()
    else
        (failwith "Cache size has been exceeded in the allocator.")
    let (var_20: uint64) = (var_10 + var_11)
    let (var_21: ManagedCuda.BasicTypes.SizeT) = ManagedCuda.BasicTypes.SizeT(var_20)
    let (var_22: ManagedCuda.BasicTypes.CUdeviceptr) = ManagedCuda.BasicTypes.CUdeviceptr(var_21)
    var_3.Push((Env1(var_22, var_2)))
    (Env4(var_22))
and method_5((var_0: (int64 []))): string =
    let (var_1: System.Text.StringBuilder) = System.Text.StringBuilder()
    let (var_2: System.Text.StringBuilder) = var_1.Append("[|")
    let (var_3: int64) = var_0.LongLength
    let (var_4: int64) = 0L
    let (var_5: string) = method_6((var_0: (int64 [])), (var_1: System.Text.StringBuilder), (var_3: int64), (var_4: int64))
    let (var_6: System.Text.StringBuilder) = var_1.Append("|]")
    var_1.ToString()
and method_6((var_0: (int64 [])), (var_1: System.Text.StringBuilder), (var_2: int64), (var_3: int64)): string =
    if (var_3 < var_2) then
        let (var_4: int64) = var_0.[int32 var_3]
        let (var_5: System.Text.StringBuilder) = var_1.Append("")
        let (var_6: string) = System.Convert.ToString(var_4)
        let (var_7: System.Text.StringBuilder) = var_1.Append(var_6)
        let (var_8: int64) = (var_3 + 1L)
        method_7((var_0: (int64 [])), (var_1: System.Text.StringBuilder), (var_2: int64), (var_8: int64))
    else
        ""
and method_7((var_0: (int64 [])), (var_1: System.Text.StringBuilder), (var_2: int64), (var_3: int64)): string =
    if (var_3 < var_2) then
        let (var_4: int64) = var_0.[int32 var_3]
        let (var_5: System.Text.StringBuilder) = var_1.Append("; ")
        let (var_6: string) = System.Convert.ToString(var_4)
        let (var_7: System.Text.StringBuilder) = var_1.Append(var_6)
        let (var_8: int64) = (var_3 + 1L)
        method_7((var_0: (int64 [])), (var_1: System.Text.StringBuilder), (var_2: int64), (var_8: int64))
    else
        "; "
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
let (var_17: (System.Diagnostics.DataReceivedEventArgs -> unit)) = method_0
var_15.ErrorDataReceived.Add(var_17)
let (var_18: string) = System.IO.Path.Combine(var_8, "VC\\bin\\x86_amd64\\vcvarsx86_amd64.bat")
let (var_19: int64) = (int64 var_18.Length)
let (var_20: int64) = (12L + var_19)
let (var_21: int32) = (int32 var_20)
let (var_22: System.Text.StringBuilder) = System.Text.StringBuilder(var_21)
let (var_23: System.Text.StringBuilder) = var_22.Append('"')
let (var_24: System.Text.StringBuilder) = var_23.Append(var_18)
let (var_25: System.Text.StringBuilder) = var_24.Append('"')
let (var_26: string) = var_25.ToString()
let (var_27: string) = System.IO.Path.Combine(var_8, "VC\\bin\\x86_amd64")
let (var_28: int64) = (int64 var_27.Length)
let (var_29: int64) = (12L + var_28)
let (var_30: int32) = (int32 var_29)
let (var_31: System.Text.StringBuilder) = System.Text.StringBuilder(var_30)
let (var_32: System.Text.StringBuilder) = var_31.Append('"')
let (var_33: System.Text.StringBuilder) = var_32.Append(var_27)
let (var_34: System.Text.StringBuilder) = var_33.Append('"')
let (var_35: string) = var_34.ToString()
let (var_36: string) = System.IO.Path.Combine(var_1, "include")
let (var_37: int64) = (int64 var_36.Length)
let (var_38: int64) = (12L + var_37)
let (var_39: int32) = (int32 var_38)
let (var_40: System.Text.StringBuilder) = System.Text.StringBuilder(var_39)
let (var_41: System.Text.StringBuilder) = var_40.Append('"')
let (var_42: System.Text.StringBuilder) = var_41.Append(var_36)
let (var_43: System.Text.StringBuilder) = var_42.Append('"')
let (var_44: string) = var_43.ToString()
let (var_45: int64) = (int64 var_9.Length)
let (var_46: int64) = (12L + var_45)
let (var_47: int32) = (int32 var_46)
let (var_48: System.Text.StringBuilder) = System.Text.StringBuilder(var_47)
let (var_49: System.Text.StringBuilder) = var_48.Append('"')
let (var_50: System.Text.StringBuilder) = var_49.Append(var_9)
let (var_51: System.Text.StringBuilder) = var_50.Append('"')
let (var_52: string) = var_51.ToString()
let (var_53: int64) = (int64 var_12.Length)
let (var_54: int64) = (12L + var_53)
let (var_55: int32) = (int32 var_54)
let (var_56: System.Text.StringBuilder) = System.Text.StringBuilder(var_55)
let (var_57: System.Text.StringBuilder) = var_56.Append('"')
let (var_58: System.Text.StringBuilder) = var_57.Append(var_12)
let (var_59: System.Text.StringBuilder) = var_58.Append('"')
let (var_60: string) = var_59.ToString()
let (var_61: string) = System.IO.Path.Combine(var_12, "cuda_kernels.ptx")
let (var_62: int64) = (int64 var_61.Length)
let (var_63: int64) = (12L + var_62)
let (var_64: int32) = (int32 var_63)
let (var_65: System.Text.StringBuilder) = System.Text.StringBuilder(var_64)
let (var_66: System.Text.StringBuilder) = var_65.Append('"')
let (var_67: System.Text.StringBuilder) = var_66.Append(var_61)
let (var_68: System.Text.StringBuilder) = var_67.Append('"')
let (var_69: string) = var_68.ToString()
let (var_70: string) = System.IO.Path.Combine(var_12, "cuda_kernels.cu")
let (var_71: int64) = (int64 var_70.Length)
let (var_72: int64) = (12L + var_71)
let (var_73: int32) = (int32 var_72)
let (var_74: System.Text.StringBuilder) = System.Text.StringBuilder(var_73)
let (var_75: System.Text.StringBuilder) = var_74.Append('"')
let (var_76: System.Text.StringBuilder) = var_75.Append(var_70)
let (var_77: System.Text.StringBuilder) = var_76.Append('"')
let (var_78: string) = var_77.ToString()
let (var_79: bool) = System.IO.File.Exists(var_70)
if var_79 then
    System.IO.File.Delete(var_70)
else
    ()
System.IO.File.WriteAllText(var_70, var_0)
let (var_80: bool) = System.IO.File.Exists(var_13)
if var_80 then
    System.IO.File.Delete(var_13)
else
    ()
let (var_81: System.IO.FileStream) = System.IO.File.OpenWrite(var_13)
let (var_82: System.IO.Stream) = (var_81 :> System.IO.Stream)
let (var_83: System.IO.StreamWriter) = System.IO.StreamWriter(var_82)
let (var_84: int64) = (int64 var_26.Length)
let (var_85: int64) = (5L + var_84)
let (var_86: int32) = (int32 var_85)
let (var_87: System.Text.StringBuilder) = System.Text.StringBuilder(var_86)
let (var_88: System.Text.StringBuilder) = var_87.Append("call ")
let (var_89: System.Text.StringBuilder) = var_88.Append(var_26)
let (var_90: string) = var_89.ToString()
var_83.WriteLine(var_90)
let (var_91: int64) = (int64 var_35.Length)
let (var_92: int64) = (int64 var_44.Length)
let (var_93: int64) = (var_91 + var_92)
let (var_94: int64) = (int64 var_52.Length)
let (var_95: int64) = (var_93 + var_94)
let (var_96: int64) = (int64 var_60.Length)
let (var_97: int64) = (var_95 + var_96)
let (var_98: int64) = (int64 var_69.Length)
let (var_99: int64) = (var_97 + var_98)
let (var_100: int64) = (int64 var_78.Length)
let (var_101: int64) = (var_99 + var_100)
let (var_102: int64) = (177L + var_101)
let (var_103: int32) = (int32 var_102)
let (var_104: System.Text.StringBuilder) = System.Text.StringBuilder(var_103)
let (var_105: System.Text.StringBuilder) = var_104.Append("nvcc -gencode=arch=compute_30,code=\\\"sm_30,compute_30\\\" --use-local-env --cl-version 2015 -ccbin ")
let (var_106: System.Text.StringBuilder) = var_105.Append(var_35)
let (var_107: System.Text.StringBuilder) = var_106.Append("  -I")
let (var_108: System.Text.StringBuilder) = var_107.Append(var_44)
let (var_109: System.Text.StringBuilder) = var_108.Append(" -I")
let (var_110: System.Text.StringBuilder) = var_109.Append(var_52)
let (var_111: System.Text.StringBuilder) = var_110.Append(" --keep-dir ")
let (var_112: System.Text.StringBuilder) = var_111.Append(var_60)
let (var_113: System.Text.StringBuilder) = var_112.Append(" -maxrregcount=0  --machine 64 -ptx -cudart static  -o ")
let (var_114: System.Text.StringBuilder) = var_113.Append(var_69)
let (var_115: System.Text.StringBuilder) = var_114.Append(' ')
let (var_116: System.Text.StringBuilder) = var_115.Append(var_78)
let (var_117: string) = var_116.ToString()
var_83.WriteLine(var_117)
var_83.Dispose()
var_81.Dispose()
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
let (var_124: ManagedCuda.BasicTypes.CUmodule) = var_11.LoadModulePTX(var_61)
var_15.Dispose()
let (var_125: int64) = (51L + var_53)
let (var_126: int32) = (int32 var_125)
let (var_127: System.Text.StringBuilder) = System.Text.StringBuilder(var_126)
let (var_128: System.Text.StringBuilder) = var_127.Append("Compiled the kernels into the following directory: ")
let (var_129: System.Text.StringBuilder) = var_128.Append(var_12)
let (var_130: string) = var_129.ToString()
System.Console.WriteLine(var_130)
let (var_131: EnvStack0) = EnvStack0((var_11: ManagedCuda.CudaContext))
let (var_132: ManagedCuda.CudaDeviceProperties) = var_11.GetDeviceInfo()
let (var_133: ManagedCuda.BasicTypes.SizeT) = var_132.get_TotalGlobalMemory()
let (var_134: int64) = int64(var_133)
let (var_135: float) = Microsoft.FSharp.Core.Operators.float(var_134)
let (var_136: float) = (0.700000 * var_135)
let (var_137: int64) = int64(var_136)
let (var_138: ManagedCuda.BasicTypes.SizeT) = ManagedCuda.BasicTypes.SizeT(var_137)
let (var_139: ManagedCuda.BasicTypes.CUdeviceptr) = var_11.AllocateMemory(var_138)
let (var_140: System.Collections.Generic.Stack<Env1>) = System.Collections.Generic.Stack<Env1>()
let (var_141: EnvStack2) = EnvStack2((var_139: ManagedCuda.BasicTypes.CUdeviceptr), (var_137: int64), (var_140: System.Collections.Generic.Stack<Env1>), (var_11: ManagedCuda.CudaContext))
let (var_142: ManagedCuda.BasicTypes.CUdeviceptr) = var_141.mem_0
let (var_143: int64) = var_141.mem_1
let (var_144: System.Collections.Generic.Stack<Env1>) = var_141.mem_2
let (var_145: ManagedCuda.CudaContext) = var_141.mem_3
let (var_146: (int64 [])) = Array.zeroCreate<int64> (System.Convert.ToInt32(8L))
let (var_147: int64) = 0L
let (var_148: int64) = 0L
let (var_149: int64) = method_1((var_146: (int64 [])), (var_148: int64), (var_147: int64))
let (var_150: EnvHeap3) = ({mem_0 = (var_146: (int64 []))} : EnvHeap3)
let (var_151: (int64 [])) = var_150.mem_0
let (var_152: int64) = var_151.LongLength
let (var_153: int64) = (int64 sizeof<int64>)
let (var_154: int64) = (var_152 * var_153)
let (var_155: Env4) = method_2((var_142: ManagedCuda.BasicTypes.CUdeviceptr), (var_143: int64), (var_154: int64), (var_144: System.Collections.Generic.Stack<Env1>))
let (var_156: ManagedCuda.BasicTypes.CUdeviceptr) = var_155.mem_0
var_145.CopyToDevice(var_156, var_151)
let (var_157: int64) = (8L * var_153)
let (var_158: Env4) = method_2((var_142: ManagedCuda.BasicTypes.CUdeviceptr), (var_143: int64), (var_157: int64), (var_144: System.Collections.Generic.Stack<Env1>))
let (var_159: ManagedCuda.BasicTypes.CUdeviceptr) = var_158.mem_0
// Cuda join point
// method_3((var_156: ManagedCuda.BasicTypes.CUdeviceptr), (var_159: ManagedCuda.BasicTypes.CUdeviceptr))
let (var_160: (System.Object [])) = Array.zeroCreate<System.Object> (System.Convert.ToInt32(2L))
var_160.[int32 0L] <- (var_159 :> System.Object)
var_160.[int32 1L] <- (var_156 :> System.Object)
let (var_161: ManagedCuda.CudaKernel) = ManagedCuda.CudaKernel("method_3", var_124, var_11)
let (var_162: ManagedCuda.VectorTypes.dim3) = ManagedCuda.VectorTypes.dim3(32u, 1u, 1u)
var_161.set_GridDimensions(var_162)
let (var_163: ManagedCuda.VectorTypes.dim3) = ManagedCuda.VectorTypes.dim3(128u, 1u, 1u)
var_161.set_BlockDimensions(var_163)
let (var_164: float32) = var_161.Run(var_160)
let (var_165: (int64 [])) = Array.zeroCreate<int64> (System.Convert.ToInt32(8L))
var_145.CopyToHost(var_165, var_159)
var_145.Synchronize()
let (var_166: string) = method_5((var_165: (int64 [])))
System.Console.WriteLine(var_166)

