Expected a bool in conditional.
Got: DotNetTypeT <tag 2633>
Error trace on line: 2, column: 1 in file "Cuda".
open Extern
^
Error trace on line: 3, column: 1 in file "Cuda".
open Console
^
Error trace on line: 5, column: 1 in file "Cuda".
inl cuda_kernels = !CudaKernels()
^
Error trace on line: 6, column: 1 in file "Cuda".
inl join_point_entry_cuda x = !JoinPointEntryCuda(x())
^
Error trace on line: 8, column: 1 in file "Cuda".
inl __threadIdxX() = !ThreadIdxX()
^
Error trace on line: 9, column: 1 in file "Cuda".
inl __threadIdxY() = !ThreadIdxY()
^
Error trace on line: 10, column: 1 in file "Cuda".
inl __threadIdxZ() = !ThreadIdxZ()
^
Error trace on line: 11, column: 1 in file "Cuda".
inl __blockIdxX() = !BlockIdxX()
^
Error trace on line: 12, column: 1 in file "Cuda".
inl __blockIdxY() = !BlockIdxY()
^
Error trace on line: 13, column: 1 in file "Cuda".
inl __blockIdxZ() = !BlockIdxZ()
^
Error trace on line: 15, column: 1 in file "Cuda".
inl __blockDimX() = !BlockDimX()
^
Error trace on line: 16, column: 1 in file "Cuda".
inl __blockDimY() = !BlockDimY()
^
Error trace on line: 17, column: 1 in file "Cuda".
inl __blockDimZ() = !BlockDimZ()
^
Error trace on line: 18, column: 1 in file "Cuda".
inl __gridDimX() = !GridDimX()
^
Error trace on line: 19, column: 1 in file "Cuda".
inl __gridDimY() = !GridDimY()
^
Error trace on line: 20, column: 1 in file "Cuda".
inl __gridDimZ() = !GridDimZ()
^
Error trace on line: 22, column: 1 in file "Cuda".
inl fsharp_core = assembly_load."FSharp.Core"
^
Error trace on line: 23, column: 1 in file "Cuda".
inl system = assembly_load ."system, Version=4.0.0.0, Culture=neutral, PublicKeyToken=b77a5c561934e089"
^
Error trace on line: 25, column: 1 in file "Cuda".
inl ops = fsharp_core.Microsoft.FSharp.Core.Operators
^
Error trace on line: 26, column: 1 in file "Cuda".
inl Environment = mscorlib.System.Environment
^
Error trace on line: 29, column: 5 in file "Cuda".
    inl x = Environment.GetEnvironmentVariable("CUDA_PATH_V8_0")
    ^
Error trace on line: 30, column: 5 in file "Cuda".
    inl x_ty = type(x)
    ^
Error trace on line: 31, column: 5 in file "Cuda".
    if ops(.isNull, x_ty, x) then failwith unit "CUDA_PATH_V8_0 environment variable not found. Make sure Cuda 8.0 SDK is installed."
    ^
