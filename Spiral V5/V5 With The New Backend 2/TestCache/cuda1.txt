Cannot find static method get_Data in the .NET type.
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
Error trace on line: 28, column: 1 in file "Cuda".
inl cuda_toolkit_path = 
^
Error trace on line: 34, column: 1 in file "Cuda".
inl visual_studio_path =
^
Error trace on line: 40, column: 1 in file "Cuda".
inl cub_path = // The path for the Cuda Unbound library.
^
Error trace on line: 53, column: 1 in file "Cuda".
inl ManagedCuda = assembly_load ."ManagedCuda, Version=7.5.7.0, Culture=neutral, PublicKeyToken=242d898828717aa0" .ManagedCuda
^
Error trace on line: 54, column: 1 in file "Cuda".
inl context = ManagedCuda.CudaContext false
^
Error trace on line: 56, column: 1 in file "Cuda".
inl compile_kernel_using_nvcc_bat_router (kernels_dir: string) =
^
Error trace on line: 125, column: 1 in file "Cuda".
inl current_directory = Environment.get_CurrentDirectory()
^
Error trace on line: 126, column: 15 in file "Cuda".
inl modules = compile_kernel_using_nvcc_bat_router current_directory
              ^
Error trace on line: 57, column: 5 in file "Cuda".
    inl Path = mscorlib .System.IO.Path
    ^
Error trace on line: 58, column: 5 in file "Cuda".
    inl File = mscorlib .System.IO.File
    ^
Error trace on line: 59, column: 5 in file "Cuda".
    inl StreamWriter = mscorlib .System.IO.StreamWriter
    ^
Error trace on line: 60, column: 5 in file "Cuda".
    inl ProcessStartInfo = system .System.Diagnostics.ProcessStartInfo
    ^
Error trace on line: 62, column: 5 in file "Cuda".
    inl nvcc_router_path = Path.Combine(kernels_dir,"nvcc_router.bat")
    ^
Error trace on line: 63, column: 5 in file "Cuda".
    inl procStartInfo = ProcessStartInfo()
    ^
Error trace on line: 68, column: 5 in file "Cuda".
    inl process = system .System.Diagnostics.Process()
    ^
Error trace on line: 71, column: 9 in file "Cuda".
        term_cast (inl args -> 
        ^
Error trace on line: 41, column: 25 in file "Core".
inl term_cast to from = !TermCast(to,from)
                        ^
Error trace on line: 73, column: 13 in file "Cuda".
            args.get_Data() |> writeline) 
            ^
