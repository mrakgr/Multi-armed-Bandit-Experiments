TyLit (LitString "The two tuples have uneven lengths.")
Error trace on line: 2, column: 1 in file "cuda2".
open Cuda
^
Error trace on line: 3, column: 1 in file "cuda2".
inl CudaTensor =
^
Error trace on line: 34, column: 1 in file "cuda2".
inl CudaKernels =
^
Error trace on line: 48, column: 1 in file "cuda2".
inl run_map map_op ins =
^
Error trace on line: 71, column: 1 in file "cuda2".
inl host_ar = Array.init 8 id
^
Error trace on line: 72, column: 1 in file "cuda2".
inl dev_ar = CudaTensor.from_host_array host_ar
^
Error trace on line: 73, column: 1 in file "cuda2".
run_map (inl (x :: ()) -> x * 2) (dev_ar :: ())
^
Error trace on line: 49, column: 5 in file "cuda2".
    inl s = CudaTensor.size
    ^
Error trace on line: 50, column: 5 in file "cuda2".
    inl length =
    ^
Error trace on line: 56, column: 5 in file "cuda2".
    inl ty = Tuple.map (CudaTensor.elem_type) ins
    ^
Error trace on line: 58, column: 5 in file "cuda2".
    inl ins = Tuple.map (CudaTensor.ptr) ins
    ^
Error trace on line: 59, column: 5 in file "cuda2".
    inl outs = Tuple.map (inl x -> CudaTensor.create x length |> CudaTensor.ptr) ty
    ^
Error trace on line: 61, column: 5 in file "cuda2".
    Cuda.run {
    ^
Error trace on line: 142, column: 5 in file "Cuda".
    inl to_obj_ar args =
    ^
Error trace on line: 148, column: 5 in file "Cuda".
    inl kernel =
    ^
Error trace on line: 160, column: 40 in file "Cuda".
    inl method_name, !to_obj_ar args = join_point_entry_cuda kernel
                                       ^
Error trace on line: 6, column: 31 in file "Cuda".
inl join_point_entry_cuda x = !JoinPointEntryCuda(x())
                              ^
Error trace on line: 155, column: 13 in file "Cuda".
            inl threadIdx = {x=__threadIdxX(); y=__threadIdxY(); z=__threadIdxZ()}
            ^
Error trace on line: 156, column: 13 in file "Cuda".
            inl blockIdx = {x=__blockIdxX(); y=__blockIdxY(); z=__blockIdxZ()}
            ^
Error trace on line: 157, column: 13 in file "Cuda".
            inl blockDim = {x=x(); y=y(); z=z()}
            ^
Error trace on line: 158, column: 13 in file "Cuda".
            inl gridDim = {x=x'(); y=y'(); z=z'()}
            ^
Error trace on line: 159, column: 13 in file "Cuda".
            kernel threadIdx blockIdx blockDim gridDim
            ^
Error trace on line: 36, column: 9 in file "cuda2".
        inl stride = gridDim.x * blockDim.x
        ^
Error trace on line: 38, column: 9 in file "cuda2".
        met rec loop i =
        ^
Error trace on line: 44, column: 9 in file "cuda2".
        loop (blockIdx.x * blockDim.x + threadIdx.x)
        ^
Error trace on line: 39, column: 13 in file "cuda2".
            if i < length then
            ^
Error trace on line: 40, column: 17 in file "cuda2".
                Tuple.map (inl x -> x i) ins 
                ^
Error trace on line: 42, column: 17 in file "cuda2".
                |> Tuple.iter2 (inl a b -> a i <- b) outs
                ^
Error trace on line: 60, column: 16 in file "Core".
inl (|>) a b = b a
               ^
Error trace on line: 42, column: 5 in file "Tuple".
    match a,b with
    ^
Error trace on line: 45, column: 12 in file "Tuple".
    | _ -> error_type "The two tuples have uneven lengths." 
           ^
Error trace on line: 8, column: 20 in file "Core".
inl error_type x = !ErrorType(x)
                   ^
