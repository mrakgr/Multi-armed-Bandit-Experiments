Usage of naked type PrimT BoolT as an instance on the term level is invalid.
Error trace on line: 2, column: 1 in file "Learning".
open Cuda
^
Error trace on line: 3, column: 1 in file "Learning".
open Extern
^
Error trace on line: 4, column: 1 in file "Learning".
open Console
^
Error trace on line: 6, column: 1 in file "Learning".
inl smartptr_create ptr =
^
Error trace on line: 20, column: 1 in file "Learning".
inl safe_alloc n create =
^
Error trace on line: 33, column: 1 in file "Learning".
inl allocator size =
^
Error trace on line: 68, column: 1 in file "Learning".
inl CudaTensor allocator =
^
Error trace on line: 121, column: 1 in file "Learning".
open CudaTensor (allocator 0.7)
^
Error trace on line: 123, column: 1 in file "Learning".
inl map f (!zip ({size layout} & in)) =
^
Error trace on line: 143, column: 1 in file "Learning".
inl map = safe_alloc 2 map
^
Error trace on line: 145, column: 1 in file "Learning".
open Console
^
Error trace on line: 147, column: 1 in file "Learning".
inl (>>=) a b ret = 
^
Error trace on line: 151, column: 19 in file "Learning".
inl host_tensor = HostTensor.init 8 id
                  ^
Error trace on line: 53, column: 5 in file "HostTensor".
    match Tuple.length size with
    ^
Error trace on line: 56, column: 9 in file "HostTensor".
        inl len :: dim_offsets = Tuple.scanr (inl (!dim_size dim) s -> dim * s) size 1
        ^
Error trace on line: 57, column: 9 in file "HostTensor".
        inl f = if num_dims = 1 then (inl x :: () -> f x) else f
        ^
Error trace on line: 58, column: 9 in file "HostTensor".
        inl ty = type (f (Tuple.repeat num_dims 0))
        ^
Error trace on line: 59, column: 9 in file "HostTensor".
        inl ar = create ty len
        ^
Error trace on line: 60, column: 9 in file "HostTensor".
        inl rec loop offset index = function
        ^
Error trace on line: 68, column: 9 in file "HostTensor".
        heap {size ar layout}
        ^
Error trace on line: 19, column: 14 in file "Core".
inl heap x = !LayoutToHeap(x)
             ^
