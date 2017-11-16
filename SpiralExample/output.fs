Pattern miss error. The argument is TyT (LitT (LitString "elem_type"))
Error trace on line: 2, column: 1 in file "Learning".
open Cuda
^
Error trace on line: 3, column: 1 in file "Learning".
open Extern
^
Error trace on line: 5, column: 1 in file "Learning".
inl allocator size =
^
Error trace on line: 32, column: 1 in file "Learning".
inl CudaTensor allocator =
^
Error trace on line: 87, column: 1 in file "Learning".
inl CudaTensor = CudaTensor (allocator 0.7)
^
Error trace on line: 88, column: 1 in file "Learning".
open CudaTensor
^
Error trace on line: 90, column: 1 in file "Learning".
inl map f (!zip ({size layout} & in)) =
^
Error trace on line: 111, column: 1 in file "Learning".
open Console
^
Error trace on line: 113, column: 1 in file "Learning".
inl dev_tensor = from_host_tensor (HostTensor.init 8 id)
^
Error trace on line: 114, column: 12 in file "Learning".
inl {ar} = map (inl x -> x * 2) dev_tensor |> to_host_tensor
           ^
Error trace on line: 91, column: 13 in file "Learning".
    inl q = elem_type in
            ^
Error trace on line: 38, column: 9 in file "Learning".
        match layout with
        ^
Error trace on line: 40, column: 19 in file "Learning".
        | .toa -> HostTensor.toa_map f ar
                  ^
Error trace on line: 32, column: 5 in file "HostTensor".
    inl rec loop = function
    ^
Error trace on line: 38, column: 5 in file "HostTensor".
    loop x
    ^
Error trace on line: 37, column: 16 in file "HostTensor".
        | x -> f x
               ^
Error trace on line: 42, column: 46 in file "Learning".
    inl elem_type = map_tensor_ar (inl ar -> ar.elem_type)
                                             ^
