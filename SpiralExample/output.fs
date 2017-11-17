Pattern miss error. The argument is TyMap
  (EnvUnfiltered
     (map
        [(" pat_var_225",
          TyList
            [TyMap
               (Env
                  (map
                     [("ar",
                       TyMap
                         (EnvUnfiltered
                            ...
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
Error trace on line: 19, column: 1 in file "Learning".
inl safe_alloc n create =
^
Error trace on line: 32, column: 1 in file "Learning".
inl allocator size =
^
Error trace on line: 67, column: 1 in file "Learning".
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
inl host_tensor = HostTensor.init 8 id
^
Error trace on line: 148, column: 1 in file "Learning".
inl dev_tensor = from_host_tensor.unsafe host_tensor
^
