module SpiralExample
let cuda_kernels = """
extern "C" {
}
"""

let rec method_0 ((var_0: int64)): (int64 -> int64) =
    method_1((var_0: int64))
and method_1 ((var_1: int64)) ((var_0: int64)): int64 =
    (var_1 + var_0)
let (var_0: string) = cuda_kernels
let (var_2: (int64 -> (int64 -> int64))) = method_0
let (var_3: (int64 -> int64)) = var_2(1L)
var_3(2L)
