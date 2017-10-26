module SpiralExample
let cuda_kernels = """
extern "C" {
}
"""

[<System.Runtime.InteropServices.StructLayout(System.Runtime.InteropServices.LayoutKind.Sequential,Pack=1)>]
type EnvPackedStack0 =
    struct
    val mem_0: int64
    val mem_1: int64
    new(arg_mem_0, arg_mem_1) = {mem_0 = arg_mem_0; mem_1 = arg_mem_1}
    end
let rec method_0((var_0: int64), (var_1: int64), (var_2: EnvPackedStack0)): int64 =
    let (var_3: int64) = var_2.mem_0
    let (var_4: int64) = var_2.mem_1
    let (var_5: int64) = (var_3 + var_4)
    let (var_6: int64) = (var_5 + var_0)
    (var_6 + var_1)
let (var_0: string) = cuda_kernels
let (var_1: int64) = 1L
let (var_2: int64) = 2L
let (var_3: EnvPackedStack0) = EnvPackedStack0((var_1: int64), (var_2: int64))
let (var_4: int64) = 3L
let (var_5: int64) = 4L
method_0((var_4: int64), (var_5: int64), (var_3: EnvPackedStack0))
