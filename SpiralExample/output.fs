module SpiralExample
let cuda_kernels = """
extern "C" {
}
"""

type EnvHeap0 =
    {
    mem_0: (int64 [])
    mem_1: (int64 [])
    }
let rec method_0((var_0: (int64 [])), (var_1: (int64 [])), (var_2: int64), (var_3: int64)): int64 =
    if (var_2 <= 9L) then
        let (var_4: int64) = 0L
        let (var_5: int64) = method_1((var_2: int64), (var_0: (int64 [])), (var_1: (int64 [])), (var_4: int64), (var_3: int64))
        let (var_6: int64) = (var_3 + 10L)
        let (var_7: int64) = (var_2 + 1L)
        method_0((var_0: (int64 [])), (var_1: (int64 [])), (var_7: int64), (var_6: int64))
    else
        var_3
and method_1((var_0: int64), (var_1: (int64 [])), (var_2: (int64 [])), (var_3: int64), (var_4: int64)): int64 =
    if (var_3 <= 9L) then
        var_1.[int32 var_4] <- var_0
        var_2.[int32 var_4] <- var_3
        let (var_5: int64) = (var_4 + 1L)
        let (var_6: int64) = (var_3 + 1L)
        method_1((var_0: int64), (var_1: (int64 [])), (var_2: (int64 [])), (var_6: int64), (var_5: int64))
    else
        var_4
let (var_0: (int64 [])) = Array.zeroCreate<int64> (System.Convert.ToInt32(100L))
let (var_1: (int64 [])) = Array.zeroCreate<int64> (System.Convert.ToInt32(100L))
let (var_2: int64) = 0L
let (var_3: int64) = 0L
let (var_4: int64) = method_0((var_0: (int64 [])), (var_1: (int64 [])), (var_3: int64), (var_2: int64))
({mem_0 = (var_0: (int64 [])); mem_1 = (var_1: (int64 []))} : EnvHeap0)
