module SpiralExample
let cuda_kernels = """
extern "C" {
}
"""

type EnvHeap0 =
    {
    mem_0: (bool [])
    mem_1: int64
    mem_2: int64
    }
let rec method_0((var_0: int64), (var_1: int64), (var_2: (bool [])), (var_3: int64), (var_4: int64), (var_5: int64)): int64 =
    if (var_4 <= var_3) then
        let (var_6: int64) = 0L
        let (var_7: int64) = method_1((var_4: int64), (var_2: (bool [])), (var_1: int64), (var_6: int64), (var_5: int64))
        let (var_8: int64) = (var_5 + var_0)
        let (var_9: int64) = (var_4 + 1L)
        method_0((var_0: int64), (var_1: int64), (var_2: (bool [])), (var_3: int64), (var_9: int64), (var_8: int64))
    else
        var_5
and method_2((var_0: EnvHeap0)): EnvHeap0 =
    var_0
and method_1((var_0: int64), (var_1: (bool [])), (var_2: int64), (var_3: int64), (var_4: int64)): int64 =
    if (var_3 <= var_2) then
        var_1.[int32 var_4] <- false
        let (var_5: int64) = (var_4 + 1L)
        let (var_6: int64) = (var_3 + 1L)
        method_1((var_0: int64), (var_1: (bool [])), (var_2: int64), (var_6: int64), (var_5: int64))
    else
        var_4
let (var_0: int64) = 10L
let (var_1: int64) = 10L
let (var_2: int64) = (var_0 - 1L)
let (var_3: int64) = (var_1 - 1L)
let (var_4: int64) = (var_3 + 1L)
let (var_5: int64) =
    if (0L > var_4) then
        0L
    else
        var_4
let (var_6: int64) = (var_2 + 1L)
let (var_7: int64) =
    if (0L > var_6) then
        0L
    else
        var_6
let (var_8: int64) = (var_7 * var_5)
let (var_9: (bool [])) = Array.zeroCreate<bool> (System.Convert.ToInt32(var_8))
let (var_10: int64) = 0L
let (var_11: int64) = 0L
let (var_12: int64) = method_0((var_5: int64), (var_3: int64), (var_9: (bool [])), (var_2: int64), (var_11: int64), (var_10: int64))
let (var_13: EnvHeap0) = ({mem_0 = (var_9: (bool [])); mem_1 = (var_2: int64); mem_2 = (var_3: int64)} : EnvHeap0)
method_2((var_13: EnvHeap0))
