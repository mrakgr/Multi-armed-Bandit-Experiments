type EnvHeap0 =
    {
    mem_0: (bool [])
    mem_1: int64
    mem_2: int64
    mem_3: int64
    }
and Env1 =
    struct
    val mem_array_data: EnvHeap0
    val mem_index: Env2
    val mem_set: Env3
    new(arg_mem_array_data, arg_mem_index, arg_mem_set) = {mem_array_data = arg_mem_array_data; mem_index = arg_mem_index; mem_set = arg_mem_set}
    end
and Env2 =
    struct
    val mem_ar: (bool [])
    val mem_x: EnvHeap0
    new(arg_mem_ar, arg_mem_x) = {mem_ar = arg_mem_ar; mem_x = arg_mem_x}
    end
and Env3 =
    struct
    val mem_array_data: EnvHeap0
    new(arg_mem_array_data) = {mem_array_data = arg_mem_array_data}
    end
let rec method_15((var_0: int64), (var_1: int64), (var_2: (bool [])), (var_3: int64), (var_4: int64), (var_5: int64)): int64 =
    if (var_4 <= var_3) then
        let (var_6: int64) = 0L
        let (var_7: int64) = method_16((var_4: int64), (var_2: (bool [])), (var_1: int64), (var_6: int64), (var_5: int64))
        let (var_8: int64) = (var_5 + var_0)
        let (var_9: int64) = (var_4 + 1L)
        method_15((var_0: int64), (var_1: int64), (var_2: (bool [])), (var_3: int64), (var_9: int64), (var_8: int64))
    else
        var_5
and method_16((var_0: int64), (var_1: (bool [])), (var_2: int64), (var_3: int64), (var_4: int64)): int64 =
    if (var_3 <= var_2) then
        var_1.[int32 var_4] <- false
        let (var_5: int64) = (var_4 + 1L)
        let (var_6: int64) = (var_3 + 1L)
        method_16((var_0: int64), (var_1: (bool [])), (var_2: int64), (var_6: int64), (var_5: int64))
    else
        var_4
and method_17((var_0: EnvHeap0), (var_1: (bool []))): Env1 =
    (Env1(var_0, (Env2(var_1, var_0)), (Env3(var_0))))
let (var_0: int64) = 10L
let (var_1: int64) = 10L
let (var_2: int64) = (var_0 - 1L)
let (var_3: int64) = (var_1 - 1L)
let (var_4: int64) = (var_3 + 1L)
let (var_5: int64) =
    if (0L < var_4) then
        var_4
    else
        0L
let (var_6: int64) = (var_2 + 1L)
let (var_7: int64) =
    if (0L < var_6) then
        var_6
    else
        0L
let (var_8: int64) = (var_7 * var_5)
let (var_9: (bool [])) = Array.zeroCreate<bool> (System.Convert.ToInt32(var_8))
let (var_10: int64) = 0L
let (var_11: int64) = 0L
let (var_12: int64) = method_15((var_5: int64), (var_3: int64), (var_9: (bool [])), (var_2: int64), (var_11: int64), (var_10: int64))
let (var_13: EnvHeap0) = ({mem_0 = (var_9: (bool [])); mem_1 = (var_5: int64); mem_2 = (var_2: int64); mem_3 = (var_3: int64)} : EnvHeap0)
let (var_14: (bool [])) = var_13.mem_0
method_17((var_13: EnvHeap0), (var_14: (bool [])))
