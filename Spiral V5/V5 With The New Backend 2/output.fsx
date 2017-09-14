type EnvHeap0 =
    {
    mem_0: EnvHeap1
    }
and EnvHeap1 =
    {
    mem_0: int64
    mem_1: int64
    mem_2: EnvStack2
    }
and EnvStack2 =
    struct
    val mem_0: int64
    val mem_1: int64
    new(arg_mem_0, arg_mem_1) = {mem_0 = arg_mem_0; mem_1 = arg_mem_1}
    end
let rec method_14((var_0: int64), (var_1: int64), (var_2: EnvHeap0)): int64 =
    let (var_3: EnvHeap1) = var_2.mem_0
    let (var_4: int64) = var_3.mem_0
    let (var_5: int64) = var_3.mem_1
    let (var_6: EnvStack2) = var_3.mem_2
    let (var_7: int64) = var_6.mem_0
    let (var_8: int64) = var_6.mem_1
    let (var_9: int64) = (var_4 + var_5)
    let (var_10: int64) = (var_9 + var_0)
    let (var_11: int64) = (var_10 + var_1)
    let (var_12: int64) = (var_11 + var_7)
    (var_12 + var_8)
let (var_0: int64) = 1L
let (var_1: int64) = 2L
let (var_2: int64) = 3L
let (var_3: int64) = 4L
let (var_4: EnvStack2) = EnvStack2((var_2: int64), (var_3: int64))
let (var_5: EnvHeap1) = ({mem_0 = (var_0: int64); mem_1 = (var_1: int64); mem_2 = (var_4: EnvStack2)} : EnvHeap1)
let (var_6: int64) = var_5.mem_0
let (var_7: int64) = var_5.mem_1
let (var_8: EnvStack2) = var_5.mem_2
let (var_9: int64) = var_8.mem_0
let (var_10: int64) = var_8.mem_1
let (var_11: int64) = 9L
let (var_12: EnvStack2) = EnvStack2((var_11: int64), (var_10: int64))
let (var_13: EnvHeap1) = ({mem_0 = (var_6: int64); mem_1 = (var_7: int64); mem_2 = (var_12: EnvStack2)} : EnvHeap1)
let (var_14: EnvHeap0) = ({mem_0 = (var_13: EnvHeap1)} : EnvHeap0)
let (var_15: int64) = 3L
let (var_16: int64) = 4L
method_14((var_15: int64), (var_16: int64), (var_14: EnvHeap0))
