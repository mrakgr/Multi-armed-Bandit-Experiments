type EnvHeap0 =
    {
    mem_0: EnvHeap1
    }
and EnvHeap1 =
    {
    mem_0: int64
    mem_1: int64
    mem_2: int64
    mem_3: int64
    }
let rec method_14((var_0: int64), (var_1: int64), (var_2: EnvHeap0)): int64 =
    let (var_3: EnvHeap1) = var_2.mem_0
    let (var_4: int64) = var_3.mem_0
    let (var_5: int64) = var_3.mem_1
    let (var_6: int64) = var_3.mem_2
    let (var_7: int64) = var_3.mem_3
    let (var_8: int64) = (var_4 + var_5)
    let (var_9: int64) = (var_8 + var_0)
    let (var_10: int64) = (var_9 + var_1)
    let (var_11: int64) = (var_10 + var_6)
    (var_11 + var_7)
let (var_0: int64) = 1L
let (var_1: int64) = 2L
let (var_2: int64) = 3L
let (var_3: int64) = 4L
let (var_4: EnvHeap1) = ({mem_0 = (var_0: int64); mem_1 = (var_1: int64); mem_2 = (var_2: int64); mem_3 = (var_3: int64)} : EnvHeap1)
let (var_5: int64) = var_4.mem_0
let (var_6: int64) = var_4.mem_1
let (var_7: int64) = var_4.mem_2
let (var_8: int64) = var_4.mem_3
let (var_9: int64) = 6L
let (var_10: EnvHeap1) = ({mem_0 = (var_5: int64); mem_1 = (var_6: int64); mem_2 = (var_9: int64); mem_3 = (var_8: int64)} : EnvHeap1)
let (var_11: EnvHeap0) = ({mem_0 = (var_4: EnvHeap1)} : EnvHeap0)
let (var_12: int64) = 3L
let (var_13: int64) = 4L
method_14((var_12: int64), (var_13: int64), (var_11: EnvHeap0))
