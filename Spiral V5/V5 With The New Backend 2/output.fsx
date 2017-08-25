let rec method_14 ((var_0: int64), (var_1: int64)): int64 =
    (var_0 + var_1)
and method_15 ((var_0: int64), (var_1: int64)): int64 =
    (var_0 * var_1)
let (var_16: (int64 ref)) = (ref 0L)
var_16 := 5L
let (var_18: int64) = (!var_16)
let (var_25: (int64 * int64 -> int64)) = method_14
let (var_26: ((int64 * int64 -> int64) ref)) = (ref var_25)
let (var_30: (int64 * int64 -> int64)) = method_15
var_26 := var_30
let (var_32: (int64 * int64 -> int64)) = (!var_26)
let (var_33: (int64 [])) = Array.zeroCreate<int64> (System.Convert.ToInt32(10L))
var_33.[int32 3L] <- 2L
let (var_35: int64) = var_33.[int32 3L]

