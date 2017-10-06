let rec method_15((var_0: (int64 [])), (var_1: int64), (var_2: int64)): int64 =
    if (var_1 < 10L) then
        let (var_3: int64) = 0L
        let (var_4: int64) = method_16((var_1: int64), (var_0: (int64 [])), (var_3: int64), (var_2: int64))
        let (var_5: int64) = (var_2 + 10L)
        let (var_6: int64) = (var_1 + 1L)
        method_15((var_0: (int64 [])), (var_6: int64), (var_5: int64))
    else
        var_2
and method_16((var_0: int64), (var_1: (int64 [])), (var_2: int64), (var_3: int64)): int64 =
    if (var_2 < 10L) then
        var_1.[int32 var_3] <- (var_2 * var_0)
        let (var_4: int64) = (var_3 + 1L)
        let (var_5: int64) = (var_2 + 1L)
        method_16((var_0: int64), (var_1: (int64 [])), (var_5: int64), (var_4: int64))
    else
        var_3
let (var_0: (int64 [])) = Array.zeroCreate<int64> (System.Convert.ToInt32(100L))
let (var_1: int64) = 0L
let (var_2: int64) = 0L
let (var_3: int64) = method_15((var_0: (int64 [])), (var_2: int64), (var_1: int64))
let (var_4: int64) = var_0.[int32 22L]
let (var_5: int64) = (var_4 + 100L)
var_0.[int32 22L] <- var_5
var_0.[int32 22L]
