let rec method_15((var_0: (int64 [])), (var_1: int64)): unit =
    if (var_1 < 4L) then
        var_0.[int32 var_1] <- var_1
        let (var_2: int64) = (var_1 + 1L)
        method_15((var_0: (int64 [])), (var_2: int64))
    else
        ()
and method_16((var_0: (int64 [])), (var_1: (int64 [])), (var_2: int64), (var_3: int64)): int64 =
    let (var_4: int64) = var_0.LongLength
    if (var_2 < var_4) then
        let (var_5: int64) = (var_2 + 1L)
        let (var_6: int64) = var_0.[int32 var_2]
        var_1.[int32 var_3] <- var_6
        let (var_7: int64) = (var_3 + 1L)
        method_16((var_0: (int64 [])), (var_1: (int64 [])), (var_5: int64), (var_7: int64))
    else
        var_3
let (var_0: (int64 [])) = Array.zeroCreate<int64> (System.Convert.ToInt32(4L))
let (var_1: int64) = 0L
method_15((var_0: (int64 [])), (var_1: int64))
let (var_2: (int64 [])) = Array.zeroCreate<int64> (System.Convert.ToInt32(4L))
let (var_3: int64) = 0L
method_15((var_2: (int64 [])), (var_3: int64))
let (var_4: (int64 [])) = Array.zeroCreate<int64> (System.Convert.ToInt32(4L))
let (var_5: int64) = 0L
method_15((var_4: (int64 [])), (var_5: int64))
let (var_6: int64) = var_0.LongLength
let (var_7: int64) = (0L + var_6)
let (var_8: int64) = var_2.LongLength
let (var_9: int64) = (var_7 + var_8)
let (var_10: int64) = var_4.LongLength
let (var_11: int64) = (var_9 + var_10)
let (var_12: (int64 [])) = Array.zeroCreate<int64> (System.Convert.ToInt32(var_11))
let (var_13: int64) = 0L
let (var_14: int64) = 0L
let (var_15: int64) = method_16((var_0: (int64 [])), (var_12: (int64 [])), (var_13: int64), (var_14: int64))
let (var_16: int64) = 0L
let (var_17: int64) = method_16((var_2: (int64 [])), (var_12: (int64 [])), (var_16: int64), (var_15: int64))
let (var_18: int64) = 0L
let (var_19: int64) = method_16((var_4: (int64 [])), (var_12: (int64 [])), (var_18: int64), (var_17: int64))
var_12
