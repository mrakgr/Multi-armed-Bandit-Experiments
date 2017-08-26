let rec method_14 ((var_3: string)) ((var_0: int64), (var_1: int64)): int64 =
    let (var_10: (int64 * int64 -> int64)) = method_15((var_1: int64), (var_3: string))
    let (var_14: int64) = 0L
    let (var_15: int64) = method_17((var_14: int64), (var_10: (int64 * int64 -> int64)), (var_3: string), (var_0: int64))
    var_15
and method_15 ((var_2: int64), (var_4: string)) ((var_0: int64), (var_1: int64)): int64 =
    let (var_11: (int64 * int64 -> int64)) = method_16((var_1: int64), (var_2: int64), (var_4: string))
    let (var_15: int64) = 0L
    let (var_16: int64) = method_17((var_15: int64), (var_11: (int64 * int64 -> int64)), (var_4: string), (var_0: int64))
    var_16
and method_16 ((var_2: int64), (var_3: int64), (var_5: string)) ((var_0: int64), (var_1: int64)): int64 =
    let (var_9: int64) = (0L + var_3)
    let (var_10: int64) = (var_9 + var_2)
    let (var_11: int64) = (var_10 + var_1)
    var_11
and method_17((var_0: int64), (var_2: (int64 * int64 -> int64)), (var_4: string), (var_7: int64)): int64 =
    let (var_13: bool) = (var_7 >= 0L)
    let (var_14: int64) = (int64 var_4.Length)
    let (var_15: bool) = (var_7 < var_14)
    let (if_var_1: int64) =
        if (var_13 && var_15) then
            let (var_17: char) = var_4.[int32 var_7]
            let (var_19: int64) = (var_7 + 1L)
            let (var_20: bool) = (var_17 >= '0')
            let (var_21: bool) = (var_17 <= '9')
            let (if_var_2: int64) =
                if (var_20 && var_21) then
                    let (var_23: int64) = System.Convert.ToInt64(var_17)
                    let (var_24: int64) = System.Convert.ToInt64('0')
                    let (var_25: int64) = (var_23 - var_24)
                    let (var_26: int64) = (var_0 * 10L)
                    let (var_27: int64) = (var_26 + var_25)
                    let (var_28: int64) = method_18((var_27: int64), (var_2: (int64 * int64 -> int64)), (var_4: string), (var_19: int64))
                    var_28
                else
                    -1L
            if_var_2
        else
            -1L
    if_var_1
and method_18((var_0: int64), (var_2: (int64 * int64 -> int64)), (var_4: string), (var_7: int64)): int64 =
    let (var_13: bool) = (var_7 >= 0L)
    let (var_14: int64) = (int64 var_4.Length)
    let (var_15: bool) = (var_7 < var_14)
    let (if_var_3: int64) =
        if (var_13 && var_15) then
            let (var_17: char) = var_4.[int32 var_7]
            let (var_19: int64) = (var_7 + 1L)
            let (var_20: bool) = (var_17 >= '0')
            let (var_21: bool) = (var_17 <= '9')
            let (if_var_4: int64) =
                if (var_20 && var_21) then
                    let (var_23: int64) = System.Convert.ToInt64(var_17)
                    let (var_24: int64) = System.Convert.ToInt64('0')
                    let (var_25: int64) = (var_23 - var_24)
                    let (var_26: int64) = (var_0 * 10L)
                    let (var_27: int64) = (var_26 + var_25)
                    let (var_28: int64) = method_18((var_27: int64), (var_2: (int64 * int64 -> int64)), (var_4: string), (var_19: int64))
                    var_28
                else
                    let (var_32: int64) = method_19((var_19: int64), (var_0: int64), (var_2: (int64 * int64 -> int64)), (var_4: string))
                    var_32
            if_var_4
        else
            let (var_36: int64) = method_19((var_7: int64), (var_0: int64), (var_2: (int64 * int64 -> int64)), (var_4: string))
            var_36
    if_var_3
and method_19((var_0: int64), (var_1: int64), (var_2: (int64 * int64 -> int64)), (var_4: string)): int64 =
    let (var_9: bool) = (var_0 >= 0L)
    let (var_10: int64) = (int64 var_4.Length)
    let (var_11: bool) = (var_0 < var_10)
    let (if_var_5: int64) =
        if (var_9 && var_11) then
            let (var_13: char) = var_4.[int32 var_0]
            let (var_15: int64) = (var_0 + 1L)
            let (var_16: bool) = (var_13 = ' ')
            let (var_17: bool) = (var_13 = '\n')
            let (var_18: bool) = (var_13 = '\r')
            let (var_19: bool) = (var_17 || var_18)
            let (if_var_6: int64) =
                if (var_16 || var_19) then
                    let (var_20: int64) = method_19((var_15: int64), (var_1: int64), (var_2: (int64 * int64 -> int64)), (var_4: string))
                    var_20
                else
                    let (var_22: int64) = (var_15 - 1L)
                    let (var_26: int64) = var_2(var_22, var_1)
                    var_26
            if_var_6
        else
            let (var_32: int64) = var_2(var_0, var_1)
            var_32
    if_var_5
let (var_24: string) = "1 2 3"
let (var_27: int64) = 0L
let (var_28: int64) = var_27
let (var_32: (int64 * int64 -> int64)) = method_14((var_24: string))
let (var_36: int64) = 0L
let (var_37: int64) = method_17((var_36: int64), (var_32: (int64 * int64 -> int64)), (var_24: string), (var_28: int64))
var_37
