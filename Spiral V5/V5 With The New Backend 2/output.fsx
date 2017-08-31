let rec method_14((var_0: int64), (var_3: string), (var_6: int64)): int64 =
    let (var_12: bool) = (var_6 >= 0L)
    let (var_13: int64) = (int64 var_3.Length)
    let (var_14: bool) = (var_6 < var_13)
    let (if_var_1: int64) =
        if (var_12 && var_14) then
            let (var_16: char) = var_3.[int32 var_6]
            let (var_18: int64) = (var_6 + 1L)
            let (var_19: bool) = (var_16 >= '0')
            let (var_20: bool) = (var_16 <= '9')
            let (if_var_2: int64) =
                if (var_19 && var_20) then
                    let (var_22: int64) = System.Convert.ToInt64(var_16)
                    let (var_23: int64) = System.Convert.ToInt64('0')
                    let (var_24: int64) = (var_22 - var_23)
                    let (var_25: int64) = (var_0 * 10L)
                    let (var_26: int64) = (var_25 + var_24)
                    let (var_27: int64) = method_15((var_26: int64), (var_3: string), (var_18: int64))
                    var_27
                else
                    -1L
            if_var_2
        else
            -1L
    if_var_1
and method_15((var_0: int64), (var_3: string), (var_6: int64)): int64 =
    let (var_12: bool) = (var_6 >= 0L)
    let (var_13: int64) = (int64 var_3.Length)
    let (var_14: bool) = (var_6 < var_13)
    let (if_var_3: int64) =
        if (var_12 && var_14) then
            let (var_16: char) = var_3.[int32 var_6]
            let (var_18: int64) = (var_6 + 1L)
            let (var_19: bool) = (var_16 >= '0')
            let (var_20: bool) = (var_16 <= '9')
            let (if_var_4: int64) =
                if (var_19 && var_20) then
                    let (var_22: int64) = System.Convert.ToInt64(var_16)
                    let (var_23: int64) = System.Convert.ToInt64('0')
                    let (var_24: int64) = (var_22 - var_23)
                    let (var_25: int64) = (var_0 * 10L)
                    let (var_26: int64) = (var_25 + var_24)
                    let (var_27: int64) = method_15((var_26: int64), (var_3: string), (var_18: int64))
                    var_27
                else
                    let (var_31: int64) = 0L
                    let (var_32: int64) = method_16((var_31: int64), (var_0: int64), (var_3: string), (var_18: int64))
                    var_32
            if_var_4
        else
            let (var_36: int64) = 0L
            let (var_37: int64) = method_16((var_36: int64), (var_0: int64), (var_3: string), (var_6: int64))
            var_37
    if_var_3
and method_16((var_0: int64), (var_2: int64), (var_4: string), (var_7: int64)): int64 =
    let (var_13: bool) = (var_7 >= 0L)
    let (var_14: int64) = (int64 var_4.Length)
    let (var_15: bool) = (var_7 < var_14)
    let (if_var_5: int64) =
        if (var_13 && var_15) then
            let (var_17: char) = var_4.[int32 var_7]
            let (var_19: int64) = (var_7 + 1L)
            let (var_20: bool) = (var_17 >= '0')
            let (var_21: bool) = (var_17 <= '9')
            let (if_var_6: int64) =
                if (var_20 && var_21) then
                    let (var_23: int64) = System.Convert.ToInt64(var_17)
                    let (var_24: int64) = System.Convert.ToInt64('0')
                    let (var_25: int64) = (var_23 - var_24)
                    let (var_26: int64) = (var_0 * 10L)
                    let (var_27: int64) = (var_26 + var_25)
                    let (var_28: int64) = method_17((var_27: int64), (var_2: int64), (var_4: string), (var_19: int64))
                    var_28
                else
                    -1L
            if_var_6
        else
            -1L
    if_var_5
and method_17((var_0: int64), (var_2: int64), (var_4: string), (var_7: int64)): int64 =
    let (var_13: bool) = (var_7 >= 0L)
    let (var_14: int64) = (int64 var_4.Length)
    let (var_15: bool) = (var_7 < var_14)
    let (if_var_7: int64) =
        if (var_13 && var_15) then
            let (var_17: char) = var_4.[int32 var_7]
            let (var_19: int64) = (var_7 + 1L)
            let (var_20: bool) = (var_17 >= '0')
            let (var_21: bool) = (var_17 <= '9')
            let (if_var_8: int64) =
                if (var_20 && var_21) then
                    let (var_23: int64) = System.Convert.ToInt64(var_17)
                    let (var_24: int64) = System.Convert.ToInt64('0')
                    let (var_25: int64) = (var_23 - var_24)
                    let (var_26: int64) = (var_0 * 10L)
                    let (var_27: int64) = (var_26 + var_25)
                    let (var_28: int64) = method_17((var_27: int64), (var_2: int64), (var_4: string), (var_19: int64))
                    var_28
                else
                    let (var_32: int64) = 0L
                    let (var_33: int64) = method_18((var_32: int64), (var_0: int64), (var_2: int64), (var_4: string), (var_19: int64))
                    var_33
            if_var_8
        else
            let (var_37: int64) = 0L
            let (var_38: int64) = method_18((var_37: int64), (var_0: int64), (var_2: int64), (var_4: string), (var_7: int64))
            var_38
    if_var_7
and method_18((var_0: int64), (var_2: int64), (var_3: int64), (var_5: string), (var_8: int64)): int64 =
    let (var_14: bool) = (var_8 >= 0L)
    let (var_15: int64) = (int64 var_5.Length)
    let (var_16: bool) = (var_8 < var_15)
    let (if_var_9: int64) =
        if (var_14 && var_16) then
            let (var_18: char) = var_5.[int32 var_8]
            let (var_20: int64) = (var_8 + 1L)
            let (var_21: bool) = (var_18 >= '0')
            let (var_22: bool) = (var_18 <= '9')
            let (if_var_10: int64) =
                if (var_21 && var_22) then
                    let (var_24: int64) = System.Convert.ToInt64(var_18)
                    let (var_25: int64) = System.Convert.ToInt64('0')
                    let (var_26: int64) = (var_24 - var_25)
                    let (var_27: int64) = (var_0 * 10L)
                    let (var_28: int64) = (var_27 + var_26)
                    let (var_29: int64) = method_19((var_28: int64), (var_2: int64), (var_3: int64), (var_5: string), (var_20: int64))
                    var_29
                else
                    -1L
            if_var_10
        else
            -1L
    if_var_9
and method_19((var_0: int64), (var_2: int64), (var_3: int64), (var_5: string), (var_8: int64)): int64 =
    let (var_14: bool) = (var_8 >= 0L)
    let (var_15: int64) = (int64 var_5.Length)
    let (var_16: bool) = (var_8 < var_15)
    let (if_var_11: int64) =
        if (var_14 && var_16) then
            let (var_18: char) = var_5.[int32 var_8]
            let (var_20: int64) = (var_8 + 1L)
            let (var_21: bool) = (var_18 >= '0')
            let (var_22: bool) = (var_18 <= '9')
            let (if_var_12: int64) =
                if (var_21 && var_22) then
                    let (var_24: int64) = System.Convert.ToInt64(var_18)
                    let (var_25: int64) = System.Convert.ToInt64('0')
                    let (var_26: int64) = (var_24 - var_25)
                    let (var_27: int64) = (var_0 * 10L)
                    let (var_28: int64) = (var_27 + var_26)
                    let (var_29: int64) = method_19((var_28: int64), (var_2: int64), (var_3: int64), (var_5: string), (var_20: int64))
                    var_29
                else
                    let (var_33: int64) = 0L
                    let (var_34: int64) = method_20((var_33: int64), (var_0: int64), (var_2: int64), (var_3: int64), (var_5: string), (var_20: int64))
                    var_34
            if_var_12
        else
            let (var_38: int64) = 0L
            let (var_39: int64) = method_20((var_38: int64), (var_0: int64), (var_2: int64), (var_3: int64), (var_5: string), (var_8: int64))
            var_39
    if_var_11
and method_20((var_0: int64), (var_2: int64), (var_3: int64), (var_4: int64), (var_6: string), (var_9: int64)): int64 =
    let (var_15: bool) = (var_9 >= 0L)
    let (var_16: int64) = (int64 var_6.Length)
    let (var_17: bool) = (var_9 < var_16)
    let (if_var_13: int64) =
        if (var_15 && var_17) then
            let (var_19: char) = var_6.[int32 var_9]
            let (var_21: int64) = (var_9 + 1L)
            let (var_22: bool) = (var_19 >= '0')
            let (var_23: bool) = (var_19 <= '9')
            let (if_var_14: int64) =
                if (var_22 && var_23) then
                    let (var_25: int64) = System.Convert.ToInt64(var_19)
                    let (var_26: int64) = System.Convert.ToInt64('0')
                    let (var_27: int64) = (var_25 - var_26)
                    let (var_28: int64) = (var_0 * 10L)
                    let (var_29: int64) = (var_28 + var_27)
                    let (var_30: int64) = method_21((var_29: int64), (var_2: int64), (var_3: int64), (var_4: int64), (var_6: string), (var_21: int64))
                    var_30
                else
                    -1L
            if_var_14
        else
            -1L
    if_var_13
and method_21((var_0: int64), (var_2: int64), (var_3: int64), (var_4: int64), (var_6: string), (var_9: int64)): int64 =
    let (var_15: bool) = (var_9 >= 0L)
    let (var_16: int64) = (int64 var_6.Length)
    let (var_17: bool) = (var_9 < var_16)
    let (if_var_15: int64) =
        if (var_15 && var_17) then
            let (var_19: char) = var_6.[int32 var_9]
            let (var_21: int64) = (var_9 + 1L)
            let (var_22: bool) = (var_19 >= '0')
            let (var_23: bool) = (var_19 <= '9')
            let (if_var_16: int64) =
                if (var_22 && var_23) then
                    let (var_25: int64) = System.Convert.ToInt64(var_19)
                    let (var_26: int64) = System.Convert.ToInt64('0')
                    let (var_27: int64) = (var_25 - var_26)
                    let (var_28: int64) = (var_0 * 10L)
                    let (var_29: int64) = (var_28 + var_27)
                    let (var_30: int64) = method_21((var_29: int64), (var_2: int64), (var_3: int64), (var_4: int64), (var_6: string), (var_21: int64))
                    var_30
                else
                    let (var_36: int64) = (0L + var_4)
                    let (var_37: int64) = (var_36 + var_3)
                    let (var_38: int64) = (var_37 + var_2)
                    let (var_39: int64) = (var_38 + var_0)
                    var_39
            if_var_16
        else
            let (var_45: int64) = (0L + var_4)
            let (var_46: int64) = (var_45 + var_3)
            let (var_47: int64) = (var_46 + var_2)
            let (var_48: int64) = (var_47 + var_0)
            var_48
    if_var_15
let (var_23: string) = System.Console.ReadLine()
let (var_24: int64) = 0L
let (var_25: int64) = var_24
let (var_27: int64) = 0L
let (var_28: int64) = method_14((var_27: int64), (var_23: string), (var_25: int64))
var_28
