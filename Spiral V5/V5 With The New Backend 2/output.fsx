let rec method_14((var_1: int64), (var_2: string)): int64 =
    let (var_29: bool) = (var_1 >= 0L)
    let (var_30: int64) = (int64 var_2.Length)
    let (var_31: bool) = (var_1 < var_30)
    let (if_var_1: int64) =
        if (var_29 && var_31) then
            let (var_32: char) = var_2.[int32 var_1]
            let (var_33: int64) = (var_1 + 1L)
            let (var_34: bool) = (var_32 >= '0')
            let (var_35: bool) = (var_32 <= '9')
            let (if_var_2: int64) =
                if (var_34 && var_35) then
                    let (var_36: int64) = System.Convert.ToInt64(var_32)
                    let (var_37: int64) = System.Convert.ToInt64('0')
                    let (var_38: int64) = (var_36 - var_37)
                    let (var_39: int64) = (0L + var_38)
                    let (var_42: int64) = method_15((var_1: int64), (var_2: string), (var_39: int64), (var_33: int64))
                    var_42
                else
                    -1L
            if_var_2
        else
            -1L
    if_var_1
and method_15((var_1: int64), (var_2: string), (var_5: int64), (var_6: int64)): int64 =
    let (var_31: bool) = (var_6 >= 0L)
    let (var_32: int64) = (int64 var_2.Length)
    let (var_33: bool) = (var_6 < var_32)
    let (if_var_3: int64) =
        if (var_31 && var_33) then
            let (var_34: char) = var_2.[int32 var_6]
            let (var_35: int64) = (var_6 + 1L)
            let (var_36: bool) = (var_34 >= '0')
            let (var_37: bool) = (var_34 <= '9')
            let (if_var_4: int64) =
                if (var_36 && var_37) then
                    let (var_38: int64) = System.Convert.ToInt64(var_34)
                    let (var_39: int64) = System.Convert.ToInt64('0')
                    let (var_40: int64) = (var_38 - var_39)
                    let (var_41: int64) = (var_5 * 10L)
                    let (var_42: int64) = (var_41 + var_40)
                    let (var_45: int64) = method_15((var_1: int64), (var_2: string), (var_42: int64), (var_35: int64))
                    var_45
                else
                    let (var_58: int64) = method_16((var_1: int64), (var_2: string), (var_5: int64), (var_35: int64))
                    var_58
            if_var_4
        else
            let (var_71: int64) = method_16((var_1: int64), (var_2: string), (var_5: int64), (var_6: int64))
            var_71
    if_var_3
and method_16((var_1: int64), (var_2: string), (var_3: int64), (var_6: int64)): int64 =
    let (var_31: bool) = (var_6 >= 0L)
    let (var_32: int64) = (int64 var_2.Length)
    let (var_33: bool) = (var_6 < var_32)
    let (if_var_5: int64) =
        if (var_31 && var_33) then
            let (var_34: char) = var_2.[int32 var_6]
            let (var_35: int64) = (var_6 + 1L)
            let (var_36: bool) = (var_34 >= '0')
            let (var_37: bool) = (var_34 <= '9')
            let (if_var_6: int64) =
                if (var_36 && var_37) then
                    let (var_38: int64) = System.Convert.ToInt64(var_34)
                    let (var_39: int64) = System.Convert.ToInt64('0')
                    let (var_40: int64) = (var_38 - var_39)
                    let (var_41: int64) = (0L + var_40)
                    let (var_44: int64) = method_17((var_1: int64), (var_2: string), (var_3: int64), (var_6: int64), (var_41: int64), (var_35: int64))
                    var_44
                else
                    -1L
            if_var_6
        else
            -1L
    if_var_5
and method_17((var_1: int64), (var_2: string), (var_3: int64), (var_6: int64), (var_7: int64), (var_8: int64)): int64 =
    let (var_33: bool) = (var_8 >= 0L)
    let (var_34: int64) = (int64 var_2.Length)
    let (var_35: bool) = (var_8 < var_34)
    let (if_var_7: int64) =
        if (var_33 && var_35) then
            let (var_36: char) = var_2.[int32 var_8]
            let (var_37: int64) = (var_8 + 1L)
            let (var_38: bool) = (var_36 >= '0')
            let (var_39: bool) = (var_36 <= '9')
            let (if_var_8: int64) =
                if (var_38 && var_39) then
                    let (var_40: int64) = System.Convert.ToInt64(var_36)
                    let (var_41: int64) = System.Convert.ToInt64('0')
                    let (var_42: int64) = (var_40 - var_41)
                    let (var_43: int64) = (var_7 * 10L)
                    let (var_44: int64) = (var_43 + var_42)
                    let (var_47: int64) = method_17((var_1: int64), (var_2: string), (var_3: int64), (var_6: int64), (var_44: int64), (var_37: int64))
                    var_47
                else
                    let (var_60: int64) = method_18((var_1: int64), (var_2: string), (var_3: int64), (var_7: int64), (var_37: int64))
                    var_60
            if_var_8
        else
            let (var_73: int64) = method_18((var_1: int64), (var_2: string), (var_3: int64), (var_7: int64), (var_8: int64))
            var_73
    if_var_7
and method_18((var_1: int64), (var_2: string), (var_3: int64), (var_4: int64), (var_7: int64)): int64 =
    let (var_32: bool) = (var_7 >= 0L)
    let (var_33: int64) = (int64 var_2.Length)
    let (var_34: bool) = (var_7 < var_33)
    let (if_var_9: int64) =
        if (var_32 && var_34) then
            let (var_35: char) = var_2.[int32 var_7]
            let (var_36: int64) = (var_7 + 1L)
            let (var_37: bool) = (var_35 >= '0')
            let (var_38: bool) = (var_35 <= '9')
            let (if_var_10: int64) =
                if (var_37 && var_38) then
                    let (var_39: int64) = System.Convert.ToInt64(var_35)
                    let (var_40: int64) = System.Convert.ToInt64('0')
                    let (var_41: int64) = (var_39 - var_40)
                    let (var_42: int64) = (0L + var_41)
                    let (var_45: int64) = method_19((var_1: int64), (var_2: string), (var_3: int64), (var_4: int64), (var_7: int64), (var_42: int64), (var_36: int64))
                    var_45
                else
                    -1L
            if_var_10
        else
            -1L
    if_var_9
and method_19((var_1: int64), (var_2: string), (var_3: int64), (var_4: int64), (var_7: int64), (var_8: int64), (var_9: int64)): int64 =
    let (var_34: bool) = (var_9 >= 0L)
    let (var_35: int64) = (int64 var_2.Length)
    let (var_36: bool) = (var_9 < var_35)
    let (if_var_11: int64) =
        if (var_34 && var_36) then
            let (var_37: char) = var_2.[int32 var_9]
            let (var_38: int64) = (var_9 + 1L)
            let (var_39: bool) = (var_37 >= '0')
            let (var_40: bool) = (var_37 <= '9')
            let (if_var_12: int64) =
                if (var_39 && var_40) then
                    let (var_41: int64) = System.Convert.ToInt64(var_37)
                    let (var_42: int64) = System.Convert.ToInt64('0')
                    let (var_43: int64) = (var_41 - var_42)
                    let (var_44: int64) = (var_8 * 10L)
                    let (var_45: int64) = (var_44 + var_43)
                    let (var_48: int64) = method_19((var_1: int64), (var_2: string), (var_3: int64), (var_4: int64), (var_7: int64), (var_45: int64), (var_38: int64))
                    var_48
                else
                    let (var_61: int64) = method_20((var_1: int64), (var_2: string), (var_3: int64), (var_4: int64), (var_8: int64), (var_38: int64))
                    var_61
            if_var_12
        else
            let (var_74: int64) = method_20((var_1: int64), (var_2: string), (var_3: int64), (var_4: int64), (var_8: int64), (var_9: int64))
            var_74
    if_var_11
and method_20((var_1: int64), (var_2: string), (var_3: int64), (var_4: int64), (var_5: int64), (var_6: int64)): int64 =
    let (var_33: bool) = (var_6 >= 0L)
    let (var_34: int64) = (int64 var_2.Length)
    let (var_35: bool) = (var_6 < var_34)
    let (if_var_13: int64) =
        if (var_33 && var_35) then
            let (var_36: char) = var_2.[int32 var_6]
            let (var_37: int64) = (var_6 + 1L)
            let (var_38: bool) = (var_36 >= '0')
            let (var_39: bool) = (var_36 <= '9')
            let (if_var_14: int64) =
                if (var_38 && var_39) then
                    let (var_40: int64) = System.Convert.ToInt64(var_36)
                    let (var_41: int64) = System.Convert.ToInt64('0')
                    let (var_42: int64) = (var_40 - var_41)
                    let (var_43: int64) = (0L + var_42)
                    let (var_46: int64) = method_21((var_1: int64), (var_2: string), (var_3: int64), (var_4: int64), (var_5: int64), (var_6: int64), (var_43: int64), (var_37: int64))
                    var_46
                else
                    -1L
            if_var_14
        else
            -1L
    if_var_13
and method_21((var_1: int64), (var_2: string), (var_3: int64), (var_4: int64), (var_5: int64), (var_6: int64), (var_7: int64), (var_10: int64)): int64 =
    let (var_35: bool) = (var_10 >= 0L)
    let (var_36: int64) = (int64 var_2.Length)
    let (var_37: bool) = (var_10 < var_36)
    let (if_var_15: int64) =
        if (var_35 && var_37) then
            let (var_38: char) = var_2.[int32 var_10]
            let (var_39: int64) = (var_10 + 1L)
            let (var_40: bool) = (var_38 >= '0')
            let (var_41: bool) = (var_38 <= '9')
            let (if_var_16: int64) =
                if (var_40 && var_41) then
                    let (var_42: int64) = System.Convert.ToInt64(var_38)
                    let (var_43: int64) = System.Convert.ToInt64('0')
                    let (var_44: int64) = (var_42 - var_43)
                    let (var_45: int64) = (var_7 * 10L)
                    let (var_46: int64) = (var_45 + var_44)
                    let (var_49: int64) = method_21((var_1: int64), (var_2: string), (var_3: int64), (var_4: int64), (var_5: int64), (var_6: int64), (var_46: int64), (var_39: int64))
                    var_49
                else
                    let (var_51: int64) = (0L + var_3)
                    let (var_52: int64) = (var_51 + var_4)
                    let (var_53: int64) = (var_52 + var_5)
                    let (var_54: int64) = (var_53 + var_7)
                    var_54
            if_var_16
        else
            let (var_56: int64) = (0L + var_3)
            let (var_57: int64) = (var_56 + var_4)
            let (var_58: int64) = (var_57 + var_5)
            let (var_59: int64) = (var_58 + var_7)
            var_59
    if_var_15
let (var_23: string) = System.Console.ReadLine()
let (var_24: int64) = 0L
let (var_25: int64) = var_24
let (var_46: int64) = method_14((var_25: int64), (var_23: string))
var_46
