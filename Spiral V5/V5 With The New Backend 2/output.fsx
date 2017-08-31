let rec method_14((var_0: string), (var_15: int64)): int64 =
    let (var_40: bool) = (var_15 >= 0L)
    let (var_41: int64) = (int64 var_0.Length)
    let (var_42: bool) = (var_15 < var_41)
    let (if_var_1: int64) =
        if (var_40 && var_42) then
            let (var_43: char) = var_0.[int32 var_15]
            let (var_44: int64) = (var_15 + 1L)
            let (var_45: bool) = (var_43 >= '0')
            let (var_46: bool) = (var_43 <= '9')
            let (if_var_2: int64) =
                if (var_45 && var_46) then
                    let (var_47: int64) = System.Convert.ToInt64(var_43)
                    let (var_48: int64) = System.Convert.ToInt64('0')
                    let (var_49: int64) = (var_47 - var_48)
                    let (var_50: int64) = (0L + var_49)
                    let (var_53: int64) = method_15((var_0: string), (var_15: int64), (var_50: int64), (var_44: int64))
                    var_53
                else
                    -1L
            if_var_2
        else
            -1L
    if_var_1
and method_15((var_0: string), (var_15: int64), (var_16: int64), (var_17: int64)): int64 =
    let (var_42: bool) = (var_17 >= 0L)
    let (var_43: int64) = (int64 var_0.Length)
    let (var_44: bool) = (var_17 < var_43)
    let (if_var_3: int64) =
        if (var_42 && var_44) then
            let (var_45: char) = var_0.[int32 var_17]
            let (var_46: int64) = (var_17 + 1L)
            let (var_47: bool) = (var_45 >= '0')
            let (var_48: bool) = (var_45 <= '9')
            let (if_var_4: int64) =
                if (var_47 && var_48) then
                    let (var_49: int64) = System.Convert.ToInt64(var_45)
                    let (var_50: int64) = System.Convert.ToInt64('0')
                    let (var_51: int64) = (var_49 - var_50)
                    let (var_52: int64) = (var_16 * 10L)
                    let (var_53: int64) = (var_52 + var_51)
                    let (var_56: int64) = method_15((var_0: string), (var_15: int64), (var_53: int64), (var_46: int64))
                    var_56
                else
                    let (var_69: int64) = method_16((var_0: string), (var_15: int64), (var_16: int64), (var_46: int64))
                    var_69
            if_var_4
        else
            let (var_82: int64) = method_16((var_0: string), (var_15: int64), (var_16: int64), (var_17: int64))
            var_82
    if_var_3
and method_16((var_0: string), (var_15: int64), (var_16: int64), (var_17: int64)): int64 =
    let (var_42: bool) = (var_17 >= 0L)
    let (var_43: int64) = (int64 var_0.Length)
    let (var_44: bool) = (var_17 < var_43)
    let (if_var_5: int64) =
        if (var_42 && var_44) then
            let (var_45: char) = var_0.[int32 var_17]
            let (var_46: int64) = (var_17 + 1L)
            let (var_47: bool) = (var_45 >= '0')
            let (var_48: bool) = (var_45 <= '9')
            let (if_var_6: int64) =
                if (var_47 && var_48) then
                    let (var_49: int64) = System.Convert.ToInt64(var_45)
                    let (var_50: int64) = System.Convert.ToInt64('0')
                    let (var_51: int64) = (var_49 - var_50)
                    let (var_52: int64) = (0L + var_51)
                    let (var_55: int64) = method_17((var_0: string), (var_15: int64), (var_16: int64), (var_17: int64), (var_52: int64), (var_46: int64))
                    var_55
                else
                    -1L
            if_var_6
        else
            -1L
    if_var_5
and method_17((var_0: string), (var_15: int64), (var_16: int64), (var_17: int64), (var_18: int64), (var_19: int64)): int64 =
    let (var_44: bool) = (var_19 >= 0L)
    let (var_45: int64) = (int64 var_0.Length)
    let (var_46: bool) = (var_19 < var_45)
    let (if_var_7: int64) =
        if (var_44 && var_46) then
            let (var_47: char) = var_0.[int32 var_19]
            let (var_48: int64) = (var_19 + 1L)
            let (var_49: bool) = (var_47 >= '0')
            let (var_50: bool) = (var_47 <= '9')
            let (if_var_8: int64) =
                if (var_49 && var_50) then
                    let (var_51: int64) = System.Convert.ToInt64(var_47)
                    let (var_52: int64) = System.Convert.ToInt64('0')
                    let (var_53: int64) = (var_51 - var_52)
                    let (var_54: int64) = (var_18 * 10L)
                    let (var_55: int64) = (var_54 + var_53)
                    let (var_58: int64) = method_17((var_0: string), (var_15: int64), (var_16: int64), (var_17: int64), (var_55: int64), (var_48: int64))
                    var_58
                else
                    let (var_71: int64) = method_18((var_0: string), (var_15: int64), (var_16: int64), (var_18: int64), (var_48: int64))
                    var_71
            if_var_8
        else
            let (var_84: int64) = method_18((var_0: string), (var_15: int64), (var_16: int64), (var_18: int64), (var_19: int64))
            var_84
    if_var_7
and method_18((var_0: string), (var_15: int64), (var_16: int64), (var_17: int64), (var_18: int64)): int64 =
    let (var_43: bool) = (var_18 >= 0L)
    let (var_44: int64) = (int64 var_0.Length)
    let (var_45: bool) = (var_18 < var_44)
    let (if_var_9: int64) =
        if (var_43 && var_45) then
            let (var_46: char) = var_0.[int32 var_18]
            let (var_47: int64) = (var_18 + 1L)
            let (var_48: bool) = (var_46 >= '0')
            let (var_49: bool) = (var_46 <= '9')
            let (if_var_10: int64) =
                if (var_48 && var_49) then
                    let (var_50: int64) = System.Convert.ToInt64(var_46)
                    let (var_51: int64) = System.Convert.ToInt64('0')
                    let (var_52: int64) = (var_50 - var_51)
                    let (var_53: int64) = (0L + var_52)
                    let (var_56: int64) = method_19((var_0: string), (var_15: int64), (var_16: int64), (var_17: int64), (var_18: int64), (var_53: int64), (var_47: int64))
                    var_56
                else
                    -1L
            if_var_10
        else
            -1L
    if_var_9
and method_19((var_0: string), (var_15: int64), (var_16: int64), (var_17: int64), (var_18: int64), (var_19: int64), (var_20: int64)): int64 =
    let (var_45: bool) = (var_20 >= 0L)
    let (var_46: int64) = (int64 var_0.Length)
    let (var_47: bool) = (var_20 < var_46)
    let (if_var_11: int64) =
        if (var_45 && var_47) then
            let (var_48: char) = var_0.[int32 var_20]
            let (var_49: int64) = (var_20 + 1L)
            let (var_50: bool) = (var_48 >= '0')
            let (var_51: bool) = (var_48 <= '9')
            let (if_var_12: int64) =
                if (var_50 && var_51) then
                    let (var_52: int64) = System.Convert.ToInt64(var_48)
                    let (var_53: int64) = System.Convert.ToInt64('0')
                    let (var_54: int64) = (var_52 - var_53)
                    let (var_55: int64) = (var_19 * 10L)
                    let (var_56: int64) = (var_55 + var_54)
                    let (var_59: int64) = method_19((var_0: string), (var_15: int64), (var_16: int64), (var_17: int64), (var_18: int64), (var_56: int64), (var_49: int64))
                    var_59
                else
                    let (var_72: int64) = method_20((var_0: string), (var_15: int64), (var_16: int64), (var_17: int64), (var_19: int64), (var_49: int64))
                    var_72
            if_var_12
        else
            let (var_85: int64) = method_20((var_0: string), (var_15: int64), (var_16: int64), (var_17: int64), (var_19: int64), (var_20: int64))
            var_85
    if_var_11
and method_20((var_0: string), (var_15: int64), (var_16: int64), (var_17: int64), (var_18: int64), (var_19: int64)): int64 =
    let (var_44: bool) = (var_19 >= 0L)
    let (var_45: int64) = (int64 var_0.Length)
    let (var_46: bool) = (var_19 < var_45)
    let (if_var_13: int64) =
        if (var_44 && var_46) then
            let (var_47: char) = var_0.[int32 var_19]
            let (var_48: int64) = (var_19 + 1L)
            let (var_49: bool) = (var_47 >= '0')
            let (var_50: bool) = (var_47 <= '9')
            let (if_var_14: int64) =
                if (var_49 && var_50) then
                    let (var_51: int64) = System.Convert.ToInt64(var_47)
                    let (var_52: int64) = System.Convert.ToInt64('0')
                    let (var_53: int64) = (var_51 - var_52)
                    let (var_54: int64) = (0L + var_53)
                    let (var_57: int64) = method_21((var_0: string), (var_15: int64), (var_16: int64), (var_17: int64), (var_18: int64), (var_19: int64), (var_54: int64), (var_48: int64))
                    var_57
                else
                    -1L
            if_var_14
        else
            -1L
    if_var_13
and method_21((var_0: string), (var_15: int64), (var_16: int64), (var_17: int64), (var_18: int64), (var_19: int64), (var_20: int64), (var_21: int64)): int64 =
    let (var_46: bool) = (var_21 >= 0L)
    let (var_47: int64) = (int64 var_0.Length)
    let (var_48: bool) = (var_21 < var_47)
    let (if_var_15: int64) =
        if (var_46 && var_48) then
            let (var_49: char) = var_0.[int32 var_21]
            let (var_50: int64) = (var_21 + 1L)
            let (var_51: bool) = (var_49 >= '0')
            let (var_52: bool) = (var_49 <= '9')
            let (if_var_16: int64) =
                if (var_51 && var_52) then
                    let (var_53: int64) = System.Convert.ToInt64(var_49)
                    let (var_54: int64) = System.Convert.ToInt64('0')
                    let (var_55: int64) = (var_53 - var_54)
                    let (var_56: int64) = (var_20 * 10L)
                    let (var_57: int64) = (var_56 + var_55)
                    let (var_60: int64) = method_21((var_0: string), (var_15: int64), (var_16: int64), (var_17: int64), (var_18: int64), (var_19: int64), (var_57: int64), (var_50: int64))
                    var_60
                else
                    let (var_62: int64) = (0L + var_16)
                    let (var_63: int64) = (var_62 + var_17)
                    let (var_64: int64) = (var_63 + var_18)
                    let (var_65: int64) = (var_64 + var_20)
                    var_65
            if_var_16
        else
            let (var_67: int64) = (0L + var_16)
            let (var_68: int64) = (var_67 + var_17)
            let (var_69: int64) = (var_68 + var_18)
            let (var_70: int64) = (var_69 + var_20)
            var_70
    if_var_15
let (var_23: string) = System.Console.ReadLine()
let (var_24: int64) = 0L
let (var_25: int64) = var_24
let (var_46: int64) = method_14((var_23: string), (var_25: int64))
var_46
