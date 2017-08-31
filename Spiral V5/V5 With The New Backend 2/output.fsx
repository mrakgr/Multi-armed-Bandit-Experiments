type Env83 =
    struct
    val mem_data: string
    val mem_main_arg: string
    val mem_pos: int64
    new(arg_mem_data, arg_mem_main_arg, arg_mem_pos) = {mem_data = arg_mem_data; mem_main_arg = arg_mem_main_arg; mem_pos = arg_mem_pos}
    end
let rec method_14 ((var_0: string), (var_15: int64)) ((var_16: Env83), (var_17: int64)): int64 =
    let (var_18: string) = var_16.mem_data
    let (var_22: string) = var_16.mem_main_arg
    let (var_24: int64) = var_16.mem_pos
    let (var_31: (Env83 * int64 -> int64)) = method_15((var_0: string), (var_15: int64), (var_17: int64))
    let (var_50: int64) = 0L
    let (var_51: int64) = method_24((var_0: string), (var_31: (Env83 * int64 -> int64)), (var_18: string), (var_22: string), (var_24: int64), (var_50: int64))
    var_51
and method_15 ((var_0: string), (var_15: int64), (var_18: int64)) ((var_16: Env83), (var_17: int64)): int64 =
    let (var_19: string) = var_16.mem_data
    let (var_23: string) = var_16.mem_main_arg
    let (var_25: int64) = var_16.mem_pos
    let (var_32: (Env83 * int64 -> int64)) = method_16((var_0: string), (var_15: int64), (var_18: int64), (var_17: int64))
    let (var_51: int64) = 0L
    let (var_52: int64) = method_24((var_0: string), (var_32: (Env83 * int64 -> int64)), (var_19: string), (var_23: string), (var_25: int64), (var_51: int64))
    var_52
and method_16 ((var_0: string), (var_15: int64), (var_18: int64), (var_19: int64)) ((var_16: Env83), (var_17: int64)): int64 =
    let (var_20: string) = var_16.mem_data
    let (var_24: string) = var_16.mem_main_arg
    let (var_26: int64) = var_16.mem_pos
    let (var_33: (Env83 * int64 -> int64)) = method_17((var_0: string), (var_15: int64), (var_18: int64), (var_19: int64), (var_17: int64))
    let (var_52: int64) = 0L
    let (var_53: int64) = method_24((var_0: string), (var_33: (Env83 * int64 -> int64)), (var_20: string), (var_24: string), (var_26: int64), (var_52: int64))
    var_53
and method_17 ((var_0: string), (var_15: int64), (var_18: int64), (var_19: int64), (var_20: int64)) ((var_16: Env83), (var_17: int64)): int64 =
    let (var_21: string) = var_16.mem_data
    let (var_25: string) = var_16.mem_main_arg
    let (var_27: int64) = var_16.mem_pos
    let (var_34: (Env83 * int64 -> int64)) = method_18((var_0: string), (var_15: int64), (var_18: int64), (var_19: int64), (var_20: int64), (var_17: int64))
    let (var_53: int64) = 0L
    let (var_54: int64) = method_24((var_0: string), (var_34: (Env83 * int64 -> int64)), (var_21: string), (var_25: string), (var_27: int64), (var_53: int64))
    var_54
and method_18 ((var_0: string), (var_15: int64), (var_18: int64), (var_19: int64), (var_20: int64), (var_21: int64)) ((var_16: Env83), (var_17: int64)): int64 =
    let (var_22: string) = var_16.mem_data
    let (var_26: string) = var_16.mem_main_arg
    let (var_28: int64) = var_16.mem_pos
    let (var_35: (Env83 * int64 -> int64)) = method_19((var_0: string), (var_15: int64), (var_18: int64), (var_19: int64), (var_20: int64), (var_21: int64), (var_17: int64))
    let (var_54: int64) = 0L
    let (var_55: int64) = method_24((var_0: string), (var_35: (Env83 * int64 -> int64)), (var_22: string), (var_26: string), (var_28: int64), (var_54: int64))
    var_55
and method_19 ((var_0: string), (var_15: int64), (var_18: int64), (var_19: int64), (var_20: int64), (var_21: int64), (var_22: int64)) ((var_16: Env83), (var_17: int64)): int64 =
    let (var_23: string) = var_16.mem_data
    let (var_27: string) = var_16.mem_main_arg
    let (var_29: int64) = var_16.mem_pos
    let (var_36: (Env83 * int64 -> int64)) = method_20((var_0: string), (var_15: int64), (var_18: int64), (var_19: int64), (var_20: int64), (var_21: int64), (var_22: int64), (var_17: int64))
    let (var_55: int64) = 0L
    let (var_56: int64) = method_24((var_0: string), (var_36: (Env83 * int64 -> int64)), (var_23: string), (var_27: string), (var_29: int64), (var_55: int64))
    var_56
and method_20 ((var_0: string), (var_15: int64), (var_18: int64), (var_19: int64), (var_20: int64), (var_21: int64), (var_22: int64), (var_23: int64)) ((var_16: Env83), (var_17: int64)): int64 =
    let (var_24: string) = var_16.mem_data
    let (var_28: string) = var_16.mem_main_arg
    let (var_30: int64) = var_16.mem_pos
    let (var_37: (Env83 * int64 -> int64)) = method_21((var_0: string), (var_15: int64), (var_18: int64), (var_19: int64), (var_20: int64), (var_21: int64), (var_22: int64), (var_23: int64), (var_17: int64))
    let (var_56: int64) = 0L
    let (var_57: int64) = method_24((var_0: string), (var_37: (Env83 * int64 -> int64)), (var_24: string), (var_28: string), (var_30: int64), (var_56: int64))
    var_57
and method_21 ((var_0: string), (var_15: int64), (var_18: int64), (var_19: int64), (var_20: int64), (var_21: int64), (var_22: int64), (var_23: int64), (var_24: int64)) ((var_16: Env83), (var_17: int64)): int64 =
    let (var_25: string) = var_16.mem_data
    let (var_29: string) = var_16.mem_main_arg
    let (var_31: int64) = var_16.mem_pos
    let (var_38: (Env83 * int64 -> int64)) = method_22((var_0: string), (var_15: int64), (var_18: int64), (var_19: int64), (var_20: int64), (var_21: int64), (var_22: int64), (var_23: int64), (var_24: int64), (var_17: int64))
    let (var_57: int64) = 0L
    let (var_58: int64) = method_24((var_0: string), (var_38: (Env83 * int64 -> int64)), (var_25: string), (var_29: string), (var_31: int64), (var_57: int64))
    var_58
and method_22 ((var_0: string), (var_15: int64), (var_18: int64), (var_19: int64), (var_20: int64), (var_21: int64), (var_22: int64), (var_23: int64), (var_24: int64), (var_25: int64)) ((var_16: Env83), (var_17: int64)): int64 =
    let (var_26: string) = var_16.mem_data
    let (var_30: string) = var_16.mem_main_arg
    let (var_32: int64) = var_16.mem_pos
    let (var_39: (Env83 * int64 -> int64)) = method_23((var_0: string), (var_15: int64), (var_18: int64), (var_19: int64), (var_20: int64), (var_21: int64), (var_22: int64), (var_23: int64), (var_24: int64), (var_25: int64), (var_17: int64))
    let (var_58: int64) = 0L
    let (var_59: int64) = method_24((var_0: string), (var_39: (Env83 * int64 -> int64)), (var_26: string), (var_30: string), (var_32: int64), (var_58: int64))
    var_59
and method_23 ((var_0: string), (var_15: int64), (var_18: int64), (var_19: int64), (var_20: int64), (var_21: int64), (var_22: int64), (var_23: int64), (var_24: int64), (var_25: int64), (var_26: int64)) ((var_16: Env83), (var_17: int64)): int64 =
    let (var_27: string) = var_16.mem_data
    let (var_31: string) = var_16.mem_main_arg
    let (var_33: int64) = var_16.mem_pos
    let (var_38: int64) = (0L + var_18)
    let (var_39: int64) = (var_38 + var_19)
    let (var_40: int64) = (var_39 + var_20)
    let (var_41: int64) = (var_40 + var_21)
    let (var_42: int64) = (var_41 + var_22)
    let (var_43: int64) = (var_42 + var_23)
    let (var_44: int64) = (var_43 + var_24)
    let (var_45: int64) = (var_44 + var_25)
    let (var_46: int64) = (var_45 + var_26)
    let (var_47: int64) = (var_46 + var_17)
    var_47
and method_24((var_0: string), (var_15: (Env83 * int64 -> int64)), (var_16: string), (var_20: string), (var_22: int64), (var_26: int64)): int64 =
    let (var_51: bool) = (var_22 >= 0L)
    let (var_52: int64) = (int64 var_0.Length)
    let (var_53: bool) = (var_22 < var_52)
    let (if_var_1: int64) =
        if (var_51 && var_53) then
            let (var_54: char) = var_0.[int32 var_22]
            let (var_55: int64) = (var_22 + 1L)
            let (var_56: bool) = (var_54 >= '0')
            let (var_57: bool) = (var_54 <= '9')
            let (if_var_2: int64) =
                if (var_56 && var_57) then
                    let (var_58: int64) = System.Convert.ToInt64(var_54)
                    let (var_59: int64) = System.Convert.ToInt64('0')
                    let (var_60: int64) = (var_58 - var_59)
                    let (var_61: int64) = (var_26 * 10L)
                    let (var_62: int64) = (var_61 + var_60)
                    let (var_63: int64) = method_25((var_0: string), (var_15: (Env83 * int64 -> int64)), (var_16: string), (var_20: string), (var_22: int64), (var_62: int64), (var_55: int64))
                    var_63
                else
                    -1L
            if_var_2
        else
            -1L
    if_var_1
and method_25((var_0: string), (var_15: (Env83 * int64 -> int64)), (var_16: string), (var_20: string), (var_22: int64), (var_26: int64), (var_27: int64)): int64 =
    let (var_52: bool) = (var_27 >= 0L)
    let (var_53: int64) = (int64 var_0.Length)
    let (var_54: bool) = (var_27 < var_53)
    let (if_var_3: int64) =
        if (var_52 && var_54) then
            let (var_55: char) = var_0.[int32 var_27]
            let (var_56: int64) = (var_27 + 1L)
            let (var_57: bool) = (var_55 >= '0')
            let (var_58: bool) = (var_55 <= '9')
            let (if_var_4: int64) =
                if (var_57 && var_58) then
                    let (var_59: int64) = System.Convert.ToInt64(var_55)
                    let (var_60: int64) = System.Convert.ToInt64('0')
                    let (var_61: int64) = (var_59 - var_60)
                    let (var_62: int64) = (var_26 * 10L)
                    let (var_63: int64) = (var_62 + var_61)
                    let (var_64: int64) = method_25((var_0: string), (var_15: (Env83 * int64 -> int64)), (var_16: string), (var_20: string), (var_22: int64), (var_63: int64), (var_56: int64))
                    var_64
                else
                    let (var_75: int64) = method_26((var_0: string), (var_15: (Env83 * int64 -> int64)), (var_16: string), (var_20: string), (var_22: int64), (var_26: int64), (var_56: int64))
                    var_75
            if_var_4
        else
            let (var_86: int64) = method_26((var_0: string), (var_15: (Env83 * int64 -> int64)), (var_16: string), (var_20: string), (var_22: int64), (var_26: int64), (var_27: int64))
            var_86
    if_var_3
and method_26((var_0: string), (var_15: (Env83 * int64 -> int64)), (var_16: string), (var_20: string), (var_22: int64), (var_26: int64), (var_27: int64)): int64 =
    let (var_46: bool) = (var_27 >= 0L)
    let (var_47: int64) = (int64 var_0.Length)
    let (var_48: bool) = (var_27 < var_47)
    let (if_var_5: int64) =
        if (var_46 && var_48) then
            let (var_49: char) = var_0.[int32 var_27]
            let (var_50: int64) = (var_27 + 1L)
            let (var_51: bool) = (var_49 = ' ')
            let (var_52: bool) = (var_49 = '\n')
            let (var_53: bool) = (var_49 = '\r')
            let (var_54: bool) = (var_52 || var_53)
            let (if_var_6: int64) =
                if (var_51 || var_54) then
                    let (var_65: int64) = method_26((var_0: string), (var_15: (Env83 * int64 -> int64)), (var_16: string), (var_20: string), (var_22: int64), (var_26: int64), (var_50: int64))
                    var_65
                else
                    let (var_72: int64) = var_15(Env83(var_16, var_20, var_27), var_26)
                    var_72
            if_var_6
        else
            let (var_79: int64) = var_15(Env83(var_16, var_20, var_27), var_26)
            var_79
    if_var_5
and method_27((var_0: string), (var_15: (Env83 * int64 -> int64)), (var_16: int64), (var_17: int64)): int64 =
    let (var_42: bool) = (var_16 >= 0L)
    let (var_43: int64) = (int64 var_0.Length)
    let (var_44: bool) = (var_16 < var_43)
    let (if_var_7: int64) =
        if (var_42 && var_44) then
            let (var_45: char) = var_0.[int32 var_16]
            let (var_46: int64) = (var_16 + 1L)
            let (var_47: bool) = (var_45 >= '0')
            let (var_48: bool) = (var_45 <= '9')
            let (if_var_8: int64) =
                if (var_47 && var_48) then
                    let (var_49: int64) = System.Convert.ToInt64(var_45)
                    let (var_50: int64) = System.Convert.ToInt64('0')
                    let (var_51: int64) = (var_49 - var_50)
                    let (var_52: int64) = (var_17 * 10L)
                    let (var_53: int64) = (var_52 + var_51)
                    let (var_54: int64) = method_28((var_0: string), (var_15: (Env83 * int64 -> int64)), (var_16: int64), (var_53: int64), (var_46: int64))
                    var_54
                else
                    -1L
            if_var_8
        else
            -1L
    if_var_7
and method_28((var_0: string), (var_15: (Env83 * int64 -> int64)), (var_16: int64), (var_17: int64), (var_18: int64)): int64 =
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
                    let (var_53: int64) = (var_17 * 10L)
                    let (var_54: int64) = (var_53 + var_52)
                    let (var_55: int64) = method_28((var_0: string), (var_15: (Env83 * int64 -> int64)), (var_16: int64), (var_54: int64), (var_47: int64))
                    var_55
                else
                    let (var_66: int64) = method_29((var_0: string), (var_15: (Env83 * int64 -> int64)), (var_16: int64), (var_17: int64), (var_47: int64))
                    var_66
            if_var_10
        else
            let (var_77: int64) = method_29((var_0: string), (var_15: (Env83 * int64 -> int64)), (var_16: int64), (var_17: int64), (var_18: int64))
            var_77
    if_var_9
and method_29((var_0: string), (var_15: (Env83 * int64 -> int64)), (var_16: int64), (var_17: int64), (var_18: int64)): int64 =
    let (var_37: bool) = (var_18 >= 0L)
    let (var_38: int64) = (int64 var_0.Length)
    let (var_39: bool) = (var_18 < var_38)
    let (if_var_11: int64) =
        if (var_37 && var_39) then
            let (var_40: char) = var_0.[int32 var_18]
            let (var_41: int64) = (var_18 + 1L)
            let (var_42: bool) = (var_40 = ' ')
            let (var_43: bool) = (var_40 = '\n')
            let (var_44: bool) = (var_40 = '\r')
            let (var_45: bool) = (var_43 || var_44)
            let (if_var_12: int64) =
                if (var_42 || var_45) then
                    let (var_56: int64) = method_29((var_0: string), (var_15: (Env83 * int64 -> int64)), (var_16: int64), (var_17: int64), (var_41: int64))
                    var_56
                else
                    let (var_63: int64) = var_15(Env83(var_0, var_0, var_18), var_17)
                    var_63
            if_var_12
        else
            let (var_70: int64) = var_15(Env83(var_0, var_0, var_18), var_17)
            var_70
    if_var_11
let (var_23: string) = System.Console.ReadLine()
let (var_25: int64) = 0L
let (var_26: int64) = var_25
let (var_38: (Env83 * int64 -> int64)) = method_14((var_23: string), (var_26: int64))
let (var_57: int64) = 0L
let (var_58: int64) = method_27((var_23: string), (var_38: (Env83 * int64 -> int64)), (var_26: int64), (var_57: int64))
var_58
