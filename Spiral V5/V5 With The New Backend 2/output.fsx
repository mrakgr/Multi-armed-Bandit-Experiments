type Env56 =
    struct
    val mem_loop: Env49
    val mem_pos: int64
    new(arg_mem_loop, arg_mem_pos) = {mem_loop = arg_mem_loop; mem_pos = arg_mem_pos}
    end
and Env42 =
    struct
    val mem_on_succ: (int64 -> (int64 -> int64))
    new(arg_mem_on_succ) = {mem_on_succ = arg_mem_on_succ}
    end
and Env46 =
    struct
    val mem_str: string
    new(arg_mem_str) = {mem_str = arg_mem_str}
    end
and Env47 =
    struct
    val mem_ret: Env42
    val mem_stream: Env46
    new(arg_mem_ret, arg_mem_stream) = {mem_ret = arg_mem_ret; mem_stream = arg_mem_stream}
    end
and Env48 =
    struct
    val mem_on_succ: Env47
    new(arg_mem_on_succ) = {mem_on_succ = arg_mem_on_succ}
    end
and Env49 =
    struct
    val mem_ret: Env48
    val mem_stream: Env46
    new(arg_mem_ret, arg_mem_stream) = {mem_ret = arg_mem_ret; mem_stream = arg_mem_stream}
    end
and Env57 =
    struct
    val mem_acc: Tuple4
    val mem_loop: Env49
    val mem_pos: int64
    new(arg_mem_acc, arg_mem_loop, arg_mem_pos) = {mem_acc = arg_mem_acc; mem_loop = arg_mem_loop; mem_pos = arg_mem_pos}
    end
and Tuple4 =
    struct
    val mem_0: int64
    new(arg_mem_0) = {mem_0 = arg_mem_0}
    end
let rec method_14 ((var_2: string)) ((var_0: int64)): (int64 -> int64) =
    let (var_8: (int64 -> int64)) = method_15((var_2: string), (var_0: int64))
    var_8
and method_15 ((var_2: string), (var_3: int64)) ((var_0: int64)): int64 =
    let (var_9: (int64 -> (int64 -> int64))) = method_16((var_0: int64), (var_2: string))
    let (var_14: (int64 -> Env56)) = method_20((var_9: (int64 -> (int64 -> int64))), (var_2: string))
    let (var_16: int64) = 0L
    let (var_17: int64) = method_21((var_16: int64), (var_14: (int64 -> Env56)), (var_2: string), (var_3: int64))
    var_17
and method_16 ((var_1: int64), (var_3: string)) ((var_0: int64)): (int64 -> int64) =
    let (var_9: (int64 -> int64)) = method_17((var_1: int64), (var_3: string), (var_0: int64))
    var_9
and method_17 ((var_1: int64), (var_3: string), (var_4: int64)) ((var_0: int64)): int64 =
    let (var_10: (int64 -> (int64 -> int64))) = method_18((var_0: int64), (var_1: int64), (var_3: string))
    let (var_15: (int64 -> Env56)) = method_20((var_10: (int64 -> (int64 -> int64))), (var_3: string))
    let (var_17: int64) = 0L
    let (var_18: int64) = method_21((var_17: int64), (var_15: (int64 -> Env56)), (var_3: string), (var_4: int64))
    var_18
and method_18 ((var_1: int64), (var_2: int64), (var_4: string)) ((var_0: int64)): (int64 -> int64) =
    let (var_7: (int64 -> int64)) = method_19((var_1: int64), (var_2: int64), (var_4: string), (var_0: int64))
    var_7
and method_19 ((var_1: int64), (var_2: int64), (var_4: string), (var_5: int64)) ((var_0: int64)): int64 =
    let (var_9: int64) = (0L + var_2)
    let (var_10: int64) = (var_9 + var_1)
    let (var_11: int64) = (var_10 + var_0)
    var_11
and method_20 ((var_2: (int64 -> (int64 -> int64))), (var_3: string)) ((var_0: int64)): Env56 =
    Env56(Env49(Env48(Env47(Env42(var_2), Env46(var_3))), Env46(var_3)), var_0)
and method_21((var_0: int64), (var_2: (int64 -> Env56)), (var_4: string), (var_7: int64)): int64 =
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
                    let (var_28: int64) = method_22((var_27: int64), (var_2: (int64 -> Env56)), (var_4: string), (var_19: int64))
                    var_28
                else
                    -1L
            if_var_2
        else
            -1L
    if_var_1
and method_22((var_0: int64), (var_2: (int64 -> Env56)), (var_4: string), (var_7: int64)): int64 =
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
                    let (var_28: int64) = method_22((var_27: int64), (var_2: (int64 -> Env56)), (var_4: string), (var_19: int64))
                    var_28
                else
                    let (var_31: Env56) = var_2(var_19)
                    let (var_34: Env49) = var_31.mem_loop
                    let (var_35: int64) = var_31.mem_pos
                    let (var_45: Env48) = var_34.mem_ret
                    let (var_46: Env46) = var_34.mem_stream
                    let (var_59: (int64 -> Env57)) = method_23((var_0: int64), (var_45: Env48), (var_46: Env46))
                    let (var_64: string) = var_46.mem_str
                    let (var_68: Env47) = var_45.mem_on_succ
                    let (var_71: int64) = method_24((var_35: int64), (var_59: (int64 -> Env57)), (var_64: string))
                    var_71
            if_var_4
        else
            let (var_74: Env56) = var_2(var_7)
            let (var_77: Env49) = var_74.mem_loop
            let (var_78: int64) = var_74.mem_pos
            let (var_88: Env48) = var_77.mem_ret
            let (var_89: Env46) = var_77.mem_stream
            let (var_102: (int64 -> Env57)) = method_23((var_0: int64), (var_88: Env48), (var_89: Env46))
            let (var_107: string) = var_89.mem_str
            let (var_111: Env47) = var_88.mem_on_succ
            let (var_114: int64) = method_24((var_78: int64), (var_102: (int64 -> Env57)), (var_107: string))
            var_114
    if_var_3
and method_23 ((var_2: int64), (var_10: Env48), (var_11: Env46)) ((var_0: int64)): Env57 =
    Env57(Tuple4(var_2), Env49(var_10, var_11), var_0)
and method_24((var_7: int64), (var_10: (int64 -> Env57)), (var_17: string)): int64 =
    let (var_47: bool) = (var_7 >= 0L)
    let (var_48: int64) = (int64 var_17.Length)
    let (var_49: bool) = (var_7 < var_48)
    let (if_var_5: int64) =
        if (var_47 && var_49) then
            let (var_51: char) = var_17.[int32 var_7]
            let (var_53: int64) = (var_7 + 1L)
            let (var_55: bool) = (var_51 = ' ')
            let (var_58: bool) = (var_51 = '\n')
            let (var_59: bool) = (var_51 = '\r')
            let (var_60: bool) = (var_58 || var_59)
            let (if_var_6: int64) =
                if (var_55 || var_60) then
                    let (var_61: int64) = method_24((var_53: int64), (var_10: (int64 -> Env57)), (var_17: string))
                    var_61
                else
                    let (var_63: int64) = (var_53 - 1L)
                    let (var_64: Env57) = var_10(var_63)
                    let (var_66: Tuple4) = var_64.mem_acc
                    let (var_67: Env49) = var_64.mem_loop
                    let (var_68: int64) = var_64.mem_pos
                    let (var_78: Env48) = var_67.mem_ret
                    let (var_79: Env46) = var_67.mem_stream
                    let (var_81: int64) = var_66.mem_0
                    let (var_84: Env47) = var_78.mem_on_succ
                    let (var_88: Env42) = var_84.mem_ret
                    let (var_89: Env46) = var_84.mem_stream
                    let (var_117: string) = var_89.mem_str
                    let (var_121: (int64 -> (int64 -> int64))) = var_88.mem_on_succ
                    let (var_124: (int64 -> int64)) = var_121(var_68)
                    let (var_125: int64) = var_124(var_81)
                    var_125
            if_var_6
        else
            let (var_128: Env57) = var_10(var_7)
            let (var_130: Tuple4) = var_128.mem_acc
            let (var_131: Env49) = var_128.mem_loop
            let (var_132: int64) = var_128.mem_pos
            let (var_142: Env48) = var_131.mem_ret
            let (var_143: Env46) = var_131.mem_stream
            let (var_145: int64) = var_130.mem_0
            let (var_148: Env47) = var_142.mem_on_succ
            let (var_152: Env42) = var_148.mem_ret
            let (var_153: Env46) = var_148.mem_stream
            let (var_181: string) = var_153.mem_str
            let (var_185: (int64 -> (int64 -> int64))) = var_152.mem_on_succ
            let (var_188: (int64 -> int64)) = var_185(var_132)
            let (var_189: int64) = var_188(var_145)
            var_189
    if_var_5
let (var_24: string) = "1 2 3"
let (var_27: int64) = 0L
let (var_28: int64) = var_27
let (var_31: (int64 -> (int64 -> int64))) = method_14((var_24: string))
let (var_36: (int64 -> Env56)) = method_20((var_31: (int64 -> (int64 -> int64))), (var_24: string))
let (var_38: int64) = 0L
let (var_39: int64) = method_21((var_38: int64), (var_36: (int64 -> Env56)), (var_24: string), (var_28: int64))
var_39
