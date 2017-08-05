type RecTy0 =
    | Rec0Case0 of Tuple11
    | Rec0Case1
and UnionTy1 =
    | Union1Case0 of int64
    | Union1Case1 of string
    | Union1Case2 of Tuple17
    | Union1Case3 of RecTy0
and Tuple11 =
    struct
    val mem_1: Tuple12
    new(arg_mem_1) = {mem_1 = arg_mem_1}
    end
and Tuple12 =
    struct
    val mem_0: int64
    val mem_1: RecTy0
    new(arg_mem_0, arg_mem_1) = {mem_0 = arg_mem_0; mem_1 = arg_mem_1}
    end
and Tuple17 =
    struct
    val mem_0: int64
    val mem_1: int64
    new(arg_mem_0, arg_mem_1) = {mem_0 = arg_mem_0; mem_1 = arg_mem_1}
    end
let rec method_0(): unit =
    ()
and method_1((var_4: string), (var_5: int64 ref), (var_7: RecTy0)): UnionTy1 =
    let (var_9: int64) = (!var_5)
    let (var_11: int64) = 0L
    let (var_12: UnionTy1) = method_2((var_4: string), (var_5: int64 ref), (var_7: RecTy0), (var_9: int64), (var_11: int64))
    var_12
and method_2((var_4: string), (var_5: int64 ref), (var_7: RecTy0), (var_8: int64), (var_10: int64)): UnionTy1 =
    let (var_13: int64) = (!var_5)
    let (var_14: bool) = (var_13 >= 0L)
    let (var_15: int64) = (int64 var_4.Length)
    let (var_16: bool) = (var_13 < var_15)
    let (var_17: bool) = (var_14 && var_16)
    let (if_var_2: UnionTy1) =
        if var_17 then
            let (var_19: char) = var_4.[int32 (var_13)]
            let (var_23: int64) = (!var_5)
            let (var_24: int64) = (var_23 + 1L)
            var_5 := var_24
            let (var_27: bool) = (var_19 >= '0')
            let (var_28: bool) = (var_19 <= '9')
            let (var_29: bool) = (var_27 && var_28)
            let (if_var_3: UnionTy1) =
                if var_29 then
                    let (var_32: int64) = System.Convert.ToInt64(var_19)
                    let (var_33: int64) = System.Convert.ToInt64('0')
                    let (var_34: int64) = (var_32 - var_33)
                    let (var_35: int64) = (var_10 * 10L)
                    let (var_36: int64) = (var_35 + var_34)
                    let (var_38: UnionTy1) = method_3((var_4: string), (var_5: int64 ref), (var_7: RecTy0), (var_8: int64), (var_36: int64))
                    var_38
                else
                    let (var_52: int64) = (!var_5)
                    let (var_53: bool) = (var_8 = var_52)
                    let (if_var_4: UnionTy1) =
                        if var_53 then
                            let (var_56: RecTy0) = Rec0Case1
                            let (var_57: RecTy0) = method_5((var_7: RecTy0), (var_56: RecTy0))
                            Union1Case3(var_57)
                        else
                            Union1Case1("many")
                    let (var_65: UnionTy1) = (if_var_4: UnionTy1)
                    var_65
            let (var_66: UnionTy1) = (if_var_3: UnionTy1)
            var_66
        else
            let (var_81: int64) = (!var_5)
            let (var_82: bool) = (var_8 = var_81)
            let (if_var_5: UnionTy1) =
                if var_82 then
                    let (var_85: RecTy0) = Rec0Case1
                    let (var_86: RecTy0) = method_5((var_7: RecTy0), (var_85: RecTy0))
                    Union1Case3(var_86)
                else
                    Union1Case1("many")
            let (var_94: UnionTy1) = (if_var_5: UnionTy1)
            var_94
    let (var_95: UnionTy1) = (if_var_2: UnionTy1)
    var_95
and method_3((var_4: string), (var_5: int64 ref), (var_7: RecTy0), (var_8: int64), (var_9: int64)): UnionTy1 =
    let (var_13: int64) = (!var_5)
    let (var_14: bool) = (var_13 >= 0L)
    let (var_15: int64) = (int64 var_4.Length)
    let (var_16: bool) = (var_13 < var_15)
    let (var_17: bool) = (var_14 && var_16)
    let (if_var_6: UnionTy1) =
        if var_17 then
            let (var_19: char) = var_4.[int32 (var_13)]
            let (var_23: int64) = (!var_5)
            let (var_24: int64) = (var_23 + 1L)
            var_5 := var_24
            let (var_27: bool) = (var_19 >= '0')
            let (var_28: bool) = (var_19 <= '9')
            let (var_29: bool) = (var_27 && var_28)
            let (if_var_7: UnionTy1) =
                if var_29 then
                    let (var_32: int64) = System.Convert.ToInt64(var_19)
                    let (var_33: int64) = System.Convert.ToInt64('0')
                    let (var_34: int64) = (var_32 - var_33)
                    let (var_35: int64) = (var_9 * 10L)
                    let (var_36: int64) = (var_35 + var_34)
                    let (var_38: UnionTy1) = method_3((var_4: string), (var_5: int64 ref), (var_7: RecTy0), (var_8: int64), (var_36: int64))
                    var_38
                else
                    let (var_47: UnionTy1) = method_4((var_4: string), (var_5: int64 ref), (var_7: RecTy0), (var_8: int64), (var_9: int64))
                    var_47
            let (var_48: UnionTy1) = (if_var_7: UnionTy1)
            var_48
        else
            let (var_58: UnionTy1) = method_4((var_4: string), (var_5: int64 ref), (var_7: RecTy0), (var_8: int64), (var_9: int64))
            var_58
    let (var_59: UnionTy1) = (if_var_6: UnionTy1)
    var_59
and method_4((var_4: string), (var_5: int64 ref), (var_7: RecTy0), (var_8: int64), (var_9: int64)): UnionTy1 =
    let (var_12: int64) = (!var_5)
    let (var_13: bool) = (var_12 >= 0L)
    let (var_14: int64) = (int64 var_4.Length)
    let (var_15: bool) = (var_12 < var_14)
    let (var_16: bool) = (var_13 && var_15)
    let (if_var_8: UnionTy1) =
        if var_16 then
            let (var_18: char) = var_4.[int32 (var_12)]
            let (var_22: int64) = (!var_5)
            let (var_23: int64) = (var_22 + 1L)
            var_5 := var_23
            let (var_26: bool) = (var_18 = ' ')
            let (var_27: bool) = (var_18 = '\n')
            let (var_28: bool) = (var_18 = '\r')
            let (var_29: bool) = (var_27 || var_28)
            let (var_30: bool) = (var_26 || var_29)
            let (if_var_9: UnionTy1) =
                if var_30 then
                    let (var_31: UnionTy1) = method_4((var_4: string), (var_5: int64 ref), (var_7: RecTy0), (var_8: int64), (var_9: int64))
                    var_31
                else
                    let (var_34: int64) = (!var_5)
                    let (var_35: int64) = (var_34 + -1L)
                    var_5 := var_35
                    let (var_45: int64) = (!var_5)
                    let (var_46: bool) = (var_8 < var_45)
                    let (if_var_10: UnionTy1) =
                        if var_46 then
                            let (var_48: RecTy0) = Rec0Case0(Tuple11(Tuple12(var_9, var_7)))
                            let (var_49: UnionTy1) = method_1((var_4: string), (var_5: int64 ref), (var_48: RecTy0))
                            var_49
                        else
                            let (var_52: int64) = (!var_5)
                            let (var_53: bool) = (var_8 = var_52)
                            let (if_var_13: UnionTy1) =
                                if var_53 then
                                    Union1Case1("Many parser succeeded without changing the parser state. Unless the computation had been aborted, the parser would have gone into an infinite loop.")
                                else
                                    Union1Case0(var_9)
                            let (var_60: UnionTy1) = (if_var_13: UnionTy1)
                            var_60
                    let (var_61: UnionTy1) = (if_var_10: UnionTy1)
                    var_61
            let (var_62: UnionTy1) = (if_var_9: UnionTy1)
            var_62
        else
            let (var_75: int64) = (!var_5)
            let (var_76: bool) = (var_8 < var_75)
            let (if_var_14: UnionTy1) =
                if var_76 then
                    let (var_78: RecTy0) = Rec0Case0(Tuple11(Tuple12(var_9, var_7)))
                    let (var_79: UnionTy1) = method_1((var_4: string), (var_5: int64 ref), (var_78: RecTy0))
                    var_79
                else
                    let (var_82: int64) = (!var_5)
                    let (var_83: bool) = (var_8 = var_82)
                    let (if_var_15: UnionTy1) =
                        if var_83 then
                            Union1Case1("Many parser succeeded without changing the parser state. Unless the computation had been aborted, the parser would have gone into an infinite loop.")
                        else
                            Union1Case0(var_9)
                    let (var_90: UnionTy1) = (if_var_15: UnionTy1)
                    var_90
            let (var_91: UnionTy1) = (if_var_14: UnionTy1)
            var_91
    let (var_92: UnionTy1) = (if_var_8: UnionTy1)
    var_92
and method_5((var_1: RecTy0), (var_2: RecTy0)): RecTy0 =
    let (if_var_16: RecTy0) =
        match var_1 with
        | Rec0Case0(var_3) ->
            let (var_6: Tuple12) = var_3.mem_1
            let (var_9: int64) = var_6.mem_0
            let (var_10: RecTy0) = var_6.mem_1
            let (var_12: RecTy0) = Rec0Case0(Tuple11(Tuple12(var_9, var_2)))
            let (var_13: RecTy0) = method_5((var_10: RecTy0), (var_12: RecTy0))
            var_13
        | Rec0Case1 ->
            var_2
    let (var_15: RecTy0) = (if_var_16: RecTy0)
    var_15
let (var_29: string) = "12 34 "
let (var_30: int64 ref) = (ref 0L)
method_0()
let (var_34: RecTy0) = Rec0Case1
let (var_35: UnionTy1) = method_1((var_29: string), (var_30: int64 ref), (var_34: RecTy0))
var_35