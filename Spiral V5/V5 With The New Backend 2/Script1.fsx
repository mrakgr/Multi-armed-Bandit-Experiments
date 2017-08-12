type Rec19 =
    | Rec19Case0 of Tuple8
    | Rec19Case1
and Union3 =
    | Union3Case0 of int64
    | Union3Case1 of string
    | Union3Case2 of Tuple4
    | Union3Case3 of Rec19
and Tuple8 =
    struct
    val mem_1: Tuple7
    new(arg_mem_1) = {mem_1 = arg_mem_1}
    end
and Tuple7 =
    struct
    val mem_0: int64
    val mem_1: Rec19
    new(arg_mem_0, arg_mem_1) = {mem_0 = arg_mem_0; mem_1 = arg_mem_1}
    end
and Tuple4 =
    struct
    val mem_0: int64
    val mem_1: int64
    new(arg_mem_0, arg_mem_1) = {mem_0 = arg_mem_0; mem_1 = arg_mem_1}
    end
let rec method_18(): unit =
    ()
and method_22((var_4: string), (var_5: int64 ref), (var_7: Rec19)): Union3 =
    let (var_9: int64) = (!var_5)
    let (var_11: int64) = 0L
    let (var_12: Union3) = method_23((var_4: string), (var_5: int64 ref), (var_7: Rec19), (var_9: int64), (var_11: int64))
    var_12
and method_23((var_4: string), (var_5: int64 ref), (var_7: Rec19), (var_8: int64), (var_10: int64)): Union3 =
    let (var_13: int64) = (!var_5)
    let (var_14: bool) = method_24((var_4: string), (var_13: int64))
    let (if_var_1: Union3) =
        if var_14 then
            let (var_16: char) = var_4.[int32 (var_13)]
            let (var_20: int64) = (!var_5)
            let (var_21: int64) = (var_20 + 1L)
            var_5 := var_21
            let (var_24: bool) = method_25((var_16: char))
            let (if_var_2: Union3) =
                if var_24 then
                    let (var_27: int64) = System.Convert.ToInt64(var_16)
                    let (var_28: int64) = System.Convert.ToInt64('0')
                    let (var_29: int64) = (var_27 - var_28)
                    let (var_30: int64) = (var_10 * 10L)
                    let (var_31: int64) = (var_30 + var_29)
                    let (var_33: Union3) = method_26((var_4: string), (var_5: int64 ref), (var_7: Rec19), (var_8: int64), (var_31: int64))
                    var_33
                else
                    let (var_47: int64) = (!var_5)
                    let (var_48: bool) = (var_8 = var_47)
                    let (if_var_3: Union3) =
                        if var_48 then
                            let (var_51: Rec19) = Rec19Case1
                            let (var_52: Rec19) = method_30((var_7: Rec19), (var_51: Rec19))
                            Union3Case3(var_52)
                        else
                            Union3Case1("many")
                    let (var_60: Union3) = (if_var_3: Union3)
                    var_60
            let (var_61: Union3) = (if_var_2: Union3)
            var_61
        else
            let (var_76: int64) = (!var_5)
            let (var_77: bool) = (var_8 = var_76)
            let (if_var_4: Union3) =
                if var_77 then
                    let (var_80: Rec19) = Rec19Case1
                    let (var_81: Rec19) = method_30((var_7: Rec19), (var_80: Rec19))
                    Union3Case3(var_81)
                else
                    Union3Case1("many")
            let (var_89: Union3) = (if_var_4: Union3)
            var_89
    let (var_90: Union3) = (if_var_1: Union3)
    var_90
and method_24((var_0: string), (var_1: int64)): bool =
    let (var_2: bool) = (var_1 >= 0L)
    let (var_3: int64) = (int64 var_0.Length)
    let (var_4: bool) = (var_1 < var_3)
    let (var_5: bool) = (var_2 && var_4)
    var_5
and method_25((var_0: char)): bool =
    let (var_1: bool) = (var_0 >= '0')
    let (var_2: bool) = (var_0 <= '9')
    let (var_3: bool) = (var_1 && var_2)
    var_3
and method_26((var_4: string), (var_5: int64 ref), (var_7: Rec19), (var_8: int64), (var_9: int64)): Union3 =
    let (var_13: int64) = (!var_5)
    let (var_14: bool) = method_24((var_4: string), (var_13: int64))
    let (if_var_5: Union3) =
        if var_14 then
            let (var_16: char) = var_4.[int32 (var_13)]
            let (var_20: int64) = (!var_5)
            let (var_21: int64) = (var_20 + 1L)
            var_5 := var_21
            let (var_24: bool) = method_25((var_16: char))
            let (if_var_6: Union3) =
                if var_24 then
                    let (var_27: int64) = System.Convert.ToInt64(var_16)
                    let (var_28: int64) = System.Convert.ToInt64('0')
                    let (var_29: int64) = (var_27 - var_28)
                    let (var_30: int64) = (var_9 * 10L)
                    let (var_31: int64) = (var_30 + var_29)
                    let (var_33: Union3) = method_26((var_4: string), (var_5: int64 ref), (var_7: Rec19), (var_8: int64), (var_31: int64))
                    var_33
                else
                    let (var_42: Union3) = method_27((var_4: string), (var_5: int64 ref), (var_7: Rec19), (var_8: int64), (var_9: int64))
                    var_42
            let (var_43: Union3) = (if_var_6: Union3)
            var_43
        else
            let (var_53: Union3) = method_27((var_4: string), (var_5: int64 ref), (var_7: Rec19), (var_8: int64), (var_9: int64))
            var_53
    let (var_54: Union3) = (if_var_5: Union3)
    var_54
and method_27((var_4: string), (var_5: int64 ref), (var_7: Rec19), (var_8: int64), (var_9: int64)): Union3 =
    let (var_12: int64) = (!var_5)
    let (var_13: bool) = method_24((var_4: string), (var_12: int64))
    let (if_var_7: Union3) =
        if var_13 then
            let (var_15: char) = var_4.[int32 (var_12)]
            let (var_19: int64) = (!var_5)
            let (var_20: int64) = (var_19 + 1L)
            var_5 := var_20
            let (var_23: bool) = method_28((var_15: char))
            let (var_24: bool) = method_29((var_15: char))
            let (var_25: bool) = (var_23 || var_24)
            let (if_var_8: Union3) =
                if var_25 then
                    let (var_26: Union3) = method_27((var_4: string), (var_5: int64 ref), (var_7: Rec19), (var_8: int64), (var_9: int64))
                    var_26
                else
                    let (var_29: int64) = (!var_5)
                    let (var_30: int64) = (var_29 + -1L)
                    var_5 := var_30
                    let (var_40: int64) = (!var_5)
                    let (var_41: bool) = (var_8 < var_40)
                    let (if_var_9: Union3) =
                        if var_41 then
                            let (var_43: Rec19) = Rec19Case0(Tuple8(Tuple7(var_9, var_7)))
                            let (var_44: Union3) = method_22((var_4: string), (var_5: int64 ref), (var_43: Rec19))
                            var_44
                        else
                            let (var_47: int64) = (!var_5)
                            let (var_48: bool) = (var_8 = var_47)
                            let (if_var_10: Union3) =
                                if var_48 then
                                    Union3Case1("Many parser succeeded without changing the parser state. Unless the computation had been aborted, the parser would have gone into an infinite loop.")
                                else
                                    Union3Case0(var_9)
                            let (var_55: Union3) = (if_var_10: Union3)
                            var_55
                    let (var_56: Union3) = (if_var_9: Union3)
                    var_56
            let (var_57: Union3) = (if_var_8: Union3)
            var_57
        else
            let (var_70: int64) = (!var_5)
            let (var_71: bool) = (var_8 < var_70)
            let (if_var_11: Union3) =
                if var_71 then
                    let (var_73: Rec19) = Rec19Case0(Tuple8(Tuple7(var_9, var_7)))
                    let (var_74: Union3) = method_22((var_4: string), (var_5: int64 ref), (var_73: Rec19))
                    var_74
                else
                    let (var_77: int64) = (!var_5)
                    let (var_78: bool) = (var_8 = var_77)
                    let (if_var_12: Union3) =
                        if var_78 then
                            Union3Case1("Many parser succeeded without changing the parser state. Unless the computation had been aborted, the parser would have gone into an infinite loop.")
                        else
                            Union3Case0(var_9)
                    let (var_85: Union3) = (if_var_12: Union3)
                    var_85
            let (var_86: Union3) = (if_var_11: Union3)
            var_86
    let (var_87: Union3) = (if_var_7: Union3)
    var_87
and method_28((var_0: char)): bool =
    let (var_1: bool) = (var_0 = ' ')
    var_1
and method_29((var_0: char)): bool =
    let (var_1: bool) = (var_0 = '\n')
    let (var_2: bool) = (var_0 = '\r')
    let (var_3: bool) = (var_1 || var_2)
    var_3
and method_30((var_1: Rec19), (var_2: Rec19)): Rec19 =
    let (if_var_13: Rec19) =
        match var_1 with
        | Rec19Case0(var_3) ->
            let (var_6: Tuple7) = var_3.mem_1
            let (var_9: int64) = var_6.mem_0
            let (var_10: Rec19) = var_6.mem_1
            let (var_12: Rec19) = Rec19Case0(Tuple8(Tuple7(var_9, var_2)))
            let (var_13: Rec19) = method_30((var_10: Rec19), (var_12: Rec19))
            var_13
        | Rec19Case1 ->
            var_2
    let (var_15: Rec19) = (if_var_13: Rec19)
    var_15
let (var_29: string) = "12 34 "
let (var_31: int64 ref) = (ref 0L)
method_18()
let (var_34: Rec19) = Rec19Case1
let (var_35: Union3) = method_22((var_29: string), (var_31: int64 ref), (var_34: Rec19))
match var_35 with
| Union3Case3 (Rec19Case0 x) -> 
    x.mem_1