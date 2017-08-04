type UnionTy1 =
    | Union1Case0 of int64
    | Union1Case1 of string
    | Union1Case2 of Tuple14
and Tuple14 =
    struct
    val mem_0: int64
    val mem_1: int64
    new(arg_mem_0, arg_mem_1) = {mem_0 = arg_mem_0; mem_1 = arg_mem_1}
    end
let rec method_0((var_4: string), (var_5: int64 ref), (var_7: int64)): UnionTy1 =
    let (var_10: int64) = (!var_5)
    let (var_11: bool) = (var_10 >= 0L)
    let (var_12: int64) = (int64 var_4.Length)
    let (var_13: bool) = (var_10 < var_12)
    let (var_14: bool) = (var_11 && var_13)
    let (if_var_2: UnionTy1) =
        if var_14 then
            let (var_16: char) = var_4.[int32 (var_10)]
            let (var_20: int64) = (!var_5)
            let (var_21: int64) = (var_20 + 1L)
            var_5 := var_21
            let (var_24: bool) = (var_16 >= '0')
            let (var_25: bool) = (var_16 <= '9')
            let (var_26: bool) = (var_24 && var_25)
            let (if_var_3: UnionTy1) =
                if var_26 then
                    let (var_29: int64) = System.Convert.ToInt64(var_16)
                    let (var_30: int64) = System.Convert.ToInt64('0')
                    let (var_31: int64) = (var_29 - var_30)
                    let (var_32: int64) = (var_7 * 10L)
                    let (var_33: int64) = (var_32 + var_31)
                    let (var_35: UnionTy1) = method_1((var_4: string), (var_5: int64 ref), (var_33: int64))
                    var_35
                else
                    Union1Case1("int64")
            let (var_49: UnionTy1) = (if_var_3: UnionTy1)
            var_49
        else
            Union1Case1("int64")
    let (var_64: UnionTy1) = (if_var_2: UnionTy1)
    var_64
and method_1((var_4: string), (var_5: int64 ref), (var_6: int64)): UnionTy1 =
    let (var_10: int64) = (!var_5)
    let (var_11: bool) = (var_10 >= 0L)
    let (var_12: int64) = (int64 var_4.Length)
    let (var_13: bool) = (var_10 < var_12)
    let (var_14: bool) = (var_11 && var_13)
    let (if_var_4: UnionTy1) =
        if var_14 then
            let (var_16: char) = var_4.[int32 (var_10)]
            let (var_20: int64) = (!var_5)
            let (var_21: int64) = (var_20 + 1L)
            var_5 := var_21
            let (var_24: bool) = (var_16 >= '0')
            let (var_25: bool) = (var_16 <= '9')
            let (var_26: bool) = (var_24 && var_25)
            let (if_var_5: UnionTy1) =
                if var_26 then
                    let (var_29: int64) = System.Convert.ToInt64(var_16)
                    let (var_30: int64) = System.Convert.ToInt64('0')
                    let (var_31: int64) = (var_29 - var_30)
                    let (var_32: int64) = (var_6 * 10L)
                    let (var_33: int64) = (var_32 + var_31)
                    let (var_35: UnionTy1) = method_1((var_4: string), (var_5: int64 ref), (var_33: int64))
                    var_35
                else
                    let (var_44: UnionTy1) = method_2((var_4: string), (var_5: int64 ref), (var_6: int64))
                    var_44
            let (var_45: UnionTy1) = (if_var_5: UnionTy1)
            var_45
        else
            let (var_55: UnionTy1) = method_2((var_4: string), (var_5: int64 ref), (var_6: int64))
            var_55
    let (var_56: UnionTy1) = (if_var_4: UnionTy1)
    var_56
and method_2((var_4: string), (var_5: int64 ref), (var_6: int64)): UnionTy1 =
    let (var_9: int64) = (!var_5)
    let (var_10: bool) = (var_9 >= 0L)
    let (var_11: int64) = (int64 var_4.Length)
    let (var_12: bool) = (var_9 < var_11)
    let (var_13: bool) = (var_10 && var_12)
    let (if_var_6: UnionTy1) =
        if var_13 then
            let (var_15: char) = var_4.[int32 (var_9)]
            let (var_19: int64) = (!var_5)
            let (var_20: int64) = (var_19 + 1L)
            var_5 := var_20
            let (var_23: bool) = (var_15 = ' ')
            let (var_24: bool) = (var_15 = '\n')
            let (var_25: bool) = (var_15 = '\r')
            let (var_26: bool) = (var_24 || var_25)
            let (var_27: bool) = (var_23 || var_26)
            let (if_var_7: UnionTy1) =
                if var_27 then
                    let (var_28: UnionTy1) = method_2((var_4: string), (var_5: int64 ref), (var_6: int64))
                    var_28
                else
                    let (var_31: int64) = (!var_5)
                    let (var_32: int64) = (var_31 + -1L)
                    var_5 := var_32
                    let (var_42: int64) = 0L
                    let (var_43: UnionTy1) = method_3((var_4: string), (var_5: int64 ref), (var_6: int64), (var_42: int64))
                    var_43
            let (var_44: UnionTy1) = (if_var_7: UnionTy1)
            var_44
        else
            let (var_57: int64) = 0L
            let (var_58: UnionTy1) = method_3((var_4: string), (var_5: int64 ref), (var_6: int64), (var_57: int64))
            var_58
    let (var_59: UnionTy1) = (if_var_6: UnionTy1)
    var_59
and method_3((var_4: string), (var_5: int64 ref), (var_6: int64), (var_8: int64)): UnionTy1 =
    let (var_11: int64) = (!var_5)
    let (var_12: bool) = (var_11 >= 0L)
    let (var_13: int64) = (int64 var_4.Length)
    let (var_14: bool) = (var_11 < var_13)
    let (var_15: bool) = (var_12 && var_14)
    let (if_var_8: UnionTy1) =
        if var_15 then
            let (var_17: char) = var_4.[int32 (var_11)]
            let (var_21: int64) = (!var_5)
            let (var_22: int64) = (var_21 + 1L)
            var_5 := var_22
            let (var_25: bool) = (var_17 >= '0')
            let (var_26: bool) = (var_17 <= '9')
            let (var_27: bool) = (var_25 && var_26)
            let (if_var_9: UnionTy1) =
                if var_27 then
                    let (var_30: int64) = System.Convert.ToInt64(var_17)
                    let (var_31: int64) = System.Convert.ToInt64('0')
                    let (var_32: int64) = (var_30 - var_31)
                    let (var_33: int64) = (var_8 * 10L)
                    let (var_34: int64) = (var_33 + var_32)
                    let (var_36: UnionTy1) = method_4((var_4: string), (var_5: int64 ref), (var_6: int64), (var_34: int64))
                    var_36
                else
                    Union1Case1("int64")
            let (var_50: UnionTy1) = (if_var_9: UnionTy1)
            var_50
        else
            Union1Case1("int64")
    let (var_65: UnionTy1) = (if_var_8: UnionTy1)
    var_65
and method_4((var_4: string), (var_5: int64 ref), (var_6: int64), (var_7: int64)): UnionTy1 =
    let (var_11: int64) = (!var_5)
    let (var_12: bool) = (var_11 >= 0L)
    let (var_13: int64) = (int64 var_4.Length)
    let (var_14: bool) = (var_11 < var_13)
    let (var_15: bool) = (var_12 && var_14)
    let (if_var_10: UnionTy1) =
        if var_15 then
            let (var_17: char) = var_4.[int32 (var_11)]
            let (var_21: int64) = (!var_5)
            let (var_22: int64) = (var_21 + 1L)
            var_5 := var_22
            let (var_25: bool) = (var_17 >= '0')
            let (var_26: bool) = (var_17 <= '9')
            let (var_27: bool) = (var_25 && var_26)
            let (if_var_11: UnionTy1) =
                if var_27 then
                    let (var_30: int64) = System.Convert.ToInt64(var_17)
                    let (var_31: int64) = System.Convert.ToInt64('0')
                    let (var_32: int64) = (var_30 - var_31)
                    let (var_33: int64) = (var_7 * 10L)
                    let (var_34: int64) = (var_33 + var_32)
                    let (var_36: UnionTy1) = method_4((var_4: string), (var_5: int64 ref), (var_6: int64), (var_34: int64))
                    var_36
                else
                    let (var_45: UnionTy1) = method_5((var_4: string), (var_5: int64 ref), (var_6: int64), (var_7: int64))
                    var_45
            let (var_46: UnionTy1) = (if_var_11: UnionTy1)
            var_46
        else
            let (var_56: UnionTy1) = method_5((var_4: string), (var_5: int64 ref), (var_6: int64), (var_7: int64))
            var_56
    let (var_57: UnionTy1) = (if_var_10: UnionTy1)
    var_57
and method_5((var_1: string), (var_2: int64 ref), (var_3: int64), (var_4: int64)): UnionTy1 =
    let (var_7: int64) = (!var_2)
    let (var_8: bool) = (var_7 >= 0L)
    let (var_9: int64) = (int64 var_1.Length)
    let (var_10: bool) = (var_7 < var_9)
    let (var_11: bool) = (var_8 && var_10)
    let (if_var_12: UnionTy1) =
        if var_11 then
            let (var_13: char) = var_1.[int32 (var_7)]
            let (var_17: int64) = (!var_2)
            let (var_18: int64) = (var_17 + 1L)
            var_2 := var_18
            let (var_21: bool) = (var_13 = ' ')
            let (var_22: bool) = (var_13 = '\n')
            let (var_23: bool) = (var_13 = '\r')
            let (var_24: bool) = (var_22 || var_23)
            let (var_25: bool) = (var_21 || var_24)
            let (if_var_13: UnionTy1) =
                if var_25 then
                    let (var_26: UnionTy1) = method_5((var_1: string), (var_2: int64 ref), (var_3: int64), (var_4: int64))
                    var_26
                else
                    let (var_29: int64) = (!var_2)
                    let (var_30: int64) = (var_29 + -1L)
                    var_2 := var_30
                    Union1Case2(Tuple14(var_3, var_4))
            let (var_42: UnionTy1) = (if_var_13: UnionTy1)
            var_42
        else
            Union1Case2(Tuple14(var_3, var_4))
    let (var_57: UnionTy1) = (if_var_12: UnionTy1)
    var_57
let (var_25: string) = "12 34 "
let (var_26: int64 ref) = (ref 0L)
let (var_29: int64) = 0L
let (var_30: UnionTy1) = method_0((var_25: string), (var_26: int64 ref), (var_29: int64))
var_30