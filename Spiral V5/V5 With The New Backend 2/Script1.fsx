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
    override x.ToString() = x.mem_1.ToString()
        
and Tuple12 =
    struct
    val mem_0: int64
    val mem_1: RecTy0
    new(arg_mem_0, arg_mem_1) = {mem_0 = arg_mem_0; mem_1 = arg_mem_1}
    end
    override x.ToString() = sprintf "(%s,%s)" (x.mem_0.ToString()) (x.mem_1.ToString())
and Tuple17 =
    struct
    val mem_0: int64
    val mem_1: int64
    new(arg_mem_0, arg_mem_1) = {mem_0 = arg_mem_0; mem_1 = arg_mem_1}
    end
    override x.ToString() = sprintf "(%s,%s)" (x.mem_0.ToString()) (x.mem_1.ToString())
let rec method_0(): unit =
    ()
and method_1((var_2: string), (var_3: int64 ref), (var_5: RecTy0)): UnionTy1 =
    let (var_7: int64) = (!var_3)
    let (var_9: int64) = 0L
    let (var_10: UnionTy1) = method_2((var_2: string), (var_3: int64 ref), (var_5: RecTy0), (var_7: int64), (var_9: int64))
    var_10
and method_2((var_2: string), (var_3: int64 ref), (var_5: RecTy0), (var_6: int64), (var_8: int64)): UnionTy1 =
    let (var_11: int64) = (!var_3)
    let (var_12: bool) = (var_11 >= 0L)
    let (var_13: int64) = (int64 var_2.Length)
    let (var_14: bool) = (var_11 < var_13)
    let (var_15: bool) = (var_12 && var_14)
    let (if_var_2: UnionTy1) =
        if var_15 then
            let (var_17: char) = var_2.[int32 (var_11)]
            let (var_21: int64) = (!var_3)
            let (var_22: int64) = (var_21 + 1L)
            var_3 := var_22
            let (var_25: bool) = (var_17 >= '0')
            let (var_26: bool) = (var_17 <= '9')
            let (var_27: bool) = (var_25 && var_26)
            let (if_var_3: UnionTy1) =
                if var_27 then
                    let (var_30: int64) = System.Convert.ToInt64(var_17)
                    let (var_31: int64) = System.Convert.ToInt64('0')
                    let (var_32: int64) = (var_30 - var_31)
                    let (var_33: int64) = (var_8 * 10L)
                    let (var_34: int64) = (var_33 + var_32)
                    let (var_36: UnionTy1) = method_3((var_2: string), (var_3: int64 ref), (var_5: RecTy0), (var_6: int64), (var_34: int64))
                    var_36
                else
                    let (var_50: int64) = (!var_3)
                    let (var_51: bool) = (var_6 = var_50)
                    let (if_var_4: UnionTy1) =
                        if var_51 then
                            let (var_54: RecTy0) = Rec0Case1
                            let (var_55: RecTy0) = method_5((var_5: RecTy0), (var_54: RecTy0))
                            Union1Case3(var_55)
                        else
                            Union1Case1("many")
                    let (var_63: UnionTy1) = (if_var_4: UnionTy1)
                    var_63
            let (var_64: UnionTy1) = (if_var_3: UnionTy1)
            var_64
        else
            let (var_79: int64) = (!var_3)
            let (var_80: bool) = (var_6 = var_79)
            let (if_var_5: UnionTy1) =
                if var_80 then
                    let (var_83: RecTy0) = Rec0Case1
                    let (var_84: RecTy0) = method_5((var_5: RecTy0), (var_83: RecTy0))
                    Union1Case3(var_84)
                else
                    Union1Case1("many")
            let (var_92: UnionTy1) = (if_var_5: UnionTy1)
            var_92
    let (var_93: UnionTy1) = (if_var_2: UnionTy1)
    var_93
and method_3((var_2: string), (var_3: int64 ref), (var_5: RecTy0), (var_6: int64), (var_7: int64)): UnionTy1 =
    let (var_11: int64) = (!var_3)
    let (var_12: bool) = (var_11 >= 0L)
    let (var_13: int64) = (int64 var_2.Length)
    let (var_14: bool) = (var_11 < var_13)
    let (var_15: bool) = (var_12 && var_14)
    let (if_var_6: UnionTy1) =
        if var_15 then
            let (var_17: char) = var_2.[int32 (var_11)]
            let (var_21: int64) = (!var_3)
            let (var_22: int64) = (var_21 + 1L)
            var_3 := var_22
            let (var_25: bool) = (var_17 >= '0')
            let (var_26: bool) = (var_17 <= '9')
            let (var_27: bool) = (var_25 && var_26)
            let (if_var_7: UnionTy1) =
                if var_27 then
                    let (var_30: int64) = System.Convert.ToInt64(var_17)
                    let (var_31: int64) = System.Convert.ToInt64('0')
                    let (var_32: int64) = (var_30 - var_31)
                    let (var_33: int64) = (var_7 * 10L)
                    let (var_34: int64) = (var_33 + var_32)
                    let (var_36: UnionTy1) = method_3((var_2: string), (var_3: int64 ref), (var_5: RecTy0), (var_6: int64), (var_34: int64))
                    var_36
                else
                    let (var_45: UnionTy1) = method_4((var_2: string), (var_3: int64 ref), (var_5: RecTy0), (var_6: int64), (var_7: int64))
                    var_45
            let (var_46: UnionTy1) = (if_var_7: UnionTy1)
            var_46
        else
            let (var_56: UnionTy1) = method_4((var_2: string), (var_3: int64 ref), (var_5: RecTy0), (var_6: int64), (var_7: int64))
            var_56
    let (var_57: UnionTy1) = (if_var_6: UnionTy1)
    var_57
and method_4((var_2: string), (var_3: int64 ref), (var_5: RecTy0), (var_6: int64), (var_7: int64)): UnionTy1 =
    let (var_10: int64) = (!var_3)
    let (var_11: bool) = (var_10 >= 0L)
    let (var_12: int64) = (int64 var_2.Length)
    let (var_13: bool) = (var_10 < var_12)
    let (var_14: bool) = (var_11 && var_13)
    let (if_var_8: UnionTy1) =
        if var_14 then
            let (var_16: char) = var_2.[int32 (var_10)]
            let (var_20: int64) = (!var_3)
            let (var_21: int64) = (var_20 + 1L)
            var_3 := var_21
            let (var_24: bool) = (var_16 = ' ')
            let (var_25: bool) = (var_16 = '\n')
            let (var_26: bool) = (var_16 = '\r')
            let (var_27: bool) = (var_25 || var_26)
            let (var_28: bool) = (var_24 || var_27)
            let (if_var_9: UnionTy1) =
                if var_28 then
                    let (var_29: UnionTy1) = method_4((var_2: string), (var_3: int64 ref), (var_5: RecTy0), (var_6: int64), (var_7: int64))
                    var_29
                else
                    let (var_32: int64) = (!var_3)
                    let (var_33: int64) = (var_32 + -1L)
                    var_3 := var_33
                    let (var_43: int64) = (!var_3)
                    let (var_44: bool) = (var_6 < var_43)
                    let (if_var_10: UnionTy1) =
                        if var_44 then
                            let (var_46: RecTy0) = Rec0Case0(Tuple11(Tuple12(var_7, var_5)))
                            let (var_47: UnionTy1) = method_1((var_2: string), (var_3: int64 ref), (var_46: RecTy0))
                            var_47
                        else
                            let (var_50: int64) = (!var_3)
                            let (var_51: bool) = (var_6 = var_50)
                            let (if_var_13: UnionTy1) =
                                if var_51 then
                                    Union1Case1("Many parser succeeded without changing the parser state. Unless the computation had been aborted, the parser would have gone into an infinite loop.")
                                else
                                    Union1Case0(var_7)
                            let (var_58: UnionTy1) = (if_var_13: UnionTy1)
                            var_58
                    let (var_59: UnionTy1) = (if_var_10: UnionTy1)
                    var_59
            let (var_60: UnionTy1) = (if_var_9: UnionTy1)
            var_60
        else
            let (var_73: int64) = (!var_3)
            let (var_74: bool) = (var_6 < var_73)
            let (if_var_14: UnionTy1) =
                if var_74 then
                    let (var_76: RecTy0) = Rec0Case0(Tuple11(Tuple12(var_7, var_5)))
                    let (var_77: UnionTy1) = method_1((var_2: string), (var_3: int64 ref), (var_76: RecTy0))
                    var_77
                else
                    let (var_80: int64) = (!var_3)
                    let (var_81: bool) = (var_6 = var_80)
                    let (if_var_15: UnionTy1) =
                        if var_81 then
                            Union1Case1("Many parser succeeded without changing the parser state. Unless the computation had been aborted, the parser would have gone into an infinite loop.")
                        else
                            Union1Case0(var_7)
                    let (var_88: UnionTy1) = (if_var_15: UnionTy1)
                    var_88
            let (var_89: UnionTy1) = (if_var_14: UnionTy1)
            var_89
    let (var_90: UnionTy1) = (if_var_8: UnionTy1)
    var_90
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
printfn "%A" var_35
match var_35 with
| Union1Case3 x -> 
    match x with
    | Rec0Case0 x -> x.mem_1.mem_1