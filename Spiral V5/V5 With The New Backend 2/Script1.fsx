type Rec19 =
    | Rec2Case0 of Tuple6
    | Rec2Case1
and Union3 =
    | Union3Case0 of int64
    | Union3Case1 of string
    | Union3Case2 of Tuple2
    | Union3Case3 of Rec19
and Tuple5 =
    struct
    val mem_0: int64
    val mem_1: Rec19
    new(arg_mem_0, arg_mem_1) = {mem_0 = arg_mem_0; mem_1 = arg_mem_1}
    end
and Tuple6 =
    struct
    val mem_1: Tuple5
    new(arg_mem_1) = {mem_1 = arg_mem_1}
    end
and Union2 =
    | Union2Case0 of Tuple6
    | Union2Case1
and Tuple2 =
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
            let (var_19: int64) = (!var_5)
            var_5 := (var_19 + 1L)
            let (var_22: char) = var_4.[int32 (var_13)]
            let (var_23: bool) = method_25((var_22: char))
            let (if_var_2: Union3) =
                if var_23 then
                    let (var_26: int64) = System.Convert.ToInt64(var_22)
                    let (var_27: int64) = System.Convert.ToInt64('0')
                    let (var_28: int64) = (var_26 - var_27)
                    let (var_29: int64) = (var_10 * 10L)
                    let (var_30: int64) = (var_29 + var_28)
                    let (var_32: Union3) = method_26((var_4: string), (var_5: int64 ref), (var_7: Rec19), (var_8: int64), (var_30: int64))
                    var_32
                else
                    let (var_46: int64) = (!var_5)
                    let (if_var_3: Union3) =
                        if (var_8 = var_46) then
                            let (var_49: Rec19) = Rec19Case1
                            let (var_50: Rec19) = method_30((var_7: Rec19), (var_49: Rec19))
                            Union3Case3(var_50)
                        else
                            Union3Case1("many")
                    if_var_3
            if_var_2
        else
            let (var_72: int64) = (!var_5)
            let (if_var_4: Union3) =
                if (var_8 = var_72) then
                    let (var_75: Rec19) = Rec19Case1
                    let (var_76: Rec19) = method_30((var_7: Rec19), (var_75: Rec19))
                    Union3Case3(var_76)
                else
                    Union3Case1("many")
            if_var_4
    if_var_1
and method_24((var_0: string), (var_1: int64)): bool =
    let (var_2: bool) = (var_1 >= 0L)
    let (var_3: int64) = (int64 var_0.Length)
    let (var_4: bool) = (var_1 < var_3)
    (var_2 && var_4)
and method_25((var_0: char)): bool =
    let (var_1: bool) = (var_0 >= '0')
    let (var_2: bool) = (var_0 <= '9')
    (var_1 && var_2)
and method_26((var_4: string), (var_5: int64 ref), (var_7: Rec19), (var_8: int64), (var_9: int64)): Union3 =
    let (var_13: int64) = (!var_5)
    let (var_14: bool) = method_24((var_4: string), (var_13: int64))
    let (if_var_5: Union3) =
        if var_14 then
            let (var_19: int64) = (!var_5)
            var_5 := (var_19 + 1L)
            let (var_22: char) = var_4.[int32 (var_13)]
            let (var_23: bool) = method_25((var_22: char))
            let (if_var_6: Union3) =
                if var_23 then
                    let (var_26: int64) = System.Convert.ToInt64(var_22)
                    let (var_27: int64) = System.Convert.ToInt64('0')
                    let (var_28: int64) = (var_26 - var_27)
                    let (var_29: int64) = (var_9 * 10L)
                    let (var_30: int64) = (var_29 + var_28)
                    let (var_32: Union3) = method_26((var_4: string), (var_5: int64 ref), (var_7: Rec19), (var_8: int64), (var_30: int64))
                    var_32
                else
                    let (var_41: Union3) = method_27((var_4: string), (var_5: int64 ref), (var_7: Rec19), (var_8: int64), (var_9: int64))
                    var_41
            if_var_6
        else
            let (var_51: Union3) = method_27((var_4: string), (var_5: int64 ref), (var_7: Rec19), (var_8: int64), (var_9: int64))
            var_51
    if_var_5
and method_27((var_4: string), (var_5: int64 ref), (var_7: Rec19), (var_8: int64), (var_9: int64)): Union3 =
    let (var_12: int64) = (!var_5)
    let (var_13: bool) = method_24((var_4: string), (var_12: int64))
    let (if_var_7: Union3) =
        if var_13 then
            let (var_18: int64) = (!var_5)
            var_5 := (var_18 + 1L)
            let (var_21: char) = var_4.[int32 (var_12)]
            let (var_22: bool) = method_28((var_21: char))
            let (var_23: bool) = method_29((var_21: char))
            let (if_var_8: Union3) =
                if (var_22 || var_23) then
                    let (var_24: Union3) = method_27((var_4: string), (var_5: int64 ref), (var_7: Rec19), (var_8: int64), (var_9: int64))
                    var_24
                else
                    let (var_27: int64) = (!var_5)
                    var_5 := (var_27 + -1L)
                    let (var_37: int64) = (!var_5)
                    let (if_var_9: Union3) =
                        if (var_8 < var_37) then
                            let (var_39: Rec19) = Rec19Case0(Tuple6(Tuple5(var_9, var_7)))
                            let (var_40: Union3) = method_22((var_4: string), (var_5: int64 ref), (var_39: Rec19))
                            var_40
                        else
                            let (var_43: int64) = (!var_5)
                            let (if_var_10: Union3) =
                                if (var_8 = var_43) then
                                    Union3Case1("Many parser succeeded without changing the parser state. Unless the computation had been aborted, the parser would have gone into an infinite loop.")
                                else
                                    Union3Case0(var_9)
                            if_var_10
                    if_var_9
            if_var_8
        else
            let (var_62: int64) = (!var_5)
            let (if_var_11: Union3) =
                if (var_8 < var_62) then
                    let (var_64: Rec19) = Rec19Case0(Tuple6(Tuple5(var_9, var_7)))
                    let (var_65: Union3) = method_22((var_4: string), (var_5: int64 ref), (var_64: Rec19))
                    var_65
                else
                    let (var_68: int64) = (!var_5)
                    let (if_var_12: Union3) =
                        if (var_8 = var_68) then
                            Union3Case1("Many parser succeeded without changing the parser state. Unless the computation had been aborted, the parser would have gone into an infinite loop.")
                        else
                            Union3Case0(var_9)
                    if_var_12
            if_var_11
    if_var_7
and method_28((var_0: char)): bool =
    (var_0 = ' ')
and method_29((var_0: char)): bool =
    let (var_1: bool) = (var_0 = '\n')
    let (var_2: bool) = (var_0 = '\r')
    (var_1 || var_2)
and method_30((var_1: Rec19), (var_2: Rec19)): Rec19 =
    let (if_var_13: Rec19) =
        match var_1 with
        | Rec19Case0(var_3) ->
            let (var_6: Tuple5) = var_3.mem_1
            let (var_9: int64) = var_6.mem_0
            let (var_10: Rec19) = var_6.mem_1
            let (var_12: Rec19) = Rec19Case0(Tuple6(Tuple5(var_9, var_2)))
            let (var_13: Rec19) = method_30((var_10: Rec19), (var_12: Rec19))
            var_13
        | Rec19Case1 ->
            var_2
    if_var_13
let (var_31: string) = "12 34 "
let (var_33: int64 ref) = (ref 0L)
method_18()
let (var_36: Rec19) = Rec19Case1
let (var_37: Union3) = method_22((var_31: string), (var_33: int64 ref), (var_36: Rec19))
match var_37 with
| Union3Case0(var_38) ->
    ()
| Union3Case1(var_39) ->
    ()
| Union3Case2(var_40) ->
    let (var_43: int64) = var_40.mem_0
    let (var_44: int64) = var_40.mem_1
    ()
| Union3Case3(var_41) ->
    let (var_47: Tuple5) = var_41.mem_1
    let (var_49: int64) = var_47.mem_0
    let (var_50: Rec19) = var_47.mem_1
    match var_50 with
    | Rec19Case0(var_51) ->
        let (var_54: Tuple5) = var_51.mem_1
        let (var_56: int64) = var_54.mem_0
        let (var_57: Rec19) = var_54.mem_1
        match var_57 with
        | Rec19Case0(var_58) ->
            let (var_61: Tuple5) = var_58.mem_1
            ()
        | Rec19Case1 ->
            System.Console.Write(var_49)
            System.Console.WriteLine()
            System.Console.Write(var_56)
            System.Console.WriteLine()
            ()
        ()
    | Rec19Case1 ->
        ()
    ()
| Union3Case4 ->
    ()