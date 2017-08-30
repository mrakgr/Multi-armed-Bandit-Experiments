type Rec15 =
    | Rec15Case0 of Tuple2
    | Rec15Case1 of Tuple4
    | Rec15Case2 of Tuple6
and Tuple2 =
    struct
    val mem_1: int64
    new(arg_mem_1) = {mem_1 = arg_mem_1}
    end
and Tuple4 =
    struct
    val mem_1: Rec15
    val mem_2: Rec15
    new(arg_mem_1, arg_mem_2) = {mem_1 = arg_mem_1; mem_2 = arg_mem_2}
    end
and Tuple6 =
    struct
    val mem_1: Rec15
    val mem_2: Rec15
    new(arg_mem_1, arg_mem_2) = {mem_1 = arg_mem_1; mem_2 = arg_mem_2}
    end
let rec method_14(): unit =
    ()
and method_19(): Rec15 =
    Rec15Case1(Tuple4(Rec15Case0(Tuple2(1L)), Rec15Case0(Tuple2(2L))))
and method_20(): Rec15 =
    Rec15Case1(Tuple4(Rec15Case0(Tuple2(3L)), Rec15Case0(Tuple2(4L))))
and method_21((var_1: Rec15), (var_2: Rec15)): int64 =
    let (var_5: int64) = method_22((var_1: Rec15))
    let (var_6: int64) = method_22((var_2: Rec15))
    (var_5 * var_6)
and method_22((var_0: Rec15)): int64 =
    let (if_var_1: int64) =
        match var_0 with
        | Rec15Case0(var_1) ->
            let (var_5: int64) = var_1.mem_1
            var_5
        | Rec15Case1(var_2) ->
            let (var_8: Rec15) = var_2.mem_1
            let (var_9: Rec15) = var_2.mem_2
            let (var_11: int64) = method_22((var_8: Rec15))
            let (var_12: int64) = method_22((var_9: Rec15))
            (var_11 + var_12)
        | Rec15Case2(var_3) ->
            let (var_14: Rec15) = var_3.mem_1
            let (var_15: Rec15) = var_3.mem_2
            let (var_18: int64) = method_22((var_14: Rec15))
            let (var_19: int64) = method_22((var_15: Rec15))
            (var_18 * var_19)
    if_var_1
method_14()
let (var_17: Rec15) = method_19()
let (var_18: Rec15) = method_20()
let (var_20: int64) = method_21((var_17: Rec15), (var_18: Rec15))
var_20
