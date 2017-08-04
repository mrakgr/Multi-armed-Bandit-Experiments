type RecTy0 =
    | Rec0Case0 of Tuple4
    | Rec0Case1
and Tuple2 =
    struct
    val mem_0: int64
    val mem_1: RecTy0
    new(arg_mem_0, arg_mem_1) = {mem_0 = arg_mem_0; mem_1 = arg_mem_1}
    end
and Tuple4 =
    struct
    val mem_1: Tuple2
    new(arg_mem_1) = {mem_1 = arg_mem_1}
    end
let rec method_0(): unit =
    ()
and method_1((var_1: RecTy0)): int64 =
    let (if_var_1: int64) =
        match var_1 with
        | Rec0Case0(var_2) ->
            let (var_5: Tuple2) = var_2.mem_1
            let (var_7: int64) = var_5.mem_0
            let (var_8: RecTy0) = var_5.mem_1
            let (var_9: int64) = (0L + var_7)
            let (var_10: int64) = method_2((var_8: RecTy0), (var_9: int64))
            var_10
        | Rec0Case1 ->
            0L
    let (var_12: int64) = (if_var_1: int64)
    var_12
and method_2((var_1: RecTy0), (var_2: int64)): int64 =
    let (if_var_3: int64) =
        match var_1 with
        | Rec0Case0(var_3) ->
            let (var_6: Tuple2) = var_3.mem_1
            let (var_8: int64) = var_6.mem_0
            let (var_9: RecTy0) = var_6.mem_1
            let (var_10: int64) = (var_2 + var_8)
            let (var_11: int64) = method_2((var_9: RecTy0), (var_10: int64))
            var_11
        | Rec0Case1 ->
            var_2
    let (var_13: int64) = (if_var_3: int64)
    var_13
method_0()
let (var_23: RecTy0) = Rec0Case0(Tuple4(Tuple2(1L, Rec0Case0(Tuple4(Tuple2(2L, Rec0Case0(Tuple4(Tuple2(3L, Rec0Case1)))))))))
let (var_24: int64) = method_1((var_23: RecTy0))
var_24