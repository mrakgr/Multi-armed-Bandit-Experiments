type Rec0 =
    | Rec0Case0 of Tuple2
    | Rec0Case1
and Tuple1 =
    struct
    val mem_0: int64
    val mem_1: Rec0
    new(arg_mem_0, arg_mem_1) = {mem_0 = arg_mem_0; mem_1 = arg_mem_1}
    end
and Tuple2 =
    struct
    val mem_1: Tuple1
    new(arg_mem_1) = {mem_1 = arg_mem_1}
    end
let rec method_13(): unit =
    ()
and method_17((var_1: Rec0), (var_2: int64)): int64 =
    let (if_var_1: int64) =
        match var_1 with
        | Rec0Case0(var_3) ->
            let (var_6: Tuple1) = var_3.mem_1
            let (var_8: int64) = var_6.mem_0
            let (var_9: Rec0) = var_6.mem_1
            let (var_10: int64) = (var_2 + var_8)
            let (var_11: int64) = method_17((var_9: Rec0), (var_10: int64))
            var_11
        | Rec0Case1 ->
            var_2
    if_var_1
method_13()
let (var_23: Rec0) = Rec0Case0(Tuple2(Tuple1(1L, Rec0Case0(Tuple2(Tuple1(2L, Rec0Case0(Tuple2(Tuple1(3L, Rec0Case1)))))))))
let (var_24: int64) = 0L
let (var_25: int64) = method_17((var_23: Rec0), (var_24: int64))
var_25