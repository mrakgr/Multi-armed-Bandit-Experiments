type Rec0 =
    | Rec0Case0 of Tuple2
    | Rec0Case1 of Tuple3
    | Rec0Case2 of Tuple1
and Tuple1 =
    struct
    val mem_1: int64
    new(arg_mem_1) = {mem_1 = arg_mem_1}
    end
and Tuple2 =
    struct
    val mem_1: Rec0
    val mem_2: Rec0
    new(arg_mem_1, arg_mem_2) = {mem_1 = arg_mem_1; mem_2 = arg_mem_2}
    end
and Tuple3 =
    struct
    val mem_1: Rec0
    val mem_2: Rec0
    new(arg_mem_1, arg_mem_2) = {mem_1 = arg_mem_1; mem_2 = arg_mem_2}
    end
let rec method_5(): Rec0 =
    (Rec0Case0(Tuple2((Rec0Case2(Tuple1(1L))), (Rec0Case2(Tuple1(2L))))))
and method_6(): Rec0 =
    (Rec0Case0(Tuple2((Rec0Case2(Tuple1(3L))), (Rec0Case2(Tuple1(4L))))))
and method_7((var_0: Rec0), (var_1: Rec0)): int64 =
    let (var_2: int64) = method_8((var_4: Rec0))
    let (var_3: int64) = method_8((var_4: Rec0))
    (var_2 * var_3)
and method_8((var_0: Rec0)): int64 =
    match var_0 with
    | Rec0Case0(var_1) ->
        let (var_4: Rec0) = var_1.mem_1
        let (var_5: Rec0) = var_1.mem_2
        let (var_6: int64) = method_8((var_4: Rec0))
        let (var_7: int64) = method_8((var_4: Rec0))
        (var_6 + var_7)
    | Rec0Case1(var_2) ->
        let (var_8: Rec0) = var_2.mem_1
        let (var_9: Rec0) = var_2.mem_2
        let (var_10: int64) = method_8((var_4: Rec0))
        let (var_11: int64) = method_8((var_4: Rec0))
        (var_10 * var_11)
    | Rec0Case2(var_3) ->
        var_3.mem_1
let (var_0: Rec0) = method_5()
let (var_1: Rec0) = method_6()
method_7((var_0: Rec0), (var_1: Rec0))
