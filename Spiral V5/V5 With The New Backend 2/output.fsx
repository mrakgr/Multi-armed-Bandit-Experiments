type Rec20 =
    | Rec20Case0
    | Rec20Case1 of Tuple4
and Rec16 =
    | Rec16Case0
    | Rec16Case1 of Tuple2
and Tuple2 =
    struct
    val mem_0: int64
    val mem_1: Rec16
    new(arg_mem_0, arg_mem_1) = {mem_0 = arg_mem_0; mem_1 = arg_mem_1}
    end
and Tuple4 =
    struct
    val mem_0: Rec16
    val mem_1: Rec20
    new(arg_mem_0, arg_mem_1) = {mem_0 = arg_mem_0; mem_1 = arg_mem_1}
    end
let rec method_25((var_0: Rec20)): Rec16 =
    let (var_1: Rec16) = Rec16Case0
    match var_0 with
    | Rec20Case0 ->
        var_1
    | Rec20Case1(var_3) ->
        let (var_4: Rec16) = var_3.mem_0
        let (var_5: Rec20) = var_3.mem_1
        let (var_6: Rec16) = method_28((var_5: Rec20), (var_1: Rec16))
        method_29((var_4: Rec16), (var_6: Rec16))
and method_28((var_0: Rec20), (var_1: Rec16)): Rec16 =
    match var_0 with
    | Rec20Case0 ->
        var_1
    | Rec20Case1(var_3) ->
        let (var_4: Rec16) = var_3.mem_0
        let (var_5: Rec20) = var_3.mem_1
        let (var_6: Rec16) = method_28((var_5: Rec20), (var_1: Rec16))
        method_29((var_4: Rec16), (var_6: Rec16))
and method_29((var_0: Rec16), (var_1: Rec16)): Rec16 =
    match var_0 with
    | Rec16Case0 ->
        var_1
    | Rec16Case1(var_3) ->
        let (var_4: int64) = var_3.mem_0
        let (var_5: Rec16) = var_3.mem_1
        let (var_6: Rec16) = method_29((var_5: Rec16), (var_1: Rec16))
        Rec16Case1(Tuple2(var_4, var_6))
let (var_0: Rec16) = Rec16Case1(Tuple2(1L, Rec16Case1(Tuple2(2L, Rec16Case1(Tuple2(3L, Rec16Case0))))))
let (var_1: Rec16) = Rec16Case1(Tuple2(4L, Rec16Case1(Tuple2(5L, Rec16Case1(Tuple2(6L, Rec16Case0))))))
let (var_2: Rec20) = Rec20Case1(Tuple4(var_0, Rec20Case1(Tuple4(var_1, Rec20Case0))))
method_25((var_2: Rec20))
