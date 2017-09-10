type Rec16 =
    | Rec16Case0
    | Rec16Case1 of Tuple2
and Tuple2 =
    struct
    val mem_0: int64
    val mem_1: Rec16
    new(arg_mem_0, arg_mem_1) = {mem_0 = arg_mem_0; mem_1 = arg_mem_1}
    end
let (var_0: Rec16) = Rec16Case0
match var_0 with
| Rec16Case0 ->
    0L
| Rec16Case1(var_2) ->
    let (var_3: int64) = var_2.mem_0
    let (var_4: Rec16) = var_2.mem_1
    match var_4 with
    | Rec16Case0 ->
        55L
    | Rec16Case1(var_6) ->
        let (var_7: int64) = var_6.mem_0
        let (var_8: Rec16) = var_6.mem_1
        match var_8 with
        | Rec16Case0 ->
            let (var_11: int64) = (var_3 + var_7)
            (var_11 + 10L)
        | Rec16Case1(var_10) ->
            let (var_12: int64) = var_10.mem_0
            let (var_13: Rec16) = var_10.mem_1
            (var_3 + var_7)

