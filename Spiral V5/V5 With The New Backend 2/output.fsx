type Rec16 =
    | Rec16Case0
    | Rec16Case1 of Tuple2
and Rec20 =
    | Rec20Case0
    | Rec20Case1 of Tuple4
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
let (var_0: Rec16) = Rec16Case0
Rec20Case0
