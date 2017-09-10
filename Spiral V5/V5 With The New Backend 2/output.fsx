type Rec17 =
    | Rec17Case0
    | Rec17Case1 of Tuple2
and Rec23 =
    | Rec23Case0
    | Rec23Case1 of Tuple4
and Tuple2 =
    struct
    val mem_0: int64
    val mem_1: Rec17
    new(arg_mem_0, arg_mem_1) = {mem_0 = arg_mem_0; mem_1 = arg_mem_1}
    end
and Tuple4 =
    struct
    val mem_0: Rec17
    val mem_1: Rec23
    new(arg_mem_0, arg_mem_1) = {mem_0 = arg_mem_0; mem_1 = arg_mem_1}
    end
let (var_0: Rec17) = Rec17Case0
Rec23Case0
