type Rec16 =
    | Rec16Case0
    | Rec16Case1 of Tuple2
and Tuple2 =
    struct
    val mem_0: int64
    val mem_1: Rec16
    new(arg_mem_0, arg_mem_1) = {mem_0 = arg_mem_0; mem_1 = arg_mem_1}
    end
Rec16Case1(Tuple2(1L, Rec16Case1(Tuple2(2L, Rec16Case1(Tuple2(3L, Rec16Case0))))))
