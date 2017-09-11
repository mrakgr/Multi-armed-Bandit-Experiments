type Rec18 =
    | Rec18Case0
    | Rec18Case1 of Tuple2
and Tuple2 =
    struct
    val mem_0: float32
    val mem_1: Rec18
    new(arg_mem_0, arg_mem_1) = {mem_0 = arg_mem_0; mem_1 = arg_mem_1}
    end
let rec method_16((var_0: int64)): Rec18 =
    if (var_0 < 10L) then
        let (var_1: int64) = (var_0 + 1L)
        let (var_2: Rec18) = method_16((var_1: int64))
        Rec18Case1(Tuple2(2.200000f, var_2))
    else
        Rec18Case0
let (var_0: int64) = 0L
method_16((var_0: int64))
