type Rec0 =
    | Rec0Case0
    | Rec0Case1 of Tuple1
and Tuple1 =
    struct
    val mem_0: float32
    val mem_1: Rec0
    new(arg_mem_0, arg_mem_1) = {mem_0 = arg_mem_0; mem_1 = arg_mem_1}
    end
let rec method_15((var_0: int64)): Rec0 =
    if (var_0 < 10L) then
        let (var_1: int64) = (var_0 + 1L)
        let (var_2: Rec0) = method_15((var_1: int64))
        Rec0Case1(Tuple1(2.200000f, var_2))
    else
        Rec0Case0
let (var_0: int64) = 0L
method_15((var_0: int64))
