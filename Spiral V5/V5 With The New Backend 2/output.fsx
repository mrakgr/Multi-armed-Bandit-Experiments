type Env2 =
    struct
    val mem_a: int64
    val mem_b: int64
    val mem_c: int64
    new(arg_mem_a, arg_mem_b, arg_mem_c) = {mem_a = arg_mem_a; mem_b = arg_mem_b; mem_c = arg_mem_c}
    end
and Tuple1 =
    struct
    val mem_0: Env2
    val mem_1: int64
    new(arg_mem_0, arg_mem_1) = {mem_0 = arg_mem_0; mem_1 = arg_mem_1}
    end
Tuple1(Env2(4L, 2L, 3L), 9L)
