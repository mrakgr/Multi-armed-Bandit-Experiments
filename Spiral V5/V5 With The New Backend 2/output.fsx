type Env0 =
    struct
    val mem_a: int64
    val mem_b: int64
    val mem_c: int64
    val mem_d: int64
    val mem_e: int64
    new(arg_mem_a, arg_mem_b, arg_mem_c, arg_mem_d, arg_mem_e) = {mem_a = arg_mem_a; mem_b = arg_mem_b; mem_c = arg_mem_c; mem_d = arg_mem_d; mem_e = arg_mem_e}
    end
Env0(1L, 2L, 3L, 4L, 5L)
