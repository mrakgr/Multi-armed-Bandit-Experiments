type Tuple1 =
    struct
    val mem_1: bool
    new(arg_mem_1) = {mem_1 = arg_mem_1}
    end
and Tuple2 =
    struct
    val mem_0: int64
    val mem_1: int64
    val mem_2: int64
    val mem_3: int64
    new(arg_mem_0, arg_mem_1, arg_mem_2, arg_mem_3) = {mem_0 = arg_mem_0; mem_1 = arg_mem_1; mem_2 = arg_mem_2; mem_3 = arg_mem_3}
    end
and Tuple3 =
    struct
    val mem_1: Tuple2
    new(arg_mem_1) = {mem_1 = arg_mem_1}
    end
and Tuple4 =
    struct
    val mem_0: Tuple1
    val mem_1: float32
    val mem_2: string
    val mem_3: Tuple3
    new(arg_mem_0, arg_mem_1, arg_mem_2, arg_mem_3) = {mem_0 = arg_mem_0; mem_1 = arg_mem_1; mem_2 = arg_mem_2; mem_3 = arg_mem_3}
    end
Tuple4(Tuple1(true), 2.200000f, "a", Tuple3(Tuple2(1L, 2L, 3L, 4L)))
