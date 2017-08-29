type Tuple1 =
    struct
    val mem_0: int64
    val mem_1: int64
    new(arg_mem_0, arg_mem_1) = {mem_0 = arg_mem_0; mem_1 = arg_mem_1}
    end
and Tuple2 =
    struct
    val mem_0: string
    val mem_1: Tuple1
    new(arg_mem_0, arg_mem_1) = {mem_0 = arg_mem_0; mem_1 = arg_mem_1}
    end
Tuple2("Coord", Tuple1(1L, 2L))
