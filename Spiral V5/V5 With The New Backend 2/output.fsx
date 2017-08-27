type Tuple1 =
    struct
    val mem_0: bool
    val mem_1: bool
    val mem_2: string
    new(arg_mem_0, arg_mem_1, arg_mem_2) = {mem_0 = arg_mem_0; mem_1 = arg_mem_1; mem_2 = arg_mem_2}
    end
and Tuple2 =
    struct
    val mem_0: float32
    val mem_1: float32
    new(arg_mem_0, arg_mem_1) = {mem_0 = arg_mem_0; mem_1 = arg_mem_1}
    end
and Tuple3 =
    struct
    val mem_0: Tuple2
    val mem_1: Tuple2
    new(arg_mem_0, arg_mem_1) = {mem_0 = arg_mem_0; mem_1 = arg_mem_1}
    end
and Tuple4 =
    struct
    val mem_0: string
    val mem_1: string
    val mem_2: string
    new(arg_mem_0, arg_mem_1, arg_mem_2) = {mem_0 = arg_mem_0; mem_1 = arg_mem_1; mem_2 = arg_mem_2}
    end
and Tuple5 =
    struct
    val mem_0: Tuple4
    val mem_1: Tuple4
    val mem_2: Tuple4
    new(arg_mem_0, arg_mem_1, arg_mem_2) = {mem_0 = arg_mem_0; mem_1 = arg_mem_1; mem_2 = arg_mem_2}
    end
and Tuple6 =
    struct
    val mem_0: int64
    val mem_1: int64
    val mem_2: int64
    val mem_3: int64
    new(arg_mem_0, arg_mem_1, arg_mem_2, arg_mem_3) = {mem_0 = arg_mem_0; mem_1 = arg_mem_1; mem_2 = arg_mem_2; mem_3 = arg_mem_3}
    end
and Tuple7 =
    struct
    val mem_0: Tuple6
    val mem_1: Tuple6
    val mem_2: string
    new(arg_mem_0, arg_mem_1, arg_mem_2) = {mem_0 = arg_mem_0; mem_1 = arg_mem_1; mem_2 = arg_mem_2}
    end
and Tuple8 =
    struct
    val mem_0: Tuple1
    val mem_1: Tuple3
    val mem_2: Tuple5
    val mem_3: Tuple7
    new(arg_mem_0, arg_mem_1, arg_mem_2, arg_mem_3) = {mem_0 = arg_mem_0; mem_1 = arg_mem_1; mem_2 = arg_mem_2; mem_3 = arg_mem_3}
    end
Tuple8(Tuple1(true, true, "rest"), Tuple3(Tuple2(2.200000f, 3.300000f), Tuple2(2.200000f, 3.300000f)), Tuple5(Tuple4("a", "b", "c"), Tuple4("a", "b", "c"), Tuple4("a", "b", "c")), Tuple7(Tuple6(1L, 2L, 3L, 4L), Tuple6(1L, 2L, 3L, 4L), "rest"))
