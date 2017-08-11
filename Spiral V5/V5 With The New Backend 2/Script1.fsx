type Env14 =
    struct
    val mem_a: RecTy14
    new(arg_mem_a) = {mem_a = arg_mem_a}
    end
and Env15 =
    struct
    val mem_a: RecTy14
    new(arg_mem_a) = {mem_a = arg_mem_a}
    end
and Env17 =
    struct
    val mem_c: RecTy14
    new(arg_mem_c) = {mem_c = arg_mem_c}
    end
and Env18 =
    struct
    val mem_ main_arg: RecTy14
    new(arg_mem_ main_arg) = {mem_ main_arg = arg_mem_ main_arg}
    end
and Env19 =
    struct
    val mem_a: RecTy14
    new(arg_mem_a) = {mem_a = arg_mem_a}
    end
and Env20 =
    struct
    val mem_ main_arg: RecTy14
    new(arg_mem_ main_arg) = {mem_ main_arg = arg_mem_ main_arg}
    end
and Env21 =
    struct
    val mem_a: RecTy14
    new(arg_mem_a) = {mem_a = arg_mem_a}
    end
and Env22 =
    struct
    val mem_x: int64
    new(arg_mem_x) = {mem_x = arg_mem_x}
    end
and RecTy14 =
    | Rec14Case0 of Tuple2
    | Rec14Case1 of Tuple4
    | Rec14Case2 of Tuple6
and UnionTy0 =
    | Union0Case0 of Tuple2
    | Union0Case1 of Tuple4
and UnionTy1 =
    | Union1Case0 of Tuple2
    | Union1Case1 of Tuple4
    | Union1Case2 of Tuple6
and Tuple2 =
    struct
    val mem_1: int64
    new(arg_mem_1) = {mem_1 = arg_mem_1}
    end
and Tuple4 =
    struct
    val mem_1: RecTy14
    val mem_2: RecTy14
    new(arg_mem_1, arg_mem_2) = {mem_1 = arg_mem_1; mem_2 = arg_mem_2}
    end
and Tuple6 =
    struct
    val mem_1: RecTy14
    val mem_2: RecTy14
    new(arg_mem_1, arg_mem_2) = {mem_1 = arg_mem_1; mem_2 = arg_mem_2}
    end
let rec method_13(): unit =
    ()
method_13()
21L