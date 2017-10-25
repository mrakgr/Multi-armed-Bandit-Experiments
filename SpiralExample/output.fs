module SpiralExample
let cuda_kernels = """
extern "C" {
}
"""

type Union0 =
    | Union0Case0 of Tuple2
    | Union0Case1 of Tuple4
and Tuple1 =
    struct
    val mem_0: int64
    val mem_1: int64
    new(arg_mem_0, arg_mem_1) = {mem_0 = arg_mem_0; mem_1 = arg_mem_1}
    end
and Tuple2 =
    struct
    val mem_0: Tuple1
    new(arg_mem_0) = {mem_0 = arg_mem_0}
    end
and Union3 =
    | Union3Case0 of Tuple2
    | Union3Case1 of Tuple4
    | Union3Case2
and Tuple4 =
    struct
    val mem_0: string
    new(arg_mem_0) = {mem_0 = arg_mem_0}
    end
let (var_0: string) = cuda_kernels
let (var_1: Union0) = (Union0Case0(Tuple2(Tuple1(2L, 3L))))
match var_1 with
| Union0Case0(var_2) ->
    let (var_4: Tuple1) = var_2.mem_0
    (Union3Case0(Tuple2(var_4)))
| Union0Case1(var_3) ->
    let (var_5: string) = var_3.mem_0
    (Union3Case1(Tuple4(var_5)))

