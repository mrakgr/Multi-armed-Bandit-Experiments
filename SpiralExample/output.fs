module SpiralExample
let cuda_kernels = """
extern "C" {
}
"""

type Rec0 =
    | Rec0Case0 of Tuple2
    | Rec0Case1 of Tuple3
    | Rec0Case2 of Tuple1
and Tuple1 =
    struct
    val mem_0: int64
    new(arg_mem_0) = {mem_0 = arg_mem_0}
    end
and Tuple2 =
    struct
    val mem_0: Rec0
    val mem_1: Rec0
    new(arg_mem_0, arg_mem_1) = {mem_0 = arg_mem_0; mem_1 = arg_mem_1}
    end
and Tuple3 =
    struct
    val mem_0: Rec0
    val mem_1: Rec0
    new(arg_mem_0, arg_mem_1) = {mem_0 = arg_mem_0; mem_1 = arg_mem_1}
    end
let rec method_1((var_0: Rec0), (var_1: Rec0)): int64 =
    let (var_2: int64) = method_2((var_0: Rec0), (var_1: Rec0))
    let (var_3: int64) = method_4((var_0: Rec0), (var_1: Rec0))
    (var_2 * var_3)
and method_2((var_0: Rec0), (var_1: Rec0)): int64 =
    match var_0 with
    | Rec0Case0(var_2) ->
        let (var_5: Rec0) = var_2.mem_0
        let (var_6: Rec0) = var_2.mem_1
        let (var_7: int64) = method_3((var_0: Rec0), (var_1: Rec0), (var_5: Rec0))
        let (var_8: int64) = method_3((var_0: Rec0), (var_1: Rec0), (var_6: Rec0))
        (var_7 + var_8)
    | Rec0Case1(var_3) ->
        let (var_9: Rec0) = var_3.mem_0
        let (var_10: Rec0) = var_3.mem_1
        let (var_11: int64) = method_3((var_0: Rec0), (var_1: Rec0), (var_9: Rec0))
        let (var_12: int64) = method_3((var_0: Rec0), (var_1: Rec0), (var_10: Rec0))
        (var_11 * var_12)
    | Rec0Case2(var_4) ->
        var_4.mem_0
and method_4((var_0: Rec0), (var_1: Rec0)): int64 =
    match var_1 with
    | Rec0Case0(var_2) ->
        let (var_5: Rec0) = var_2.mem_0
        let (var_6: Rec0) = var_2.mem_1
        let (var_7: int64) = method_3((var_0: Rec0), (var_1: Rec0), (var_5: Rec0))
        let (var_8: int64) = method_3((var_0: Rec0), (var_1: Rec0), (var_6: Rec0))
        (var_7 + var_8)
    | Rec0Case1(var_3) ->
        let (var_9: Rec0) = var_3.mem_0
        let (var_10: Rec0) = var_3.mem_1
        let (var_11: int64) = method_3((var_0: Rec0), (var_1: Rec0), (var_9: Rec0))
        let (var_12: int64) = method_3((var_0: Rec0), (var_1: Rec0), (var_10: Rec0))
        (var_11 * var_12)
    | Rec0Case2(var_4) ->
        var_4.mem_0
and method_3((var_0: Rec0), (var_1: Rec0), (var_2: Rec0)): int64 =
    match var_2 with
    | Rec0Case0(var_3) ->
        let (var_6: Rec0) = var_3.mem_0
        let (var_7: Rec0) = var_3.mem_1
        let (var_8: int64) = method_3((var_0: Rec0), (var_1: Rec0), (var_6: Rec0))
        let (var_9: int64) = method_3((var_0: Rec0), (var_1: Rec0), (var_7: Rec0))
        (var_8 + var_9)
    | Rec0Case1(var_4) ->
        let (var_10: Rec0) = var_4.mem_0
        let (var_11: Rec0) = var_4.mem_1
        let (var_12: int64) = method_3((var_0: Rec0), (var_1: Rec0), (var_10: Rec0))
        let (var_13: int64) = method_3((var_0: Rec0), (var_1: Rec0), (var_11: Rec0))
        (var_12 * var_13)
    | Rec0Case2(var_5) ->
        var_5.mem_0
let (var_0: Rec0) = (Rec0Case0(Tuple2((Rec0Case2(Tuple1(1L))), (Rec0Case2(Tuple1(2L))))))
let (var_1: Rec0) = (Rec0Case0(Tuple2((Rec0Case2(Tuple1(3L))), (Rec0Case2(Tuple1(4L))))))
method_1((var_0: Rec0), (var_1: Rec0))
