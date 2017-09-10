type Rec17 =
    | Rec17Case0
    | Rec17Case1 of Tuple2
and Tuple2 =
    struct
    val mem_0: int64
    val mem_1: Rec17
    new(arg_mem_0, arg_mem_1) = {mem_0 = arg_mem_0; mem_1 = arg_mem_1}
    end
let (var_0: Rec17) = Rec17Case1(Tuple2(1L, Rec17Case1(Tuple2(2L, Rec17Case1(Tuple2(3L, Rec17Case0))))))
let (var_1: Rec17) = Rec17Case1(Tuple2(4L, Rec17Case1(Tuple2(5L, Rec17Case1(Tuple2(6L, Rec17Case0))))))
match var_1 with
| Rec17Case0 ->
    ()
| Rec17Case1(var_3) ->
    let (var_4: int64) = var_3.mem_0
    let (var_5: Rec17) = var_3.mem_1
match var_0 with
| Rec17Case0 ->
    ()
| Rec17Case1(var_7) ->
    let (var_8: int64) = var_7.mem_0
    let (var_9: Rec17) = var_7.mem_1
match var_0 with
| Rec17Case0 ->
    ()
| Rec17Case1(var_11) ->
    let (var_12: int64) = var_11.mem_0
    let (var_13: Rec17) = var_11.mem_1

