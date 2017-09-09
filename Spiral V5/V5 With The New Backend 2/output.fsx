type Rec18 =
    | Rec18Case0
    | Rec18Case1 of Tuple2
and Tuple2 =
    struct
    val mem_0: int64
    val mem_1: Rec18
    new(arg_mem_0, arg_mem_1) = {mem_0 = arg_mem_0; mem_1 = arg_mem_1}
    end
let rec method_17(): unit =
    ()
method_17()
method_17()
method_17()
method_17()
method_17()
method_17()
Rec18Case1(Tuple2(1L, Rec18Case1(Tuple2(2L, Rec18Case1(Tuple2(3L, Rec18Case0))))))
