type Env0 =
    struct
    val mem_ar: (int64 [])
    val mem_from: int64
    val mem_to: int64
    new(arg_mem_ar, arg_mem_from, arg_mem_to) = {mem_ar = arg_mem_ar; mem_from = arg_mem_from; mem_to = arg_mem_to}
    end
let rec method_14((var_0: (Env0 ref)), (var_1: int64)): unit =
    let (var_2: Env0) = (!var_0)
    let (var_3: (int64 [])) = var_2.mem_ar
    let (var_4: int64) = var_2.mem_from
    let (var_5: int64) = var_2.mem_to
    var_3.[int32 var_5] <- var_1
    let (var_6: int64) = var_3.LongLength
    let (var_7: int64) = (var_5 + 1L)
    let (var_8: int64) =
        if (var_7 = var_6) then
            0L
        else
            var_7
    let (var_14: Env0) =
        if (var_4 = var_8) then
            let (var_9: int64) = (var_6 * 3L)
            let (var_10: int64) = (var_9 / 2L)
            let (var_11: int64) = (var_10 + 3L)
            let (var_12: (int64 [])) = Array.zeroCreate<int64> (System.Convert.ToInt32(var_11))
            method_15((var_3: (int64 [])), (var_12: (int64 [])), (var_4: int64), (var_6: int64))
            let (var_13: int64) = 0L
            method_17((var_3: (int64 [])), (var_12: (int64 [])), (var_4: int64), (var_6: int64), (var_13: int64))
            (Env0(var_12, 0L, var_6))
        else
            (Env0(var_3, var_4, var_8))
    var_0 := var_14
and method_15((var_0: (int64 [])), (var_1: (int64 [])), (var_2: int64), (var_3: int64)): unit =
    if (var_2 < var_3) then
        let (var_4: int64) = var_0.[int32 var_2]
        var_1.[int32 (var_2 - var_2)] <- var_4
        let (var_5: int64) = (var_2 + 1L)
        method_16((var_0: (int64 [])), (var_1: (int64 [])), (var_2: int64), (var_3: int64), (var_5: int64))
    else
        ()
and method_16((var_0: (int64 [])), (var_1: (int64 [])), (var_2: int64), (var_3: int64), (var_4: int64)): unit =
    if (var_4 < var_3) then
        let (var_5: int64) = var_0.[int32 var_4]
        var_1.[int32 (var_4 - var_2)] <- var_5
        let (var_6: int64) = (var_4 + 1L)
        method_16((var_0: (int64 [])), (var_1: (int64 [])), (var_2: int64), (var_3: int64), (var_6: int64))
    else
        ()
and method_17((var_0: (int64 [])), (var_1: (int64 [])), (var_2: int64), (var_3: int64), (var_4: int64)): unit =
    if (var_4 < var_2) then
        let (var_5: int64) = (var_3 - var_2)
        let (var_6: int64) = var_0.[int32 var_4]
        var_1.[int32 (var_5 + var_4)] <- var_6
        let (var_7: int64) = (var_4 + 1L)
        method_17((var_0: (int64 [])), (var_1: (int64 [])), (var_2: int64), (var_3: int64), (var_7: int64))
    else
        ()
and method_18((var_0: (Env0 ref))): int64 =
    let (var_1: Env0) = (!var_0)
    let (var_2: (int64 [])) = var_1.mem_ar
    let (var_3: int64) = var_1.mem_from
    let (var_4: int64) = var_1.mem_to
    let (var_5: bool) = (var_3 <> var_4)
    if (var_5 = false) then
        (failwith "Cannot dequeue past the end of the queue.")
    else
        ()
    let (var_6: int64) = var_2.LongLength
    let (var_7: int64) = (var_3 + 1L)
    let (var_8: int64) =
        if (var_7 = var_6) then
            0L
        else
            var_7
    var_0 := (Env0(var_2, var_8, var_4))
    var_2.[int32 var_3]
let (var_0: (int64 [])) = Array.zeroCreate<int64> (System.Convert.ToInt32(1L))
let (var_1: (Env0 ref)) = (ref (Env0(var_0, 0L, 0L)))
let (var_2: int64) = 1L
method_14((var_1: (Env0 ref)), (var_2: int64))
let (var_3: int64) = 2L
method_14((var_1: (Env0 ref)), (var_3: int64))
let (var_4: int64) = 3L
method_14((var_1: (Env0 ref)), (var_4: int64))
let (var_5: int64) = 4L
method_14((var_1: (Env0 ref)), (var_5: int64))
let (var_6: int64) = method_18((var_1: (Env0 ref)))
System.Console.WriteLine(var_6)
let (var_7: int64) = method_18((var_1: (Env0 ref)))
System.Console.WriteLine(var_7)
let (var_8: int64) = 1L
method_14((var_1: (Env0 ref)), (var_8: int64))
let (var_9: int64) = 2L
method_14((var_1: (Env0 ref)), (var_9: int64))
let (var_10: int64) = 3L
method_14((var_1: (Env0 ref)), (var_10: int64))
let (var_11: int64) = 4L
method_14((var_1: (Env0 ref)), (var_11: int64))
let (var_12: int64) = 1L
method_14((var_1: (Env0 ref)), (var_12: int64))
let (var_13: int64) = 2L
method_14((var_1: (Env0 ref)), (var_13: int64))
let (var_14: int64) = 3L
method_14((var_1: (Env0 ref)), (var_14: int64))
let (var_15: int64) = 4L
method_14((var_1: (Env0 ref)), (var_15: int64))
let (var_16: int64) = method_18((var_1: (Env0 ref)))
System.Console.WriteLine(var_16)
let (var_17: int64) = method_18((var_1: (Env0 ref)))
System.Console.WriteLine(var_17)
let (var_18: int64) = method_18((var_1: (Env0 ref)))
System.Console.WriteLine(var_18)
let (var_19: int64) = method_18((var_1: (Env0 ref)))
System.Console.WriteLine(var_19)
let (var_20: int64) = method_18((var_1: (Env0 ref)))
System.Console.WriteLine(var_20)
let (var_21: int64) = method_18((var_1: (Env0 ref)))
System.Console.WriteLine(var_21)
let (var_22: int64) = method_18((var_1: (Env0 ref)))
System.Console.WriteLine(var_22)
let (var_23: int64) = method_18((var_1: (Env0 ref)))
System.Console.WriteLine(var_23)
let (var_24: int64) = method_18((var_1: (Env0 ref)))
System.Console.WriteLine(var_24)
let (var_25: int64) = method_18((var_1: (Env0 ref)))
System.Console.WriteLine(var_25)

