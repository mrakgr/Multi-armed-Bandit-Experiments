type Union0 =
    | Union0Case0 of Tuple1
    | Union0Case1
and Tuple1 =
    struct
    val mem_1: int64
    new(arg_mem_1) = {mem_1 = arg_mem_1}
    end
let rec method_15((var_0: (bool [])), (var_1: int64), (var_2: int64)): unit =
    if (var_2 < var_1) then
        var_0.[int32 var_2] <- true
        let (var_3: int64) = (var_2 + 1L)
        method_15((var_0: (bool [])), (var_1: int64), (var_3: int64))
    else
        ()
and method_16((var_0: (bool [])), (var_1: int64), (var_2: int64)): unit =
    if (var_2 <= var_1) then
        let (var_3: bool) = var_0.[int32 var_2]
        if (var_3 = true) then
            let (var_4: int64) = (var_2 + var_2)
            if (var_2 = 0L) then
                (failwith "The by field should not be zero in loop as the program would diverge.")
            else
                if (var_2 < 0L) then
                    method_17((var_0: (bool [])), (var_2: int64), (var_1: int64), (var_4: int64))
                else
                    method_18((var_0: (bool [])), (var_2: int64), (var_1: int64), (var_4: int64))
        else
            ()
        let (var_5: int64) = (var_2 + 1L)
        method_16((var_0: (bool [])), (var_1: int64), (var_5: int64))
    else
        ()
and method_17((var_0: (bool [])), (var_1: int64), (var_2: int64), (var_3: int64)): unit =
    if (var_3 >= var_2) then
        var_0.[int32 var_3] <- false
        let (var_4: int64) = (var_3 + var_1)
        method_17((var_0: (bool [])), (var_1: int64), (var_2: int64), (var_4: int64))
    else
        ()
and method_18((var_0: (bool [])), (var_1: int64), (var_2: int64), (var_3: int64)): unit =
    if (var_3 <= var_2) then
        var_0.[int32 var_3] <- false
        let (var_4: int64) = (var_3 + var_1)
        method_18((var_0: (bool [])), (var_1: int64), (var_2: int64), (var_4: int64))
    else
        ()
and method_23((var_0: (bool [])), (var_1: int64), (var_2: int64)): Union0 =
    if (var_2 >= 2L) then
        let (var_3: bool) = var_0.[int32 var_2]
        let (var_5: bool) =
            if (var_3 = true) then
                let (var_4: int64) = (var_1 % var_2)
                (var_4 = 0L)
            else
                false
        if var_5 then
            Union0Case0(Tuple1(var_2))
        else
            let (var_6: int64) = (var_2 + -1L)
            method_23((var_0: (bool [])), (var_1: int64), (var_6: int64))
    else
        Union0Case1
let (var_0: int64) = 600851475143L
let (var_1: float) = float var_0
let (var_2: float) = System.Math.Sqrt(var_1)
let (var_3: int64) = int64 var_2
let (var_4: int64) = (var_3 + 1L)
let (var_5: bool) = (var_4 >= 0L)
if (var_5 = false) then
    (failwith "The input to init needs to be greater or equal than 0.")
else
    ()
let (var_6: (bool [])) = Array.zeroCreate<bool> (System.Convert.ToInt32(var_4))
let (var_7: int64) = 0L
method_15((var_6: (bool [])), (var_4: int64), (var_7: int64))
let (var_8: int64) = 2L
method_16((var_6: (bool [])), (var_3: int64), (var_8: int64))
let (var_9: Union0) = method_23((var_6: (bool [])), (var_0: int64), (var_3: int64))
match var_9 with
| Union0Case0(var_10) ->
    let (var_12: int64) = var_10.mem_1
    System.Console.WriteLine(var_12)
| Union0Case1 ->
    (failwith "No prime factor found!")

