type Env0 =
    struct
    val mem_pos: int64
    new(arg_mem_pos) = {mem_pos = arg_mem_pos}
    end
let rec method_14 ((var_2: string)) ((var_0: int64), (var_1: Env0)): unit =
    let (var_3: int64) = var_1.mem_pos
    let (var_6: (int64 * Env0 -> unit)) = method_15((var_0: int64), (var_2: string))
    method_24((var_6: (int64 * Env0 -> unit)), (var_2: string), (var_3: int64))
and method_15 ((var_2: int64), (var_3: string)) ((var_0: int64), (var_1: Env0)): unit =
    let (var_4: int64) = var_1.mem_pos
    let (var_7: (int64 * Env0 -> unit)) = method_16((var_0: int64), (var_2: int64), (var_3: string))
    method_24((var_7: (int64 * Env0 -> unit)), (var_3: string), (var_4: int64))
and method_16 ((var_2: int64), (var_3: int64), (var_4: string)) ((var_0: int64), (var_1: Env0)): unit =
    let (var_5: int64) = var_1.mem_pos
    let (var_8: (int64 * Env0 -> unit)) = method_17((var_0: int64), (var_2: int64), (var_3: int64), (var_4: string))
    method_24((var_8: (int64 * Env0 -> unit)), (var_4: string), (var_5: int64))
and method_17 ((var_2: int64), (var_3: int64), (var_4: int64), (var_5: string)) ((var_0: int64), (var_1: Env0)): unit =
    let (var_6: int64) = var_1.mem_pos
    let (var_9: (int64 * Env0 -> unit)) = method_18((var_0: int64), (var_2: int64), (var_3: int64), (var_4: int64), (var_5: string))
    method_24((var_9: (int64 * Env0 -> unit)), (var_5: string), (var_6: int64))
and method_18 ((var_2: int64), (var_3: int64), (var_4: int64), (var_5: int64), (var_6: string)) ((var_0: int64), (var_1: Env0)): unit =
    let (var_7: int64) = var_1.mem_pos
    let (var_10: (int64 * Env0 -> unit)) = method_19((var_0: int64), (var_2: int64), (var_3: int64), (var_4: int64), (var_5: int64), (var_6: string))
    method_24((var_10: (int64 * Env0 -> unit)), (var_6: string), (var_7: int64))
and method_19 ((var_2: int64), (var_3: int64), (var_4: int64), (var_5: int64), (var_6: int64), (var_7: string)) ((var_0: int64), (var_1: Env0)): unit =
    let (var_8: int64) = var_1.mem_pos
    let (var_11: (int64 * Env0 -> unit)) = method_20((var_0: int64), (var_2: int64), (var_3: int64), (var_4: int64), (var_5: int64), (var_6: int64), (var_7: string))
    method_24((var_11: (int64 * Env0 -> unit)), (var_7: string), (var_8: int64))
and method_20 ((var_2: int64), (var_3: int64), (var_4: int64), (var_5: int64), (var_6: int64), (var_7: int64), (var_8: string)) ((var_0: int64), (var_1: Env0)): unit =
    let (var_9: int64) = var_1.mem_pos
    let (var_12: (int64 * Env0 -> unit)) = method_21((var_0: int64), (var_2: int64), (var_3: int64), (var_4: int64), (var_5: int64), (var_6: int64), (var_7: int64), (var_8: string))
    method_24((var_12: (int64 * Env0 -> unit)), (var_8: string), (var_9: int64))
and method_21 ((var_2: int64), (var_3: int64), (var_4: int64), (var_5: int64), (var_6: int64), (var_7: int64), (var_8: int64), (var_9: string)) ((var_0: int64), (var_1: Env0)): unit =
    let (var_10: int64) = var_1.mem_pos
    let (var_13: (int64 * Env0 -> unit)) = method_22((var_0: int64), (var_2: int64), (var_3: int64), (var_4: int64), (var_5: int64), (var_6: int64), (var_7: int64), (var_8: int64), (var_9: string))
    method_24((var_13: (int64 * Env0 -> unit)), (var_9: string), (var_10: int64))
and method_22 ((var_2: int64), (var_3: int64), (var_4: int64), (var_5: int64), (var_6: int64), (var_7: int64), (var_8: int64), (var_9: int64), (var_10: string)) ((var_0: int64), (var_1: Env0)): unit =
    let (var_11: int64) = var_1.mem_pos
    let (var_14: (int64 * Env0 -> unit)) = method_23((var_0: int64), (var_2: int64), (var_3: int64), (var_4: int64), (var_5: int64), (var_6: int64), (var_7: int64), (var_8: int64), (var_9: int64), (var_10: string))
    method_24((var_14: (int64 * Env0 -> unit)), (var_10: string), (var_11: int64))
and method_23 ((var_2: int64), (var_3: int64), (var_4: int64), (var_5: int64), (var_6: int64), (var_7: int64), (var_8: int64), (var_9: int64), (var_10: int64), (var_11: string)) ((var_0: int64), (var_1: Env0)): unit =
    let (var_12: int64) = var_1.mem_pos
    let (var_13: int64) = (0L + var_10)
    let (var_14: int64) = (var_13 + var_9)
    let (var_15: int64) = (var_14 + var_8)
    let (var_16: int64) = (var_15 + var_7)
    let (var_17: int64) = (var_16 + var_6)
    let (var_18: int64) = (var_17 + var_5)
    let (var_19: int64) = (var_18 + var_4)
    let (var_20: int64) = (var_19 + var_3)
    let (var_21: int64) = (var_20 + var_2)
    let (var_22: int64) = (var_21 + var_0)
    System.Console.WriteLine(var_22)
and method_24((var_0: (int64 * Env0 -> unit)), (var_1: string), (var_2: int64)): unit =
    let (var_4: bool) =
        if (var_2 >= 0L) then
            let (var_3: int64) = (int64 var_1.Length)
            (var_2 < var_3)
        else
            false
    if var_4 then
        let (var_5: char) = var_1.[int32 var_2]
        let (var_6: bool) =
            if (var_5 >= '0') then
                (var_5 <= '9')
            else
                false
        let (var_7: int64) = (var_2 + 1L)
        if var_6 then
            let (var_8: int64) = System.Convert.ToInt64(var_5)
            let (var_9: int64) = System.Convert.ToInt64('0')
            let (var_10: int64) = (var_8 - var_9)
            let (var_11: int64) = (0L + var_10)
            method_25((var_0: (int64 * Env0 -> unit)), (var_1: string), (var_11: int64), (var_7: int64))
        else
            (failwith "pint64")
    else
        (failwith "pint64")
and method_25((var_0: (int64 * Env0 -> unit)), (var_1: string), (var_2: int64), (var_3: int64)): unit =
    let (var_5: bool) =
        if (var_3 >= 0L) then
            let (var_4: int64) = (int64 var_1.Length)
            (var_3 < var_4)
        else
            false
    if var_5 then
        let (var_6: char) = var_1.[int32 var_3]
        let (var_7: bool) =
            if (var_6 >= '0') then
                (var_6 <= '9')
            else
                false
        let (var_8: int64) = (var_3 + 1L)
        if var_7 then
            let (var_9: int64) = System.Convert.ToInt64(var_6)
            let (var_10: int64) = System.Convert.ToInt64('0')
            let (var_11: int64) = (var_9 - var_10)
            let (var_12: bool) =
                if (var_2 = 922337203685477580L) then
                    (var_11 <= 7L)
                else
                    false
            let (var_13: bool) =
                if var_12 then
                    true
                else
                    (var_2 < 922337203685477580L)
            if var_13 then
                let (var_14: int64) = (var_2 * 10L)
                let (var_15: int64) = (var_14 + var_11)
                method_25((var_0: (int64 * Env0 -> unit)), (var_1: string), (var_15: int64), (var_8: int64))
            else
                (failwith "integer overflow")
        else
            let (var_16: int64) = 0L
            method_26((var_2: int64), (var_0: (int64 * Env0 -> unit)), (var_1: string), (var_16: int64), (var_8: int64))
    else
        let (var_17: int64) = 0L
        method_26((var_2: int64), (var_0: (int64 * Env0 -> unit)), (var_1: string), (var_17: int64), (var_3: int64))
and method_26((var_0: int64), (var_1: (int64 * Env0 -> unit)), (var_2: string), (var_3: int64), (var_4: int64)): unit =
    let (var_5: int64) = (var_3 + 1L)
    let (var_7: bool) =
        if (var_4 >= 0L) then
            let (var_6: int64) = (int64 var_2.Length)
            (var_4 < var_6)
        else
            false
    if var_7 then
        let (var_8: char) = var_2.[int32 var_4]
        let (var_10: bool) =
            if (var_8 = ' ') then
                true
            else
                if (var_8 = '\n') then
                    true
                else
                    (var_8 = '\r')
        let (var_11: int64) = (var_4 + 1L)
        if var_10 then
            method_26((var_0: int64), (var_1: (int64 * Env0 -> unit)), (var_2: string), (var_5: int64), (var_11: int64))
        else
            var_1(var_0, Env0(var_11))
    else
        var_1(var_0, Env0(var_4))
let (var_0: System.IO.Stream) = System.Console.OpenStandardInput()
let (var_1: System.IO.StreamReader) = System.IO.StreamReader(var_0)
let (var_2: string) = var_1.ReadToEnd()
let (var_3: int64) = 0L
let (var_6: (int64 * Env0 -> unit)) = method_14((var_2: string))
method_24((var_6: (int64 * Env0 -> unit)), (var_2: string), (var_3: int64))

