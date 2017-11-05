module SpiralExample
let cuda_kernels = """
extern "C" {
}
"""

type Tuple0 =
    struct
    val mem_0: int64
    val mem_1: int64
    new(arg_mem_0, arg_mem_1) = {mem_0 = arg_mem_0; mem_1 = arg_mem_1}
    end
let rec method_0((var_0: bool), (var_1: int64), (var_2: string), (var_3: int64)): unit =
    let (var_5: bool) =
        if (var_3 >= 0L) then
            let (var_4: int64) = (int64 var_2.Length)
            (var_3 < var_4)
        else
            false
    if var_5 then
        let (var_6: char) = var_2.[int32 var_3]
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
            method_1((var_0: bool), (var_1: int64), (var_2: string), (var_11: int64), (var_8: int64))
        else
            (failwith "pint64")
    else
        (failwith "pint64")
and method_1((var_0: bool), (var_1: int64), (var_2: string), (var_3: int64), (var_4: int64)): unit =
    let (var_6: bool) =
        if (var_4 >= 0L) then
            let (var_5: int64) = (int64 var_2.Length)
            (var_4 < var_5)
        else
            false
    if var_6 then
        let (var_7: char) = var_2.[int32 var_4]
        let (var_8: bool) =
            if (var_7 >= '0') then
                (var_7 <= '9')
            else
                false
        let (var_9: int64) = (var_4 + 1L)
        if var_8 then
            let (var_10: int64) = System.Convert.ToInt64(var_7)
            let (var_11: int64) = System.Convert.ToInt64('0')
            let (var_12: int64) = (var_10 - var_11)
            let (var_13: bool) =
                if (var_3 = 922337203685477580L) then
                    (var_12 <= 7L)
                else
                    false
            let (var_14: bool) =
                if var_13 then
                    true
                else
                    (var_3 < 922337203685477580L)
            if var_14 then
                let (var_15: int64) = (var_3 * 10L)
                let (var_16: int64) = (var_15 + var_12)
                method_1((var_0: bool), (var_1: int64), (var_2: string), (var_16: int64), (var_9: int64))
            else
                (failwith "integer overflow")
        else
            let (var_17: int64) =
                if var_0 then
                    var_3
                else
                    (-var_3)
            let (var_18: int64) = 0L
            method_2((var_17: int64), (var_1: int64), (var_2: string), (var_18: int64), (var_4: int64))
    else
        let (var_19: int64) =
            if var_0 then
                var_3
            else
                (-var_3)
        let (var_20: int64) = 0L
        method_2((var_19: int64), (var_1: int64), (var_2: string), (var_20: int64), (var_4: int64))
and method_2((var_0: int64), (var_1: int64), (var_2: string), (var_3: int64), (var_4: int64)): unit =
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
            method_2((var_0: int64), (var_1: int64), (var_2: string), (var_5: int64), (var_11: int64))
        else
            let (var_12: bool) = (var_0 >= 0L)
            if var_12 then
                let (var_13: (int64 [])) = Array.zeroCreate<int64> (System.Convert.ToInt32(var_0))
                let (var_14: int64) = 0L
                method_3((var_13: (int64 [])), (var_1: int64), (var_2: string), (var_14: int64), (var_0: int64), (var_4: int64))
            else
                (failwith "n in parse array must be >= 0")
    else
        let (var_15: bool) = (var_0 >= 0L)
        if var_15 then
            let (var_16: (int64 [])) = Array.zeroCreate<int64> (System.Convert.ToInt32(var_0))
            let (var_17: int64) = 0L
            method_3((var_16: (int64 [])), (var_1: int64), (var_2: string), (var_17: int64), (var_0: int64), (var_4: int64))
        else
            (failwith "n in parse array must be >= 0")
and method_3((var_0: (int64 [])), (var_1: int64), (var_2: string), (var_3: int64), (var_4: int64), (var_5: int64)): unit =
    let (var_6: bool) = (var_3 < var_4)
    if var_6 then
        let (var_7: int64) = (var_3 + 1L)
        let (var_9: bool) =
            if (var_5 >= 0L) then
                let (var_8: int64) = (int64 var_2.Length)
                (var_5 < var_8)
            else
                false
        if var_9 then
            let (var_10: char) = var_2.[int32 var_5]
            let (var_11: bool) = ('-' = var_10)
            let (var_12: int64) = (var_5 + 1L)
            if var_11 then
                let (var_13: bool) = false
                method_4((var_13: bool), (var_0: (int64 [])), (var_3: int64), (var_7: int64), (var_4: int64), (var_1: int64), (var_2: string), (var_12: int64))
            else
                let (var_14: bool) = true
                method_4((var_14: bool), (var_0: (int64 [])), (var_3: int64), (var_7: int64), (var_4: int64), (var_1: int64), (var_2: string), (var_5: int64))
        else
            let (var_15: bool) = true
            method_4((var_15: bool), (var_0: (int64 [])), (var_3: int64), (var_7: int64), (var_4: int64), (var_1: int64), (var_2: string), (var_5: int64))
    else
        let (var_16: int64) = 0L
        let (var_17: int64) = var_0.LongLength
        let (var_18: int64) = 0L
        let (var_19: Tuple0) = method_7((var_0: (int64 [])), (var_17: int64), (var_18: int64), (var_16: int64), (var_1: int64))
        let (var_20: int64) = var_19.mem_0
        let (var_21: int64) = var_19.mem_1
        System.Console.WriteLine(var_20)
and method_4((var_0: bool), (var_1: (int64 [])), (var_2: int64), (var_3: int64), (var_4: int64), (var_5: int64), (var_6: string), (var_7: int64)): unit =
    let (var_9: bool) =
        if (var_7 >= 0L) then
            let (var_8: int64) = (int64 var_6.Length)
            (var_7 < var_8)
        else
            false
    if var_9 then
        let (var_10: char) = var_6.[int32 var_7]
        let (var_11: bool) =
            if (var_10 >= '0') then
                (var_10 <= '9')
            else
                false
        let (var_12: int64) = (var_7 + 1L)
        if var_11 then
            let (var_13: int64) = System.Convert.ToInt64(var_10)
            let (var_14: int64) = System.Convert.ToInt64('0')
            let (var_15: int64) = (var_13 - var_14)
            method_5((var_0: bool), (var_1: (int64 [])), (var_2: int64), (var_3: int64), (var_4: int64), (var_5: int64), (var_6: string), (var_15: int64), (var_12: int64))
        else
            (failwith "pint64")
    else
        (failwith "pint64")
and method_7((var_0: (int64 [])), (var_1: int64), (var_2: int64), (var_3: int64), (var_4: int64)): Tuple0 =
    if (var_2 < var_1) then
        let (var_5: int64) = var_0.[int32 var_2]
        let (var_8: Tuple0) =
            if (var_5 > var_4) then
                Tuple0(1L, var_5)
            else
                if (var_5 = var_4) then
                    let (var_6: int64) = (var_3 + 1L)
                    Tuple0(var_6, var_4)
                else
                    Tuple0(var_3, var_4)
        let (var_9: int64) = var_8.mem_0
        let (var_10: int64) = var_8.mem_1
        let (var_11: int64) = (var_2 + 1L)
        method_7((var_0: (int64 [])), (var_1: int64), (var_11: int64), (var_9: int64), (var_10: int64))
    else
        Tuple0(var_3, var_4)
and method_5((var_0: bool), (var_1: (int64 [])), (var_2: int64), (var_3: int64), (var_4: int64), (var_5: int64), (var_6: string), (var_7: int64), (var_8: int64)): unit =
    let (var_10: bool) =
        if (var_8 >= 0L) then
            let (var_9: int64) = (int64 var_6.Length)
            (var_8 < var_9)
        else
            false
    if var_10 then
        let (var_11: char) = var_6.[int32 var_8]
        let (var_12: bool) =
            if (var_11 >= '0') then
                (var_11 <= '9')
            else
                false
        let (var_13: int64) = (var_8 + 1L)
        if var_12 then
            let (var_14: int64) = System.Convert.ToInt64(var_11)
            let (var_15: int64) = System.Convert.ToInt64('0')
            let (var_16: int64) = (var_14 - var_15)
            let (var_17: bool) =
                if (var_7 = 922337203685477580L) then
                    (var_16 <= 7L)
                else
                    false
            let (var_18: bool) =
                if var_17 then
                    true
                else
                    (var_7 < 922337203685477580L)
            if var_18 then
                let (var_19: int64) = (var_7 * 10L)
                let (var_20: int64) = (var_19 + var_16)
                method_5((var_0: bool), (var_1: (int64 [])), (var_2: int64), (var_3: int64), (var_4: int64), (var_5: int64), (var_6: string), (var_20: int64), (var_13: int64))
            else
                (failwith "integer overflow")
        else
            let (var_21: int64) =
                if var_0 then
                    var_7
                else
                    (-var_7)
            let (var_22: int64) = 0L
            method_6((var_21: int64), (var_1: (int64 [])), (var_2: int64), (var_3: int64), (var_4: int64), (var_5: int64), (var_6: string), (var_22: int64), (var_8: int64))
    else
        let (var_23: int64) =
            if var_0 then
                var_7
            else
                (-var_7)
        let (var_24: int64) = 0L
        method_6((var_23: int64), (var_1: (int64 [])), (var_2: int64), (var_3: int64), (var_4: int64), (var_5: int64), (var_6: string), (var_24: int64), (var_8: int64))
and method_6((var_0: int64), (var_1: (int64 [])), (var_2: int64), (var_3: int64), (var_4: int64), (var_5: int64), (var_6: string), (var_7: int64), (var_8: int64)): unit =
    let (var_9: int64) = (var_7 + 1L)
    let (var_11: bool) =
        if (var_8 >= 0L) then
            let (var_10: int64) = (int64 var_6.Length)
            (var_8 < var_10)
        else
            false
    if var_11 then
        let (var_12: char) = var_6.[int32 var_8]
        let (var_14: bool) =
            if (var_12 = ' ') then
                true
            else
                if (var_12 = '\n') then
                    true
                else
                    (var_12 = '\r')
        let (var_15: int64) = (var_8 + 1L)
        if var_14 then
            method_6((var_0: int64), (var_1: (int64 [])), (var_2: int64), (var_3: int64), (var_4: int64), (var_5: int64), (var_6: string), (var_9: int64), (var_15: int64))
        else
            var_1.[int32 var_2] <- var_0
            method_3((var_1: (int64 [])), (var_5: int64), (var_6: string), (var_3: int64), (var_4: int64), (var_8: int64))
    else
        var_1.[int32 var_2] <- var_0
        method_3((var_1: (int64 [])), (var_5: int64), (var_6: string), (var_3: int64), (var_4: int64), (var_8: int64))
let (var_0: int64) = System.Int64.MinValue
let (var_1: System.IO.Stream) = System.Console.OpenStandardInput()
let (var_2: System.IO.StreamReader) = System.IO.StreamReader(var_1)
let (var_3: string) = var_2.ReadToEnd()
let (var_4: int64) = 0L
let (var_6: bool) =
    if (var_4 >= 0L) then
        let (var_5: int64) = (int64 var_3.Length)
        (var_4 < var_5)
    else
        false
if var_6 then
    let (var_7: char) = var_3.[int32 var_4]
    let (var_8: bool) = ('-' = var_7)
    let (var_9: int64) = (var_4 + 1L)
    if var_8 then
        let (var_10: bool) = false
        method_0((var_10: bool), (var_0: int64), (var_3: string), (var_9: int64))
    else
        let (var_11: bool) = true
        method_0((var_11: bool), (var_0: int64), (var_3: string), (var_4: int64))
else
    let (var_12: bool) = true
    method_0((var_12: bool), (var_0: int64), (var_3: string), (var_4: int64))

