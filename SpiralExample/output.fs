module SpiralExample
let cuda_kernels = """
extern "C" {
}
"""

type EnvHeap0 =
    {
    mem_0: (int64 [])
    }
let rec method_1((var_0: (int64 [])), (var_1: int64), (var_2: int64)): int64 =
    if (var_1 <= 1000L) then
        let (var_3: int64) = 2L
        let (var_4: int64) = method_2((var_1: int64), (var_0: (int64 [])), (var_3: int64), (var_2: int64))
        let (var_5: int64) = (var_2 + 9L)
        let (var_6: int64) = (var_1 + 1L)
        method_1((var_0: (int64 [])), (var_6: int64), (var_5: int64))
    else
        var_2
and method_5((var_0: bool), (var_1: int64), (var_2: EnvHeap0), (var_3: int64), (var_4: int64), (var_5: string), (var_6: int64)): unit =
    let (var_8: bool) =
        if (var_6 >= 0L) then
            let (var_7: int64) = (int64 var_5.Length)
            (var_6 < var_7)
        else
            false
    if var_8 then
        let (var_9: char) = var_5.[int32 var_6]
        let (var_10: bool) =
            if (var_9 >= '0') then
                (var_9 <= '9')
            else
                false
        let (var_11: int64) = (var_6 + 1L)
        if var_10 then
            let (var_12: int64) = System.Convert.ToInt64(var_9)
            let (var_13: int64) = System.Convert.ToInt64('0')
            let (var_14: int64) = (var_12 - var_13)
            method_6((var_0: bool), (var_1: int64), (var_2: EnvHeap0), (var_3: int64), (var_4: int64), (var_5: string), (var_14: int64), (var_11: int64))
        else
            (failwith "pint64")
    else
        (failwith "pint64")
and method_2((var_0: int64), (var_1: (int64 [])), (var_2: int64), (var_3: int64)): int64 =
    if (var_2 <= 10L) then
        let (var_4: int64) = 2L
        let (var_5: int64) = method_3((var_0: int64), (var_2: int64), (var_4: int64))
        var_1.[int32 var_3] <- var_5
        let (var_6: int64) = (var_3 + 1L)
        let (var_7: int64) = (var_2 + 1L)
        method_2((var_0: int64), (var_1: (int64 [])), (var_7: int64), (var_6: int64))
    else
        var_3
and method_6((var_0: bool), (var_1: int64), (var_2: EnvHeap0), (var_3: int64), (var_4: int64), (var_5: string), (var_6: int64), (var_7: int64)): unit =
    let (var_9: bool) =
        if (var_7 >= 0L) then
            let (var_8: int64) = (int64 var_5.Length)
            (var_7 < var_8)
        else
            false
    if var_9 then
        let (var_10: char) = var_5.[int32 var_7]
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
            let (var_16: bool) =
                if (var_6 = 922337203685477580L) then
                    (var_15 <= 7L)
                else
                    false
            let (var_17: bool) =
                if var_16 then
                    true
                else
                    (var_6 < 922337203685477580L)
            if var_17 then
                let (var_18: int64) = (var_6 * 10L)
                let (var_19: int64) = (var_18 + var_15)
                method_6((var_0: bool), (var_1: int64), (var_2: EnvHeap0), (var_3: int64), (var_4: int64), (var_5: string), (var_19: int64), (var_12: int64))
            else
                (failwith "integer overflow")
        else
            let (var_20: int64) =
                if var_0 then
                    var_6
                else
                    (-var_6)
            let (var_21: int64) = 0L
            method_7((var_20: int64), (var_1: int64), (var_2: EnvHeap0), (var_3: int64), (var_4: int64), (var_5: string), (var_21: int64), (var_7: int64))
    else
        let (var_22: int64) =
            if var_0 then
                var_6
            else
                (-var_6)
        let (var_23: int64) = 0L
        method_7((var_22: int64), (var_1: int64), (var_2: EnvHeap0), (var_3: int64), (var_4: int64), (var_5: string), (var_23: int64), (var_7: int64))
and method_3((var_0: int64), (var_1: int64), (var_2: int64)): int64 =
    if (var_2 <= var_1) then
        let (var_3: int64) = (var_0 * var_0)
        let (var_4: int64) = (var_2 + 1L)
        method_4((var_0: int64), (var_1: int64), (var_4: int64), (var_3: int64))
    else
        var_0
and method_7((var_0: int64), (var_1: int64), (var_2: EnvHeap0), (var_3: int64), (var_4: int64), (var_5: string), (var_6: int64), (var_7: int64)): unit =
    let (var_8: int64) = (var_6 + 1L)
    let (var_10: bool) =
        if (var_7 >= 0L) then
            let (var_9: int64) = (int64 var_5.Length)
            (var_7 < var_9)
        else
            false
    if var_10 then
        let (var_11: char) = var_5.[int32 var_7]
        let (var_13: bool) =
            if (var_11 = ' ') then
                true
            else
                if (var_11 = '\n') then
                    true
                else
                    (var_11 = '\r')
        let (var_14: int64) = (var_7 + 1L)
        if var_13 then
            method_7((var_0: int64), (var_1: int64), (var_2: EnvHeap0), (var_3: int64), (var_4: int64), (var_5: string), (var_8: int64), (var_14: int64))
        else
            let (var_16: bool) =
                if (var_7 >= 0L) then
                    let (var_15: int64) = (int64 var_5.Length)
                    (var_7 < var_15)
                else
                    false
            if var_16 then
                let (var_17: bool) = ('-' = var_11)
                if var_17 then
                    let (var_18: bool) = false
                    method_8((var_18: bool), (var_0: int64), (var_1: int64), (var_2: EnvHeap0), (var_3: int64), (var_4: int64), (var_5: string), (var_14: int64))
                else
                    let (var_19: bool) = true
                    method_8((var_19: bool), (var_0: int64), (var_1: int64), (var_2: EnvHeap0), (var_3: int64), (var_4: int64), (var_5: string), (var_7: int64))
            else
                let (var_20: bool) = true
                method_8((var_20: bool), (var_0: int64), (var_1: int64), (var_2: EnvHeap0), (var_3: int64), (var_4: int64), (var_5: string), (var_7: int64))
    else
        let (var_22: bool) =
            if (var_7 >= 0L) then
                let (var_21: int64) = (int64 var_5.Length)
                (var_7 < var_21)
            else
                false
        if var_22 then
            let (var_23: char) = var_5.[int32 var_7]
            let (var_24: bool) = ('-' = var_23)
            let (var_25: int64) = (var_7 + 1L)
            if var_24 then
                let (var_26: bool) = false
                method_8((var_26: bool), (var_0: int64), (var_1: int64), (var_2: EnvHeap0), (var_3: int64), (var_4: int64), (var_5: string), (var_25: int64))
            else
                let (var_27: bool) = true
                method_8((var_27: bool), (var_0: int64), (var_1: int64), (var_2: EnvHeap0), (var_3: int64), (var_4: int64), (var_5: string), (var_7: int64))
        else
            let (var_28: bool) = true
            method_8((var_28: bool), (var_0: int64), (var_1: int64), (var_2: EnvHeap0), (var_3: int64), (var_4: int64), (var_5: string), (var_7: int64))
and method_4((var_0: int64), (var_1: int64), (var_2: int64), (var_3: int64)): int64 =
    if (var_2 <= var_1) then
        let (var_4: int64) = (var_0 * var_3)
        let (var_5: int64) = (var_2 + 1L)
        method_4((var_0: int64), (var_1: int64), (var_5: int64), (var_4: int64))
    else
        var_3
and method_8((var_0: bool), (var_1: int64), (var_2: int64), (var_3: EnvHeap0), (var_4: int64), (var_5: int64), (var_6: string), (var_7: int64)): unit =
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
            method_9((var_0: bool), (var_1: int64), (var_2: int64), (var_3: EnvHeap0), (var_4: int64), (var_5: int64), (var_6: string), (var_15: int64), (var_12: int64))
        else
            (failwith "pint64")
    else
        (failwith "pint64")
and method_9((var_0: bool), (var_1: int64), (var_2: int64), (var_3: EnvHeap0), (var_4: int64), (var_5: int64), (var_6: string), (var_7: int64), (var_8: int64)): unit =
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
                method_9((var_0: bool), (var_1: int64), (var_2: int64), (var_3: EnvHeap0), (var_4: int64), (var_5: int64), (var_6: string), (var_20: int64), (var_13: int64))
            else
                (failwith "integer overflow")
        else
            let (var_21: int64) =
                if var_0 then
                    var_7
                else
                    (-var_7)
            let (var_22: int64) = 0L
            method_10((var_21: int64), (var_1: int64), (var_2: int64), (var_3: EnvHeap0), (var_4: int64), (var_5: int64), (var_6: string), (var_22: int64), (var_8: int64))
    else
        let (var_23: int64) =
            if var_0 then
                var_7
            else
                (-var_7)
        let (var_24: int64) = 0L
        method_10((var_23: int64), (var_1: int64), (var_2: int64), (var_3: EnvHeap0), (var_4: int64), (var_5: int64), (var_6: string), (var_24: int64), (var_8: int64))
and method_10((var_0: int64), (var_1: int64), (var_2: int64), (var_3: EnvHeap0), (var_4: int64), (var_5: int64), (var_6: string), (var_7: int64), (var_8: int64)): unit =
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
            method_10((var_0: int64), (var_1: int64), (var_2: int64), (var_3: EnvHeap0), (var_4: int64), (var_5: int64), (var_6: string), (var_9: int64), (var_15: int64))
        else
            let (var_16: int64) = method_11((var_2: int64), (var_0: int64), (var_3: EnvHeap0), (var_4: int64), (var_5: int64), (var_1: int64))
            System.Console.WriteLine(var_16)
    else
        let (var_17: int64) = method_11((var_2: int64), (var_0: int64), (var_3: EnvHeap0), (var_4: int64), (var_5: int64), (var_1: int64))
        System.Console.WriteLine(var_17)
and method_11((var_0: int64), (var_1: int64), (var_2: EnvHeap0), (var_3: int64), (var_4: int64), (var_5: int64)): int64 =
    method_12((var_1: int64), (var_2: EnvHeap0), (var_4: int64), (var_5: int64), (var_0: int64), (var_3: int64))
and method_12((var_0: int64), (var_1: EnvHeap0), (var_2: int64), (var_3: int64), (var_4: int64), (var_5: int64)): int64 =
    if (var_4 <= var_3) then
        let (var_6: (int64 [])) = var_1.mem_0
        let (var_7: bool) =
            if (var_0 >= 2L) then
                (var_0 <= 10L)
            else
                false
        if (var_7 = false) then
            (failwith "Argument out of bounds.")
        else
            ()
        let (var_8: int64) = (var_0 - 2L)
        let (var_9: bool) =
            if (var_4 >= 1L) then
                (var_4 <= 1000L)
            else
                false
        if (var_9 = false) then
            (failwith "Argument out of bounds.")
        else
            ()
        let (var_10: int64) = (var_4 - 1L)
        let (var_11: int64) = (9L * var_10)
        let (var_12: int64) = (var_8 + var_11)
        let (var_13: int64) = var_6.[int32 var_12]
        let (var_14: int64) = (var_2 + var_13)
        if (var_14 = var_3) then
            (var_5 + 1L)
        else
            if (var_14 < var_3) then
                let (var_15: int64) = (var_4 + 1L)
                let (var_16: int64) = method_11((var_15: int64), (var_0: int64), (var_1: EnvHeap0), (var_5: int64), (var_14: int64), (var_3: int64))
                method_12((var_0: int64), (var_1: EnvHeap0), (var_2: int64), (var_3: int64), (var_15: int64), (var_16: int64))
            else
                var_5
    else
        var_5
let (var_0: string) = cuda_kernels
let (var_3: (int64 [])) = Array.zeroCreate<int64> (System.Convert.ToInt32(9000L))
let (var_4: int64) = 0L
let (var_5: int64) = 1L
let (var_6: int64) = method_1((var_3: (int64 [])), (var_5: int64), (var_4: int64))
let (var_7: EnvHeap0) = ({mem_0 = (var_3: (int64 []))} : EnvHeap0)
let (var_8: int64) = 0L
let (var_9: int64) = 0L
let (var_10: int64) = 1L
let (var_11: System.IO.Stream) = System.Console.OpenStandardInput()
let (var_12: System.IO.StreamReader) = System.IO.StreamReader(var_11)
let (var_13: string) = var_12.ReadToEnd()
let (var_14: int64) = 0L
let (var_16: bool) =
    if (var_14 >= 0L) then
        let (var_15: int64) = (int64 var_13.Length)
        (var_14 < var_15)
    else
        false
if var_16 then
    let (var_17: char) = var_13.[int32 var_14]
    let (var_18: bool) = ('-' = var_17)
    let (var_19: int64) = (var_14 + 1L)
    if var_18 then
        let (var_20: bool) = false
        method_5((var_20: bool), (var_10: int64), (var_7: EnvHeap0), (var_8: int64), (var_9: int64), (var_13: string), (var_19: int64))
    else
        let (var_21: bool) = true
        method_5((var_21: bool), (var_10: int64), (var_7: EnvHeap0), (var_8: int64), (var_9: int64), (var_13: string), (var_14: int64))
else
    let (var_22: bool) = true
    method_5((var_22: bool), (var_10: int64), (var_7: EnvHeap0), (var_8: int64), (var_9: int64), (var_13: string), (var_14: int64))

