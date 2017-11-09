module SpiralExample
let cuda_kernels = """
extern "C" {
}
"""

type Union0 =
    | Union0Case0 of Tuple2
    | Union0Case1
and EnvHeap1 =
    {
    mem_0: (Union0 [])
    }
and Tuple2 =
    struct
    val mem_0: int64
    new(arg_mem_0) = {mem_0 = arg_mem_0}
    end
let rec method_0((var_0: (Union0 [])), (var_1: int64), (var_2: int64)): int64 =
    if (var_1 <= 14L) then
        let (var_3: int64) = 0L
        let (var_4: int64) = method_1((var_1: int64), (var_0: (Union0 [])), (var_3: int64), (var_2: int64))
        let (var_5: int64) = (var_2 + 30L)
        let (var_6: int64) = (var_1 + 1L)
        method_0((var_0: (Union0 [])), (var_6: int64), (var_5: int64))
    else
        var_2
and method_3((var_0: bool), (var_1: EnvHeap1), (var_2: string), (var_3: int64)): unit =
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
            method_4((var_0: bool), (var_1: EnvHeap1), (var_2: string), (var_11: int64), (var_8: int64))
        else
            (failwith "pint64")
    else
        (failwith "pint64")
and method_1((var_0: int64), (var_1: (Union0 [])), (var_2: int64), (var_3: int64)): int64 =
    if (var_2 <= 14L) then
        let (var_4: int64) = 0L
        let (var_5: int64) = method_2((var_2: int64), (var_0: int64), (var_1: (Union0 [])), (var_4: int64), (var_3: int64))
        let (var_6: int64) = (var_3 + 2L)
        let (var_7: int64) = (var_2 + 1L)
        method_1((var_0: int64), (var_1: (Union0 [])), (var_7: int64), (var_6: int64))
    else
        var_3
and method_4((var_0: bool), (var_1: EnvHeap1), (var_2: string), (var_3: int64), (var_4: int64)): unit =
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
                method_4((var_0: bool), (var_1: EnvHeap1), (var_2: string), (var_16: int64), (var_9: int64))
            else
                (failwith "integer overflow")
        else
            let (var_17: int64) =
                if var_0 then
                    var_3
                else
                    (-var_3)
            let (var_18: int64) = 0L
            method_5((var_17: int64), (var_1: EnvHeap1), (var_2: string), (var_18: int64), (var_4: int64))
    else
        let (var_19: int64) =
            if var_0 then
                var_3
            else
                (-var_3)
        let (var_20: int64) = 0L
        method_5((var_19: int64), (var_1: EnvHeap1), (var_2: string), (var_20: int64), (var_4: int64))
and method_2((var_0: int64), (var_1: int64), (var_2: (Union0 [])), (var_3: int64), (var_4: int64)): int64 =
    if (var_3 <= 1L) then
        var_2.[int32 var_4] <- Union0Case1
        let (var_5: int64) = (var_4 + 1L)
        let (var_6: int64) = (var_3 + 1L)
        method_2((var_0: int64), (var_1: int64), (var_2: (Union0 [])), (var_6: int64), (var_5: int64))
    else
        var_4
and method_5((var_0: int64), (var_1: EnvHeap1), (var_2: string), (var_3: int64), (var_4: int64)): unit =
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
            method_5((var_0: int64), (var_1: EnvHeap1), (var_2: string), (var_5: int64), (var_11: int64))
        else
            let (var_12: int64) = 0L
            method_6((var_2: string), (var_12: int64), (var_0: int64), (var_1: EnvHeap1), (var_4: int64))
    else
        let (var_13: int64) = 0L
        method_6((var_2: string), (var_13: int64), (var_0: int64), (var_1: EnvHeap1), (var_4: int64))
and method_6((var_0: string), (var_1: int64), (var_2: int64), (var_3: EnvHeap1), (var_4: int64)): unit =
    let (var_5: bool) = (var_1 < var_2)
    if var_5 then
        let (var_6: int64) = (var_1 + 1L)
        let (var_8: bool) =
            if (var_4 >= 0L) then
                let (var_7: int64) = (int64 var_0.Length)
                (var_4 < var_7)
            else
                false
        if var_8 then
            let (var_9: char) = var_0.[int32 var_4]
            let (var_10: bool) = ('-' = var_9)
            let (var_11: int64) = (var_4 + 1L)
            if var_10 then
                let (var_12: bool) = false
                method_7((var_12: bool), (var_3: EnvHeap1), (var_6: int64), (var_2: int64), (var_0: string), (var_11: int64))
            else
                let (var_13: bool) = true
                method_7((var_13: bool), (var_3: EnvHeap1), (var_6: int64), (var_2: int64), (var_0: string), (var_4: int64))
        else
            let (var_14: bool) = true
            method_7((var_14: bool), (var_3: EnvHeap1), (var_6: int64), (var_2: int64), (var_0: string), (var_4: int64))
    else
        ()
and method_7((var_0: bool), (var_1: EnvHeap1), (var_2: int64), (var_3: int64), (var_4: string), (var_5: int64)): unit =
    let (var_7: bool) =
        if (var_5 >= 0L) then
            let (var_6: int64) = (int64 var_4.Length)
            (var_5 < var_6)
        else
            false
    if var_7 then
        let (var_8: char) = var_4.[int32 var_5]
        let (var_9: bool) =
            if (var_8 >= '0') then
                (var_8 <= '9')
            else
                false
        let (var_10: int64) = (var_5 + 1L)
        if var_9 then
            let (var_11: int64) = System.Convert.ToInt64(var_8)
            let (var_12: int64) = System.Convert.ToInt64('0')
            let (var_13: int64) = (var_11 - var_12)
            method_8((var_0: bool), (var_1: EnvHeap1), (var_2: int64), (var_3: int64), (var_4: string), (var_13: int64), (var_10: int64))
        else
            (failwith "pint64")
    else
        (failwith "pint64")
and method_8((var_0: bool), (var_1: EnvHeap1), (var_2: int64), (var_3: int64), (var_4: string), (var_5: int64), (var_6: int64)): unit =
    let (var_8: bool) =
        if (var_6 >= 0L) then
            let (var_7: int64) = (int64 var_4.Length)
            (var_6 < var_7)
        else
            false
    if var_8 then
        let (var_9: char) = var_4.[int32 var_6]
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
            let (var_15: bool) =
                if (var_5 = 922337203685477580L) then
                    (var_14 <= 7L)
                else
                    false
            let (var_16: bool) =
                if var_15 then
                    true
                else
                    (var_5 < 922337203685477580L)
            if var_16 then
                let (var_17: int64) = (var_5 * 10L)
                let (var_18: int64) = (var_17 + var_14)
                method_8((var_0: bool), (var_1: EnvHeap1), (var_2: int64), (var_3: int64), (var_4: string), (var_18: int64), (var_11: int64))
            else
                (failwith "integer overflow")
        else
            let (var_19: int64) =
                if var_0 then
                    var_5
                else
                    (-var_5)
            let (var_20: int64) = 0L
            method_9((var_19: int64), (var_1: EnvHeap1), (var_2: int64), (var_3: int64), (var_4: string), (var_20: int64), (var_6: int64))
    else
        let (var_21: int64) =
            if var_0 then
                var_5
            else
                (-var_5)
        let (var_22: int64) = 0L
        method_9((var_21: int64), (var_1: EnvHeap1), (var_2: int64), (var_3: int64), (var_4: string), (var_22: int64), (var_6: int64))
and method_9((var_0: int64), (var_1: EnvHeap1), (var_2: int64), (var_3: int64), (var_4: string), (var_5: int64), (var_6: int64)): unit =
    let (var_7: int64) = (var_5 + 1L)
    let (var_9: bool) =
        if (var_6 >= 0L) then
            let (var_8: int64) = (int64 var_4.Length)
            (var_6 < var_8)
        else
            false
    if var_9 then
        let (var_10: char) = var_4.[int32 var_6]
        let (var_12: bool) =
            if (var_10 = ' ') then
                true
            else
                if (var_10 = '\n') then
                    true
                else
                    (var_10 = '\r')
        let (var_13: int64) = (var_6 + 1L)
        if var_12 then
            method_9((var_0: int64), (var_1: EnvHeap1), (var_2: int64), (var_3: int64), (var_4: string), (var_7: int64), (var_13: int64))
        else
            let (var_15: bool) =
                if (var_6 >= 0L) then
                    let (var_14: int64) = (int64 var_4.Length)
                    (var_6 < var_14)
                else
                    false
            if var_15 then
                let (var_16: bool) = ('-' = var_10)
                if var_16 then
                    let (var_17: bool) = false
                    method_10((var_17: bool), (var_0: int64), (var_1: EnvHeap1), (var_2: int64), (var_3: int64), (var_4: string), (var_13: int64))
                else
                    let (var_18: bool) = true
                    method_10((var_18: bool), (var_0: int64), (var_1: EnvHeap1), (var_2: int64), (var_3: int64), (var_4: string), (var_6: int64))
            else
                let (var_19: bool) = true
                method_10((var_19: bool), (var_0: int64), (var_1: EnvHeap1), (var_2: int64), (var_3: int64), (var_4: string), (var_6: int64))
    else
        let (var_21: bool) =
            if (var_6 >= 0L) then
                let (var_20: int64) = (int64 var_4.Length)
                (var_6 < var_20)
            else
                false
        if var_21 then
            let (var_22: char) = var_4.[int32 var_6]
            let (var_23: bool) = ('-' = var_22)
            let (var_24: int64) = (var_6 + 1L)
            if var_23 then
                let (var_25: bool) = false
                method_10((var_25: bool), (var_0: int64), (var_1: EnvHeap1), (var_2: int64), (var_3: int64), (var_4: string), (var_24: int64))
            else
                let (var_26: bool) = true
                method_10((var_26: bool), (var_0: int64), (var_1: EnvHeap1), (var_2: int64), (var_3: int64), (var_4: string), (var_6: int64))
        else
            let (var_27: bool) = true
            method_10((var_27: bool), (var_0: int64), (var_1: EnvHeap1), (var_2: int64), (var_3: int64), (var_4: string), (var_6: int64))
and method_10((var_0: bool), (var_1: int64), (var_2: EnvHeap1), (var_3: int64), (var_4: int64), (var_5: string), (var_6: int64)): unit =
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
            method_11((var_0: bool), (var_1: int64), (var_2: EnvHeap1), (var_3: int64), (var_4: int64), (var_5: string), (var_14: int64), (var_11: int64))
        else
            (failwith "pint64")
    else
        (failwith "pint64")
and method_11((var_0: bool), (var_1: int64), (var_2: EnvHeap1), (var_3: int64), (var_4: int64), (var_5: string), (var_6: int64), (var_7: int64)): unit =
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
                method_11((var_0: bool), (var_1: int64), (var_2: EnvHeap1), (var_3: int64), (var_4: int64), (var_5: string), (var_19: int64), (var_12: int64))
            else
                (failwith "integer overflow")
        else
            let (var_20: int64) =
                if var_0 then
                    var_6
                else
                    (-var_6)
            let (var_21: int64) = 0L
            method_12((var_20: int64), (var_1: int64), (var_2: EnvHeap1), (var_3: int64), (var_4: int64), (var_5: string), (var_21: int64), (var_7: int64))
    else
        let (var_22: int64) =
            if var_0 then
                var_6
            else
                (-var_6)
        let (var_23: int64) = 0L
        method_12((var_22: int64), (var_1: int64), (var_2: EnvHeap1), (var_3: int64), (var_4: int64), (var_5: string), (var_23: int64), (var_7: int64))
and method_12((var_0: int64), (var_1: int64), (var_2: EnvHeap1), (var_3: int64), (var_4: int64), (var_5: string), (var_6: int64), (var_7: int64)): unit =
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
            method_12((var_0: int64), (var_1: int64), (var_2: EnvHeap1), (var_3: int64), (var_4: int64), (var_5: string), (var_8: int64), (var_14: int64))
        else
            let (var_15: int64) = 0L
            let (var_16: int64) = 1L
            let (var_17: int64) = method_13((var_2: EnvHeap1), (var_1: int64), (var_0: int64), (var_15: int64), (var_16: int64))
            if (var_17 = 0L) then
                System.Console.WriteLine("First")
            else
                System.Console.WriteLine("Second")
            method_6((var_5: string), (var_3: int64), (var_4: int64), (var_2: EnvHeap1), (var_7: int64))
    else
        let (var_18: int64) = 0L
        let (var_19: int64) = 1L
        let (var_20: int64) = method_13((var_2: EnvHeap1), (var_1: int64), (var_0: int64), (var_18: int64), (var_19: int64))
        if (var_20 = 0L) then
            System.Console.WriteLine("First")
        else
            System.Console.WriteLine("Second")
        method_6((var_5: string), (var_3: int64), (var_4: int64), (var_2: EnvHeap1), (var_7: int64))
and method_13((var_0: EnvHeap1), (var_1: int64), (var_2: int64), (var_3: int64), (var_4: int64)): int64 =
    let (var_5: int64) = (var_1 - 2L)
    let (var_6: int64) = (var_2 + 1L)
    let (var_7: int64) = (var_2 - 1L)
    let (var_8: int64) = (var_1 + 1L)
    let (var_9: int64) = (var_2 - 2L)
    let (var_10: int64) = (var_1 - 1L)
    let (var_11: (Union0 [])) = var_0.mem_0
    let (var_12: bool) =
        if (var_3 >= 0L) then
            (var_3 <= 1L)
        else
            false
    if var_12 then
        ()
    else
        (failwith "Argument out of bounds.")
    let (var_13: bool) =
        if (var_7 >= 0L) then
            (var_7 <= 14L)
        else
            false
    if var_13 then
        ()
    else
        (failwith "Argument out of bounds.")
    let (var_14: int64) = (2L * var_7)
    let (var_15: int64) = (var_3 + var_14)
    let (var_16: bool) =
        if (var_10 >= 0L) then
            (var_10 <= 14L)
        else
            false
    if var_16 then
        ()
    else
        (failwith "Argument out of bounds.")
    let (var_17: int64) = (30L * var_10)
    let (var_18: int64) = (var_15 + var_17)
    let (var_19: Union0) = var_11.[int32 var_18]
    match var_19 with
    | Union0Case0(var_20) ->
        var_20.mem_0
    | Union0Case1 ->
        let (var_22: bool) =
            if (var_5 >= 1L) then
                (var_5 <= 15L)
            else
                false
        let (var_26: bool) =
            if var_22 then
                let (var_23: bool) =
                    if (var_6 >= 1L) then
                        (var_6 <= 15L)
                    else
                        false
                if var_23 then
                    let (var_24: int64) = method_13((var_0: EnvHeap1), (var_5: int64), (var_6: int64), (var_4: int64), (var_3: int64))
                    (var_24 = var_3)
                else
                    false
            else
                false
        let (var_45: int64) =
            if var_26 then
                var_3
            else
                let (var_27: bool) =
                    if (var_5 >= 1L) then
                        (var_5 <= 15L)
                    else
                        false
                let (var_31: bool) =
                    if var_27 then
                        let (var_28: bool) =
                            if (var_7 >= 1L) then
                                (var_7 <= 15L)
                            else
                                false
                        if var_28 then
                            let (var_29: int64) = method_13((var_0: EnvHeap1), (var_5: int64), (var_7: int64), (var_4: int64), (var_3: int64))
                            (var_29 = var_3)
                        else
                            false
                    else
                        false
                if var_31 then
                    var_3
                else
                    let (var_32: bool) =
                        if (var_8 >= 1L) then
                            (var_8 <= 15L)
                        else
                            false
                    let (var_36: bool) =
                        if var_32 then
                            let (var_33: bool) =
                                if (var_9 >= 1L) then
                                    (var_9 <= 15L)
                                else
                                    false
                            if var_33 then
                                let (var_34: int64) = method_13((var_0: EnvHeap1), (var_8: int64), (var_9: int64), (var_4: int64), (var_3: int64))
                                (var_34 = var_3)
                            else
                                false
                        else
                            false
                    if var_36 then
                        var_3
                    else
                        let (var_37: bool) =
                            if (var_10 >= 1L) then
                                (var_10 <= 15L)
                            else
                                false
                        let (var_41: bool) =
                            if var_37 then
                                let (var_38: bool) =
                                    if (var_9 >= 1L) then
                                        (var_9 <= 15L)
                                    else
                                        false
                                if var_38 then
                                    let (var_39: int64) = method_13((var_0: EnvHeap1), (var_10: int64), (var_9: int64), (var_4: int64), (var_3: int64))
                                    (var_39 = var_3)
                                else
                                    false
                            else
                                false
                        if var_41 then
                            var_3
                        else
                            var_4
        let (var_46: bool) =
            if (var_3 >= 0L) then
                (var_3 <= 1L)
            else
                false
        if var_46 then
            ()
        else
            (failwith "Argument out of bounds.")
        let (var_47: bool) =
            if (var_7 >= 0L) then
                (var_7 <= 14L)
            else
                false
        if var_47 then
            ()
        else
            (failwith "Argument out of bounds.")
        let (var_48: bool) =
            if (var_10 >= 0L) then
                (var_10 <= 14L)
            else
                false
        if var_48 then
            ()
        else
            (failwith "Argument out of bounds.")
        var_11.[int32 var_18] <- (Union0Case0(Tuple2(var_45)))
        var_45
let (var_0: (Union0 [])) = Array.zeroCreate<Union0> (System.Convert.ToInt32(450L))
let (var_1: int64) = 0L
let (var_2: int64) = 0L
let (var_3: int64) = method_0((var_0: (Union0 [])), (var_2: int64), (var_1: int64))
let (var_4: EnvHeap1) = ({mem_0 = (var_0: (Union0 []))} : EnvHeap1)
let (var_5: System.IO.Stream) = System.Console.OpenStandardInput()
let (var_6: System.IO.StreamReader) = System.IO.StreamReader(var_5)
let (var_7: string) = var_6.ReadToEnd()
let (var_8: int64) = 0L
let (var_10: bool) =
    if (var_8 >= 0L) then
        let (var_9: int64) = (int64 var_7.Length)
        (var_8 < var_9)
    else
        false
if var_10 then
    let (var_11: char) = var_7.[int32 var_8]
    let (var_12: bool) = ('-' = var_11)
    let (var_13: int64) = (var_8 + 1L)
    if var_12 then
        let (var_14: bool) = false
        method_3((var_14: bool), (var_4: EnvHeap1), (var_7: string), (var_13: int64))
    else
        let (var_15: bool) = true
        method_3((var_15: bool), (var_4: EnvHeap1), (var_7: string), (var_8: int64))
else
    let (var_16: bool) = true
    method_3((var_16: bool), (var_4: EnvHeap1), (var_7: string), (var_8: int64))

