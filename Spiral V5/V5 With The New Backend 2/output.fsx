let rec method_15((var_0: int64)): int64 =
    if (var_0 <= 0L) then
        let (var_1: int64) = (var_0 + 1L)
        method_15((var_1: int64))
    else
        0L
and method_16((var_0: (int64 [])), (var_1: int64), (var_2: int64)): int64 =
    if (var_1 <= 1000L) then
        let (var_3: int64) = 2L
        let (var_4: int64) = method_17((var_1: int64), (var_0: (int64 [])), (var_3: int64), (var_2: int64))
        let (var_5: int64) = (var_2 + 9L)
        let (var_6: int64) = (var_1 + 1L)
        method_16((var_0: (int64 [])), (var_6: int64), (var_5: int64))
    else
        var_2
and method_17((var_0: int64), (var_1: (int64 [])), (var_2: int64), (var_3: int64)): int64 =
    if (var_2 <= 10L) then
        let (var_4: int64) = 2L
        let (var_5: int64) = method_18((var_0: int64), (var_2: int64), (var_4: int64))
        var_1.[int32 var_3] <- var_5
        let (var_6: int64) = (var_3 + 1L)
        let (var_7: int64) = (var_2 + 1L)
        method_17((var_0: int64), (var_1: (int64 [])), (var_7: int64), (var_6: int64))
    else
        var_3
and method_18((var_0: int64), (var_1: int64), (var_2: int64)): int64 =
    if (var_2 <= var_1) then
        let (var_3: int64) = (var_0 * var_0)
        let (var_4: int64) = (var_2 + 1L)
        method_19((var_0: int64), (var_1: int64), (var_4: int64), (var_3: int64))
    else
        var_0
and method_19((var_0: int64), (var_1: int64), (var_2: int64), (var_3: int64)): int64 =
    if (var_2 <= var_1) then
        let (var_4: int64) = (var_0 * var_3)
        let (var_5: int64) = (var_2 + 1L)
        method_19((var_0: int64), (var_1: int64), (var_5: int64), (var_4: int64))
    else
        var_3
and method_20((var_0: bool), (var_1: int64), (var_2: (int64 [])), (var_3: int64), (var_4: int64), (var_5: string), (var_6: int64)): unit =
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
            let (var_15: int64) = (0L + var_14)
            method_21((var_0: bool), (var_1: int64), (var_2: (int64 [])), (var_3: int64), (var_4: int64), (var_5: string), (var_15: int64), (var_11: int64))
        else
            (failwith "pint64")
    else
        (failwith "pint64")
and method_21((var_0: bool), (var_1: int64), (var_2: (int64 [])), (var_3: int64), (var_4: int64), (var_5: string), (var_6: int64), (var_7: int64)): unit =
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
                method_21((var_0: bool), (var_1: int64), (var_2: (int64 [])), (var_3: int64), (var_4: int64), (var_5: string), (var_19: int64), (var_12: int64))
            else
                (failwith "integer overflow")
        else
            let (var_20: int64) =
                if var_0 then
                    var_6
                else
                    (-var_6)
            let (var_21: int64) = 0L
            method_22((var_20: int64), (var_1: int64), (var_2: (int64 [])), (var_3: int64), (var_4: int64), (var_5: string), (var_21: int64), (var_7: int64))
    else
        let (var_22: int64) =
            if var_0 then
                var_6
            else
                (-var_6)
        let (var_23: int64) = 0L
        method_22((var_22: int64), (var_1: int64), (var_2: (int64 [])), (var_3: int64), (var_4: int64), (var_5: string), (var_23: int64), (var_7: int64))
and method_22((var_0: int64), (var_1: int64), (var_2: (int64 [])), (var_3: int64), (var_4: int64), (var_5: string), (var_6: int64), (var_7: int64)): unit =
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
            method_22((var_0: int64), (var_1: int64), (var_2: (int64 [])), (var_3: int64), (var_4: int64), (var_5: string), (var_8: int64), (var_14: int64))
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
                    method_23((var_18: bool), (var_0: int64), (var_1: int64), (var_2: (int64 [])), (var_3: int64), (var_4: int64), (var_5: string), (var_14: int64))
                else
                    let (var_19: bool) = true
                    method_23((var_19: bool), (var_0: int64), (var_1: int64), (var_2: (int64 [])), (var_3: int64), (var_4: int64), (var_5: string), (var_7: int64))
            else
                let (var_20: bool) = true
                method_23((var_20: bool), (var_0: int64), (var_1: int64), (var_2: (int64 [])), (var_3: int64), (var_4: int64), (var_5: string), (var_7: int64))
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
                method_23((var_26: bool), (var_0: int64), (var_1: int64), (var_2: (int64 [])), (var_3: int64), (var_4: int64), (var_5: string), (var_25: int64))
            else
                let (var_27: bool) = true
                method_23((var_27: bool), (var_0: int64), (var_1: int64), (var_2: (int64 [])), (var_3: int64), (var_4: int64), (var_5: string), (var_7: int64))
        else
            let (var_28: bool) = true
            method_23((var_28: bool), (var_0: int64), (var_1: int64), (var_2: (int64 [])), (var_3: int64), (var_4: int64), (var_5: string), (var_7: int64))
and method_23((var_0: bool), (var_1: int64), (var_2: int64), (var_3: (int64 [])), (var_4: int64), (var_5: int64), (var_6: string), (var_7: int64)): unit =
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
            let (var_16: int64) = (0L + var_15)
            method_24((var_0: bool), (var_1: int64), (var_2: int64), (var_3: (int64 [])), (var_4: int64), (var_5: int64), (var_6: string), (var_16: int64), (var_12: int64))
        else
            (failwith "pint64")
    else
        (failwith "pint64")
and method_24((var_0: bool), (var_1: int64), (var_2: int64), (var_3: (int64 [])), (var_4: int64), (var_5: int64), (var_6: string), (var_7: int64), (var_8: int64)): unit =
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
                method_24((var_0: bool), (var_1: int64), (var_2: int64), (var_3: (int64 [])), (var_4: int64), (var_5: int64), (var_6: string), (var_20: int64), (var_13: int64))
            else
                (failwith "integer overflow")
        else
            let (var_21: int64) =
                if var_0 then
                    var_7
                else
                    (-var_7)
            let (var_22: int64) = 0L
            method_25((var_21: int64), (var_1: int64), (var_2: int64), (var_3: (int64 [])), (var_4: int64), (var_5: int64), (var_6: string), (var_22: int64), (var_8: int64))
    else
        let (var_23: int64) =
            if var_0 then
                var_7
            else
                (-var_7)
        let (var_24: int64) = 0L
        method_25((var_23: int64), (var_1: int64), (var_2: int64), (var_3: (int64 [])), (var_4: int64), (var_5: int64), (var_6: string), (var_24: int64), (var_8: int64))
and method_25((var_0: int64), (var_1: int64), (var_2: int64), (var_3: (int64 [])), (var_4: int64), (var_5: int64), (var_6: string), (var_7: int64), (var_8: int64)): unit =
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
            method_25((var_0: int64), (var_1: int64), (var_2: int64), (var_3: (int64 [])), (var_4: int64), (var_5: int64), (var_6: string), (var_9: int64), (var_15: int64))
        else
            let (var_16: int64) = method_26((var_2: int64), (var_0: int64), (var_3: (int64 [])), (var_4: int64), (var_5: int64), (var_1: int64))
            System.Console.WriteLine(var_16)
    else
        let (var_17: int64) = method_26((var_2: int64), (var_0: int64), (var_3: (int64 [])), (var_4: int64), (var_5: int64), (var_1: int64))
        System.Console.WriteLine(var_17)
and method_26((var_0: int64), (var_1: int64), (var_2: (int64 [])), (var_3: int64), (var_4: int64), (var_5: int64)): int64 =
    method_27((var_1: int64), (var_2: (int64 [])), (var_4: int64), (var_5: int64), (var_0: int64), (var_3: int64))
and method_27((var_0: int64), (var_1: (int64 [])), (var_2: int64), (var_3: int64), (var_4: int64), (var_5: int64)): int64 =
    if (var_4 <= var_3) then
        let (var_6: bool) =
            if (var_4 >= 1L) then
                (var_4 <= 1000L)
            else
                false
        if (var_6 = false) then
            (failwith "Argument out of bounds.")
        else
            ()
        let (var_7: int64) = (var_4 - 1L)
        let (var_8: int64) = (9L * var_7)
        let (var_9: int64) = (0L + var_8)
        let (var_10: bool) =
            if (var_0 >= 2L) then
                (var_0 <= 10L)
            else
                false
        if (var_10 = false) then
            (failwith "Argument out of bounds.")
        else
            ()
        let (var_11: int64) = (var_0 - 2L)
        let (var_12: int64) = (1L * var_11)
        let (var_13: int64) = (var_9 + var_12)
        let (var_14: int64) = var_1.[int32 var_13]
        let (var_15: int64) = (var_2 + var_14)
        if (var_15 = var_3) then
            (var_5 + 1L)
        else
            if (var_15 < var_3) then
                let (var_16: int64) = (var_4 + 1L)
                let (var_17: int64) = method_26((var_16: int64), (var_0: int64), (var_1: (int64 [])), (var_5: int64), (var_15: int64), (var_3: int64))
                method_27((var_0: int64), (var_1: (int64 [])), (var_2: int64), (var_3: int64), (var_16: int64), (var_17: int64))
            else
                var_5
    else
        var_5
let (var_2: (int64 [])) = Array.zeroCreate<int64> (System.Convert.ToInt32(9000L))
let (var_3: int64) = 0L
let (var_4: int64) = 1L
let (var_5: int64) = method_16((var_2: (int64 [])), (var_4: int64), (var_3: int64))
let (var_6: int64) = 0L
let (var_7: int64) = 0L
let (var_8: int64) = 1L
let (var_9: System.IO.Stream) = System.Console.OpenStandardInput()
let (var_10: System.IO.StreamReader) = System.IO.StreamReader(var_9)
let (var_11: string) = var_10.ReadToEnd()
let (var_12: int64) = 0L
let (var_14: bool) =
    if (var_12 >= 0L) then
        let (var_13: int64) = (int64 var_11.Length)
        (var_12 < var_13)
    else
        false
if var_14 then
    let (var_15: char) = var_11.[int32 var_12]
    let (var_16: bool) = ('-' = var_15)
    let (var_17: int64) = (var_12 + 1L)
    if var_16 then
        let (var_18: bool) = false
        method_20((var_18: bool), (var_8: int64), (var_2: (int64 [])), (var_6: int64), (var_7: int64), (var_11: string), (var_17: int64))
    else
        let (var_19: bool) = true
        method_20((var_19: bool), (var_8: int64), (var_2: (int64 [])), (var_6: int64), (var_7: int64), (var_11: string), (var_12: int64))
else
    let (var_20: bool) = true
    method_20((var_20: bool), (var_8: int64), (var_2: (int64 [])), (var_6: int64), (var_7: int64), (var_11: string), (var_12: int64))

