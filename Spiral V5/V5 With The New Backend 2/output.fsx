let rec method_14((var_0: bool), (var_1: string), (var_2: int64)): unit =
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
            method_15((var_0: bool), (var_1: string), (var_11: int64), (var_7: int64))
        else
            (failwith "pint64")
    else
        (failwith "pint64")
and method_15((var_0: bool), (var_1: string), (var_2: int64), (var_3: int64)): unit =
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
                method_15((var_0: bool), (var_1: string), (var_15: int64), (var_8: int64))
            else
                (failwith "integer overflow")
        else
            let (var_16: int64) =
                if var_0 then
                    var_2
                else
                    (-var_2)
            let (var_17: int64) = 0L
            method_16((var_16: int64), (var_1: string), (var_17: int64), (var_3: int64))
    else
        let (var_18: int64) =
            if var_0 then
                var_2
            else
                (-var_2)
        let (var_19: int64) = 0L
        method_16((var_18: int64), (var_1: string), (var_19: int64), (var_3: int64))
and method_16((var_0: int64), (var_1: string), (var_2: int64), (var_3: int64)): unit =
    let (var_4: int64) = (var_2 + 1L)
    let (var_6: bool) =
        if (var_3 >= 0L) then
            let (var_5: int64) = (int64 var_1.Length)
            (var_3 < var_5)
        else
            false
    if var_6 then
        let (var_7: char) = var_1.[int32 var_3]
        let (var_9: bool) =
            if (var_7 = ' ') then
                true
            else
                if (var_7 = '\n') then
                    true
                else
                    (var_7 = '\r')
        let (var_10: int64) = (var_3 + 1L)
        if var_9 then
            method_16((var_0: int64), (var_1: string), (var_4: int64), (var_10: int64))
        else
            let (var_11: int64) = 0L
            method_17((var_1: string), (var_11: int64), (var_0: int64), (var_3: int64))
    else
        let (var_12: int64) = 0L
        method_17((var_1: string), (var_12: int64), (var_0: int64), (var_3: int64))
and method_17((var_0: string), (var_1: int64), (var_2: int64), (var_3: int64)): unit =
    let (var_4: bool) = (var_1 < var_2)
    if var_4 then
        let (var_5: int64) = (var_1 + 1L)
        let (var_7: bool) =
            if (var_3 >= 0L) then
                let (var_6: int64) = (int64 var_0.Length)
                (var_3 < var_6)
            else
                false
        if var_7 then
            let (var_8: char) = var_0.[int32 var_3]
            let (var_9: bool) = ('-' = var_8)
            let (var_10: int64) = (var_3 + 1L)
            if var_9 then
                let (var_11: bool) = false
                method_18((var_11: bool), (var_5: int64), (var_2: int64), (var_0: string), (var_10: int64))
            else
                let (var_12: bool) = true
                method_18((var_12: bool), (var_5: int64), (var_2: int64), (var_0: string), (var_3: int64))
        else
            let (var_13: bool) = true
            method_18((var_13: bool), (var_5: int64), (var_2: int64), (var_0: string), (var_3: int64))
    else
        ()
and method_18((var_0: bool), (var_1: int64), (var_2: int64), (var_3: string), (var_4: int64)): unit =
    let (var_6: bool) =
        if (var_4 >= 0L) then
            let (var_5: int64) = (int64 var_3.Length)
            (var_4 < var_5)
        else
            false
    if var_6 then
        let (var_7: char) = var_3.[int32 var_4]
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
            let (var_13: int64) = (0L + var_12)
            method_19((var_0: bool), (var_1: int64), (var_2: int64), (var_3: string), (var_13: int64), (var_9: int64))
        else
            (failwith "pint64")
    else
        (failwith "pint64")
and method_19((var_0: bool), (var_1: int64), (var_2: int64), (var_3: string), (var_4: int64), (var_5: int64)): unit =
    let (var_7: bool) =
        if (var_5 >= 0L) then
            let (var_6: int64) = (int64 var_3.Length)
            (var_5 < var_6)
        else
            false
    if var_7 then
        let (var_8: char) = var_3.[int32 var_5]
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
            let (var_14: bool) =
                if (var_4 = 922337203685477580L) then
                    (var_13 <= 7L)
                else
                    false
            let (var_15: bool) =
                if var_14 then
                    true
                else
                    (var_4 < 922337203685477580L)
            if var_15 then
                let (var_16: int64) = (var_4 * 10L)
                let (var_17: int64) = (var_16 + var_13)
                method_19((var_0: bool), (var_1: int64), (var_2: int64), (var_3: string), (var_17: int64), (var_10: int64))
            else
                (failwith "integer overflow")
        else
            let (var_18: int64) =
                if var_0 then
                    var_4
                else
                    (-var_4)
            let (var_19: int64) = 0L
            method_20((var_18: int64), (var_1: int64), (var_2: int64), (var_3: string), (var_19: int64), (var_5: int64))
    else
        let (var_20: int64) =
            if var_0 then
                var_4
            else
                (-var_4)
        let (var_21: int64) = 0L
        method_20((var_20: int64), (var_1: int64), (var_2: int64), (var_3: string), (var_21: int64), (var_5: int64))
and method_20((var_0: int64), (var_1: int64), (var_2: int64), (var_3: string), (var_4: int64), (var_5: int64)): unit =
    let (var_6: int64) = (var_4 + 1L)
    let (var_8: bool) =
        if (var_5 >= 0L) then
            let (var_7: int64) = (int64 var_3.Length)
            (var_5 < var_7)
        else
            false
    if var_8 then
        let (var_9: char) = var_3.[int32 var_5]
        let (var_11: bool) =
            if (var_9 = ' ') then
                true
            else
                if (var_9 = '\n') then
                    true
                else
                    (var_9 = '\r')
        let (var_12: int64) = (var_5 + 1L)
        if var_11 then
            method_20((var_0: int64), (var_1: int64), (var_2: int64), (var_3: string), (var_6: int64), (var_12: int64))
        else
            let (var_13: bool) = (var_0 >= 0L)
            if var_13 then
                let (var_14: (int64 [])) = Array.zeroCreate<int64> (System.Convert.ToInt32(var_0))
                let (var_15: int64) = 0L
                method_21((var_14: (int64 [])), (var_1: int64), (var_2: int64), (var_3: string), (var_15: int64), (var_0: int64), (var_5: int64))
            else
                (failwith "n in parse array must be >= 0")
    else
        let (var_16: bool) = (var_0 >= 0L)
        if var_16 then
            let (var_17: (int64 [])) = Array.zeroCreate<int64> (System.Convert.ToInt32(var_0))
            let (var_18: int64) = 0L
            method_21((var_17: (int64 [])), (var_1: int64), (var_2: int64), (var_3: string), (var_18: int64), (var_0: int64), (var_5: int64))
        else
            (failwith "n in parse array must be >= 0")
and method_21((var_0: (int64 [])), (var_1: int64), (var_2: int64), (var_3: string), (var_4: int64), (var_5: int64), (var_6: int64)): unit =
    let (var_7: bool) = (var_4 < var_5)
    if var_7 then
        let (var_8: int64) = (var_4 + 1L)
        let (var_10: bool) =
            if (var_6 >= 0L) then
                let (var_9: int64) = (int64 var_3.Length)
                (var_6 < var_9)
            else
                false
        if var_10 then
            let (var_11: char) = var_3.[int32 var_6]
            let (var_12: bool) = ('-' = var_11)
            let (var_13: int64) = (var_6 + 1L)
            if var_12 then
                let (var_14: bool) = false
                method_22((var_14: bool), (var_0: (int64 [])), (var_4: int64), (var_8: int64), (var_5: int64), (var_1: int64), (var_2: int64), (var_3: string), (var_13: int64))
            else
                let (var_15: bool) = true
                method_22((var_15: bool), (var_0: (int64 [])), (var_4: int64), (var_8: int64), (var_5: int64), (var_1: int64), (var_2: int64), (var_3: string), (var_6: int64))
        else
            let (var_16: bool) = true
            method_22((var_16: bool), (var_0: (int64 [])), (var_4: int64), (var_8: int64), (var_5: int64), (var_1: int64), (var_2: int64), (var_3: string), (var_6: int64))
    else
        let (var_17: int64) = var_0.LongLength
        let (var_18: int64) = 0L
        let (var_19: int64) = method_25((var_0: (int64 [])), (var_17: int64), (var_18: int64))
        if (var_19 = 0L) then
            System.Console.WriteLine("Second")
        else
            System.Console.WriteLine("First")
        method_17((var_3: string), (var_1: int64), (var_2: int64), (var_6: int64))
and method_22((var_0: bool), (var_1: (int64 [])), (var_2: int64), (var_3: int64), (var_4: int64), (var_5: int64), (var_6: int64), (var_7: string), (var_8: int64)): unit =
    let (var_10: bool) =
        if (var_8 >= 0L) then
            let (var_9: int64) = (int64 var_7.Length)
            (var_8 < var_9)
        else
            false
    if var_10 then
        let (var_11: char) = var_7.[int32 var_8]
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
            let (var_17: int64) = (0L + var_16)
            method_23((var_0: bool), (var_1: (int64 [])), (var_2: int64), (var_3: int64), (var_4: int64), (var_5: int64), (var_6: int64), (var_7: string), (var_17: int64), (var_13: int64))
        else
            (failwith "pint64")
    else
        (failwith "pint64")
and method_23((var_0: bool), (var_1: (int64 [])), (var_2: int64), (var_3: int64), (var_4: int64), (var_5: int64), (var_6: int64), (var_7: string), (var_8: int64), (var_9: int64)): unit =
    let (var_11: bool) =
        if (var_9 >= 0L) then
            let (var_10: int64) = (int64 var_7.Length)
            (var_9 < var_10)
        else
            false
    if var_11 then
        let (var_12: char) = var_7.[int32 var_9]
        let (var_13: bool) =
            if (var_12 >= '0') then
                (var_12 <= '9')
            else
                false
        let (var_14: int64) = (var_9 + 1L)
        if var_13 then
            let (var_15: int64) = System.Convert.ToInt64(var_12)
            let (var_16: int64) = System.Convert.ToInt64('0')
            let (var_17: int64) = (var_15 - var_16)
            let (var_18: bool) =
                if (var_8 = 922337203685477580L) then
                    (var_17 <= 7L)
                else
                    false
            let (var_19: bool) =
                if var_18 then
                    true
                else
                    (var_8 < 922337203685477580L)
            if var_19 then
                let (var_20: int64) = (var_8 * 10L)
                let (var_21: int64) = (var_20 + var_17)
                method_23((var_0: bool), (var_1: (int64 [])), (var_2: int64), (var_3: int64), (var_4: int64), (var_5: int64), (var_6: int64), (var_7: string), (var_21: int64), (var_14: int64))
            else
                (failwith "integer overflow")
        else
            let (var_22: int64) =
                if var_0 then
                    var_8
                else
                    (-var_8)
            let (var_23: int64) = 0L
            method_24((var_22: int64), (var_1: (int64 [])), (var_2: int64), (var_3: int64), (var_4: int64), (var_5: int64), (var_6: int64), (var_7: string), (var_23: int64), (var_9: int64))
    else
        let (var_24: int64) =
            if var_0 then
                var_8
            else
                (-var_8)
        let (var_25: int64) = 0L
        method_24((var_24: int64), (var_1: (int64 [])), (var_2: int64), (var_3: int64), (var_4: int64), (var_5: int64), (var_6: int64), (var_7: string), (var_25: int64), (var_9: int64))
and method_24((var_0: int64), (var_1: (int64 [])), (var_2: int64), (var_3: int64), (var_4: int64), (var_5: int64), (var_6: int64), (var_7: string), (var_8: int64), (var_9: int64)): unit =
    let (var_10: int64) = (var_8 + 1L)
    let (var_12: bool) =
        if (var_9 >= 0L) then
            let (var_11: int64) = (int64 var_7.Length)
            (var_9 < var_11)
        else
            false
    if var_12 then
        let (var_13: char) = var_7.[int32 var_9]
        let (var_15: bool) =
            if (var_13 = ' ') then
                true
            else
                if (var_13 = '\n') then
                    true
                else
                    (var_13 = '\r')
        let (var_16: int64) = (var_9 + 1L)
        if var_15 then
            method_24((var_0: int64), (var_1: (int64 [])), (var_2: int64), (var_3: int64), (var_4: int64), (var_5: int64), (var_6: int64), (var_7: string), (var_10: int64), (var_16: int64))
        else
            var_1.[int32 var_2] <- var_0
            method_21((var_1: (int64 [])), (var_5: int64), (var_6: int64), (var_7: string), (var_3: int64), (var_4: int64), (var_9: int64))
    else
        var_1.[int32 var_2] <- var_0
        method_21((var_1: (int64 [])), (var_5: int64), (var_6: int64), (var_7: string), (var_3: int64), (var_4: int64), (var_9: int64))
and method_25((var_0: (int64 [])), (var_1: int64), (var_2: int64)): int64 =
    if (var_2 < var_1) then
        let (var_3: int64) = var_0.[int32 var_2]
        let (var_4: int64) = (0L ^^^ var_3)
        let (var_5: int64) = (var_2 + 1L)
        method_26((var_0: (int64 [])), (var_1: int64), (var_5: int64), (var_4: int64))
    else
        0L
and method_26((var_0: (int64 [])), (var_1: int64), (var_2: int64), (var_3: int64)): int64 =
    if (var_2 < var_1) then
        let (var_4: int64) = var_0.[int32 var_2]
        let (var_5: int64) = (var_3 ^^^ var_4)
        let (var_6: int64) = (var_2 + 1L)
        method_26((var_0: (int64 [])), (var_1: int64), (var_6: int64), (var_5: int64))
    else
        var_3
let (var_0: System.IO.Stream) = System.Console.OpenStandardInput()
let (var_1: System.IO.StreamReader) = System.IO.StreamReader(var_0)
let (var_2: string) = var_1.ReadToEnd()
let (var_3: int64) = 0L
let (var_5: bool) =
    if (var_3 >= 0L) then
        let (var_4: int64) = (int64 var_2.Length)
        (var_3 < var_4)
    else
        false
if var_5 then
    let (var_6: char) = var_2.[int32 var_3]
    let (var_7: bool) = ('-' = var_6)
    let (var_8: int64) = (var_3 + 1L)
    if var_7 then
        let (var_9: bool) = false
        method_14((var_9: bool), (var_2: string), (var_8: int64))
    else
        let (var_10: bool) = true
        method_14((var_10: bool), (var_2: string), (var_3: int64))
else
    let (var_11: bool) = true
    method_14((var_11: bool), (var_2: string), (var_3: int64))

