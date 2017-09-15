let rec method_25((var_0: (int64 [])), (var_1: string), (var_2: int64), (var_3: int64)): unit =
    let (var_4: bool) = (var_2 < 10L)
    if var_4 then
        let (var_6: bool) =
            if (var_3 >= 0L) then
                let (var_5: int64) = (int64 var_1.Length)
                (var_3 < var_5)
            else
                false
        if var_6 then
            let (var_7: char) = var_1.[int32 var_3]
            let (var_8: int64) = (var_3 + 1L)
            let (var_9: bool) = ('-' = var_7)
            if var_9 then
                let (var_10: bool) = false
                method_26((var_10: bool), (var_0: (int64 [])), (var_2: int64), (var_1: string), (var_8: int64))
            else
                let (var_11: bool) = true
                method_26((var_11: bool), (var_0: (int64 [])), (var_2: int64), (var_1: string), (var_3: int64))
        else
            let (var_12: bool) = true
            method_26((var_12: bool), (var_0: (int64 [])), (var_2: int64), (var_1: string), (var_3: int64))
    else
        System.Console.WriteLine(var_0)
and method_26((var_0: bool), (var_1: (int64 [])), (var_2: int64), (var_3: string), (var_4: int64)): unit =
    let (var_6: bool) =
        if (var_4 >= 0L) then
            let (var_5: int64) = (int64 var_3.Length)
            (var_4 < var_5)
        else
            false
    if var_6 then
        let (var_7: char) = var_3.[int32 var_4]
        let (var_8: int64) = (var_4 + 1L)
        let (var_9: bool) = ((var_7 >= '0') && (var_7 <= '9'))
        if var_9 then
            let (var_10: int64) = System.Convert.ToInt64(var_7)
            let (var_11: int64) = System.Convert.ToInt64('0')
            let (var_12: int64) = (var_10 - var_11)
            let (var_13: int64) = (0L + var_12)
            method_27((var_0: bool), (var_1: (int64 [])), (var_2: int64), (var_3: string), (var_13: int64), (var_8: int64))
        else
            (failwith "pint64")
    else
        (failwith "pint64")
and method_27((var_0: bool), (var_1: (int64 [])), (var_2: int64), (var_3: string), (var_4: int64), (var_5: int64)): unit =
    let (var_7: bool) =
        if (var_5 >= 0L) then
            let (var_6: int64) = (int64 var_3.Length)
            (var_5 < var_6)
        else
            false
    if var_7 then
        let (var_8: char) = var_3.[int32 var_5]
        let (var_9: int64) = (var_5 + 1L)
        let (var_10: bool) = ((var_8 >= '0') && (var_8 <= '9'))
        if var_10 then
            let (var_11: int64) = System.Convert.ToInt64(var_8)
            let (var_12: int64) = System.Convert.ToInt64('0')
            let (var_13: int64) = (var_11 - var_12)
            let (var_14: bool) = ((var_4 = 922337203685477580L) && (var_13 <= 7L))
            let (var_15: bool) = (var_14 || (var_4 < 922337203685477580L))
            if var_15 then
                let (var_16: int64) = (var_4 * 10L)
                let (var_17: int64) = (var_16 + var_13)
                method_27((var_0: bool), (var_1: (int64 [])), (var_2: int64), (var_3: string), (var_17: int64), (var_9: int64))
            else
                (failwith "integer overflow")
        else
            let (var_18: int64) =
                if var_0 then
                    var_4
                else
                    (-var_4)
            let (var_19: int64) = 0L
            method_28((var_18: int64), (var_1: (int64 [])), (var_2: int64), (var_3: string), (var_19: int64), (var_5: int64))
    else
        let (var_20: int64) =
            if var_0 then
                var_4
            else
                (-var_4)
        let (var_21: int64) = 0L
        method_28((var_20: int64), (var_1: (int64 [])), (var_2: int64), (var_3: string), (var_21: int64), (var_5: int64))
and method_28((var_0: int64), (var_1: (int64 [])), (var_2: int64), (var_3: string), (var_4: int64), (var_5: int64)): unit =
    let (var_6: int64) = (var_4 + 1L)
    let (var_8: bool) =
        if (var_5 >= 0L) then
            let (var_7: int64) = (int64 var_3.Length)
            (var_5 < var_7)
        else
            false
    if var_8 then
        let (var_9: char) = var_3.[int32 var_5]
        let (var_10: int64) = (var_5 + 1L)
        ((var_9 = '\n') || (var_9 = '\r'))
        let (var_12: bool) = ((var_9 = ' ') || )
        if var_12 then
            method_28((var_0: int64), (var_1: (int64 [])), (var_2: int64), (var_3: string), (var_6: int64), (var_10: int64))
        else
            var_1.[int32 var_2] <- var_0
            let (var_13: int64) = (var_2 + 1L)
            method_25((var_1: (int64 [])), (var_3: string), (var_13: int64), (var_5: int64))
    else
        var_1.[int32 var_2] <- var_0
        let (var_14: int64) = (var_2 + 1L)
        method_25((var_1: (int64 [])), (var_3: string), (var_14: int64), (var_5: int64))
let (var_0: System.IO.Stream) = System.Console.OpenStandardInput()
let (var_1: System.IO.StreamReader) = System.IO.StreamReader(var_0)
let (var_2: string) = var_1.ReadToEnd()
let (var_3: int64) = 0L
let (var_7: (int64 [])) = Array.zeroCreate<int64> (System.Convert.ToInt32(10L))
let (var_8: int64) = 0L
method_25((var_7: (int64 [])), (var_2: string), (var_8: int64), (var_3: int64))

