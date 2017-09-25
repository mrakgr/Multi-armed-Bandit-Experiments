let rec method_14((var_0: string), (var_1: int64)): unit =
    let (var_3: bool) =
        if (var_1 >= 0L) then
            let (var_2: int64) = (int64 var_0.Length)
            (var_1 < var_2)
        else
            false
    if var_3 then
        let (var_4: char) = var_0.[int32 var_1]
        let (var_5: bool) =
            if (var_4 >= '0') then
                (var_4 <= '9')
            else
                false
        let (var_6: int64) = (var_1 + 1L)
        if var_5 then
            let (var_7: int64) = System.Convert.ToInt64(var_4)
            let (var_8: int64) = System.Convert.ToInt64('0')
            let (var_9: int64) = (var_7 - var_8)
            let (var_10: int64) = (0L + var_9)
            method_15((var_0: string), (var_10: int64), (var_6: int64))
        else
            (failwith "pint64")
    else
        (failwith "pint64")
and method_15((var_0: string), (var_1: int64), (var_2: int64)): unit =
    let (var_4: bool) =
        if (var_2 >= 0L) then
            let (var_3: int64) = (int64 var_0.Length)
            (var_2 < var_3)
        else
            false
    if var_4 then
        let (var_5: char) = var_0.[int32 var_2]
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
            let (var_11: bool) =
                if (var_1 = 922337203685477580L) then
                    (var_10 <= 7L)
                else
                    false
            let (var_12: bool) =
                if var_11 then
                    true
                else
                    (var_1 < 922337203685477580L)
            if var_12 then
                let (var_13: int64) = (var_1 * 10L)
                let (var_14: int64) = (var_13 + var_10)
                method_15((var_0: string), (var_14: int64), (var_7: int64))
            else
                (failwith "integer overflow")
        else
            let (var_15: int64) = 0L
            method_16((var_1: int64), (var_0: string), (var_15: int64), (var_7: int64))
    else
        let (var_16: int64) = 0L
        method_16((var_1: int64), (var_0: string), (var_16: int64), (var_2: int64))
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
            let (var_11: int64) = (0L + var_0)
            System.Console.WriteLine(var_11)
    else
        let (var_12: int64) = (0L + var_0)
        System.Console.WriteLine(var_12)
let (var_0: System.IO.Stream) = System.Console.OpenStandardInput()
let (var_1: System.IO.StreamReader) = System.IO.StreamReader(var_0)
let (var_2: string) = var_1.ReadToEnd()
let (var_3: int64) = 0L
method_14((var_2: string), (var_3: int64))

