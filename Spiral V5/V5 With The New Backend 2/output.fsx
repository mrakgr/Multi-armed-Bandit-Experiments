let rec method_14((var_2: string), (var_3: int64), (var_6: int64)): unit =
    let (var_23: bool) = (var_6 >= 0L)
    let (var_24: int64) = (int64 var_2.Length)
    let (var_25: bool) = (var_6 < var_24)
    if (var_23 && var_25) then
        let (var_26: char) = var_2.[int32 var_6]
        let (var_27: int64) = (var_6 + 1L)
        let (var_28: bool) = (var_26 >= '0')
        let (var_29: bool) = (var_26 <= '9')
        let (var_30: bool) = (var_28 && var_29)
        if var_30 then
            let (var_37: int64) = System.Convert.ToInt64(var_26)
            let (var_38: int64) = System.Convert.ToInt64('0')
            let (var_39: int64) = (var_37 - var_38)
            let (var_40: int64) = (var_3 * 10L)
            let (var_41: int64) = (var_40 + var_39)
            method_15((var_2: string), (var_41: int64), (var_27: int64))
        else
            (failwith "pint64")
    else
        (failwith "pint64")
and method_15((var_2: string), (var_3: int64), (var_6: int64)): unit =
    let (var_23: bool) = (var_6 >= 0L)
    let (var_24: int64) = (int64 var_2.Length)
    let (var_25: bool) = (var_6 < var_24)
    if (var_23 && var_25) then
        let (var_26: char) = var_2.[int32 var_6]
        let (var_27: int64) = (var_6 + 1L)
        let (var_28: bool) = (var_26 >= '0')
        let (var_29: bool) = (var_26 <= '9')
        let (var_30: bool) = (var_28 && var_29)
        if var_30 then
            let (var_37: int64) = System.Convert.ToInt64(var_26)
            let (var_38: int64) = System.Convert.ToInt64('0')
            let (var_39: int64) = (var_37 - var_38)
            let (var_40: int64) = (var_3 * 10L)
            let (var_41: int64) = (var_40 + var_39)
            method_15((var_2: string), (var_41: int64), (var_27: int64))
        else
            System.Console.WriteLine(var_3)
    else
        System.Console.WriteLine(var_3)
let (var_19: int64) = 0L
let (var_27: System.IO.Stream) = System.Console.OpenStandardInput()
let (var_30: System.IO.StreamReader) = System.IO.StreamReader(var_27)
let (var_32: string) = var_30.ReadToEnd()
let (var_34: int64) = 0L
method_14((var_32: string), (var_19: int64), (var_34: int64))
