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
            let (var_35: int64) = System.Convert.ToInt64(var_26)
            let (var_36: int64) = System.Convert.ToInt64('0')
            let (var_37: int64) = (var_35 - var_36)
            let (var_38: int64) = (var_3 * 10L)
            let (var_39: int64) = (var_38 + var_37)
            method_14((var_2: string), (var_39: int64), (var_27: int64))
        else
            (failwith "digit")
    else
        (failwith "string index out of bounds")
let (var_26: System.IO.Stream) = System.Console.OpenStandardInput()
let (var_29: System.IO.StreamReader) = System.IO.StreamReader(var_26)
let (var_31: string) = var_29.ReadToEnd()
let (var_33: int64) = 0L
let (var_40: int64) = 0L
method_14((var_31: string), (var_40: int64), (var_33: int64))
