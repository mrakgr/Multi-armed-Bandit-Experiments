let rec method_14((var_2: string), (var_3: int64), (var_4: int64)): unit =
    let (var_5: bool) = (var_3 < 5L)
    if var_5 then
        let (var_22: bool) = (var_4 >= 0L)
        let (var_23: int64) = (int64 var_2.Length)
        let (var_24: bool) = (var_4 < var_23)
        if (var_22 && var_24) then
            let (var_25: char) = var_2.[int32 var_4]
            let (var_26: int64) = (var_4 + 1L)
            let (var_27: char) = "Hello".[int32 var_3]
            let (var_28: bool) = (var_25 = var_27)
            if var_28 then
                let (var_31: int64) = (var_3 + 1L)
                method_14((var_2: string), (var_31: int64), (var_26: int64))
            else
                (failwith "Hello")
        else
            (failwith "string index out of bounds")
    else
        System.Console.WriteLine()
let (var_19: int64) = 0L
let (var_26: int64) = 0L
let (var_28: System.IO.Stream) = System.Console.OpenStandardInput()
let (var_31: System.IO.StreamReader) = System.IO.StreamReader(var_28)
let (var_33: string) = "Hello"
let (var_35: int64) = 0L
method_14((var_33: string), (var_26: int64), (var_35: int64))
