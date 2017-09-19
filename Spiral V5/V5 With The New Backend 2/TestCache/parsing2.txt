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
    let (var_7: bool) =
        if (var_6 >= '0') then
            (var_6 <= '9')
        else
            false
    let (var_8: int64) = (var_3 + 1L)
    if var_7 then
        System.Console.WriteLine(var_6)
    else
        (failwith "digit")
else
    (failwith "string index out of bounds")

