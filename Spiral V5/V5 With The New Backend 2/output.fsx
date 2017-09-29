let rec method_14((var_0: int64), (var_1: int64), (var_2: int64), (var_3: int64)): int64 =
    if (var_0 < var_1) then
        let (var_4: int64) = (var_0 + var_2)
        let (var_5: int64) = (var_0 % 3L)
        let (var_7: bool) =
            if (var_5 = 0L) then
                true
            else
                let (var_6: int64) = (var_0 % 5L)
                (var_6 = 0L)
        let (var_8: int64) =
            if var_7 then
                (var_3 + var_0)
            else
                var_3
        method_14((var_4: int64), (var_1: int64), (var_2: int64), (var_8: int64))
    else
        var_3
let (var_0: int64) = 1000L
let (var_1: int64) = 0L
let (var_2: int64) = 3L
let (var_3: int64) = 1L
let (var_4: int64) = method_14((var_2: int64), (var_0: int64), (var_3: int64), (var_1: int64))
System.Console.WriteLine(var_4)

