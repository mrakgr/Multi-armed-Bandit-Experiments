let rec method_14((var_0: int64), (var_1: int64), (var_2: int64), (var_3: int64)): int64 =
    if (var_0 >= var_1) then
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
and method_15((var_0: int64), (var_1: int64), (var_2: int64), (var_3: int64)): int64 =
    if (var_0 <= var_1) then
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
        method_15((var_4: int64), (var_1: int64), (var_2: int64), (var_8: int64))
    else
        var_3
let (var_0: int64) = 999L
let (var_1: int64) = 3L
let (var_2: int64) = 0L
let (var_3: int64) = 0L
let (var_7: int64) =
    if (var_2 = 0L) then
        (failwith "The by field should not be zero in loop as the program would diverge.")
        var_3
    else
        if (var_2 < 0L) then
            method_14((var_0: int64), (var_1: int64), (var_2: int64), (var_3: int64))
        else
            method_15((var_0: int64), (var_1: int64), (var_2: int64), (var_3: int64))
System.Console.WriteLine(var_7)

