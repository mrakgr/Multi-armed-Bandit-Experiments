let rec method_14((var_1: int64), (var_2: int64)): int64 =
    let (var_3: bool) = true
    let (if_var_1: int64) =
        if var_3 then
            let (var_4: int64) = method_15((var_1: int64), (var_2: int64))
            var_4
        else
            (var_1 + var_2)
    if_var_1
and method_15((var_1: int64), (var_2: int64)): int64 =
    let (var_3: bool) = true
    let (if_var_2: int64) =
        if var_3 then
            let (var_4: int64) = method_14((var_1: int64), (var_2: int64))
            var_4
        else
            (var_2 + var_1)
    if_var_2
let (var_16: int64) = 1L
let (var_17: int64) = 2L
let (var_18: int64) = method_14((var_16: int64), (var_17: int64))
var_18
