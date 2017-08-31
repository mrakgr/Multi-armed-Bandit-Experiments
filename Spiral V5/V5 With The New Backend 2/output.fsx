type Tuple2 =
    struct
    val mem_0: char
    val mem_1: char
    val mem_2: char
    val mem_3: char
    new(arg_mem_0, arg_mem_1, arg_mem_2, arg_mem_3) = {mem_0 = arg_mem_0; mem_1 = arg_mem_1; mem_2 = arg_mem_2; mem_3 = arg_mem_3}
    end
let rec method_14((var_0: char), (var_2: string), (var_3: int64)): Tuple2 =
    let (var_4: bool) = (var_0 >= '0')
    let (var_5: bool) = (var_0 <= '9')
    let (if_var_1: Tuple2) =
        if (var_4 && var_5) then
            let (var_30: bool) = (var_3 >= 0L)
            let (var_31: int64) = (int64 var_2.Length)
            let (var_32: bool) = (var_3 < var_31)
            let (if_var_2: Tuple2) =
                if (var_30 && var_32) then
                    let (var_33: char) = var_2.[int32 var_3]
                    let (var_34: int64) = (var_3 + 1L)
                    let (var_35: Tuple2) = method_15((var_33: char), (var_0: char), (var_2: string), (var_34: int64))
                    var_35
                else
                    Tuple2('a', 'a', 'a', 'a')
            if_var_2
        else
            Tuple2('a', 'a', 'a', 'a')
    if_var_1
and method_15((var_0: char), (var_1: char), (var_3: string), (var_4: int64)): Tuple2 =
    let (var_5: bool) = (var_0 >= '0')
    let (var_6: bool) = (var_0 <= '9')
    let (if_var_3: Tuple2) =
        if (var_5 && var_6) then
            let (var_31: bool) = (var_4 >= 0L)
            let (var_32: int64) = (int64 var_3.Length)
            let (var_33: bool) = (var_4 < var_32)
            let (if_var_4: Tuple2) =
                if (var_31 && var_33) then
                    let (var_34: char) = var_3.[int32 var_4]
                    let (var_35: int64) = (var_4 + 1L)
                    let (var_36: Tuple2) = method_16((var_34: char), (var_1: char), (var_0: char), (var_3: string), (var_35: int64))
                    var_36
                else
                    Tuple2('a', 'a', 'a', 'a')
            if_var_4
        else
            Tuple2('a', 'a', 'a', 'a')
    if_var_3
and method_16((var_0: char), (var_1: char), (var_2: char), (var_4: string), (var_5: int64)): Tuple2 =
    let (var_6: bool) = (var_0 >= '0')
    let (var_7: bool) = (var_0 <= '9')
    let (if_var_5: Tuple2) =
        if (var_6 && var_7) then
            let (var_32: bool) = (var_5 >= 0L)
            let (var_33: int64) = (int64 var_4.Length)
            let (var_34: bool) = (var_5 < var_33)
            let (if_var_6: Tuple2) =
                if (var_32 && var_34) then
                    let (var_35: char) = var_4.[int32 var_5]
                    let (var_36: int64) = (var_5 + 1L)
                    let (var_37: Tuple2) = method_17((var_35: char), (var_1: char), (var_2: char), (var_0: char), (var_4: string), (var_36: int64))
                    var_37
                else
                    Tuple2('a', 'a', 'a', 'a')
            if_var_6
        else
            Tuple2('a', 'a', 'a', 'a')
    if_var_5
and method_17((var_0: char), (var_1: char), (var_2: char), (var_3: char), (var_5: string), (var_6: int64)): Tuple2 =
    let (var_7: bool) = (var_0 >= '0')
    let (var_8: bool) = (var_0 <= '9')
    let (if_var_7: Tuple2) =
        if (var_7 && var_8) then
            Tuple2(var_1, var_2, var_3, var_0)
        else
            Tuple2('a', 'a', 'a', 'a')
    if_var_7
let (var_23: string) = System.Console.ReadLine()
let (var_24: int64) = 0L
let (var_25: int64) = var_24
let (var_50: bool) = (var_25 >= 0L)
let (var_51: int64) = (int64 var_23.Length)
let (var_52: bool) = (var_25 < var_51)
let (if_var_8: Tuple2) =
    if (var_50 && var_52) then
        let (var_53: char) = var_23.[int32 var_25]
        let (var_54: int64) = (var_25 + 1L)
        let (var_55: Tuple2) = method_14((var_53: char), (var_23: string), (var_54: int64))
        var_55
    else
        Tuple2('a', 'a', 'a', 'a')
if_var_8
