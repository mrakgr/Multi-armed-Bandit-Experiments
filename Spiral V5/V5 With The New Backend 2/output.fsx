type Env0 =
    struct
    val mem_highest_palindrome: int64
    new(arg_mem_highest_palindrome) = {mem_highest_palindrome = arg_mem_highest_palindrome}
    end
and Env1 =
    struct
    val mem_x: int64
    val mem_x': int64
    new(arg_mem_x, arg_mem_x') = {mem_x = arg_mem_x; mem_x' = arg_mem_x'}
    end
let rec method_14((var_0: int64), (var_1: int64), (var_2: int64)): Env0 =
    if (var_0 <= var_2) then
        let (var_3: int64) = 999L
        let (var_4: Env0) = method_15((var_0: int64), (var_1: int64), (var_3: int64))
        let (var_5: int64) = var_4.mem_highest_palindrome
        let (var_6: int64) = (var_0 + 1L)
        method_14((var_6: int64), (var_5: int64), (var_2: int64))
    else
        Env0(var_1)
and method_15((var_0: int64), (var_1: int64), (var_2: int64)): Env0 =
    if (var_0 <= var_2) then
        let (var_3: int64) = (var_0 * var_0)
        let (var_4: int64) = 0L
        let (var_5: Env1) = method_16((var_3: int64), (var_4: int64))
        let (var_6: int64) = var_5.mem_x
        let (var_7: int64) = var_5.mem_x'
        let (var_8: bool) =
            if (var_3 = var_7) then
                (var_1 < var_3)
            else
                false
        let (var_9: Env0) =
            if var_8 then
                Env0(var_3)
            else
                Env0(var_1)
        let (var_10: int64) = var_9.mem_highest_palindrome
        let (var_11: int64) = (var_0 + 1L)
        method_17((var_0: int64), (var_11: int64), (var_10: int64), (var_2: int64))
    else
        Env0(var_1)
and method_16((var_0: int64), (var_1: int64)): Env1 =
    if (var_0 > 0L) then
        let (var_2: int64) = (var_0 / 10L)
        let (var_3: int64) = (var_1 * 10L)
        let (var_4: int64) = (var_0 % 10L)
        let (var_5: int64) = (var_3 + var_4)
        method_16((var_2: int64), (var_5: int64))
    else
        Env1(var_0, var_1)
and method_17((var_0: int64), (var_1: int64), (var_2: int64), (var_3: int64)): Env0 =
    if (var_1 <= var_3) then
        let (var_4: int64) = (var_0 * var_1)
        let (var_5: int64) = 0L
        let (var_6: Env1) = method_16((var_4: int64), (var_5: int64))
        let (var_7: int64) = var_6.mem_x
        let (var_8: int64) = var_6.mem_x'
        let (var_9: bool) =
            if (var_4 = var_8) then
                (var_2 < var_4)
            else
                false
        let (var_10: Env0) =
            if var_9 then
                Env0(var_4)
            else
                Env0(var_2)
        let (var_11: int64) = var_10.mem_highest_palindrome
        let (var_12: int64) = (var_1 + 1L)
        method_17((var_0: int64), (var_12: int64), (var_11: int64), (var_3: int64))
    else
        Env0(var_2)
let (var_0: int64) = 100L
let (var_1: int64) = 999L
let (var_2: int64) = 0L
let (var_3: Env0) = method_14((var_0: int64), (var_2: int64), (var_1: int64))
let (var_4: int64) = var_3.mem_highest_palindrome
System.Console.WriteLine(var_4)

