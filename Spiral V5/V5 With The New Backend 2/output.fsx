type Env0 =
    struct
    val mem_a: int64
    val mem_b: int64
    val mem_sum: int64
    new(arg_mem_a, arg_mem_b, arg_mem_sum) = {mem_a = arg_mem_a; mem_b = arg_mem_b; mem_sum = arg_mem_sum}
    end
let rec method_14((var_0: int64), (var_1: int64), (var_2: int64)): Env0 =
    let (var_3: bool) =
        if (var_1 <= 4000000L) then
            true
        else
            false
    if var_3 then
        let (var_4: int64) = (var_1 % 2L)
        let (var_5: int64) =
            if (var_4 = 0L) then
                (var_2 + var_1)
            else
                var_2
        let (var_6: int64) = (var_0 + var_1)
        method_14((var_1: int64), (var_6: int64), (var_5: int64))
    else
        Env0(var_0, var_1, var_2)
let (var_0: int64) = 0L
let (var_1: int64) = 1L
let (var_2: int64) = 2L
let (var_3: Env0) = method_14((var_1: int64), (var_2: int64), (var_0: int64))
let (var_4: int64) = var_3.mem_a
let (var_5: int64) = var_3.mem_b
let (var_6: int64) = var_3.mem_sum
System.Console.WriteLine(var_6)

