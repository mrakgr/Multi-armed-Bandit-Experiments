type Rec0 =
    | Rec0Case0
    | Rec0Case1 of Tuple1
and Tuple1 =
    struct
    val mem_0: int64
    val mem_1: Rec0
    new(arg_mem_0, arg_mem_1) = {mem_0 = arg_mem_0; mem_1 = arg_mem_1}
    end
let rec method_18((var_0: Rec0), (var_1: Rec0)): bool =
    match var_0 with
    | Rec0Case0 ->
        match var_1 with
        | Rec0Case0 ->
            true
        | Rec0Case1(var_3) ->
            let (var_4: int64) = var_3.mem_0
            let (var_5: Rec0) = var_3.mem_1
            false
    | Rec0Case1(var_2) ->
        let (var_7: int64) = var_2.mem_0
        let (var_8: Rec0) = var_2.mem_1
        match var_1 with
        | Rec0Case0 ->
            false
        | Rec0Case1(var_9) ->
            let (var_10: int64) = var_9.mem_0
            let (var_11: Rec0) = var_9.mem_1
            if (var_7 = var_10) then
                let (var_12: bool) = method_18((var_8: Rec0), (var_11: Rec0))
                if var_12 then
                    true
                else
                    false
            else
                false
let (var_0: Rec0) = Rec0Case0
let (var_1: Rec0) = Rec0Case0
method_18((var_0: Rec0), (var_1: Rec0))
