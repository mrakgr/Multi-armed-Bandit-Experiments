type Tuple0 =
    struct
    val mem_0: Union1
    val mem_1: Union1
    val mem_2: Union1
    new(arg_mem_0, arg_mem_1, arg_mem_2) = {mem_0 = arg_mem_0; mem_1 = arg_mem_1; mem_2 = arg_mem_2}
    end
and Union1 =
    | Union1Case0
    | Union1Case1
let rec method_16(): Tuple0 =
    Tuple0(Union1Case0, Union1Case0, Union1Case0)
let (var_24: Tuple0) = method_16()
let (var_25: Union1) = var_24.mem_0
let (var_26: Union1) = var_24.mem_1
let (var_27: Union1) = var_24.mem_2
let (if_var_1: int64) =
    match var_25 with
    | Union1Case0 ->
        let (if_var_2: int64) =
            match var_26 with
            | Union1Case0 ->
                let (if_var_3: int64) =
                    match var_27 with
                    | Union1Case0 ->
                        4L
                    | Union1Case1 ->
                        4L
                if_var_3
            | Union1Case1 ->
                let (if_var_4: int64) =
                    match var_27 with
                    | Union1Case0 ->
                        4L
                    | Union1Case1 ->
                        4L
                if_var_4
        if_var_2
    | Union1Case1 ->
        let (if_var_5: int64) =
            match var_26 with
            | Union1Case0 ->
                let (if_var_6: int64) =
                    match var_27 with
                    | Union1Case0 ->
                        4L
                    | Union1Case1 ->
                        4L
                if_var_6
            | Union1Case1 ->
                let (if_var_7: int64) =
                    match var_27 with
                    | Union1Case0 ->
                        4L
                    | Union1Case1 ->
                        4L
                if_var_7
        if_var_5
if_var_1