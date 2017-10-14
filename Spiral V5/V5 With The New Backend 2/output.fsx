type Union0 =
    | Union0Case0
    | Union0Case1
    | Union0Case2
and Env1 =
    struct
    val mem_mario: Union2
    val mem_princess: Union2
    new(arg_mem_mario, arg_mem_princess) = {mem_mario = arg_mem_mario; mem_princess = arg_mem_princess}
    end
and Union2 =
    | Union2Case0 of Tuple8
    | Union2Case1
and Tuple3 =
    struct
    val mem_0: int64
    val mem_1: int64
    new(arg_mem_0, arg_mem_1) = {mem_0 = arg_mem_0; mem_1 = arg_mem_1}
    end
and EnvHeap4 =
    {
    mem_0: (bool [])
    mem_1: int64
    mem_2: int64
    }
and Tuple5 =
    struct
    val mem_0: Tuple3
    val mem_1: Rec6
    new(arg_mem_0, arg_mem_1) = {mem_0 = arg_mem_0; mem_1 = arg_mem_1}
    end
and Rec6 =
    | Rec6Case0
    | Rec6Case1 of Tuple9
and Union7 =
    | Union7Case0 of Tuple10
    | Union7Case1
and Tuple8 =
    struct
    val mem_1: Tuple3
    new(arg_mem_1) = {mem_1 = arg_mem_1}
    end
and Tuple9 =
    struct
    val mem_0: string
    val mem_1: Rec6
    new(arg_mem_0, arg_mem_1) = {mem_0 = arg_mem_0; mem_1 = arg_mem_1}
    end
and Tuple10 =
    struct
    val mem_1: Tuple5
    new(arg_mem_1) = {mem_1 = arg_mem_1}
    end
let rec method_19((var_0: bool), (var_1: string), (var_2: int64)): unit =
    let (var_4: bool) =
        if (var_2 >= 0L) then
            let (var_3: int64) = (int64 var_1.Length)
            (var_2 < var_3)
        else
            false
    if var_4 then
        let (var_5: char) = var_1.[int32 var_2]
        let (var_6: bool) =
            if (var_5 >= '0') then
                (var_5 <= '9')
            else
                false
        let (var_7: int64) = (var_2 + 1L)
        if var_6 then
            let (var_8: int64) = System.Convert.ToInt64(var_5)
            let (var_9: int64) = System.Convert.ToInt64('0')
            let (var_10: int64) = (var_8 - var_9)
            method_20((var_0: bool), (var_1: string), (var_10: int64), (var_7: int64))
        else
            (failwith "pint64")
    else
        (failwith "pint64")
and method_20((var_0: bool), (var_1: string), (var_2: int64), (var_3: int64)): unit =
    let (var_5: bool) =
        if (var_3 >= 0L) then
            let (var_4: int64) = (int64 var_1.Length)
            (var_3 < var_4)
        else
            false
    if var_5 then
        let (var_6: char) = var_1.[int32 var_3]
        let (var_7: bool) =
            if (var_6 >= '0') then
                (var_6 <= '9')
            else
                false
        let (var_8: int64) = (var_3 + 1L)
        if var_7 then
            let (var_9: int64) = System.Convert.ToInt64(var_6)
            let (var_10: int64) = System.Convert.ToInt64('0')
            let (var_11: int64) = (var_9 - var_10)
            let (var_12: bool) =
                if (var_2 = 922337203685477580L) then
                    (var_11 <= 7L)
                else
                    false
            let (var_13: bool) =
                if var_12 then
                    true
                else
                    (var_2 < 922337203685477580L)
            if var_13 then
                let (var_14: int64) = (var_2 * 10L)
                let (var_15: int64) = (var_14 + var_11)
                method_20((var_0: bool), (var_1: string), (var_15: int64), (var_8: int64))
            else
                (failwith "integer overflow")
        else
            let (var_16: int64) =
                if var_0 then
                    var_2
                else
                    (-var_2)
            let (var_17: int64) = 0L
            method_21((var_16: int64), (var_1: string), (var_17: int64), (var_3: int64))
    else
        let (var_18: int64) =
            if var_0 then
                var_2
            else
                (-var_2)
        let (var_19: int64) = 0L
        method_21((var_18: int64), (var_1: string), (var_19: int64), (var_3: int64))
and method_21((var_0: int64), (var_1: string), (var_2: int64), (var_3: int64)): unit =
    let (var_4: int64) = (var_2 + 1L)
    let (var_6: bool) =
        if (var_3 >= 0L) then
            let (var_5: int64) = (int64 var_1.Length)
            (var_3 < var_5)
        else
            false
    if var_6 then
        let (var_7: char) = var_1.[int32 var_3]
        let (var_9: bool) =
            if (var_7 = ' ') then
                true
            else
                if (var_7 = '\n') then
                    true
                else
                    (var_7 = '\r')
        let (var_10: int64) = (var_3 + 1L)
        if var_9 then
            method_21((var_0: int64), (var_1: string), (var_4: int64), (var_10: int64))
        else
            let (var_11: bool) = (var_0 >= 0L)
            if var_11 then
                let (var_13: ((Union0 []) [])) = Array.zeroCreate<(Union0 [])> (System.Convert.ToInt32(var_0))
                let (var_14: int64) = 0L
                method_23((var_13: ((Union0 []) [])), (var_0: int64), (var_1: string), (var_14: int64), (var_11: bool), (var_3: int64))
            else
                (failwith "n in parse array must be >= 0")
    else
        let (var_15: bool) = (var_0 >= 0L)
        if var_15 then
            let (var_16: ((Union0 []) [])) = Array.zeroCreate<(Union0 [])> (System.Convert.ToInt32(var_0))
            let (var_17: int64) = 0L
            method_23((var_16: ((Union0 []) [])), (var_0: int64), (var_1: string), (var_17: int64), (var_15: bool), (var_3: int64))
        else
            (failwith "n in parse array must be >= 0")
and method_23((var_0: ((Union0 []) [])), (var_1: int64), (var_2: string), (var_3: int64), (var_4: bool), (var_5: int64)): unit =
    let (var_6: bool) = (var_3 < var_1)
    if var_6 then
        let (var_7: int64) = (var_3 + 1L)
        if var_4 then
            let (var_8: (Union0 [])) = Array.zeroCreate<Union0> (System.Convert.ToInt32(var_1))
            let (var_9: int64) = 0L
            method_24((var_8: (Union0 [])), (var_0: ((Union0 []) [])), (var_3: int64), (var_7: int64), (var_1: int64), (var_4: bool), (var_2: string), (var_9: int64), (var_5: int64))
        else
            (failwith "n in parse array must be >= 0")
    else
        let (var_10: int64) = 0L
        let (var_11: Env1) = method_30((var_0: ((Union0 []) [])), (var_1: int64), (var_10: int64))
        let (var_12: Union2) = var_11.mem_mario
        let (var_13: Union2) = var_11.mem_princess
        match var_12 with
        | Union2Case0(var_14) ->
            let (var_15: Tuple3) = var_14.mem_1
            let (var_16: int64) = var_15.mem_0
            let (var_17: int64) = var_15.mem_1
            match var_13 with
            | Union2Case0(var_18) ->
                let (var_19: Tuple3) = var_18.mem_1
                let (var_20: int64) = var_19.mem_0
                let (var_21: int64) = var_19.mem_1
                let (var_22: int64) = (var_1 - 1L)
                let (var_23: int64) = (var_22 + 1L)
                let (var_24: int64) =
                    if (0L < var_23) then
                        var_23
                    else
                        0L
                let (var_25: int64) =
                    if (0L < var_23) then
                        var_23
                    else
                        0L
                let (var_26: int64) = (var_25 * var_24)
                let (var_27: (bool [])) = Array.zeroCreate<bool> (System.Convert.ToInt32(var_26))
                let (var_28: int64) = 0L
                let (var_29: int64) = 0L
                let (var_30: int64) = method_39((var_24: int64), (var_22: int64), (var_27: (bool [])), (var_29: int64), (var_28: int64))
                let (var_31: EnvHeap4) = ({mem_0 = (var_27: (bool [])); mem_1 = (var_24: int64); mem_2 = (var_22: int64)} : EnvHeap4)
                let (var_32: (bool [])) = var_31.mem_0
                let (var_33: int64) = var_31.mem_2
                let (var_34: int64) = var_31.mem_1
                let (var_35: bool) =
                    if (var_16 >= 0L) then
                        (var_16 <= var_33)
                    else
                        false
                if (var_35 = false) then
                    (failwith "Argument out of bounds.")
                else
                    ()
                let (var_36: int64) = (var_34 * var_16)
                let (var_37: bool) =
                    if (var_17 >= 0L) then
                        (var_17 <= var_33)
                    else
                        false
                if (var_37 = false) then
                    (failwith "Argument out of bounds.")
                else
                    ()
                let (var_38: int64) = (var_36 + var_17)
                var_32.[int32 var_38] <- true
                let (var_39: string) = "UP"
                let (var_40: string) = "DOWN"
                let (var_41: string) = "LEFT"
                let (var_42: string) = "RIGHT"
                let (var_43: (Tuple5 [])) = Array.zeroCreate<Tuple5> (System.Convert.ToInt32(1L))
                var_43.[int32 0L] <- Tuple5(Tuple3(var_16, var_17), Rec6Case0)
                let (var_44: (Union7 ref)) = (ref Union7Case1)
                method_49((var_31: EnvHeap4), (var_1: int64), (var_21: int64), (var_20: int64), (var_39: string), (var_40: string), (var_41: string), (var_42: string), (var_44: (Union7 ref)), (var_43: (Tuple5 [])))
            | Union2Case1 ->
                (failwith "Current position not found.")
        | Union2Case1 ->
            (failwith "Current position not found.")
and method_24((var_0: (Union0 [])), (var_1: ((Union0 []) [])), (var_2: int64), (var_3: int64), (var_4: int64), (var_5: bool), (var_6: string), (var_7: int64), (var_8: int64)): unit =
    let (var_9: bool) = (var_7 < var_4)
    if var_9 then
        let (var_10: int64) = (var_7 + 1L)
        let (var_12: bool) =
            if (var_8 >= 0L) then
                let (var_11: int64) = (int64 var_6.Length)
                (var_8 < var_11)
            else
                false
        if var_12 then
            let (var_13: char) = var_6.[int32 var_8]
            let (var_14: bool) = ('-' = var_13)
            let (var_15: int64) = (var_8 + 1L)
            if var_14 then
                var_0.[int32 var_7] <- Union0Case0
                method_24((var_0: (Union0 [])), (var_1: ((Union0 []) [])), (var_2: int64), (var_3: int64), (var_4: int64), (var_5: bool), (var_6: string), (var_10: int64), (var_15: int64))
            else
                let (var_17: bool) =
                    if (var_8 >= 0L) then
                        let (var_16: int64) = (int64 var_6.Length)
                        (var_8 < var_16)
                    else
                        false
                if var_17 then
                    let (var_18: bool) = ('p' = var_13)
                    if var_18 then
                        var_0.[int32 var_7] <- Union0Case2
                        method_24((var_0: (Union0 [])), (var_1: ((Union0 []) [])), (var_2: int64), (var_3: int64), (var_4: int64), (var_5: bool), (var_6: string), (var_10: int64), (var_15: int64))
                    else
                        let (var_20: bool) =
                            if (var_8 >= 0L) then
                                let (var_19: int64) = (int64 var_6.Length)
                                (var_8 < var_19)
                            else
                                false
                        if var_20 then
                            let (var_21: bool) = ('m' = var_13)
                            if var_21 then
                                var_0.[int32 var_7] <- Union0Case1
                                method_24((var_0: (Union0 [])), (var_1: ((Union0 []) [])), (var_2: int64), (var_3: int64), (var_4: int64), (var_5: bool), (var_6: string), (var_10: int64), (var_15: int64))
                            else
                                (failwith "char")
                        else
                            (failwith "string index out of bounds")
                else
                    let (var_23: bool) =
                        if (var_8 >= 0L) then
                            let (var_22: int64) = (int64 var_6.Length)
                            (var_8 < var_22)
                        else
                            false
                    if var_23 then
                        let (var_24: bool) = ('m' = var_13)
                        if var_24 then
                            var_0.[int32 var_7] <- Union0Case1
                            method_24((var_0: (Union0 [])), (var_1: ((Union0 []) [])), (var_2: int64), (var_3: int64), (var_4: int64), (var_5: bool), (var_6: string), (var_10: int64), (var_15: int64))
                        else
                            (failwith "char")
                    else
                        (failwith "string index out of bounds")
        else
            let (var_26: bool) =
                if (var_8 >= 0L) then
                    let (var_25: int64) = (int64 var_6.Length)
                    (var_8 < var_25)
                else
                    false
            if var_26 then
                let (var_27: char) = var_6.[int32 var_8]
                let (var_28: bool) = ('p' = var_27)
                let (var_29: int64) = (var_8 + 1L)
                if var_28 then
                    var_0.[int32 var_7] <- Union0Case2
                    method_24((var_0: (Union0 [])), (var_1: ((Union0 []) [])), (var_2: int64), (var_3: int64), (var_4: int64), (var_5: bool), (var_6: string), (var_10: int64), (var_29: int64))
                else
                    let (var_31: bool) =
                        if (var_8 >= 0L) then
                            let (var_30: int64) = (int64 var_6.Length)
                            (var_8 < var_30)
                        else
                            false
                    if var_31 then
                        let (var_32: bool) = ('m' = var_27)
                        if var_32 then
                            var_0.[int32 var_7] <- Union0Case1
                            method_24((var_0: (Union0 [])), (var_1: ((Union0 []) [])), (var_2: int64), (var_3: int64), (var_4: int64), (var_5: bool), (var_6: string), (var_10: int64), (var_29: int64))
                        else
                            (failwith "char")
                    else
                        (failwith "string index out of bounds")
            else
                let (var_34: bool) =
                    if (var_8 >= 0L) then
                        let (var_33: int64) = (int64 var_6.Length)
                        (var_8 < var_33)
                    else
                        false
                if var_34 then
                    let (var_35: char) = var_6.[int32 var_8]
                    let (var_36: bool) = ('m' = var_35)
                    let (var_37: int64) = (var_8 + 1L)
                    if var_36 then
                        var_0.[int32 var_7] <- Union0Case1
                        method_24((var_0: (Union0 [])), (var_1: ((Union0 []) [])), (var_2: int64), (var_3: int64), (var_4: int64), (var_5: bool), (var_6: string), (var_10: int64), (var_37: int64))
                    else
                        (failwith "char")
                else
                    (failwith "string index out of bounds")
    else
        let (var_38: int64) = 0L
        method_25((var_0: (Union0 [])), (var_1: ((Union0 []) [])), (var_2: int64), (var_3: int64), (var_4: int64), (var_5: bool), (var_6: string), (var_38: int64), (var_8: int64))
and method_25((var_0: (Union0 [])), (var_1: ((Union0 []) [])), (var_2: int64), (var_3: int64), (var_4: int64), (var_5: bool), (var_6: string), (var_7: int64), (var_8: int64)): unit =
    let (var_9: int64) = (var_7 + 1L)
    let (var_11: bool) =
        if (var_8 >= 0L) then
            let (var_10: int64) = (int64 var_6.Length)
            (var_8 < var_10)
        else
            false
    if var_11 then
        let (var_12: char) = var_6.[int32 var_8]
        let (var_14: bool) =
            if (var_12 = ' ') then
                true
            else
                if (var_12 = '\n') then
                    true
                else
                    (var_12 = '\r')
        let (var_15: int64) = (var_8 + 1L)
        if var_14 then
            method_25((var_0: (Union0 [])), (var_1: ((Union0 []) [])), (var_2: int64), (var_3: int64), (var_4: int64), (var_5: bool), (var_6: string), (var_9: int64), (var_15: int64))
        else
            var_1.[int32 var_2] <- var_0
            method_23((var_1: ((Union0 []) [])), (var_4: int64), (var_6: string), (var_3: int64), (var_5: bool), (var_8: int64))
    else
        var_1.[int32 var_2] <- var_0
        method_23((var_1: ((Union0 []) [])), (var_4: int64), (var_6: string), (var_3: int64), (var_5: bool), (var_8: int64))
and method_30((var_0: ((Union0 []) [])), (var_1: int64), (var_2: int64)): Env1 =
    if (var_2 < var_1) then
        let (var_3: int64) = 0L
        method_31((var_0: ((Union0 []) [])), (var_2: int64), (var_1: int64), (var_3: int64))
    else
        (Env1(Union2Case1, Union2Case1))
and method_31((var_0: ((Union0 []) [])), (var_1: int64), (var_2: int64), (var_3: int64)): Env1 =
    if (var_3 < var_2) then
        let (var_4: (Union0 [])) = var_0.[int32 var_1]
        let (var_5: Union0) = var_4.[int32 var_3]
        match var_5 with
        | Union0Case0 ->
            let (var_6: int64) = (var_3 + 1L)
            method_31((var_0: ((Union0 []) [])), (var_1: int64), (var_2: int64), (var_6: int64))
        | Union0Case1 ->
            let (var_8: int64) = (var_3 + 1L)
            method_32((var_0: ((Union0 []) [])), (var_1: int64), (var_2: int64), (var_8: int64), (var_3: int64))
        | Union0Case2 ->
            let (var_10: int64) = (var_3 + 1L)
            method_35((var_0: ((Union0 []) [])), (var_1: int64), (var_2: int64), (var_10: int64), (var_3: int64))
    else
        let (var_13: int64) = (var_1 + 1L)
        method_30((var_0: ((Union0 []) [])), (var_2: int64), (var_13: int64))
and method_32((var_0: ((Union0 []) [])), (var_1: int64), (var_2: int64), (var_3: int64), (var_4: int64)): Env1 =
    if (var_3 < var_2) then
        let (var_5: (Union0 [])) = var_0.[int32 var_1]
        let (var_6: Union0) = var_5.[int32 var_3]
        match var_6 with
        | Union0Case0 ->
            let (var_7: int64) = (var_3 + 1L)
            method_32((var_0: ((Union0 []) [])), (var_1: int64), (var_2: int64), (var_7: int64), (var_4: int64))
        | Union0Case1 ->
            let (var_9: int64) = (var_3 + 1L)
            method_32((var_0: ((Union0 []) [])), (var_1: int64), (var_2: int64), (var_9: int64), (var_3: int64))
        | Union0Case2 ->
            (Env1((Union2Case0(Tuple8(Tuple3(var_1, var_4)))), (Union2Case0(Tuple8(Tuple3(var_1, var_3))))))
    else
        let (var_12: int64) = (var_1 + 1L)
        method_33((var_0: ((Union0 []) [])), (var_2: int64), (var_12: int64), (var_1: int64), (var_4: int64))
and method_33((var_0: ((Union0 []) [])), (var_1: int64), (var_2: int64), (var_3: int64), (var_4: int64)): Env1 =
    if (var_2 < var_1) then
        let (var_5: int64) = 0L
        method_34((var_0: ((Union0 []) [])), (var_2: int64), (var_1: int64), (var_5: int64), (var_3: int64), (var_4: int64))
    else
        (Env1((Union2Case0(Tuple8(Tuple3(var_3, var_4)))), Union2Case1))
and method_34((var_0: ((Union0 []) [])), (var_1: int64), (var_2: int64), (var_3: int64), (var_4: int64), (var_5: int64)): Env1 =
    if (var_3 < var_2) then
        let (var_6: (Union0 [])) = var_0.[int32 var_1]
        let (var_7: Union0) = var_6.[int32 var_3]
        match var_7 with
        | Union0Case0 ->
            let (var_8: int64) = (var_3 + 1L)
            method_34((var_0: ((Union0 []) [])), (var_1: int64), (var_2: int64), (var_8: int64), (var_4: int64), (var_5: int64))
        | Union0Case1 ->
            let (var_10: int64) = (var_3 + 1L)
            method_32((var_0: ((Union0 []) [])), (var_1: int64), (var_2: int64), (var_10: int64), (var_3: int64))
        | Union0Case2 ->
            (Env1((Union2Case0(Tuple8(Tuple3(var_4, var_5)))), (Union2Case0(Tuple8(Tuple3(var_1, var_3))))))
    else
        let (var_13: int64) = (var_1 + 1L)
        method_33((var_0: ((Union0 []) [])), (var_2: int64), (var_13: int64), (var_4: int64), (var_5: int64))
and method_35((var_0: ((Union0 []) [])), (var_1: int64), (var_2: int64), (var_3: int64), (var_4: int64)): Env1 =
    if (var_3 < var_2) then
        let (var_5: (Union0 [])) = var_0.[int32 var_1]
        let (var_6: Union0) = var_5.[int32 var_3]
        match var_6 with
        | Union0Case0 ->
            let (var_7: int64) = (var_3 + 1L)
            method_35((var_0: ((Union0 []) [])), (var_1: int64), (var_2: int64), (var_7: int64), (var_4: int64))
        | Union0Case1 ->
            (Env1((Union2Case0(Tuple8(Tuple3(var_1, var_3)))), (Union2Case0(Tuple8(Tuple3(var_1, var_4))))))
        | Union0Case2 ->
            let (var_9: int64) = (var_3 + 1L)
            method_35((var_0: ((Union0 []) [])), (var_1: int64), (var_2: int64), (var_9: int64), (var_3: int64))
    else
        let (var_12: int64) = (var_1 + 1L)
        method_36((var_0: ((Union0 []) [])), (var_2: int64), (var_12: int64), (var_1: int64), (var_4: int64))
and method_36((var_0: ((Union0 []) [])), (var_1: int64), (var_2: int64), (var_3: int64), (var_4: int64)): Env1 =
    if (var_2 < var_1) then
        let (var_5: int64) = 0L
        method_37((var_0: ((Union0 []) [])), (var_2: int64), (var_1: int64), (var_5: int64), (var_3: int64), (var_4: int64))
    else
        (Env1(Union2Case1, (Union2Case0(Tuple8(Tuple3(var_3, var_4))))))
and method_37((var_0: ((Union0 []) [])), (var_1: int64), (var_2: int64), (var_3: int64), (var_4: int64), (var_5: int64)): Env1 =
    if (var_3 < var_2) then
        let (var_6: (Union0 [])) = var_0.[int32 var_1]
        let (var_7: Union0) = var_6.[int32 var_3]
        match var_7 with
        | Union0Case0 ->
            let (var_8: int64) = (var_3 + 1L)
            method_37((var_0: ((Union0 []) [])), (var_1: int64), (var_2: int64), (var_8: int64), (var_4: int64), (var_5: int64))
        | Union0Case1 ->
            (Env1((Union2Case0(Tuple8(Tuple3(var_1, var_3)))), (Union2Case0(Tuple8(Tuple3(var_4, var_5))))))
        | Union0Case2 ->
            let (var_10: int64) = (var_3 + 1L)
            method_35((var_0: ((Union0 []) [])), (var_1: int64), (var_2: int64), (var_10: int64), (var_3: int64))
    else
        let (var_13: int64) = (var_1 + 1L)
        method_36((var_0: ((Union0 []) [])), (var_2: int64), (var_13: int64), (var_4: int64), (var_5: int64))
and method_39((var_0: int64), (var_1: int64), (var_2: (bool [])), (var_3: int64), (var_4: int64)): int64 =
    if (var_3 <= var_1) then
        let (var_5: int64) = 0L
        let (var_6: int64) = method_40((var_3: int64), (var_2: (bool [])), (var_1: int64), (var_5: int64), (var_4: int64))
        let (var_7: int64) = (var_4 + var_0)
        let (var_8: int64) = (var_3 + 1L)
        method_39((var_0: int64), (var_1: int64), (var_2: (bool [])), (var_8: int64), (var_7: int64))
    else
        var_4
and method_40((var_0: int64), (var_1: (bool [])), (var_2: int64), (var_3: int64), (var_4: int64)): int64 =
    if (var_3 <= var_2) then
        var_1.[int32 var_4] <- false
        let (var_5: int64) = (var_4 + 1L)
        let (var_6: int64) = (var_3 + 1L)
        method_40((var_0: int64), (var_1: (bool [])), (var_2: int64), (var_6: int64), (var_5: int64))
    else
        var_4
and method_49((var_0: EnvHeap4), (var_1: int64), (var_2: int64), (var_3: int64), (var_4: string), (var_5: string), (var_6: string), (var_7: string), (var_8: (Union7 ref)), (var_9: (Tuple5 []))): unit =
    let (var_10: int64) = var_9.LongLength
    let (var_11: bool) = (var_10 >= 0L)
    if (var_11 = false) then
        (failwith "The input to init needs to be greater or equal than 0.")
    else
        ()
    let (var_117: ((Tuple5 []) [])) = Array.zeroCreate<(Tuple5 [])> (System.Convert.ToInt32(var_10))
    let (var_118: int64) = 0L
    method_51((var_117: ((Tuple5 []) [])), (var_9: (Tuple5 [])), (var_0: EnvHeap4), (var_1: int64), (var_2: int64), (var_3: int64), (var_4: string), (var_5: string), (var_6: string), (var_7: string), (var_8: (Union7 ref)), (var_10: int64), (var_118: int64))
    let (var_119: int64) = 0L
    let (var_120: int64) = var_117.LongLength
    let (var_121: int64) = 0L
    let (var_122: int64) = method_52((var_117: ((Tuple5 []) [])), (var_120: int64), (var_121: int64), (var_119: int64))
    let (var_123: (Tuple5 [])) = Array.zeroCreate<Tuple5> (System.Convert.ToInt32(var_122))
    let (var_124: int64) = 0L
    let (var_125: int64) = var_117.LongLength
    let (var_126: int64) = 0L
    let (var_127: int64) = method_53((var_117: ((Tuple5 []) [])), (var_123: (Tuple5 [])), (var_125: int64), (var_126: int64), (var_124: int64))
    let (var_128: Union7) = (!var_8)
    match var_128 with
    | Union7Case0(var_129) ->
        let (var_130: Tuple5) = var_129.mem_1
        let (var_131: Tuple3) = var_130.mem_0
        let (var_132: Rec6) = var_130.mem_1
        method_55((var_132: Rec6))
    | Union7Case1 ->
        method_49((var_0: EnvHeap4), (var_1: int64), (var_2: int64), (var_3: int64), (var_4: string), (var_5: string), (var_6: string), (var_7: string), (var_8: (Union7 ref)), (var_123: (Tuple5 [])))
and method_51((var_0: ((Tuple5 []) [])), (var_1: (Tuple5 [])), (var_2: EnvHeap4), (var_3: int64), (var_4: int64), (var_5: int64), (var_6: string), (var_7: string), (var_8: string), (var_9: string), (var_10: (Union7 ref)), (var_11: int64), (var_12: int64)): unit =
    if (var_12 < var_11) then
        let (var_13: Tuple5) = var_1.[int32 var_12]
        let (var_14: Tuple3) = var_13.mem_0
        let (var_15: Rec6) = var_13.mem_1
        let (var_16: int64) = var_14.mem_0
        let (var_17: int64) = var_14.mem_1
        let (var_18: int64) = (var_16 - 1L)
        let (var_19: bool) =
            if (var_18 >= 0L) then
                (var_18 < var_3)
            else
                false
        let (var_21: bool) =
            if var_19 then
                if (var_17 >= 0L) then
                    (var_17 < var_3)
                else
                    false
            else
                false
        let (var_30: bool) =
            if var_21 then
                let (var_22: (bool [])) = var_2.mem_0
                let (var_23: int64) = var_2.mem_2
                let (var_24: int64) = var_2.mem_1
                let (var_25: bool) =
                    if (var_18 >= 0L) then
                        (var_18 <= var_23)
                    else
                        false
                if (var_25 = false) then
                    (failwith "Argument out of bounds.")
                else
                    ()
                let (var_26: int64) = (var_24 * var_18)
                let (var_27: bool) =
                    if (var_17 >= 0L) then
                        (var_17 <= var_23)
                    else
                        false
                if (var_27 = false) then
                    (failwith "Argument out of bounds.")
                else
                    ()
                let (var_28: int64) = (var_26 + var_17)
                let (var_29: bool) = var_22.[int32 var_28]
                (var_29 = false)
            else
                false
        let (var_39: bool) =
            if var_30 then
                let (var_31: bool) =
                    if (var_18 = var_5) then
                        (var_17 = var_4)
                    else
                        false
                if var_31 then
                    var_10 := (Union7Case0(Tuple10(Tuple5(Tuple3(var_18, var_17), (Rec6Case1(Tuple9(var_6, var_15)))))))
                else
                    ()
                let (var_32: (bool [])) = var_2.mem_0
                let (var_33: int64) = var_2.mem_2
                let (var_34: int64) = var_2.mem_1
                let (var_35: bool) =
                    if (var_18 >= 0L) then
                        (var_18 <= var_33)
                    else
                        false
                if (var_35 = false) then
                    (failwith "Argument out of bounds.")
                else
                    ()
                let (var_36: int64) = (var_34 * var_18)
                let (var_37: bool) =
                    if (var_17 >= 0L) then
                        (var_17 <= var_33)
                    else
                        false
                if (var_37 = false) then
                    (failwith "Argument out of bounds.")
                else
                    ()
                let (var_38: int64) = (var_36 + var_17)
                var_32.[int32 var_38] <- true
                true
            else
                false
        let (var_40: int64) = (var_16 + 1L)
        let (var_41: bool) =
            if (var_40 >= 0L) then
                (var_40 < var_3)
            else
                false
        let (var_43: bool) =
            if var_41 then
                if (var_17 >= 0L) then
                    (var_17 < var_3)
                else
                    false
            else
                false
        let (var_52: bool) =
            if var_43 then
                let (var_44: (bool [])) = var_2.mem_0
                let (var_45: int64) = var_2.mem_2
                let (var_46: int64) = var_2.mem_1
                let (var_47: bool) =
                    if (var_40 >= 0L) then
                        (var_40 <= var_45)
                    else
                        false
                if (var_47 = false) then
                    (failwith "Argument out of bounds.")
                else
                    ()
                let (var_48: int64) = (var_46 * var_40)
                let (var_49: bool) =
                    if (var_17 >= 0L) then
                        (var_17 <= var_45)
                    else
                        false
                if (var_49 = false) then
                    (failwith "Argument out of bounds.")
                else
                    ()
                let (var_50: int64) = (var_48 + var_17)
                let (var_51: bool) = var_44.[int32 var_50]
                (var_51 = false)
            else
                false
        let (var_61: bool) =
            if var_52 then
                let (var_53: bool) =
                    if (var_40 = var_5) then
                        (var_17 = var_4)
                    else
                        false
                if var_53 then
                    var_10 := (Union7Case0(Tuple10(Tuple5(Tuple3(var_40, var_17), (Rec6Case1(Tuple9(var_7, var_15)))))))
                else
                    ()
                let (var_54: (bool [])) = var_2.mem_0
                let (var_55: int64) = var_2.mem_2
                let (var_56: int64) = var_2.mem_1
                let (var_57: bool) =
                    if (var_40 >= 0L) then
                        (var_40 <= var_55)
                    else
                        false
                if (var_57 = false) then
                    (failwith "Argument out of bounds.")
                else
                    ()
                let (var_58: int64) = (var_56 * var_40)
                let (var_59: bool) =
                    if (var_17 >= 0L) then
                        (var_17 <= var_55)
                    else
                        false
                if (var_59 = false) then
                    (failwith "Argument out of bounds.")
                else
                    ()
                let (var_60: int64) = (var_58 + var_17)
                var_54.[int32 var_60] <- true
                true
            else
                false
        let (var_62: int64) = (var_17 - 1L)
        let (var_63: bool) =
            if (var_16 >= 0L) then
                (var_16 < var_3)
            else
                false
        let (var_65: bool) =
            if var_63 then
                if (var_62 >= 0L) then
                    (var_62 < var_3)
                else
                    false
            else
                false
        let (var_74: bool) =
            if var_65 then
                let (var_66: (bool [])) = var_2.mem_0
                let (var_67: int64) = var_2.mem_2
                let (var_68: int64) = var_2.mem_1
                let (var_69: bool) =
                    if (var_16 >= 0L) then
                        (var_16 <= var_67)
                    else
                        false
                if (var_69 = false) then
                    (failwith "Argument out of bounds.")
                else
                    ()
                let (var_70: int64) = (var_68 * var_16)
                let (var_71: bool) =
                    if (var_62 >= 0L) then
                        (var_62 <= var_67)
                    else
                        false
                if (var_71 = false) then
                    (failwith "Argument out of bounds.")
                else
                    ()
                let (var_72: int64) = (var_70 + var_62)
                let (var_73: bool) = var_66.[int32 var_72]
                (var_73 = false)
            else
                false
        let (var_83: bool) =
            if var_74 then
                let (var_75: bool) =
                    if (var_16 = var_5) then
                        (var_62 = var_4)
                    else
                        false
                if var_75 then
                    var_10 := (Union7Case0(Tuple10(Tuple5(Tuple3(var_16, var_62), (Rec6Case1(Tuple9(var_8, var_15)))))))
                else
                    ()
                let (var_76: (bool [])) = var_2.mem_0
                let (var_77: int64) = var_2.mem_2
                let (var_78: int64) = var_2.mem_1
                let (var_79: bool) =
                    if (var_16 >= 0L) then
                        (var_16 <= var_77)
                    else
                        false
                if (var_79 = false) then
                    (failwith "Argument out of bounds.")
                else
                    ()
                let (var_80: int64) = (var_78 * var_16)
                let (var_81: bool) =
                    if (var_62 >= 0L) then
                        (var_62 <= var_77)
                    else
                        false
                if (var_81 = false) then
                    (failwith "Argument out of bounds.")
                else
                    ()
                let (var_82: int64) = (var_80 + var_62)
                var_76.[int32 var_82] <- true
                true
            else
                false
        let (var_84: int64) = (var_17 + 1L)
        let (var_85: bool) =
            if (var_16 >= 0L) then
                (var_16 < var_3)
            else
                false
        let (var_87: bool) =
            if var_85 then
                if (var_84 >= 0L) then
                    (var_84 < var_3)
                else
                    false
            else
                false
        let (var_96: bool) =
            if var_87 then
                let (var_88: (bool [])) = var_2.mem_0
                let (var_89: int64) = var_2.mem_2
                let (var_90: int64) = var_2.mem_1
                let (var_91: bool) =
                    if (var_16 >= 0L) then
                        (var_16 <= var_89)
                    else
                        false
                if (var_91 = false) then
                    (failwith "Argument out of bounds.")
                else
                    ()
                let (var_92: int64) = (var_90 * var_16)
                let (var_93: bool) =
                    if (var_84 >= 0L) then
                        (var_84 <= var_89)
                    else
                        false
                if (var_93 = false) then
                    (failwith "Argument out of bounds.")
                else
                    ()
                let (var_94: int64) = (var_92 + var_84)
                let (var_95: bool) = var_88.[int32 var_94]
                (var_95 = false)
            else
                false
        let (var_105: bool) =
            if var_96 then
                let (var_97: bool) =
                    if (var_16 = var_5) then
                        (var_84 = var_4)
                    else
                        false
                if var_97 then
                    var_10 := (Union7Case0(Tuple10(Tuple5(Tuple3(var_16, var_84), (Rec6Case1(Tuple9(var_9, var_15)))))))
                else
                    ()
                let (var_98: (bool [])) = var_2.mem_0
                let (var_99: int64) = var_2.mem_2
                let (var_100: int64) = var_2.mem_1
                let (var_101: bool) =
                    if (var_16 >= 0L) then
                        (var_16 <= var_99)
                    else
                        false
                if (var_101 = false) then
                    (failwith "Argument out of bounds.")
                else
                    ()
                let (var_102: int64) = (var_100 * var_16)
                let (var_103: bool) =
                    if (var_84 >= 0L) then
                        (var_84 <= var_99)
                    else
                        false
                if (var_103 = false) then
                    (failwith "Argument out of bounds.")
                else
                    ()
                let (var_104: int64) = (var_102 + var_84)
                var_98.[int32 var_104] <- true
                true
            else
                false
        let (var_106: int64) =
            if var_39 then
                1L
            else
                0L
        let (var_107: int64) =
            if var_61 then
                1L
            else
                0L
        let (var_108: int64) = (var_106 + var_107)
        let (var_109: int64) =
            if var_83 then
                1L
            else
                0L
        let (var_110: int64) = (var_108 + var_109)
        let (var_111: int64) =
            if var_105 then
                1L
            else
                0L
        let (var_112: int64) = (var_110 + var_111)
        let (var_113: (Tuple5 [])) = Array.zeroCreate<Tuple5> (System.Convert.ToInt32(var_112))
        let (var_114: int64) =
            if var_39 then
                var_113.[int32 0L] <- Tuple5(Tuple3(var_18, var_17), (Rec6Case1(Tuple9(var_6, var_15))))
                1L
            else
                0L
        let (var_115: int64) =
            if var_61 then
                var_113.[int32 var_114] <- Tuple5(Tuple3(var_40, var_17), (Rec6Case1(Tuple9(var_7, var_15))))
                (var_114 + 1L)
            else
                var_114
        let (var_116: int64) =
            if var_83 then
                var_113.[int32 var_115] <- Tuple5(Tuple3(var_16, var_62), (Rec6Case1(Tuple9(var_8, var_15))))
                (var_115 + 1L)
            else
                var_115
        let (var_117: int64) =
            if var_105 then
                var_113.[int32 var_116] <- Tuple5(Tuple3(var_16, var_84), (Rec6Case1(Tuple9(var_9, var_15))))
                (var_116 + 1L)
            else
                var_116
        var_0.[int32 var_12] <- var_113
        let (var_118: int64) = (var_12 + 1L)
        method_51((var_0: ((Tuple5 []) [])), (var_1: (Tuple5 [])), (var_2: EnvHeap4), (var_3: int64), (var_4: int64), (var_5: int64), (var_6: string), (var_7: string), (var_8: string), (var_9: string), (var_10: (Union7 ref)), (var_11: int64), (var_118: int64))
    else
        ()
and method_52((var_0: ((Tuple5 []) [])), (var_1: int64), (var_2: int64), (var_3: int64)): int64 =
    if (var_2 < var_1) then
        let (var_4: (Tuple5 [])) = var_0.[int32 var_2]
        let (var_5: int64) = var_4.LongLength
        let (var_6: int64) = (var_3 + var_5)
        let (var_7: int64) = (var_2 + 1L)
        method_52((var_0: ((Tuple5 []) [])), (var_1: int64), (var_7: int64), (var_6: int64))
    else
        var_3
and method_53((var_0: ((Tuple5 []) [])), (var_1: (Tuple5 [])), (var_2: int64), (var_3: int64), (var_4: int64)): int64 =
    if (var_3 < var_2) then
        let (var_5: (Tuple5 [])) = var_0.[int32 var_3]
        let (var_6: int64) = var_5.LongLength
        let (var_7: int64) = 0L
        let (var_8: int64) = method_54((var_5: (Tuple5 [])), (var_1: (Tuple5 [])), (var_6: int64), (var_7: int64), (var_4: int64))
        let (var_9: int64) = (var_3 + 1L)
        method_53((var_0: ((Tuple5 []) [])), (var_1: (Tuple5 [])), (var_2: int64), (var_9: int64), (var_8: int64))
    else
        var_4
and method_54((var_0: (Tuple5 [])), (var_1: (Tuple5 [])), (var_2: int64), (var_3: int64), (var_4: int64)): int64 =
    if (var_3 < var_2) then
        let (var_5: Tuple5) = var_0.[int32 var_3]
        let (var_6: Tuple3) = var_5.mem_0
        let (var_7: Rec6) = var_5.mem_1
        var_1.[int32 var_4] <- Tuple5(var_6, var_7)
        let (var_8: int64) = (var_4 + 1L)
        let (var_9: int64) = (var_3 + 1L)
        method_54((var_0: (Tuple5 [])), (var_1: (Tuple5 [])), (var_2: int64), (var_9: int64), (var_8: int64))
    else
        var_4
and method_55((var_0: Rec6)): unit =
    match var_0 with
    | Rec6Case0 ->
        ()
    | Rec6Case1(var_1) ->
        let (var_2: string) = var_1.mem_0
        let (var_3: Rec6) = var_1.mem_1
        method_55((var_3: Rec6))
        System.Console.WriteLine(var_2)
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
    let (var_7: bool) = ('-' = var_6)
    let (var_8: int64) = (var_3 + 1L)
    if var_7 then
        let (var_9: bool) = false
        method_19((var_9: bool), (var_2: string), (var_8: int64))
    else
        let (var_10: bool) = true
        method_19((var_10: bool), (var_2: string), (var_3: int64))
else
    let (var_11: bool) = true
    method_19((var_11: bool), (var_2: string), (var_3: int64))

