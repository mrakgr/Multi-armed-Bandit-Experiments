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
    | Union2Case0 of Tuple7
    | Union2Case1
and Tuple3 =
    struct
    val mem_0: int64
    val mem_1: int64
    new(arg_mem_0, arg_mem_1) = {mem_0 = arg_mem_0; mem_1 = arg_mem_1}
    end
and Tuple4 =
    struct
    val mem_0: Tuple3
    val mem_1: Rec5
    new(arg_mem_0, arg_mem_1) = {mem_0 = arg_mem_0; mem_1 = arg_mem_1}
    end
and Rec5 =
    | Rec5Case0
    | Rec5Case1 of Tuple8
and Union6 =
    | Union6Case0 of Tuple9
    | Union6Case1
and Tuple7 =
    struct
    val mem_1: Tuple3
    new(arg_mem_1) = {mem_1 = arg_mem_1}
    end
and Tuple8 =
    struct
    val mem_0: string
    val mem_1: Rec5
    new(arg_mem_0, arg_mem_1) = {mem_0 = arg_mem_0; mem_1 = arg_mem_1}
    end
and Tuple9 =
    struct
    val mem_1: Tuple4
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
            let (var_11: int64) = (0L + var_10)
            method_20((var_0: bool), (var_1: string), (var_11: int64), (var_7: int64))
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
        if var_4 then
            let (var_7: (Union0 [])) = Array.zeroCreate<Union0> (System.Convert.ToInt32(var_1))
            let (var_8: int64) = 0L
            method_24((var_7: (Union0 [])), (var_0: ((Union0 []) [])), (var_3: int64), (var_1: int64), (var_4: bool), (var_2: string), (var_8: int64), (var_5: int64))
        else
            (failwith "n in parse array must be >= 0")
    else
        let (var_9: int64) = 0L
        let (var_10: Env1) = method_30((var_0: ((Union0 []) [])), (var_1: int64), (var_9: int64))
        let (var_11: Union2) = var_10.mem_mario
        let (var_12: Union2) = var_10.mem_princess
        match var_11 with
        | Union2Case0(var_13) ->
            let (var_15: Tuple3) = var_13.mem_1
            let (var_16: int64) = var_15.mem_0
            let (var_17: int64) = var_15.mem_1
            match var_12 with
            | Union2Case0(var_18) ->
                let (var_20: Tuple3) = var_18.mem_1
                let (var_21: int64) = var_20.mem_0
                let (var_22: int64) = var_20.mem_1
                let (var_23: int64) = (var_1 * 1L)
                let (var_24: int64) = (var_1 * var_23)
                let (var_25: (bool [])) = Array.zeroCreate<bool> (System.Convert.ToInt32(var_24))
                let (var_26: int64) = 0L
                let (var_27: int64) = 0L
                let (var_28: int64) = method_39((var_23: int64), (var_1: int64), (var_25: (bool [])), (var_27: int64), (var_26: int64))
                let (var_29: bool) =
                    if (var_16 >= 0L) then
                        (var_16 < var_1)
                    else
                        false
                if (var_29 = false) then
                    (failwith "Argument out of bounds.")
                else
                    ()
                let (var_30: int64) = (var_23 * var_16)
                let (var_31: int64) = (0L + var_30)
                let (var_32: bool) =
                    if (var_17 >= 0L) then
                        (var_17 < var_1)
                    else
                        false
                if (var_32 = false) then
                    (failwith "Argument out of bounds.")
                else
                    ()
                let (var_33: int64) = (1L * var_17)
                let (var_34: int64) = (var_31 + var_33)
                var_25.[int32 var_34] <- true
                let (var_35: string) = "UP"
                let (var_36: string) = "DOWN"
                let (var_37: string) = "LEFT"
                let (var_38: string) = "RIGHT"
                let (var_39: (Tuple4 [])) = Array.zeroCreate<Tuple4> (System.Convert.ToInt32(1L))
                var_39.[int32 0L] <- Tuple4(Tuple3(var_16, var_17), Rec5Case0)
                let (var_40: (Union6 ref)) = (ref Union6Case1)
                method_49((var_25: (bool [])), (var_23: int64), (var_1: int64), (var_22: int64), (var_21: int64), (var_35: string), (var_36: string), (var_37: string), (var_38: string), (var_40: (Union6 ref)), (var_39: (Tuple4 [])))
            | Union2Case1 ->
                (failwith "Current position not found.")
        | Union2Case1 ->
            (failwith "Current position not found.")
and method_24((var_0: (Union0 [])), (var_1: ((Union0 []) [])), (var_2: int64), (var_3: int64), (var_4: bool), (var_5: string), (var_6: int64), (var_7: int64)): unit =
    let (var_8: bool) = (var_6 < var_3)
    if var_8 then
        let (var_10: bool) =
            if (var_7 >= 0L) then
                let (var_9: int64) = (int64 var_5.Length)
                (var_7 < var_9)
            else
                false
        if var_10 then
            let (var_11: char) = var_5.[int32 var_7]
            let (var_12: bool) = ('-' = var_11)
            let (var_13: int64) = (var_7 + 1L)
            if var_12 then
                var_0.[int32 var_6] <- Union0Case0
                let (var_14: int64) = (var_6 + 1L)
                method_24((var_0: (Union0 [])), (var_1: ((Union0 []) [])), (var_2: int64), (var_3: int64), (var_4: bool), (var_5: string), (var_14: int64), (var_13: int64))
            else
                let (var_16: bool) =
                    if (var_7 >= 0L) then
                        let (var_15: int64) = (int64 var_5.Length)
                        (var_7 < var_15)
                    else
                        false
                if var_16 then
                    let (var_17: bool) = ('p' = var_11)
                    if var_17 then
                        var_0.[int32 var_6] <- Union0Case2
                        let (var_18: int64) = (var_6 + 1L)
                        method_24((var_0: (Union0 [])), (var_1: ((Union0 []) [])), (var_2: int64), (var_3: int64), (var_4: bool), (var_5: string), (var_18: int64), (var_13: int64))
                    else
                        let (var_20: bool) =
                            if (var_7 >= 0L) then
                                let (var_19: int64) = (int64 var_5.Length)
                                (var_7 < var_19)
                            else
                                false
                        if var_20 then
                            let (var_21: bool) = ('m' = var_11)
                            if var_21 then
                                var_0.[int32 var_6] <- Union0Case1
                                let (var_22: int64) = (var_6 + 1L)
                                method_24((var_0: (Union0 [])), (var_1: ((Union0 []) [])), (var_2: int64), (var_3: int64), (var_4: bool), (var_5: string), (var_22: int64), (var_13: int64))
                            else
                                (failwith "char")
                        else
                            (failwith "string index out of bounds")
                else
                    let (var_24: bool) =
                        if (var_7 >= 0L) then
                            let (var_23: int64) = (int64 var_5.Length)
                            (var_7 < var_23)
                        else
                            false
                    if var_24 then
                        let (var_25: bool) = ('m' = var_11)
                        if var_25 then
                            var_0.[int32 var_6] <- Union0Case1
                            let (var_26: int64) = (var_6 + 1L)
                            method_24((var_0: (Union0 [])), (var_1: ((Union0 []) [])), (var_2: int64), (var_3: int64), (var_4: bool), (var_5: string), (var_26: int64), (var_13: int64))
                        else
                            (failwith "char")
                    else
                        (failwith "string index out of bounds")
        else
            let (var_28: bool) =
                if (var_7 >= 0L) then
                    let (var_27: int64) = (int64 var_5.Length)
                    (var_7 < var_27)
                else
                    false
            if var_28 then
                let (var_29: char) = var_5.[int32 var_7]
                let (var_30: bool) = ('p' = var_29)
                let (var_31: int64) = (var_7 + 1L)
                if var_30 then
                    var_0.[int32 var_6] <- Union0Case2
                    let (var_32: int64) = (var_6 + 1L)
                    method_24((var_0: (Union0 [])), (var_1: ((Union0 []) [])), (var_2: int64), (var_3: int64), (var_4: bool), (var_5: string), (var_32: int64), (var_31: int64))
                else
                    let (var_34: bool) =
                        if (var_7 >= 0L) then
                            let (var_33: int64) = (int64 var_5.Length)
                            (var_7 < var_33)
                        else
                            false
                    if var_34 then
                        let (var_35: bool) = ('m' = var_29)
                        if var_35 then
                            var_0.[int32 var_6] <- Union0Case1
                            let (var_36: int64) = (var_6 + 1L)
                            method_24((var_0: (Union0 [])), (var_1: ((Union0 []) [])), (var_2: int64), (var_3: int64), (var_4: bool), (var_5: string), (var_36: int64), (var_31: int64))
                        else
                            (failwith "char")
                    else
                        (failwith "string index out of bounds")
            else
                let (var_38: bool) =
                    if (var_7 >= 0L) then
                        let (var_37: int64) = (int64 var_5.Length)
                        (var_7 < var_37)
                    else
                        false
                if var_38 then
                    let (var_39: char) = var_5.[int32 var_7]
                    let (var_40: bool) = ('m' = var_39)
                    let (var_41: int64) = (var_7 + 1L)
                    if var_40 then
                        var_0.[int32 var_6] <- Union0Case1
                        let (var_42: int64) = (var_6 + 1L)
                        method_24((var_0: (Union0 [])), (var_1: ((Union0 []) [])), (var_2: int64), (var_3: int64), (var_4: bool), (var_5: string), (var_42: int64), (var_41: int64))
                    else
                        (failwith "char")
                else
                    (failwith "string index out of bounds")
    else
        let (var_43: int64) = 0L
        method_25((var_0: (Union0 [])), (var_1: ((Union0 []) [])), (var_2: int64), (var_3: int64), (var_4: bool), (var_5: string), (var_43: int64), (var_7: int64))
and method_25((var_0: (Union0 [])), (var_1: ((Union0 []) [])), (var_2: int64), (var_3: int64), (var_4: bool), (var_5: string), (var_6: int64), (var_7: int64)): unit =
    let (var_8: int64) = (var_6 + 1L)
    let (var_10: bool) =
        if (var_7 >= 0L) then
            let (var_9: int64) = (int64 var_5.Length)
            (var_7 < var_9)
        else
            false
    if var_10 then
        let (var_11: char) = var_5.[int32 var_7]
        let (var_13: bool) =
            if (var_11 = ' ') then
                true
            else
                if (var_11 = '\n') then
                    true
                else
                    (var_11 = '\r')
        let (var_14: int64) = (var_7 + 1L)
        if var_13 then
            method_25((var_0: (Union0 [])), (var_1: ((Union0 []) [])), (var_2: int64), (var_3: int64), (var_4: bool), (var_5: string), (var_8: int64), (var_14: int64))
        else
            var_1.[int32 var_2] <- var_0
            let (var_15: int64) = (var_2 + 1L)
            method_23((var_1: ((Union0 []) [])), (var_3: int64), (var_5: string), (var_15: int64), (var_4: bool), (var_7: int64))
    else
        var_1.[int32 var_2] <- var_0
        let (var_16: int64) = (var_2 + 1L)
        method_23((var_1: ((Union0 []) [])), (var_3: int64), (var_5: string), (var_16: int64), (var_4: bool), (var_7: int64))
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
            let (var_9: int64) = (var_3 + 1L)
            method_31((var_0: ((Union0 []) [])), (var_1: int64), (var_2: int64), (var_9: int64))
        | Union0Case1 ->
            let (var_11: int64) = (var_3 + 1L)
            method_32((var_0: ((Union0 []) [])), (var_1: int64), (var_2: int64), (var_11: int64), (var_3: int64))
        | Union0Case2 ->
            let (var_13: int64) = (var_3 + 1L)
            method_35((var_0: ((Union0 []) [])), (var_1: int64), (var_2: int64), (var_13: int64), (var_3: int64))
    else
        let (var_16: int64) = (var_1 + 1L)
        method_30((var_0: ((Union0 []) [])), (var_2: int64), (var_16: int64))
and method_32((var_0: ((Union0 []) [])), (var_1: int64), (var_2: int64), (var_3: int64), (var_4: int64)): Env1 =
    if (var_3 < var_2) then
        let (var_5: (Union0 [])) = var_0.[int32 var_1]
        let (var_6: Union0) = var_5.[int32 var_3]
        match var_6 with
        | Union0Case0 ->
            let (var_10: int64) = (var_3 + 1L)
            method_32((var_0: ((Union0 []) [])), (var_1: int64), (var_2: int64), (var_10: int64), (var_4: int64))
        | Union0Case1 ->
            let (var_12: int64) = (var_3 + 1L)
            method_32((var_0: ((Union0 []) [])), (var_1: int64), (var_2: int64), (var_12: int64), (var_3: int64))
        | Union0Case2 ->
            (Env1((Union2Case0(Tuple7(Tuple3(var_1, var_4)))), (Union2Case0(Tuple7(Tuple3(var_1, var_3))))))
    else
        let (var_15: int64) = (var_1 + 1L)
        method_33((var_0: ((Union0 []) [])), (var_2: int64), (var_15: int64), (var_1: int64), (var_4: int64))
and method_33((var_0: ((Union0 []) [])), (var_1: int64), (var_2: int64), (var_3: int64), (var_4: int64)): Env1 =
    if (var_2 < var_1) then
        let (var_5: int64) = 0L
        method_34((var_0: ((Union0 []) [])), (var_2: int64), (var_1: int64), (var_5: int64), (var_3: int64), (var_4: int64))
    else
        (Env1((Union2Case0(Tuple7(Tuple3(var_3, var_4)))), Union2Case1))
and method_34((var_0: ((Union0 []) [])), (var_1: int64), (var_2: int64), (var_3: int64), (var_4: int64), (var_5: int64)): Env1 =
    if (var_3 < var_2) then
        let (var_6: (Union0 [])) = var_0.[int32 var_1]
        let (var_7: Union0) = var_6.[int32 var_3]
        match var_7 with
        | Union0Case0 ->
            let (var_11: int64) = (var_3 + 1L)
            method_34((var_0: ((Union0 []) [])), (var_1: int64), (var_2: int64), (var_11: int64), (var_4: int64), (var_5: int64))
        | Union0Case1 ->
            let (var_13: int64) = (var_3 + 1L)
            method_32((var_0: ((Union0 []) [])), (var_1: int64), (var_2: int64), (var_13: int64), (var_3: int64))
        | Union0Case2 ->
            (Env1((Union2Case0(Tuple7(Tuple3(var_4, var_5)))), (Union2Case0(Tuple7(Tuple3(var_1, var_3))))))
    else
        let (var_16: int64) = (var_1 + 1L)
        method_33((var_0: ((Union0 []) [])), (var_2: int64), (var_16: int64), (var_4: int64), (var_5: int64))
and method_35((var_0: ((Union0 []) [])), (var_1: int64), (var_2: int64), (var_3: int64), (var_4: int64)): Env1 =
    if (var_3 < var_2) then
        let (var_5: (Union0 [])) = var_0.[int32 var_1]
        let (var_6: Union0) = var_5.[int32 var_3]
        match var_6 with
        | Union0Case0 ->
            let (var_10: int64) = (var_3 + 1L)
            method_35((var_0: ((Union0 []) [])), (var_1: int64), (var_2: int64), (var_10: int64), (var_4: int64))
        | Union0Case1 ->
            (Env1((Union2Case0(Tuple7(Tuple3(var_1, var_3)))), (Union2Case0(Tuple7(Tuple3(var_1, var_4))))))
        | Union0Case2 ->
            let (var_12: int64) = (var_3 + 1L)
            method_35((var_0: ((Union0 []) [])), (var_1: int64), (var_2: int64), (var_12: int64), (var_3: int64))
    else
        let (var_15: int64) = (var_1 + 1L)
        method_36((var_0: ((Union0 []) [])), (var_2: int64), (var_15: int64), (var_1: int64), (var_4: int64))
and method_36((var_0: ((Union0 []) [])), (var_1: int64), (var_2: int64), (var_3: int64), (var_4: int64)): Env1 =
    if (var_2 < var_1) then
        let (var_5: int64) = 0L
        method_37((var_0: ((Union0 []) [])), (var_2: int64), (var_1: int64), (var_5: int64), (var_3: int64), (var_4: int64))
    else
        (Env1(Union2Case1, (Union2Case0(Tuple7(Tuple3(var_3, var_4))))))
and method_37((var_0: ((Union0 []) [])), (var_1: int64), (var_2: int64), (var_3: int64), (var_4: int64), (var_5: int64)): Env1 =
    if (var_3 < var_2) then
        let (var_6: (Union0 [])) = var_0.[int32 var_1]
        let (var_7: Union0) = var_6.[int32 var_3]
        match var_7 with
        | Union0Case0 ->
            let (var_11: int64) = (var_3 + 1L)
            method_37((var_0: ((Union0 []) [])), (var_1: int64), (var_2: int64), (var_11: int64), (var_4: int64), (var_5: int64))
        | Union0Case1 ->
            (Env1((Union2Case0(Tuple7(Tuple3(var_1, var_3)))), (Union2Case0(Tuple7(Tuple3(var_4, var_5))))))
        | Union0Case2 ->
            let (var_13: int64) = (var_3 + 1L)
            method_35((var_0: ((Union0 []) [])), (var_1: int64), (var_2: int64), (var_13: int64), (var_3: int64))
    else
        let (var_16: int64) = (var_1 + 1L)
        method_36((var_0: ((Union0 []) [])), (var_2: int64), (var_16: int64), (var_4: int64), (var_5: int64))
and method_39((var_0: int64), (var_1: int64), (var_2: (bool [])), (var_3: int64), (var_4: int64)): int64 =
    if (var_3 < var_1) then
        let (var_5: int64) = 0L
        let (var_6: int64) = method_40((var_3: int64), (var_2: (bool [])), (var_1: int64), (var_5: int64), (var_4: int64))
        let (var_7: int64) = (var_4 + var_0)
        let (var_8: int64) = (var_3 + 1L)
        method_39((var_0: int64), (var_1: int64), (var_2: (bool [])), (var_8: int64), (var_7: int64))
    else
        var_4
and method_40((var_0: int64), (var_1: (bool [])), (var_2: int64), (var_3: int64), (var_4: int64)): int64 =
    if (var_3 < var_2) then
        var_1.[int32 var_4] <- false
        let (var_5: int64) = (var_4 + 1L)
        let (var_6: int64) = (var_3 + 1L)
        method_40((var_0: int64), (var_1: (bool [])), (var_2: int64), (var_6: int64), (var_5: int64))
    else
        var_4
and method_49((var_0: (bool [])), (var_1: int64), (var_2: int64), (var_3: int64), (var_4: int64), (var_5: string), (var_6: string), (var_7: string), (var_8: string), (var_9: (Union6 ref)), (var_10: (Tuple4 []))): unit =
    let (var_11: int64) = var_10.LongLength
    let (var_12: bool) = (var_11 >= 0L)
    if (var_12 = false) then
        (failwith "The input to init needs to be greater or equal than 0.")
    else
        ()
    let (var_111: ((Tuple4 []) [])) = Array.zeroCreate<(Tuple4 [])> (System.Convert.ToInt32(var_11))
    let (var_112: int64) = 0L
    method_51((var_111: ((Tuple4 []) [])), (var_10: (Tuple4 [])), (var_0: (bool [])), (var_1: int64), (var_2: int64), (var_3: int64), (var_4: int64), (var_5: string), (var_6: string), (var_7: string), (var_8: string), (var_9: (Union6 ref)), (var_11: int64), (var_112: int64))
    let (var_113: int64) = 0L
    let (var_114: int64) = var_111.LongLength
    let (var_115: int64) = 0L
    let (var_116: int64) = method_52((var_111: ((Tuple4 []) [])), (var_114: int64), (var_115: int64), (var_113: int64))
    let (var_117: (Tuple4 [])) = Array.zeroCreate<Tuple4> (System.Convert.ToInt32(var_116))
    let (var_118: int64) = 0L
    let (var_119: int64) = var_111.LongLength
    let (var_120: int64) = 0L
    let (var_121: int64) = method_53((var_111: ((Tuple4 []) [])), (var_117: (Tuple4 [])), (var_119: int64), (var_120: int64), (var_118: int64))
    let (var_122: Union6) = (!var_9)
    match var_122 with
    | Union6Case0(var_123) ->
        let (var_125: Tuple4) = var_123.mem_1
        let (var_126: Tuple3) = var_125.mem_0
        let (var_127: Rec5) = var_125.mem_1
        method_55((var_127: Rec5))
    | Union6Case1 ->
        method_49((var_0: (bool [])), (var_1: int64), (var_2: int64), (var_3: int64), (var_4: int64), (var_5: string), (var_6: string), (var_7: string), (var_8: string), (var_9: (Union6 ref)), (var_117: (Tuple4 [])))
and method_51((var_0: ((Tuple4 []) [])), (var_1: (Tuple4 [])), (var_2: (bool [])), (var_3: int64), (var_4: int64), (var_5: int64), (var_6: int64), (var_7: string), (var_8: string), (var_9: string), (var_10: string), (var_11: (Union6 ref)), (var_12: int64), (var_13: int64)): unit =
    if (var_13 < var_12) then
        let (var_14: Tuple4) = var_1.[int32 var_13]
        let (var_15: Tuple3) = var_14.mem_0
        let (var_16: Rec5) = var_14.mem_1
        let (var_17: int64) = var_15.mem_0
        let (var_18: int64) = var_15.mem_1
        let (var_19: int64) = (var_17 - 1L)
        let (var_20: bool) =
            if (var_19 >= 0L) then
                (var_19 < var_4)
            else
                false
        let (var_22: bool) =
            if var_20 then
                if (var_18 >= 0L) then
                    (var_18 < var_4)
                else
                    false
            else
                false
        let (var_30: bool) =
            if var_22 then
                let (var_23: bool) =
                    if (var_19 >= 0L) then
                        (var_19 < var_4)
                    else
                        false
                if (var_23 = false) then
                    (failwith "Argument out of bounds.")
                else
                    ()
                let (var_24: int64) = (var_3 * var_19)
                let (var_25: int64) = (0L + var_24)
                let (var_26: bool) =
                    if (var_18 >= 0L) then
                        (var_18 < var_4)
                    else
                        false
                if (var_26 = false) then
                    (failwith "Argument out of bounds.")
                else
                    ()
                let (var_27: int64) = (1L * var_18)
                let (var_28: int64) = (var_25 + var_27)
                let (var_29: bool) = var_2.[int32 var_28]
                (var_29 = false)
            else
                false
        let (var_38: bool) =
            if var_30 then
                let (var_31: bool) =
                    if (var_19 = var_6) then
                        (var_18 = var_5)
                    else
                        false
                if var_31 then
                    var_11 := (Union6Case0(Tuple9(Tuple4(Tuple3(var_19, var_18), (Rec5Case1(Tuple8(var_7, var_16)))))))
                else
                    ()
                let (var_32: bool) =
                    if (var_19 >= 0L) then
                        (var_19 < var_4)
                    else
                        false
                if (var_32 = false) then
                    (failwith "Argument out of bounds.")
                else
                    ()
                let (var_33: int64) = (var_3 * var_19)
                let (var_34: int64) = (0L + var_33)
                let (var_35: bool) =
                    if (var_18 >= 0L) then
                        (var_18 < var_4)
                    else
                        false
                if (var_35 = false) then
                    (failwith "Argument out of bounds.")
                else
                    ()
                let (var_36: int64) = (1L * var_18)
                let (var_37: int64) = (var_34 + var_36)
                var_2.[int32 var_37] <- true
                true
            else
                false
        let (var_39: int64) = (var_17 + 1L)
        let (var_40: bool) =
            if (var_39 >= 0L) then
                (var_39 < var_4)
            else
                false
        let (var_42: bool) =
            if var_40 then
                if (var_18 >= 0L) then
                    (var_18 < var_4)
                else
                    false
            else
                false
        let (var_50: bool) =
            if var_42 then
                let (var_43: bool) =
                    if (var_39 >= 0L) then
                        (var_39 < var_4)
                    else
                        false
                if (var_43 = false) then
                    (failwith "Argument out of bounds.")
                else
                    ()
                let (var_44: int64) = (var_3 * var_39)
                let (var_45: int64) = (0L + var_44)
                let (var_46: bool) =
                    if (var_18 >= 0L) then
                        (var_18 < var_4)
                    else
                        false
                if (var_46 = false) then
                    (failwith "Argument out of bounds.")
                else
                    ()
                let (var_47: int64) = (1L * var_18)
                let (var_48: int64) = (var_45 + var_47)
                let (var_49: bool) = var_2.[int32 var_48]
                (var_49 = false)
            else
                false
        let (var_58: bool) =
            if var_50 then
                let (var_51: bool) =
                    if (var_39 = var_6) then
                        (var_18 = var_5)
                    else
                        false
                if var_51 then
                    var_11 := (Union6Case0(Tuple9(Tuple4(Tuple3(var_39, var_18), (Rec5Case1(Tuple8(var_8, var_16)))))))
                else
                    ()
                let (var_52: bool) =
                    if (var_39 >= 0L) then
                        (var_39 < var_4)
                    else
                        false
                if (var_52 = false) then
                    (failwith "Argument out of bounds.")
                else
                    ()
                let (var_53: int64) = (var_3 * var_39)
                let (var_54: int64) = (0L + var_53)
                let (var_55: bool) =
                    if (var_18 >= 0L) then
                        (var_18 < var_4)
                    else
                        false
                if (var_55 = false) then
                    (failwith "Argument out of bounds.")
                else
                    ()
                let (var_56: int64) = (1L * var_18)
                let (var_57: int64) = (var_54 + var_56)
                var_2.[int32 var_57] <- true
                true
            else
                false
        let (var_59: int64) = (var_18 - 1L)
        let (var_60: bool) =
            if (var_17 >= 0L) then
                (var_17 < var_4)
            else
                false
        let (var_62: bool) =
            if var_60 then
                if (var_59 >= 0L) then
                    (var_59 < var_4)
                else
                    false
            else
                false
        let (var_70: bool) =
            if var_62 then
                let (var_63: bool) =
                    if (var_17 >= 0L) then
                        (var_17 < var_4)
                    else
                        false
                if (var_63 = false) then
                    (failwith "Argument out of bounds.")
                else
                    ()
                let (var_64: int64) = (var_3 * var_17)
                let (var_65: int64) = (0L + var_64)
                let (var_66: bool) =
                    if (var_59 >= 0L) then
                        (var_59 < var_4)
                    else
                        false
                if (var_66 = false) then
                    (failwith "Argument out of bounds.")
                else
                    ()
                let (var_67: int64) = (1L * var_59)
                let (var_68: int64) = (var_65 + var_67)
                let (var_69: bool) = var_2.[int32 var_68]
                (var_69 = false)
            else
                false
        let (var_78: bool) =
            if var_70 then
                let (var_71: bool) =
                    if (var_17 = var_6) then
                        (var_59 = var_5)
                    else
                        false
                if var_71 then
                    var_11 := (Union6Case0(Tuple9(Tuple4(Tuple3(var_17, var_59), (Rec5Case1(Tuple8(var_9, var_16)))))))
                else
                    ()
                let (var_72: bool) =
                    if (var_17 >= 0L) then
                        (var_17 < var_4)
                    else
                        false
                if (var_72 = false) then
                    (failwith "Argument out of bounds.")
                else
                    ()
                let (var_73: int64) = (var_3 * var_17)
                let (var_74: int64) = (0L + var_73)
                let (var_75: bool) =
                    if (var_59 >= 0L) then
                        (var_59 < var_4)
                    else
                        false
                if (var_75 = false) then
                    (failwith "Argument out of bounds.")
                else
                    ()
                let (var_76: int64) = (1L * var_59)
                let (var_77: int64) = (var_74 + var_76)
                var_2.[int32 var_77] <- true
                true
            else
                false
        let (var_79: int64) = (var_18 + 1L)
        let (var_80: bool) =
            if (var_17 >= 0L) then
                (var_17 < var_4)
            else
                false
        let (var_82: bool) =
            if var_80 then
                if (var_79 >= 0L) then
                    (var_79 < var_4)
                else
                    false
            else
                false
        let (var_90: bool) =
            if var_82 then
                let (var_83: bool) =
                    if (var_17 >= 0L) then
                        (var_17 < var_4)
                    else
                        false
                if (var_83 = false) then
                    (failwith "Argument out of bounds.")
                else
                    ()
                let (var_84: int64) = (var_3 * var_17)
                let (var_85: int64) = (0L + var_84)
                let (var_86: bool) =
                    if (var_79 >= 0L) then
                        (var_79 < var_4)
                    else
                        false
                if (var_86 = false) then
                    (failwith "Argument out of bounds.")
                else
                    ()
                let (var_87: int64) = (1L * var_79)
                let (var_88: int64) = (var_85 + var_87)
                let (var_89: bool) = var_2.[int32 var_88]
                (var_89 = false)
            else
                false
        let (var_98: bool) =
            if var_90 then
                let (var_91: bool) =
                    if (var_17 = var_6) then
                        (var_79 = var_5)
                    else
                        false
                if var_91 then
                    var_11 := (Union6Case0(Tuple9(Tuple4(Tuple3(var_17, var_79), (Rec5Case1(Tuple8(var_10, var_16)))))))
                else
                    ()
                let (var_92: bool) =
                    if (var_17 >= 0L) then
                        (var_17 < var_4)
                    else
                        false
                if (var_92 = false) then
                    (failwith "Argument out of bounds.")
                else
                    ()
                let (var_93: int64) = (var_3 * var_17)
                let (var_94: int64) = (0L + var_93)
                let (var_95: bool) =
                    if (var_79 >= 0L) then
                        (var_79 < var_4)
                    else
                        false
                if (var_95 = false) then
                    (failwith "Argument out of bounds.")
                else
                    ()
                let (var_96: int64) = (1L * var_79)
                let (var_97: int64) = (var_94 + var_96)
                var_2.[int32 var_97] <- true
                true
            else
                false
        let (var_99: int64) =
            if var_38 then
                1L
            else
                0L
        let (var_100: int64) = (0L + var_99)
        let (var_101: int64) =
            if var_58 then
                1L
            else
                0L
        let (var_102: int64) = (var_100 + var_101)
        let (var_103: int64) =
            if var_78 then
                1L
            else
                0L
        let (var_104: int64) = (var_102 + var_103)
        let (var_105: int64) =
            if var_98 then
                1L
            else
                0L
        let (var_106: int64) = (var_104 + var_105)
        let (var_107: (Tuple4 [])) = Array.zeroCreate<Tuple4> (System.Convert.ToInt32(var_106))
        let (var_108: int64) =
            if var_38 then
                var_107.[int32 0L] <- Tuple4(Tuple3(var_19, var_18), (Rec5Case1(Tuple8(var_7, var_16))))
                1L
            else
                0L
        let (var_109: int64) =
            if var_58 then
                var_107.[int32 var_108] <- Tuple4(Tuple3(var_39, var_18), (Rec5Case1(Tuple8(var_8, var_16))))
                (var_108 + 1L)
            else
                var_108
        let (var_110: int64) =
            if var_78 then
                var_107.[int32 var_109] <- Tuple4(Tuple3(var_17, var_59), (Rec5Case1(Tuple8(var_9, var_16))))
                (var_109 + 1L)
            else
                var_109
        let (var_111: int64) =
            if var_98 then
                var_107.[int32 var_110] <- Tuple4(Tuple3(var_17, var_79), (Rec5Case1(Tuple8(var_10, var_16))))
                (var_110 + 1L)
            else
                var_110
        var_0.[int32 var_13] <- var_107
        let (var_112: int64) = (var_13 + 1L)
        method_51((var_0: ((Tuple4 []) [])), (var_1: (Tuple4 [])), (var_2: (bool [])), (var_3: int64), (var_4: int64), (var_5: int64), (var_6: int64), (var_7: string), (var_8: string), (var_9: string), (var_10: string), (var_11: (Union6 ref)), (var_12: int64), (var_112: int64))
    else
        ()
and method_52((var_0: ((Tuple4 []) [])), (var_1: int64), (var_2: int64), (var_3: int64)): int64 =
    if (var_2 < var_1) then
        let (var_4: (Tuple4 [])) = var_0.[int32 var_2]
        let (var_5: int64) = var_4.LongLength
        let (var_6: int64) = (var_3 + var_5)
        let (var_7: int64) = (var_2 + 1L)
        method_52((var_0: ((Tuple4 []) [])), (var_1: int64), (var_7: int64), (var_6: int64))
    else
        var_3
and method_53((var_0: ((Tuple4 []) [])), (var_1: (Tuple4 [])), (var_2: int64), (var_3: int64), (var_4: int64)): int64 =
    if (var_3 < var_2) then
        let (var_5: (Tuple4 [])) = var_0.[int32 var_3]
        let (var_6: int64) = var_5.LongLength
        let (var_7: int64) = 0L
        let (var_8: int64) = method_54((var_5: (Tuple4 [])), (var_1: (Tuple4 [])), (var_6: int64), (var_7: int64), (var_4: int64))
        let (var_9: int64) = (var_3 + 1L)
        method_53((var_0: ((Tuple4 []) [])), (var_1: (Tuple4 [])), (var_2: int64), (var_9: int64), (var_8: int64))
    else
        var_4
and method_54((var_0: (Tuple4 [])), (var_1: (Tuple4 [])), (var_2: int64), (var_3: int64), (var_4: int64)): int64 =
    if (var_3 < var_2) then
        let (var_5: Tuple4) = var_0.[int32 var_3]
        let (var_6: Tuple3) = var_5.mem_0
        let (var_7: Rec5) = var_5.mem_1
        var_1.[int32 var_4] <- Tuple4(var_6, var_7)
        let (var_8: int64) = (var_4 + 1L)
        let (var_9: int64) = (var_3 + 1L)
        method_54((var_0: (Tuple4 [])), (var_1: (Tuple4 [])), (var_2: int64), (var_9: int64), (var_8: int64))
    else
        var_4
and method_55((var_0: Rec5)): unit =
    match var_0 with
    | Rec5Case0 ->
        ()
    | Rec5Case1(var_2) ->
        let (var_3: string) = var_2.mem_0
        let (var_4: Rec5) = var_2.mem_1
        method_55((var_4: Rec5))
        System.Console.WriteLine(var_3)
let (var_0: string) = "3
---
-m-
p--
    "
let (var_1: int64) = 0L
let (var_3: bool) =
    if (var_1 >= 0L) then
        let (var_2: int64) = (int64 var_0.Length)
        (var_1 < var_2)
    else
        false
if var_3 then
    let (var_4: char) = var_0.[int32 var_1]
    let (var_5: bool) = ('-' = var_4)
    let (var_6: int64) = (var_1 + 1L)
    if var_5 then
        let (var_7: bool) = false
        method_19((var_7: bool), (var_0: string), (var_6: int64))
    else
        let (var_8: bool) = true
        method_19((var_8: bool), (var_0: string), (var_1: int64))
else
    let (var_9: bool) = true
    method_19((var_9: bool), (var_0: string), (var_1: int64))

