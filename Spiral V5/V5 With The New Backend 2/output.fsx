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
                let (var_25: int64) = (var_24 * 1L)
                let (var_26: int64) =
                    if (0L < var_23) then
                        var_23
                    else
                        0L
                let (var_27: int64) = (var_26 * var_25)
                let (var_28: (bool [])) = Array.zeroCreate<bool> (System.Convert.ToInt32(var_27))
                let (var_29: int64) = 0L
                let (var_30: int64) = 0L
                let (var_31: int64) = method_39((var_25: int64), (var_22: int64), (var_28: (bool [])), (var_30: int64), (var_29: int64))
                let (var_32: bool) =
                    if (var_16 >= 0L) then
                        (var_16 <= var_22)
                    else
                        false
                if (var_32 = false) then
                    (failwith "Argument out of bounds.")
                else
                    ()
                let (var_33: int64) = (var_25 * var_16)
                let (var_34: bool) =
                    if (var_17 >= 0L) then
                        (var_17 <= var_22)
                    else
                        false
                if (var_34 = false) then
                    (failwith "Argument out of bounds.")
                else
                    ()
                let (var_35: int64) = (1L * var_17)
                let (var_36: int64) = (var_33 + var_35)
                var_28.[int32 var_36] <- true
                let (var_37: string) = "UP"
                let (var_38: string) = "DOWN"
                let (var_39: string) = "LEFT"
                let (var_40: string) = "RIGHT"
                let (var_41: (Tuple4 [])) = Array.zeroCreate<Tuple4> (System.Convert.ToInt32(1L))
                var_41.[int32 0L] <- Tuple4(Tuple3(var_16, var_17), Rec5Case0)
                let (var_42: (Union6 ref)) = (ref Union6Case1)
                method_49((var_28: (bool [])), (var_25: int64), (var_22: int64), (var_1: int64), (var_21: int64), (var_20: int64), (var_37: string), (var_38: string), (var_39: string), (var_40: string), (var_42: (Union6 ref)), (var_41: (Tuple4 [])))
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
            (Env1((Union2Case0(Tuple7(Tuple3(var_1, var_4)))), (Union2Case0(Tuple7(Tuple3(var_1, var_3))))))
    else
        let (var_12: int64) = (var_1 + 1L)
        method_33((var_0: ((Union0 []) [])), (var_2: int64), (var_12: int64), (var_1: int64), (var_4: int64))
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
            let (var_8: int64) = (var_3 + 1L)
            method_34((var_0: ((Union0 []) [])), (var_1: int64), (var_2: int64), (var_8: int64), (var_4: int64), (var_5: int64))
        | Union0Case1 ->
            let (var_10: int64) = (var_3 + 1L)
            method_32((var_0: ((Union0 []) [])), (var_1: int64), (var_2: int64), (var_10: int64), (var_3: int64))
        | Union0Case2 ->
            (Env1((Union2Case0(Tuple7(Tuple3(var_4, var_5)))), (Union2Case0(Tuple7(Tuple3(var_1, var_3))))))
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
            (Env1((Union2Case0(Tuple7(Tuple3(var_1, var_3)))), (Union2Case0(Tuple7(Tuple3(var_1, var_4))))))
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
        (Env1(Union2Case1, (Union2Case0(Tuple7(Tuple3(var_3, var_4))))))
and method_37((var_0: ((Union0 []) [])), (var_1: int64), (var_2: int64), (var_3: int64), (var_4: int64), (var_5: int64)): Env1 =
    if (var_3 < var_2) then
        let (var_6: (Union0 [])) = var_0.[int32 var_1]
        let (var_7: Union0) = var_6.[int32 var_3]
        match var_7 with
        | Union0Case0 ->
            let (var_8: int64) = (var_3 + 1L)
            method_37((var_0: ((Union0 []) [])), (var_1: int64), (var_2: int64), (var_8: int64), (var_4: int64), (var_5: int64))
        | Union0Case1 ->
            (Env1((Union2Case0(Tuple7(Tuple3(var_1, var_3)))), (Union2Case0(Tuple7(Tuple3(var_4, var_5))))))
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
and method_49((var_0: (bool [])), (var_1: int64), (var_2: int64), (var_3: int64), (var_4: int64), (var_5: int64), (var_6: string), (var_7: string), (var_8: string), (var_9: string), (var_10: (Union6 ref)), (var_11: (Tuple4 []))): unit =
    let (var_12: int64) = var_11.LongLength
    let (var_13: bool) = (var_12 >= 0L)
    if (var_13 = false) then
        (failwith "The input to init needs to be greater or equal than 0.")
    else
        ()
    let (var_103: ((Tuple4 []) [])) = Array.zeroCreate<(Tuple4 [])> (System.Convert.ToInt32(var_12))
    let (var_104: int64) = 0L
    method_51((var_103: ((Tuple4 []) [])), (var_11: (Tuple4 [])), (var_0: (bool [])), (var_1: int64), (var_2: int64), (var_3: int64), (var_4: int64), (var_5: int64), (var_6: string), (var_7: string), (var_8: string), (var_9: string), (var_10: (Union6 ref)), (var_12: int64), (var_104: int64))
    let (var_105: int64) = 0L
    let (var_106: int64) = var_103.LongLength
    let (var_107: int64) = 0L
    let (var_108: int64) = method_52((var_103: ((Tuple4 []) [])), (var_106: int64), (var_107: int64), (var_105: int64))
    let (var_109: (Tuple4 [])) = Array.zeroCreate<Tuple4> (System.Convert.ToInt32(var_108))
    let (var_110: int64) = 0L
    let (var_111: int64) = var_103.LongLength
    let (var_112: int64) = 0L
    let (var_113: int64) = method_53((var_103: ((Tuple4 []) [])), (var_109: (Tuple4 [])), (var_111: int64), (var_112: int64), (var_110: int64))
    let (var_114: Union6) = (!var_10)
    match var_114 with
    | Union6Case0(var_115) ->
        let (var_116: Tuple4) = var_115.mem_1
        let (var_117: Tuple3) = var_116.mem_0
        let (var_118: Rec5) = var_116.mem_1
        method_55((var_118: Rec5))
    | Union6Case1 ->
        method_49((var_0: (bool [])), (var_1: int64), (var_2: int64), (var_3: int64), (var_4: int64), (var_5: int64), (var_6: string), (var_7: string), (var_8: string), (var_9: string), (var_10: (Union6 ref)), (var_109: (Tuple4 [])))
and method_51((var_0: ((Tuple4 []) [])), (var_1: (Tuple4 [])), (var_2: (bool [])), (var_3: int64), (var_4: int64), (var_5: int64), (var_6: int64), (var_7: int64), (var_8: string), (var_9: string), (var_10: string), (var_11: string), (var_12: (Union6 ref)), (var_13: int64), (var_14: int64)): unit =
    if (var_14 < var_13) then
        let (var_15: Tuple4) = var_1.[int32 var_14]
        let (var_16: Tuple3) = var_15.mem_0
        let (var_17: Rec5) = var_15.mem_1
        let (var_18: int64) = var_16.mem_0
        let (var_19: int64) = var_16.mem_1
        let (var_20: int64) = (var_18 - 1L)
        let (var_21: bool) =
            if (var_20 >= 0L) then
                (var_20 < var_5)
            else
                false
        let (var_23: bool) =
            if var_21 then
                if (var_19 >= 0L) then
                    (var_19 < var_5)
                else
                    false
            else
                false
        let (var_30: bool) =
            if var_23 then
                let (var_24: bool) =
                    if (var_20 >= 0L) then
                        (var_20 <= var_4)
                    else
                        false
                if (var_24 = false) then
                    (failwith "Argument out of bounds.")
                else
                    ()
                let (var_25: int64) = (var_3 * var_20)
                let (var_26: bool) =
                    if (var_19 >= 0L) then
                        (var_19 <= var_4)
                    else
                        false
                if (var_26 = false) then
                    (failwith "Argument out of bounds.")
                else
                    ()
                let (var_27: int64) = (1L * var_19)
                let (var_28: int64) = (var_25 + var_27)
                let (var_29: bool) = var_2.[int32 var_28]
                (var_29 = false)
            else
                false
        let (var_37: bool) =
            if var_30 then
                let (var_31: bool) =
                    if (var_20 = var_7) then
                        (var_19 = var_6)
                    else
                        false
                if var_31 then
                    var_12 := (Union6Case0(Tuple9(Tuple4(Tuple3(var_20, var_19), (Rec5Case1(Tuple8(var_8, var_17)))))))
                else
                    ()
                let (var_32: bool) =
                    if (var_20 >= 0L) then
                        (var_20 <= var_4)
                    else
                        false
                if (var_32 = false) then
                    (failwith "Argument out of bounds.")
                else
                    ()
                let (var_33: int64) = (var_3 * var_20)
                let (var_34: bool) =
                    if (var_19 >= 0L) then
                        (var_19 <= var_4)
                    else
                        false
                if (var_34 = false) then
                    (failwith "Argument out of bounds.")
                else
                    ()
                let (var_35: int64) = (1L * var_19)
                let (var_36: int64) = (var_33 + var_35)
                var_2.[int32 var_36] <- true
                true
            else
                false
        let (var_38: int64) = (var_18 + 1L)
        let (var_39: bool) =
            if (var_38 >= 0L) then
                (var_38 < var_5)
            else
                false
        let (var_41: bool) =
            if var_39 then
                if (var_19 >= 0L) then
                    (var_19 < var_5)
                else
                    false
            else
                false
        let (var_48: bool) =
            if var_41 then
                let (var_42: bool) =
                    if (var_38 >= 0L) then
                        (var_38 <= var_4)
                    else
                        false
                if (var_42 = false) then
                    (failwith "Argument out of bounds.")
                else
                    ()
                let (var_43: int64) = (var_3 * var_38)
                let (var_44: bool) =
                    if (var_19 >= 0L) then
                        (var_19 <= var_4)
                    else
                        false
                if (var_44 = false) then
                    (failwith "Argument out of bounds.")
                else
                    ()
                let (var_45: int64) = (1L * var_19)
                let (var_46: int64) = (var_43 + var_45)
                let (var_47: bool) = var_2.[int32 var_46]
                (var_47 = false)
            else
                false
        let (var_55: bool) =
            if var_48 then
                let (var_49: bool) =
                    if (var_38 = var_7) then
                        (var_19 = var_6)
                    else
                        false
                if var_49 then
                    var_12 := (Union6Case0(Tuple9(Tuple4(Tuple3(var_38, var_19), (Rec5Case1(Tuple8(var_9, var_17)))))))
                else
                    ()
                let (var_50: bool) =
                    if (var_38 >= 0L) then
                        (var_38 <= var_4)
                    else
                        false
                if (var_50 = false) then
                    (failwith "Argument out of bounds.")
                else
                    ()
                let (var_51: int64) = (var_3 * var_38)
                let (var_52: bool) =
                    if (var_19 >= 0L) then
                        (var_19 <= var_4)
                    else
                        false
                if (var_52 = false) then
                    (failwith "Argument out of bounds.")
                else
                    ()
                let (var_53: int64) = (1L * var_19)
                let (var_54: int64) = (var_51 + var_53)
                var_2.[int32 var_54] <- true
                true
            else
                false
        let (var_56: int64) = (var_19 - 1L)
        let (var_57: bool) =
            if (var_18 >= 0L) then
                (var_18 < var_5)
            else
                false
        let (var_59: bool) =
            if var_57 then
                if (var_56 >= 0L) then
                    (var_56 < var_5)
                else
                    false
            else
                false
        let (var_66: bool) =
            if var_59 then
                let (var_60: bool) =
                    if (var_18 >= 0L) then
                        (var_18 <= var_4)
                    else
                        false
                if (var_60 = false) then
                    (failwith "Argument out of bounds.")
                else
                    ()
                let (var_61: int64) = (var_3 * var_18)
                let (var_62: bool) =
                    if (var_56 >= 0L) then
                        (var_56 <= var_4)
                    else
                        false
                if (var_62 = false) then
                    (failwith "Argument out of bounds.")
                else
                    ()
                let (var_63: int64) = (1L * var_56)
                let (var_64: int64) = (var_61 + var_63)
                let (var_65: bool) = var_2.[int32 var_64]
                (var_65 = false)
            else
                false
        let (var_73: bool) =
            if var_66 then
                let (var_67: bool) =
                    if (var_18 = var_7) then
                        (var_56 = var_6)
                    else
                        false
                if var_67 then
                    var_12 := (Union6Case0(Tuple9(Tuple4(Tuple3(var_18, var_56), (Rec5Case1(Tuple8(var_10, var_17)))))))
                else
                    ()
                let (var_68: bool) =
                    if (var_18 >= 0L) then
                        (var_18 <= var_4)
                    else
                        false
                if (var_68 = false) then
                    (failwith "Argument out of bounds.")
                else
                    ()
                let (var_69: int64) = (var_3 * var_18)
                let (var_70: bool) =
                    if (var_56 >= 0L) then
                        (var_56 <= var_4)
                    else
                        false
                if (var_70 = false) then
                    (failwith "Argument out of bounds.")
                else
                    ()
                let (var_71: int64) = (1L * var_56)
                let (var_72: int64) = (var_69 + var_71)
                var_2.[int32 var_72] <- true
                true
            else
                false
        let (var_74: int64) = (var_19 + 1L)
        let (var_75: bool) =
            if (var_18 >= 0L) then
                (var_18 < var_5)
            else
                false
        let (var_77: bool) =
            if var_75 then
                if (var_74 >= 0L) then
                    (var_74 < var_5)
                else
                    false
            else
                false
        let (var_84: bool) =
            if var_77 then
                let (var_78: bool) =
                    if (var_18 >= 0L) then
                        (var_18 <= var_4)
                    else
                        false
                if (var_78 = false) then
                    (failwith "Argument out of bounds.")
                else
                    ()
                let (var_79: int64) = (var_3 * var_18)
                let (var_80: bool) =
                    if (var_74 >= 0L) then
                        (var_74 <= var_4)
                    else
                        false
                if (var_80 = false) then
                    (failwith "Argument out of bounds.")
                else
                    ()
                let (var_81: int64) = (1L * var_74)
                let (var_82: int64) = (var_79 + var_81)
                let (var_83: bool) = var_2.[int32 var_82]
                (var_83 = false)
            else
                false
        let (var_91: bool) =
            if var_84 then
                let (var_85: bool) =
                    if (var_18 = var_7) then
                        (var_74 = var_6)
                    else
                        false
                if var_85 then
                    var_12 := (Union6Case0(Tuple9(Tuple4(Tuple3(var_18, var_74), (Rec5Case1(Tuple8(var_11, var_17)))))))
                else
                    ()
                let (var_86: bool) =
                    if (var_18 >= 0L) then
                        (var_18 <= var_4)
                    else
                        false
                if (var_86 = false) then
                    (failwith "Argument out of bounds.")
                else
                    ()
                let (var_87: int64) = (var_3 * var_18)
                let (var_88: bool) =
                    if (var_74 >= 0L) then
                        (var_74 <= var_4)
                    else
                        false
                if (var_88 = false) then
                    (failwith "Argument out of bounds.")
                else
                    ()
                let (var_89: int64) = (1L * var_74)
                let (var_90: int64) = (var_87 + var_89)
                var_2.[int32 var_90] <- true
                true
            else
                false
        let (var_92: int64) =
            if var_37 then
                1L
            else
                0L
        let (var_93: int64) =
            if var_55 then
                1L
            else
                0L
        let (var_94: int64) = (var_92 + var_93)
        let (var_95: int64) =
            if var_73 then
                1L
            else
                0L
        let (var_96: int64) = (var_94 + var_95)
        let (var_97: int64) =
            if var_91 then
                1L
            else
                0L
        let (var_98: int64) = (var_96 + var_97)
        let (var_99: (Tuple4 [])) = Array.zeroCreate<Tuple4> (System.Convert.ToInt32(var_98))
        let (var_100: int64) =
            if var_37 then
                var_99.[int32 0L] <- Tuple4(Tuple3(var_20, var_19), (Rec5Case1(Tuple8(var_8, var_17))))
                1L
            else
                0L
        let (var_101: int64) =
            if var_55 then
                var_99.[int32 var_100] <- Tuple4(Tuple3(var_38, var_19), (Rec5Case1(Tuple8(var_9, var_17))))
                (var_100 + 1L)
            else
                var_100
        let (var_102: int64) =
            if var_73 then
                var_99.[int32 var_101] <- Tuple4(Tuple3(var_18, var_56), (Rec5Case1(Tuple8(var_10, var_17))))
                (var_101 + 1L)
            else
                var_101
        let (var_103: int64) =
            if var_91 then
                var_99.[int32 var_102] <- Tuple4(Tuple3(var_18, var_74), (Rec5Case1(Tuple8(var_11, var_17))))
                (var_102 + 1L)
            else
                var_102
        var_0.[int32 var_14] <- var_99
        let (var_104: int64) = (var_14 + 1L)
        method_51((var_0: ((Tuple4 []) [])), (var_1: (Tuple4 [])), (var_2: (bool [])), (var_3: int64), (var_4: int64), (var_5: int64), (var_6: int64), (var_7: int64), (var_8: string), (var_9: string), (var_10: string), (var_11: string), (var_12: (Union6 ref)), (var_13: int64), (var_104: int64))
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
    | Rec5Case1(var_1) ->
        let (var_2: string) = var_1.mem_0
        let (var_3: Rec5) = var_1.mem_1
        method_55((var_3: Rec5))
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

