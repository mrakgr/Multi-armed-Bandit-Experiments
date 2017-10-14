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
                let (var_31: EnvHeap4) = ({mem_0 = (var_27: (bool [])); mem_1 = (var_22: int64)} : EnvHeap4)
                let (var_32: (bool [])) = var_31.mem_0
                let (var_33: int64) = var_31.mem_1
                let (var_34: bool) =
                    if (var_17 >= 0L) then
                        (var_17 <= var_33)
                    else
                        false
                if (var_34 = false) then
                    (failwith "Argument out of bounds.")
                else
                    ()
                let (var_35: int64) = (var_33 + 1L)
                let (var_36: int64) =
                    if (0L < var_35) then
                        var_35
                    else
                        0L
                let (var_37: bool) =
                    if (var_16 >= 0L) then
                        (var_16 <= var_33)
                    else
                        false
                if (var_37 = false) then
                    (failwith "Argument out of bounds.")
                else
                    ()
                let (var_38: int64) = (var_36 * var_16)
                let (var_39: int64) = (var_17 + var_38)
                var_32.[int32 var_39] <- true
                let (var_40: string) = "UP"
                let (var_41: string) = "DOWN"
                let (var_42: string) = "LEFT"
                let (var_43: string) = "RIGHT"
                let (var_44: (Tuple5 [])) = Array.zeroCreate<Tuple5> (System.Convert.ToInt32(1L))
                var_44.[int32 0L] <- Tuple5(Tuple3(var_16, var_17), Rec6Case0)
                let (var_45: (Union7 ref)) = (ref Union7Case1)
                method_49((var_31: EnvHeap4), (var_1: int64), (var_21: int64), (var_20: int64), (var_40: string), (var_41: string), (var_42: string), (var_43: string), (var_45: (Union7 ref)), (var_44: (Tuple5 [])))
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
    let (var_125: ((Tuple5 []) [])) = Array.zeroCreate<(Tuple5 [])> (System.Convert.ToInt32(var_10))
    let (var_126: int64) = 0L
    method_51((var_125: ((Tuple5 []) [])), (var_9: (Tuple5 [])), (var_0: EnvHeap4), (var_1: int64), (var_2: int64), (var_3: int64), (var_4: string), (var_5: string), (var_6: string), (var_7: string), (var_8: (Union7 ref)), (var_10: int64), (var_126: int64))
    let (var_127: int64) = 0L
    let (var_128: int64) = var_125.LongLength
    let (var_129: int64) = 0L
    let (var_130: int64) = method_52((var_125: ((Tuple5 []) [])), (var_128: int64), (var_129: int64), (var_127: int64))
    let (var_131: (Tuple5 [])) = Array.zeroCreate<Tuple5> (System.Convert.ToInt32(var_130))
    let (var_132: int64) = 0L
    let (var_133: int64) = var_125.LongLength
    let (var_134: int64) = 0L
    let (var_135: int64) = method_53((var_125: ((Tuple5 []) [])), (var_131: (Tuple5 [])), (var_133: int64), (var_134: int64), (var_132: int64))
    let (var_136: Union7) = (!var_8)
    match var_136 with
    | Union7Case0(var_137) ->
        let (var_138: Tuple5) = var_137.mem_1
        let (var_139: Tuple3) = var_138.mem_0
        let (var_140: Rec6) = var_138.mem_1
        method_55((var_140: Rec6))
    | Union7Case1 ->
        method_49((var_0: EnvHeap4), (var_1: int64), (var_2: int64), (var_3: int64), (var_4: string), (var_5: string), (var_6: string), (var_7: string), (var_8: (Union7 ref)), (var_131: (Tuple5 [])))
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
        let (var_31: bool) =
            if var_21 then
                let (var_22: (bool [])) = var_2.mem_0
                let (var_23: int64) = var_2.mem_1
                let (var_24: bool) =
                    if (var_17 >= 0L) then
                        (var_17 <= var_23)
                    else
                        false
                if (var_24 = false) then
                    (failwith "Argument out of bounds.")
                else
                    ()
                let (var_25: int64) = (var_23 + 1L)
                let (var_26: int64) =
                    if (0L < var_25) then
                        var_25
                    else
                        0L
                let (var_27: bool) =
                    if (var_18 >= 0L) then
                        (var_18 <= var_23)
                    else
                        false
                if (var_27 = false) then
                    (failwith "Argument out of bounds.")
                else
                    ()
                let (var_28: int64) = (var_26 * var_18)
                let (var_29: int64) = (var_17 + var_28)
                let (var_30: bool) = var_22.[int32 var_29]
                (var_30 = false)
            else
                false
        let (var_41: bool) =
            if var_31 then
                let (var_32: bool) =
                    if (var_18 = var_5) then
                        (var_17 = var_4)
                    else
                        false
                if var_32 then
                    var_10 := (Union7Case0(Tuple10(Tuple5(Tuple3(var_18, var_17), (Rec6Case1(Tuple9(var_6, var_15)))))))
                else
                    ()
                let (var_33: (bool [])) = var_2.mem_0
                let (var_34: int64) = var_2.mem_1
                let (var_35: bool) =
                    if (var_17 >= 0L) then
                        (var_17 <= var_34)
                    else
                        false
                if (var_35 = false) then
                    (failwith "Argument out of bounds.")
                else
                    ()
                let (var_36: int64) = (var_34 + 1L)
                let (var_37: int64) =
                    if (0L < var_36) then
                        var_36
                    else
                        0L
                let (var_38: bool) =
                    if (var_18 >= 0L) then
                        (var_18 <= var_34)
                    else
                        false
                if (var_38 = false) then
                    (failwith "Argument out of bounds.")
                else
                    ()
                let (var_39: int64) = (var_37 * var_18)
                let (var_40: int64) = (var_17 + var_39)
                var_33.[int32 var_40] <- true
                true
            else
                false
        let (var_42: int64) = (var_16 + 1L)
        let (var_43: bool) =
            if (var_42 >= 0L) then
                (var_42 < var_3)
            else
                false
        let (var_45: bool) =
            if var_43 then
                if (var_17 >= 0L) then
                    (var_17 < var_3)
                else
                    false
            else
                false
        let (var_55: bool) =
            if var_45 then
                let (var_46: (bool [])) = var_2.mem_0
                let (var_47: int64) = var_2.mem_1
                let (var_48: bool) =
                    if (var_17 >= 0L) then
                        (var_17 <= var_47)
                    else
                        false
                if (var_48 = false) then
                    (failwith "Argument out of bounds.")
                else
                    ()
                let (var_49: int64) = (var_47 + 1L)
                let (var_50: int64) =
                    if (0L < var_49) then
                        var_49
                    else
                        0L
                let (var_51: bool) =
                    if (var_42 >= 0L) then
                        (var_42 <= var_47)
                    else
                        false
                if (var_51 = false) then
                    (failwith "Argument out of bounds.")
                else
                    ()
                let (var_52: int64) = (var_50 * var_42)
                let (var_53: int64) = (var_17 + var_52)
                let (var_54: bool) = var_46.[int32 var_53]
                (var_54 = false)
            else
                false
        let (var_65: bool) =
            if var_55 then
                let (var_56: bool) =
                    if (var_42 = var_5) then
                        (var_17 = var_4)
                    else
                        false
                if var_56 then
                    var_10 := (Union7Case0(Tuple10(Tuple5(Tuple3(var_42, var_17), (Rec6Case1(Tuple9(var_7, var_15)))))))
                else
                    ()
                let (var_57: (bool [])) = var_2.mem_0
                let (var_58: int64) = var_2.mem_1
                let (var_59: bool) =
                    if (var_17 >= 0L) then
                        (var_17 <= var_58)
                    else
                        false
                if (var_59 = false) then
                    (failwith "Argument out of bounds.")
                else
                    ()
                let (var_60: int64) = (var_58 + 1L)
                let (var_61: int64) =
                    if (0L < var_60) then
                        var_60
                    else
                        0L
                let (var_62: bool) =
                    if (var_42 >= 0L) then
                        (var_42 <= var_58)
                    else
                        false
                if (var_62 = false) then
                    (failwith "Argument out of bounds.")
                else
                    ()
                let (var_63: int64) = (var_61 * var_42)
                let (var_64: int64) = (var_17 + var_63)
                var_57.[int32 var_64] <- true
                true
            else
                false
        let (var_66: int64) = (var_17 - 1L)
        let (var_67: bool) =
            if (var_16 >= 0L) then
                (var_16 < var_3)
            else
                false
        let (var_69: bool) =
            if var_67 then
                if (var_66 >= 0L) then
                    (var_66 < var_3)
                else
                    false
            else
                false
        let (var_79: bool) =
            if var_69 then
                let (var_70: (bool [])) = var_2.mem_0
                let (var_71: int64) = var_2.mem_1
                let (var_72: bool) =
                    if (var_66 >= 0L) then
                        (var_66 <= var_71)
                    else
                        false
                if (var_72 = false) then
                    (failwith "Argument out of bounds.")
                else
                    ()
                let (var_73: int64) = (var_71 + 1L)
                let (var_74: int64) =
                    if (0L < var_73) then
                        var_73
                    else
                        0L
                let (var_75: bool) =
                    if (var_16 >= 0L) then
                        (var_16 <= var_71)
                    else
                        false
                if (var_75 = false) then
                    (failwith "Argument out of bounds.")
                else
                    ()
                let (var_76: int64) = (var_74 * var_16)
                let (var_77: int64) = (var_66 + var_76)
                let (var_78: bool) = var_70.[int32 var_77]
                (var_78 = false)
            else
                false
        let (var_89: bool) =
            if var_79 then
                let (var_80: bool) =
                    if (var_16 = var_5) then
                        (var_66 = var_4)
                    else
                        false
                if var_80 then
                    var_10 := (Union7Case0(Tuple10(Tuple5(Tuple3(var_16, var_66), (Rec6Case1(Tuple9(var_8, var_15)))))))
                else
                    ()
                let (var_81: (bool [])) = var_2.mem_0
                let (var_82: int64) = var_2.mem_1
                let (var_83: bool) =
                    if (var_66 >= 0L) then
                        (var_66 <= var_82)
                    else
                        false
                if (var_83 = false) then
                    (failwith "Argument out of bounds.")
                else
                    ()
                let (var_84: int64) = (var_82 + 1L)
                let (var_85: int64) =
                    if (0L < var_84) then
                        var_84
                    else
                        0L
                let (var_86: bool) =
                    if (var_16 >= 0L) then
                        (var_16 <= var_82)
                    else
                        false
                if (var_86 = false) then
                    (failwith "Argument out of bounds.")
                else
                    ()
                let (var_87: int64) = (var_85 * var_16)
                let (var_88: int64) = (var_66 + var_87)
                var_81.[int32 var_88] <- true
                true
            else
                false
        let (var_90: int64) = (var_17 + 1L)
        let (var_91: bool) =
            if (var_16 >= 0L) then
                (var_16 < var_3)
            else
                false
        let (var_93: bool) =
            if var_91 then
                if (var_90 >= 0L) then
                    (var_90 < var_3)
                else
                    false
            else
                false
        let (var_103: bool) =
            if var_93 then
                let (var_94: (bool [])) = var_2.mem_0
                let (var_95: int64) = var_2.mem_1
                let (var_96: bool) =
                    if (var_90 >= 0L) then
                        (var_90 <= var_95)
                    else
                        false
                if (var_96 = false) then
                    (failwith "Argument out of bounds.")
                else
                    ()
                let (var_97: int64) = (var_95 + 1L)
                let (var_98: int64) =
                    if (0L < var_97) then
                        var_97
                    else
                        0L
                let (var_99: bool) =
                    if (var_16 >= 0L) then
                        (var_16 <= var_95)
                    else
                        false
                if (var_99 = false) then
                    (failwith "Argument out of bounds.")
                else
                    ()
                let (var_100: int64) = (var_98 * var_16)
                let (var_101: int64) = (var_90 + var_100)
                let (var_102: bool) = var_94.[int32 var_101]
                (var_102 = false)
            else
                false
        let (var_113: bool) =
            if var_103 then
                let (var_104: bool) =
                    if (var_16 = var_5) then
                        (var_90 = var_4)
                    else
                        false
                if var_104 then
                    var_10 := (Union7Case0(Tuple10(Tuple5(Tuple3(var_16, var_90), (Rec6Case1(Tuple9(var_9, var_15)))))))
                else
                    ()
                let (var_105: (bool [])) = var_2.mem_0
                let (var_106: int64) = var_2.mem_1
                let (var_107: bool) =
                    if (var_90 >= 0L) then
                        (var_90 <= var_106)
                    else
                        false
                if (var_107 = false) then
                    (failwith "Argument out of bounds.")
                else
                    ()
                let (var_108: int64) = (var_106 + 1L)
                let (var_109: int64) =
                    if (0L < var_108) then
                        var_108
                    else
                        0L
                let (var_110: bool) =
                    if (var_16 >= 0L) then
                        (var_16 <= var_106)
                    else
                        false
                if (var_110 = false) then
                    (failwith "Argument out of bounds.")
                else
                    ()
                let (var_111: int64) = (var_109 * var_16)
                let (var_112: int64) = (var_90 + var_111)
                var_105.[int32 var_112] <- true
                true
            else
                false
        let (var_114: int64) =
            if var_41 then
                1L
            else
                0L
        let (var_115: int64) =
            if var_65 then
                1L
            else
                0L
        let (var_116: int64) = (var_114 + var_115)
        let (var_117: int64) =
            if var_89 then
                1L
            else
                0L
        let (var_118: int64) = (var_116 + var_117)
        let (var_119: int64) =
            if var_113 then
                1L
            else
                0L
        let (var_120: int64) = (var_118 + var_119)
        let (var_121: (Tuple5 [])) = Array.zeroCreate<Tuple5> (System.Convert.ToInt32(var_120))
        let (var_122: int64) =
            if var_41 then
                var_121.[int32 0L] <- Tuple5(Tuple3(var_18, var_17), (Rec6Case1(Tuple9(var_6, var_15))))
                1L
            else
                0L
        let (var_123: int64) =
            if var_65 then
                var_121.[int32 var_122] <- Tuple5(Tuple3(var_42, var_17), (Rec6Case1(Tuple9(var_7, var_15))))
                (var_122 + 1L)
            else
                var_122
        let (var_124: int64) =
            if var_89 then
                var_121.[int32 var_123] <- Tuple5(Tuple3(var_16, var_66), (Rec6Case1(Tuple9(var_8, var_15))))
                (var_123 + 1L)
            else
                var_123
        let (var_125: int64) =
            if var_113 then
                var_121.[int32 var_124] <- Tuple5(Tuple3(var_16, var_90), (Rec6Case1(Tuple9(var_9, var_15))))
                (var_124 + 1L)
            else
                var_124
        var_0.[int32 var_12] <- var_121
        let (var_126: int64) = (var_12 + 1L)
        method_51((var_0: ((Tuple5 []) [])), (var_1: (Tuple5 [])), (var_2: EnvHeap4), (var_3: int64), (var_4: int64), (var_5: int64), (var_6: string), (var_7: string), (var_8: string), (var_9: string), (var_10: (Union7 ref)), (var_11: int64), (var_126: int64))
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

