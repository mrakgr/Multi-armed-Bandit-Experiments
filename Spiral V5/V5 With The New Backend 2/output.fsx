type Env0 =
    struct
    val mem_some: Env5
    new(arg_mem_some) = {mem_some = arg_mem_some}
    end
and Env1 =
    struct
    val mem_er_msg: string
    new(arg_mem_er_msg) = {mem_er_msg = arg_mem_er_msg}
    end
and Env2 =
    struct
    val mem_y: Env1
    new(arg_mem_y) = {mem_y = arg_mem_y}
    end
and Env3 =
    struct
    val mem_for: Env2
    new(arg_mem_for) = {mem_for = arg_mem_for}
    end
and Env4 =
    struct
    val mem_init: Env3
    new(arg_mem_init) = {mem_init = arg_mem_init}
    end
and Env5 =
    struct
    val mem_ArrayN: Env4
    new(arg_mem_ArrayN) = {mem_ArrayN = arg_mem_ArrayN}
    end
and Union6 =
    | Union6Case0
    | Union6Case1
    | Union6Case2
let rec method_19(): Env0 =
    (Env0((Env5((Env4((Env3((Env2((Env1("The by field should not be zero in loop as the program would diverge."))))))))))))
and method_20((var_0: bool), (var_1: Env5), (var_2: string), (var_3: int64)): unit =
    let (var_5: bool) =
        if (var_3 >= 0L) then
            let (var_4: int64) = (int64 var_2.Length)
            (var_3 < var_4)
        else
            false
    if var_5 then
        let (var_6: char) = var_2.[int32 var_3]
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
            let (var_12: int64) = (0L + var_11)
            method_21((var_0: bool), (var_1: Env5), (var_2: string), (var_12: int64), (var_8: int64))
        else
            (failwith "pint64")
    else
        (failwith "pint64")
and method_21((var_0: bool), (var_1: Env5), (var_2: string), (var_3: int64), (var_4: int64)): unit =
    let (var_6: bool) =
        if (var_4 >= 0L) then
            let (var_5: int64) = (int64 var_2.Length)
            (var_4 < var_5)
        else
            false
    if var_6 then
        let (var_7: char) = var_2.[int32 var_4]
        let (var_8: bool) =
            if (var_7 >= '0') then
                (var_7 <= '9')
            else
                false
        let (var_9: int64) = (var_4 + 1L)
        if var_8 then
            let (var_10: int64) = System.Convert.ToInt64(var_7)
            let (var_11: int64) = System.Convert.ToInt64('0')
            let (var_12: int64) = (var_10 - var_11)
            let (var_13: bool) =
                if (var_3 = 922337203685477580L) then
                    (var_12 <= 7L)
                else
                    false
            let (var_14: bool) =
                if var_13 then
                    true
                else
                    (var_3 < 922337203685477580L)
            if var_14 then
                let (var_15: int64) = (var_3 * 10L)
                let (var_16: int64) = (var_15 + var_12)
                method_21((var_0: bool), (var_1: Env5), (var_2: string), (var_16: int64), (var_9: int64))
            else
                (failwith "integer overflow")
        else
            let (var_17: int64) =
                if var_0 then
                    var_3
                else
                    (-var_3)
            let (var_18: int64) = 0L
            method_22((var_17: int64), (var_1: Env5), (var_2: string), (var_18: int64), (var_4: int64))
    else
        let (var_19: int64) =
            if var_0 then
                var_3
            else
                (-var_3)
        let (var_20: int64) = 0L
        method_22((var_19: int64), (var_1: Env5), (var_2: string), (var_20: int64), (var_4: int64))
and method_22((var_0: int64), (var_1: Env5), (var_2: string), (var_3: int64), (var_4: int64)): unit =
    let (var_5: int64) = (var_3 + 1L)
    let (var_7: bool) =
        if (var_4 >= 0L) then
            let (var_6: int64) = (int64 var_2.Length)
            (var_4 < var_6)
        else
            false
    if var_7 then
        let (var_8: char) = var_2.[int32 var_4]
        let (var_10: bool) =
            if (var_8 = ' ') then
                true
            else
                if (var_8 = '\n') then
                    true
                else
                    (var_8 = '\r')
        let (var_11: int64) = (var_4 + 1L)
        if var_10 then
            method_22((var_0: int64), (var_1: Env5), (var_2: string), (var_5: int64), (var_11: int64))
        else
            let (var_13: bool) =
                if (var_4 >= 0L) then
                    let (var_12: int64) = (int64 var_2.Length)
                    (var_4 < var_12)
                else
                    false
            if var_13 then
                let (var_14: bool) = ('-' = var_8)
                if var_14 then
                    let (var_15: bool) = false
                    method_23((var_15: bool), (var_0: int64), (var_1: Env5), (var_2: string), (var_11: int64))
                else
                    let (var_16: bool) = true
                    method_23((var_16: bool), (var_0: int64), (var_1: Env5), (var_2: string), (var_4: int64))
            else
                let (var_17: bool) = true
                method_23((var_17: bool), (var_0: int64), (var_1: Env5), (var_2: string), (var_4: int64))
    else
        let (var_19: bool) =
            if (var_4 >= 0L) then
                let (var_18: int64) = (int64 var_2.Length)
                (var_4 < var_18)
            else
                false
        if var_19 then
            let (var_20: char) = var_2.[int32 var_4]
            let (var_21: bool) = ('-' = var_20)
            let (var_22: int64) = (var_4 + 1L)
            if var_21 then
                let (var_23: bool) = false
                method_23((var_23: bool), (var_0: int64), (var_1: Env5), (var_2: string), (var_22: int64))
            else
                let (var_24: bool) = true
                method_23((var_24: bool), (var_0: int64), (var_1: Env5), (var_2: string), (var_4: int64))
        else
            let (var_25: bool) = true
            method_23((var_25: bool), (var_0: int64), (var_1: Env5), (var_2: string), (var_4: int64))
and method_23((var_0: bool), (var_1: int64), (var_2: Env5), (var_3: string), (var_4: int64)): unit =
    let (var_6: bool) =
        if (var_4 >= 0L) then
            let (var_5: int64) = (int64 var_3.Length)
            (var_4 < var_5)
        else
            false
    if var_6 then
        let (var_7: char) = var_3.[int32 var_4]
        let (var_8: bool) =
            if (var_7 >= '0') then
                (var_7 <= '9')
            else
                false
        let (var_9: int64) = (var_4 + 1L)
        if var_8 then
            let (var_10: int64) = System.Convert.ToInt64(var_7)
            let (var_11: int64) = System.Convert.ToInt64('0')
            let (var_12: int64) = (var_10 - var_11)
            let (var_13: int64) = (0L + var_12)
            method_24((var_0: bool), (var_1: int64), (var_2: Env5), (var_3: string), (var_13: int64), (var_9: int64))
        else
            (failwith "pint64")
    else
        (failwith "pint64")
and method_24((var_0: bool), (var_1: int64), (var_2: Env5), (var_3: string), (var_4: int64), (var_5: int64)): unit =
    let (var_7: bool) =
        if (var_5 >= 0L) then
            let (var_6: int64) = (int64 var_3.Length)
            (var_5 < var_6)
        else
            false
    if var_7 then
        let (var_8: char) = var_3.[int32 var_5]
        let (var_9: bool) =
            if (var_8 >= '0') then
                (var_8 <= '9')
            else
                false
        let (var_10: int64) = (var_5 + 1L)
        if var_9 then
            let (var_11: int64) = System.Convert.ToInt64(var_8)
            let (var_12: int64) = System.Convert.ToInt64('0')
            let (var_13: int64) = (var_11 - var_12)
            let (var_14: bool) =
                if (var_4 = 922337203685477580L) then
                    (var_13 <= 7L)
                else
                    false
            let (var_15: bool) =
                if var_14 then
                    true
                else
                    (var_4 < 922337203685477580L)
            if var_15 then
                let (var_16: int64) = (var_4 * 10L)
                let (var_17: int64) = (var_16 + var_13)
                method_24((var_0: bool), (var_1: int64), (var_2: Env5), (var_3: string), (var_17: int64), (var_10: int64))
            else
                (failwith "integer overflow")
        else
            let (var_18: int64) =
                if var_0 then
                    var_4
                else
                    (-var_4)
            let (var_19: int64) = 0L
            method_25((var_18: int64), (var_1: int64), (var_2: Env5), (var_3: string), (var_19: int64), (var_5: int64))
    else
        let (var_20: int64) =
            if var_0 then
                var_4
            else
                (-var_4)
        let (var_21: int64) = 0L
        method_25((var_20: int64), (var_1: int64), (var_2: Env5), (var_3: string), (var_21: int64), (var_5: int64))
and method_25((var_0: int64), (var_1: int64), (var_2: Env5), (var_3: string), (var_4: int64), (var_5: int64)): unit =
    let (var_6: int64) = (var_4 + 1L)
    let (var_8: bool) =
        if (var_5 >= 0L) then
            let (var_7: int64) = (int64 var_3.Length)
            (var_5 < var_7)
        else
            false
    if var_8 then
        let (var_9: char) = var_3.[int32 var_5]
        let (var_11: bool) =
            if (var_9 = ' ') then
                true
            else
                if (var_9 = '\n') then
                    true
                else
                    (var_9 = '\r')
        let (var_12: int64) = (var_5 + 1L)
        if var_11 then
            method_25((var_0: int64), (var_1: int64), (var_2: Env5), (var_3: string), (var_6: int64), (var_12: int64))
        else
            let (var_14: bool) =
                if (var_5 >= 0L) then
                    let (var_13: int64) = (int64 var_3.Length)
                    (var_5 < var_13)
                else
                    false
            if var_14 then
                let (var_15: bool) = ('-' = var_9)
                if var_15 then
                    let (var_16: bool) = false
                    method_26((var_16: bool), (var_1: int64), (var_2: Env5), (var_3: string), (var_12: int64))
                else
                    let (var_17: bool) = true
                    method_26((var_17: bool), (var_1: int64), (var_2: Env5), (var_3: string), (var_5: int64))
            else
                let (var_18: bool) = true
                method_26((var_18: bool), (var_1: int64), (var_2: Env5), (var_3: string), (var_5: int64))
    else
        let (var_20: bool) =
            if (var_5 >= 0L) then
                let (var_19: int64) = (int64 var_3.Length)
                (var_5 < var_19)
            else
                false
        if var_20 then
            let (var_21: char) = var_3.[int32 var_5]
            let (var_22: bool) = ('-' = var_21)
            let (var_23: int64) = (var_5 + 1L)
            if var_22 then
                let (var_24: bool) = false
                method_26((var_24: bool), (var_1: int64), (var_2: Env5), (var_3: string), (var_23: int64))
            else
                let (var_25: bool) = true
                method_26((var_25: bool), (var_1: int64), (var_2: Env5), (var_3: string), (var_5: int64))
        else
            let (var_26: bool) = true
            method_26((var_26: bool), (var_1: int64), (var_2: Env5), (var_3: string), (var_5: int64))
and method_26((var_0: bool), (var_1: int64), (var_2: Env5), (var_3: string), (var_4: int64)): unit =
    let (var_6: bool) =
        if (var_4 >= 0L) then
            let (var_5: int64) = (int64 var_3.Length)
            (var_4 < var_5)
        else
            false
    if var_6 then
        let (var_7: char) = var_3.[int32 var_4]
        let (var_8: bool) =
            if (var_7 >= '0') then
                (var_7 <= '9')
            else
                false
        let (var_9: int64) = (var_4 + 1L)
        if var_8 then
            let (var_10: int64) = System.Convert.ToInt64(var_7)
            let (var_11: int64) = System.Convert.ToInt64('0')
            let (var_12: int64) = (var_10 - var_11)
            let (var_13: int64) = (0L + var_12)
            method_27((var_0: bool), (var_1: int64), (var_2: Env5), (var_3: string), (var_13: int64), (var_9: int64))
        else
            (failwith "pint64")
    else
        (failwith "pint64")
and method_27((var_0: bool), (var_1: int64), (var_2: Env5), (var_3: string), (var_4: int64), (var_5: int64)): unit =
    let (var_7: bool) =
        if (var_5 >= 0L) then
            let (var_6: int64) = (int64 var_3.Length)
            (var_5 < var_6)
        else
            false
    if var_7 then
        let (var_8: char) = var_3.[int32 var_5]
        let (var_9: bool) =
            if (var_8 >= '0') then
                (var_8 <= '9')
            else
                false
        let (var_10: int64) = (var_5 + 1L)
        if var_9 then
            let (var_11: int64) = System.Convert.ToInt64(var_8)
            let (var_12: int64) = System.Convert.ToInt64('0')
            let (var_13: int64) = (var_11 - var_12)
            let (var_14: bool) =
                if (var_4 = 922337203685477580L) then
                    (var_13 <= 7L)
                else
                    false
            let (var_15: bool) =
                if var_14 then
                    true
                else
                    (var_4 < 922337203685477580L)
            if var_15 then
                let (var_16: int64) = (var_4 * 10L)
                let (var_17: int64) = (var_16 + var_13)
                method_27((var_0: bool), (var_1: int64), (var_2: Env5), (var_3: string), (var_17: int64), (var_10: int64))
            else
                (failwith "integer overflow")
        else
            let (var_18: int64) =
                if var_0 then
                    var_4
                else
                    (-var_4)
            let (var_19: int64) = 0L
            method_28((var_18: int64), (var_1: int64), (var_2: Env5), (var_3: string), (var_19: int64), (var_5: int64))
    else
        let (var_20: int64) =
            if var_0 then
                var_4
            else
                (-var_4)
        let (var_21: int64) = 0L
        method_28((var_20: int64), (var_1: int64), (var_2: Env5), (var_3: string), (var_21: int64), (var_5: int64))
and method_28((var_0: int64), (var_1: int64), (var_2: Env5), (var_3: string), (var_4: int64), (var_5: int64)): unit =
    let (var_6: int64) = (var_4 + 1L)
    let (var_8: bool) =
        if (var_5 >= 0L) then
            let (var_7: int64) = (int64 var_3.Length)
            (var_5 < var_7)
        else
            false
    if var_8 then
        let (var_9: char) = var_3.[int32 var_5]
        let (var_11: bool) =
            if (var_9 = ' ') then
                true
            else
                if (var_9 = '\n') then
                    true
                else
                    (var_9 = '\r')
        let (var_12: int64) = (var_5 + 1L)
        if var_11 then
            method_28((var_0: int64), (var_1: int64), (var_2: Env5), (var_3: string), (var_6: int64), (var_12: int64))
        else
            let (var_13: bool) = (var_1 >= 0L)
            if var_13 then
                let (var_15: ((Union6 []) [])) = Array.zeroCreate<(Union6 [])> (System.Convert.ToInt32(var_1))
                let (var_16: int64) = 0L
                method_30((var_15: ((Union6 []) [])), (var_1: int64), (var_2: Env5), (var_3: string), (var_16: int64), (var_13: bool), (var_5: int64))
            else
                (failwith "n in parse array must be >= 0")
    else
        let (var_17: bool) = (var_1 >= 0L)
        if var_17 then
            let (var_18: ((Union6 []) [])) = Array.zeroCreate<(Union6 [])> (System.Convert.ToInt32(var_1))
            let (var_19: int64) = 0L
            method_30((var_18: ((Union6 []) [])), (var_1: int64), (var_2: Env5), (var_3: string), (var_19: int64), (var_17: bool), (var_5: int64))
        else
            (failwith "n in parse array must be >= 0")
and method_30((var_0: ((Union6 []) [])), (var_1: int64), (var_2: Env5), (var_3: string), (var_4: int64), (var_5: bool), (var_6: int64)): unit =
    let (var_7: bool) = (var_4 < var_1)
    if var_7 then
        if var_5 then
            let (var_8: (Union6 [])) = Array.zeroCreate<Union6> (System.Convert.ToInt32(var_1))
            let (var_9: int64) = 0L
            method_31((var_8: (Union6 [])), (var_0: ((Union6 []) [])), (var_4: int64), (var_1: int64), (var_5: bool), (var_2: Env5), (var_3: string), (var_9: int64), (var_6: int64))
        else
            (failwith "n in parse array must be >= 0")
    else
        let (var_10: int64) = 0L
        method_34((var_0: ((Union6 []) [])), (var_1: int64), (var_2: Env5), (var_10: int64))
and method_31((var_0: (Union6 [])), (var_1: ((Union6 []) [])), (var_2: int64), (var_3: int64), (var_4: bool), (var_5: Env5), (var_6: string), (var_7: int64), (var_8: int64)): unit =
    let (var_9: bool) = (var_7 < var_3)
    if var_9 then
        let (var_11: bool) =
            if (var_8 >= 0L) then
                let (var_10: int64) = (int64 var_6.Length)
                (var_8 < var_10)
            else
                false
        if var_11 then
            let (var_12: char) = var_6.[int32 var_8]
            let (var_13: bool) = ('-' = var_12)
            let (var_14: int64) = (var_8 + 1L)
            if var_13 then
                var_0.[int32 var_7] <- Union6Case0
                let (var_15: int64) = (var_7 + 1L)
                method_31((var_0: (Union6 [])), (var_1: ((Union6 []) [])), (var_2: int64), (var_3: int64), (var_4: bool), (var_5: Env5), (var_6: string), (var_15: int64), (var_14: int64))
            else
                let (var_17: bool) =
                    if (var_8 >= 0L) then
                        let (var_16: int64) = (int64 var_6.Length)
                        (var_8 < var_16)
                    else
                        false
                if var_17 then
                    let (var_18: bool) = ('p' = var_12)
                    if var_18 then
                        var_0.[int32 var_7] <- Union6Case2
                        let (var_19: int64) = (var_7 + 1L)
                        method_31((var_0: (Union6 [])), (var_1: ((Union6 []) [])), (var_2: int64), (var_3: int64), (var_4: bool), (var_5: Env5), (var_6: string), (var_19: int64), (var_14: int64))
                    else
                        let (var_21: bool) =
                            if (var_8 >= 0L) then
                                let (var_20: int64) = (int64 var_6.Length)
                                (var_8 < var_20)
                            else
                                false
                        if var_21 then
                            let (var_22: bool) = ('m' = var_12)
                            if var_22 then
                                var_0.[int32 var_7] <- Union6Case1
                                let (var_23: int64) = (var_7 + 1L)
                                method_31((var_0: (Union6 [])), (var_1: ((Union6 []) [])), (var_2: int64), (var_3: int64), (var_4: bool), (var_5: Env5), (var_6: string), (var_23: int64), (var_14: int64))
                            else
                                (failwith "char")
                        else
                            (failwith "string index out of bounds")
                else
                    let (var_25: bool) =
                        if (var_8 >= 0L) then
                            let (var_24: int64) = (int64 var_6.Length)
                            (var_8 < var_24)
                        else
                            false
                    if var_25 then
                        let (var_26: bool) = ('m' = var_12)
                        if var_26 then
                            var_0.[int32 var_7] <- Union6Case1
                            let (var_27: int64) = (var_7 + 1L)
                            method_31((var_0: (Union6 [])), (var_1: ((Union6 []) [])), (var_2: int64), (var_3: int64), (var_4: bool), (var_5: Env5), (var_6: string), (var_27: int64), (var_14: int64))
                        else
                            (failwith "char")
                    else
                        (failwith "string index out of bounds")
        else
            let (var_29: bool) =
                if (var_8 >= 0L) then
                    let (var_28: int64) = (int64 var_6.Length)
                    (var_8 < var_28)
                else
                    false
            if var_29 then
                let (var_30: char) = var_6.[int32 var_8]
                let (var_31: bool) = ('p' = var_30)
                let (var_32: int64) = (var_8 + 1L)
                if var_31 then
                    var_0.[int32 var_7] <- Union6Case2
                    let (var_33: int64) = (var_7 + 1L)
                    method_31((var_0: (Union6 [])), (var_1: ((Union6 []) [])), (var_2: int64), (var_3: int64), (var_4: bool), (var_5: Env5), (var_6: string), (var_33: int64), (var_32: int64))
                else
                    let (var_35: bool) =
                        if (var_8 >= 0L) then
                            let (var_34: int64) = (int64 var_6.Length)
                            (var_8 < var_34)
                        else
                            false
                    if var_35 then
                        let (var_36: bool) = ('m' = var_30)
                        if var_36 then
                            var_0.[int32 var_7] <- Union6Case1
                            let (var_37: int64) = (var_7 + 1L)
                            method_31((var_0: (Union6 [])), (var_1: ((Union6 []) [])), (var_2: int64), (var_3: int64), (var_4: bool), (var_5: Env5), (var_6: string), (var_37: int64), (var_32: int64))
                        else
                            (failwith "char")
                    else
                        (failwith "string index out of bounds")
            else
                let (var_39: bool) =
                    if (var_8 >= 0L) then
                        let (var_38: int64) = (int64 var_6.Length)
                        (var_8 < var_38)
                    else
                        false
                if var_39 then
                    let (var_40: char) = var_6.[int32 var_8]
                    let (var_41: bool) = ('m' = var_40)
                    let (var_42: int64) = (var_8 + 1L)
                    if var_41 then
                        var_0.[int32 var_7] <- Union6Case1
                        let (var_43: int64) = (var_7 + 1L)
                        method_31((var_0: (Union6 [])), (var_1: ((Union6 []) [])), (var_2: int64), (var_3: int64), (var_4: bool), (var_5: Env5), (var_6: string), (var_43: int64), (var_42: int64))
                    else
                        (failwith "char")
                else
                    (failwith "string index out of bounds")
    else
        let (var_44: int64) = 0L
        method_32((var_0: (Union6 [])), (var_1: ((Union6 []) [])), (var_2: int64), (var_3: int64), (var_4: bool), (var_5: Env5), (var_6: string), (var_44: int64), (var_8: int64))
and method_32((var_0: (Union6 [])), (var_1: ((Union6 []) [])), (var_2: int64), (var_3: int64), (var_4: bool), (var_5: Env5), (var_6: string), (var_7: int64), (var_8: int64)): unit =
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
            method_32((var_0: (Union6 [])), (var_1: ((Union6 []) [])), (var_2: int64), (var_3: int64), (var_4: bool), (var_5: Env5), (var_6: string), (var_9: int64), (var_15: int64))
        else
            var_1.[int32 var_2] <- var_0
            let (var_16: int64) = (var_2 + 1L)
            method_30((var_1: ((Union6 []) [])), (var_3: int64), (var_5: Env5), (var_6: string), (var_16: int64), (var_4: bool), (var_8: int64))
    else
        var_1.[int32 var_2] <- var_0
        let (var_17: int64) = (var_2 + 1L)
        method_30((var_1: ((Union6 []) [])), (var_3: int64), (var_5: Env5), (var_6: string), (var_17: int64), (var_4: bool), (var_8: int64))
and method_34((var_0: ((Union6 []) [])), (var_1: int64), (var_2: Env5), (var_3: int64)): unit =
    if (var_3 < var_1) then
        let (var_5: int64) = 0L
        method_36((var_0: ((Union6 []) [])), (var_3: int64), (var_2: Env5), (var_1: int64), (var_5: int64))
    else
        (failwith "Current position not found.")
and method_36((var_0: ((Union6 []) [])), (var_1: int64), (var_2: Env5), (var_3: int64), (var_4: int64)): unit =
    if (var_4 < var_3) then
        let (var_5: (Union6 [])) = var_0.[int32 var_1]
        let (var_6: Union6) = var_5.[int32 var_4]
        match var_6 with
        | Union6Case0 ->
            let (var_10: int64) = (var_4 + 1L)
            method_36((var_0: ((Union6 []) [])), (var_1: int64), (var_2: Env5), (var_3: int64), (var_10: int64))
        | Union6Case1 ->
            let (var_11: int64) = (var_4 + 1L)
            method_37((var_0: ((Union6 []) [])), (var_1: int64), (var_2: Env5), (var_3: int64), (var_11: int64), (var_4: int64))
        | Union6Case2 ->
            let (var_12: int64) = (var_4 + 1L)
            method_55((var_0: ((Union6 []) [])), (var_1: int64), (var_2: Env5), (var_3: int64), (var_12: int64), (var_4: int64))
    else
        let (var_13: int64) = (var_1 + 1L)
        method_34((var_0: ((Union6 []) [])), (var_3: int64), (var_2: Env5), (var_13: int64))
and method_37((var_0: ((Union6 []) [])), (var_1: int64), (var_2: Env5), (var_3: int64), (var_4: int64), (var_5: int64)): unit =
    if (var_4 < var_3) then
        let (var_6: (Union6 [])) = var_0.[int32 var_1]
        let (var_7: Union6) = var_6.[int32 var_4]
        match var_7 with
        | Union6Case0 ->
            let (var_11: int64) = (var_4 + 1L)
            method_37((var_0: ((Union6 []) [])), (var_1: int64), (var_2: Env5), (var_3: int64), (var_11: int64), (var_5: int64))
        | Union6Case1 ->
            let (var_12: int64) = (var_4 + 1L)
            method_37((var_0: ((Union6 []) [])), (var_1: int64), (var_2: Env5), (var_3: int64), (var_12: int64), (var_4: int64))
        | Union6Case2 ->
            let (var_13: Env4) = var_2.mem_ArrayN
            let (var_14: Env3) = var_13.mem_init
            let (var_15: Env2) = var_14.mem_for
            let (var_16: int64) = (var_3 * 1L)
            let (var_17: int64) = (var_3 * var_16)
            let (var_18: (bool [])) = Array.zeroCreate<bool> (System.Convert.ToInt32(var_17))
            let (var_19: int64) = 0L
            let (var_20: Env1) = var_15.mem_y
            let (var_21: int64) = 0L
            let (var_22: string) = var_20.mem_er_msg
            let (var_23: int64) = method_40((var_16: int64), (var_3: int64), (var_18: (bool [])), (var_15: Env2), (var_21: int64), (var_19: int64))
            let (var_24: bool) =
                if (var_1 >= 0L) then
                    (var_1 < var_3)
                else
                    false
            if (var_24 = false) then
                (failwith "Argument out of bounds.")
            else
                ()
            let (var_25: int64) = (var_16 * var_1)
            let (var_26: int64) = (0L + var_25)
            let (var_27: bool) =
                if (var_5 >= 0L) then
                    (var_5 < var_3)
                else
                    false
            if (var_27 = false) then
                (failwith "Argument out of bounds.")
            else
                ()
            let (var_28: int64) = (1L * var_5)
            let (var_29: int64) = (var_26 + var_28)
            var_18.[int32 var_29] <- true
            let (var_30: string) = "UP"
            let (var_31: string) = "DOWN"
            let (var_32: string) = "LEFT"
            let (var_33: string) = "RIGHT"
            ()
    else
        let (var_34: int64) = (var_1 + 1L)
        method_47((var_0: ((Union6 []) [])), (var_3: int64), (var_2: Env5), (var_34: int64), (var_1: int64), (var_5: int64))
and method_40((var_0: int64), (var_1: int64), (var_2: (bool [])), (var_3: Env2), (var_4: int64), (var_5: int64)): int64 =
    if (var_4 < var_1) then
        let (var_6: Env1) = var_3.mem_y
        let (var_7: int64) = 0L
        let (var_8: string) = var_6.mem_er_msg
        let (var_9: int64) = method_42((var_4: int64), (var_2: (bool [])), (var_3: Env2), (var_1: int64), (var_7: int64), (var_5: int64))
        let (var_10: int64) = (var_5 + var_0)
        let (var_11: int64) = (var_4 + 1L)
        method_40((var_0: int64), (var_1: int64), (var_2: (bool [])), (var_3: Env2), (var_11: int64), (var_10: int64))
    else
        var_5
and method_42((var_0: int64), (var_1: (bool [])), (var_2: Env2), (var_3: int64), (var_4: int64), (var_5: int64)): int64 =
    if (var_4 < var_3) then
        var_1.[int32 var_5] <- false
        let (var_6: int64) = (var_5 + 1L)
        let (var_7: int64) = (var_4 + 1L)
        method_42((var_0: int64), (var_1: (bool [])), (var_2: Env2), (var_3: int64), (var_7: int64), (var_6: int64))
    else
        var_5
and method_47((var_0: ((Union6 []) [])), (var_1: int64), (var_2: Env5), (var_3: int64), (var_4: int64), (var_5: int64)): unit =
    if (var_3 < var_1) then
        let (var_7: int64) = 0L
        method_49((var_0: ((Union6 []) [])), (var_3: int64), (var_2: Env5), (var_1: int64), (var_4: int64), (var_5: int64), (var_7: int64))
    else
        (failwith "Current position not found.")
and method_49((var_0: ((Union6 []) [])), (var_1: int64), (var_2: Env5), (var_3: int64), (var_4: int64), (var_5: int64), (var_6: int64)): unit =
    if (var_6 < var_3) then
        let (var_7: (Union6 [])) = var_0.[int32 var_1]
        let (var_8: Union6) = var_7.[int32 var_6]
        match var_8 with
        | Union6Case0 ->
            let (var_12: int64) = (var_6 + 1L)
            method_49((var_0: ((Union6 []) [])), (var_1: int64), (var_2: Env5), (var_3: int64), (var_4: int64), (var_5: int64), (var_12: int64))
        | Union6Case1 ->
            let (var_13: int64) = (var_6 + 1L)
            method_50((var_0: ((Union6 []) [])), (var_1: int64), (var_2: Env5), (var_3: int64), (var_4: int64), (var_5: int64), (var_13: int64), (var_6: int64))
        | Union6Case2 ->
            let (var_14: Env4) = var_2.mem_ArrayN
            let (var_15: Env3) = var_14.mem_init
            let (var_16: Env2) = var_15.mem_for
            let (var_17: int64) = (var_3 * 1L)
            let (var_18: int64) = (var_3 * var_17)
            let (var_19: (bool [])) = Array.zeroCreate<bool> (System.Convert.ToInt32(var_18))
            let (var_20: int64) = 0L
            let (var_21: Env1) = var_16.mem_y
            let (var_22: int64) = 0L
            let (var_23: string) = var_21.mem_er_msg
            let (var_24: int64) = method_40((var_17: int64), (var_3: int64), (var_19: (bool [])), (var_16: Env2), (var_22: int64), (var_20: int64))
            let (var_25: bool) =
                if (var_4 >= 0L) then
                    (var_4 < var_3)
                else
                    false
            if (var_25 = false) then
                (failwith "Argument out of bounds.")
            else
                ()
            let (var_26: int64) = (var_17 * var_4)
            let (var_27: int64) = (0L + var_26)
            let (var_28: bool) =
                if (var_5 >= 0L) then
                    (var_5 < var_3)
                else
                    false
            if (var_28 = false) then
                (failwith "Argument out of bounds.")
            else
                ()
            let (var_29: int64) = (1L * var_5)
            let (var_30: int64) = (var_27 + var_29)
            var_19.[int32 var_30] <- true
            let (var_31: string) = "UP"
            let (var_32: string) = "DOWN"
            let (var_33: string) = "LEFT"
            let (var_34: string) = "RIGHT"
            ()
    else
        let (var_35: int64) = (var_1 + 1L)
        method_47((var_0: ((Union6 []) [])), (var_3: int64), (var_2: Env5), (var_35: int64), (var_4: int64), (var_5: int64))
and method_50((var_0: ((Union6 []) [])), (var_1: int64), (var_2: Env5), (var_3: int64), (var_4: int64), (var_5: int64), (var_6: int64), (var_7: int64)): unit =
    if (var_6 < var_3) then
        let (var_8: (Union6 [])) = var_0.[int32 var_1]
        let (var_9: Union6) = var_8.[int32 var_6]
        match var_9 with
        | Union6Case0 ->
            let (var_13: int64) = (var_6 + 1L)
            method_50((var_0: ((Union6 []) [])), (var_1: int64), (var_2: Env5), (var_3: int64), (var_4: int64), (var_5: int64), (var_13: int64), (var_7: int64))
        | Union6Case1 ->
            let (var_14: int64) = (var_6 + 1L)
            method_50((var_0: ((Union6 []) [])), (var_1: int64), (var_2: Env5), (var_3: int64), (var_4: int64), (var_5: int64), (var_14: int64), (var_6: int64))
        | Union6Case2 ->
            let (var_15: Env4) = var_2.mem_ArrayN
            let (var_16: Env3) = var_15.mem_init
            let (var_17: Env2) = var_16.mem_for
            let (var_18: int64) = (var_3 * 1L)
            let (var_19: int64) = (var_3 * var_18)
            let (var_20: (bool [])) = Array.zeroCreate<bool> (System.Convert.ToInt32(var_19))
            let (var_21: int64) = 0L
            let (var_22: Env1) = var_17.mem_y
            let (var_23: int64) = 0L
            let (var_24: string) = var_22.mem_er_msg
            let (var_25: int64) = method_40((var_18: int64), (var_3: int64), (var_20: (bool [])), (var_17: Env2), (var_23: int64), (var_21: int64))
            let (var_26: bool) =
                if (var_1 >= 0L) then
                    (var_1 < var_3)
                else
                    false
            if (var_26 = false) then
                (failwith "Argument out of bounds.")
            else
                ()
            let (var_27: int64) = (var_18 * var_1)
            let (var_28: int64) = (0L + var_27)
            let (var_29: bool) =
                if (var_7 >= 0L) then
                    (var_7 < var_3)
                else
                    false
            if (var_29 = false) then
                (failwith "Argument out of bounds.")
            else
                ()
            let (var_30: int64) = (1L * var_7)
            let (var_31: int64) = (var_28 + var_30)
            var_20.[int32 var_31] <- true
            let (var_32: string) = "UP"
            let (var_33: string) = "DOWN"
            let (var_34: string) = "LEFT"
            let (var_35: string) = "RIGHT"
            ()
    else
        let (var_36: int64) = (var_1 + 1L)
        method_47((var_0: ((Union6 []) [])), (var_3: int64), (var_2: Env5), (var_36: int64), (var_1: int64), (var_7: int64))
and method_55((var_0: ((Union6 []) [])), (var_1: int64), (var_2: Env5), (var_3: int64), (var_4: int64), (var_5: int64)): unit =
    if (var_4 < var_3) then
        let (var_6: (Union6 [])) = var_0.[int32 var_1]
        let (var_7: Union6) = var_6.[int32 var_4]
        match var_7 with
        | Union6Case0 ->
            let (var_11: int64) = (var_4 + 1L)
            method_55((var_0: ((Union6 []) [])), (var_1: int64), (var_2: Env5), (var_3: int64), (var_11: int64), (var_5: int64))
        | Union6Case1 ->
            let (var_12: Env4) = var_2.mem_ArrayN
            let (var_13: Env3) = var_12.mem_init
            let (var_14: Env2) = var_13.mem_for
            let (var_15: int64) = (var_3 * 1L)
            let (var_16: int64) = (var_3 * var_15)
            let (var_17: (bool [])) = Array.zeroCreate<bool> (System.Convert.ToInt32(var_16))
            let (var_18: int64) = 0L
            let (var_19: Env1) = var_14.mem_y
            let (var_20: int64) = 0L
            let (var_21: string) = var_19.mem_er_msg
            let (var_22: int64) = method_40((var_15: int64), (var_3: int64), (var_17: (bool [])), (var_14: Env2), (var_20: int64), (var_18: int64))
            let (var_23: bool) =
                if (var_1 >= 0L) then
                    (var_1 < var_3)
                else
                    false
            if (var_23 = false) then
                (failwith "Argument out of bounds.")
            else
                ()
            let (var_24: int64) = (var_15 * var_1)
            let (var_25: int64) = (0L + var_24)
            let (var_26: bool) =
                if (var_4 >= 0L) then
                    (var_4 < var_3)
                else
                    false
            if (var_26 = false) then
                (failwith "Argument out of bounds.")
            else
                ()
            let (var_27: int64) = (1L * var_4)
            let (var_28: int64) = (var_25 + var_27)
            var_17.[int32 var_28] <- true
            let (var_29: string) = "UP"
            let (var_30: string) = "DOWN"
            let (var_31: string) = "LEFT"
            let (var_32: string) = "RIGHT"
            ()
        | Union6Case2 ->
            let (var_33: int64) = (var_4 + 1L)
            method_55((var_0: ((Union6 []) [])), (var_1: int64), (var_2: Env5), (var_3: int64), (var_33: int64), (var_4: int64))
    else
        let (var_34: int64) = (var_1 + 1L)
        method_58((var_0: ((Union6 []) [])), (var_3: int64), (var_2: Env5), (var_34: int64), (var_1: int64), (var_5: int64))
and method_58((var_0: ((Union6 []) [])), (var_1: int64), (var_2: Env5), (var_3: int64), (var_4: int64), (var_5: int64)): unit =
    if (var_3 < var_1) then
        let (var_7: int64) = 0L
        method_60((var_0: ((Union6 []) [])), (var_3: int64), (var_2: Env5), (var_1: int64), (var_4: int64), (var_5: int64), (var_7: int64))
    else
        (failwith "Current position not found.")
and method_60((var_0: ((Union6 []) [])), (var_1: int64), (var_2: Env5), (var_3: int64), (var_4: int64), (var_5: int64), (var_6: int64)): unit =
    if (var_6 < var_3) then
        let (var_7: (Union6 [])) = var_0.[int32 var_1]
        let (var_8: Union6) = var_7.[int32 var_6]
        match var_8 with
        | Union6Case0 ->
            let (var_12: int64) = (var_6 + 1L)
            method_60((var_0: ((Union6 []) [])), (var_1: int64), (var_2: Env5), (var_3: int64), (var_4: int64), (var_5: int64), (var_12: int64))
        | Union6Case1 ->
            let (var_13: Env4) = var_2.mem_ArrayN
            let (var_14: Env3) = var_13.mem_init
            let (var_15: Env2) = var_14.mem_for
            let (var_16: int64) = (var_3 * 1L)
            let (var_17: int64) = (var_3 * var_16)
            let (var_18: (bool [])) = Array.zeroCreate<bool> (System.Convert.ToInt32(var_17))
            let (var_19: int64) = 0L
            let (var_20: Env1) = var_15.mem_y
            let (var_21: int64) = 0L
            let (var_22: string) = var_20.mem_er_msg
            let (var_23: int64) = method_40((var_16: int64), (var_3: int64), (var_18: (bool [])), (var_15: Env2), (var_21: int64), (var_19: int64))
            let (var_24: bool) =
                if (var_1 >= 0L) then
                    (var_1 < var_3)
                else
                    false
            if (var_24 = false) then
                (failwith "Argument out of bounds.")
            else
                ()
            let (var_25: int64) = (var_16 * var_1)
            let (var_26: int64) = (0L + var_25)
            let (var_27: bool) =
                if (var_6 >= 0L) then
                    (var_6 < var_3)
                else
                    false
            if (var_27 = false) then
                (failwith "Argument out of bounds.")
            else
                ()
            let (var_28: int64) = (1L * var_6)
            let (var_29: int64) = (var_26 + var_28)
            var_18.[int32 var_29] <- true
            let (var_30: string) = "UP"
            let (var_31: string) = "DOWN"
            let (var_32: string) = "LEFT"
            let (var_33: string) = "RIGHT"
            ()
        | Union6Case2 ->
            let (var_34: int64) = (var_6 + 1L)
            method_61((var_0: ((Union6 []) [])), (var_1: int64), (var_2: Env5), (var_3: int64), (var_4: int64), (var_5: int64), (var_34: int64), (var_6: int64))
    else
        let (var_35: int64) = (var_1 + 1L)
        method_58((var_0: ((Union6 []) [])), (var_3: int64), (var_2: Env5), (var_35: int64), (var_4: int64), (var_5: int64))
and method_61((var_0: ((Union6 []) [])), (var_1: int64), (var_2: Env5), (var_3: int64), (var_4: int64), (var_5: int64), (var_6: int64), (var_7: int64)): unit =
    if (var_6 < var_3) then
        let (var_8: (Union6 [])) = var_0.[int32 var_1]
        let (var_9: Union6) = var_8.[int32 var_6]
        match var_9 with
        | Union6Case0 ->
            let (var_13: int64) = (var_6 + 1L)
            method_61((var_0: ((Union6 []) [])), (var_1: int64), (var_2: Env5), (var_3: int64), (var_4: int64), (var_5: int64), (var_13: int64), (var_7: int64))
        | Union6Case1 ->
            let (var_14: Env4) = var_2.mem_ArrayN
            let (var_15: Env3) = var_14.mem_init
            let (var_16: Env2) = var_15.mem_for
            let (var_17: int64) = (var_3 * 1L)
            let (var_18: int64) = (var_3 * var_17)
            let (var_19: (bool [])) = Array.zeroCreate<bool> (System.Convert.ToInt32(var_18))
            let (var_20: int64) = 0L
            let (var_21: Env1) = var_16.mem_y
            let (var_22: int64) = 0L
            let (var_23: string) = var_21.mem_er_msg
            let (var_24: int64) = method_40((var_17: int64), (var_3: int64), (var_19: (bool [])), (var_16: Env2), (var_22: int64), (var_20: int64))
            let (var_25: bool) =
                if (var_1 >= 0L) then
                    (var_1 < var_3)
                else
                    false
            if (var_25 = false) then
                (failwith "Argument out of bounds.")
            else
                ()
            let (var_26: int64) = (var_17 * var_1)
            let (var_27: int64) = (0L + var_26)
            let (var_28: bool) =
                if (var_6 >= 0L) then
                    (var_6 < var_3)
                else
                    false
            if (var_28 = false) then
                (failwith "Argument out of bounds.")
            else
                ()
            let (var_29: int64) = (1L * var_6)
            let (var_30: int64) = (var_27 + var_29)
            var_19.[int32 var_30] <- true
            let (var_31: string) = "UP"
            let (var_32: string) = "DOWN"
            let (var_33: string) = "LEFT"
            let (var_34: string) = "RIGHT"
            ()
        | Union6Case2 ->
            let (var_35: int64) = (var_6 + 1L)
            method_61((var_0: ((Union6 []) [])), (var_1: int64), (var_2: Env5), (var_3: int64), (var_4: int64), (var_5: int64), (var_35: int64), (var_6: int64))
    else
        let (var_36: int64) = (var_1 + 1L)
        method_58((var_0: ((Union6 []) [])), (var_3: int64), (var_2: Env5), (var_36: int64), (var_1: int64), (var_7: int64))
let (var_0: Env0) = method_19()
let (var_1: Env5) = var_0.mem_some
let (var_2: string) = "3
1 1
---
-m-
p--
    "
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
        method_20((var_9: bool), (var_1: Env5), (var_2: string), (var_8: int64))
    else
        let (var_10: bool) = true
        method_20((var_10: bool), (var_1: Env5), (var_2: string), (var_3: int64))
else
    let (var_11: bool) = true
    method_20((var_11: bool), (var_1: Env5), (var_2: string), (var_3: int64))

