let rec method_13((var_4: string), (var_5: int64), (var_6: bool), (var_7: int64)): int64 =
    let (var_13: bool) = (var_5 >= 0L)
    let (var_14: int64) = (int64 var_4.Length)
    let (var_15: bool) = (var_5 < var_14)
    let (if_var_1: int64) =
        if (var_13 && var_15) then
            let (var_17: char) = var_4.[int32 (var_5)]
            let (var_19: int64) = (var_5 + 1L)
            let (var_20: bool) = method_14((var_17: char))
            let (if_var_2: int64) =
                if var_20 then
                    let (var_22: int64) = System.Convert.ToInt64(var_17)
                    let (var_23: int64) = System.Convert.ToInt64('0')
                    let (var_24: int64) = (var_22 - var_23)
                    let (var_25: int64) = (var_7 * 10L)
                    let (var_26: int64) = (var_25 + var_24)
                    let (var_27: bool) = false
                    let (var_28: int64) = method_15((var_4: string), (var_19: int64), (var_26: int64), (var_27: bool))
                    var_28
                else
                    let (if_var_3: int64) =
                        if var_6 then
                            0L
                        else
                            let (var_33: int64) = method_16((var_4: string), (var_7: int64), (var_19: int64))
                            var_33
                    if_var_3
            if_var_2
        else
            let (if_var_4: int64) =
                if var_6 then
                    0L
                else
                    let (var_38: int64) = method_29((var_4: string), (var_5: int64), (var_7: int64))
                    var_38
            if_var_4
    if_var_1
and method_14((var_0: char)): bool =
    let (var_1: bool) = (var_0 >= '0')
    let (var_2: bool) = (var_0 <= '9')
    (var_1 && var_2)
and method_15((var_4: string), (var_5: int64), (var_6: int64), (var_7: bool)): int64 =
    let (var_13: bool) = (var_5 >= 0L)
    let (var_14: int64) = (int64 var_4.Length)
    let (var_15: bool) = (var_5 < var_14)
    let (if_var_5: int64) =
        if (var_13 && var_15) then
            let (var_17: char) = var_4.[int32 (var_5)]
            let (var_19: int64) = (var_5 + 1L)
            let (var_20: bool) = method_14((var_17: char))
            let (if_var_6: int64) =
                if var_20 then
                    let (var_22: int64) = System.Convert.ToInt64(var_17)
                    let (var_23: int64) = System.Convert.ToInt64('0')
                    let (var_24: int64) = (var_22 - var_23)
                    let (var_25: int64) = (var_6 * 10L)
                    let (var_26: int64) = (var_25 + var_24)
                    let (var_27: bool) = false
                    let (var_28: int64) = method_15((var_4: string), (var_19: int64), (var_26: int64), (var_27: bool))
                    var_28
                else
                    let (if_var_7: int64) =
                        if var_7 then
                            0L
                        else
                            let (var_33: int64) = method_16((var_4: string), (var_6: int64), (var_19: int64))
                            var_33
                    if_var_7
            if_var_6
        else
            let (if_var_8: int64) =
                if var_7 then
                    0L
                else
                    let (var_38: int64) = method_29((var_4: string), (var_5: int64), (var_6: int64))
                    var_38
            if_var_8
    if_var_5
and method_16((var_4: string), (var_5: int64), (var_6: int64)): int64 =
    let (var_11: bool) = (var_6 >= 0L)
    let (var_12: int64) = (int64 var_4.Length)
    let (var_13: bool) = (var_6 < var_12)
    let (if_var_9: int64) =
        if (var_11 && var_13) then
            let (var_15: char) = var_4.[int32 (var_6)]
            let (var_17: int64) = (var_6 + 1L)
            let (var_18: bool) = method_17((var_15: char))
            let (var_19: bool) = method_18((var_15: char))
            let (if_var_10: int64) =
                if (var_18 || var_19) then
                    let (var_20: int64) = method_16((var_4: string), (var_5: int64), (var_17: int64))
                    var_20
                else
                    let (var_22: int64) = (var_17 - 1L)
                    let (var_29: bool) = true
                    let (var_30: int64) = 0L
                    let (var_31: int64) = method_19((var_4: string), (var_5: int64), (var_22: int64), (var_29: bool), (var_30: int64))
                    var_31
            if_var_10
        else
            let (var_40: bool) = true
            let (var_41: int64) = 0L
            let (var_42: int64) = method_19((var_4: string), (var_5: int64), (var_6: int64), (var_40: bool), (var_41: int64))
            var_42
    if_var_9
and method_17((var_0: char)): bool =
    (var_0 = ' ')
and method_18((var_0: char)): bool =
    let (var_1: bool) = (var_0 = '\n')
    let (var_2: bool) = (var_0 = '\r')
    (var_1 || var_2)
and method_19((var_4: string), (var_5: int64), (var_6: int64), (var_7: bool), (var_8: int64)): int64 =
    let (var_14: bool) = (var_6 >= 0L)
    let (var_15: int64) = (int64 var_4.Length)
    let (var_16: bool) = (var_6 < var_15)
    let (if_var_11: int64) =
        if (var_14 && var_16) then
            let (var_18: char) = var_4.[int32 (var_6)]
            let (var_20: int64) = (var_6 + 1L)
            let (var_21: bool) = method_14((var_18: char))
            let (if_var_12: int64) =
                if var_21 then
                    let (var_23: int64) = System.Convert.ToInt64(var_18)
                    let (var_24: int64) = System.Convert.ToInt64('0')
                    let (var_25: int64) = (var_23 - var_24)
                    let (var_26: int64) = (var_8 * 10L)
                    let (var_27: int64) = (var_26 + var_25)
                    let (var_28: bool) = false
                    let (var_29: int64) = method_20((var_4: string), (var_5: int64), (var_20: int64), (var_27: int64), (var_28: bool))
                    var_29
                else
                    let (if_var_13: int64) =
                        if var_7 then
                            0L
                        else
                            let (var_34: int64) = method_21((var_4: string), (var_5: int64), (var_8: int64), (var_20: int64))
                            var_34
                    if_var_13
            if_var_12
        else
            let (if_var_14: int64) =
                if var_7 then
                    0L
                else
                    let (var_39: int64) = method_26((var_4: string), (var_5: int64), (var_6: int64), (var_8: int64))
                    var_39
            if_var_14
    if_var_11
and method_20((var_4: string), (var_5: int64), (var_6: int64), (var_7: int64), (var_8: bool)): int64 =
    let (var_14: bool) = (var_6 >= 0L)
    let (var_15: int64) = (int64 var_4.Length)
    let (var_16: bool) = (var_6 < var_15)
    let (if_var_15: int64) =
        if (var_14 && var_16) then
            let (var_18: char) = var_4.[int32 (var_6)]
            let (var_20: int64) = (var_6 + 1L)
            let (var_21: bool) = method_14((var_18: char))
            let (if_var_16: int64) =
                if var_21 then
                    let (var_23: int64) = System.Convert.ToInt64(var_18)
                    let (var_24: int64) = System.Convert.ToInt64('0')
                    let (var_25: int64) = (var_23 - var_24)
                    let (var_26: int64) = (var_7 * 10L)
                    let (var_27: int64) = (var_26 + var_25)
                    let (var_28: bool) = false
                    let (var_29: int64) = method_20((var_4: string), (var_5: int64), (var_20: int64), (var_27: int64), (var_28: bool))
                    var_29
                else
                    let (if_var_17: int64) =
                        if var_8 then
                            0L
                        else
                            let (var_34: int64) = method_21((var_4: string), (var_5: int64), (var_7: int64), (var_20: int64))
                            var_34
                    if_var_17
            if_var_16
        else
            let (if_var_18: int64) =
                if var_8 then
                    0L
                else
                    let (var_39: int64) = method_26((var_4: string), (var_5: int64), (var_6: int64), (var_7: int64))
                    var_39
            if_var_18
    if_var_15
and method_21((var_4: string), (var_5: int64), (var_6: int64), (var_7: int64)): int64 =
    let (var_12: bool) = (var_7 >= 0L)
    let (var_13: int64) = (int64 var_4.Length)
    let (var_14: bool) = (var_7 < var_13)
    let (if_var_19: int64) =
        if (var_12 && var_14) then
            let (var_16: char) = var_4.[int32 (var_7)]
            let (var_18: int64) = (var_7 + 1L)
            let (var_19: bool) = method_17((var_16: char))
            let (var_20: bool) = method_18((var_16: char))
            let (if_var_20: int64) =
                if (var_19 || var_20) then
                    let (var_21: int64) = method_21((var_4: string), (var_5: int64), (var_6: int64), (var_18: int64))
                    var_21
                else
                    let (var_23: int64) = (var_18 - 1L)
                    let (var_30: bool) = true
                    let (var_31: int64) = 0L
                    let (var_32: int64) = method_22((var_4: string), (var_5: int64), (var_6: int64), (var_23: int64), (var_30: bool), (var_31: int64))
                    var_32
            if_var_20
        else
            let (var_41: bool) = true
            let (var_42: int64) = 0L
            let (var_43: int64) = method_22((var_4: string), (var_5: int64), (var_6: int64), (var_7: int64), (var_41: bool), (var_42: int64))
            var_43
    if_var_19
and method_22((var_4: string), (var_5: int64), (var_6: int64), (var_7: int64), (var_8: bool), (var_9: int64)): int64 =
    let (var_15: bool) = (var_7 >= 0L)
    let (var_16: int64) = (int64 var_4.Length)
    let (var_17: bool) = (var_7 < var_16)
    let (if_var_21: int64) =
        if (var_15 && var_17) then
            let (var_19: char) = var_4.[int32 (var_7)]
            let (var_21: int64) = (var_7 + 1L)
            let (var_22: bool) = method_14((var_19: char))
            let (if_var_22: int64) =
                if var_22 then
                    let (var_24: int64) = System.Convert.ToInt64(var_19)
                    let (var_25: int64) = System.Convert.ToInt64('0')
                    let (var_26: int64) = (var_24 - var_25)
                    let (var_27: int64) = (var_9 * 10L)
                    let (var_28: int64) = (var_27 + var_26)
                    let (var_29: bool) = false
                    let (var_30: int64) = method_23((var_4: string), (var_5: int64), (var_6: int64), (var_21: int64), (var_28: int64), (var_29: bool))
                    var_30
                else
                    let (if_var_23: int64) =
                        if var_8 then
                            0L
                        else
                            let (var_35: int64) = method_24((var_4: string), (var_5: int64), (var_6: int64), (var_9: int64), (var_21: int64))
                            var_35
                    if_var_23
            if_var_22
        else
            let (if_var_24: int64) =
                if var_8 then
                    0L
                else
                    let (var_40: int64) = method_25((var_4: string), (var_5: int64), (var_6: int64), (var_7: int64), (var_9: int64))
                    var_40
            if_var_24
    if_var_21
and method_23((var_4: string), (var_5: int64), (var_6: int64), (var_7: int64), (var_8: int64), (var_9: bool)): int64 =
    let (var_15: bool) = (var_7 >= 0L)
    let (var_16: int64) = (int64 var_4.Length)
    let (var_17: bool) = (var_7 < var_16)
    let (if_var_25: int64) =
        if (var_15 && var_17) then
            let (var_19: char) = var_4.[int32 (var_7)]
            let (var_21: int64) = (var_7 + 1L)
            let (var_22: bool) = method_14((var_19: char))
            let (if_var_26: int64) =
                if var_22 then
                    let (var_24: int64) = System.Convert.ToInt64(var_19)
                    let (var_25: int64) = System.Convert.ToInt64('0')
                    let (var_26: int64) = (var_24 - var_25)
                    let (var_27: int64) = (var_8 * 10L)
                    let (var_28: int64) = (var_27 + var_26)
                    let (var_29: bool) = false
                    let (var_30: int64) = method_23((var_4: string), (var_5: int64), (var_6: int64), (var_21: int64), (var_28: int64), (var_29: bool))
                    var_30
                else
                    let (if_var_27: int64) =
                        if var_9 then
                            0L
                        else
                            let (var_35: int64) = method_24((var_4: string), (var_5: int64), (var_6: int64), (var_8: int64), (var_21: int64))
                            var_35
                    if_var_27
            if_var_26
        else
            let (if_var_28: int64) =
                if var_9 then
                    0L
                else
                    let (var_40: int64) = method_25((var_4: string), (var_5: int64), (var_6: int64), (var_7: int64), (var_8: int64))
                    var_40
            if_var_28
    if_var_25
and method_24((var_1: string), (var_2: int64), (var_3: int64), (var_4: int64), (var_5: int64)): int64 =
    let (var_10: bool) = (var_5 >= 0L)
    let (var_11: int64) = (int64 var_1.Length)
    let (var_12: bool) = (var_5 < var_11)
    let (if_var_29: int64) =
        if (var_10 && var_12) then
            let (var_14: char) = var_1.[int32 (var_5)]
            let (var_16: int64) = (var_5 + 1L)
            let (var_17: bool) = method_17((var_14: char))
            let (var_18: bool) = method_18((var_14: char))
            let (if_var_30: int64) =
                if (var_17 || var_18) then
                    let (var_19: int64) = method_24((var_1: string), (var_2: int64), (var_3: int64), (var_4: int64), (var_16: int64))
                    var_19
                else
                    let (var_21: int64) = (var_16 - 1L)
                    let (var_28: int64) = (0L + var_2)
                    let (var_29: int64) = (var_28 + var_3)
                    let (var_30: int64) = (var_29 + var_4)
                    var_30
            if_var_30
        else
            let (var_39: int64) = (0L + var_2)
            let (var_40: int64) = (var_39 + var_3)
            let (var_41: int64) = (var_40 + var_4)
            var_41
    if_var_29
and method_25((var_1: string), (var_2: int64), (var_3: int64), (var_4: int64), (var_5: int64)): int64 =
    let (var_10: bool) = (var_4 >= 0L)
    let (var_11: int64) = (int64 var_1.Length)
    let (var_12: bool) = (var_4 < var_11)
    let (if_var_31: int64) =
        if (var_10 && var_12) then
            let (var_14: char) = var_1.[int32 (var_4)]
            let (var_16: int64) = (var_4 + 1L)
            let (var_17: bool) = method_17((var_14: char))
            let (var_18: bool) = method_18((var_14: char))
            let (if_var_32: int64) =
                if (var_17 || var_18) then
                    let (var_19: int64) = method_24((var_1: string), (var_2: int64), (var_3: int64), (var_5: int64), (var_16: int64))
                    var_19
                else
                    let (var_21: int64) = (var_16 - 1L)
                    let (var_28: int64) = (0L + var_2)
                    let (var_29: int64) = (var_28 + var_3)
                    let (var_30: int64) = (var_29 + var_5)
                    var_30
            if_var_32
        else
            let (var_39: int64) = (0L + var_2)
            let (var_40: int64) = (var_39 + var_3)
            let (var_41: int64) = (var_40 + var_5)
            var_41
    if_var_31
and method_26((var_4: string), (var_5: int64), (var_6: int64), (var_7: int64)): int64 =
    let (var_12: bool) = (var_6 >= 0L)
    let (var_13: int64) = (int64 var_4.Length)
    let (var_14: bool) = (var_6 < var_13)
    let (if_var_33: int64) =
        if (var_12 && var_14) then
            let (var_16: char) = var_4.[int32 (var_6)]
            let (var_18: int64) = (var_6 + 1L)
            let (var_19: bool) = method_17((var_16: char))
            let (var_20: bool) = method_18((var_16: char))
            let (if_var_34: int64) =
                if (var_19 || var_20) then
                    let (var_21: int64) = method_21((var_4: string), (var_5: int64), (var_7: int64), (var_18: int64))
                    var_21
                else
                    let (var_23: int64) = (var_18 - 1L)
                    let (var_30: bool) = true
                    let (var_31: int64) = 0L
                    let (var_32: int64) = method_22((var_4: string), (var_5: int64), (var_7: int64), (var_23: int64), (var_30: bool), (var_31: int64))
                    var_32
            if_var_34
        else
            let (var_41: bool) = true
            let (var_42: int64) = 0L
            let (var_43: int64) = method_27((var_4: string), (var_5: int64), (var_6: int64), (var_7: int64), (var_41: bool), (var_42: int64))
            var_43
    if_var_33
and method_27((var_4: string), (var_5: int64), (var_6: int64), (var_7: int64), (var_8: bool), (var_9: int64)): int64 =
    let (var_15: bool) = (var_6 >= 0L)
    let (var_16: int64) = (int64 var_4.Length)
    let (var_17: bool) = (var_6 < var_16)
    let (if_var_35: int64) =
        if (var_15 && var_17) then
            let (var_19: char) = var_4.[int32 (var_6)]
            let (var_21: int64) = (var_6 + 1L)
            let (var_22: bool) = method_14((var_19: char))
            let (if_var_36: int64) =
                if var_22 then
                    let (var_24: int64) = System.Convert.ToInt64(var_19)
                    let (var_25: int64) = System.Convert.ToInt64('0')
                    let (var_26: int64) = (var_24 - var_25)
                    let (var_27: int64) = (var_9 * 10L)
                    let (var_28: int64) = (var_27 + var_26)
                    let (var_29: bool) = false
                    let (var_30: int64) = method_23((var_4: string), (var_5: int64), (var_7: int64), (var_21: int64), (var_28: int64), (var_29: bool))
                    var_30
                else
                    let (if_var_37: int64) =
                        if var_8 then
                            0L
                        else
                            let (var_35: int64) = method_24((var_4: string), (var_5: int64), (var_7: int64), (var_9: int64), (var_21: int64))
                            var_35
                    if_var_37
            if_var_36
        else
            let (if_var_38: int64) =
                if var_8 then
                    0L
                else
                    let (var_40: int64) = method_28((var_4: string), (var_5: int64), (var_6: int64), (var_7: int64), (var_9: int64))
                    var_40
            if_var_38
    if_var_35
and method_28((var_1: string), (var_2: int64), (var_3: int64), (var_4: int64), (var_5: int64)): int64 =
    let (var_10: bool) = (var_3 >= 0L)
    let (var_11: int64) = (int64 var_1.Length)
    let (var_12: bool) = (var_3 < var_11)
    let (if_var_39: int64) =
        if (var_10 && var_12) then
            let (var_14: char) = var_1.[int32 (var_3)]
            let (var_16: int64) = (var_3 + 1L)
            let (var_17: bool) = method_17((var_14: char))
            let (var_18: bool) = method_18((var_14: char))
            let (if_var_40: int64) =
                if (var_17 || var_18) then
                    let (var_19: int64) = method_24((var_1: string), (var_2: int64), (var_4: int64), (var_5: int64), (var_16: int64))
                    var_19
                else
                    let (var_21: int64) = (var_16 - 1L)
                    let (var_28: int64) = (0L + var_2)
                    let (var_29: int64) = (var_28 + var_4)
                    let (var_30: int64) = (var_29 + var_5)
                    var_30
            if_var_40
        else
            let (var_39: int64) = (0L + var_2)
            let (var_40: int64) = (var_39 + var_4)
            let (var_41: int64) = (var_40 + var_5)
            var_41
    if_var_39
and method_29((var_4: string), (var_5: int64), (var_6: int64)): int64 =
    let (var_11: bool) = (var_5 >= 0L)
    let (var_12: int64) = (int64 var_4.Length)
    let (var_13: bool) = (var_5 < var_12)
    let (if_var_41: int64) =
        if (var_11 && var_13) then
            let (var_15: char) = var_4.[int32 (var_5)]
            let (var_17: int64) = (var_5 + 1L)
            let (var_18: bool) = method_17((var_15: char))
            let (var_19: bool) = method_18((var_15: char))
            let (if_var_42: int64) =
                if (var_18 || var_19) then
                    let (var_20: int64) = method_16((var_4: string), (var_6: int64), (var_17: int64))
                    var_20
                else
                    let (var_22: int64) = (var_17 - 1L)
                    let (var_29: bool) = true
                    let (var_30: int64) = 0L
                    let (var_31: int64) = method_19((var_4: string), (var_6: int64), (var_22: int64), (var_29: bool), (var_30: int64))
                    var_31
            if_var_42
        else
            let (var_40: bool) = true
            let (var_41: int64) = 0L
            let (var_42: int64) = method_30((var_4: string), (var_5: int64), (var_6: int64), (var_40: bool), (var_41: int64))
            var_42
    if_var_41
and method_30((var_4: string), (var_5: int64), (var_6: int64), (var_7: bool), (var_8: int64)): int64 =
    let (var_14: bool) = (var_5 >= 0L)
    let (var_15: int64) = (int64 var_4.Length)
    let (var_16: bool) = (var_5 < var_15)
    let (if_var_43: int64) =
        if (var_14 && var_16) then
            let (var_18: char) = var_4.[int32 (var_5)]
            let (var_20: int64) = (var_5 + 1L)
            let (var_21: bool) = method_14((var_18: char))
            let (if_var_44: int64) =
                if var_21 then
                    let (var_23: int64) = System.Convert.ToInt64(var_18)
                    let (var_24: int64) = System.Convert.ToInt64('0')
                    let (var_25: int64) = (var_23 - var_24)
                    let (var_26: int64) = (var_8 * 10L)
                    let (var_27: int64) = (var_26 + var_25)
                    let (var_28: bool) = false
                    let (var_29: int64) = method_20((var_4: string), (var_6: int64), (var_20: int64), (var_27: int64), (var_28: bool))
                    var_29
                else
                    let (if_var_45: int64) =
                        if var_7 then
                            0L
                        else
                            let (var_34: int64) = method_21((var_4: string), (var_6: int64), (var_8: int64), (var_20: int64))
                            var_34
                    if_var_45
            if_var_44
        else
            let (if_var_46: int64) =
                if var_7 then
                    0L
                else
                    let (var_39: int64) = method_31((var_4: string), (var_5: int64), (var_6: int64), (var_8: int64))
                    var_39
            if_var_46
    if_var_43
and method_31((var_4: string), (var_5: int64), (var_6: int64), (var_7: int64)): int64 =
    let (var_12: bool) = (var_5 >= 0L)
    let (var_13: int64) = (int64 var_4.Length)
    let (var_14: bool) = (var_5 < var_13)
    let (if_var_47: int64) =
        if (var_12 && var_14) then
            let (var_16: char) = var_4.[int32 (var_5)]
            let (var_18: int64) = (var_5 + 1L)
            let (var_19: bool) = method_17((var_16: char))
            let (var_20: bool) = method_18((var_16: char))
            let (if_var_48: int64) =
                if (var_19 || var_20) then
                    let (var_21: int64) = method_21((var_4: string), (var_6: int64), (var_7: int64), (var_18: int64))
                    var_21
                else
                    let (var_23: int64) = (var_18 - 1L)
                    let (var_30: bool) = true
                    let (var_31: int64) = 0L
                    let (var_32: int64) = method_22((var_4: string), (var_6: int64), (var_7: int64), (var_23: int64), (var_30: bool), (var_31: int64))
                    var_32
            if_var_48
        else
            let (var_41: bool) = true
            let (var_42: int64) = 0L
            let (var_43: int64) = method_32((var_4: string), (var_5: int64), (var_6: int64), (var_7: int64), (var_41: bool), (var_42: int64))
            var_43
    if_var_47
and method_32((var_4: string), (var_5: int64), (var_6: int64), (var_7: int64), (var_8: bool), (var_9: int64)): int64 =
    let (var_15: bool) = (var_5 >= 0L)
    let (var_16: int64) = (int64 var_4.Length)
    let (var_17: bool) = (var_5 < var_16)
    let (if_var_49: int64) =
        if (var_15 && var_17) then
            let (var_19: char) = var_4.[int32 (var_5)]
            let (var_21: int64) = (var_5 + 1L)
            let (var_22: bool) = method_14((var_19: char))
            let (if_var_50: int64) =
                if var_22 then
                    let (var_24: int64) = System.Convert.ToInt64(var_19)
                    let (var_25: int64) = System.Convert.ToInt64('0')
                    let (var_26: int64) = (var_24 - var_25)
                    let (var_27: int64) = (var_9 * 10L)
                    let (var_28: int64) = (var_27 + var_26)
                    let (var_29: bool) = false
                    let (var_30: int64) = method_23((var_4: string), (var_6: int64), (var_7: int64), (var_21: int64), (var_28: int64), (var_29: bool))
                    var_30
                else
                    let (if_var_51: int64) =
                        if var_8 then
                            0L
                        else
                            let (var_35: int64) = method_24((var_4: string), (var_6: int64), (var_7: int64), (var_9: int64), (var_21: int64))
                            var_35
                    if_var_51
            if_var_50
        else
            let (if_var_52: int64) =
                if var_8 then
                    0L
                else
                    let (var_40: int64) = method_33((var_4: string), (var_5: int64), (var_6: int64), (var_7: int64), (var_9: int64))
                    var_40
            if_var_52
    if_var_49
and method_33((var_1: string), (var_2: int64), (var_3: int64), (var_4: int64), (var_5: int64)): int64 =
    let (var_10: bool) = (var_2 >= 0L)
    let (var_11: int64) = (int64 var_1.Length)
    let (var_12: bool) = (var_2 < var_11)
    let (if_var_53: int64) =
        if (var_10 && var_12) then
            let (var_14: char) = var_1.[int32 (var_2)]
            let (var_16: int64) = (var_2 + 1L)
            let (var_17: bool) = method_17((var_14: char))
            let (var_18: bool) = method_18((var_14: char))
            let (if_var_54: int64) =
                if (var_17 || var_18) then
                    let (var_19: int64) = method_24((var_1: string), (var_3: int64), (var_4: int64), (var_5: int64), (var_16: int64))
                    var_19
                else
                    let (var_21: int64) = (var_16 - 1L)
                    let (var_28: int64) = (0L + var_3)
                    let (var_29: int64) = (var_28 + var_4)
                    let (var_30: int64) = (var_29 + var_5)
                    var_30
            if_var_54
        else
            let (var_39: int64) = (0L + var_3)
            let (var_40: int64) = (var_39 + var_4)
            let (var_41: int64) = (var_40 + var_5)
            var_41
    if_var_53
let (var_22: string) = System.Console.ReadLine()
let (var_24: int64) = 0L
let (var_28: bool) = true
let (var_29: int64) = 0L
let (var_30: int64) = method_13((var_22: string), (var_24: int64), (var_28: bool), (var_29: int64))
var_30
