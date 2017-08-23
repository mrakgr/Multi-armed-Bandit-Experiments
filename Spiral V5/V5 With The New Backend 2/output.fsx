let rec method_13((var_4: string), (var_5: int64 ref), (var_7: int64)): int64 =
    let (var_14: int64) = (!var_5)
    let (var_16: bool) = (var_14 >= 0L)
    let (var_17: int64) = (int64 var_4.Length)
    let (var_18: bool) = (var_14 < var_17)
    let (if_var_1: int64) =
        if (var_16 && var_18) then
            let (var_20: char) = var_4.[int32 (var_14)]
            let (var_23: int64) = (!var_5)
            var_5 := (var_23 + 1L)
            let (var_26: bool) = method_14((var_20: char))
            let (if_var_2: int64) =
                if var_26 then
                    let (var_28: int64) = System.Convert.ToInt64(var_20)
                    let (var_29: int64) = System.Convert.ToInt64('0')
                    let (var_30: int64) = (var_28 - var_29)
                    let (var_31: int64) = (var_7 * 10L)
                    let (var_32: int64) = (var_31 + var_30)
                    let (var_34: int64) = method_15((var_4: string), (var_5: int64 ref), (var_32: int64))
                    var_34
                else
                    0L
            if_var_2
        else
            0L
    if_var_1
and method_14((var_0: char)): bool =
    let (var_1: bool) = (var_0 >= '0')
    let (var_2: bool) = (var_0 <= '9')
    (var_1 && var_2)
and method_15((var_4: string), (var_5: int64 ref), (var_6: int64)): int64 =
    let (var_14: int64) = (!var_5)
    let (var_16: bool) = (var_14 >= 0L)
    let (var_17: int64) = (int64 var_4.Length)
    let (var_18: bool) = (var_14 < var_17)
    let (if_var_3: int64) =
        if (var_16 && var_18) then
            let (var_20: char) = var_4.[int32 (var_14)]
            let (var_23: int64) = (!var_5)
            var_5 := (var_23 + 1L)
            let (var_26: bool) = method_14((var_20: char))
            let (if_var_4: int64) =
                if var_26 then
                    let (var_28: int64) = System.Convert.ToInt64(var_20)
                    let (var_29: int64) = System.Convert.ToInt64('0')
                    let (var_30: int64) = (var_28 - var_29)
                    let (var_31: int64) = (var_6 * 10L)
                    let (var_32: int64) = (var_31 + var_30)
                    let (var_34: int64) = method_15((var_4: string), (var_5: int64 ref), (var_32: int64))
                    var_34
                else
                    let (var_41: int64) = method_16((var_4: string), (var_5: int64 ref), (var_6: int64))
                    var_41
            if_var_4
        else
            let (var_47: int64) = method_16((var_4: string), (var_5: int64 ref), (var_6: int64))
            var_47
    if_var_3
and method_16((var_4: string), (var_5: int64 ref), (var_6: int64)): int64 =
    let (var_12: int64) = (!var_5)
    let (var_14: bool) = (var_12 >= 0L)
    let (var_15: int64) = (int64 var_4.Length)
    let (var_16: bool) = (var_12 < var_15)
    let (if_var_5: int64) =
        if (var_14 && var_16) then
            let (var_18: char) = var_4.[int32 (var_12)]
            let (var_21: int64) = (!var_5)
            var_5 := (var_21 + 1L)
            let (var_24: bool) = method_17((var_18: char))
            let (var_25: bool) = method_18((var_18: char))
            let (if_var_6: int64) =
                if (var_24 || var_25) then
                    let (var_26: int64) = method_16((var_4: string), (var_5: int64 ref), (var_6: int64))
                    var_26
                else
                    let (var_29: int64) = (!var_5)
                    var_5 := (var_29 + -1L)
                    let (var_39: int64) = 0L
                    let (var_40: int64) = method_19((var_4: string), (var_5: int64 ref), (var_6: int64), (var_39: int64))
                    var_40
            if_var_6
        else
            let (var_50: int64) = 0L
            let (var_51: int64) = method_19((var_4: string), (var_5: int64 ref), (var_6: int64), (var_50: int64))
            var_51
    if_var_5
and method_17((var_0: char)): bool =
    (var_0 = ' ')
and method_18((var_0: char)): bool =
    let (var_1: bool) = (var_0 = '\n')
    let (var_2: bool) = (var_0 = '\r')
    (var_1 || var_2)
and method_19((var_4: string), (var_5: int64 ref), (var_6: int64), (var_8: int64)): int64 =
    let (var_15: int64) = (!var_5)
    let (var_17: bool) = (var_15 >= 0L)
    let (var_18: int64) = (int64 var_4.Length)
    let (var_19: bool) = (var_15 < var_18)
    let (if_var_7: int64) =
        if (var_17 && var_19) then
            let (var_21: char) = var_4.[int32 (var_15)]
            let (var_24: int64) = (!var_5)
            var_5 := (var_24 + 1L)
            let (var_27: bool) = method_14((var_21: char))
            let (if_var_8: int64) =
                if var_27 then
                    let (var_29: int64) = System.Convert.ToInt64(var_21)
                    let (var_30: int64) = System.Convert.ToInt64('0')
                    let (var_31: int64) = (var_29 - var_30)
                    let (var_32: int64) = (var_8 * 10L)
                    let (var_33: int64) = (var_32 + var_31)
                    let (var_35: int64) = method_20((var_4: string), (var_5: int64 ref), (var_6: int64), (var_33: int64))
                    var_35
                else
                    0L
            if_var_8
        else
            0L
    if_var_7
and method_20((var_4: string), (var_5: int64 ref), (var_6: int64), (var_7: int64)): int64 =
    let (var_15: int64) = (!var_5)
    let (var_17: bool) = (var_15 >= 0L)
    let (var_18: int64) = (int64 var_4.Length)
    let (var_19: bool) = (var_15 < var_18)
    let (if_var_9: int64) =
        if (var_17 && var_19) then
            let (var_21: char) = var_4.[int32 (var_15)]
            let (var_24: int64) = (!var_5)
            var_5 := (var_24 + 1L)
            let (var_27: bool) = method_14((var_21: char))
            let (if_var_10: int64) =
                if var_27 then
                    let (var_29: int64) = System.Convert.ToInt64(var_21)
                    let (var_30: int64) = System.Convert.ToInt64('0')
                    let (var_31: int64) = (var_29 - var_30)
                    let (var_32: int64) = (var_7 * 10L)
                    let (var_33: int64) = (var_32 + var_31)
                    let (var_35: int64) = method_20((var_4: string), (var_5: int64 ref), (var_6: int64), (var_33: int64))
                    var_35
                else
                    let (var_42: int64) = method_21((var_4: string), (var_5: int64 ref), (var_6: int64), (var_7: int64))
                    var_42
            if_var_10
        else
            let (var_48: int64) = method_21((var_4: string), (var_5: int64 ref), (var_6: int64), (var_7: int64))
            var_48
    if_var_9
and method_21((var_4: string), (var_5: int64 ref), (var_6: int64), (var_7: int64)): int64 =
    let (var_13: int64) = (!var_5)
    let (var_15: bool) = (var_13 >= 0L)
    let (var_16: int64) = (int64 var_4.Length)
    let (var_17: bool) = (var_13 < var_16)
    let (if_var_11: int64) =
        if (var_15 && var_17) then
            let (var_19: char) = var_4.[int32 (var_13)]
            let (var_22: int64) = (!var_5)
            var_5 := (var_22 + 1L)
            let (var_25: bool) = method_17((var_19: char))
            let (var_26: bool) = method_18((var_19: char))
            let (if_var_12: int64) =
                if (var_25 || var_26) then
                    let (var_27: int64) = method_21((var_4: string), (var_5: int64 ref), (var_6: int64), (var_7: int64))
                    var_27
                else
                    let (var_30: int64) = (!var_5)
                    var_5 := (var_30 + -1L)
                    let (var_40: int64) = 0L
                    let (var_41: int64) = method_22((var_4: string), (var_5: int64 ref), (var_6: int64), (var_7: int64), (var_40: int64))
                    var_41
            if_var_12
        else
            let (var_51: int64) = 0L
            let (var_52: int64) = method_22((var_4: string), (var_5: int64 ref), (var_6: int64), (var_7: int64), (var_51: int64))
            var_52
    if_var_11
and method_22((var_4: string), (var_5: int64 ref), (var_6: int64), (var_7: int64), (var_9: int64)): int64 =
    let (var_16: int64) = (!var_5)
    let (var_18: bool) = (var_16 >= 0L)
    let (var_19: int64) = (int64 var_4.Length)
    let (var_20: bool) = (var_16 < var_19)
    let (if_var_13: int64) =
        if (var_18 && var_20) then
            let (var_22: char) = var_4.[int32 (var_16)]
            let (var_25: int64) = (!var_5)
            var_5 := (var_25 + 1L)
            let (var_28: bool) = method_14((var_22: char))
            let (if_var_14: int64) =
                if var_28 then
                    let (var_30: int64) = System.Convert.ToInt64(var_22)
                    let (var_31: int64) = System.Convert.ToInt64('0')
                    let (var_32: int64) = (var_30 - var_31)
                    let (var_33: int64) = (var_9 * 10L)
                    let (var_34: int64) = (var_33 + var_32)
                    let (var_36: int64) = method_23((var_4: string), (var_5: int64 ref), (var_6: int64), (var_7: int64), (var_34: int64))
                    var_36
                else
                    0L
            if_var_14
        else
            0L
    if_var_13
and method_23((var_4: string), (var_5: int64 ref), (var_6: int64), (var_7: int64), (var_8: int64)): int64 =
    let (var_16: int64) = (!var_5)
    let (var_18: bool) = (var_16 >= 0L)
    let (var_19: int64) = (int64 var_4.Length)
    let (var_20: bool) = (var_16 < var_19)
    let (if_var_15: int64) =
        if (var_18 && var_20) then
            let (var_22: char) = var_4.[int32 (var_16)]
            let (var_25: int64) = (!var_5)
            var_5 := (var_25 + 1L)
            let (var_28: bool) = method_14((var_22: char))
            let (if_var_16: int64) =
                if var_28 then
                    let (var_30: int64) = System.Convert.ToInt64(var_22)
                    let (var_31: int64) = System.Convert.ToInt64('0')
                    let (var_32: int64) = (var_30 - var_31)
                    let (var_33: int64) = (var_8 * 10L)
                    let (var_34: int64) = (var_33 + var_32)
                    let (var_36: int64) = method_23((var_4: string), (var_5: int64 ref), (var_6: int64), (var_7: int64), (var_34: int64))
                    var_36
                else
                    let (var_43: int64) = method_24((var_4: string), (var_5: int64 ref), (var_6: int64), (var_7: int64), (var_8: int64))
                    var_43
            if_var_16
        else
            let (var_49: int64) = method_24((var_4: string), (var_5: int64 ref), (var_6: int64), (var_7: int64), (var_8: int64))
            var_49
    if_var_15
and method_24((var_4: string), (var_5: int64 ref), (var_6: int64), (var_7: int64), (var_8: int64)): int64 =
    let (var_14: int64) = (!var_5)
    let (var_16: bool) = (var_14 >= 0L)
    let (var_17: int64) = (int64 var_4.Length)
    let (var_18: bool) = (var_14 < var_17)
    let (if_var_17: int64) =
        if (var_16 && var_18) then
            let (var_20: char) = var_4.[int32 (var_14)]
            let (var_23: int64) = (!var_5)
            var_5 := (var_23 + 1L)
            let (var_26: bool) = method_17((var_20: char))
            let (var_27: bool) = method_18((var_20: char))
            let (if_var_18: int64) =
                if (var_26 || var_27) then
                    let (var_28: int64) = method_24((var_4: string), (var_5: int64 ref), (var_6: int64), (var_7: int64), (var_8: int64))
                    var_28
                else
                    let (var_31: int64) = (!var_5)
                    var_5 := (var_31 + -1L)
                    let (var_41: int64) = 0L
                    let (var_42: int64) = method_25((var_4: string), (var_5: int64 ref), (var_6: int64), (var_7: int64), (var_8: int64), (var_41: int64))
                    var_42
            if_var_18
        else
            let (var_52: int64) = 0L
            let (var_53: int64) = method_25((var_4: string), (var_5: int64 ref), (var_6: int64), (var_7: int64), (var_8: int64), (var_52: int64))
            var_53
    if_var_17
and method_25((var_4: string), (var_5: int64 ref), (var_6: int64), (var_7: int64), (var_8: int64), (var_10: int64)): int64 =
    let (var_17: int64) = (!var_5)
    let (var_19: bool) = (var_17 >= 0L)
    let (var_20: int64) = (int64 var_4.Length)
    let (var_21: bool) = (var_17 < var_20)
    let (if_var_19: int64) =
        if (var_19 && var_21) then
            let (var_23: char) = var_4.[int32 (var_17)]
            let (var_26: int64) = (!var_5)
            var_5 := (var_26 + 1L)
            let (var_29: bool) = method_14((var_23: char))
            let (if_var_20: int64) =
                if var_29 then
                    let (var_31: int64) = System.Convert.ToInt64(var_23)
                    let (var_32: int64) = System.Convert.ToInt64('0')
                    let (var_33: int64) = (var_31 - var_32)
                    let (var_34: int64) = (var_10 * 10L)
                    let (var_35: int64) = (var_34 + var_33)
                    let (var_37: int64) = method_26((var_4: string), (var_5: int64 ref), (var_6: int64), (var_7: int64), (var_8: int64), (var_35: int64))
                    var_37
                else
                    0L
            if_var_20
        else
            0L
    if_var_19
and method_26((var_4: string), (var_5: int64 ref), (var_6: int64), (var_7: int64), (var_8: int64), (var_9: int64)): int64 =
    let (var_17: int64) = (!var_5)
    let (var_19: bool) = (var_17 >= 0L)
    let (var_20: int64) = (int64 var_4.Length)
    let (var_21: bool) = (var_17 < var_20)
    let (if_var_21: int64) =
        if (var_19 && var_21) then
            let (var_23: char) = var_4.[int32 (var_17)]
            let (var_26: int64) = (!var_5)
            var_5 := (var_26 + 1L)
            let (var_29: bool) = method_14((var_23: char))
            let (if_var_22: int64) =
                if var_29 then
                    let (var_31: int64) = System.Convert.ToInt64(var_23)
                    let (var_32: int64) = System.Convert.ToInt64('0')
                    let (var_33: int64) = (var_31 - var_32)
                    let (var_34: int64) = (var_9 * 10L)
                    let (var_35: int64) = (var_34 + var_33)
                    let (var_37: int64) = method_26((var_4: string), (var_5: int64 ref), (var_6: int64), (var_7: int64), (var_8: int64), (var_35: int64))
                    var_37
                else
                    let (var_44: int64) = method_27((var_4: string), (var_5: int64 ref), (var_6: int64), (var_7: int64), (var_8: int64), (var_9: int64))
                    var_44
            if_var_22
        else
            let (var_50: int64) = method_27((var_4: string), (var_5: int64 ref), (var_6: int64), (var_7: int64), (var_8: int64), (var_9: int64))
            var_50
    if_var_21
and method_27((var_4: string), (var_5: int64 ref), (var_6: int64), (var_7: int64), (var_8: int64), (var_9: int64)): int64 =
    let (var_15: int64) = (!var_5)
    let (var_17: bool) = (var_15 >= 0L)
    let (var_18: int64) = (int64 var_4.Length)
    let (var_19: bool) = (var_15 < var_18)
    let (if_var_23: int64) =
        if (var_17 && var_19) then
            let (var_21: char) = var_4.[int32 (var_15)]
            let (var_24: int64) = (!var_5)
            var_5 := (var_24 + 1L)
            let (var_27: bool) = method_17((var_21: char))
            let (var_28: bool) = method_18((var_21: char))
            let (if_var_24: int64) =
                if (var_27 || var_28) then
                    let (var_29: int64) = method_27((var_4: string), (var_5: int64 ref), (var_6: int64), (var_7: int64), (var_8: int64), (var_9: int64))
                    var_29
                else
                    let (var_32: int64) = (!var_5)
                    var_5 := (var_32 + -1L)
                    let (var_42: int64) = 0L
                    let (var_43: int64) = method_28((var_4: string), (var_5: int64 ref), (var_6: int64), (var_7: int64), (var_8: int64), (var_9: int64), (var_42: int64))
                    var_43
            if_var_24
        else
            let (var_53: int64) = 0L
            let (var_54: int64) = method_28((var_4: string), (var_5: int64 ref), (var_6: int64), (var_7: int64), (var_8: int64), (var_9: int64), (var_53: int64))
            var_54
    if_var_23
and method_28((var_4: string), (var_5: int64 ref), (var_6: int64), (var_7: int64), (var_8: int64), (var_9: int64), (var_11: int64)): int64 =
    let (var_18: int64) = (!var_5)
    let (var_20: bool) = (var_18 >= 0L)
    let (var_21: int64) = (int64 var_4.Length)
    let (var_22: bool) = (var_18 < var_21)
    let (if_var_25: int64) =
        if (var_20 && var_22) then
            let (var_24: char) = var_4.[int32 (var_18)]
            let (var_27: int64) = (!var_5)
            var_5 := (var_27 + 1L)
            let (var_30: bool) = method_14((var_24: char))
            let (if_var_26: int64) =
                if var_30 then
                    let (var_32: int64) = System.Convert.ToInt64(var_24)
                    let (var_33: int64) = System.Convert.ToInt64('0')
                    let (var_34: int64) = (var_32 - var_33)
                    let (var_35: int64) = (var_11 * 10L)
                    let (var_36: int64) = (var_35 + var_34)
                    let (var_38: int64) = method_29((var_4: string), (var_5: int64 ref), (var_6: int64), (var_7: int64), (var_8: int64), (var_9: int64), (var_36: int64))
                    var_38
                else
                    0L
            if_var_26
        else
            0L
    if_var_25
and method_29((var_4: string), (var_5: int64 ref), (var_6: int64), (var_7: int64), (var_8: int64), (var_9: int64), (var_10: int64)): int64 =
    let (var_18: int64) = (!var_5)
    let (var_20: bool) = (var_18 >= 0L)
    let (var_21: int64) = (int64 var_4.Length)
    let (var_22: bool) = (var_18 < var_21)
    let (if_var_27: int64) =
        if (var_20 && var_22) then
            let (var_24: char) = var_4.[int32 (var_18)]
            let (var_27: int64) = (!var_5)
            var_5 := (var_27 + 1L)
            let (var_30: bool) = method_14((var_24: char))
            let (if_var_28: int64) =
                if var_30 then
                    let (var_32: int64) = System.Convert.ToInt64(var_24)
                    let (var_33: int64) = System.Convert.ToInt64('0')
                    let (var_34: int64) = (var_32 - var_33)
                    let (var_35: int64) = (var_10 * 10L)
                    let (var_36: int64) = (var_35 + var_34)
                    let (var_38: int64) = method_29((var_4: string), (var_5: int64 ref), (var_6: int64), (var_7: int64), (var_8: int64), (var_9: int64), (var_36: int64))
                    var_38
                else
                    let (var_45: int64) = method_30((var_4: string), (var_5: int64 ref), (var_6: int64), (var_7: int64), (var_8: int64), (var_9: int64), (var_10: int64))
                    var_45
            if_var_28
        else
            let (var_51: int64) = method_30((var_4: string), (var_5: int64 ref), (var_6: int64), (var_7: int64), (var_8: int64), (var_9: int64), (var_10: int64))
            var_51
    if_var_27
and method_30((var_4: string), (var_5: int64 ref), (var_6: int64), (var_7: int64), (var_8: int64), (var_9: int64), (var_10: int64)): int64 =
    let (var_16: int64) = (!var_5)
    let (var_18: bool) = (var_16 >= 0L)
    let (var_19: int64) = (int64 var_4.Length)
    let (var_20: bool) = (var_16 < var_19)
    let (if_var_29: int64) =
        if (var_18 && var_20) then
            let (var_22: char) = var_4.[int32 (var_16)]
            let (var_25: int64) = (!var_5)
            var_5 := (var_25 + 1L)
            let (var_28: bool) = method_17((var_22: char))
            let (var_29: bool) = method_18((var_22: char))
            let (if_var_30: int64) =
                if (var_28 || var_29) then
                    let (var_30: int64) = method_30((var_4: string), (var_5: int64 ref), (var_6: int64), (var_7: int64), (var_8: int64), (var_9: int64), (var_10: int64))
                    var_30
                else
                    let (var_33: int64) = (!var_5)
                    var_5 := (var_33 + -1L)
                    let (var_43: int64) = 0L
                    let (var_44: int64) = method_31((var_4: string), (var_5: int64 ref), (var_6: int64), (var_7: int64), (var_8: int64), (var_9: int64), (var_10: int64), (var_43: int64))
                    var_44
            if_var_30
        else
            let (var_54: int64) = 0L
            let (var_55: int64) = method_31((var_4: string), (var_5: int64 ref), (var_6: int64), (var_7: int64), (var_8: int64), (var_9: int64), (var_10: int64), (var_54: int64))
            var_55
    if_var_29
and method_31((var_4: string), (var_5: int64 ref), (var_6: int64), (var_7: int64), (var_8: int64), (var_9: int64), (var_10: int64), (var_12: int64)): int64 =
    let (var_19: int64) = (!var_5)
    let (var_21: bool) = (var_19 >= 0L)
    let (var_22: int64) = (int64 var_4.Length)
    let (var_23: bool) = (var_19 < var_22)
    let (if_var_31: int64) =
        if (var_21 && var_23) then
            let (var_25: char) = var_4.[int32 (var_19)]
            let (var_28: int64) = (!var_5)
            var_5 := (var_28 + 1L)
            let (var_31: bool) = method_14((var_25: char))
            let (if_var_32: int64) =
                if var_31 then
                    let (var_33: int64) = System.Convert.ToInt64(var_25)
                    let (var_34: int64) = System.Convert.ToInt64('0')
                    let (var_35: int64) = (var_33 - var_34)
                    let (var_36: int64) = (var_12 * 10L)
                    let (var_37: int64) = (var_36 + var_35)
                    let (var_39: int64) = method_32((var_4: string), (var_5: int64 ref), (var_6: int64), (var_7: int64), (var_8: int64), (var_9: int64), (var_10: int64), (var_37: int64))
                    var_39
                else
                    0L
            if_var_32
        else
            0L
    if_var_31
and method_32((var_4: string), (var_5: int64 ref), (var_6: int64), (var_7: int64), (var_8: int64), (var_9: int64), (var_10: int64), (var_11: int64)): int64 =
    let (var_19: int64) = (!var_5)
    let (var_21: bool) = (var_19 >= 0L)
    let (var_22: int64) = (int64 var_4.Length)
    let (var_23: bool) = (var_19 < var_22)
    let (if_var_33: int64) =
        if (var_21 && var_23) then
            let (var_25: char) = var_4.[int32 (var_19)]
            let (var_28: int64) = (!var_5)
            var_5 := (var_28 + 1L)
            let (var_31: bool) = method_14((var_25: char))
            let (if_var_34: int64) =
                if var_31 then
                    let (var_33: int64) = System.Convert.ToInt64(var_25)
                    let (var_34: int64) = System.Convert.ToInt64('0')
                    let (var_35: int64) = (var_33 - var_34)
                    let (var_36: int64) = (var_11 * 10L)
                    let (var_37: int64) = (var_36 + var_35)
                    let (var_39: int64) = method_32((var_4: string), (var_5: int64 ref), (var_6: int64), (var_7: int64), (var_8: int64), (var_9: int64), (var_10: int64), (var_37: int64))
                    var_39
                else
                    let (var_46: int64) = method_33((var_4: string), (var_5: int64 ref), (var_6: int64), (var_7: int64), (var_8: int64), (var_9: int64), (var_10: int64), (var_11: int64))
                    var_46
            if_var_34
        else
            let (var_52: int64) = method_33((var_4: string), (var_5: int64 ref), (var_6: int64), (var_7: int64), (var_8: int64), (var_9: int64), (var_10: int64), (var_11: int64))
            var_52
    if_var_33
and method_33((var_4: string), (var_5: int64 ref), (var_6: int64), (var_7: int64), (var_8: int64), (var_9: int64), (var_10: int64), (var_11: int64)): int64 =
    let (var_17: int64) = (!var_5)
    let (var_19: bool) = (var_17 >= 0L)
    let (var_20: int64) = (int64 var_4.Length)
    let (var_21: bool) = (var_17 < var_20)
    let (if_var_35: int64) =
        if (var_19 && var_21) then
            let (var_23: char) = var_4.[int32 (var_17)]
            let (var_26: int64) = (!var_5)
            var_5 := (var_26 + 1L)
            let (var_29: bool) = method_17((var_23: char))
            let (var_30: bool) = method_18((var_23: char))
            let (if_var_36: int64) =
                if (var_29 || var_30) then
                    let (var_31: int64) = method_33((var_4: string), (var_5: int64 ref), (var_6: int64), (var_7: int64), (var_8: int64), (var_9: int64), (var_10: int64), (var_11: int64))
                    var_31
                else
                    let (var_34: int64) = (!var_5)
                    var_5 := (var_34 + -1L)
                    let (var_44: int64) = 0L
                    let (var_45: int64) = method_34((var_4: string), (var_5: int64 ref), (var_6: int64), (var_7: int64), (var_8: int64), (var_9: int64), (var_10: int64), (var_11: int64), (var_44: int64))
                    var_45
            if_var_36
        else
            let (var_55: int64) = 0L
            let (var_56: int64) = method_34((var_4: string), (var_5: int64 ref), (var_6: int64), (var_7: int64), (var_8: int64), (var_9: int64), (var_10: int64), (var_11: int64), (var_55: int64))
            var_56
    if_var_35
and method_34((var_4: string), (var_5: int64 ref), (var_6: int64), (var_7: int64), (var_8: int64), (var_9: int64), (var_10: int64), (var_11: int64), (var_13: int64)): int64 =
    let (var_20: int64) = (!var_5)
    let (var_22: bool) = (var_20 >= 0L)
    let (var_23: int64) = (int64 var_4.Length)
    let (var_24: bool) = (var_20 < var_23)
    let (if_var_37: int64) =
        if (var_22 && var_24) then
            let (var_26: char) = var_4.[int32 (var_20)]
            let (var_29: int64) = (!var_5)
            var_5 := (var_29 + 1L)
            let (var_32: bool) = method_14((var_26: char))
            let (if_var_38: int64) =
                if var_32 then
                    let (var_34: int64) = System.Convert.ToInt64(var_26)
                    let (var_35: int64) = System.Convert.ToInt64('0')
                    let (var_36: int64) = (var_34 - var_35)
                    let (var_37: int64) = (var_13 * 10L)
                    let (var_38: int64) = (var_37 + var_36)
                    let (var_40: int64) = method_35((var_4: string), (var_5: int64 ref), (var_6: int64), (var_7: int64), (var_8: int64), (var_9: int64), (var_10: int64), (var_11: int64), (var_38: int64))
                    var_40
                else
                    0L
            if_var_38
        else
            0L
    if_var_37
and method_35((var_4: string), (var_5: int64 ref), (var_6: int64), (var_7: int64), (var_8: int64), (var_9: int64), (var_10: int64), (var_11: int64), (var_12: int64)): int64 =
    let (var_20: int64) = (!var_5)
    let (var_22: bool) = (var_20 >= 0L)
    let (var_23: int64) = (int64 var_4.Length)
    let (var_24: bool) = (var_20 < var_23)
    let (if_var_39: int64) =
        if (var_22 && var_24) then
            let (var_26: char) = var_4.[int32 (var_20)]
            let (var_29: int64) = (!var_5)
            var_5 := (var_29 + 1L)
            let (var_32: bool) = method_14((var_26: char))
            let (if_var_40: int64) =
                if var_32 then
                    let (var_34: int64) = System.Convert.ToInt64(var_26)
                    let (var_35: int64) = System.Convert.ToInt64('0')
                    let (var_36: int64) = (var_34 - var_35)
                    let (var_37: int64) = (var_12 * 10L)
                    let (var_38: int64) = (var_37 + var_36)
                    let (var_40: int64) = method_35((var_4: string), (var_5: int64 ref), (var_6: int64), (var_7: int64), (var_8: int64), (var_9: int64), (var_10: int64), (var_11: int64), (var_38: int64))
                    var_40
                else
                    let (var_47: int64) = method_36((var_4: string), (var_5: int64 ref), (var_6: int64), (var_7: int64), (var_8: int64), (var_9: int64), (var_10: int64), (var_11: int64), (var_12: int64))
                    var_47
            if_var_40
        else
            let (var_53: int64) = method_36((var_4: string), (var_5: int64 ref), (var_6: int64), (var_7: int64), (var_8: int64), (var_9: int64), (var_10: int64), (var_11: int64), (var_12: int64))
            var_53
    if_var_39
and method_36((var_4: string), (var_5: int64 ref), (var_6: int64), (var_7: int64), (var_8: int64), (var_9: int64), (var_10: int64), (var_11: int64), (var_12: int64)): int64 =
    let (var_18: int64) = (!var_5)
    let (var_20: bool) = (var_18 >= 0L)
    let (var_21: int64) = (int64 var_4.Length)
    let (var_22: bool) = (var_18 < var_21)
    let (if_var_41: int64) =
        if (var_20 && var_22) then
            let (var_24: char) = var_4.[int32 (var_18)]
            let (var_27: int64) = (!var_5)
            var_5 := (var_27 + 1L)
            let (var_30: bool) = method_17((var_24: char))
            let (var_31: bool) = method_18((var_24: char))
            let (if_var_42: int64) =
                if (var_30 || var_31) then
                    let (var_32: int64) = method_36((var_4: string), (var_5: int64 ref), (var_6: int64), (var_7: int64), (var_8: int64), (var_9: int64), (var_10: int64), (var_11: int64), (var_12: int64))
                    var_32
                else
                    let (var_35: int64) = (!var_5)
                    var_5 := (var_35 + -1L)
                    let (var_45: int64) = 0L
                    let (var_46: int64) = method_37((var_4: string), (var_5: int64 ref), (var_6: int64), (var_7: int64), (var_8: int64), (var_9: int64), (var_10: int64), (var_11: int64), (var_12: int64), (var_45: int64))
                    var_46
            if_var_42
        else
            let (var_56: int64) = 0L
            let (var_57: int64) = method_37((var_4: string), (var_5: int64 ref), (var_6: int64), (var_7: int64), (var_8: int64), (var_9: int64), (var_10: int64), (var_11: int64), (var_12: int64), (var_56: int64))
            var_57
    if_var_41
and method_37((var_4: string), (var_5: int64 ref), (var_6: int64), (var_7: int64), (var_8: int64), (var_9: int64), (var_10: int64), (var_11: int64), (var_12: int64), (var_14: int64)): int64 =
    let (var_21: int64) = (!var_5)
    let (var_23: bool) = (var_21 >= 0L)
    let (var_24: int64) = (int64 var_4.Length)
    let (var_25: bool) = (var_21 < var_24)
    let (if_var_43: int64) =
        if (var_23 && var_25) then
            let (var_27: char) = var_4.[int32 (var_21)]
            let (var_30: int64) = (!var_5)
            var_5 := (var_30 + 1L)
            let (var_33: bool) = method_14((var_27: char))
            let (if_var_44: int64) =
                if var_33 then
                    let (var_35: int64) = System.Convert.ToInt64(var_27)
                    let (var_36: int64) = System.Convert.ToInt64('0')
                    let (var_37: int64) = (var_35 - var_36)
                    let (var_38: int64) = (var_14 * 10L)
                    let (var_39: int64) = (var_38 + var_37)
                    let (var_41: int64) = method_38((var_4: string), (var_5: int64 ref), (var_6: int64), (var_7: int64), (var_8: int64), (var_9: int64), (var_10: int64), (var_11: int64), (var_12: int64), (var_39: int64))
                    var_41
                else
                    0L
            if_var_44
        else
            0L
    if_var_43
and method_38((var_4: string), (var_5: int64 ref), (var_6: int64), (var_7: int64), (var_8: int64), (var_9: int64), (var_10: int64), (var_11: int64), (var_12: int64), (var_13: int64)): int64 =
    let (var_21: int64) = (!var_5)
    let (var_23: bool) = (var_21 >= 0L)
    let (var_24: int64) = (int64 var_4.Length)
    let (var_25: bool) = (var_21 < var_24)
    let (if_var_45: int64) =
        if (var_23 && var_25) then
            let (var_27: char) = var_4.[int32 (var_21)]
            let (var_30: int64) = (!var_5)
            var_5 := (var_30 + 1L)
            let (var_33: bool) = method_14((var_27: char))
            let (if_var_46: int64) =
                if var_33 then
                    let (var_35: int64) = System.Convert.ToInt64(var_27)
                    let (var_36: int64) = System.Convert.ToInt64('0')
                    let (var_37: int64) = (var_35 - var_36)
                    let (var_38: int64) = (var_13 * 10L)
                    let (var_39: int64) = (var_38 + var_37)
                    let (var_41: int64) = method_38((var_4: string), (var_5: int64 ref), (var_6: int64), (var_7: int64), (var_8: int64), (var_9: int64), (var_10: int64), (var_11: int64), (var_12: int64), (var_39: int64))
                    var_41
                else
                    let (var_48: int64) = method_39((var_4: string), (var_5: int64 ref), (var_6: int64), (var_7: int64), (var_8: int64), (var_9: int64), (var_10: int64), (var_11: int64), (var_12: int64), (var_13: int64))
                    var_48
            if_var_46
        else
            let (var_54: int64) = method_39((var_4: string), (var_5: int64 ref), (var_6: int64), (var_7: int64), (var_8: int64), (var_9: int64), (var_10: int64), (var_11: int64), (var_12: int64), (var_13: int64))
            var_54
    if_var_45
and method_39((var_4: string), (var_5: int64 ref), (var_6: int64), (var_7: int64), (var_8: int64), (var_9: int64), (var_10: int64), (var_11: int64), (var_12: int64), (var_13: int64)): int64 =
    let (var_19: int64) = (!var_5)
    let (var_21: bool) = (var_19 >= 0L)
    let (var_22: int64) = (int64 var_4.Length)
    let (var_23: bool) = (var_19 < var_22)
    let (if_var_47: int64) =
        if (var_21 && var_23) then
            let (var_25: char) = var_4.[int32 (var_19)]
            let (var_28: int64) = (!var_5)
            var_5 := (var_28 + 1L)
            let (var_31: bool) = method_17((var_25: char))
            let (var_32: bool) = method_18((var_25: char))
            let (if_var_48: int64) =
                if (var_31 || var_32) then
                    let (var_33: int64) = method_39((var_4: string), (var_5: int64 ref), (var_6: int64), (var_7: int64), (var_8: int64), (var_9: int64), (var_10: int64), (var_11: int64), (var_12: int64), (var_13: int64))
                    var_33
                else
                    let (var_36: int64) = (!var_5)
                    var_5 := (var_36 + -1L)
                    let (var_46: int64) = 0L
                    let (var_47: int64) = method_40((var_4: string), (var_5: int64 ref), (var_6: int64), (var_7: int64), (var_8: int64), (var_9: int64), (var_10: int64), (var_11: int64), (var_12: int64), (var_13: int64), (var_46: int64))
                    var_47
            if_var_48
        else
            let (var_57: int64) = 0L
            let (var_58: int64) = method_40((var_4: string), (var_5: int64 ref), (var_6: int64), (var_7: int64), (var_8: int64), (var_9: int64), (var_10: int64), (var_11: int64), (var_12: int64), (var_13: int64), (var_57: int64))
            var_58
    if_var_47
and method_40((var_4: string), (var_5: int64 ref), (var_6: int64), (var_7: int64), (var_8: int64), (var_9: int64), (var_10: int64), (var_11: int64), (var_12: int64), (var_13: int64), (var_15: int64)): int64 =
    let (var_22: int64) = (!var_5)
    let (var_24: bool) = (var_22 >= 0L)
    let (var_25: int64) = (int64 var_4.Length)
    let (var_26: bool) = (var_22 < var_25)
    let (if_var_49: int64) =
        if (var_24 && var_26) then
            let (var_28: char) = var_4.[int32 (var_22)]
            let (var_31: int64) = (!var_5)
            var_5 := (var_31 + 1L)
            let (var_34: bool) = method_14((var_28: char))
            let (if_var_50: int64) =
                if var_34 then
                    let (var_36: int64) = System.Convert.ToInt64(var_28)
                    let (var_37: int64) = System.Convert.ToInt64('0')
                    let (var_38: int64) = (var_36 - var_37)
                    let (var_39: int64) = (var_15 * 10L)
                    let (var_40: int64) = (var_39 + var_38)
                    let (var_42: int64) = method_41((var_4: string), (var_5: int64 ref), (var_6: int64), (var_7: int64), (var_8: int64), (var_9: int64), (var_10: int64), (var_11: int64), (var_12: int64), (var_13: int64), (var_40: int64))
                    var_42
                else
                    0L
            if_var_50
        else
            0L
    if_var_49
and method_41((var_4: string), (var_5: int64 ref), (var_6: int64), (var_7: int64), (var_8: int64), (var_9: int64), (var_10: int64), (var_11: int64), (var_12: int64), (var_13: int64), (var_14: int64)): int64 =
    let (var_22: int64) = (!var_5)
    let (var_24: bool) = (var_22 >= 0L)
    let (var_25: int64) = (int64 var_4.Length)
    let (var_26: bool) = (var_22 < var_25)
    let (if_var_51: int64) =
        if (var_24 && var_26) then
            let (var_28: char) = var_4.[int32 (var_22)]
            let (var_31: int64) = (!var_5)
            var_5 := (var_31 + 1L)
            let (var_34: bool) = method_14((var_28: char))
            let (if_var_52: int64) =
                if var_34 then
                    let (var_36: int64) = System.Convert.ToInt64(var_28)
                    let (var_37: int64) = System.Convert.ToInt64('0')
                    let (var_38: int64) = (var_36 - var_37)
                    let (var_39: int64) = (var_14 * 10L)
                    let (var_40: int64) = (var_39 + var_38)
                    let (var_42: int64) = method_41((var_4: string), (var_5: int64 ref), (var_6: int64), (var_7: int64), (var_8: int64), (var_9: int64), (var_10: int64), (var_11: int64), (var_12: int64), (var_13: int64), (var_40: int64))
                    var_42
                else
                    let (var_49: int64) = method_42((var_4: string), (var_5: int64 ref), (var_6: int64), (var_7: int64), (var_8: int64), (var_9: int64), (var_10: int64), (var_11: int64), (var_12: int64), (var_13: int64), (var_14: int64))
                    var_49
            if_var_52
        else
            let (var_55: int64) = method_42((var_4: string), (var_5: int64 ref), (var_6: int64), (var_7: int64), (var_8: int64), (var_9: int64), (var_10: int64), (var_11: int64), (var_12: int64), (var_13: int64), (var_14: int64))
            var_55
    if_var_51
and method_42((var_4: string), (var_5: int64 ref), (var_6: int64), (var_7: int64), (var_8: int64), (var_9: int64), (var_10: int64), (var_11: int64), (var_12: int64), (var_13: int64), (var_14: int64)): int64 =
    let (var_20: int64) = (!var_5)
    let (var_22: bool) = (var_20 >= 0L)
    let (var_23: int64) = (int64 var_4.Length)
    let (var_24: bool) = (var_20 < var_23)
    let (if_var_53: int64) =
        if (var_22 && var_24) then
            let (var_26: char) = var_4.[int32 (var_20)]
            let (var_29: int64) = (!var_5)
            var_5 := (var_29 + 1L)
            let (var_32: bool) = method_17((var_26: char))
            let (var_33: bool) = method_18((var_26: char))
            let (if_var_54: int64) =
                if (var_32 || var_33) then
                    let (var_34: int64) = method_42((var_4: string), (var_5: int64 ref), (var_6: int64), (var_7: int64), (var_8: int64), (var_9: int64), (var_10: int64), (var_11: int64), (var_12: int64), (var_13: int64), (var_14: int64))
                    var_34
                else
                    let (var_37: int64) = (!var_5)
                    var_5 := (var_37 + -1L)
                    let (var_47: int64) = 0L
                    let (var_48: int64) = method_43((var_4: string), (var_5: int64 ref), (var_6: int64), (var_7: int64), (var_8: int64), (var_9: int64), (var_10: int64), (var_11: int64), (var_12: int64), (var_13: int64), (var_14: int64), (var_47: int64))
                    var_48
            if_var_54
        else
            let (var_58: int64) = 0L
            let (var_59: int64) = method_43((var_4: string), (var_5: int64 ref), (var_6: int64), (var_7: int64), (var_8: int64), (var_9: int64), (var_10: int64), (var_11: int64), (var_12: int64), (var_13: int64), (var_14: int64), (var_58: int64))
            var_59
    if_var_53
and method_43((var_4: string), (var_5: int64 ref), (var_6: int64), (var_7: int64), (var_8: int64), (var_9: int64), (var_10: int64), (var_11: int64), (var_12: int64), (var_13: int64), (var_14: int64), (var_16: int64)): int64 =
    let (var_23: int64) = (!var_5)
    let (var_25: bool) = (var_23 >= 0L)
    let (var_26: int64) = (int64 var_4.Length)
    let (var_27: bool) = (var_23 < var_26)
    let (if_var_55: int64) =
        if (var_25 && var_27) then
            let (var_29: char) = var_4.[int32 (var_23)]
            let (var_32: int64) = (!var_5)
            var_5 := (var_32 + 1L)
            let (var_35: bool) = method_14((var_29: char))
            let (if_var_56: int64) =
                if var_35 then
                    let (var_37: int64) = System.Convert.ToInt64(var_29)
                    let (var_38: int64) = System.Convert.ToInt64('0')
                    let (var_39: int64) = (var_37 - var_38)
                    let (var_40: int64) = (var_16 * 10L)
                    let (var_41: int64) = (var_40 + var_39)
                    let (var_43: int64) = method_44((var_4: string), (var_5: int64 ref), (var_6: int64), (var_7: int64), (var_8: int64), (var_9: int64), (var_10: int64), (var_11: int64), (var_12: int64), (var_13: int64), (var_14: int64), (var_41: int64))
                    var_43
                else
                    0L
            if_var_56
        else
            0L
    if_var_55
and method_44((var_4: string), (var_5: int64 ref), (var_6: int64), (var_7: int64), (var_8: int64), (var_9: int64), (var_10: int64), (var_11: int64), (var_12: int64), (var_13: int64), (var_14: int64), (var_15: int64)): int64 =
    let (var_23: int64) = (!var_5)
    let (var_25: bool) = (var_23 >= 0L)
    let (var_26: int64) = (int64 var_4.Length)
    let (var_27: bool) = (var_23 < var_26)
    let (if_var_57: int64) =
        if (var_25 && var_27) then
            let (var_29: char) = var_4.[int32 (var_23)]
            let (var_32: int64) = (!var_5)
            var_5 := (var_32 + 1L)
            let (var_35: bool) = method_14((var_29: char))
            let (if_var_58: int64) =
                if var_35 then
                    let (var_37: int64) = System.Convert.ToInt64(var_29)
                    let (var_38: int64) = System.Convert.ToInt64('0')
                    let (var_39: int64) = (var_37 - var_38)
                    let (var_40: int64) = (var_15 * 10L)
                    let (var_41: int64) = (var_40 + var_39)
                    let (var_43: int64) = method_44((var_4: string), (var_5: int64 ref), (var_6: int64), (var_7: int64), (var_8: int64), (var_9: int64), (var_10: int64), (var_11: int64), (var_12: int64), (var_13: int64), (var_14: int64), (var_41: int64))
                    var_43
                else
                    let (var_50: int64) = method_45((var_4: string), (var_5: int64 ref), (var_6: int64), (var_7: int64), (var_8: int64), (var_9: int64), (var_10: int64), (var_11: int64), (var_12: int64), (var_13: int64), (var_14: int64), (var_15: int64))
                    var_50
            if_var_58
        else
            let (var_56: int64) = method_45((var_4: string), (var_5: int64 ref), (var_6: int64), (var_7: int64), (var_8: int64), (var_9: int64), (var_10: int64), (var_11: int64), (var_12: int64), (var_13: int64), (var_14: int64), (var_15: int64))
            var_56
    if_var_57
and method_45((var_1: string), (var_2: int64 ref), (var_3: int64), (var_4: int64), (var_5: int64), (var_6: int64), (var_7: int64), (var_8: int64), (var_9: int64), (var_10: int64), (var_11: int64), (var_12: int64)): int64 =
    let (var_18: int64) = (!var_2)
    let (var_20: bool) = (var_18 >= 0L)
    let (var_21: int64) = (int64 var_1.Length)
    let (var_22: bool) = (var_18 < var_21)
    let (if_var_59: int64) =
        if (var_20 && var_22) then
            let (var_24: char) = var_1.[int32 (var_18)]
            let (var_27: int64) = (!var_2)
            var_2 := (var_27 + 1L)
            let (var_30: bool) = method_17((var_24: char))
            let (var_31: bool) = method_18((var_24: char))
            let (if_var_60: int64) =
                if (var_30 || var_31) then
                    let (var_32: int64) = method_45((var_1: string), (var_2: int64 ref), (var_3: int64), (var_4: int64), (var_5: int64), (var_6: int64), (var_7: int64), (var_8: int64), (var_9: int64), (var_10: int64), (var_11: int64), (var_12: int64))
                    var_32
                else
                    let (var_35: int64) = (!var_2)
                    var_2 := (var_35 + -1L)
                    let (var_44: int64) = (0L + var_3)
                    let (var_45: int64) = (var_44 + var_4)
                    let (var_46: int64) = (var_45 + var_5)
                    let (var_47: int64) = (var_46 + var_6)
                    let (var_48: int64) = (var_47 + var_7)
                    let (var_49: int64) = (var_48 + var_8)
                    let (var_50: int64) = (var_49 + var_9)
                    let (var_51: int64) = (var_50 + var_10)
                    let (var_52: int64) = (var_51 + var_11)
                    let (var_53: int64) = (var_52 + var_12)
                    var_53
            if_var_60
        else
            let (var_62: int64) = (0L + var_3)
            let (var_63: int64) = (var_62 + var_4)
            let (var_64: int64) = (var_63 + var_5)
            let (var_65: int64) = (var_64 + var_6)
            let (var_66: int64) = (var_65 + var_7)
            let (var_67: int64) = (var_66 + var_8)
            let (var_68: int64) = (var_67 + var_9)
            let (var_69: int64) = (var_68 + var_10)
            let (var_70: int64) = (var_69 + var_11)
            let (var_71: int64) = (var_70 + var_12)
            var_71
    if_var_59
let (var_22: string) = System.Console.ReadLine()
let (var_24: int64 ref) = (ref 0L)
let (var_29: int64) = 0L
let (var_30: int64) = method_13((var_22: string), (var_24: int64 ref), (var_29: int64))
var_30
