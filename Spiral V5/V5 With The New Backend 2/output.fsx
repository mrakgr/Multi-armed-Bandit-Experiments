let rec method_14((var_14: string), (var_15: int64), (var_16: int64)): unit =
    let (var_22: bool) = (var_15 >= 0L)
    let (var_23: int64) = (int64 var_14.Length)
    let (var_24: bool) = (var_15 < var_23)
    if (var_22 && var_24) then
        let (var_26: char) = var_14.[int32 (var_15)]
        let (var_28: int64) = (var_15 + 1L)
        let (var_29: bool) = (var_26 >= '0')
        let (var_30: bool) = (var_26 <= '9')
        if (var_29 && var_30) then
            let (var_32: int64) = System.Convert.ToInt64(var_26)
            let (var_33: int64) = System.Convert.ToInt64('0')
            let (var_34: int64) = (var_32 - var_33)
            let (var_35: int64) = (var_16 * 10L)
            let (var_36: int64) = (var_35 + var_34)
            method_15((var_14: string), (var_28: int64), (var_36: int64))
            ()
        else
            ()
        ()
    else
        ()
    ()
and method_15((var_14: string), (var_15: int64), (var_16: int64)): unit =
    let (var_22: bool) = (var_15 >= 0L)
    let (var_23: int64) = (int64 var_14.Length)
    let (var_24: bool) = (var_15 < var_23)
    if (var_22 && var_24) then
        let (var_26: char) = var_14.[int32 (var_15)]
        let (var_28: int64) = (var_15 + 1L)
        let (var_29: bool) = (var_26 >= '0')
        let (var_30: bool) = (var_26 <= '9')
        if (var_29 && var_30) then
            let (var_32: int64) = System.Convert.ToInt64(var_26)
            let (var_33: int64) = System.Convert.ToInt64('0')
            let (var_34: int64) = (var_32 - var_33)
            let (var_35: int64) = (var_16 * 10L)
            let (var_36: int64) = (var_35 + var_34)
            method_15((var_14: string), (var_28: int64), (var_36: int64))
            ()
        else
            method_16((var_14: string), (var_16: int64), (var_28: int64))
            ()
        ()
    else
        method_36((var_14: string), (var_15: int64), (var_16: int64))
        ()
    ()
and method_16((var_14: string), (var_15: int64), (var_16: int64)): unit =
    let (var_21: bool) = (var_16 >= 0L)
    let (var_22: int64) = (int64 var_14.Length)
    let (var_23: bool) = (var_16 < var_22)
    if (var_21 && var_23) then
        let (var_25: char) = var_14.[int32 (var_16)]
        let (var_27: int64) = (var_16 + 1L)
        let (var_28: bool) = (var_25 = ' ')
        let (var_29: bool) = (var_25 = '\n')
        let (var_30: bool) = (var_25 = '\r')
        let (var_31: bool) = (var_29 || var_30)
        if (var_28 || var_31) then
            method_16((var_14: string), (var_15: int64), (var_27: int64))
            ()
        else
            let (var_34: int64) = (var_27 - 1L)
            let (var_41: int64) = 0L
            method_17((var_14: string), (var_15: int64), (var_34: int64), (var_41: int64))
            ()
        ()
    else
        let (var_51: int64) = 0L
        method_17((var_14: string), (var_15: int64), (var_16: int64), (var_51: int64))
        ()
    ()
and method_17((var_14: string), (var_15: int64), (var_16: int64), (var_17: int64)): unit =
    let (var_23: bool) = (var_16 >= 0L)
    let (var_24: int64) = (int64 var_14.Length)
    let (var_25: bool) = (var_16 < var_24)
    if (var_23 && var_25) then
        let (var_27: char) = var_14.[int32 (var_16)]
        let (var_29: int64) = (var_16 + 1L)
        let (var_30: bool) = (var_27 >= '0')
        let (var_31: bool) = (var_27 <= '9')
        if (var_30 && var_31) then
            let (var_33: int64) = System.Convert.ToInt64(var_27)
            let (var_34: int64) = System.Convert.ToInt64('0')
            let (var_35: int64) = (var_33 - var_34)
            let (var_36: int64) = (var_17 * 10L)
            let (var_37: int64) = (var_36 + var_35)
            method_18((var_14: string), (var_15: int64), (var_29: int64), (var_37: int64))
            ()
        else
            ()
        ()
    else
        ()
    ()
and method_18((var_14: string), (var_15: int64), (var_16: int64), (var_17: int64)): unit =
    let (var_23: bool) = (var_16 >= 0L)
    let (var_24: int64) = (int64 var_14.Length)
    let (var_25: bool) = (var_16 < var_24)
    if (var_23 && var_25) then
        let (var_27: char) = var_14.[int32 (var_16)]
        let (var_29: int64) = (var_16 + 1L)
        let (var_30: bool) = (var_27 >= '0')
        let (var_31: bool) = (var_27 <= '9')
        if (var_30 && var_31) then
            let (var_33: int64) = System.Convert.ToInt64(var_27)
            let (var_34: int64) = System.Convert.ToInt64('0')
            let (var_35: int64) = (var_33 - var_34)
            let (var_36: int64) = (var_17 * 10L)
            let (var_37: int64) = (var_36 + var_35)
            method_18((var_14: string), (var_15: int64), (var_29: int64), (var_37: int64))
            ()
        else
            method_19((var_14: string), (var_15: int64), (var_17: int64), (var_29: int64))
            ()
        ()
    else
        method_34((var_14: string), (var_15: int64), (var_16: int64), (var_17: int64))
        ()
    ()
and method_19((var_14: string), (var_15: int64), (var_16: int64), (var_17: int64)): unit =
    let (var_22: bool) = (var_17 >= 0L)
    let (var_23: int64) = (int64 var_14.Length)
    let (var_24: bool) = (var_17 < var_23)
    if (var_22 && var_24) then
        let (var_26: char) = var_14.[int32 (var_17)]
        let (var_28: int64) = (var_17 + 1L)
        let (var_29: bool) = (var_26 = ' ')
        let (var_30: bool) = (var_26 = '\n')
        let (var_31: bool) = (var_26 = '\r')
        let (var_32: bool) = (var_30 || var_31)
        if (var_29 || var_32) then
            method_19((var_14: string), (var_15: int64), (var_16: int64), (var_28: int64))
            ()
        else
            let (var_35: int64) = (var_28 - 1L)
            let (var_42: int64) = 0L
            method_20((var_14: string), (var_15: int64), (var_16: int64), (var_35: int64), (var_42: int64))
            ()
        ()
    else
        let (var_52: int64) = 0L
        method_20((var_14: string), (var_15: int64), (var_16: int64), (var_17: int64), (var_52: int64))
        ()
    ()
and method_20((var_14: string), (var_15: int64), (var_16: int64), (var_17: int64), (var_18: int64)): unit =
    let (var_24: bool) = (var_17 >= 0L)
    let (var_25: int64) = (int64 var_14.Length)
    let (var_26: bool) = (var_17 < var_25)
    if (var_24 && var_26) then
        let (var_28: char) = var_14.[int32 (var_17)]
        let (var_30: int64) = (var_17 + 1L)
        let (var_31: bool) = (var_28 >= '0')
        let (var_32: bool) = (var_28 <= '9')
        if (var_31 && var_32) then
            let (var_34: int64) = System.Convert.ToInt64(var_28)
            let (var_35: int64) = System.Convert.ToInt64('0')
            let (var_36: int64) = (var_34 - var_35)
            let (var_37: int64) = (var_18 * 10L)
            let (var_38: int64) = (var_37 + var_36)
            method_21((var_14: string), (var_15: int64), (var_16: int64), (var_30: int64), (var_38: int64))
            ()
        else
            ()
        ()
    else
        ()
    ()
and method_21((var_14: string), (var_15: int64), (var_16: int64), (var_17: int64), (var_18: int64)): unit =
    let (var_24: bool) = (var_17 >= 0L)
    let (var_25: int64) = (int64 var_14.Length)
    let (var_26: bool) = (var_17 < var_25)
    if (var_24 && var_26) then
        let (var_28: char) = var_14.[int32 (var_17)]
        let (var_30: int64) = (var_17 + 1L)
        let (var_31: bool) = (var_28 >= '0')
        let (var_32: bool) = (var_28 <= '9')
        if (var_31 && var_32) then
            let (var_34: int64) = System.Convert.ToInt64(var_28)
            let (var_35: int64) = System.Convert.ToInt64('0')
            let (var_36: int64) = (var_34 - var_35)
            let (var_37: int64) = (var_18 * 10L)
            let (var_38: int64) = (var_37 + var_36)
            method_21((var_14: string), (var_15: int64), (var_16: int64), (var_30: int64), (var_38: int64))
            ()
        else
            method_22((var_14: string), (var_15: int64), (var_16: int64), (var_18: int64), (var_30: int64))
            ()
        ()
    else
        method_32((var_14: string), (var_15: int64), (var_16: int64), (var_17: int64), (var_18: int64))
        ()
    ()
and method_22((var_14: string), (var_15: int64), (var_16: int64), (var_17: int64), (var_18: int64)): unit =
    let (var_23: bool) = (var_18 >= 0L)
    let (var_24: int64) = (int64 var_14.Length)
    let (var_25: bool) = (var_18 < var_24)
    if (var_23 && var_25) then
        let (var_27: char) = var_14.[int32 (var_18)]
        let (var_29: int64) = (var_18 + 1L)
        let (var_30: bool) = (var_27 = ' ')
        let (var_31: bool) = (var_27 = '\n')
        let (var_32: bool) = (var_27 = '\r')
        let (var_33: bool) = (var_31 || var_32)
        if (var_30 || var_33) then
            method_22((var_14: string), (var_15: int64), (var_16: int64), (var_17: int64), (var_29: int64))
            ()
        else
            let (var_36: int64) = (var_29 - 1L)
            let (var_43: int64) = 0L
            method_23((var_14: string), (var_15: int64), (var_16: int64), (var_17: int64), (var_36: int64), (var_43: int64))
            ()
        ()
    else
        let (var_53: int64) = 0L
        method_23((var_14: string), (var_15: int64), (var_16: int64), (var_17: int64), (var_18: int64), (var_53: int64))
        ()
    ()
and method_23((var_14: string), (var_15: int64), (var_16: int64), (var_17: int64), (var_18: int64), (var_19: int64)): unit =
    let (var_25: bool) = (var_18 >= 0L)
    let (var_26: int64) = (int64 var_14.Length)
    let (var_27: bool) = (var_18 < var_26)
    if (var_25 && var_27) then
        let (var_29: char) = var_14.[int32 (var_18)]
        let (var_31: int64) = (var_18 + 1L)
        let (var_32: bool) = (var_29 >= '0')
        let (var_33: bool) = (var_29 <= '9')
        if (var_32 && var_33) then
            let (var_35: int64) = System.Convert.ToInt64(var_29)
            let (var_36: int64) = System.Convert.ToInt64('0')
            let (var_37: int64) = (var_35 - var_36)
            let (var_38: int64) = (var_19 * 10L)
            let (var_39: int64) = (var_38 + var_37)
            method_24((var_14: string), (var_15: int64), (var_16: int64), (var_17: int64), (var_31: int64), (var_39: int64))
            ()
        else
            ()
        ()
    else
        ()
    ()
and method_24((var_14: string), (var_15: int64), (var_16: int64), (var_17: int64), (var_18: int64), (var_19: int64)): unit =
    let (var_25: bool) = (var_18 >= 0L)
    let (var_26: int64) = (int64 var_14.Length)
    let (var_27: bool) = (var_18 < var_26)
    if (var_25 && var_27) then
        let (var_29: char) = var_14.[int32 (var_18)]
        let (var_31: int64) = (var_18 + 1L)
        let (var_32: bool) = (var_29 >= '0')
        let (var_33: bool) = (var_29 <= '9')
        if (var_32 && var_33) then
            let (var_35: int64) = System.Convert.ToInt64(var_29)
            let (var_36: int64) = System.Convert.ToInt64('0')
            let (var_37: int64) = (var_35 - var_36)
            let (var_38: int64) = (var_19 * 10L)
            let (var_39: int64) = (var_38 + var_37)
            method_24((var_14: string), (var_15: int64), (var_16: int64), (var_17: int64), (var_31: int64), (var_39: int64))
            ()
        else
            method_25((var_14: string), (var_15: int64), (var_16: int64), (var_17: int64), (var_19: int64), (var_31: int64))
            ()
        ()
    else
        method_30((var_14: string), (var_15: int64), (var_16: int64), (var_17: int64), (var_18: int64), (var_19: int64))
        ()
    ()
and method_25((var_14: string), (var_15: int64), (var_16: int64), (var_17: int64), (var_18: int64), (var_19: int64)): unit =
    let (var_24: bool) = (var_19 >= 0L)
    let (var_25: int64) = (int64 var_14.Length)
    let (var_26: bool) = (var_19 < var_25)
    if (var_24 && var_26) then
        let (var_28: char) = var_14.[int32 (var_19)]
        let (var_30: int64) = (var_19 + 1L)
        let (var_31: bool) = (var_28 = ' ')
        let (var_32: bool) = (var_28 = '\n')
        let (var_33: bool) = (var_28 = '\r')
        let (var_34: bool) = (var_32 || var_33)
        if (var_31 || var_34) then
            method_25((var_14: string), (var_15: int64), (var_16: int64), (var_17: int64), (var_18: int64), (var_30: int64))
            ()
        else
            let (var_37: int64) = (var_30 - 1L)
            let (var_44: int64) = 0L
            method_26((var_14: string), (var_15: int64), (var_16: int64), (var_17: int64), (var_18: int64), (var_37: int64), (var_44: int64))
            ()
        ()
    else
        let (var_54: int64) = 0L
        method_26((var_14: string), (var_15: int64), (var_16: int64), (var_17: int64), (var_18: int64), (var_19: int64), (var_54: int64))
        ()
    ()
and method_26((var_14: string), (var_15: int64), (var_16: int64), (var_17: int64), (var_18: int64), (var_19: int64), (var_20: int64)): unit =
    let (var_26: bool) = (var_19 >= 0L)
    let (var_27: int64) = (int64 var_14.Length)
    let (var_28: bool) = (var_19 < var_27)
    if (var_26 && var_28) then
        let (var_30: char) = var_14.[int32 (var_19)]
        let (var_32: int64) = (var_19 + 1L)
        let (var_33: bool) = (var_30 >= '0')
        let (var_34: bool) = (var_30 <= '9')
        if (var_33 && var_34) then
            let (var_36: int64) = System.Convert.ToInt64(var_30)
            let (var_37: int64) = System.Convert.ToInt64('0')
            let (var_38: int64) = (var_36 - var_37)
            let (var_39: int64) = (var_20 * 10L)
            let (var_40: int64) = (var_39 + var_38)
            method_27((var_14: string), (var_15: int64), (var_16: int64), (var_17: int64), (var_18: int64), (var_32: int64), (var_40: int64))
            ()
        else
            ()
        ()
    else
        ()
    ()
and method_27((var_14: string), (var_15: int64), (var_16: int64), (var_17: int64), (var_18: int64), (var_19: int64), (var_20: int64)): unit =
    let (var_26: bool) = (var_19 >= 0L)
    let (var_27: int64) = (int64 var_14.Length)
    let (var_28: bool) = (var_19 < var_27)
    if (var_26 && var_28) then
        let (var_30: char) = var_14.[int32 (var_19)]
        let (var_32: int64) = (var_19 + 1L)
        let (var_33: bool) = (var_30 >= '0')
        let (var_34: bool) = (var_30 <= '9')
        if (var_33 && var_34) then
            let (var_36: int64) = System.Convert.ToInt64(var_30)
            let (var_37: int64) = System.Convert.ToInt64('0')
            let (var_38: int64) = (var_36 - var_37)
            let (var_39: int64) = (var_20 * 10L)
            let (var_40: int64) = (var_39 + var_38)
            method_27((var_14: string), (var_15: int64), (var_16: int64), (var_17: int64), (var_18: int64), (var_32: int64), (var_40: int64))
            ()
        else
            method_28((var_14: string), (var_15: int64), (var_16: int64), (var_17: int64), (var_18: int64), (var_20: int64), (var_32: int64))
            ()
        ()
    else
        method_29((var_14: string), (var_15: int64), (var_16: int64), (var_17: int64), (var_18: int64), (var_19: int64), (var_20: int64))
        ()
    ()
and method_28((var_12: string), (var_13: int64), (var_14: int64), (var_15: int64), (var_16: int64), (var_17: int64), (var_18: int64)): unit =
    let (var_23: bool) = (var_18 >= 0L)
    let (var_24: int64) = (int64 var_12.Length)
    let (var_25: bool) = (var_18 < var_24)
    if (var_23 && var_25) then
        let (var_27: char) = var_12.[int32 (var_18)]
        let (var_29: int64) = (var_18 + 1L)
        let (var_30: bool) = (var_27 = ' ')
        let (var_31: bool) = (var_27 = '\n')
        let (var_32: bool) = (var_27 = '\r')
        let (var_33: bool) = (var_31 || var_32)
        if (var_30 || var_33) then
            method_28((var_12: string), (var_13: int64), (var_14: int64), (var_15: int64), (var_16: int64), (var_17: int64), (var_29: int64))
            ()
        else
            let (var_36: int64) = (var_29 - 1L)
            let (var_43: int64) = (0L + var_13)
            let (var_44: int64) = (var_43 + var_14)
            let (var_45: int64) = (var_44 + var_15)
            let (var_46: int64) = (var_45 + var_16)
            let (var_47: int64) = (var_46 + var_17)
            let (if_var_1: int64) =
                if (var_13 < var_13) then
                    var_13
                else
                    var_13
            let (var_49: int64) = if_var_1
            let (if_var_2: int64) =
                if (var_49 < var_14) then
                    var_49
                else
                    var_14
            let (var_50: int64) = if_var_2
            let (if_var_3: int64) =
                if (var_50 < var_15) then
                    var_50
                else
                    var_15
            let (var_51: int64) = if_var_3
            let (if_var_4: int64) =
                if (var_51 < var_16) then
                    var_51
                else
                    var_16
            let (var_52: int64) = if_var_4
            let (if_var_5: int64) =
                if (var_52 < var_17) then
                    var_52
                else
                    var_17
            let (var_53: int64) = if_var_5
            let (if_var_6: int64) =
                if (var_13 > var_13) then
                    var_13
                else
                    var_13
            let (var_55: int64) = if_var_6
            let (if_var_7: int64) =
                if (var_55 > var_14) then
                    var_55
                else
                    var_14
            let (var_56: int64) = if_var_7
            let (if_var_8: int64) =
                if (var_56 > var_15) then
                    var_56
                else
                    var_15
            let (var_57: int64) = if_var_8
            let (if_var_9: int64) =
                if (var_57 > var_16) then
                    var_57
                else
                    var_16
            let (var_58: int64) = if_var_9
            let (if_var_10: int64) =
                if (var_58 > var_17) then
                    var_58
                else
                    var_17
            let (var_59: int64) = if_var_10
            let (var_77: int64) = (var_47 - var_59)
            System.Console.Write(var_77)
            System.Console.Write(" ")
            let (var_106: int64) = (var_47 - var_53)
            System.Console.Write(var_106)
            ()
        ()
    else
        let (var_127: int64) = (0L + var_13)
        let (var_128: int64) = (var_127 + var_14)
        let (var_129: int64) = (var_128 + var_15)
        let (var_130: int64) = (var_129 + var_16)
        let (var_131: int64) = (var_130 + var_17)
        let (if_var_11: int64) =
            if (var_13 < var_13) then
                var_13
            else
                var_13
        let (var_133: int64) = if_var_11
        let (if_var_12: int64) =
            if (var_133 < var_14) then
                var_133
            else
                var_14
        let (var_134: int64) = if_var_12
        let (if_var_13: int64) =
            if (var_134 < var_15) then
                var_134
            else
                var_15
        let (var_135: int64) = if_var_13
        let (if_var_14: int64) =
            if (var_135 < var_16) then
                var_135
            else
                var_16
        let (var_136: int64) = if_var_14
        let (if_var_15: int64) =
            if (var_136 < var_17) then
                var_136
            else
                var_17
        let (var_137: int64) = if_var_15
        let (if_var_16: int64) =
            if (var_13 > var_13) then
                var_13
            else
                var_13
        let (var_139: int64) = if_var_16
        let (if_var_17: int64) =
            if (var_139 > var_14) then
                var_139
            else
                var_14
        let (var_140: int64) = if_var_17
        let (if_var_18: int64) =
            if (var_140 > var_15) then
                var_140
            else
                var_15
        let (var_141: int64) = if_var_18
        let (if_var_19: int64) =
            if (var_141 > var_16) then
                var_141
            else
                var_16
        let (var_142: int64) = if_var_19
        let (if_var_20: int64) =
            if (var_142 > var_17) then
                var_142
            else
                var_17
        let (var_143: int64) = if_var_20
        let (var_161: int64) = (var_131 - var_143)
        System.Console.Write(var_161)
        System.Console.Write(" ")
        let (var_190: int64) = (var_131 - var_137)
        System.Console.Write(var_190)
        ()
    ()
and method_29((var_12: string), (var_13: int64), (var_14: int64), (var_15: int64), (var_16: int64), (var_17: int64), (var_18: int64)): unit =
    let (var_23: bool) = (var_17 >= 0L)
    let (var_24: int64) = (int64 var_12.Length)
    let (var_25: bool) = (var_17 < var_24)
    if (var_23 && var_25) then
        let (var_27: char) = var_12.[int32 (var_17)]
        let (var_29: int64) = (var_17 + 1L)
        let (var_30: bool) = (var_27 = ' ')
        let (var_31: bool) = (var_27 = '\n')
        let (var_32: bool) = (var_27 = '\r')
        let (var_33: bool) = (var_31 || var_32)
        if (var_30 || var_33) then
            method_28((var_12: string), (var_13: int64), (var_14: int64), (var_15: int64), (var_16: int64), (var_18: int64), (var_29: int64))
            ()
        else
            let (var_36: int64) = (var_29 - 1L)
            let (var_43: int64) = (0L + var_13)
            let (var_44: int64) = (var_43 + var_14)
            let (var_45: int64) = (var_44 + var_15)
            let (var_46: int64) = (var_45 + var_16)
            let (var_47: int64) = (var_46 + var_18)
            let (if_var_21: int64) =
                if (var_13 < var_13) then
                    var_13
                else
                    var_13
            let (var_49: int64) = if_var_21
            let (if_var_22: int64) =
                if (var_49 < var_14) then
                    var_49
                else
                    var_14
            let (var_50: int64) = if_var_22
            let (if_var_23: int64) =
                if (var_50 < var_15) then
                    var_50
                else
                    var_15
            let (var_51: int64) = if_var_23
            let (if_var_24: int64) =
                if (var_51 < var_16) then
                    var_51
                else
                    var_16
            let (var_52: int64) = if_var_24
            let (if_var_25: int64) =
                if (var_52 < var_18) then
                    var_52
                else
                    var_18
            let (var_53: int64) = if_var_25
            let (if_var_26: int64) =
                if (var_13 > var_13) then
                    var_13
                else
                    var_13
            let (var_55: int64) = if_var_26
            let (if_var_27: int64) =
                if (var_55 > var_14) then
                    var_55
                else
                    var_14
            let (var_56: int64) = if_var_27
            let (if_var_28: int64) =
                if (var_56 > var_15) then
                    var_56
                else
                    var_15
            let (var_57: int64) = if_var_28
            let (if_var_29: int64) =
                if (var_57 > var_16) then
                    var_57
                else
                    var_16
            let (var_58: int64) = if_var_29
            let (if_var_30: int64) =
                if (var_58 > var_18) then
                    var_58
                else
                    var_18
            let (var_59: int64) = if_var_30
            let (var_77: int64) = (var_47 - var_59)
            System.Console.Write(var_77)
            System.Console.Write(" ")
            let (var_106: int64) = (var_47 - var_53)
            System.Console.Write(var_106)
            ()
        ()
    else
        let (var_127: int64) = (0L + var_13)
        let (var_128: int64) = (var_127 + var_14)
        let (var_129: int64) = (var_128 + var_15)
        let (var_130: int64) = (var_129 + var_16)
        let (var_131: int64) = (var_130 + var_18)
        let (if_var_31: int64) =
            if (var_13 < var_13) then
                var_13
            else
                var_13
        let (var_133: int64) = if_var_31
        let (if_var_32: int64) =
            if (var_133 < var_14) then
                var_133
            else
                var_14
        let (var_134: int64) = if_var_32
        let (if_var_33: int64) =
            if (var_134 < var_15) then
                var_134
            else
                var_15
        let (var_135: int64) = if_var_33
        let (if_var_34: int64) =
            if (var_135 < var_16) then
                var_135
            else
                var_16
        let (var_136: int64) = if_var_34
        let (if_var_35: int64) =
            if (var_136 < var_18) then
                var_136
            else
                var_18
        let (var_137: int64) = if_var_35
        let (if_var_36: int64) =
            if (var_13 > var_13) then
                var_13
            else
                var_13
        let (var_139: int64) = if_var_36
        let (if_var_37: int64) =
            if (var_139 > var_14) then
                var_139
            else
                var_14
        let (var_140: int64) = if_var_37
        let (if_var_38: int64) =
            if (var_140 > var_15) then
                var_140
            else
                var_15
        let (var_141: int64) = if_var_38
        let (if_var_39: int64) =
            if (var_141 > var_16) then
                var_141
            else
                var_16
        let (var_142: int64) = if_var_39
        let (if_var_40: int64) =
            if (var_142 > var_18) then
                var_142
            else
                var_18
        let (var_143: int64) = if_var_40
        let (var_161: int64) = (var_131 - var_143)
        System.Console.Write(var_161)
        System.Console.Write(" ")
        let (var_190: int64) = (var_131 - var_137)
        System.Console.Write(var_190)
        ()
    ()
and method_30((var_14: string), (var_15: int64), (var_16: int64), (var_17: int64), (var_18: int64), (var_19: int64)): unit =
    let (var_24: bool) = (var_18 >= 0L)
    let (var_25: int64) = (int64 var_14.Length)
    let (var_26: bool) = (var_18 < var_25)
    if (var_24 && var_26) then
        let (var_28: char) = var_14.[int32 (var_18)]
        let (var_30: int64) = (var_18 + 1L)
        let (var_31: bool) = (var_28 = ' ')
        let (var_32: bool) = (var_28 = '\n')
        let (var_33: bool) = (var_28 = '\r')
        let (var_34: bool) = (var_32 || var_33)
        if (var_31 || var_34) then
            method_25((var_14: string), (var_15: int64), (var_16: int64), (var_17: int64), (var_19: int64), (var_30: int64))
            ()
        else
            let (var_37: int64) = (var_30 - 1L)
            let (var_44: int64) = 0L
            method_26((var_14: string), (var_15: int64), (var_16: int64), (var_17: int64), (var_19: int64), (var_37: int64), (var_44: int64))
            ()
        ()
    else
        let (var_54: int64) = 0L
        method_31((var_14: string), (var_15: int64), (var_16: int64), (var_17: int64), (var_18: int64), (var_19: int64), (var_54: int64))
        ()
    ()
and method_31((var_14: string), (var_15: int64), (var_16: int64), (var_17: int64), (var_18: int64), (var_19: int64), (var_20: int64)): unit =
    let (var_26: bool) = (var_18 >= 0L)
    let (var_27: int64) = (int64 var_14.Length)
    let (var_28: bool) = (var_18 < var_27)
    if (var_26 && var_28) then
        let (var_30: char) = var_14.[int32 (var_18)]
        let (var_32: int64) = (var_18 + 1L)
        let (var_33: bool) = (var_30 >= '0')
        let (var_34: bool) = (var_30 <= '9')
        if (var_33 && var_34) then
            let (var_36: int64) = System.Convert.ToInt64(var_30)
            let (var_37: int64) = System.Convert.ToInt64('0')
            let (var_38: int64) = (var_36 - var_37)
            let (var_39: int64) = (var_20 * 10L)
            let (var_40: int64) = (var_39 + var_38)
            method_27((var_14: string), (var_15: int64), (var_16: int64), (var_17: int64), (var_19: int64), (var_32: int64), (var_40: int64))
            ()
        else
            ()
        ()
    else
        ()
    ()
and method_32((var_14: string), (var_15: int64), (var_16: int64), (var_17: int64), (var_18: int64)): unit =
    let (var_23: bool) = (var_17 >= 0L)
    let (var_24: int64) = (int64 var_14.Length)
    let (var_25: bool) = (var_17 < var_24)
    if (var_23 && var_25) then
        let (var_27: char) = var_14.[int32 (var_17)]
        let (var_29: int64) = (var_17 + 1L)
        let (var_30: bool) = (var_27 = ' ')
        let (var_31: bool) = (var_27 = '\n')
        let (var_32: bool) = (var_27 = '\r')
        let (var_33: bool) = (var_31 || var_32)
        if (var_30 || var_33) then
            method_22((var_14: string), (var_15: int64), (var_16: int64), (var_18: int64), (var_29: int64))
            ()
        else
            let (var_36: int64) = (var_29 - 1L)
            let (var_43: int64) = 0L
            method_23((var_14: string), (var_15: int64), (var_16: int64), (var_18: int64), (var_36: int64), (var_43: int64))
            ()
        ()
    else
        let (var_53: int64) = 0L
        method_33((var_14: string), (var_15: int64), (var_16: int64), (var_17: int64), (var_18: int64), (var_53: int64))
        ()
    ()
and method_33((var_14: string), (var_15: int64), (var_16: int64), (var_17: int64), (var_18: int64), (var_19: int64)): unit =
    let (var_25: bool) = (var_17 >= 0L)
    let (var_26: int64) = (int64 var_14.Length)
    let (var_27: bool) = (var_17 < var_26)
    if (var_25 && var_27) then
        let (var_29: char) = var_14.[int32 (var_17)]
        let (var_31: int64) = (var_17 + 1L)
        let (var_32: bool) = (var_29 >= '0')
        let (var_33: bool) = (var_29 <= '9')
        if (var_32 && var_33) then
            let (var_35: int64) = System.Convert.ToInt64(var_29)
            let (var_36: int64) = System.Convert.ToInt64('0')
            let (var_37: int64) = (var_35 - var_36)
            let (var_38: int64) = (var_19 * 10L)
            let (var_39: int64) = (var_38 + var_37)
            method_24((var_14: string), (var_15: int64), (var_16: int64), (var_18: int64), (var_31: int64), (var_39: int64))
            ()
        else
            ()
        ()
    else
        ()
    ()
and method_34((var_14: string), (var_15: int64), (var_16: int64), (var_17: int64)): unit =
    let (var_22: bool) = (var_16 >= 0L)
    let (var_23: int64) = (int64 var_14.Length)
    let (var_24: bool) = (var_16 < var_23)
    if (var_22 && var_24) then
        let (var_26: char) = var_14.[int32 (var_16)]
        let (var_28: int64) = (var_16 + 1L)
        let (var_29: bool) = (var_26 = ' ')
        let (var_30: bool) = (var_26 = '\n')
        let (var_31: bool) = (var_26 = '\r')
        let (var_32: bool) = (var_30 || var_31)
        if (var_29 || var_32) then
            method_19((var_14: string), (var_15: int64), (var_17: int64), (var_28: int64))
            ()
        else
            let (var_35: int64) = (var_28 - 1L)
            let (var_42: int64) = 0L
            method_20((var_14: string), (var_15: int64), (var_17: int64), (var_35: int64), (var_42: int64))
            ()
        ()
    else
        let (var_52: int64) = 0L
        method_35((var_14: string), (var_15: int64), (var_16: int64), (var_17: int64), (var_52: int64))
        ()
    ()
and method_35((var_14: string), (var_15: int64), (var_16: int64), (var_17: int64), (var_18: int64)): unit =
    let (var_24: bool) = (var_16 >= 0L)
    let (var_25: int64) = (int64 var_14.Length)
    let (var_26: bool) = (var_16 < var_25)
    if (var_24 && var_26) then
        let (var_28: char) = var_14.[int32 (var_16)]
        let (var_30: int64) = (var_16 + 1L)
        let (var_31: bool) = (var_28 >= '0')
        let (var_32: bool) = (var_28 <= '9')
        if (var_31 && var_32) then
            let (var_34: int64) = System.Convert.ToInt64(var_28)
            let (var_35: int64) = System.Convert.ToInt64('0')
            let (var_36: int64) = (var_34 - var_35)
            let (var_37: int64) = (var_18 * 10L)
            let (var_38: int64) = (var_37 + var_36)
            method_21((var_14: string), (var_15: int64), (var_17: int64), (var_30: int64), (var_38: int64))
            ()
        else
            ()
        ()
    else
        ()
    ()
and method_36((var_14: string), (var_15: int64), (var_16: int64)): unit =
    let (var_21: bool) = (var_15 >= 0L)
    let (var_22: int64) = (int64 var_14.Length)
    let (var_23: bool) = (var_15 < var_22)
    if (var_21 && var_23) then
        let (var_25: char) = var_14.[int32 (var_15)]
        let (var_27: int64) = (var_15 + 1L)
        let (var_28: bool) = (var_25 = ' ')
        let (var_29: bool) = (var_25 = '\n')
        let (var_30: bool) = (var_25 = '\r')
        let (var_31: bool) = (var_29 || var_30)
        if (var_28 || var_31) then
            method_16((var_14: string), (var_16: int64), (var_27: int64))
            ()
        else
            let (var_34: int64) = (var_27 - 1L)
            let (var_41: int64) = 0L
            method_17((var_14: string), (var_16: int64), (var_34: int64), (var_41: int64))
            ()
        ()
    else
        let (var_51: int64) = 0L
        method_37((var_14: string), (var_15: int64), (var_16: int64), (var_51: int64))
        ()
    ()
and method_37((var_14: string), (var_15: int64), (var_16: int64), (var_17: int64)): unit =
    let (var_23: bool) = (var_15 >= 0L)
    let (var_24: int64) = (int64 var_14.Length)
    let (var_25: bool) = (var_15 < var_24)
    if (var_23 && var_25) then
        let (var_27: char) = var_14.[int32 (var_15)]
        let (var_29: int64) = (var_15 + 1L)
        let (var_30: bool) = (var_27 >= '0')
        let (var_31: bool) = (var_27 <= '9')
        if (var_30 && var_31) then
            let (var_33: int64) = System.Convert.ToInt64(var_27)
            let (var_34: int64) = System.Convert.ToInt64('0')
            let (var_35: int64) = (var_33 - var_34)
            let (var_36: int64) = (var_17 * 10L)
            let (var_37: int64) = (var_36 + var_35)
            method_18((var_14: string), (var_16: int64), (var_29: int64), (var_37: int64))
            ()
        else
            ()
        ()
    else
        ()
    ()
let (var_27: string) = System.Console.ReadLine()
let (var_29: int64) = 0L
let (var_30: int64) = var_29
let (var_34: int64) = 0L
method_14((var_27: string), (var_30: int64), (var_34: int64))

