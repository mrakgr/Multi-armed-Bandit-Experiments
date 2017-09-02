type Tuple4 =
    struct
    val mem_0: int64
    val mem_1: int64
    new(arg_mem_0, arg_mem_1) = {mem_0 = arg_mem_0; mem_1 = arg_mem_1}
    end
let rec method_14((var_5: string), (var_6: int64), (var_7: int64)): unit =
    let (var_30: bool) = (var_7 >= 0L)
    let (var_31: int64) = (int64 var_5.Length)
    let (var_32: bool) = (var_7 < var_31)
    if (var_30 && var_32) then
        let (var_33: char) = var_5.[int32 var_7]
        let (var_34: int64) = (var_7 + 1L)
        let (var_35: bool) = (var_33 >= '0')
        let (var_36: bool) = (var_33 <= '9')
        if (var_35 && var_36) then
            let (var_37: int64) = System.Convert.ToInt64(var_33)
            let (var_38: int64) = System.Convert.ToInt64('0')
            let (var_39: int64) = (var_37 - var_38)
            let (var_40: int64) = (0L + var_39)
            method_15((var_5: string), (var_6: int64), (var_40: int64), (var_34: int64))
        else
            method_30((var_5: string), (var_6: int64))
    else
        method_30((var_5: string), (var_6: int64))
and method_15((var_5: string), (var_6: int64), (var_7: int64), (var_8: int64)): unit =
    let (var_31: bool) = (var_8 >= 0L)
    let (var_32: int64) = (int64 var_5.Length)
    let (var_33: bool) = (var_8 < var_32)
    if (var_31 && var_33) then
        let (var_34: char) = var_5.[int32 var_8]
        let (var_35: int64) = (var_8 + 1L)
        let (var_36: bool) = (var_34 >= '0')
        let (var_37: bool) = (var_34 <= '9')
        if (var_36 && var_37) then
            let (var_38: int64) = System.Convert.ToInt64(var_34)
            let (var_39: int64) = System.Convert.ToInt64('0')
            let (var_40: int64) = (var_38 - var_39)
            let (var_41: int64) = (var_7 * 10L)
            let (var_42: int64) = (var_41 + var_40)
            method_15((var_5: string), (var_6: int64), (var_42: int64), (var_35: int64))
        else
            let (var_52: int64) = (-var_7)
            method_16((var_5: string), (var_52: int64), (var_35: int64))
    else
        let (var_70: int64) = (-var_7)
        method_16((var_5: string), (var_70: int64), (var_8: int64))
and method_16((var_5: string), (var_6: int64), (var_7: int64)): unit =
    let (var_24: bool) = (var_7 >= 0L)
    let (var_25: int64) = (int64 var_5.Length)
    let (var_26: bool) = (var_7 < var_25)
    if (var_24 && var_26) then
        let (var_27: char) = var_5.[int32 var_7]
        let (var_28: int64) = (var_7 + 1L)
        let (var_29: bool) = (var_27 = ' ')
        let (var_30: bool) = (var_27 = '\n')
        let (var_31: bool) = (var_27 = '\r')
        let (var_32: bool) = (var_30 || var_31)
        if (var_29 || var_32) then
            method_16((var_5: string), (var_6: int64), (var_28: int64))
        else
            if (var_6 > 0L) then
                if (var_24 && var_26) then
                    if ('-' = var_27) then
                        method_17((var_6: int64), (var_5: string), (var_7: int64), (var_28: int64))
                    else
                        method_28((var_6: int64), (var_5: string), (var_7: int64))
                else
                    method_28((var_6: int64), (var_5: string), (var_7: int64))
            else
                (failwith "n in parse array must be > 0.")
    else
        if (var_6 > 0L) then
            if (var_24 && var_26) then
                let (var_151: char) = var_5.[int32 var_7]
                let (var_152: int64) = (var_7 + 1L)
                if ('-' = var_151) then
                    method_17((var_6: int64), (var_5: string), (var_7: int64), (var_152: int64))
                else
                    method_28((var_6: int64), (var_5: string), (var_7: int64))
            else
                method_28((var_6: int64), (var_5: string), (var_7: int64))
        else
            (failwith "n in parse array must be > 0.")
and method_17((var_2: int64), (var_6: string), (var_7: int64), (var_8: int64)): unit =
    let (var_31: bool) = (var_8 >= 0L)
    let (var_32: int64) = (int64 var_6.Length)
    let (var_33: bool) = (var_8 < var_32)
    if (var_31 && var_33) then
        let (var_34: char) = var_6.[int32 var_8]
        let (var_35: int64) = (var_8 + 1L)
        let (var_36: bool) = (var_34 >= '0')
        let (var_37: bool) = (var_34 <= '9')
        if (var_36 && var_37) then
            let (var_38: int64) = System.Convert.ToInt64(var_34)
            let (var_39: int64) = System.Convert.ToInt64('0')
            let (var_40: int64) = (var_38 - var_39)
            let (var_41: int64) = (0L + var_40)
            method_18((var_2: int64), (var_6: string), (var_7: int64), (var_41: int64), (var_35: int64))
        else
            method_28((var_2: int64), (var_6: string), (var_7: int64))
    else
        method_28((var_2: int64), (var_6: string), (var_7: int64))
and method_18((var_2: int64), (var_6: string), (var_7: int64), (var_8: int64), (var_9: int64)): unit =
    let (var_32: bool) = (var_9 >= 0L)
    let (var_33: int64) = (int64 var_6.Length)
    let (var_34: bool) = (var_9 < var_33)
    if (var_32 && var_34) then
        let (var_35: char) = var_6.[int32 var_9]
        let (var_36: int64) = (var_9 + 1L)
        let (var_37: bool) = (var_35 >= '0')
        let (var_38: bool) = (var_35 <= '9')
        if (var_37 && var_38) then
            let (var_39: int64) = System.Convert.ToInt64(var_35)
            let (var_40: int64) = System.Convert.ToInt64('0')
            let (var_41: int64) = (var_39 - var_40)
            let (var_42: int64) = (var_8 * 10L)
            let (var_43: int64) = (var_42 + var_41)
            method_18((var_2: int64), (var_6: string), (var_7: int64), (var_43: int64), (var_36: int64))
        else
            let (var_53: int64) = (-var_8)
            method_19((var_2: int64), (var_6: string), (var_53: int64), (var_36: int64))
    else
        let (var_71: int64) = (-var_8)
        method_19((var_2: int64), (var_6: string), (var_71: int64), (var_9: int64))
and method_19((var_0: int64), (var_6: string), (var_7: int64), (var_8: int64)): unit =
    let (var_25: bool) = (var_8 >= 0L)
    let (var_26: int64) = (int64 var_6.Length)
    let (var_27: bool) = (var_8 < var_26)
    if (var_25 && var_27) then
        let (var_28: char) = var_6.[int32 var_8]
        let (var_29: int64) = (var_8 + 1L)
        let (var_30: bool) = (var_28 = ' ')
        let (var_31: bool) = (var_28 = '\n')
        let (var_32: bool) = (var_28 = '\r')
        let (var_33: bool) = (var_31 || var_32)
        if (var_30 || var_33) then
            method_19((var_0: int64), (var_6: string), (var_7: int64), (var_29: int64))
        else
            let (var_47: (int64 [])) = Array.zeroCreate<int64> (System.Convert.ToInt32(var_0))
            var_47.[int32 0L] <- var_7
            let (var_49: int64) = 1L
            method_20((var_47: (int64 [])), (var_6: string), (var_49: int64), (var_0: int64), (var_8: int64))
    else
        let (var_61: (int64 [])) = Array.zeroCreate<int64> (System.Convert.ToInt32(var_0))
        var_61.[int32 0L] <- var_7
        let (var_63: int64) = 1L
        method_20((var_61: (int64 [])), (var_6: string), (var_63: int64), (var_0: int64), (var_8: int64))
and method_20((var_0: (int64 [])), (var_4: string), (var_5: int64), (var_6: int64), (var_9: int64)): unit =
    if (var_5 < var_6) then
        let (var_40: bool) = (var_9 >= 0L)
        let (var_41: int64) = (int64 var_4.Length)
        let (var_42: bool) = (var_9 < var_41)
        if (var_40 && var_42) then
            let (var_43: char) = var_4.[int32 var_9]
            let (var_44: int64) = (var_9 + 1L)
            if ('-' = var_43) then
                method_21((var_0: (int64 [])), (var_5: int64), (var_6: int64), (var_4: string), (var_9: int64), (var_44: int64))
            else
                method_24((var_0: (int64 [])), (var_5: int64), (var_6: int64), (var_4: string), (var_9: int64))
        else
            method_24((var_0: (int64 [])), (var_5: int64), (var_6: int64), (var_4: string), (var_9: int64))
    else
        let (var_86: int64) = System.Int64.MinValue
        let (var_87: int64) = 0L
        let (var_88: Tuple4) = method_26((var_0: (int64 [])), (var_87: int64), (var_86: int64))
        let (var_89: int64) = var_88.mem_0
        let (var_90: int64) = var_88.mem_1
        System.Console.WriteLine(var_89)
and method_21((var_2: (int64 [])), (var_3: int64), (var_4: int64), (var_8: string), (var_9: int64), (var_10: int64)): unit =
    let (var_33: bool) = (var_10 >= 0L)
    let (var_34: int64) = (int64 var_8.Length)
    let (var_35: bool) = (var_10 < var_34)
    if (var_33 && var_35) then
        let (var_36: char) = var_8.[int32 var_10]
        let (var_37: int64) = (var_10 + 1L)
        let (var_38: bool) = (var_36 >= '0')
        let (var_39: bool) = (var_36 <= '9')
        if (var_38 && var_39) then
            let (var_40: int64) = System.Convert.ToInt64(var_36)
            let (var_41: int64) = System.Convert.ToInt64('0')
            let (var_42: int64) = (var_40 - var_41)
            let (var_43: int64) = (0L + var_42)
            method_22((var_2: (int64 [])), (var_3: int64), (var_4: int64), (var_8: string), (var_9: int64), (var_43: int64), (var_37: int64))
        else
            method_24((var_2: (int64 [])), (var_3: int64), (var_4: int64), (var_8: string), (var_9: int64))
    else
        method_24((var_2: (int64 [])), (var_3: int64), (var_4: int64), (var_8: string), (var_9: int64))
and method_22((var_2: (int64 [])), (var_3: int64), (var_4: int64), (var_8: string), (var_9: int64), (var_10: int64), (var_11: int64)): unit =
    let (var_34: bool) = (var_11 >= 0L)
    let (var_35: int64) = (int64 var_8.Length)
    let (var_36: bool) = (var_11 < var_35)
    if (var_34 && var_36) then
        let (var_37: char) = var_8.[int32 var_11]
        let (var_38: int64) = (var_11 + 1L)
        let (var_39: bool) = (var_37 >= '0')
        let (var_40: bool) = (var_37 <= '9')
        if (var_39 && var_40) then
            let (var_41: int64) = System.Convert.ToInt64(var_37)
            let (var_42: int64) = System.Convert.ToInt64('0')
            let (var_43: int64) = (var_41 - var_42)
            let (var_44: int64) = (var_10 * 10L)
            let (var_45: int64) = (var_44 + var_43)
            method_22((var_2: (int64 [])), (var_3: int64), (var_4: int64), (var_8: string), (var_9: int64), (var_45: int64), (var_38: int64))
        else
            let (var_55: int64) = (-var_10)
            method_23((var_2: (int64 [])), (var_3: int64), (var_4: int64), (var_8: string), (var_55: int64), (var_38: int64))
    else
        let (var_73: int64) = (-var_10)
        method_23((var_2: (int64 [])), (var_3: int64), (var_4: int64), (var_8: string), (var_73: int64), (var_11: int64))
and method_23((var_0: (int64 [])), (var_1: int64), (var_2: int64), (var_8: string), (var_9: int64), (var_10: int64)): unit =
    let (var_27: bool) = (var_10 >= 0L)
    let (var_28: int64) = (int64 var_8.Length)
    let (var_29: bool) = (var_10 < var_28)
    if (var_27 && var_29) then
        let (var_30: char) = var_8.[int32 var_10]
        let (var_31: int64) = (var_10 + 1L)
        let (var_32: bool) = (var_30 = ' ')
        let (var_33: bool) = (var_30 = '\n')
        let (var_34: bool) = (var_30 = '\r')
        let (var_35: bool) = (var_33 || var_34)
        if (var_32 || var_35) then
            method_23((var_0: (int64 [])), (var_1: int64), (var_2: int64), (var_8: string), (var_9: int64), (var_31: int64))
        else
            var_0.[int32 var_1] <- var_9
            let (var_50: int64) = (var_1 + 1L)
            method_20((var_0: (int64 [])), (var_8: string), (var_50: int64), (var_2: int64), (var_10: int64))
    else
        var_0.[int32 var_1] <- var_9
        let (var_63: int64) = (var_1 + 1L)
        method_20((var_0: (int64 [])), (var_8: string), (var_63: int64), (var_2: int64), (var_10: int64))
and method_24((var_0: (int64 [])), (var_1: int64), (var_2: int64), (var_8: string), (var_9: int64)): unit =
    let (var_32: bool) = (var_9 >= 0L)
    let (var_33: int64) = (int64 var_8.Length)
    let (var_34: bool) = (var_9 < var_33)
    if (var_32 && var_34) then
        let (var_35: char) = var_8.[int32 var_9]
        let (var_36: int64) = (var_9 + 1L)
        let (var_37: bool) = (var_35 >= '0')
        let (var_38: bool) = (var_35 <= '9')
        if (var_37 && var_38) then
            let (var_39: int64) = System.Convert.ToInt64(var_35)
            let (var_40: int64) = System.Convert.ToInt64('0')
            let (var_41: int64) = (var_39 - var_40)
            let (var_42: int64) = (0L + var_41)
            method_25((var_0: (int64 [])), (var_1: int64), (var_2: int64), (var_8: string), (var_42: int64), (var_36: int64))
        else
            (failwith "pint64")
    else
        (failwith "pint64")
and method_25((var_0: (int64 [])), (var_1: int64), (var_2: int64), (var_8: string), (var_9: int64), (var_10: int64)): unit =
    let (var_33: bool) = (var_10 >= 0L)
    let (var_34: int64) = (int64 var_8.Length)
    let (var_35: bool) = (var_10 < var_34)
    if (var_33 && var_35) then
        let (var_36: char) = var_8.[int32 var_10]
        let (var_37: int64) = (var_10 + 1L)
        let (var_38: bool) = (var_36 >= '0')
        let (var_39: bool) = (var_36 <= '9')
        if (var_38 && var_39) then
            let (var_40: int64) = System.Convert.ToInt64(var_36)
            let (var_41: int64) = System.Convert.ToInt64('0')
            let (var_42: int64) = (var_40 - var_41)
            let (var_43: int64) = (var_9 * 10L)
            let (var_44: int64) = (var_43 + var_42)
            method_25((var_0: (int64 [])), (var_1: int64), (var_2: int64), (var_8: string), (var_44: int64), (var_37: int64))
        else
            method_23((var_0: (int64 [])), (var_1: int64), (var_2: int64), (var_8: string), (var_9: int64), (var_37: int64))
    else
        method_23((var_0: (int64 [])), (var_1: int64), (var_2: int64), (var_8: string), (var_9: int64), (var_10: int64))
and method_26((var_0: (int64 [])), (var_1: int64), (var_2: int64)): Tuple4 =
    let (var_3: int64) = var_0.LongLength
    let (if_var_1: Tuple4) =
        if (var_1 < var_3) then
            let (var_4: int64) = (var_1 + 1L)
            let (var_5: int64) = var_0.[int32 var_1]
            let (if_var_2: Tuple4) =
                if (var_5 > var_2) then
                    Tuple4(1L, var_5)
                else
                    let (if_var_3: Tuple4) =
                        if (var_5 = var_2) then
                            Tuple4(1L, var_2)
                        else
                            Tuple4(0L, var_2)
                    if_var_3
            let (var_6: Tuple4) = if_var_2
            let (var_7: int64) = var_6.mem_0
            let (var_8: int64) = var_6.mem_1
            method_27((var_0: (int64 [])), (var_4: int64), (var_7: int64), (var_8: int64))
        else
            Tuple4(0L, var_2)
    if_var_1
and method_27((var_0: (int64 [])), (var_1: int64), (var_2: int64), (var_3: int64)): Tuple4 =
    let (var_4: int64) = var_0.LongLength
    let (if_var_4: Tuple4) =
        if (var_1 < var_4) then
            let (var_5: int64) = (var_1 + 1L)
            let (var_6: int64) = var_0.[int32 var_1]
            let (if_var_5: Tuple4) =
                if (var_6 > var_3) then
                    Tuple4(1L, var_6)
                else
                    let (if_var_6: Tuple4) =
                        if (var_6 = var_3) then
                            let (var_7: int64) = (var_2 + 1L)
                            Tuple4(var_7, var_3)
                        else
                            Tuple4(var_2, var_3)
                    if_var_6
            let (var_8: Tuple4) = if_var_5
            let (var_9: int64) = var_8.mem_0
            let (var_10: int64) = var_8.mem_1
            method_27((var_0: (int64 [])), (var_5: int64), (var_9: int64), (var_10: int64))
        else
            Tuple4(var_2, var_3)
    if_var_4
and method_28((var_0: int64), (var_6: string), (var_7: int64)): unit =
    let (var_30: bool) = (var_7 >= 0L)
    let (var_31: int64) = (int64 var_6.Length)
    let (var_32: bool) = (var_7 < var_31)
    if (var_30 && var_32) then
        let (var_33: char) = var_6.[int32 var_7]
        let (var_34: int64) = (var_7 + 1L)
        let (var_35: bool) = (var_33 >= '0')
        let (var_36: bool) = (var_33 <= '9')
        if (var_35 && var_36) then
            let (var_37: int64) = System.Convert.ToInt64(var_33)
            let (var_38: int64) = System.Convert.ToInt64('0')
            let (var_39: int64) = (var_37 - var_38)
            let (var_40: int64) = (0L + var_39)
            method_29((var_0: int64), (var_6: string), (var_40: int64), (var_34: int64))
        else
            (failwith "pint64")
    else
        (failwith "pint64")
and method_29((var_0: int64), (var_6: string), (var_7: int64), (var_8: int64)): unit =
    let (var_31: bool) = (var_8 >= 0L)
    let (var_32: int64) = (int64 var_6.Length)
    let (var_33: bool) = (var_8 < var_32)
    if (var_31 && var_33) then
        let (var_34: char) = var_6.[int32 var_8]
        let (var_35: int64) = (var_8 + 1L)
        let (var_36: bool) = (var_34 >= '0')
        let (var_37: bool) = (var_34 <= '9')
        if (var_36 && var_37) then
            let (var_38: int64) = System.Convert.ToInt64(var_34)
            let (var_39: int64) = System.Convert.ToInt64('0')
            let (var_40: int64) = (var_38 - var_39)
            let (var_41: int64) = (var_7 * 10L)
            let (var_42: int64) = (var_41 + var_40)
            method_29((var_0: int64), (var_6: string), (var_42: int64), (var_35: int64))
        else
            method_19((var_0: int64), (var_6: string), (var_7: int64), (var_35: int64))
    else
        method_19((var_0: int64), (var_6: string), (var_7: int64), (var_8: int64))
and method_30((var_5: string), (var_6: int64)): unit =
    let (var_29: bool) = (var_6 >= 0L)
    let (var_30: int64) = (int64 var_5.Length)
    let (var_31: bool) = (var_6 < var_30)
    if (var_29 && var_31) then
        let (var_32: char) = var_5.[int32 var_6]
        let (var_33: int64) = (var_6 + 1L)
        let (var_34: bool) = (var_32 >= '0')
        let (var_35: bool) = (var_32 <= '9')
        if (var_34 && var_35) then
            let (var_36: int64) = System.Convert.ToInt64(var_32)
            let (var_37: int64) = System.Convert.ToInt64('0')
            let (var_38: int64) = (var_36 - var_37)
            let (var_39: int64) = (0L + var_38)
            method_31((var_5: string), (var_39: int64), (var_33: int64))
        else
            (failwith "pint64")
    else
        (failwith "pint64")
and method_31((var_5: string), (var_6: int64), (var_7: int64)): unit =
    let (var_30: bool) = (var_7 >= 0L)
    let (var_31: int64) = (int64 var_5.Length)
    let (var_32: bool) = (var_7 < var_31)
    if (var_30 && var_32) then
        let (var_33: char) = var_5.[int32 var_7]
        let (var_34: int64) = (var_7 + 1L)
        let (var_35: bool) = (var_33 >= '0')
        let (var_36: bool) = (var_33 <= '9')
        if (var_35 && var_36) then
            let (var_37: int64) = System.Convert.ToInt64(var_33)
            let (var_38: int64) = System.Convert.ToInt64('0')
            let (var_39: int64) = (var_37 - var_38)
            let (var_40: int64) = (var_6 * 10L)
            let (var_41: int64) = (var_40 + var_39)
            method_31((var_5: string), (var_41: int64), (var_34: int64))
        else
            method_16((var_5: string), (var_6: int64), (var_34: int64))
    else
        method_16((var_5: string), (var_6: int64), (var_7: int64))
let (var_27: System.IO.Stream) = System.Console.OpenStandardInput()
let (var_30: System.IO.StreamReader) = System.IO.StreamReader(var_27)
let (var_32: string) = var_30.ReadToEnd()
let (var_34: int64) = 0L
let (var_65: bool) = (var_34 >= 0L)
let (var_66: int64) = (int64 var_32.Length)
let (var_67: bool) = (var_34 < var_66)
if (var_65 && var_67) then
    let (var_68: char) = var_32.[int32 var_34]
    let (var_69: int64) = (var_34 + 1L)
    if ('-' = var_68) then
        method_14((var_32: string), (var_34: int64), (var_69: int64))
    else
        method_30((var_32: string), (var_34: int64))
else
    method_30((var_32: string), (var_34: int64))

