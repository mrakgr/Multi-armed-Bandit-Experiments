type Tuple4 =
    struct
    val mem_0: int64
    val mem_1: int64
    new(arg_mem_0, arg_mem_1) = {mem_0 = arg_mem_0; mem_1 = arg_mem_1}
    end
let rec method_14((var_0: bool), (var_1: int64), (var_7: string), (var_8: int64)): unit =
    let (var_29: bool) = (var_8 >= 0L)
    let (var_30: int64) = (int64 var_7.Length)
    let (var_31: bool) = (var_8 < var_30)
    if (var_29 && var_31) then
        let (var_32: char) = var_7.[int32 var_8]
        let (var_33: int64) = (var_8 + 1L)
        let (var_34: bool) = (var_32 >= '0')
        let (var_35: bool) = (var_32 <= '9')
        let (var_36: bool) = (var_34 && var_35)
        if var_36 then
            let (var_43: int64) = System.Convert.ToInt64(var_32)
            let (var_44: int64) = System.Convert.ToInt64('0')
            let (var_45: int64) = (var_43 - var_44)
            let (var_46: bool) = (var_45 <= 7L)
            let (var_49: int64) = (0L + var_45)
            method_15((var_0: bool), (var_1: int64), (var_7: string), (var_49: int64), (var_33: int64))
        else
            (failwith "pint64")
    else
        (failwith "pint64")
and method_15((var_0: bool), (var_1: int64), (var_7: string), (var_8: int64), (var_9: int64)): unit =
    let (var_30: bool) = (var_9 >= 0L)
    let (var_31: int64) = (int64 var_7.Length)
    let (var_32: bool) = (var_9 < var_31)
    if (var_30 && var_32) then
        let (var_33: char) = var_7.[int32 var_9]
        let (var_34: int64) = (var_9 + 1L)
        let (var_35: bool) = (var_33 >= '0')
        let (var_36: bool) = (var_33 <= '9')
        let (var_37: bool) = (var_35 && var_36)
        if var_37 then
            let (var_44: int64) = System.Convert.ToInt64(var_33)
            let (var_45: int64) = System.Convert.ToInt64('0')
            let (var_46: int64) = (var_44 - var_45)
            let (var_47: bool) = (var_8 = 922337203685477580L)
            let (var_48: bool) = (var_46 <= 7L)
            let (var_49: bool) = (var_47 && var_48)
            let (var_50: bool) = (var_8 < 922337203685477580L)
            let (var_51: bool) = (var_49 || var_50)
            if var_51 then
                let (var_54: int64) = (var_8 * 10L)
                let (var_55: int64) = (var_54 + var_46)
                method_15((var_0: bool), (var_1: int64), (var_7: string), (var_55: int64), (var_34: int64))
            else
                (failwith "integer overflow")
        else
            let (if_var_1: int64) =
                if var_0 then
                    var_8
                else
                    (-var_8)
            let (var_75: int64) = if_var_1
            method_16((var_75: int64), (var_1: int64), (var_7: string), (var_9: int64))
    else
        let (if_var_2: int64) =
            if var_0 then
                var_8
            else
                (-var_8)
        let (var_87: int64) = if_var_2
        method_16((var_87: int64), (var_1: int64), (var_7: string), (var_9: int64))
and method_16((var_0: int64), (var_4: int64), (var_7: string), (var_8: int64)): unit =
    let (var_9: int64) = (var_4 + 1L)
    let (var_30: bool) = (var_8 >= 0L)
    let (var_31: int64) = (int64 var_7.Length)
    let (var_32: bool) = (var_8 < var_31)
    if (var_30 && var_32) then
        let (var_33: char) = var_7.[int32 var_8]
        let (var_34: int64) = (var_8 + 1L)
        let (var_35: bool) = (var_33 = ' ')
        let (var_36: bool) = (var_33 = '\n')
        let (var_37: bool) = (var_33 = '\r')
        let (var_38: bool) = (var_36 || var_37)
        let (var_39: bool) = (var_35 || var_38)
        if var_39 then
            method_17((var_0: int64), (var_4: int64), (var_7: string), (var_9: int64), (var_34: int64))
        else
            let (var_77: bool) = (var_0 > 0L)
            if var_77 then
                if (var_30 && var_32) then
                    let (var_100: bool) = ('-' = var_33)
                    if var_100 then
                        let (var_111: bool) = false
                        method_18((var_111: bool), (var_4: int64), (var_0: int64), (var_7: string), (var_34: int64))
                    else
                        let (var_131: bool) = true
                        method_18((var_131: bool), (var_4: int64), (var_0: int64), (var_7: string), (var_8: int64))
                else
                    let (var_143: bool) = true
                    method_18((var_143: bool), (var_4: int64), (var_0: int64), (var_7: string), (var_8: int64))
            else
                (failwith "n in parse array must be > 0")
    else
        let (var_171: bool) = (var_0 > 0L)
        if var_171 then
            if (var_30 && var_32) then
                let (var_194: char) = var_7.[int32 var_8]
                let (var_195: int64) = (var_8 + 1L)
                let (var_196: bool) = ('-' = var_194)
                if var_196 then
                    let (var_207: bool) = false
                    method_18((var_207: bool), (var_4: int64), (var_0: int64), (var_7: string), (var_195: int64))
                else
                    let (var_227: bool) = true
                    method_18((var_227: bool), (var_4: int64), (var_0: int64), (var_7: string), (var_8: int64))
            else
                let (var_239: bool) = true
                method_18((var_239: bool), (var_4: int64), (var_0: int64), (var_7: string), (var_8: int64))
        else
            (failwith "n in parse array must be > 0")
and method_17((var_0: int64), (var_4: int64), (var_7: string), (var_8: int64), (var_9: int64)): unit =
    let (var_10: int64) = (var_8 + 1L)
    let (var_31: bool) = (var_9 >= 0L)
    let (var_32: int64) = (int64 var_7.Length)
    let (var_33: bool) = (var_9 < var_32)
    if (var_31 && var_33) then
        let (var_34: char) = var_7.[int32 var_9]
        let (var_35: int64) = (var_9 + 1L)
        let (var_36: bool) = (var_34 = ' ')
        let (var_37: bool) = (var_34 = '\n')
        let (var_38: bool) = (var_34 = '\r')
        let (var_39: bool) = (var_37 || var_38)
        let (var_40: bool) = (var_36 || var_39)
        if var_40 then
            method_17((var_0: int64), (var_4: int64), (var_7: string), (var_10: int64), (var_35: int64))
        else
            let (var_78: bool) = (var_0 > 0L)
            if var_78 then
                if (var_31 && var_33) then
                    let (var_101: bool) = ('-' = var_34)
                    if var_101 then
                        let (var_112: bool) = false
                        method_18((var_112: bool), (var_4: int64), (var_0: int64), (var_7: string), (var_35: int64))
                    else
                        let (var_132: bool) = true
                        method_18((var_132: bool), (var_4: int64), (var_0: int64), (var_7: string), (var_9: int64))
                else
                    let (var_144: bool) = true
                    method_18((var_144: bool), (var_4: int64), (var_0: int64), (var_7: string), (var_9: int64))
            else
                (failwith "n in parse array must be > 0")
    else
        let (var_172: bool) = (var_0 > 0L)
        if var_172 then
            if (var_31 && var_33) then
                let (var_195: char) = var_7.[int32 var_9]
                let (var_196: int64) = (var_9 + 1L)
                let (var_197: bool) = ('-' = var_195)
                if var_197 then
                    let (var_208: bool) = false
                    method_18((var_208: bool), (var_4: int64), (var_0: int64), (var_7: string), (var_196: int64))
                else
                    let (var_228: bool) = true
                    method_18((var_228: bool), (var_4: int64), (var_0: int64), (var_7: string), (var_9: int64))
            else
                let (var_240: bool) = true
                method_18((var_240: bool), (var_4: int64), (var_0: int64), (var_7: string), (var_9: int64))
        else
            (failwith "n in parse array must be > 0")
and method_18((var_0: bool), (var_1: int64), (var_2: int64), (var_8: string), (var_9: int64)): unit =
    let (var_30: bool) = (var_9 >= 0L)
    let (var_31: int64) = (int64 var_8.Length)
    let (var_32: bool) = (var_9 < var_31)
    if (var_30 && var_32) then
        let (var_33: char) = var_8.[int32 var_9]
        let (var_34: int64) = (var_9 + 1L)
        let (var_35: bool) = (var_33 >= '0')
        let (var_36: bool) = (var_33 <= '9')
        let (var_37: bool) = (var_35 && var_36)
        if var_37 then
            let (var_44: int64) = System.Convert.ToInt64(var_33)
            let (var_45: int64) = System.Convert.ToInt64('0')
            let (var_46: int64) = (var_44 - var_45)
            let (var_47: bool) = (var_46 <= 7L)
            let (var_50: int64) = (0L + var_46)
            method_19((var_0: bool), (var_1: int64), (var_2: int64), (var_8: string), (var_50: int64), (var_34: int64))
        else
            (failwith "pint64")
    else
        (failwith "pint64")
and method_19((var_0: bool), (var_1: int64), (var_2: int64), (var_8: string), (var_9: int64), (var_10: int64)): unit =
    let (var_31: bool) = (var_10 >= 0L)
    let (var_32: int64) = (int64 var_8.Length)
    let (var_33: bool) = (var_10 < var_32)
    if (var_31 && var_33) then
        let (var_34: char) = var_8.[int32 var_10]
        let (var_35: int64) = (var_10 + 1L)
        let (var_36: bool) = (var_34 >= '0')
        let (var_37: bool) = (var_34 <= '9')
        let (var_38: bool) = (var_36 && var_37)
        if var_38 then
            let (var_45: int64) = System.Convert.ToInt64(var_34)
            let (var_46: int64) = System.Convert.ToInt64('0')
            let (var_47: int64) = (var_45 - var_46)
            let (var_48: bool) = (var_9 = 922337203685477580L)
            let (var_49: bool) = (var_47 <= 7L)
            let (var_50: bool) = (var_48 && var_49)
            let (var_51: bool) = (var_9 < 922337203685477580L)
            let (var_52: bool) = (var_50 || var_51)
            if var_52 then
                let (var_55: int64) = (var_9 * 10L)
                let (var_56: int64) = (var_55 + var_47)
                method_19((var_0: bool), (var_1: int64), (var_2: int64), (var_8: string), (var_56: int64), (var_35: int64))
            else
                (failwith "integer overflow")
        else
            let (if_var_3: int64) =
                if var_0 then
                    var_9
                else
                    (-var_9)
            let (var_76: int64) = if_var_3
            method_20((var_76: int64), (var_2: int64), (var_1: int64), (var_8: string), (var_10: int64))
    else
        let (if_var_4: int64) =
            if var_0 then
                var_9
            else
                (-var_9)
        let (var_88: int64) = if_var_4
        method_20((var_88: int64), (var_2: int64), (var_1: int64), (var_8: string), (var_10: int64))
and method_20((var_0: int64), (var_1: int64), (var_4: int64), (var_8: string), (var_9: int64)): unit =
    let (var_10: int64) = (var_4 + 1L)
    let (var_31: bool) = (var_9 >= 0L)
    let (var_32: int64) = (int64 var_8.Length)
    let (var_33: bool) = (var_9 < var_32)
    if (var_31 && var_33) then
        let (var_34: char) = var_8.[int32 var_9]
        let (var_35: int64) = (var_9 + 1L)
        let (var_36: bool) = (var_34 = ' ')
        let (var_37: bool) = (var_34 = '\n')
        let (var_38: bool) = (var_34 = '\r')
        let (var_39: bool) = (var_37 || var_38)
        let (var_40: bool) = (var_36 || var_39)
        if var_40 then
            method_21((var_0: int64), (var_1: int64), (var_4: int64), (var_8: string), (var_10: int64), (var_35: int64))
        else
            let (var_78: (int64 [])) = Array.zeroCreate<int64> (System.Convert.ToInt32(var_1))
            var_78.[int32 0L] <- var_0
            let (var_80: int64) = 1L
            method_22((var_78: (int64 [])), (var_8: string), (var_80: int64), (var_1: int64), (var_4: int64), (var_9: int64))
    else
        let (var_98: (int64 [])) = Array.zeroCreate<int64> (System.Convert.ToInt32(var_1))
        var_98.[int32 0L] <- var_0
        let (var_100: int64) = 1L
        method_22((var_98: (int64 [])), (var_8: string), (var_100: int64), (var_1: int64), (var_4: int64), (var_9: int64))
and method_21((var_0: int64), (var_1: int64), (var_4: int64), (var_8: string), (var_9: int64), (var_10: int64)): unit =
    let (var_11: int64) = (var_9 + 1L)
    let (var_32: bool) = (var_10 >= 0L)
    let (var_33: int64) = (int64 var_8.Length)
    let (var_34: bool) = (var_10 < var_33)
    if (var_32 && var_34) then
        let (var_35: char) = var_8.[int32 var_10]
        let (var_36: int64) = (var_10 + 1L)
        let (var_37: bool) = (var_35 = ' ')
        let (var_38: bool) = (var_35 = '\n')
        let (var_39: bool) = (var_35 = '\r')
        let (var_40: bool) = (var_38 || var_39)
        let (var_41: bool) = (var_37 || var_40)
        if var_41 then
            method_21((var_0: int64), (var_1: int64), (var_4: int64), (var_8: string), (var_11: int64), (var_36: int64))
        else
            let (var_79: (int64 [])) = Array.zeroCreate<int64> (System.Convert.ToInt32(var_1))
            var_79.[int32 0L] <- var_0
            let (var_81: int64) = 1L
            method_22((var_79: (int64 [])), (var_8: string), (var_81: int64), (var_1: int64), (var_4: int64), (var_10: int64))
    else
        let (var_99: (int64 [])) = Array.zeroCreate<int64> (System.Convert.ToInt32(var_1))
        var_99.[int32 0L] <- var_0
        let (var_101: int64) = 1L
        method_22((var_99: (int64 [])), (var_8: string), (var_101: int64), (var_1: int64), (var_4: int64), (var_10: int64))
and method_22((var_0: (int64 [])), (var_4: string), (var_5: int64), (var_6: int64), (var_9: int64), (var_10: int64)): unit =
    let (var_11: bool) = (var_5 < var_6)
    if var_11 then
        let (var_32: bool) = (var_10 >= 0L)
        let (var_33: int64) = (int64 var_4.Length)
        let (var_34: bool) = (var_10 < var_33)
        if (var_32 && var_34) then
            let (var_35: char) = var_4.[int32 var_10]
            let (var_36: int64) = (var_10 + 1L)
            let (var_37: bool) = ('-' = var_35)
            if var_37 then
                let (var_48: bool) = false
                method_23((var_48: bool), (var_9: int64), (var_0: (int64 [])), (var_5: int64), (var_6: int64), (var_4: string), (var_36: int64))
            else
                let (var_68: bool) = true
                method_23((var_68: bool), (var_9: int64), (var_0: (int64 [])), (var_5: int64), (var_6: int64), (var_4: string), (var_10: int64))
        else
            let (var_80: bool) = true
            method_23((var_80: bool), (var_9: int64), (var_0: (int64 [])), (var_5: int64), (var_6: int64), (var_4: string), (var_10: int64))
    else
        let (var_96: int64) = System.Int64.MinValue
        let (var_97: int64) = 0L
        let (var_98: Tuple4) = method_27((var_0: (int64 [])), (var_97: int64), (var_96: int64))
        let (var_99: int64) = var_98.mem_0
        let (var_100: int64) = var_98.mem_1
        System.Console.WriteLine(var_99)
and method_23((var_0: bool), (var_1: int64), (var_2: (int64 [])), (var_3: int64), (var_4: int64), (var_10: string), (var_11: int64)): unit =
    let (var_32: bool) = (var_11 >= 0L)
    let (var_33: int64) = (int64 var_10.Length)
    let (var_34: bool) = (var_11 < var_33)
    if (var_32 && var_34) then
        let (var_35: char) = var_10.[int32 var_11]
        let (var_36: int64) = (var_11 + 1L)
        let (var_37: bool) = (var_35 >= '0')
        let (var_38: bool) = (var_35 <= '9')
        let (var_39: bool) = (var_37 && var_38)
        if var_39 then
            let (var_46: int64) = System.Convert.ToInt64(var_35)
            let (var_47: int64) = System.Convert.ToInt64('0')
            let (var_48: int64) = (var_46 - var_47)
            let (var_49: bool) = (var_48 <= 7L)
            let (var_52: int64) = (0L + var_48)
            method_24((var_0: bool), (var_1: int64), (var_2: (int64 [])), (var_3: int64), (var_4: int64), (var_10: string), (var_52: int64), (var_36: int64))
        else
            (failwith "pint64")
    else
        (failwith "pint64")
and method_24((var_0: bool), (var_1: int64), (var_2: (int64 [])), (var_3: int64), (var_4: int64), (var_10: string), (var_11: int64), (var_12: int64)): unit =
    let (var_33: bool) = (var_12 >= 0L)
    let (var_34: int64) = (int64 var_10.Length)
    let (var_35: bool) = (var_12 < var_34)
    if (var_33 && var_35) then
        let (var_36: char) = var_10.[int32 var_12]
        let (var_37: int64) = (var_12 + 1L)
        let (var_38: bool) = (var_36 >= '0')
        let (var_39: bool) = (var_36 <= '9')
        let (var_40: bool) = (var_38 && var_39)
        if var_40 then
            let (var_47: int64) = System.Convert.ToInt64(var_36)
            let (var_48: int64) = System.Convert.ToInt64('0')
            let (var_49: int64) = (var_47 - var_48)
            let (var_50: bool) = (var_11 = 922337203685477580L)
            let (var_51: bool) = (var_49 <= 7L)
            let (var_52: bool) = (var_50 && var_51)
            let (var_53: bool) = (var_11 < 922337203685477580L)
            let (var_54: bool) = (var_52 || var_53)
            if var_54 then
                let (var_57: int64) = (var_11 * 10L)
                let (var_58: int64) = (var_57 + var_49)
                method_24((var_0: bool), (var_1: int64), (var_2: (int64 [])), (var_3: int64), (var_4: int64), (var_10: string), (var_58: int64), (var_37: int64))
            else
                (failwith "integer overflow")
        else
            let (if_var_5: int64) =
                if var_0 then
                    var_11
                else
                    (-var_11)
            let (var_78: int64) = if_var_5
            method_25((var_78: int64), (var_2: (int64 [])), (var_3: int64), (var_4: int64), (var_1: int64), (var_10: string), (var_12: int64))
    else
        let (if_var_6: int64) =
            if var_0 then
                var_11
            else
                (-var_11)
        let (var_90: int64) = if_var_6
        method_25((var_90: int64), (var_2: (int64 [])), (var_3: int64), (var_4: int64), (var_1: int64), (var_10: string), (var_12: int64))
and method_25((var_0: int64), (var_1: (int64 [])), (var_2: int64), (var_3: int64), (var_6: int64), (var_10: string), (var_11: int64)): unit =
    let (var_12: int64) = (var_6 + 1L)
    let (var_33: bool) = (var_11 >= 0L)
    let (var_34: int64) = (int64 var_10.Length)
    let (var_35: bool) = (var_11 < var_34)
    if (var_33 && var_35) then
        let (var_36: char) = var_10.[int32 var_11]
        let (var_37: int64) = (var_11 + 1L)
        let (var_38: bool) = (var_36 = ' ')
        let (var_39: bool) = (var_36 = '\n')
        let (var_40: bool) = (var_36 = '\r')
        let (var_41: bool) = (var_39 || var_40)
        let (var_42: bool) = (var_38 || var_41)
        if var_42 then
            method_26((var_0: int64), (var_1: (int64 [])), (var_2: int64), (var_3: int64), (var_6: int64), (var_10: string), (var_12: int64), (var_37: int64))
        else
            var_1.[int32 var_2] <- var_0
            let (var_81: int64) = (var_2 + 1L)
            method_22((var_1: (int64 [])), (var_10: string), (var_81: int64), (var_3: int64), (var_6: int64), (var_11: int64))
    else
        var_1.[int32 var_2] <- var_0
        let (var_100: int64) = (var_2 + 1L)
        method_22((var_1: (int64 [])), (var_10: string), (var_100: int64), (var_3: int64), (var_6: int64), (var_11: int64))
and method_26((var_0: int64), (var_1: (int64 [])), (var_2: int64), (var_3: int64), (var_6: int64), (var_10: string), (var_11: int64), (var_12: int64)): unit =
    let (var_13: int64) = (var_11 + 1L)
    let (var_34: bool) = (var_12 >= 0L)
    let (var_35: int64) = (int64 var_10.Length)
    let (var_36: bool) = (var_12 < var_35)
    if (var_34 && var_36) then
        let (var_37: char) = var_10.[int32 var_12]
        let (var_38: int64) = (var_12 + 1L)
        let (var_39: bool) = (var_37 = ' ')
        let (var_40: bool) = (var_37 = '\n')
        let (var_41: bool) = (var_37 = '\r')
        let (var_42: bool) = (var_40 || var_41)
        let (var_43: bool) = (var_39 || var_42)
        if var_43 then
            method_26((var_0: int64), (var_1: (int64 [])), (var_2: int64), (var_3: int64), (var_6: int64), (var_10: string), (var_13: int64), (var_38: int64))
        else
            var_1.[int32 var_2] <- var_0
            let (var_82: int64) = (var_2 + 1L)
            method_22((var_1: (int64 [])), (var_10: string), (var_82: int64), (var_3: int64), (var_6: int64), (var_12: int64))
    else
        var_1.[int32 var_2] <- var_0
        let (var_101: int64) = (var_2 + 1L)
        method_22((var_1: (int64 [])), (var_10: string), (var_101: int64), (var_3: int64), (var_6: int64), (var_12: int64))
and method_27((var_0: (int64 [])), (var_1: int64), (var_2: int64)): Tuple4 =
    let (var_3: int64) = var_0.LongLength
    if (var_1 < var_3) then
        let (var_4: int64) = (var_1 + 1L)
        let (var_5: int64) = var_0.[int32 var_1]
        let (if_var_8: Tuple4) =
            if (var_5 > var_2) then
                Tuple4(1L, var_5)
            else
                let (if_var_9: Tuple4) =
                    if (var_5 = var_2) then
                        Tuple4(1L, var_2)
                    else
                        Tuple4(0L, var_2)
                if_var_9
        let (var_6: Tuple4) = if_var_8
        let (var_7: int64) = var_6.mem_0
        let (var_8: int64) = var_6.mem_1
        method_28((var_0: (int64 [])), (var_4: int64), (var_7: int64), (var_8: int64))
    else
        Tuple4(0L, var_2)
and method_28((var_0: (int64 [])), (var_1: int64), (var_2: int64), (var_3: int64)): Tuple4 =
    let (var_4: int64) = var_0.LongLength
    if (var_1 < var_4) then
        let (var_5: int64) = (var_1 + 1L)
        let (var_6: int64) = var_0.[int32 var_1]
        let (if_var_11: Tuple4) =
            if (var_6 > var_3) then
                Tuple4(1L, var_6)
            else
                let (if_var_12: Tuple4) =
                    if (var_6 = var_3) then
                        let (var_7: int64) = (var_2 + 1L)
                        Tuple4(var_7, var_3)
                    else
                        Tuple4(var_2, var_3)
                if_var_12
        let (var_8: Tuple4) = if_var_11
        let (var_9: int64) = var_8.mem_0
        let (var_10: int64) = var_8.mem_1
        method_28((var_0: (int64 [])), (var_5: int64), (var_9: int64), (var_10: int64))
    else
        Tuple4(var_2, var_3)
let (var_19: int64) = 0L
let (var_27: System.IO.Stream) = System.Console.OpenStandardInput()
let (var_30: System.IO.StreamReader) = System.IO.StreamReader(var_27)
let f n m =
    let rng = System.Random()
    let ar =
        Array.init n (fun i -> rng.Next(1,m) |> string)
        |> String.concat " "
    sprintf "%i\n%s" n ar
let (var_32: string) = f (1000*100) 100
let (var_34: int64) = 0L
let (var_55: bool) = (var_34 >= 0L)
let (var_56: int64) = (int64 var_32.Length)
let (var_57: bool) = (var_34 < var_56)
if (var_55 && var_57) then
    let (var_58: char) = var_32.[int32 var_34]
    let (var_59: int64) = (var_34 + 1L)
    let (var_60: bool) = ('-' = var_58)
    if var_60 then
        let (var_71: bool) = false
        method_14((var_71: bool), (var_19: int64), (var_32: string), (var_59: int64))
    else
        let (var_91: bool) = true
        method_14((var_91: bool), (var_19: int64), (var_32: string), (var_34: int64))
else
    let (var_103: bool) = true
    method_14((var_103: bool), (var_19: int64), (var_32: string), (var_34: int64))

