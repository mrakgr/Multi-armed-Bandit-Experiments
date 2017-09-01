let rec method_14((var_4: string), (var_5: int64)): unit =
    let (var_22: bool) = (var_5 >= 0L)
    let (var_23: int64) = (int64 var_4.Length)
    let (var_24: bool) = (var_5 < var_23)
    if (var_22 && var_24) then
        let (var_25: char) = var_4.[int32 var_5]
        let (var_26: int64) = (var_5 + 1L)
        let (var_27: bool) = (var_25 = ' ')
        let (var_28: bool) = (var_25 = '\n')
        let (var_29: bool) = (var_25 = '\r')
        let (var_30: bool) = (var_28 || var_29)
        if (var_27 || var_30) then
            method_14((var_4: string), (var_26: int64))
            ()
        else
            method_15((var_4: string), (var_5: int64))
            ()
        ()
    else
        method_15((var_4: string), (var_5: int64))
        ()
    ()
and method_15((var_4: string), (var_5: int64)): unit =
    let (var_28: bool) = (var_5 >= 0L)
    let (var_29: int64) = (int64 var_4.Length)
    let (var_30: bool) = (var_5 < var_29)
    if (var_28 && var_30) then
        let (var_31: char) = var_4.[int32 var_5]
        let (var_32: int64) = (var_5 + 1L)
        let (var_33: bool) = (var_31 >= '0')
        let (var_34: bool) = (var_31 <= '9')
        if (var_33 && var_34) then
            let (var_35: int64) = System.Convert.ToInt64(var_31)
            let (var_36: int64) = System.Convert.ToInt64('0')
            let (var_37: int64) = (var_35 - var_36)
            let (var_38: int64) = (0L + var_37)
            method_16((var_4: string), (var_38: int64), (var_32: int64))
            ()
        else
            ()
        ()
    else
        ()
    ()
and method_16((var_4: string), (var_5: int64), (var_6: int64)): unit =
    let (var_29: bool) = (var_6 >= 0L)
    let (var_30: int64) = (int64 var_4.Length)
    let (var_31: bool) = (var_6 < var_30)
    if (var_29 && var_31) then
        let (var_32: char) = var_4.[int32 var_6]
        let (var_33: int64) = (var_6 + 1L)
        let (var_34: bool) = (var_32 >= '0')
        let (var_35: bool) = (var_32 <= '9')
        if (var_34 && var_35) then
            let (var_36: int64) = System.Convert.ToInt64(var_32)
            let (var_37: int64) = System.Convert.ToInt64('0')
            let (var_38: int64) = (var_36 - var_37)
            let (var_39: int64) = (var_5 * 10L)
            let (var_40: int64) = (var_39 + var_38)
            method_16((var_4: string), (var_40: int64), (var_33: int64))
            ()
        else
            method_17((var_4: string), (var_5: int64), (var_33: int64))
            ()
        ()
    else
        method_17((var_4: string), (var_5: int64), (var_6: int64))
        ()
    ()
and method_17((var_4: string), (var_5: int64), (var_6: int64)): unit =
    let (var_23: bool) = (var_6 >= 0L)
    let (var_24: int64) = (int64 var_4.Length)
    let (var_25: bool) = (var_6 < var_24)
    if (var_23 && var_25) then
        let (var_26: char) = var_4.[int32 var_6]
        let (var_27: int64) = (var_6 + 1L)
        let (var_28: bool) = (var_26 = ' ')
        let (var_29: bool) = (var_26 = '\n')
        let (var_30: bool) = (var_26 = '\r')
        let (var_31: bool) = (var_29 || var_30)
        if (var_28 || var_31) then
            method_17((var_4: string), (var_5: int64), (var_27: int64))
            ()
        else
            if (var_5 > 0L) then
                method_18((var_5: int64), (var_4: string), (var_6: int64))
                ()
            else
                ()
            ()
        ()
    else
        if (var_5 > 0L) then
            method_18((var_5: int64), (var_4: string), (var_6: int64))
            ()
        else
            ()
        ()
    ()
and method_18((var_0: int64), (var_5: string), (var_6: int64)): unit =
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
            method_19((var_0: int64), (var_5: string), (var_39: int64), (var_33: int64))
            ()
        else
            ()
        ()
    else
        ()
    ()
and method_19((var_0: int64), (var_5: string), (var_6: int64), (var_7: int64)): unit =
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
            method_19((var_0: int64), (var_5: string), (var_41: int64), (var_34: int64))
            ()
        else
            method_20((var_0: int64), (var_5: string), (var_6: int64), (var_34: int64))
            ()
        ()
    else
        method_20((var_0: int64), (var_5: string), (var_6: int64), (var_7: int64))
        ()
    ()
and method_20((var_0: int64), (var_5: string), (var_6: int64), (var_7: int64)): unit =
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
            method_20((var_0: int64), (var_5: string), (var_6: int64), (var_28: int64))
            ()
        else
            let (var_46: (int64 [])) = Array.zeroCreate<int64> (System.Convert.ToInt32(var_0))
            var_46.[int32 0L] <- var_6
            let (var_48: int64) = 1L
            method_21((var_46: (int64 [])), (var_5: string), (var_48: int64), (var_0: int64), (var_7: int64))
            ()
        ()
    else
        let (var_60: (int64 [])) = Array.zeroCreate<int64> (System.Convert.ToInt32(var_0))
        var_60.[int32 0L] <- var_6
        let (var_62: int64) = 1L
        method_21((var_60: (int64 [])), (var_5: string), (var_62: int64), (var_0: int64), (var_7: int64))
        ()
    ()
and method_21((var_0: (int64 [])), (var_3: string), (var_4: int64), (var_5: int64), (var_8: int64)): unit =
    if (var_4 < var_5) then
        method_22((var_0: (int64 [])), (var_4: int64), (var_5: int64), (var_3: string), (var_8: int64))
        ()
    else
        let (var_22: int64) = 0L
        let (var_23: int64) = method_25((var_0: (int64 [])), (var_22: int64))
        System.Console.WriteLine(var_23)
        ()
    ()
and method_22((var_0: (int64 [])), (var_1: int64), (var_2: int64), (var_7: string), (var_8: int64)): unit =
    let (var_31: bool) = (var_8 >= 0L)
    let (var_32: int64) = (int64 var_7.Length)
    let (var_33: bool) = (var_8 < var_32)
    if (var_31 && var_33) then
        let (var_34: char) = var_7.[int32 var_8]
        let (var_35: int64) = (var_8 + 1L)
        let (var_36: bool) = (var_34 >= '0')
        let (var_37: bool) = (var_34 <= '9')
        if (var_36 && var_37) then
            let (var_38: int64) = System.Convert.ToInt64(var_34)
            let (var_39: int64) = System.Convert.ToInt64('0')
            let (var_40: int64) = (var_38 - var_39)
            let (var_41: int64) = (0L + var_40)
            method_23((var_0: (int64 [])), (var_1: int64), (var_2: int64), (var_7: string), (var_41: int64), (var_35: int64))
            ()
        else
            ()
        ()
    else
        ()
    ()
and method_23((var_0: (int64 [])), (var_1: int64), (var_2: int64), (var_7: string), (var_8: int64), (var_9: int64)): unit =
    let (var_32: bool) = (var_9 >= 0L)
    let (var_33: int64) = (int64 var_7.Length)
    let (var_34: bool) = (var_9 < var_33)
    if (var_32 && var_34) then
        let (var_35: char) = var_7.[int32 var_9]
        let (var_36: int64) = (var_9 + 1L)
        let (var_37: bool) = (var_35 >= '0')
        let (var_38: bool) = (var_35 <= '9')
        if (var_37 && var_38) then
            let (var_39: int64) = System.Convert.ToInt64(var_35)
            let (var_40: int64) = System.Convert.ToInt64('0')
            let (var_41: int64) = (var_39 - var_40)
            let (var_42: int64) = (var_8 * 10L)
            let (var_43: int64) = (var_42 + var_41)
            method_23((var_0: (int64 [])), (var_1: int64), (var_2: int64), (var_7: string), (var_43: int64), (var_36: int64))
            ()
        else
            method_24((var_0: (int64 [])), (var_1: int64), (var_2: int64), (var_7: string), (var_8: int64), (var_36: int64))
            ()
        ()
    else
        method_24((var_0: (int64 [])), (var_1: int64), (var_2: int64), (var_7: string), (var_8: int64), (var_9: int64))
        ()
    ()
and method_24((var_0: (int64 [])), (var_1: int64), (var_2: int64), (var_7: string), (var_8: int64), (var_9: int64)): unit =
    let (var_26: bool) = (var_9 >= 0L)
    let (var_27: int64) = (int64 var_7.Length)
    let (var_28: bool) = (var_9 < var_27)
    if (var_26 && var_28) then
        let (var_29: char) = var_7.[int32 var_9]
        let (var_30: int64) = (var_9 + 1L)
        let (var_31: bool) = (var_29 = ' ')
        let (var_32: bool) = (var_29 = '\n')
        let (var_33: bool) = (var_29 = '\r')
        let (var_34: bool) = (var_32 || var_33)
        if (var_31 || var_34) then
            method_24((var_0: (int64 [])), (var_1: int64), (var_2: int64), (var_7: string), (var_8: int64), (var_30: int64))
            ()
        else
            var_0.[int32 var_1] <- var_8
            let (var_49: int64) = (var_1 + 1L)
            method_21((var_0: (int64 [])), (var_7: string), (var_49: int64), (var_2: int64), (var_9: int64))
            ()
        ()
    else
        var_0.[int32 var_1] <- var_8
        let (var_62: int64) = (var_1 + 1L)
        method_21((var_0: (int64 [])), (var_7: string), (var_62: int64), (var_2: int64), (var_9: int64))
        ()
    ()
and method_25((var_0: (int64 [])), (var_1: int64)): int64 =
    let (var_2: int64) = var_0.LongLength
    let (if_var_1: int64) =
        if (var_1 < var_2) then
            let (var_3: int64) = (var_1 + 1L)
            let (var_4: int64) = var_0.[int32 var_1]
            let (var_5: int64) = (0L + var_4)
            let (var_6: int64) = method_26((var_0: (int64 [])), (var_3: int64), (var_5: int64))
            var_6
        else
            0L
    if_var_1
and method_26((var_0: (int64 [])), (var_1: int64), (var_2: int64)): int64 =
    let (var_3: int64) = var_0.LongLength
    let (if_var_2: int64) =
        if (var_1 < var_3) then
            let (var_4: int64) = (var_1 + 1L)
            let (var_5: int64) = var_0.[int32 var_1]
            let (var_6: int64) = (var_2 + var_5)
            let (var_7: int64) = method_26((var_0: (int64 [])), (var_4: int64), (var_6: int64))
            var_7
        else
            var_2
    if_var_2
let (var_27: System.IO.Stream) = System.Console.OpenStandardInput()
let (var_30: System.IO.StreamReader) = System.IO.StreamReader(var_27)
let (var_32: string) = var_30.ReadToEnd()
let (var_34: int64) = 0L
let (var_35: int64) = var_34
method_14((var_32: string), (var_35: int64))

