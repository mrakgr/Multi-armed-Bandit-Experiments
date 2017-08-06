let rec method_0((var_3: int64 ref), (var_4: string), (var_5: int64 ref), (var_7: int64)): unit =
    let (var_10: int64) = (!var_5)
    let (var_11: bool) = (var_10 >= 0L)
    let (var_12: int64) = (int64 var_4.Length)
    let (var_13: bool) = (var_10 < var_12)
    let (var_14: bool) = (var_11 && var_13)
    if var_14 then
        let (var_16: char) = var_4.[int32 (var_10)]
        let (var_20: int64) = (!var_5)
        let (var_21: int64) = (var_20 + 1L)
        var_5 := var_21
        let (var_24: bool) = (var_16 >= '0')
        let (var_25: bool) = (var_16 <= '9')
        let (var_26: bool) = (var_24 && var_25)
        if var_26 then
            let (var_29: int64) = System.Convert.ToInt64(var_16)
            let (var_30: int64) = System.Convert.ToInt64('0')
            let (var_31: int64) = (var_29 - var_30)
            let (var_32: int64) = (var_7 * 10L)
            let (var_33: int64) = (var_32 + var_31)
            method_1((var_3: int64 ref), (var_4: string), (var_5: int64 ref), (var_33: int64))
            ()
        else
            ()
        ()
    else
        ()
    ()
and method_1((var_3: int64 ref), (var_4: string), (var_5: int64 ref), (var_6: int64)): unit =
    let (var_10: int64) = (!var_5)
    let (var_11: bool) = (var_10 >= 0L)
    let (var_12: int64) = (int64 var_4.Length)
    let (var_13: bool) = (var_10 < var_12)
    let (var_14: bool) = (var_11 && var_13)
    if var_14 then
        let (var_16: char) = var_4.[int32 (var_10)]
        let (var_20: int64) = (!var_5)
        let (var_21: int64) = (var_20 + 1L)
        var_5 := var_21
        let (var_24: bool) = (var_16 >= '0')
        let (var_25: bool) = (var_16 <= '9')
        let (var_26: bool) = (var_24 && var_25)
        if var_26 then
            let (var_29: int64) = System.Convert.ToInt64(var_16)
            let (var_30: int64) = System.Convert.ToInt64('0')
            let (var_31: int64) = (var_29 - var_30)
            let (var_32: int64) = (var_6 * 10L)
            let (var_33: int64) = (var_32 + var_31)
            method_1((var_3: int64 ref), (var_4: string), (var_5: int64 ref), (var_33: int64))
            ()
        else
            method_2((var_3: int64 ref), (var_4: string), (var_5: int64 ref), (var_6: int64))
            ()
        ()
    else
        method_2((var_3: int64 ref), (var_4: string), (var_5: int64 ref), (var_6: int64))
        ()
    ()
and method_2((var_0: int64 ref), (var_1: string), (var_2: int64 ref), (var_3: int64)): unit =
    let (var_6: int64) = (!var_2)
    let (var_7: bool) = (var_6 >= 0L)
    let (var_8: int64) = (int64 var_1.Length)
    let (var_9: bool) = (var_6 < var_8)
    let (var_10: bool) = (var_7 && var_9)
    if var_10 then
        let (var_12: char) = var_1.[int32 (var_6)]
        let (var_16: int64) = (!var_2)
        let (var_17: int64) = (var_16 + 1L)
        var_2 := var_17
        let (var_20: bool) = (var_12 = ' ')
        let (var_21: bool) = (var_12 = '\n')
        let (var_22: bool) = (var_12 = '\r')
        let (var_23: bool) = (var_21 || var_22)
        let (var_24: bool) = (var_20 || var_23)
        if var_24 then
            method_2((var_0: int64 ref), (var_1: string), (var_2: int64 ref), (var_3: int64))
            ()
        else
            let (var_28: int64) = (!var_2)
            let (var_29: int64) = (var_28 + -1L)
            var_2 := var_29
            var_0 := var_3
            ()
        ()
    else
        var_0 := var_3
        ()
    ()
let (var_21: int64 ref) = (ref 0L)
let (var_24: string) = System.Console.ReadLine()
let (var_26: int64 ref) = (ref 0L)
let (var_28: int64) = 0L
method_0((var_21: int64 ref), (var_24: string), (var_26: int64 ref), (var_28: int64))
let (var_30: int64) = (!var_21)
System.Console.Write(var_30)
System.Console.WriteLine()