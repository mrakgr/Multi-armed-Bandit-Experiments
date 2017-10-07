let rec method_14((var_0: int64), (var_1: int64), (var_2: int64), (var_3: int64), (var_4: int64), (var_5: int64)): unit =
    System.Console.Write("Failure.")
    System.Console.WriteLine()
    if (var_5 < var_2) then
        let (var_6: int64) = 0L
        method_15((var_0: int64), (var_1: int64), (var_3: int64), (var_4: int64), (var_5: int64), (var_2: int64), (var_6: int64))
    else
        ()
and method_15((var_0: int64), (var_1: int64), (var_2: int64), (var_3: int64), (var_4: int64), (var_5: int64), (var_6: int64)): unit =
    let (var_7: int64) = (var_4 + 1L)
    method_14((var_0: int64), (var_1: int64), (var_5: int64), (var_2: int64), (var_3: int64), (var_7: int64))
    if (var_6 < var_5) then
        System.Console.Write("I am at (")
        System.Console.Write(var_4)
        System.Console.Write(",")
        System.Console.Write(var_6)
        System.Console.Write(")")
        System.Console.WriteLine()
        let (var_8: bool) =
            if (var_4 = var_0) then
                (var_6 = var_1)
            else
                false
        if var_8 then
            System.Console.Write("I've found Mario.")
            System.Console.WriteLine()
            let (var_9: int64) = (var_6 + 1L)
            method_16((var_0: int64), (var_1: int64), (var_2: int64), (var_3: int64), (var_4: int64), (var_5: int64), (var_9: int64))
        else
            let (var_10: bool) =
                if (var_4 = var_2) then
                    (var_6 = var_3)
                else
                    false
            if var_10 then
                System.Console.Write("I've found Princess.")
                System.Console.WriteLine()
                let (var_11: int64) = (var_6 + 1L)
                method_18((var_0: int64), (var_1: int64), (var_2: int64), (var_3: int64), (var_4: int64), (var_5: int64), (var_11: int64))
            else
                let (var_12: int64) = (var_6 + 1L)
                method_15((var_0: int64), (var_1: int64), (var_2: int64), (var_3: int64), (var_4: int64), (var_5: int64), (var_12: int64))
    else
        ()
and method_16((var_0: int64), (var_1: int64), (var_2: int64), (var_3: int64), (var_4: int64), (var_5: int64), (var_6: int64)): unit =
    let (var_7: int64) = (var_4 + 1L)
    method_17((var_0: int64), (var_1: int64), (var_5: int64), (var_2: int64), (var_3: int64), (var_7: int64))
    if (var_6 < var_5) then
        System.Console.Write("I am at (")
        System.Console.Write(var_4)
        System.Console.Write(",")
        System.Console.Write(var_6)
        System.Console.Write(")")
        System.Console.WriteLine()
        let (var_8: bool) =
            if (var_4 = var_0) then
                (var_6 = var_1)
            else
                false
        if var_8 then
            System.Console.Write("I've found Mario.")
            System.Console.WriteLine()
            let (var_9: int64) = (var_6 + 1L)
            method_16((var_0: int64), (var_1: int64), (var_2: int64), (var_3: int64), (var_4: int64), (var_5: int64), (var_9: int64))
        else
            let (var_10: bool) =
                if (var_4 = var_2) then
                    (var_6 = var_3)
                else
                    false
            if var_10 then
                System.Console.Write("I've found Princess.")
                System.Console.WriteLine()
                System.Console.Write("Success.")
                System.Console.WriteLine()
            else
                let (var_11: int64) = (var_6 + 1L)
                method_16((var_0: int64), (var_1: int64), (var_2: int64), (var_3: int64), (var_4: int64), (var_5: int64), (var_11: int64))
    else
        ()
and method_17((var_0: int64), (var_1: int64), (var_2: int64), (var_3: int64), (var_4: int64), (var_5: int64)): unit =
    System.Console.Write("Failure.")
    System.Console.WriteLine()
    if (var_5 < var_2) then
        let (var_6: int64) = 0L
        method_16((var_0: int64), (var_1: int64), (var_3: int64), (var_4: int64), (var_5: int64), (var_2: int64), (var_6: int64))
    else
        ()
and method_18((var_0: int64), (var_1: int64), (var_2: int64), (var_3: int64), (var_4: int64), (var_5: int64), (var_6: int64)): unit =
    let (var_7: int64) = (var_4 + 1L)
    method_19((var_0: int64), (var_1: int64), (var_5: int64), (var_2: int64), (var_3: int64), (var_7: int64))
    if (var_6 < var_5) then
        System.Console.Write("I am at (")
        System.Console.Write(var_4)
        System.Console.Write(",")
        System.Console.Write(var_6)
        System.Console.Write(")")
        System.Console.WriteLine()
        let (var_8: bool) =
            if (var_4 = var_0) then
                (var_6 = var_1)
            else
                false
        if var_8 then
            System.Console.Write("I've found Mario.")
            System.Console.WriteLine()
            System.Console.Write("Success.")
            System.Console.WriteLine()
        else
            let (var_9: bool) =
                if (var_4 = var_2) then
                    (var_6 = var_3)
                else
                    false
            if var_9 then
                System.Console.Write("I've found Princess.")
                System.Console.WriteLine()
                let (var_10: int64) = (var_6 + 1L)
                method_18((var_0: int64), (var_1: int64), (var_2: int64), (var_3: int64), (var_4: int64), (var_5: int64), (var_10: int64))
            else
                let (var_11: int64) = (var_6 + 1L)
                method_18((var_0: int64), (var_1: int64), (var_2: int64), (var_3: int64), (var_4: int64), (var_5: int64), (var_11: int64))
    else
        ()
and method_19((var_0: int64), (var_1: int64), (var_2: int64), (var_3: int64), (var_4: int64), (var_5: int64)): unit =
    System.Console.Write("Failure.")
    System.Console.WriteLine()
    if (var_5 < var_2) then
        let (var_6: int64) = 0L
        method_18((var_0: int64), (var_1: int64), (var_3: int64), (var_4: int64), (var_5: int64), (var_2: int64), (var_6: int64))
    else
        ()
let (var_0: int64) = 0L
let (var_1: int64) = 0L
let (var_2: int64) = 1L
let (var_3: int64) = 1L
let (var_4: int64) = 5L
let (var_5: int64) = 0L
method_14((var_2: int64), (var_3: int64), (var_4: int64), (var_0: int64), (var_1: int64), (var_5: int64))

