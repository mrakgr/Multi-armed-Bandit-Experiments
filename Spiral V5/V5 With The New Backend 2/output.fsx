let rec method_14((var_0: int64), (var_1: int64), (var_2: int64), (var_3: int64), (var_4: int64)): unit =
    if (var_4 < 5L) then
        let (var_5: int64) = 0L
        method_15((var_0: int64), (var_1: int64), (var_2: int64), (var_3: int64), (var_4: int64), (var_5: int64))
    else
        (failwith "Failure.")
and method_15((var_0: int64), (var_1: int64), (var_2: int64), (var_3: int64), (var_4: int64), (var_5: int64)): unit =
    if (var_5 < 5L) then
        let (var_6: bool) =
            if (var_4 = var_0) then
                (var_5 = var_1)
            else
                false
        if var_6 then
            let (var_7: int64) = (var_5 + 1L)
            method_16((var_0: int64), (var_1: int64), (var_2: int64), (var_3: int64), (var_4: int64), (var_7: int64))
        else
            let (var_8: bool) =
                if (var_4 = var_2) then
                    (var_5 = var_3)
                else
                    false
            if var_8 then
                let (var_9: int64) = (var_5 + 1L)
                method_19((var_0: int64), (var_1: int64), (var_2: int64), (var_3: int64), (var_4: int64), (var_9: int64))
            else
                let (var_10: int64) = (var_5 + 1L)
                method_15((var_0: int64), (var_1: int64), (var_2: int64), (var_3: int64), (var_4: int64), (var_10: int64))
    else
        let (var_11: int64) = (var_4 + 1L)
        method_14((var_0: int64), (var_1: int64), (var_2: int64), (var_3: int64), (var_11: int64))
and method_16((var_0: int64), (var_1: int64), (var_2: int64), (var_3: int64), (var_4: int64), (var_5: int64)): unit =
    if (var_5 < 5L) then
        let (var_6: bool) =
            if (var_4 = var_0) then
                (var_5 = var_1)
            else
                false
        if var_6 then
            let (var_7: int64) = (var_5 + 1L)
            method_16((var_0: int64), (var_1: int64), (var_2: int64), (var_3: int64), (var_4: int64), (var_7: int64))
        else
            let (var_8: bool) =
                if (var_4 = var_2) then
                    (var_5 = var_3)
                else
                    false
            if var_8 then
                System.Console.Write("Success.")
                System.Console.WriteLine()
            else
                let (var_9: int64) = (var_5 + 1L)
                method_16((var_0: int64), (var_1: int64), (var_2: int64), (var_3: int64), (var_4: int64), (var_9: int64))
    else
        let (var_10: int64) = (var_4 + 1L)
        method_17((var_0: int64), (var_1: int64), (var_2: int64), (var_3: int64), (var_10: int64))
and method_17((var_0: int64), (var_1: int64), (var_2: int64), (var_3: int64), (var_4: int64)): unit =
    if (var_4 < 5L) then
        let (var_5: int64) = 0L
        method_18((var_0: int64), (var_1: int64), (var_2: int64), (var_3: int64), (var_4: int64), (var_5: int64))
    else
        (failwith "Failure.")
and method_18((var_0: int64), (var_1: int64), (var_2: int64), (var_3: int64), (var_4: int64), (var_5: int64)): unit =
    if (var_5 < 5L) then
        let (var_6: bool) =
            if (var_4 = var_0) then
                (var_5 = var_1)
            else
                false
        if var_6 then
            let (var_7: int64) = (var_5 + 1L)
            method_18((var_0: int64), (var_1: int64), (var_2: int64), (var_3: int64), (var_4: int64), (var_7: int64))
        else
            let (var_8: bool) =
                if (var_4 = var_2) then
                    (var_5 = var_3)
                else
                    false
            if var_8 then
                System.Console.Write("Success.")
                System.Console.WriteLine()
            else
                let (var_9: int64) = (var_5 + 1L)
                method_18((var_0: int64), (var_1: int64), (var_2: int64), (var_3: int64), (var_4: int64), (var_9: int64))
    else
        let (var_10: int64) = (var_4 + 1L)
        method_17((var_0: int64), (var_1: int64), (var_2: int64), (var_3: int64), (var_10: int64))
and method_19((var_0: int64), (var_1: int64), (var_2: int64), (var_3: int64), (var_4: int64), (var_5: int64)): unit =
    if (var_5 < 5L) then
        let (var_6: bool) =
            if (var_4 = var_0) then
                (var_5 = var_1)
            else
                false
        if var_6 then
            System.Console.Write("Success.")
            System.Console.WriteLine()
        else
            let (var_7: bool) =
                if (var_4 = var_2) then
                    (var_5 = var_3)
                else
                    false
            if var_7 then
                let (var_8: int64) = (var_5 + 1L)
                method_19((var_0: int64), (var_1: int64), (var_2: int64), (var_3: int64), (var_4: int64), (var_8: int64))
            else
                let (var_9: int64) = (var_5 + 1L)
                method_19((var_0: int64), (var_1: int64), (var_2: int64), (var_3: int64), (var_4: int64), (var_9: int64))
    else
        let (var_10: int64) = (var_4 + 1L)
        method_20((var_0: int64), (var_1: int64), (var_2: int64), (var_3: int64), (var_10: int64))
and method_20((var_0: int64), (var_1: int64), (var_2: int64), (var_3: int64), (var_4: int64)): unit =
    if (var_4 < 5L) then
        let (var_5: int64) = 0L
        method_21((var_0: int64), (var_1: int64), (var_2: int64), (var_3: int64), (var_4: int64), (var_5: int64))
    else
        (failwith "Failure.")
and method_21((var_0: int64), (var_1: int64), (var_2: int64), (var_3: int64), (var_4: int64), (var_5: int64)): unit =
    if (var_5 < 5L) then
        let (var_6: bool) =
            if (var_4 = var_0) then
                (var_5 = var_1)
            else
                false
        if var_6 then
            System.Console.Write("Success.")
            System.Console.WriteLine()
        else
            let (var_7: bool) =
                if (var_4 = var_2) then
                    (var_5 = var_3)
                else
                    false
            if var_7 then
                let (var_8: int64) = (var_5 + 1L)
                method_21((var_0: int64), (var_1: int64), (var_2: int64), (var_3: int64), (var_4: int64), (var_8: int64))
            else
                let (var_9: int64) = (var_5 + 1L)
                method_21((var_0: int64), (var_1: int64), (var_2: int64), (var_3: int64), (var_4: int64), (var_9: int64))
    else
        let (var_10: int64) = (var_4 + 1L)
        method_20((var_0: int64), (var_1: int64), (var_2: int64), (var_3: int64), (var_10: int64))
let (var_0: int64) = 0L
let (var_1: int64) = 0L
let (var_2: int64) = 1L
let (var_3: int64) = 1L
let (var_4: int64) = 0L
method_14((var_2: int64), (var_3: int64), (var_0: int64), (var_1: int64), (var_4: int64))

