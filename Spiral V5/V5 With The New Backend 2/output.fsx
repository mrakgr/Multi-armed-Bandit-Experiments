let rec method_14((var_0: int64), (var_1: int64)): int64 =
    if (var_0 <= var_1) then
        let (var_2: int64) = (var_0 % 4L)
        let (var_25: bool) =
            if (var_2 = 0L) then
                let (var_3: int64) = (var_0 % 6L)
                if (var_3 = 0L) then
                    let (var_4: int64) = (var_0 % 7L)
                    if (var_4 = 0L) then
                        let (var_5: int64) = (var_0 % 8L)
                        if (var_5 = 0L) then
                            let (var_6: int64) = (var_0 % 9L)
                            if (var_6 = 0L) then
                                let (var_7: int64) = (var_0 % 10L)
                                if (var_7 = 0L) then
                                    let (var_8: int64) = (var_0 % 12L)
                                    if (var_8 = 0L) then
                                        let (var_9: int64) = (var_0 % 14L)
                                        if (var_9 = 0L) then
                                            let (var_10: int64) = (var_0 % 15L)
                                            if (var_10 = 0L) then
                                                let (var_11: int64) = (var_0 % 16L)
                                                if (var_11 = 0L) then
                                                    let (var_12: int64) = (var_0 % 18L)
                                                    if (var_12 = 0L) then
                                                        let (var_13: int64) = (var_0 % 20L)
                                                        if (var_13 = 0L) then
                                                            true
                                                        else
                                                            false
                                                    else
                                                        false
                                                else
                                                    false
                                            else
                                                false
                                        else
                                            false
                                    else
                                        false
                                else
                                    false
                            else
                                false
                        else
                            false
                    else
                        false
                else
                    false
            else
                false
        if var_25 then
            var_0
        else
            let (var_26: int64) = (var_0 + 1385670L)
            method_14((var_26: int64), (var_1: int64))
    else
        -1L
let (var_0: int64) = System.Int64.MaxValue
let (var_1: int64) = 1385670L
let (var_2: int64) = method_14((var_1: int64), (var_0: int64))
System.Console.WriteLine(var_2)

