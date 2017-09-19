let rec method_14((var_0: bool), (var_1: string), (var_2: int64)): unit =
    let (var_4: bool) =
        if (var_2 >= 0L) then
            let (var_3: int64) = (int64 var_1.Length)
            (var_2 < var_3)
        else
            false
    if var_4 then
        let (var_5: char) = var_1.[int32 var_2]
        let (var_6: bool) =
            if (var_5 >= '0') then
                (var_5 <= '9')
            else
                false
        let (var_7: int64) = (var_2 + 1L)
        if var_6 then
            let (var_8: int64) = System.Convert.ToInt64(var_5)
            let (var_9: int64) = System.Convert.ToInt64('0')
            let (var_10: int64) = (var_8 - var_9)
            let (var_11: int64) = (0L + var_10)
            method_15((var_0: bool), (var_1: string), (var_11: int64), (var_7: int64))
        else
            (failwith "pint64")
    else
        (failwith "pint64")
and method_15((var_0: bool), (var_1: string), (var_2: int64), (var_3: int64)): unit =
    let (var_5: bool) =
        if (var_3 >= 0L) then
            let (var_4: int64) = (int64 var_1.Length)
            (var_3 < var_4)
        else
            false
    if var_5 then
        let (var_6: char) = var_1.[int32 var_3]
        let (var_7: bool) =
            if (var_6 >= '0') then
                (var_6 <= '9')
            else
                false
        let (var_8: int64) = (var_3 + 1L)
        if var_7 then
            let (var_9: int64) = System.Convert.ToInt64(var_6)
            let (var_10: int64) = System.Convert.ToInt64('0')
            let (var_11: int64) = (var_9 - var_10)
            let (var_12: bool) =
                if (var_2 = 922337203685477580L) then
                    (var_11 <= 7L)
                else
                    false
            let (var_13: bool) =
                if var_12 then
                    true
                else
                    (var_2 < 922337203685477580L)
            if var_13 then
                let (var_14: int64) = (var_2 * 10L)
                let (var_15: int64) = (var_14 + var_11)
                method_15((var_0: bool), (var_1: string), (var_15: int64), (var_8: int64))
            else
                (failwith "integer overflow")
        else
            let (var_16: int64) =
                if var_0 then
                    var_2
                else
                    (-var_2)
            let (var_17: int64) = 0L
            method_16((var_16: int64), (var_1: string), (var_17: int64), (var_8: int64))
    else
        let (var_18: int64) =
            if var_0 then
                var_2
            else
                (-var_2)
        let (var_19: int64) = 0L
        method_16((var_18: int64), (var_1: string), (var_19: int64), (var_3: int64))
and method_16((var_0: int64), (var_1: string), (var_2: int64), (var_3: int64)): unit =
    let (var_4: int64) = (var_2 + 1L)
    let (var_6: bool) =
        if (var_3 >= 0L) then
            let (var_5: int64) = (int64 var_1.Length)
            (var_3 < var_5)
        else
            false
    if var_6 then
        let (var_7: char) = var_1.[int32 var_3]
        let (var_9: bool) =
            if (var_7 = ' ') then
                true
            else
                if (var_7 = '\n') then
                    true
                else
                    (var_7 = '\r')
        let (var_10: int64) = (var_3 + 1L)
        if var_9 then
            method_16((var_0: int64), (var_1: string), (var_4: int64), (var_10: int64))
        else
            let (var_12: bool) =
                if (var_10 >= 0L) then
                    let (var_11: int64) = (int64 var_1.Length)
                    (var_10 < var_11)
                else
                    false
            if var_12 then
                let (var_13: char) = var_1.[int32 var_10]
                let (var_14: bool) = ('-' = var_13)
                let (var_15: int64) = (var_10 + 1L)
                if var_14 then
                    let (var_16: bool) = false
                    method_17((var_16: bool), (var_0: int64), (var_1: string), (var_15: int64))
                else
                    let (var_17: bool) = true
                    method_17((var_17: bool), (var_0: int64), (var_1: string), (var_15: int64))
            else
                let (var_18: bool) = true
                method_17((var_18: bool), (var_0: int64), (var_1: string), (var_10: int64))
    else
        let (var_20: bool) =
            if (var_3 >= 0L) then
                let (var_19: int64) = (int64 var_1.Length)
                (var_3 < var_19)
            else
                false
        if var_20 then
            let (var_21: char) = var_1.[int32 var_3]
            let (var_22: bool) = ('-' = var_21)
            let (var_23: int64) = (var_3 + 1L)
            if var_22 then
                let (var_24: bool) = false
                method_17((var_24: bool), (var_0: int64), (var_1: string), (var_23: int64))
            else
                let (var_25: bool) = true
                method_17((var_25: bool), (var_0: int64), (var_1: string), (var_23: int64))
        else
            let (var_26: bool) = true
            method_17((var_26: bool), (var_0: int64), (var_1: string), (var_3: int64))
and method_17((var_0: bool), (var_1: int64), (var_2: string), (var_3: int64)): unit =
    let (var_5: bool) =
        if (var_3 >= 0L) then
            let (var_4: int64) = (int64 var_2.Length)
            (var_3 < var_4)
        else
            false
    if var_5 then
        let (var_6: char) = var_2.[int32 var_3]
        let (var_7: bool) =
            if (var_6 >= '0') then
                (var_6 <= '9')
            else
                false
        let (var_8: int64) = (var_3 + 1L)
        if var_7 then
            let (var_9: int64) = System.Convert.ToInt64(var_6)
            let (var_10: int64) = System.Convert.ToInt64('0')
            let (var_11: int64) = (var_9 - var_10)
            let (var_12: int64) = (0L + var_11)
            method_18((var_0: bool), (var_1: int64), (var_2: string), (var_12: int64), (var_8: int64))
        else
            (failwith "pint64")
    else
        (failwith "pint64")
and method_18((var_0: bool), (var_1: int64), (var_2: string), (var_3: int64), (var_4: int64)): unit =
    let (var_6: bool) =
        if (var_4 >= 0L) then
            let (var_5: int64) = (int64 var_2.Length)
            (var_4 < var_5)
        else
            false
    if var_6 then
        let (var_7: char) = var_2.[int32 var_4]
        let (var_8: bool) =
            if (var_7 >= '0') then
                (var_7 <= '9')
            else
                false
        let (var_9: int64) = (var_4 + 1L)
        if var_8 then
            let (var_10: int64) = System.Convert.ToInt64(var_7)
            let (var_11: int64) = System.Convert.ToInt64('0')
            let (var_12: int64) = (var_10 - var_11)
            let (var_13: bool) =
                if (var_3 = 922337203685477580L) then
                    (var_12 <= 7L)
                else
                    false
            let (var_14: bool) =
                if var_13 then
                    true
                else
                    (var_3 < 922337203685477580L)
            if var_14 then
                let (var_15: int64) = (var_3 * 10L)
                let (var_16: int64) = (var_15 + var_12)
                method_18((var_0: bool), (var_1: int64), (var_2: string), (var_16: int64), (var_9: int64))
            else
                (failwith "integer overflow")
        else
            let (var_17: int64) =
                if var_0 then
                    var_3
                else
                    (-var_3)
            let (var_18: int64) = 0L
            method_19((var_17: int64), (var_1: int64), (var_2: string), (var_18: int64), (var_9: int64))
    else
        let (var_19: int64) =
            if var_0 then
                var_3
            else
                (-var_3)
        let (var_20: int64) = 0L
        method_19((var_19: int64), (var_1: int64), (var_2: string), (var_20: int64), (var_4: int64))
and method_19((var_0: int64), (var_1: int64), (var_2: string), (var_3: int64), (var_4: int64)): unit =
    let (var_5: int64) = (var_3 + 1L)
    let (var_7: bool) =
        if (var_4 >= 0L) then
            let (var_6: int64) = (int64 var_2.Length)
            (var_4 < var_6)
        else
            false
    if var_7 then
        let (var_8: char) = var_2.[int32 var_4]
        let (var_10: bool) =
            if (var_8 = ' ') then
                true
            else
                if (var_8 = '\n') then
                    true
                else
                    (var_8 = '\r')
        let (var_11: int64) = (var_4 + 1L)
        if var_10 then
            method_19((var_0: int64), (var_1: int64), (var_2: string), (var_5: int64), (var_11: int64))
        else
            let (var_13: bool) =
                if (var_11 >= 0L) then
                    let (var_12: int64) = (int64 var_2.Length)
                    (var_11 < var_12)
                else
                    false
            if var_13 then
                let (var_14: char) = var_2.[int32 var_11]
                let (var_15: bool) = ('-' = var_14)
                let (var_16: int64) = (var_11 + 1L)
                if var_15 then
                    let (var_17: bool) = false
                    method_20((var_17: bool), (var_0: int64), (var_1: int64), (var_2: string), (var_16: int64))
                else
                    let (var_18: bool) = true
                    method_20((var_18: bool), (var_0: int64), (var_1: int64), (var_2: string), (var_16: int64))
            else
                let (var_19: bool) = true
                method_20((var_19: bool), (var_0: int64), (var_1: int64), (var_2: string), (var_11: int64))
    else
        let (var_21: bool) =
            if (var_4 >= 0L) then
                let (var_20: int64) = (int64 var_2.Length)
                (var_4 < var_20)
            else
                false
        if var_21 then
            let (var_22: char) = var_2.[int32 var_4]
            let (var_23: bool) = ('-' = var_22)
            let (var_24: int64) = (var_4 + 1L)
            if var_23 then
                let (var_25: bool) = false
                method_20((var_25: bool), (var_0: int64), (var_1: int64), (var_2: string), (var_24: int64))
            else
                let (var_26: bool) = true
                method_20((var_26: bool), (var_0: int64), (var_1: int64), (var_2: string), (var_24: int64))
        else
            let (var_27: bool) = true
            method_20((var_27: bool), (var_0: int64), (var_1: int64), (var_2: string), (var_4: int64))
and method_20((var_0: bool), (var_1: int64), (var_2: int64), (var_3: string), (var_4: int64)): unit =
    let (var_6: bool) =
        if (var_4 >= 0L) then
            let (var_5: int64) = (int64 var_3.Length)
            (var_4 < var_5)
        else
            false
    if var_6 then
        let (var_7: char) = var_3.[int32 var_4]
        let (var_8: bool) =
            if (var_7 >= '0') then
                (var_7 <= '9')
            else
                false
        let (var_9: int64) = (var_4 + 1L)
        if var_8 then
            let (var_10: int64) = System.Convert.ToInt64(var_7)
            let (var_11: int64) = System.Convert.ToInt64('0')
            let (var_12: int64) = (var_10 - var_11)
            let (var_13: int64) = (0L + var_12)
            method_21((var_0: bool), (var_1: int64), (var_2: int64), (var_3: string), (var_13: int64), (var_9: int64))
        else
            (failwith "pint64")
    else
        (failwith "pint64")
and method_21((var_0: bool), (var_1: int64), (var_2: int64), (var_3: string), (var_4: int64), (var_5: int64)): unit =
    let (var_7: bool) =
        if (var_5 >= 0L) then
            let (var_6: int64) = (int64 var_3.Length)
            (var_5 < var_6)
        else
            false
    if var_7 then
        let (var_8: char) = var_3.[int32 var_5]
        let (var_9: bool) =
            if (var_8 >= '0') then
                (var_8 <= '9')
            else
                false
        let (var_10: int64) = (var_5 + 1L)
        if var_9 then
            let (var_11: int64) = System.Convert.ToInt64(var_8)
            let (var_12: int64) = System.Convert.ToInt64('0')
            let (var_13: int64) = (var_11 - var_12)
            let (var_14: bool) =
                if (var_4 = 922337203685477580L) then
                    (var_13 <= 7L)
                else
                    false
            let (var_15: bool) =
                if var_14 then
                    true
                else
                    (var_4 < 922337203685477580L)
            if var_15 then
                let (var_16: int64) = (var_4 * 10L)
                let (var_17: int64) = (var_16 + var_13)
                method_21((var_0: bool), (var_1: int64), (var_2: int64), (var_3: string), (var_17: int64), (var_10: int64))
            else
                (failwith "integer overflow")
        else
            let (var_18: int64) =
                if var_0 then
                    var_4
                else
                    (-var_4)
            let (var_19: int64) = 0L
            method_22((var_18: int64), (var_1: int64), (var_2: int64), (var_3: string), (var_19: int64), (var_10: int64))
    else
        let (var_20: int64) =
            if var_0 then
                var_4
            else
                (-var_4)
        let (var_21: int64) = 0L
        method_22((var_20: int64), (var_1: int64), (var_2: int64), (var_3: string), (var_21: int64), (var_5: int64))
and method_22((var_0: int64), (var_1: int64), (var_2: int64), (var_3: string), (var_4: int64), (var_5: int64)): unit =
    let (var_6: int64) = (var_4 + 1L)
    let (var_8: bool) =
        if (var_5 >= 0L) then
            let (var_7: int64) = (int64 var_3.Length)
            (var_5 < var_7)
        else
            false
    if var_8 then
        let (var_9: char) = var_3.[int32 var_5]
        let (var_11: bool) =
            if (var_9 = ' ') then
                true
            else
                if (var_9 = '\n') then
                    true
                else
                    (var_9 = '\r')
        let (var_12: int64) = (var_5 + 1L)
        if var_11 then
            method_22((var_0: int64), (var_1: int64), (var_2: int64), (var_3: string), (var_6: int64), (var_12: int64))
        else
            let (var_14: bool) =
                if (var_12 >= 0L) then
                    let (var_13: int64) = (int64 var_3.Length)
                    (var_12 < var_13)
                else
                    false
            if var_14 then
                let (var_15: char) = var_3.[int32 var_12]
                let (var_16: bool) = ('-' = var_15)
                let (var_17: int64) = (var_12 + 1L)
                if var_16 then
                    let (var_18: bool) = false
                    method_23((var_18: bool), (var_0: int64), (var_1: int64), (var_2: int64), (var_3: string), (var_17: int64))
                else
                    let (var_19: bool) = true
                    method_23((var_19: bool), (var_0: int64), (var_1: int64), (var_2: int64), (var_3: string), (var_17: int64))
            else
                let (var_20: bool) = true
                method_23((var_20: bool), (var_0: int64), (var_1: int64), (var_2: int64), (var_3: string), (var_12: int64))
    else
        let (var_22: bool) =
            if (var_5 >= 0L) then
                let (var_21: int64) = (int64 var_3.Length)
                (var_5 < var_21)
            else
                false
        if var_22 then
            let (var_23: char) = var_3.[int32 var_5]
            let (var_24: bool) = ('-' = var_23)
            let (var_25: int64) = (var_5 + 1L)
            if var_24 then
                let (var_26: bool) = false
                method_23((var_26: bool), (var_0: int64), (var_1: int64), (var_2: int64), (var_3: string), (var_25: int64))
            else
                let (var_27: bool) = true
                method_23((var_27: bool), (var_0: int64), (var_1: int64), (var_2: int64), (var_3: string), (var_25: int64))
        else
            let (var_28: bool) = true
            method_23((var_28: bool), (var_0: int64), (var_1: int64), (var_2: int64), (var_3: string), (var_5: int64))
and method_23((var_0: bool), (var_1: int64), (var_2: int64), (var_3: int64), (var_4: string), (var_5: int64)): unit =
    let (var_7: bool) =
        if (var_5 >= 0L) then
            let (var_6: int64) = (int64 var_4.Length)
            (var_5 < var_6)
        else
            false
    if var_7 then
        let (var_8: char) = var_4.[int32 var_5]
        let (var_9: bool) =
            if (var_8 >= '0') then
                (var_8 <= '9')
            else
                false
        let (var_10: int64) = (var_5 + 1L)
        if var_9 then
            let (var_11: int64) = System.Convert.ToInt64(var_8)
            let (var_12: int64) = System.Convert.ToInt64('0')
            let (var_13: int64) = (var_11 - var_12)
            let (var_14: int64) = (0L + var_13)
            method_24((var_0: bool), (var_1: int64), (var_2: int64), (var_3: int64), (var_4: string), (var_14: int64), (var_10: int64))
        else
            (failwith "pint64")
    else
        (failwith "pint64")
and method_24((var_0: bool), (var_1: int64), (var_2: int64), (var_3: int64), (var_4: string), (var_5: int64), (var_6: int64)): unit =
    let (var_8: bool) =
        if (var_6 >= 0L) then
            let (var_7: int64) = (int64 var_4.Length)
            (var_6 < var_7)
        else
            false
    if var_8 then
        let (var_9: char) = var_4.[int32 var_6]
        let (var_10: bool) =
            if (var_9 >= '0') then
                (var_9 <= '9')
            else
                false
        let (var_11: int64) = (var_6 + 1L)
        if var_10 then
            let (var_12: int64) = System.Convert.ToInt64(var_9)
            let (var_13: int64) = System.Convert.ToInt64('0')
            let (var_14: int64) = (var_12 - var_13)
            let (var_15: bool) =
                if (var_5 = 922337203685477580L) then
                    (var_14 <= 7L)
                else
                    false
            let (var_16: bool) =
                if var_15 then
                    true
                else
                    (var_5 < 922337203685477580L)
            if var_16 then
                let (var_17: int64) = (var_5 * 10L)
                let (var_18: int64) = (var_17 + var_14)
                method_24((var_0: bool), (var_1: int64), (var_2: int64), (var_3: int64), (var_4: string), (var_18: int64), (var_11: int64))
            else
                (failwith "integer overflow")
        else
            let (var_19: int64) =
                if var_0 then
                    var_5
                else
                    (-var_5)
            let (var_20: int64) = 0L
            method_25((var_19: int64), (var_1: int64), (var_2: int64), (var_3: int64), (var_4: string), (var_20: int64), (var_11: int64))
    else
        let (var_21: int64) =
            if var_0 then
                var_5
            else
                (-var_5)
        let (var_22: int64) = 0L
        method_25((var_21: int64), (var_1: int64), (var_2: int64), (var_3: int64), (var_4: string), (var_22: int64), (var_6: int64))
and method_25((var_0: int64), (var_1: int64), (var_2: int64), (var_3: int64), (var_4: string), (var_5: int64), (var_6: int64)): unit =
    let (var_7: int64) = (var_5 + 1L)
    let (var_9: bool) =
        if (var_6 >= 0L) then
            let (var_8: int64) = (int64 var_4.Length)
            (var_6 < var_8)
        else
            false
    if var_9 then
        let (var_10: char) = var_4.[int32 var_6]
        let (var_12: bool) =
            if (var_10 = ' ') then
                true
            else
                if (var_10 = '\n') then
                    true
                else
                    (var_10 = '\r')
        let (var_13: int64) = (var_6 + 1L)
        if var_12 then
            method_25((var_0: int64), (var_1: int64), (var_2: int64), (var_3: int64), (var_4: string), (var_7: int64), (var_13: int64))
        else
            let (var_15: bool) =
                if (var_13 >= 0L) then
                    let (var_14: int64) = (int64 var_4.Length)
                    (var_13 < var_14)
                else
                    false
            if var_15 then
                let (var_16: char) = var_4.[int32 var_13]
                let (var_17: bool) = ('-' = var_16)
                let (var_18: int64) = (var_13 + 1L)
                if var_17 then
                    let (var_19: bool) = false
                    method_26((var_19: bool), (var_0: int64), (var_1: int64), (var_2: int64), (var_3: int64), (var_4: string), (var_18: int64))
                else
                    let (var_20: bool) = true
                    method_26((var_20: bool), (var_0: int64), (var_1: int64), (var_2: int64), (var_3: int64), (var_4: string), (var_18: int64))
            else
                let (var_21: bool) = true
                method_26((var_21: bool), (var_0: int64), (var_1: int64), (var_2: int64), (var_3: int64), (var_4: string), (var_13: int64))
    else
        let (var_23: bool) =
            if (var_6 >= 0L) then
                let (var_22: int64) = (int64 var_4.Length)
                (var_6 < var_22)
            else
                false
        if var_23 then
            let (var_24: char) = var_4.[int32 var_6]
            let (var_25: bool) = ('-' = var_24)
            let (var_26: int64) = (var_6 + 1L)
            if var_25 then
                let (var_27: bool) = false
                method_26((var_27: bool), (var_0: int64), (var_1: int64), (var_2: int64), (var_3: int64), (var_4: string), (var_26: int64))
            else
                let (var_28: bool) = true
                method_26((var_28: bool), (var_0: int64), (var_1: int64), (var_2: int64), (var_3: int64), (var_4: string), (var_26: int64))
        else
            let (var_29: bool) = true
            method_26((var_29: bool), (var_0: int64), (var_1: int64), (var_2: int64), (var_3: int64), (var_4: string), (var_6: int64))
and method_26((var_0: bool), (var_1: int64), (var_2: int64), (var_3: int64), (var_4: int64), (var_5: string), (var_6: int64)): unit =
    let (var_8: bool) =
        if (var_6 >= 0L) then
            let (var_7: int64) = (int64 var_5.Length)
            (var_6 < var_7)
        else
            false
    if var_8 then
        let (var_9: char) = var_5.[int32 var_6]
        let (var_10: bool) =
            if (var_9 >= '0') then
                (var_9 <= '9')
            else
                false
        let (var_11: int64) = (var_6 + 1L)
        if var_10 then
            let (var_12: int64) = System.Convert.ToInt64(var_9)
            let (var_13: int64) = System.Convert.ToInt64('0')
            let (var_14: int64) = (var_12 - var_13)
            let (var_15: int64) = (0L + var_14)
            method_27((var_0: bool), (var_1: int64), (var_2: int64), (var_3: int64), (var_4: int64), (var_5: string), (var_15: int64), (var_11: int64))
        else
            (failwith "pint64")
    else
        (failwith "pint64")
and method_27((var_0: bool), (var_1: int64), (var_2: int64), (var_3: int64), (var_4: int64), (var_5: string), (var_6: int64), (var_7: int64)): unit =
    let (var_9: bool) =
        if (var_7 >= 0L) then
            let (var_8: int64) = (int64 var_5.Length)
            (var_7 < var_8)
        else
            false
    if var_9 then
        let (var_10: char) = var_5.[int32 var_7]
        let (var_11: bool) =
            if (var_10 >= '0') then
                (var_10 <= '9')
            else
                false
        let (var_12: int64) = (var_7 + 1L)
        if var_11 then
            let (var_13: int64) = System.Convert.ToInt64(var_10)
            let (var_14: int64) = System.Convert.ToInt64('0')
            let (var_15: int64) = (var_13 - var_14)
            let (var_16: bool) =
                if (var_6 = 922337203685477580L) then
                    (var_15 <= 7L)
                else
                    false
            let (var_17: bool) =
                if var_16 then
                    true
                else
                    (var_6 < 922337203685477580L)
            if var_17 then
                let (var_18: int64) = (var_6 * 10L)
                let (var_19: int64) = (var_18 + var_15)
                method_27((var_0: bool), (var_1: int64), (var_2: int64), (var_3: int64), (var_4: int64), (var_5: string), (var_19: int64), (var_12: int64))
            else
                (failwith "integer overflow")
        else
            let (var_20: int64) =
                if var_0 then
                    var_6
                else
                    (-var_6)
            let (var_21: int64) = 0L
            method_28((var_20: int64), (var_1: int64), (var_2: int64), (var_3: int64), (var_4: int64), (var_5: string), (var_21: int64), (var_12: int64))
    else
        let (var_22: int64) =
            if var_0 then
                var_6
            else
                (-var_6)
        let (var_23: int64) = 0L
        method_28((var_22: int64), (var_1: int64), (var_2: int64), (var_3: int64), (var_4: int64), (var_5: string), (var_23: int64), (var_7: int64))
and method_28((var_0: int64), (var_1: int64), (var_2: int64), (var_3: int64), (var_4: int64), (var_5: string), (var_6: int64), (var_7: int64)): unit =
    let (var_8: int64) = (var_6 + 1L)
    let (var_10: bool) =
        if (var_7 >= 0L) then
            let (var_9: int64) = (int64 var_5.Length)
            (var_7 < var_9)
        else
            false
    if var_10 then
        let (var_11: char) = var_5.[int32 var_7]
        let (var_13: bool) =
            if (var_11 = ' ') then
                true
            else
                if (var_11 = '\n') then
                    true
                else
                    (var_11 = '\r')
        let (var_14: int64) = (var_7 + 1L)
        if var_13 then
            method_28((var_0: int64), (var_1: int64), (var_2: int64), (var_3: int64), (var_4: int64), (var_5: string), (var_8: int64), (var_14: int64))
        else
            let (var_16: bool) =
                if (var_14 >= 0L) then
                    let (var_15: int64) = (int64 var_5.Length)
                    (var_14 < var_15)
                else
                    false
            if var_16 then
                let (var_17: char) = var_5.[int32 var_14]
                let (var_18: bool) = ('-' = var_17)
                let (var_19: int64) = (var_14 + 1L)
                if var_18 then
                    let (var_20: bool) = false
                    method_29((var_20: bool), (var_0: int64), (var_1: int64), (var_2: int64), (var_3: int64), (var_4: int64), (var_5: string), (var_19: int64))
                else
                    let (var_21: bool) = true
                    method_29((var_21: bool), (var_0: int64), (var_1: int64), (var_2: int64), (var_3: int64), (var_4: int64), (var_5: string), (var_19: int64))
            else
                let (var_22: bool) = true
                method_29((var_22: bool), (var_0: int64), (var_1: int64), (var_2: int64), (var_3: int64), (var_4: int64), (var_5: string), (var_14: int64))
    else
        let (var_24: bool) =
            if (var_7 >= 0L) then
                let (var_23: int64) = (int64 var_5.Length)
                (var_7 < var_23)
            else
                false
        if var_24 then
            let (var_25: char) = var_5.[int32 var_7]
            let (var_26: bool) = ('-' = var_25)
            let (var_27: int64) = (var_7 + 1L)
            if var_26 then
                let (var_28: bool) = false
                method_29((var_28: bool), (var_0: int64), (var_1: int64), (var_2: int64), (var_3: int64), (var_4: int64), (var_5: string), (var_27: int64))
            else
                let (var_29: bool) = true
                method_29((var_29: bool), (var_0: int64), (var_1: int64), (var_2: int64), (var_3: int64), (var_4: int64), (var_5: string), (var_27: int64))
        else
            let (var_30: bool) = true
            method_29((var_30: bool), (var_0: int64), (var_1: int64), (var_2: int64), (var_3: int64), (var_4: int64), (var_5: string), (var_7: int64))
and method_29((var_0: bool), (var_1: int64), (var_2: int64), (var_3: int64), (var_4: int64), (var_5: int64), (var_6: string), (var_7: int64)): unit =
    let (var_9: bool) =
        if (var_7 >= 0L) then
            let (var_8: int64) = (int64 var_6.Length)
            (var_7 < var_8)
        else
            false
    if var_9 then
        let (var_10: char) = var_6.[int32 var_7]
        let (var_11: bool) =
            if (var_10 >= '0') then
                (var_10 <= '9')
            else
                false
        let (var_12: int64) = (var_7 + 1L)
        if var_11 then
            let (var_13: int64) = System.Convert.ToInt64(var_10)
            let (var_14: int64) = System.Convert.ToInt64('0')
            let (var_15: int64) = (var_13 - var_14)
            let (var_16: int64) = (0L + var_15)
            method_30((var_0: bool), (var_1: int64), (var_2: int64), (var_3: int64), (var_4: int64), (var_5: int64), (var_6: string), (var_16: int64), (var_12: int64))
        else
            (failwith "pint64")
    else
        (failwith "pint64")
and method_30((var_0: bool), (var_1: int64), (var_2: int64), (var_3: int64), (var_4: int64), (var_5: int64), (var_6: string), (var_7: int64), (var_8: int64)): unit =
    let (var_10: bool) =
        if (var_8 >= 0L) then
            let (var_9: int64) = (int64 var_6.Length)
            (var_8 < var_9)
        else
            false
    if var_10 then
        let (var_11: char) = var_6.[int32 var_8]
        let (var_12: bool) =
            if (var_11 >= '0') then
                (var_11 <= '9')
            else
                false
        let (var_13: int64) = (var_8 + 1L)
        if var_12 then
            let (var_14: int64) = System.Convert.ToInt64(var_11)
            let (var_15: int64) = System.Convert.ToInt64('0')
            let (var_16: int64) = (var_14 - var_15)
            let (var_17: bool) =
                if (var_7 = 922337203685477580L) then
                    (var_16 <= 7L)
                else
                    false
            let (var_18: bool) =
                if var_17 then
                    true
                else
                    (var_7 < 922337203685477580L)
            if var_18 then
                let (var_19: int64) = (var_7 * 10L)
                let (var_20: int64) = (var_19 + var_16)
                method_30((var_0: bool), (var_1: int64), (var_2: int64), (var_3: int64), (var_4: int64), (var_5: int64), (var_6: string), (var_20: int64), (var_13: int64))
            else
                (failwith "integer overflow")
        else
            let (var_21: int64) =
                if var_0 then
                    var_7
                else
                    (-var_7)
            let (var_22: int64) = 0L
            method_31((var_21: int64), (var_1: int64), (var_2: int64), (var_3: int64), (var_4: int64), (var_5: int64), (var_6: string), (var_22: int64), (var_13: int64))
    else
        let (var_23: int64) =
            if var_0 then
                var_7
            else
                (-var_7)
        let (var_24: int64) = 0L
        method_31((var_23: int64), (var_1: int64), (var_2: int64), (var_3: int64), (var_4: int64), (var_5: int64), (var_6: string), (var_24: int64), (var_8: int64))
and method_31((var_0: int64), (var_1: int64), (var_2: int64), (var_3: int64), (var_4: int64), (var_5: int64), (var_6: string), (var_7: int64), (var_8: int64)): unit =
    let (var_9: int64) = (var_7 + 1L)
    let (var_11: bool) =
        if (var_8 >= 0L) then
            let (var_10: int64) = (int64 var_6.Length)
            (var_8 < var_10)
        else
            false
    if var_11 then
        let (var_12: char) = var_6.[int32 var_8]
        let (var_14: bool) =
            if (var_12 = ' ') then
                true
            else
                if (var_12 = '\n') then
                    true
                else
                    (var_12 = '\r')
        let (var_15: int64) = (var_8 + 1L)
        if var_14 then
            method_31((var_0: int64), (var_1: int64), (var_2: int64), (var_3: int64), (var_4: int64), (var_5: int64), (var_6: string), (var_9: int64), (var_15: int64))
        else
            let (var_17: bool) =
                if (var_15 >= 0L) then
                    let (var_16: int64) = (int64 var_6.Length)
                    (var_15 < var_16)
                else
                    false
            if var_17 then
                let (var_18: char) = var_6.[int32 var_15]
                let (var_19: bool) = ('-' = var_18)
                let (var_20: int64) = (var_15 + 1L)
                if var_19 then
                    let (var_21: bool) = false
                    method_32((var_21: bool), (var_0: int64), (var_1: int64), (var_2: int64), (var_3: int64), (var_4: int64), (var_5: int64), (var_6: string), (var_20: int64))
                else
                    let (var_22: bool) = true
                    method_32((var_22: bool), (var_0: int64), (var_1: int64), (var_2: int64), (var_3: int64), (var_4: int64), (var_5: int64), (var_6: string), (var_20: int64))
            else
                let (var_23: bool) = true
                method_32((var_23: bool), (var_0: int64), (var_1: int64), (var_2: int64), (var_3: int64), (var_4: int64), (var_5: int64), (var_6: string), (var_15: int64))
    else
        let (var_25: bool) =
            if (var_8 >= 0L) then
                let (var_24: int64) = (int64 var_6.Length)
                (var_8 < var_24)
            else
                false
        if var_25 then
            let (var_26: char) = var_6.[int32 var_8]
            let (var_27: bool) = ('-' = var_26)
            let (var_28: int64) = (var_8 + 1L)
            if var_27 then
                let (var_29: bool) = false
                method_32((var_29: bool), (var_0: int64), (var_1: int64), (var_2: int64), (var_3: int64), (var_4: int64), (var_5: int64), (var_6: string), (var_28: int64))
            else
                let (var_30: bool) = true
                method_32((var_30: bool), (var_0: int64), (var_1: int64), (var_2: int64), (var_3: int64), (var_4: int64), (var_5: int64), (var_6: string), (var_28: int64))
        else
            let (var_31: bool) = true
            method_32((var_31: bool), (var_0: int64), (var_1: int64), (var_2: int64), (var_3: int64), (var_4: int64), (var_5: int64), (var_6: string), (var_8: int64))
and method_32((var_0: bool), (var_1: int64), (var_2: int64), (var_3: int64), (var_4: int64), (var_5: int64), (var_6: int64), (var_7: string), (var_8: int64)): unit =
    let (var_10: bool) =
        if (var_8 >= 0L) then
            let (var_9: int64) = (int64 var_7.Length)
            (var_8 < var_9)
        else
            false
    if var_10 then
        let (var_11: char) = var_7.[int32 var_8]
        let (var_12: bool) =
            if (var_11 >= '0') then
                (var_11 <= '9')
            else
                false
        let (var_13: int64) = (var_8 + 1L)
        if var_12 then
            let (var_14: int64) = System.Convert.ToInt64(var_11)
            let (var_15: int64) = System.Convert.ToInt64('0')
            let (var_16: int64) = (var_14 - var_15)
            let (var_17: int64) = (0L + var_16)
            method_33((var_0: bool), (var_1: int64), (var_2: int64), (var_3: int64), (var_4: int64), (var_5: int64), (var_6: int64), (var_7: string), (var_17: int64), (var_13: int64))
        else
            (failwith "pint64")
    else
        (failwith "pint64")
and method_33((var_0: bool), (var_1: int64), (var_2: int64), (var_3: int64), (var_4: int64), (var_5: int64), (var_6: int64), (var_7: string), (var_8: int64), (var_9: int64)): unit =
    let (var_11: bool) =
        if (var_9 >= 0L) then
            let (var_10: int64) = (int64 var_7.Length)
            (var_9 < var_10)
        else
            false
    if var_11 then
        let (var_12: char) = var_7.[int32 var_9]
        let (var_13: bool) =
            if (var_12 >= '0') then
                (var_12 <= '9')
            else
                false
        let (var_14: int64) = (var_9 + 1L)
        if var_13 then
            let (var_15: int64) = System.Convert.ToInt64(var_12)
            let (var_16: int64) = System.Convert.ToInt64('0')
            let (var_17: int64) = (var_15 - var_16)
            let (var_18: bool) =
                if (var_8 = 922337203685477580L) then
                    (var_17 <= 7L)
                else
                    false
            let (var_19: bool) =
                if var_18 then
                    true
                else
                    (var_8 < 922337203685477580L)
            if var_19 then
                let (var_20: int64) = (var_8 * 10L)
                let (var_21: int64) = (var_20 + var_17)
                method_33((var_0: bool), (var_1: int64), (var_2: int64), (var_3: int64), (var_4: int64), (var_5: int64), (var_6: int64), (var_7: string), (var_21: int64), (var_14: int64))
            else
                (failwith "integer overflow")
        else
            let (var_22: int64) =
                if var_0 then
                    var_8
                else
                    (-var_8)
            let (var_23: int64) = 0L
            method_34((var_22: int64), (var_1: int64), (var_2: int64), (var_3: int64), (var_4: int64), (var_5: int64), (var_6: int64), (var_7: string), (var_23: int64), (var_14: int64))
    else
        let (var_24: int64) =
            if var_0 then
                var_8
            else
                (-var_8)
        let (var_25: int64) = 0L
        method_34((var_24: int64), (var_1: int64), (var_2: int64), (var_3: int64), (var_4: int64), (var_5: int64), (var_6: int64), (var_7: string), (var_25: int64), (var_9: int64))
and method_34((var_0: int64), (var_1: int64), (var_2: int64), (var_3: int64), (var_4: int64), (var_5: int64), (var_6: int64), (var_7: string), (var_8: int64), (var_9: int64)): unit =
    let (var_10: int64) = (var_8 + 1L)
    let (var_12: bool) =
        if (var_9 >= 0L) then
            let (var_11: int64) = (int64 var_7.Length)
            (var_9 < var_11)
        else
            false
    if var_12 then
        let (var_13: char) = var_7.[int32 var_9]
        let (var_15: bool) =
            if (var_13 = ' ') then
                true
            else
                if (var_13 = '\n') then
                    true
                else
                    (var_13 = '\r')
        let (var_16: int64) = (var_9 + 1L)
        if var_15 then
            method_34((var_0: int64), (var_1: int64), (var_2: int64), (var_3: int64), (var_4: int64), (var_5: int64), (var_6: int64), (var_7: string), (var_10: int64), (var_16: int64))
        else
            let (var_18: bool) =
                if (var_16 >= 0L) then
                    let (var_17: int64) = (int64 var_7.Length)
                    (var_16 < var_17)
                else
                    false
            if var_18 then
                let (var_19: char) = var_7.[int32 var_16]
                let (var_20: bool) = ('-' = var_19)
                let (var_21: int64) = (var_16 + 1L)
                if var_20 then
                    let (var_22: bool) = false
                    method_35((var_22: bool), (var_0: int64), (var_1: int64), (var_2: int64), (var_3: int64), (var_4: int64), (var_5: int64), (var_6: int64), (var_7: string), (var_21: int64))
                else
                    let (var_23: bool) = true
                    method_35((var_23: bool), (var_0: int64), (var_1: int64), (var_2: int64), (var_3: int64), (var_4: int64), (var_5: int64), (var_6: int64), (var_7: string), (var_21: int64))
            else
                let (var_24: bool) = true
                method_35((var_24: bool), (var_0: int64), (var_1: int64), (var_2: int64), (var_3: int64), (var_4: int64), (var_5: int64), (var_6: int64), (var_7: string), (var_16: int64))
    else
        let (var_26: bool) =
            if (var_9 >= 0L) then
                let (var_25: int64) = (int64 var_7.Length)
                (var_9 < var_25)
            else
                false
        if var_26 then
            let (var_27: char) = var_7.[int32 var_9]
            let (var_28: bool) = ('-' = var_27)
            let (var_29: int64) = (var_9 + 1L)
            if var_28 then
                let (var_30: bool) = false
                method_35((var_30: bool), (var_0: int64), (var_1: int64), (var_2: int64), (var_3: int64), (var_4: int64), (var_5: int64), (var_6: int64), (var_7: string), (var_29: int64))
            else
                let (var_31: bool) = true
                method_35((var_31: bool), (var_0: int64), (var_1: int64), (var_2: int64), (var_3: int64), (var_4: int64), (var_5: int64), (var_6: int64), (var_7: string), (var_29: int64))
        else
            let (var_32: bool) = true
            method_35((var_32: bool), (var_0: int64), (var_1: int64), (var_2: int64), (var_3: int64), (var_4: int64), (var_5: int64), (var_6: int64), (var_7: string), (var_9: int64))
and method_35((var_0: bool), (var_1: int64), (var_2: int64), (var_3: int64), (var_4: int64), (var_5: int64), (var_6: int64), (var_7: int64), (var_8: string), (var_9: int64)): unit =
    let (var_11: bool) =
        if (var_9 >= 0L) then
            let (var_10: int64) = (int64 var_8.Length)
            (var_9 < var_10)
        else
            false
    if var_11 then
        let (var_12: char) = var_8.[int32 var_9]
        let (var_13: bool) =
            if (var_12 >= '0') then
                (var_12 <= '9')
            else
                false
        let (var_14: int64) = (var_9 + 1L)
        if var_13 then
            let (var_15: int64) = System.Convert.ToInt64(var_12)
            let (var_16: int64) = System.Convert.ToInt64('0')
            let (var_17: int64) = (var_15 - var_16)
            let (var_18: int64) = (0L + var_17)
            method_36((var_0: bool), (var_1: int64), (var_2: int64), (var_3: int64), (var_4: int64), (var_5: int64), (var_6: int64), (var_7: int64), (var_8: string), (var_18: int64), (var_14: int64))
        else
            (failwith "pint64")
    else
        (failwith "pint64")
and method_36((var_0: bool), (var_1: int64), (var_2: int64), (var_3: int64), (var_4: int64), (var_5: int64), (var_6: int64), (var_7: int64), (var_8: string), (var_9: int64), (var_10: int64)): unit =
    let (var_12: bool) =
        if (var_10 >= 0L) then
            let (var_11: int64) = (int64 var_8.Length)
            (var_10 < var_11)
        else
            false
    if var_12 then
        let (var_13: char) = var_8.[int32 var_10]
        let (var_14: bool) =
            if (var_13 >= '0') then
                (var_13 <= '9')
            else
                false
        let (var_15: int64) = (var_10 + 1L)
        if var_14 then
            let (var_16: int64) = System.Convert.ToInt64(var_13)
            let (var_17: int64) = System.Convert.ToInt64('0')
            let (var_18: int64) = (var_16 - var_17)
            let (var_19: bool) =
                if (var_9 = 922337203685477580L) then
                    (var_18 <= 7L)
                else
                    false
            let (var_20: bool) =
                if var_19 then
                    true
                else
                    (var_9 < 922337203685477580L)
            if var_20 then
                let (var_21: int64) = (var_9 * 10L)
                let (var_22: int64) = (var_21 + var_18)
                method_36((var_0: bool), (var_1: int64), (var_2: int64), (var_3: int64), (var_4: int64), (var_5: int64), (var_6: int64), (var_7: int64), (var_8: string), (var_22: int64), (var_15: int64))
            else
                (failwith "integer overflow")
        else
            let (var_23: int64) =
                if var_0 then
                    var_9
                else
                    (-var_9)
            let (var_24: int64) = 0L
            method_37((var_23: int64), (var_1: int64), (var_2: int64), (var_3: int64), (var_4: int64), (var_5: int64), (var_6: int64), (var_7: int64), (var_8: string), (var_24: int64), (var_15: int64))
    else
        let (var_25: int64) =
            if var_0 then
                var_9
            else
                (-var_9)
        let (var_26: int64) = 0L
        method_37((var_25: int64), (var_1: int64), (var_2: int64), (var_3: int64), (var_4: int64), (var_5: int64), (var_6: int64), (var_7: int64), (var_8: string), (var_26: int64), (var_10: int64))
and method_37((var_0: int64), (var_1: int64), (var_2: int64), (var_3: int64), (var_4: int64), (var_5: int64), (var_6: int64), (var_7: int64), (var_8: string), (var_9: int64), (var_10: int64)): unit =
    let (var_11: int64) = (var_9 + 1L)
    let (var_13: bool) =
        if (var_10 >= 0L) then
            let (var_12: int64) = (int64 var_8.Length)
            (var_10 < var_12)
        else
            false
    if var_13 then
        let (var_14: char) = var_8.[int32 var_10]
        let (var_16: bool) =
            if (var_14 = ' ') then
                true
            else
                if (var_14 = '\n') then
                    true
                else
                    (var_14 = '\r')
        let (var_17: int64) = (var_10 + 1L)
        if var_16 then
            method_37((var_0: int64), (var_1: int64), (var_2: int64), (var_3: int64), (var_4: int64), (var_5: int64), (var_6: int64), (var_7: int64), (var_8: string), (var_11: int64), (var_17: int64))
        else
            let (var_19: bool) =
                if (var_17 >= 0L) then
                    let (var_18: int64) = (int64 var_8.Length)
                    (var_17 < var_18)
                else
                    false
            if var_19 then
                let (var_20: char) = var_8.[int32 var_17]
                let (var_21: bool) = ('-' = var_20)
                let (var_22: int64) = (var_17 + 1L)
                if var_21 then
                    let (var_23: bool) = false
                    method_38((var_23: bool), (var_0: int64), (var_1: int64), (var_2: int64), (var_3: int64), (var_4: int64), (var_5: int64), (var_6: int64), (var_7: int64), (var_8: string), (var_22: int64))
                else
                    let (var_24: bool) = true
                    method_38((var_24: bool), (var_0: int64), (var_1: int64), (var_2: int64), (var_3: int64), (var_4: int64), (var_5: int64), (var_6: int64), (var_7: int64), (var_8: string), (var_22: int64))
            else
                let (var_25: bool) = true
                method_38((var_25: bool), (var_0: int64), (var_1: int64), (var_2: int64), (var_3: int64), (var_4: int64), (var_5: int64), (var_6: int64), (var_7: int64), (var_8: string), (var_17: int64))
    else
        let (var_27: bool) =
            if (var_10 >= 0L) then
                let (var_26: int64) = (int64 var_8.Length)
                (var_10 < var_26)
            else
                false
        if var_27 then
            let (var_28: char) = var_8.[int32 var_10]
            let (var_29: bool) = ('-' = var_28)
            let (var_30: int64) = (var_10 + 1L)
            if var_29 then
                let (var_31: bool) = false
                method_38((var_31: bool), (var_0: int64), (var_1: int64), (var_2: int64), (var_3: int64), (var_4: int64), (var_5: int64), (var_6: int64), (var_7: int64), (var_8: string), (var_30: int64))
            else
                let (var_32: bool) = true
                method_38((var_32: bool), (var_0: int64), (var_1: int64), (var_2: int64), (var_3: int64), (var_4: int64), (var_5: int64), (var_6: int64), (var_7: int64), (var_8: string), (var_30: int64))
        else
            let (var_33: bool) = true
            method_38((var_33: bool), (var_0: int64), (var_1: int64), (var_2: int64), (var_3: int64), (var_4: int64), (var_5: int64), (var_6: int64), (var_7: int64), (var_8: string), (var_10: int64))
and method_38((var_0: bool), (var_1: int64), (var_2: int64), (var_3: int64), (var_4: int64), (var_5: int64), (var_6: int64), (var_7: int64), (var_8: int64), (var_9: string), (var_10: int64)): unit =
    let (var_12: bool) =
        if (var_10 >= 0L) then
            let (var_11: int64) = (int64 var_9.Length)
            (var_10 < var_11)
        else
            false
    if var_12 then
        let (var_13: char) = var_9.[int32 var_10]
        let (var_14: bool) =
            if (var_13 >= '0') then
                (var_13 <= '9')
            else
                false
        let (var_15: int64) = (var_10 + 1L)
        if var_14 then
            let (var_16: int64) = System.Convert.ToInt64(var_13)
            let (var_17: int64) = System.Convert.ToInt64('0')
            let (var_18: int64) = (var_16 - var_17)
            let (var_19: int64) = (0L + var_18)
            method_39((var_0: bool), (var_1: int64), (var_2: int64), (var_3: int64), (var_4: int64), (var_5: int64), (var_6: int64), (var_7: int64), (var_8: int64), (var_9: string), (var_19: int64), (var_15: int64))
        else
            (failwith "pint64")
    else
        (failwith "pint64")
and method_39((var_0: bool), (var_1: int64), (var_2: int64), (var_3: int64), (var_4: int64), (var_5: int64), (var_6: int64), (var_7: int64), (var_8: int64), (var_9: string), (var_10: int64), (var_11: int64)): unit =
    let (var_13: bool) =
        if (var_11 >= 0L) then
            let (var_12: int64) = (int64 var_9.Length)
            (var_11 < var_12)
        else
            false
    if var_13 then
        let (var_14: char) = var_9.[int32 var_11]
        let (var_15: bool) =
            if (var_14 >= '0') then
                (var_14 <= '9')
            else
                false
        let (var_16: int64) = (var_11 + 1L)
        if var_15 then
            let (var_17: int64) = System.Convert.ToInt64(var_14)
            let (var_18: int64) = System.Convert.ToInt64('0')
            let (var_19: int64) = (var_17 - var_18)
            let (var_20: bool) =
                if (var_10 = 922337203685477580L) then
                    (var_19 <= 7L)
                else
                    false
            let (var_21: bool) =
                if var_20 then
                    true
                else
                    (var_10 < 922337203685477580L)
            if var_21 then
                let (var_22: int64) = (var_10 * 10L)
                let (var_23: int64) = (var_22 + var_19)
                method_39((var_0: bool), (var_1: int64), (var_2: int64), (var_3: int64), (var_4: int64), (var_5: int64), (var_6: int64), (var_7: int64), (var_8: int64), (var_9: string), (var_23: int64), (var_16: int64))
            else
                (failwith "integer overflow")
        else
            let (var_24: int64) =
                if var_0 then
                    var_10
                else
                    (-var_10)
            let (var_25: int64) = 0L
            method_40((var_24: int64), (var_1: int64), (var_2: int64), (var_3: int64), (var_4: int64), (var_5: int64), (var_6: int64), (var_7: int64), (var_8: int64), (var_9: string), (var_25: int64), (var_16: int64))
    else
        let (var_26: int64) =
            if var_0 then
                var_10
            else
                (-var_10)
        let (var_27: int64) = 0L
        method_40((var_26: int64), (var_1: int64), (var_2: int64), (var_3: int64), (var_4: int64), (var_5: int64), (var_6: int64), (var_7: int64), (var_8: int64), (var_9: string), (var_27: int64), (var_11: int64))
and method_40((var_0: int64), (var_1: int64), (var_2: int64), (var_3: int64), (var_4: int64), (var_5: int64), (var_6: int64), (var_7: int64), (var_8: int64), (var_9: string), (var_10: int64), (var_11: int64)): unit =
    let (var_12: int64) = (var_10 + 1L)
    let (var_14: bool) =
        if (var_11 >= 0L) then
            let (var_13: int64) = (int64 var_9.Length)
            (var_11 < var_13)
        else
            false
    if var_14 then
        let (var_15: char) = var_9.[int32 var_11]
        let (var_17: bool) =
            if (var_15 = ' ') then
                true
            else
                if (var_15 = '\n') then
                    true
                else
                    (var_15 = '\r')
        let (var_18: int64) = (var_11 + 1L)
        if var_17 then
            method_40((var_0: int64), (var_1: int64), (var_2: int64), (var_3: int64), (var_4: int64), (var_5: int64), (var_6: int64), (var_7: int64), (var_8: int64), (var_9: string), (var_12: int64), (var_18: int64))
        else
            let (var_20: bool) =
                if (var_18 >= 0L) then
                    let (var_19: int64) = (int64 var_9.Length)
                    (var_18 < var_19)
                else
                    false
            if var_20 then
                let (var_21: char) = var_9.[int32 var_18]
                let (var_22: bool) = ('-' = var_21)
                let (var_23: int64) = (var_18 + 1L)
                if var_22 then
                    let (var_24: bool) = false
                    method_41((var_24: bool), (var_0: int64), (var_1: int64), (var_2: int64), (var_3: int64), (var_4: int64), (var_5: int64), (var_6: int64), (var_7: int64), (var_8: int64), (var_9: string), (var_23: int64))
                else
                    let (var_25: bool) = true
                    method_41((var_25: bool), (var_0: int64), (var_1: int64), (var_2: int64), (var_3: int64), (var_4: int64), (var_5: int64), (var_6: int64), (var_7: int64), (var_8: int64), (var_9: string), (var_23: int64))
            else
                let (var_26: bool) = true
                method_41((var_26: bool), (var_0: int64), (var_1: int64), (var_2: int64), (var_3: int64), (var_4: int64), (var_5: int64), (var_6: int64), (var_7: int64), (var_8: int64), (var_9: string), (var_18: int64))
    else
        let (var_28: bool) =
            if (var_11 >= 0L) then
                let (var_27: int64) = (int64 var_9.Length)
                (var_11 < var_27)
            else
                false
        if var_28 then
            let (var_29: char) = var_9.[int32 var_11]
            let (var_30: bool) = ('-' = var_29)
            let (var_31: int64) = (var_11 + 1L)
            if var_30 then
                let (var_32: bool) = false
                method_41((var_32: bool), (var_0: int64), (var_1: int64), (var_2: int64), (var_3: int64), (var_4: int64), (var_5: int64), (var_6: int64), (var_7: int64), (var_8: int64), (var_9: string), (var_31: int64))
            else
                let (var_33: bool) = true
                method_41((var_33: bool), (var_0: int64), (var_1: int64), (var_2: int64), (var_3: int64), (var_4: int64), (var_5: int64), (var_6: int64), (var_7: int64), (var_8: int64), (var_9: string), (var_31: int64))
        else
            let (var_34: bool) = true
            method_41((var_34: bool), (var_0: int64), (var_1: int64), (var_2: int64), (var_3: int64), (var_4: int64), (var_5: int64), (var_6: int64), (var_7: int64), (var_8: int64), (var_9: string), (var_11: int64))
and method_41((var_0: bool), (var_1: int64), (var_2: int64), (var_3: int64), (var_4: int64), (var_5: int64), (var_6: int64), (var_7: int64), (var_8: int64), (var_9: int64), (var_10: string), (var_11: int64)): unit =
    let (var_13: bool) =
        if (var_11 >= 0L) then
            let (var_12: int64) = (int64 var_10.Length)
            (var_11 < var_12)
        else
            false
    if var_13 then
        let (var_14: char) = var_10.[int32 var_11]
        let (var_15: bool) =
            if (var_14 >= '0') then
                (var_14 <= '9')
            else
                false
        let (var_16: int64) = (var_11 + 1L)
        if var_15 then
            let (var_17: int64) = System.Convert.ToInt64(var_14)
            let (var_18: int64) = System.Convert.ToInt64('0')
            let (var_19: int64) = (var_17 - var_18)
            let (var_20: int64) = (0L + var_19)
            method_42((var_0: bool), (var_1: int64), (var_2: int64), (var_3: int64), (var_4: int64), (var_5: int64), (var_6: int64), (var_7: int64), (var_8: int64), (var_9: int64), (var_10: string), (var_20: int64), (var_16: int64))
        else
            (failwith "pint64")
    else
        (failwith "pint64")
and method_42((var_0: bool), (var_1: int64), (var_2: int64), (var_3: int64), (var_4: int64), (var_5: int64), (var_6: int64), (var_7: int64), (var_8: int64), (var_9: int64), (var_10: string), (var_11: int64), (var_12: int64)): unit =
    let (var_14: bool) =
        if (var_12 >= 0L) then
            let (var_13: int64) = (int64 var_10.Length)
            (var_12 < var_13)
        else
            false
    if var_14 then
        let (var_15: char) = var_10.[int32 var_12]
        let (var_16: bool) =
            if (var_15 >= '0') then
                (var_15 <= '9')
            else
                false
        let (var_17: int64) = (var_12 + 1L)
        if var_16 then
            let (var_18: int64) = System.Convert.ToInt64(var_15)
            let (var_19: int64) = System.Convert.ToInt64('0')
            let (var_20: int64) = (var_18 - var_19)
            let (var_21: bool) =
                if (var_11 = 922337203685477580L) then
                    (var_20 <= 7L)
                else
                    false
            let (var_22: bool) =
                if var_21 then
                    true
                else
                    (var_11 < 922337203685477580L)
            if var_22 then
                let (var_23: int64) = (var_11 * 10L)
                let (var_24: int64) = (var_23 + var_20)
                method_42((var_0: bool), (var_1: int64), (var_2: int64), (var_3: int64), (var_4: int64), (var_5: int64), (var_6: int64), (var_7: int64), (var_8: int64), (var_9: int64), (var_10: string), (var_24: int64), (var_17: int64))
            else
                (failwith "integer overflow")
        else
            let (var_25: int64) =
                if var_0 then
                    var_11
                else
                    (-var_11)
            let (var_26: int64) = 0L
            method_43((var_25: int64), (var_1: int64), (var_2: int64), (var_3: int64), (var_4: int64), (var_5: int64), (var_6: int64), (var_7: int64), (var_8: int64), (var_9: int64), (var_10: string), (var_26: int64), (var_17: int64))
    else
        let (var_27: int64) =
            if var_0 then
                var_11
            else
                (-var_11)
        let (var_28: int64) = 0L
        method_43((var_27: int64), (var_1: int64), (var_2: int64), (var_3: int64), (var_4: int64), (var_5: int64), (var_6: int64), (var_7: int64), (var_8: int64), (var_9: int64), (var_10: string), (var_28: int64), (var_12: int64))
and method_43((var_0: int64), (var_1: int64), (var_2: int64), (var_3: int64), (var_4: int64), (var_5: int64), (var_6: int64), (var_7: int64), (var_8: int64), (var_9: int64), (var_10: string), (var_11: int64), (var_12: int64)): unit =
    let (var_13: int64) = (var_11 + 1L)
    let (var_15: bool) =
        if (var_12 >= 0L) then
            let (var_14: int64) = (int64 var_10.Length)
            (var_12 < var_14)
        else
            false
    if var_15 then
        let (var_16: char) = var_10.[int32 var_12]
        let (var_18: bool) =
            if (var_16 = ' ') then
                true
            else
                if (var_16 = '\n') then
                    true
                else
                    (var_16 = '\r')
        let (var_19: int64) = (var_12 + 1L)
        if var_18 then
            method_43((var_0: int64), (var_1: int64), (var_2: int64), (var_3: int64), (var_4: int64), (var_5: int64), (var_6: int64), (var_7: int64), (var_8: int64), (var_9: int64), (var_10: string), (var_13: int64), (var_19: int64))
        else
            let (var_21: bool) =
                if (var_19 >= 0L) then
                    let (var_20: int64) = (int64 var_10.Length)
                    (var_19 < var_20)
                else
                    false
            if var_21 then
                let (var_22: char) = var_10.[int32 var_19]
                let (var_23: bool) = ('-' = var_22)
                let (var_24: int64) = (var_19 + 1L)
                if var_23 then
                    let (var_25: bool) = false
                    method_44((var_25: bool), (var_0: int64), (var_1: int64), (var_2: int64), (var_3: int64), (var_4: int64), (var_5: int64), (var_6: int64), (var_7: int64), (var_8: int64), (var_9: int64), (var_10: string), (var_24: int64))
                else
                    let (var_26: bool) = true
                    method_44((var_26: bool), (var_0: int64), (var_1: int64), (var_2: int64), (var_3: int64), (var_4: int64), (var_5: int64), (var_6: int64), (var_7: int64), (var_8: int64), (var_9: int64), (var_10: string), (var_24: int64))
            else
                let (var_27: bool) = true
                method_44((var_27: bool), (var_0: int64), (var_1: int64), (var_2: int64), (var_3: int64), (var_4: int64), (var_5: int64), (var_6: int64), (var_7: int64), (var_8: int64), (var_9: int64), (var_10: string), (var_19: int64))
    else
        let (var_29: bool) =
            if (var_12 >= 0L) then
                let (var_28: int64) = (int64 var_10.Length)
                (var_12 < var_28)
            else
                false
        if var_29 then
            let (var_30: char) = var_10.[int32 var_12]
            let (var_31: bool) = ('-' = var_30)
            let (var_32: int64) = (var_12 + 1L)
            if var_31 then
                let (var_33: bool) = false
                method_44((var_33: bool), (var_0: int64), (var_1: int64), (var_2: int64), (var_3: int64), (var_4: int64), (var_5: int64), (var_6: int64), (var_7: int64), (var_8: int64), (var_9: int64), (var_10: string), (var_32: int64))
            else
                let (var_34: bool) = true
                method_44((var_34: bool), (var_0: int64), (var_1: int64), (var_2: int64), (var_3: int64), (var_4: int64), (var_5: int64), (var_6: int64), (var_7: int64), (var_8: int64), (var_9: int64), (var_10: string), (var_32: int64))
        else
            let (var_35: bool) = true
            method_44((var_35: bool), (var_0: int64), (var_1: int64), (var_2: int64), (var_3: int64), (var_4: int64), (var_5: int64), (var_6: int64), (var_7: int64), (var_8: int64), (var_9: int64), (var_10: string), (var_12: int64))
and method_44((var_0: bool), (var_1: int64), (var_2: int64), (var_3: int64), (var_4: int64), (var_5: int64), (var_6: int64), (var_7: int64), (var_8: int64), (var_9: int64), (var_10: int64), (var_11: string), (var_12: int64)): unit =
    let (var_14: bool) =
        if (var_12 >= 0L) then
            let (var_13: int64) = (int64 var_11.Length)
            (var_12 < var_13)
        else
            false
    if var_14 then
        let (var_15: char) = var_11.[int32 var_12]
        let (var_16: bool) =
            if (var_15 >= '0') then
                (var_15 <= '9')
            else
                false
        let (var_17: int64) = (var_12 + 1L)
        if var_16 then
            let (var_18: int64) = System.Convert.ToInt64(var_15)
            let (var_19: int64) = System.Convert.ToInt64('0')
            let (var_20: int64) = (var_18 - var_19)
            let (var_21: int64) = (0L + var_20)
            method_45((var_0: bool), (var_1: int64), (var_2: int64), (var_3: int64), (var_4: int64), (var_5: int64), (var_6: int64), (var_7: int64), (var_8: int64), (var_9: int64), (var_10: int64), (var_11: string), (var_21: int64), (var_17: int64))
        else
            (failwith "pint64")
    else
        (failwith "pint64")
and method_45((var_0: bool), (var_1: int64), (var_2: int64), (var_3: int64), (var_4: int64), (var_5: int64), (var_6: int64), (var_7: int64), (var_8: int64), (var_9: int64), (var_10: int64), (var_11: string), (var_12: int64), (var_13: int64)): unit =
    let (var_15: bool) =
        if (var_13 >= 0L) then
            let (var_14: int64) = (int64 var_11.Length)
            (var_13 < var_14)
        else
            false
    if var_15 then
        let (var_16: char) = var_11.[int32 var_13]
        let (var_17: bool) =
            if (var_16 >= '0') then
                (var_16 <= '9')
            else
                false
        let (var_18: int64) = (var_13 + 1L)
        if var_17 then
            let (var_19: int64) = System.Convert.ToInt64(var_16)
            let (var_20: int64) = System.Convert.ToInt64('0')
            let (var_21: int64) = (var_19 - var_20)
            let (var_22: bool) =
                if (var_12 = 922337203685477580L) then
                    (var_21 <= 7L)
                else
                    false
            let (var_23: bool) =
                if var_22 then
                    true
                else
                    (var_12 < 922337203685477580L)
            if var_23 then
                let (var_24: int64) = (var_12 * 10L)
                let (var_25: int64) = (var_24 + var_21)
                method_45((var_0: bool), (var_1: int64), (var_2: int64), (var_3: int64), (var_4: int64), (var_5: int64), (var_6: int64), (var_7: int64), (var_8: int64), (var_9: int64), (var_10: int64), (var_11: string), (var_25: int64), (var_18: int64))
            else
                (failwith "integer overflow")
        else
            let (var_26: int64) =
                if var_0 then
                    var_12
                else
                    (-var_12)
            let (var_27: int64) = 0L
            method_46((var_26: int64), (var_1: int64), (var_2: int64), (var_3: int64), (var_4: int64), (var_5: int64), (var_6: int64), (var_7: int64), (var_8: int64), (var_9: int64), (var_10: int64), (var_11: string), (var_27: int64), (var_18: int64))
    else
        let (var_28: int64) =
            if var_0 then
                var_12
            else
                (-var_12)
        let (var_29: int64) = 0L
        method_46((var_28: int64), (var_1: int64), (var_2: int64), (var_3: int64), (var_4: int64), (var_5: int64), (var_6: int64), (var_7: int64), (var_8: int64), (var_9: int64), (var_10: int64), (var_11: string), (var_29: int64), (var_13: int64))
and method_46((var_0: int64), (var_1: int64), (var_2: int64), (var_3: int64), (var_4: int64), (var_5: int64), (var_6: int64), (var_7: int64), (var_8: int64), (var_9: int64), (var_10: int64), (var_11: string), (var_12: int64), (var_13: int64)): unit =
    let (var_14: int64) = (var_12 + 1L)
    let (var_16: bool) =
        if (var_13 >= 0L) then
            let (var_15: int64) = (int64 var_11.Length)
            (var_13 < var_15)
        else
            false
    if var_16 then
        let (var_17: char) = var_11.[int32 var_13]
        let (var_19: bool) =
            if (var_17 = ' ') then
                true
            else
                if (var_17 = '\n') then
                    true
                else
                    (var_17 = '\r')
        let (var_20: int64) = (var_13 + 1L)
        if var_19 then
            method_46((var_0: int64), (var_1: int64), (var_2: int64), (var_3: int64), (var_4: int64), (var_5: int64), (var_6: int64), (var_7: int64), (var_8: int64), (var_9: int64), (var_10: int64), (var_11: string), (var_14: int64), (var_20: int64))
        else
            let (var_22: bool) =
                if (var_20 >= 0L) then
                    let (var_21: int64) = (int64 var_11.Length)
                    (var_20 < var_21)
                else
                    false
            if var_22 then
                let (var_23: char) = var_11.[int32 var_20]
                let (var_24: bool) = ('-' = var_23)
                let (var_25: int64) = (var_20 + 1L)
                if var_24 then
                    let (var_26: bool) = false
                    method_47((var_26: bool), (var_0: int64), (var_1: int64), (var_2: int64), (var_3: int64), (var_4: int64), (var_5: int64), (var_6: int64), (var_7: int64), (var_8: int64), (var_9: int64), (var_10: int64), (var_11: string), (var_25: int64))
                else
                    let (var_27: bool) = true
                    method_47((var_27: bool), (var_0: int64), (var_1: int64), (var_2: int64), (var_3: int64), (var_4: int64), (var_5: int64), (var_6: int64), (var_7: int64), (var_8: int64), (var_9: int64), (var_10: int64), (var_11: string), (var_25: int64))
            else
                let (var_28: bool) = true
                method_47((var_28: bool), (var_0: int64), (var_1: int64), (var_2: int64), (var_3: int64), (var_4: int64), (var_5: int64), (var_6: int64), (var_7: int64), (var_8: int64), (var_9: int64), (var_10: int64), (var_11: string), (var_20: int64))
    else
        let (var_30: bool) =
            if (var_13 >= 0L) then
                let (var_29: int64) = (int64 var_11.Length)
                (var_13 < var_29)
            else
                false
        if var_30 then
            let (var_31: char) = var_11.[int32 var_13]
            let (var_32: bool) = ('-' = var_31)
            let (var_33: int64) = (var_13 + 1L)
            if var_32 then
                let (var_34: bool) = false
                method_47((var_34: bool), (var_0: int64), (var_1: int64), (var_2: int64), (var_3: int64), (var_4: int64), (var_5: int64), (var_6: int64), (var_7: int64), (var_8: int64), (var_9: int64), (var_10: int64), (var_11: string), (var_33: int64))
            else
                let (var_35: bool) = true
                method_47((var_35: bool), (var_0: int64), (var_1: int64), (var_2: int64), (var_3: int64), (var_4: int64), (var_5: int64), (var_6: int64), (var_7: int64), (var_8: int64), (var_9: int64), (var_10: int64), (var_11: string), (var_33: int64))
        else
            let (var_36: bool) = true
            method_47((var_36: bool), (var_0: int64), (var_1: int64), (var_2: int64), (var_3: int64), (var_4: int64), (var_5: int64), (var_6: int64), (var_7: int64), (var_8: int64), (var_9: int64), (var_10: int64), (var_11: string), (var_13: int64))
and method_47((var_0: bool), (var_1: int64), (var_2: int64), (var_3: int64), (var_4: int64), (var_5: int64), (var_6: int64), (var_7: int64), (var_8: int64), (var_9: int64), (var_10: int64), (var_11: int64), (var_12: string), (var_13: int64)): unit =
    let (var_15: bool) =
        if (var_13 >= 0L) then
            let (var_14: int64) = (int64 var_12.Length)
            (var_13 < var_14)
        else
            false
    if var_15 then
        let (var_16: char) = var_12.[int32 var_13]
        let (var_17: bool) =
            if (var_16 >= '0') then
                (var_16 <= '9')
            else
                false
        let (var_18: int64) = (var_13 + 1L)
        if var_17 then
            let (var_19: int64) = System.Convert.ToInt64(var_16)
            let (var_20: int64) = System.Convert.ToInt64('0')
            let (var_21: int64) = (var_19 - var_20)
            let (var_22: int64) = (0L + var_21)
            method_48((var_0: bool), (var_1: int64), (var_2: int64), (var_3: int64), (var_4: int64), (var_5: int64), (var_6: int64), (var_7: int64), (var_8: int64), (var_9: int64), (var_10: int64), (var_11: int64), (var_12: string), (var_22: int64), (var_18: int64))
        else
            (failwith "pint64")
    else
        (failwith "pint64")
and method_48((var_0: bool), (var_1: int64), (var_2: int64), (var_3: int64), (var_4: int64), (var_5: int64), (var_6: int64), (var_7: int64), (var_8: int64), (var_9: int64), (var_10: int64), (var_11: int64), (var_12: string), (var_13: int64), (var_14: int64)): unit =
    let (var_16: bool) =
        if (var_14 >= 0L) then
            let (var_15: int64) = (int64 var_12.Length)
            (var_14 < var_15)
        else
            false
    if var_16 then
        let (var_17: char) = var_12.[int32 var_14]
        let (var_18: bool) =
            if (var_17 >= '0') then
                (var_17 <= '9')
            else
                false
        let (var_19: int64) = (var_14 + 1L)
        if var_18 then
            let (var_20: int64) = System.Convert.ToInt64(var_17)
            let (var_21: int64) = System.Convert.ToInt64('0')
            let (var_22: int64) = (var_20 - var_21)
            let (var_23: bool) =
                if (var_13 = 922337203685477580L) then
                    (var_22 <= 7L)
                else
                    false
            let (var_24: bool) =
                if var_23 then
                    true
                else
                    (var_13 < 922337203685477580L)
            if var_24 then
                let (var_25: int64) = (var_13 * 10L)
                let (var_26: int64) = (var_25 + var_22)
                method_48((var_0: bool), (var_1: int64), (var_2: int64), (var_3: int64), (var_4: int64), (var_5: int64), (var_6: int64), (var_7: int64), (var_8: int64), (var_9: int64), (var_10: int64), (var_11: int64), (var_12: string), (var_26: int64), (var_19: int64))
            else
                (failwith "integer overflow")
        else
            let (var_27: int64) =
                if var_0 then
                    var_13
                else
                    (-var_13)
            let (var_28: int64) = 0L
            method_49((var_27: int64), (var_1: int64), (var_2: int64), (var_3: int64), (var_4: int64), (var_5: int64), (var_6: int64), (var_7: int64), (var_8: int64), (var_9: int64), (var_10: int64), (var_11: int64), (var_12: string), (var_28: int64), (var_19: int64))
    else
        let (var_29: int64) =
            if var_0 then
                var_13
            else
                (-var_13)
        let (var_30: int64) = 0L
        method_49((var_29: int64), (var_1: int64), (var_2: int64), (var_3: int64), (var_4: int64), (var_5: int64), (var_6: int64), (var_7: int64), (var_8: int64), (var_9: int64), (var_10: int64), (var_11: int64), (var_12: string), (var_30: int64), (var_14: int64))
and method_49((var_0: int64), (var_1: int64), (var_2: int64), (var_3: int64), (var_4: int64), (var_5: int64), (var_6: int64), (var_7: int64), (var_8: int64), (var_9: int64), (var_10: int64), (var_11: int64), (var_12: string), (var_13: int64), (var_14: int64)): unit =
    let (var_15: int64) = (var_13 + 1L)
    let (var_17: bool) =
        if (var_14 >= 0L) then
            let (var_16: int64) = (int64 var_12.Length)
            (var_14 < var_16)
        else
            false
    if var_17 then
        let (var_18: char) = var_12.[int32 var_14]
        let (var_20: bool) =
            if (var_18 = ' ') then
                true
            else
                if (var_18 = '\n') then
                    true
                else
                    (var_18 = '\r')
        let (var_21: int64) = (var_14 + 1L)
        if var_20 then
            method_49((var_0: int64), (var_1: int64), (var_2: int64), (var_3: int64), (var_4: int64), (var_5: int64), (var_6: int64), (var_7: int64), (var_8: int64), (var_9: int64), (var_10: int64), (var_11: int64), (var_12: string), (var_15: int64), (var_21: int64))
        else
            let (var_23: bool) =
                if (var_21 >= 0L) then
                    let (var_22: int64) = (int64 var_12.Length)
                    (var_21 < var_22)
                else
                    false
            if var_23 then
                let (var_24: char) = var_12.[int32 var_21]
                let (var_25: bool) = ('-' = var_24)
                let (var_26: int64) = (var_21 + 1L)
                if var_25 then
                    let (var_27: bool) = false
                    method_50((var_27: bool), (var_0: int64), (var_1: int64), (var_2: int64), (var_3: int64), (var_4: int64), (var_5: int64), (var_6: int64), (var_7: int64), (var_8: int64), (var_9: int64), (var_10: int64), (var_11: int64), (var_12: string), (var_26: int64))
                else
                    let (var_28: bool) = true
                    method_50((var_28: bool), (var_0: int64), (var_1: int64), (var_2: int64), (var_3: int64), (var_4: int64), (var_5: int64), (var_6: int64), (var_7: int64), (var_8: int64), (var_9: int64), (var_10: int64), (var_11: int64), (var_12: string), (var_26: int64))
            else
                let (var_29: bool) = true
                method_50((var_29: bool), (var_0: int64), (var_1: int64), (var_2: int64), (var_3: int64), (var_4: int64), (var_5: int64), (var_6: int64), (var_7: int64), (var_8: int64), (var_9: int64), (var_10: int64), (var_11: int64), (var_12: string), (var_21: int64))
    else
        let (var_31: bool) =
            if (var_14 >= 0L) then
                let (var_30: int64) = (int64 var_12.Length)
                (var_14 < var_30)
            else
                false
        if var_31 then
            let (var_32: char) = var_12.[int32 var_14]
            let (var_33: bool) = ('-' = var_32)
            let (var_34: int64) = (var_14 + 1L)
            if var_33 then
                let (var_35: bool) = false
                method_50((var_35: bool), (var_0: int64), (var_1: int64), (var_2: int64), (var_3: int64), (var_4: int64), (var_5: int64), (var_6: int64), (var_7: int64), (var_8: int64), (var_9: int64), (var_10: int64), (var_11: int64), (var_12: string), (var_34: int64))
            else
                let (var_36: bool) = true
                method_50((var_36: bool), (var_0: int64), (var_1: int64), (var_2: int64), (var_3: int64), (var_4: int64), (var_5: int64), (var_6: int64), (var_7: int64), (var_8: int64), (var_9: int64), (var_10: int64), (var_11: int64), (var_12: string), (var_34: int64))
        else
            let (var_37: bool) = true
            method_50((var_37: bool), (var_0: int64), (var_1: int64), (var_2: int64), (var_3: int64), (var_4: int64), (var_5: int64), (var_6: int64), (var_7: int64), (var_8: int64), (var_9: int64), (var_10: int64), (var_11: int64), (var_12: string), (var_14: int64))
and method_50((var_0: bool), (var_1: int64), (var_2: int64), (var_3: int64), (var_4: int64), (var_5: int64), (var_6: int64), (var_7: int64), (var_8: int64), (var_9: int64), (var_10: int64), (var_11: int64), (var_12: int64), (var_13: string), (var_14: int64)): unit =
    let (var_16: bool) =
        if (var_14 >= 0L) then
            let (var_15: int64) = (int64 var_13.Length)
            (var_14 < var_15)
        else
            false
    if var_16 then
        let (var_17: char) = var_13.[int32 var_14]
        let (var_18: bool) =
            if (var_17 >= '0') then
                (var_17 <= '9')
            else
                false
        let (var_19: int64) = (var_14 + 1L)
        if var_18 then
            let (var_20: int64) = System.Convert.ToInt64(var_17)
            let (var_21: int64) = System.Convert.ToInt64('0')
            let (var_22: int64) = (var_20 - var_21)
            let (var_23: int64) = (0L + var_22)
            method_51((var_0: bool), (var_1: int64), (var_2: int64), (var_3: int64), (var_4: int64), (var_5: int64), (var_6: int64), (var_7: int64), (var_8: int64), (var_9: int64), (var_10: int64), (var_11: int64), (var_12: int64), (var_13: string), (var_23: int64), (var_19: int64))
        else
            (failwith "pint64")
    else
        (failwith "pint64")
and method_51((var_0: bool), (var_1: int64), (var_2: int64), (var_3: int64), (var_4: int64), (var_5: int64), (var_6: int64), (var_7: int64), (var_8: int64), (var_9: int64), (var_10: int64), (var_11: int64), (var_12: int64), (var_13: string), (var_14: int64), (var_15: int64)): unit =
    let (var_17: bool) =
        if (var_15 >= 0L) then
            let (var_16: int64) = (int64 var_13.Length)
            (var_15 < var_16)
        else
            false
    if var_17 then
        let (var_18: char) = var_13.[int32 var_15]
        let (var_19: bool) =
            if (var_18 >= '0') then
                (var_18 <= '9')
            else
                false
        let (var_20: int64) = (var_15 + 1L)
        if var_19 then
            let (var_21: int64) = System.Convert.ToInt64(var_18)
            let (var_22: int64) = System.Convert.ToInt64('0')
            let (var_23: int64) = (var_21 - var_22)
            let (var_24: bool) =
                if (var_14 = 922337203685477580L) then
                    (var_23 <= 7L)
                else
                    false
            let (var_25: bool) =
                if var_24 then
                    true
                else
                    (var_14 < 922337203685477580L)
            if var_25 then
                let (var_26: int64) = (var_14 * 10L)
                let (var_27: int64) = (var_26 + var_23)
                method_51((var_0: bool), (var_1: int64), (var_2: int64), (var_3: int64), (var_4: int64), (var_5: int64), (var_6: int64), (var_7: int64), (var_8: int64), (var_9: int64), (var_10: int64), (var_11: int64), (var_12: int64), (var_13: string), (var_27: int64), (var_20: int64))
            else
                (failwith "integer overflow")
        else
            let (var_28: int64) =
                if var_0 then
                    var_14
                else
                    (-var_14)
            let (var_29: int64) = 0L
            method_52((var_28: int64), (var_1: int64), (var_2: int64), (var_3: int64), (var_4: int64), (var_5: int64), (var_6: int64), (var_7: int64), (var_8: int64), (var_9: int64), (var_10: int64), (var_11: int64), (var_12: int64), (var_13: string), (var_29: int64), (var_20: int64))
    else
        let (var_30: int64) =
            if var_0 then
                var_14
            else
                (-var_14)
        let (var_31: int64) = 0L
        method_52((var_30: int64), (var_1: int64), (var_2: int64), (var_3: int64), (var_4: int64), (var_5: int64), (var_6: int64), (var_7: int64), (var_8: int64), (var_9: int64), (var_10: int64), (var_11: int64), (var_12: int64), (var_13: string), (var_31: int64), (var_15: int64))
and method_52((var_0: int64), (var_1: int64), (var_2: int64), (var_3: int64), (var_4: int64), (var_5: int64), (var_6: int64), (var_7: int64), (var_8: int64), (var_9: int64), (var_10: int64), (var_11: int64), (var_12: int64), (var_13: string), (var_14: int64), (var_15: int64)): unit =
    let (var_16: int64) = (var_14 + 1L)
    let (var_18: bool) =
        if (var_15 >= 0L) then
            let (var_17: int64) = (int64 var_13.Length)
            (var_15 < var_17)
        else
            false
    if var_18 then
        let (var_19: char) = var_13.[int32 var_15]
        let (var_21: bool) =
            if (var_19 = ' ') then
                true
            else
                if (var_19 = '\n') then
                    true
                else
                    (var_19 = '\r')
        let (var_22: int64) = (var_15 + 1L)
        if var_21 then
            method_52((var_0: int64), (var_1: int64), (var_2: int64), (var_3: int64), (var_4: int64), (var_5: int64), (var_6: int64), (var_7: int64), (var_8: int64), (var_9: int64), (var_10: int64), (var_11: int64), (var_12: int64), (var_13: string), (var_16: int64), (var_22: int64))
        else
            let (var_24: bool) =
                if (var_22 >= 0L) then
                    let (var_23: int64) = (int64 var_13.Length)
                    (var_22 < var_23)
                else
                    false
            if var_24 then
                let (var_25: char) = var_13.[int32 var_22]
                let (var_26: bool) = ('-' = var_25)
                let (var_27: int64) = (var_22 + 1L)
                if var_26 then
                    let (var_28: bool) = false
                    method_53((var_28: bool), (var_0: int64), (var_1: int64), (var_2: int64), (var_3: int64), (var_4: int64), (var_5: int64), (var_6: int64), (var_7: int64), (var_8: int64), (var_9: int64), (var_10: int64), (var_11: int64), (var_12: int64), (var_13: string), (var_27: int64))
                else
                    let (var_29: bool) = true
                    method_53((var_29: bool), (var_0: int64), (var_1: int64), (var_2: int64), (var_3: int64), (var_4: int64), (var_5: int64), (var_6: int64), (var_7: int64), (var_8: int64), (var_9: int64), (var_10: int64), (var_11: int64), (var_12: int64), (var_13: string), (var_27: int64))
            else
                let (var_30: bool) = true
                method_53((var_30: bool), (var_0: int64), (var_1: int64), (var_2: int64), (var_3: int64), (var_4: int64), (var_5: int64), (var_6: int64), (var_7: int64), (var_8: int64), (var_9: int64), (var_10: int64), (var_11: int64), (var_12: int64), (var_13: string), (var_22: int64))
    else
        let (var_32: bool) =
            if (var_15 >= 0L) then
                let (var_31: int64) = (int64 var_13.Length)
                (var_15 < var_31)
            else
                false
        if var_32 then
            let (var_33: char) = var_13.[int32 var_15]
            let (var_34: bool) = ('-' = var_33)
            let (var_35: int64) = (var_15 + 1L)
            if var_34 then
                let (var_36: bool) = false
                method_53((var_36: bool), (var_0: int64), (var_1: int64), (var_2: int64), (var_3: int64), (var_4: int64), (var_5: int64), (var_6: int64), (var_7: int64), (var_8: int64), (var_9: int64), (var_10: int64), (var_11: int64), (var_12: int64), (var_13: string), (var_35: int64))
            else
                let (var_37: bool) = true
                method_53((var_37: bool), (var_0: int64), (var_1: int64), (var_2: int64), (var_3: int64), (var_4: int64), (var_5: int64), (var_6: int64), (var_7: int64), (var_8: int64), (var_9: int64), (var_10: int64), (var_11: int64), (var_12: int64), (var_13: string), (var_35: int64))
        else
            let (var_38: bool) = true
            method_53((var_38: bool), (var_0: int64), (var_1: int64), (var_2: int64), (var_3: int64), (var_4: int64), (var_5: int64), (var_6: int64), (var_7: int64), (var_8: int64), (var_9: int64), (var_10: int64), (var_11: int64), (var_12: int64), (var_13: string), (var_15: int64))
and method_53((var_0: bool), (var_1: int64), (var_2: int64), (var_3: int64), (var_4: int64), (var_5: int64), (var_6: int64), (var_7: int64), (var_8: int64), (var_9: int64), (var_10: int64), (var_11: int64), (var_12: int64), (var_13: int64), (var_14: string), (var_15: int64)): unit =
    let (var_17: bool) =
        if (var_15 >= 0L) then
            let (var_16: int64) = (int64 var_14.Length)
            (var_15 < var_16)
        else
            false
    if var_17 then
        let (var_18: char) = var_14.[int32 var_15]
        let (var_19: bool) =
            if (var_18 >= '0') then
                (var_18 <= '9')
            else
                false
        let (var_20: int64) = (var_15 + 1L)
        if var_19 then
            let (var_21: int64) = System.Convert.ToInt64(var_18)
            let (var_22: int64) = System.Convert.ToInt64('0')
            let (var_23: int64) = (var_21 - var_22)
            let (var_24: int64) = (0L + var_23)
            method_54((var_0: bool), (var_1: int64), (var_2: int64), (var_3: int64), (var_4: int64), (var_5: int64), (var_6: int64), (var_7: int64), (var_8: int64), (var_9: int64), (var_10: int64), (var_11: int64), (var_12: int64), (var_13: int64), (var_14: string), (var_24: int64), (var_20: int64))
        else
            (failwith "pint64")
    else
        (failwith "pint64")
and method_54((var_0: bool), (var_1: int64), (var_2: int64), (var_3: int64), (var_4: int64), (var_5: int64), (var_6: int64), (var_7: int64), (var_8: int64), (var_9: int64), (var_10: int64), (var_11: int64), (var_12: int64), (var_13: int64), (var_14: string), (var_15: int64), (var_16: int64)): unit =
    let (var_18: bool) =
        if (var_16 >= 0L) then
            let (var_17: int64) = (int64 var_14.Length)
            (var_16 < var_17)
        else
            false
    if var_18 then
        let (var_19: char) = var_14.[int32 var_16]
        let (var_20: bool) =
            if (var_19 >= '0') then
                (var_19 <= '9')
            else
                false
        let (var_21: int64) = (var_16 + 1L)
        if var_20 then
            let (var_22: int64) = System.Convert.ToInt64(var_19)
            let (var_23: int64) = System.Convert.ToInt64('0')
            let (var_24: int64) = (var_22 - var_23)
            let (var_25: bool) =
                if (var_15 = 922337203685477580L) then
                    (var_24 <= 7L)
                else
                    false
            let (var_26: bool) =
                if var_25 then
                    true
                else
                    (var_15 < 922337203685477580L)
            if var_26 then
                let (var_27: int64) = (var_15 * 10L)
                let (var_28: int64) = (var_27 + var_24)
                method_54((var_0: bool), (var_1: int64), (var_2: int64), (var_3: int64), (var_4: int64), (var_5: int64), (var_6: int64), (var_7: int64), (var_8: int64), (var_9: int64), (var_10: int64), (var_11: int64), (var_12: int64), (var_13: int64), (var_14: string), (var_28: int64), (var_21: int64))
            else
                (failwith "integer overflow")
        else
            let (var_29: int64) =
                if var_0 then
                    var_15
                else
                    (-var_15)
            let (var_30: int64) = 0L
            method_55((var_29: int64), (var_1: int64), (var_2: int64), (var_3: int64), (var_4: int64), (var_5: int64), (var_6: int64), (var_7: int64), (var_8: int64), (var_9: int64), (var_10: int64), (var_11: int64), (var_12: int64), (var_13: int64), (var_14: string), (var_30: int64), (var_21: int64))
    else
        let (var_31: int64) =
            if var_0 then
                var_15
            else
                (-var_15)
        let (var_32: int64) = 0L
        method_55((var_31: int64), (var_1: int64), (var_2: int64), (var_3: int64), (var_4: int64), (var_5: int64), (var_6: int64), (var_7: int64), (var_8: int64), (var_9: int64), (var_10: int64), (var_11: int64), (var_12: int64), (var_13: int64), (var_14: string), (var_32: int64), (var_16: int64))
and method_55((var_0: int64), (var_1: int64), (var_2: int64), (var_3: int64), (var_4: int64), (var_5: int64), (var_6: int64), (var_7: int64), (var_8: int64), (var_9: int64), (var_10: int64), (var_11: int64), (var_12: int64), (var_13: int64), (var_14: string), (var_15: int64), (var_16: int64)): unit =
    let (var_17: int64) = (var_15 + 1L)
    let (var_19: bool) =
        if (var_16 >= 0L) then
            let (var_18: int64) = (int64 var_14.Length)
            (var_16 < var_18)
        else
            false
    if var_19 then
        let (var_20: char) = var_14.[int32 var_16]
        let (var_22: bool) =
            if (var_20 = ' ') then
                true
            else
                if (var_20 = '\n') then
                    true
                else
                    (var_20 = '\r')
        let (var_23: int64) = (var_16 + 1L)
        if var_22 then
            method_55((var_0: int64), (var_1: int64), (var_2: int64), (var_3: int64), (var_4: int64), (var_5: int64), (var_6: int64), (var_7: int64), (var_8: int64), (var_9: int64), (var_10: int64), (var_11: int64), (var_12: int64), (var_13: int64), (var_14: string), (var_17: int64), (var_23: int64))
        else
            let (var_25: bool) =
                if (var_23 >= 0L) then
                    let (var_24: int64) = (int64 var_14.Length)
                    (var_23 < var_24)
                else
                    false
            if var_25 then
                let (var_26: char) = var_14.[int32 var_23]
                let (var_27: bool) = ('-' = var_26)
                let (var_28: int64) = (var_23 + 1L)
                if var_27 then
                    let (var_29: bool) = false
                    method_56((var_29: bool), (var_0: int64), (var_1: int64), (var_2: int64), (var_3: int64), (var_4: int64), (var_5: int64), (var_6: int64), (var_7: int64), (var_8: int64), (var_9: int64), (var_10: int64), (var_11: int64), (var_12: int64), (var_13: int64), (var_14: string), (var_28: int64))
                else
                    let (var_30: bool) = true
                    method_56((var_30: bool), (var_0: int64), (var_1: int64), (var_2: int64), (var_3: int64), (var_4: int64), (var_5: int64), (var_6: int64), (var_7: int64), (var_8: int64), (var_9: int64), (var_10: int64), (var_11: int64), (var_12: int64), (var_13: int64), (var_14: string), (var_28: int64))
            else
                let (var_31: bool) = true
                method_56((var_31: bool), (var_0: int64), (var_1: int64), (var_2: int64), (var_3: int64), (var_4: int64), (var_5: int64), (var_6: int64), (var_7: int64), (var_8: int64), (var_9: int64), (var_10: int64), (var_11: int64), (var_12: int64), (var_13: int64), (var_14: string), (var_23: int64))
    else
        let (var_33: bool) =
            if (var_16 >= 0L) then
                let (var_32: int64) = (int64 var_14.Length)
                (var_16 < var_32)
            else
                false
        if var_33 then
            let (var_34: char) = var_14.[int32 var_16]
            let (var_35: bool) = ('-' = var_34)
            let (var_36: int64) = (var_16 + 1L)
            if var_35 then
                let (var_37: bool) = false
                method_56((var_37: bool), (var_0: int64), (var_1: int64), (var_2: int64), (var_3: int64), (var_4: int64), (var_5: int64), (var_6: int64), (var_7: int64), (var_8: int64), (var_9: int64), (var_10: int64), (var_11: int64), (var_12: int64), (var_13: int64), (var_14: string), (var_36: int64))
            else
                let (var_38: bool) = true
                method_56((var_38: bool), (var_0: int64), (var_1: int64), (var_2: int64), (var_3: int64), (var_4: int64), (var_5: int64), (var_6: int64), (var_7: int64), (var_8: int64), (var_9: int64), (var_10: int64), (var_11: int64), (var_12: int64), (var_13: int64), (var_14: string), (var_36: int64))
        else
            let (var_39: bool) = true
            method_56((var_39: bool), (var_0: int64), (var_1: int64), (var_2: int64), (var_3: int64), (var_4: int64), (var_5: int64), (var_6: int64), (var_7: int64), (var_8: int64), (var_9: int64), (var_10: int64), (var_11: int64), (var_12: int64), (var_13: int64), (var_14: string), (var_16: int64))
and method_56((var_0: bool), (var_1: int64), (var_2: int64), (var_3: int64), (var_4: int64), (var_5: int64), (var_6: int64), (var_7: int64), (var_8: int64), (var_9: int64), (var_10: int64), (var_11: int64), (var_12: int64), (var_13: int64), (var_14: int64), (var_15: string), (var_16: int64)): unit =
    let (var_18: bool) =
        if (var_16 >= 0L) then
            let (var_17: int64) = (int64 var_15.Length)
            (var_16 < var_17)
        else
            false
    if var_18 then
        let (var_19: char) = var_15.[int32 var_16]
        let (var_20: bool) =
            if (var_19 >= '0') then
                (var_19 <= '9')
            else
                false
        let (var_21: int64) = (var_16 + 1L)
        if var_20 then
            let (var_22: int64) = System.Convert.ToInt64(var_19)
            let (var_23: int64) = System.Convert.ToInt64('0')
            let (var_24: int64) = (var_22 - var_23)
            let (var_25: int64) = (0L + var_24)
            method_57((var_0: bool), (var_1: int64), (var_2: int64), (var_3: int64), (var_4: int64), (var_5: int64), (var_6: int64), (var_7: int64), (var_8: int64), (var_9: int64), (var_10: int64), (var_11: int64), (var_12: int64), (var_13: int64), (var_14: int64), (var_15: string), (var_25: int64), (var_21: int64))
        else
            (failwith "pint64")
    else
        (failwith "pint64")
and method_57((var_0: bool), (var_1: int64), (var_2: int64), (var_3: int64), (var_4: int64), (var_5: int64), (var_6: int64), (var_7: int64), (var_8: int64), (var_9: int64), (var_10: int64), (var_11: int64), (var_12: int64), (var_13: int64), (var_14: int64), (var_15: string), (var_16: int64), (var_17: int64)): unit =
    let (var_19: bool) =
        if (var_17 >= 0L) then
            let (var_18: int64) = (int64 var_15.Length)
            (var_17 < var_18)
        else
            false
    if var_19 then
        let (var_20: char) = var_15.[int32 var_17]
        let (var_21: bool) =
            if (var_20 >= '0') then
                (var_20 <= '9')
            else
                false
        let (var_22: int64) = (var_17 + 1L)
        if var_21 then
            let (var_23: int64) = System.Convert.ToInt64(var_20)
            let (var_24: int64) = System.Convert.ToInt64('0')
            let (var_25: int64) = (var_23 - var_24)
            let (var_26: bool) =
                if (var_16 = 922337203685477580L) then
                    (var_25 <= 7L)
                else
                    false
            let (var_27: bool) =
                if var_26 then
                    true
                else
                    (var_16 < 922337203685477580L)
            if var_27 then
                let (var_28: int64) = (var_16 * 10L)
                let (var_29: int64) = (var_28 + var_25)
                method_57((var_0: bool), (var_1: int64), (var_2: int64), (var_3: int64), (var_4: int64), (var_5: int64), (var_6: int64), (var_7: int64), (var_8: int64), (var_9: int64), (var_10: int64), (var_11: int64), (var_12: int64), (var_13: int64), (var_14: int64), (var_15: string), (var_29: int64), (var_22: int64))
            else
                (failwith "integer overflow")
        else
            let (var_30: int64) =
                if var_0 then
                    var_16
                else
                    (-var_16)
            let (var_31: int64) = 0L
            method_58((var_30: int64), (var_1: int64), (var_2: int64), (var_3: int64), (var_4: int64), (var_5: int64), (var_6: int64), (var_7: int64), (var_8: int64), (var_9: int64), (var_10: int64), (var_11: int64), (var_12: int64), (var_13: int64), (var_14: int64), (var_15: string), (var_31: int64), (var_22: int64))
    else
        let (var_32: int64) =
            if var_0 then
                var_16
            else
                (-var_16)
        let (var_33: int64) = 0L
        method_58((var_32: int64), (var_1: int64), (var_2: int64), (var_3: int64), (var_4: int64), (var_5: int64), (var_6: int64), (var_7: int64), (var_8: int64), (var_9: int64), (var_10: int64), (var_11: int64), (var_12: int64), (var_13: int64), (var_14: int64), (var_15: string), (var_33: int64), (var_17: int64))
and method_58((var_0: int64), (var_1: int64), (var_2: int64), (var_3: int64), (var_4: int64), (var_5: int64), (var_6: int64), (var_7: int64), (var_8: int64), (var_9: int64), (var_10: int64), (var_11: int64), (var_12: int64), (var_13: int64), (var_14: int64), (var_15: string), (var_16: int64), (var_17: int64)): unit =
    let (var_18: int64) = (var_16 + 1L)
    let (var_20: bool) =
        if (var_17 >= 0L) then
            let (var_19: int64) = (int64 var_15.Length)
            (var_17 < var_19)
        else
            false
    if var_20 then
        let (var_21: char) = var_15.[int32 var_17]
        let (var_23: bool) =
            if (var_21 = ' ') then
                true
            else
                if (var_21 = '\n') then
                    true
                else
                    (var_21 = '\r')
        let (var_24: int64) = (var_17 + 1L)
        if var_23 then
            method_58((var_0: int64), (var_1: int64), (var_2: int64), (var_3: int64), (var_4: int64), (var_5: int64), (var_6: int64), (var_7: int64), (var_8: int64), (var_9: int64), (var_10: int64), (var_11: int64), (var_12: int64), (var_13: int64), (var_14: int64), (var_15: string), (var_18: int64), (var_24: int64))
        else
            let (var_26: bool) =
                if (var_24 >= 0L) then
                    let (var_25: int64) = (int64 var_15.Length)
                    (var_24 < var_25)
                else
                    false
            if var_26 then
                let (var_27: char) = var_15.[int32 var_24]
                let (var_28: bool) = ('-' = var_27)
                let (var_29: int64) = (var_24 + 1L)
                if var_28 then
                    let (var_30: bool) = false
                    method_59((var_30: bool), (var_0: int64), (var_1: int64), (var_2: int64), (var_3: int64), (var_4: int64), (var_5: int64), (var_6: int64), (var_7: int64), (var_8: int64), (var_9: int64), (var_10: int64), (var_11: int64), (var_12: int64), (var_13: int64), (var_14: int64), (var_15: string), (var_29: int64))
                else
                    let (var_31: bool) = true
                    method_59((var_31: bool), (var_0: int64), (var_1: int64), (var_2: int64), (var_3: int64), (var_4: int64), (var_5: int64), (var_6: int64), (var_7: int64), (var_8: int64), (var_9: int64), (var_10: int64), (var_11: int64), (var_12: int64), (var_13: int64), (var_14: int64), (var_15: string), (var_29: int64))
            else
                let (var_32: bool) = true
                method_59((var_32: bool), (var_0: int64), (var_1: int64), (var_2: int64), (var_3: int64), (var_4: int64), (var_5: int64), (var_6: int64), (var_7: int64), (var_8: int64), (var_9: int64), (var_10: int64), (var_11: int64), (var_12: int64), (var_13: int64), (var_14: int64), (var_15: string), (var_24: int64))
    else
        let (var_34: bool) =
            if (var_17 >= 0L) then
                let (var_33: int64) = (int64 var_15.Length)
                (var_17 < var_33)
            else
                false
        if var_34 then
            let (var_35: char) = var_15.[int32 var_17]
            let (var_36: bool) = ('-' = var_35)
            let (var_37: int64) = (var_17 + 1L)
            if var_36 then
                let (var_38: bool) = false
                method_59((var_38: bool), (var_0: int64), (var_1: int64), (var_2: int64), (var_3: int64), (var_4: int64), (var_5: int64), (var_6: int64), (var_7: int64), (var_8: int64), (var_9: int64), (var_10: int64), (var_11: int64), (var_12: int64), (var_13: int64), (var_14: int64), (var_15: string), (var_37: int64))
            else
                let (var_39: bool) = true
                method_59((var_39: bool), (var_0: int64), (var_1: int64), (var_2: int64), (var_3: int64), (var_4: int64), (var_5: int64), (var_6: int64), (var_7: int64), (var_8: int64), (var_9: int64), (var_10: int64), (var_11: int64), (var_12: int64), (var_13: int64), (var_14: int64), (var_15: string), (var_37: int64))
        else
            let (var_40: bool) = true
            method_59((var_40: bool), (var_0: int64), (var_1: int64), (var_2: int64), (var_3: int64), (var_4: int64), (var_5: int64), (var_6: int64), (var_7: int64), (var_8: int64), (var_9: int64), (var_10: int64), (var_11: int64), (var_12: int64), (var_13: int64), (var_14: int64), (var_15: string), (var_17: int64))
and method_59((var_0: bool), (var_1: int64), (var_2: int64), (var_3: int64), (var_4: int64), (var_5: int64), (var_6: int64), (var_7: int64), (var_8: int64), (var_9: int64), (var_10: int64), (var_11: int64), (var_12: int64), (var_13: int64), (var_14: int64), (var_15: int64), (var_16: string), (var_17: int64)): unit =
    let (var_19: bool) =
        if (var_17 >= 0L) then
            let (var_18: int64) = (int64 var_16.Length)
            (var_17 < var_18)
        else
            false
    if var_19 then
        let (var_20: char) = var_16.[int32 var_17]
        let (var_21: bool) =
            if (var_20 >= '0') then
                (var_20 <= '9')
            else
                false
        let (var_22: int64) = (var_17 + 1L)
        if var_21 then
            let (var_23: int64) = System.Convert.ToInt64(var_20)
            let (var_24: int64) = System.Convert.ToInt64('0')
            let (var_25: int64) = (var_23 - var_24)
            let (var_26: int64) = (0L + var_25)
            method_60((var_0: bool), (var_1: int64), (var_2: int64), (var_3: int64), (var_4: int64), (var_5: int64), (var_6: int64), (var_7: int64), (var_8: int64), (var_9: int64), (var_10: int64), (var_11: int64), (var_12: int64), (var_13: int64), (var_14: int64), (var_15: int64), (var_16: string), (var_26: int64), (var_22: int64))
        else
            (failwith "pint64")
    else
        (failwith "pint64")
and method_60((var_0: bool), (var_1: int64), (var_2: int64), (var_3: int64), (var_4: int64), (var_5: int64), (var_6: int64), (var_7: int64), (var_8: int64), (var_9: int64), (var_10: int64), (var_11: int64), (var_12: int64), (var_13: int64), (var_14: int64), (var_15: int64), (var_16: string), (var_17: int64), (var_18: int64)): unit =
    let (var_20: bool) =
        if (var_18 >= 0L) then
            let (var_19: int64) = (int64 var_16.Length)
            (var_18 < var_19)
        else
            false
    if var_20 then
        let (var_21: char) = var_16.[int32 var_18]
        let (var_22: bool) =
            if (var_21 >= '0') then
                (var_21 <= '9')
            else
                false
        let (var_23: int64) = (var_18 + 1L)
        if var_22 then
            let (var_24: int64) = System.Convert.ToInt64(var_21)
            let (var_25: int64) = System.Convert.ToInt64('0')
            let (var_26: int64) = (var_24 - var_25)
            let (var_27: bool) =
                if (var_17 = 922337203685477580L) then
                    (var_26 <= 7L)
                else
                    false
            let (var_28: bool) =
                if var_27 then
                    true
                else
                    (var_17 < 922337203685477580L)
            if var_28 then
                let (var_29: int64) = (var_17 * 10L)
                let (var_30: int64) = (var_29 + var_26)
                method_60((var_0: bool), (var_1: int64), (var_2: int64), (var_3: int64), (var_4: int64), (var_5: int64), (var_6: int64), (var_7: int64), (var_8: int64), (var_9: int64), (var_10: int64), (var_11: int64), (var_12: int64), (var_13: int64), (var_14: int64), (var_15: int64), (var_16: string), (var_30: int64), (var_23: int64))
            else
                (failwith "integer overflow")
        else
            let (var_31: int64) =
                if var_0 then
                    var_17
                else
                    (-var_17)
            let (var_32: int64) = 0L
            method_61((var_31: int64), (var_1: int64), (var_2: int64), (var_3: int64), (var_4: int64), (var_5: int64), (var_6: int64), (var_7: int64), (var_8: int64), (var_9: int64), (var_10: int64), (var_11: int64), (var_12: int64), (var_13: int64), (var_14: int64), (var_15: int64), (var_16: string), (var_32: int64), (var_23: int64))
    else
        let (var_33: int64) =
            if var_0 then
                var_17
            else
                (-var_17)
        let (var_34: int64) = 0L
        method_61((var_33: int64), (var_1: int64), (var_2: int64), (var_3: int64), (var_4: int64), (var_5: int64), (var_6: int64), (var_7: int64), (var_8: int64), (var_9: int64), (var_10: int64), (var_11: int64), (var_12: int64), (var_13: int64), (var_14: int64), (var_15: int64), (var_16: string), (var_34: int64), (var_18: int64))
and method_61((var_0: int64), (var_1: int64), (var_2: int64), (var_3: int64), (var_4: int64), (var_5: int64), (var_6: int64), (var_7: int64), (var_8: int64), (var_9: int64), (var_10: int64), (var_11: int64), (var_12: int64), (var_13: int64), (var_14: int64), (var_15: int64), (var_16: string), (var_17: int64), (var_18: int64)): unit =
    let (var_19: int64) = (var_17 + 1L)
    let (var_21: bool) =
        if (var_18 >= 0L) then
            let (var_20: int64) = (int64 var_16.Length)
            (var_18 < var_20)
        else
            false
    if var_21 then
        let (var_22: char) = var_16.[int32 var_18]
        let (var_24: bool) =
            if (var_22 = ' ') then
                true
            else
                if (var_22 = '\n') then
                    true
                else
                    (var_22 = '\r')
        let (var_25: int64) = (var_18 + 1L)
        if var_24 then
            method_61((var_0: int64), (var_1: int64), (var_2: int64), (var_3: int64), (var_4: int64), (var_5: int64), (var_6: int64), (var_7: int64), (var_8: int64), (var_9: int64), (var_10: int64), (var_11: int64), (var_12: int64), (var_13: int64), (var_14: int64), (var_15: int64), (var_16: string), (var_19: int64), (var_25: int64))
        else
            let (var_27: bool) =
                if (var_25 >= 0L) then
                    let (var_26: int64) = (int64 var_16.Length)
                    (var_25 < var_26)
                else
                    false
            if var_27 then
                let (var_28: char) = var_16.[int32 var_25]
                let (var_29: bool) = ('-' = var_28)
                let (var_30: int64) = (var_25 + 1L)
                if var_29 then
                    let (var_31: bool) = false
                    method_62((var_31: bool), (var_0: int64), (var_1: int64), (var_2: int64), (var_3: int64), (var_4: int64), (var_5: int64), (var_6: int64), (var_7: int64), (var_8: int64), (var_9: int64), (var_10: int64), (var_11: int64), (var_12: int64), (var_13: int64), (var_14: int64), (var_15: int64), (var_16: string), (var_30: int64))
                else
                    let (var_32: bool) = true
                    method_62((var_32: bool), (var_0: int64), (var_1: int64), (var_2: int64), (var_3: int64), (var_4: int64), (var_5: int64), (var_6: int64), (var_7: int64), (var_8: int64), (var_9: int64), (var_10: int64), (var_11: int64), (var_12: int64), (var_13: int64), (var_14: int64), (var_15: int64), (var_16: string), (var_30: int64))
            else
                let (var_33: bool) = true
                method_62((var_33: bool), (var_0: int64), (var_1: int64), (var_2: int64), (var_3: int64), (var_4: int64), (var_5: int64), (var_6: int64), (var_7: int64), (var_8: int64), (var_9: int64), (var_10: int64), (var_11: int64), (var_12: int64), (var_13: int64), (var_14: int64), (var_15: int64), (var_16: string), (var_25: int64))
    else
        let (var_35: bool) =
            if (var_18 >= 0L) then
                let (var_34: int64) = (int64 var_16.Length)
                (var_18 < var_34)
            else
                false
        if var_35 then
            let (var_36: char) = var_16.[int32 var_18]
            let (var_37: bool) = ('-' = var_36)
            let (var_38: int64) = (var_18 + 1L)
            if var_37 then
                let (var_39: bool) = false
                method_62((var_39: bool), (var_0: int64), (var_1: int64), (var_2: int64), (var_3: int64), (var_4: int64), (var_5: int64), (var_6: int64), (var_7: int64), (var_8: int64), (var_9: int64), (var_10: int64), (var_11: int64), (var_12: int64), (var_13: int64), (var_14: int64), (var_15: int64), (var_16: string), (var_38: int64))
            else
                let (var_40: bool) = true
                method_62((var_40: bool), (var_0: int64), (var_1: int64), (var_2: int64), (var_3: int64), (var_4: int64), (var_5: int64), (var_6: int64), (var_7: int64), (var_8: int64), (var_9: int64), (var_10: int64), (var_11: int64), (var_12: int64), (var_13: int64), (var_14: int64), (var_15: int64), (var_16: string), (var_38: int64))
        else
            let (var_41: bool) = true
            method_62((var_41: bool), (var_0: int64), (var_1: int64), (var_2: int64), (var_3: int64), (var_4: int64), (var_5: int64), (var_6: int64), (var_7: int64), (var_8: int64), (var_9: int64), (var_10: int64), (var_11: int64), (var_12: int64), (var_13: int64), (var_14: int64), (var_15: int64), (var_16: string), (var_18: int64))
and method_62((var_0: bool), (var_1: int64), (var_2: int64), (var_3: int64), (var_4: int64), (var_5: int64), (var_6: int64), (var_7: int64), (var_8: int64), (var_9: int64), (var_10: int64), (var_11: int64), (var_12: int64), (var_13: int64), (var_14: int64), (var_15: int64), (var_16: int64), (var_17: string), (var_18: int64)): unit =
    let (var_20: bool) =
        if (var_18 >= 0L) then
            let (var_19: int64) = (int64 var_17.Length)
            (var_18 < var_19)
        else
            false
    if var_20 then
        let (var_21: char) = var_17.[int32 var_18]
        let (var_22: bool) =
            if (var_21 >= '0') then
                (var_21 <= '9')
            else
                false
        let (var_23: int64) = (var_18 + 1L)
        if var_22 then
            let (var_24: int64) = System.Convert.ToInt64(var_21)
            let (var_25: int64) = System.Convert.ToInt64('0')
            let (var_26: int64) = (var_24 - var_25)
            let (var_27: int64) = (0L + var_26)
            method_63((var_0: bool), (var_1: int64), (var_2: int64), (var_3: int64), (var_4: int64), (var_5: int64), (var_6: int64), (var_7: int64), (var_8: int64), (var_9: int64), (var_10: int64), (var_11: int64), (var_12: int64), (var_13: int64), (var_14: int64), (var_15: int64), (var_16: int64), (var_17: string), (var_27: int64), (var_23: int64))
        else
            (failwith "pint64")
    else
        (failwith "pint64")
and method_63((var_0: bool), (var_1: int64), (var_2: int64), (var_3: int64), (var_4: int64), (var_5: int64), (var_6: int64), (var_7: int64), (var_8: int64), (var_9: int64), (var_10: int64), (var_11: int64), (var_12: int64), (var_13: int64), (var_14: int64), (var_15: int64), (var_16: int64), (var_17: string), (var_18: int64), (var_19: int64)): unit =
    let (var_21: bool) =
        if (var_19 >= 0L) then
            let (var_20: int64) = (int64 var_17.Length)
            (var_19 < var_20)
        else
            false
    if var_21 then
        let (var_22: char) = var_17.[int32 var_19]
        let (var_23: bool) =
            if (var_22 >= '0') then
                (var_22 <= '9')
            else
                false
        let (var_24: int64) = (var_19 + 1L)
        if var_23 then
            let (var_25: int64) = System.Convert.ToInt64(var_22)
            let (var_26: int64) = System.Convert.ToInt64('0')
            let (var_27: int64) = (var_25 - var_26)
            let (var_28: bool) =
                if (var_18 = 922337203685477580L) then
                    (var_27 <= 7L)
                else
                    false
            let (var_29: bool) =
                if var_28 then
                    true
                else
                    (var_18 < 922337203685477580L)
            if var_29 then
                let (var_30: int64) = (var_18 * 10L)
                let (var_31: int64) = (var_30 + var_27)
                method_63((var_0: bool), (var_1: int64), (var_2: int64), (var_3: int64), (var_4: int64), (var_5: int64), (var_6: int64), (var_7: int64), (var_8: int64), (var_9: int64), (var_10: int64), (var_11: int64), (var_12: int64), (var_13: int64), (var_14: int64), (var_15: int64), (var_16: int64), (var_17: string), (var_31: int64), (var_24: int64))
            else
                (failwith "integer overflow")
        else
            let (var_32: int64) =
                if var_0 then
                    var_18
                else
                    (-var_18)
            let (var_33: int64) = 0L
            method_64((var_32: int64), (var_1: int64), (var_2: int64), (var_3: int64), (var_4: int64), (var_5: int64), (var_6: int64), (var_7: int64), (var_8: int64), (var_9: int64), (var_10: int64), (var_11: int64), (var_12: int64), (var_13: int64), (var_14: int64), (var_15: int64), (var_16: int64), (var_17: string), (var_33: int64), (var_24: int64))
    else
        let (var_34: int64) =
            if var_0 then
                var_18
            else
                (-var_18)
        let (var_35: int64) = 0L
        method_64((var_34: int64), (var_1: int64), (var_2: int64), (var_3: int64), (var_4: int64), (var_5: int64), (var_6: int64), (var_7: int64), (var_8: int64), (var_9: int64), (var_10: int64), (var_11: int64), (var_12: int64), (var_13: int64), (var_14: int64), (var_15: int64), (var_16: int64), (var_17: string), (var_35: int64), (var_19: int64))
and method_64((var_0: int64), (var_1: int64), (var_2: int64), (var_3: int64), (var_4: int64), (var_5: int64), (var_6: int64), (var_7: int64), (var_8: int64), (var_9: int64), (var_10: int64), (var_11: int64), (var_12: int64), (var_13: int64), (var_14: int64), (var_15: int64), (var_16: int64), (var_17: string), (var_18: int64), (var_19: int64)): unit =
    let (var_20: int64) = (var_18 + 1L)
    let (var_22: bool) =
        if (var_19 >= 0L) then
            let (var_21: int64) = (int64 var_17.Length)
            (var_19 < var_21)
        else
            false
    if var_22 then
        let (var_23: char) = var_17.[int32 var_19]
        let (var_25: bool) =
            if (var_23 = ' ') then
                true
            else
                if (var_23 = '\n') then
                    true
                else
                    (var_23 = '\r')
        let (var_26: int64) = (var_19 + 1L)
        if var_25 then
            method_64((var_0: int64), (var_1: int64), (var_2: int64), (var_3: int64), (var_4: int64), (var_5: int64), (var_6: int64), (var_7: int64), (var_8: int64), (var_9: int64), (var_10: int64), (var_11: int64), (var_12: int64), (var_13: int64), (var_14: int64), (var_15: int64), (var_16: int64), (var_17: string), (var_20: int64), (var_26: int64))
        else
            let (var_28: bool) =
                if (var_26 >= 0L) then
                    let (var_27: int64) = (int64 var_17.Length)
                    (var_26 < var_27)
                else
                    false
            if var_28 then
                let (var_29: char) = var_17.[int32 var_26]
                let (var_30: bool) = ('-' = var_29)
                let (var_31: int64) = (var_26 + 1L)
                if var_30 then
                    let (var_32: bool) = false
                    method_65((var_32: bool), (var_0: int64), (var_1: int64), (var_2: int64), (var_3: int64), (var_4: int64), (var_5: int64), (var_6: int64), (var_7: int64), (var_8: int64), (var_9: int64), (var_10: int64), (var_11: int64), (var_12: int64), (var_13: int64), (var_14: int64), (var_15: int64), (var_16: int64), (var_17: string), (var_31: int64))
                else
                    let (var_33: bool) = true
                    method_65((var_33: bool), (var_0: int64), (var_1: int64), (var_2: int64), (var_3: int64), (var_4: int64), (var_5: int64), (var_6: int64), (var_7: int64), (var_8: int64), (var_9: int64), (var_10: int64), (var_11: int64), (var_12: int64), (var_13: int64), (var_14: int64), (var_15: int64), (var_16: int64), (var_17: string), (var_31: int64))
            else
                let (var_34: bool) = true
                method_65((var_34: bool), (var_0: int64), (var_1: int64), (var_2: int64), (var_3: int64), (var_4: int64), (var_5: int64), (var_6: int64), (var_7: int64), (var_8: int64), (var_9: int64), (var_10: int64), (var_11: int64), (var_12: int64), (var_13: int64), (var_14: int64), (var_15: int64), (var_16: int64), (var_17: string), (var_26: int64))
    else
        let (var_36: bool) =
            if (var_19 >= 0L) then
                let (var_35: int64) = (int64 var_17.Length)
                (var_19 < var_35)
            else
                false
        if var_36 then
            let (var_37: char) = var_17.[int32 var_19]
            let (var_38: bool) = ('-' = var_37)
            let (var_39: int64) = (var_19 + 1L)
            if var_38 then
                let (var_40: bool) = false
                method_65((var_40: bool), (var_0: int64), (var_1: int64), (var_2: int64), (var_3: int64), (var_4: int64), (var_5: int64), (var_6: int64), (var_7: int64), (var_8: int64), (var_9: int64), (var_10: int64), (var_11: int64), (var_12: int64), (var_13: int64), (var_14: int64), (var_15: int64), (var_16: int64), (var_17: string), (var_39: int64))
            else
                let (var_41: bool) = true
                method_65((var_41: bool), (var_0: int64), (var_1: int64), (var_2: int64), (var_3: int64), (var_4: int64), (var_5: int64), (var_6: int64), (var_7: int64), (var_8: int64), (var_9: int64), (var_10: int64), (var_11: int64), (var_12: int64), (var_13: int64), (var_14: int64), (var_15: int64), (var_16: int64), (var_17: string), (var_39: int64))
        else
            let (var_42: bool) = true
            method_65((var_42: bool), (var_0: int64), (var_1: int64), (var_2: int64), (var_3: int64), (var_4: int64), (var_5: int64), (var_6: int64), (var_7: int64), (var_8: int64), (var_9: int64), (var_10: int64), (var_11: int64), (var_12: int64), (var_13: int64), (var_14: int64), (var_15: int64), (var_16: int64), (var_17: string), (var_19: int64))
and method_65((var_0: bool), (var_1: int64), (var_2: int64), (var_3: int64), (var_4: int64), (var_5: int64), (var_6: int64), (var_7: int64), (var_8: int64), (var_9: int64), (var_10: int64), (var_11: int64), (var_12: int64), (var_13: int64), (var_14: int64), (var_15: int64), (var_16: int64), (var_17: int64), (var_18: string), (var_19: int64)): unit =
    let (var_21: bool) =
        if (var_19 >= 0L) then
            let (var_20: int64) = (int64 var_18.Length)
            (var_19 < var_20)
        else
            false
    if var_21 then
        let (var_22: char) = var_18.[int32 var_19]
        let (var_23: bool) =
            if (var_22 >= '0') then
                (var_22 <= '9')
            else
                false
        let (var_24: int64) = (var_19 + 1L)
        if var_23 then
            let (var_25: int64) = System.Convert.ToInt64(var_22)
            let (var_26: int64) = System.Convert.ToInt64('0')
            let (var_27: int64) = (var_25 - var_26)
            let (var_28: int64) = (0L + var_27)
            method_66((var_0: bool), (var_1: int64), (var_2: int64), (var_3: int64), (var_4: int64), (var_5: int64), (var_6: int64), (var_7: int64), (var_8: int64), (var_9: int64), (var_10: int64), (var_11: int64), (var_12: int64), (var_13: int64), (var_14: int64), (var_15: int64), (var_16: int64), (var_17: int64), (var_18: string), (var_28: int64), (var_24: int64))
        else
            (failwith "pint64")
    else
        (failwith "pint64")
and method_66((var_0: bool), (var_1: int64), (var_2: int64), (var_3: int64), (var_4: int64), (var_5: int64), (var_6: int64), (var_7: int64), (var_8: int64), (var_9: int64), (var_10: int64), (var_11: int64), (var_12: int64), (var_13: int64), (var_14: int64), (var_15: int64), (var_16: int64), (var_17: int64), (var_18: string), (var_19: int64), (var_20: int64)): unit =
    let (var_22: bool) =
        if (var_20 >= 0L) then
            let (var_21: int64) = (int64 var_18.Length)
            (var_20 < var_21)
        else
            false
    if var_22 then
        let (var_23: char) = var_18.[int32 var_20]
        let (var_24: bool) =
            if (var_23 >= '0') then
                (var_23 <= '9')
            else
                false
        let (var_25: int64) = (var_20 + 1L)
        if var_24 then
            let (var_26: int64) = System.Convert.ToInt64(var_23)
            let (var_27: int64) = System.Convert.ToInt64('0')
            let (var_28: int64) = (var_26 - var_27)
            let (var_29: bool) =
                if (var_19 = 922337203685477580L) then
                    (var_28 <= 7L)
                else
                    false
            let (var_30: bool) =
                if var_29 then
                    true
                else
                    (var_19 < 922337203685477580L)
            if var_30 then
                let (var_31: int64) = (var_19 * 10L)
                let (var_32: int64) = (var_31 + var_28)
                method_66((var_0: bool), (var_1: int64), (var_2: int64), (var_3: int64), (var_4: int64), (var_5: int64), (var_6: int64), (var_7: int64), (var_8: int64), (var_9: int64), (var_10: int64), (var_11: int64), (var_12: int64), (var_13: int64), (var_14: int64), (var_15: int64), (var_16: int64), (var_17: int64), (var_18: string), (var_32: int64), (var_25: int64))
            else
                (failwith "integer overflow")
        else
            let (var_33: int64) =
                if var_0 then
                    var_19
                else
                    (-var_19)
            let (var_34: int64) = 0L
            method_67((var_33: int64), (var_1: int64), (var_2: int64), (var_3: int64), (var_4: int64), (var_5: int64), (var_6: int64), (var_7: int64), (var_8: int64), (var_9: int64), (var_10: int64), (var_11: int64), (var_12: int64), (var_13: int64), (var_14: int64), (var_15: int64), (var_16: int64), (var_17: int64), (var_18: string), (var_34: int64), (var_25: int64))
    else
        let (var_35: int64) =
            if var_0 then
                var_19
            else
                (-var_19)
        let (var_36: int64) = 0L
        method_67((var_35: int64), (var_1: int64), (var_2: int64), (var_3: int64), (var_4: int64), (var_5: int64), (var_6: int64), (var_7: int64), (var_8: int64), (var_9: int64), (var_10: int64), (var_11: int64), (var_12: int64), (var_13: int64), (var_14: int64), (var_15: int64), (var_16: int64), (var_17: int64), (var_18: string), (var_36: int64), (var_20: int64))
and method_67((var_0: int64), (var_1: int64), (var_2: int64), (var_3: int64), (var_4: int64), (var_5: int64), (var_6: int64), (var_7: int64), (var_8: int64), (var_9: int64), (var_10: int64), (var_11: int64), (var_12: int64), (var_13: int64), (var_14: int64), (var_15: int64), (var_16: int64), (var_17: int64), (var_18: string), (var_19: int64), (var_20: int64)): unit =
    let (var_21: int64) = (var_19 + 1L)
    let (var_23: bool) =
        if (var_20 >= 0L) then
            let (var_22: int64) = (int64 var_18.Length)
            (var_20 < var_22)
        else
            false
    if var_23 then
        let (var_24: char) = var_18.[int32 var_20]
        let (var_26: bool) =
            if (var_24 = ' ') then
                true
            else
                if (var_24 = '\n') then
                    true
                else
                    (var_24 = '\r')
        let (var_27: int64) = (var_20 + 1L)
        if var_26 then
            method_67((var_0: int64), (var_1: int64), (var_2: int64), (var_3: int64), (var_4: int64), (var_5: int64), (var_6: int64), (var_7: int64), (var_8: int64), (var_9: int64), (var_10: int64), (var_11: int64), (var_12: int64), (var_13: int64), (var_14: int64), (var_15: int64), (var_16: int64), (var_17: int64), (var_18: string), (var_21: int64), (var_27: int64))
        else
            let (var_29: bool) =
                if (var_27 >= 0L) then
                    let (var_28: int64) = (int64 var_18.Length)
                    (var_27 < var_28)
                else
                    false
            if var_29 then
                let (var_30: char) = var_18.[int32 var_27]
                let (var_31: bool) = ('-' = var_30)
                let (var_32: int64) = (var_27 + 1L)
                if var_31 then
                    let (var_33: bool) = false
                    method_68((var_33: bool), (var_0: int64), (var_1: int64), (var_2: int64), (var_3: int64), (var_4: int64), (var_5: int64), (var_6: int64), (var_7: int64), (var_8: int64), (var_9: int64), (var_10: int64), (var_11: int64), (var_12: int64), (var_13: int64), (var_14: int64), (var_15: int64), (var_16: int64), (var_17: int64), (var_18: string), (var_32: int64))
                else
                    let (var_34: bool) = true
                    method_68((var_34: bool), (var_0: int64), (var_1: int64), (var_2: int64), (var_3: int64), (var_4: int64), (var_5: int64), (var_6: int64), (var_7: int64), (var_8: int64), (var_9: int64), (var_10: int64), (var_11: int64), (var_12: int64), (var_13: int64), (var_14: int64), (var_15: int64), (var_16: int64), (var_17: int64), (var_18: string), (var_32: int64))
            else
                let (var_35: bool) = true
                method_68((var_35: bool), (var_0: int64), (var_1: int64), (var_2: int64), (var_3: int64), (var_4: int64), (var_5: int64), (var_6: int64), (var_7: int64), (var_8: int64), (var_9: int64), (var_10: int64), (var_11: int64), (var_12: int64), (var_13: int64), (var_14: int64), (var_15: int64), (var_16: int64), (var_17: int64), (var_18: string), (var_27: int64))
    else
        let (var_37: bool) =
            if (var_20 >= 0L) then
                let (var_36: int64) = (int64 var_18.Length)
                (var_20 < var_36)
            else
                false
        if var_37 then
            let (var_38: char) = var_18.[int32 var_20]
            let (var_39: bool) = ('-' = var_38)
            let (var_40: int64) = (var_20 + 1L)
            if var_39 then
                let (var_41: bool) = false
                method_68((var_41: bool), (var_0: int64), (var_1: int64), (var_2: int64), (var_3: int64), (var_4: int64), (var_5: int64), (var_6: int64), (var_7: int64), (var_8: int64), (var_9: int64), (var_10: int64), (var_11: int64), (var_12: int64), (var_13: int64), (var_14: int64), (var_15: int64), (var_16: int64), (var_17: int64), (var_18: string), (var_40: int64))
            else
                let (var_42: bool) = true
                method_68((var_42: bool), (var_0: int64), (var_1: int64), (var_2: int64), (var_3: int64), (var_4: int64), (var_5: int64), (var_6: int64), (var_7: int64), (var_8: int64), (var_9: int64), (var_10: int64), (var_11: int64), (var_12: int64), (var_13: int64), (var_14: int64), (var_15: int64), (var_16: int64), (var_17: int64), (var_18: string), (var_40: int64))
        else
            let (var_43: bool) = true
            method_68((var_43: bool), (var_0: int64), (var_1: int64), (var_2: int64), (var_3: int64), (var_4: int64), (var_5: int64), (var_6: int64), (var_7: int64), (var_8: int64), (var_9: int64), (var_10: int64), (var_11: int64), (var_12: int64), (var_13: int64), (var_14: int64), (var_15: int64), (var_16: int64), (var_17: int64), (var_18: string), (var_20: int64))
and method_68((var_0: bool), (var_1: int64), (var_2: int64), (var_3: int64), (var_4: int64), (var_5: int64), (var_6: int64), (var_7: int64), (var_8: int64), (var_9: int64), (var_10: int64), (var_11: int64), (var_12: int64), (var_13: int64), (var_14: int64), (var_15: int64), (var_16: int64), (var_17: int64), (var_18: int64), (var_19: string), (var_20: int64)): unit =
    let (var_22: bool) =
        if (var_20 >= 0L) then
            let (var_21: int64) = (int64 var_19.Length)
            (var_20 < var_21)
        else
            false
    if var_22 then
        let (var_23: char) = var_19.[int32 var_20]
        let (var_24: bool) =
            if (var_23 >= '0') then
                (var_23 <= '9')
            else
                false
        let (var_25: int64) = (var_20 + 1L)
        if var_24 then
            let (var_26: int64) = System.Convert.ToInt64(var_23)
            let (var_27: int64) = System.Convert.ToInt64('0')
            let (var_28: int64) = (var_26 - var_27)
            let (var_29: int64) = (0L + var_28)
            method_69((var_0: bool), (var_1: int64), (var_2: int64), (var_3: int64), (var_4: int64), (var_5: int64), (var_6: int64), (var_7: int64), (var_8: int64), (var_9: int64), (var_10: int64), (var_11: int64), (var_12: int64), (var_13: int64), (var_14: int64), (var_15: int64), (var_16: int64), (var_17: int64), (var_18: int64), (var_19: string), (var_29: int64), (var_25: int64))
        else
            (failwith "pint64")
    else
        (failwith "pint64")
and method_69((var_0: bool), (var_1: int64), (var_2: int64), (var_3: int64), (var_4: int64), (var_5: int64), (var_6: int64), (var_7: int64), (var_8: int64), (var_9: int64), (var_10: int64), (var_11: int64), (var_12: int64), (var_13: int64), (var_14: int64), (var_15: int64), (var_16: int64), (var_17: int64), (var_18: int64), (var_19: string), (var_20: int64), (var_21: int64)): unit =
    let (var_23: bool) =
        if (var_21 >= 0L) then
            let (var_22: int64) = (int64 var_19.Length)
            (var_21 < var_22)
        else
            false
    if var_23 then
        let (var_24: char) = var_19.[int32 var_21]
        let (var_25: bool) =
            if (var_24 >= '0') then
                (var_24 <= '9')
            else
                false
        let (var_26: int64) = (var_21 + 1L)
        if var_25 then
            let (var_27: int64) = System.Convert.ToInt64(var_24)
            let (var_28: int64) = System.Convert.ToInt64('0')
            let (var_29: int64) = (var_27 - var_28)
            let (var_30: bool) =
                if (var_20 = 922337203685477580L) then
                    (var_29 <= 7L)
                else
                    false
            let (var_31: bool) =
                if var_30 then
                    true
                else
                    (var_20 < 922337203685477580L)
            if var_31 then
                let (var_32: int64) = (var_20 * 10L)
                let (var_33: int64) = (var_32 + var_29)
                method_69((var_0: bool), (var_1: int64), (var_2: int64), (var_3: int64), (var_4: int64), (var_5: int64), (var_6: int64), (var_7: int64), (var_8: int64), (var_9: int64), (var_10: int64), (var_11: int64), (var_12: int64), (var_13: int64), (var_14: int64), (var_15: int64), (var_16: int64), (var_17: int64), (var_18: int64), (var_19: string), (var_33: int64), (var_26: int64))
            else
                (failwith "integer overflow")
        else
            let (var_34: int64) =
                if var_0 then
                    var_20
                else
                    (-var_20)
            let (var_35: int64) = 0L
            method_70((var_34: int64), (var_1: int64), (var_2: int64), (var_3: int64), (var_4: int64), (var_5: int64), (var_6: int64), (var_7: int64), (var_8: int64), (var_9: int64), (var_10: int64), (var_11: int64), (var_12: int64), (var_13: int64), (var_14: int64), (var_15: int64), (var_16: int64), (var_17: int64), (var_18: int64), (var_19: string), (var_35: int64), (var_26: int64))
    else
        let (var_36: int64) =
            if var_0 then
                var_20
            else
                (-var_20)
        let (var_37: int64) = 0L
        method_70((var_36: int64), (var_1: int64), (var_2: int64), (var_3: int64), (var_4: int64), (var_5: int64), (var_6: int64), (var_7: int64), (var_8: int64), (var_9: int64), (var_10: int64), (var_11: int64), (var_12: int64), (var_13: int64), (var_14: int64), (var_15: int64), (var_16: int64), (var_17: int64), (var_18: int64), (var_19: string), (var_37: int64), (var_21: int64))
and method_70((var_0: int64), (var_1: int64), (var_2: int64), (var_3: int64), (var_4: int64), (var_5: int64), (var_6: int64), (var_7: int64), (var_8: int64), (var_9: int64), (var_10: int64), (var_11: int64), (var_12: int64), (var_13: int64), (var_14: int64), (var_15: int64), (var_16: int64), (var_17: int64), (var_18: int64), (var_19: string), (var_20: int64), (var_21: int64)): unit =
    let (var_22: int64) = (var_20 + 1L)
    let (var_24: bool) =
        if (var_21 >= 0L) then
            let (var_23: int64) = (int64 var_19.Length)
            (var_21 < var_23)
        else
            false
    if var_24 then
        let (var_25: char) = var_19.[int32 var_21]
        let (var_27: bool) =
            if (var_25 = ' ') then
                true
            else
                if (var_25 = '\n') then
                    true
                else
                    (var_25 = '\r')
        let (var_28: int64) = (var_21 + 1L)
        if var_27 then
            method_70((var_0: int64), (var_1: int64), (var_2: int64), (var_3: int64), (var_4: int64), (var_5: int64), (var_6: int64), (var_7: int64), (var_8: int64), (var_9: int64), (var_10: int64), (var_11: int64), (var_12: int64), (var_13: int64), (var_14: int64), (var_15: int64), (var_16: int64), (var_17: int64), (var_18: int64), (var_19: string), (var_22: int64), (var_28: int64))
        else
            let (var_30: bool) =
                if (var_28 >= 0L) then
                    let (var_29: int64) = (int64 var_19.Length)
                    (var_28 < var_29)
                else
                    false
            if var_30 then
                let (var_31: char) = var_19.[int32 var_28]
                let (var_32: bool) = ('-' = var_31)
                let (var_33: int64) = (var_28 + 1L)
                if var_32 then
                    let (var_34: bool) = false
                    method_71((var_34: bool), (var_0: int64), (var_1: int64), (var_2: int64), (var_3: int64), (var_4: int64), (var_5: int64), (var_6: int64), (var_7: int64), (var_8: int64), (var_9: int64), (var_10: int64), (var_11: int64), (var_12: int64), (var_13: int64), (var_14: int64), (var_15: int64), (var_16: int64), (var_17: int64), (var_18: int64), (var_19: string), (var_33: int64))
                else
                    let (var_35: bool) = true
                    method_71((var_35: bool), (var_0: int64), (var_1: int64), (var_2: int64), (var_3: int64), (var_4: int64), (var_5: int64), (var_6: int64), (var_7: int64), (var_8: int64), (var_9: int64), (var_10: int64), (var_11: int64), (var_12: int64), (var_13: int64), (var_14: int64), (var_15: int64), (var_16: int64), (var_17: int64), (var_18: int64), (var_19: string), (var_33: int64))
            else
                let (var_36: bool) = true
                method_71((var_36: bool), (var_0: int64), (var_1: int64), (var_2: int64), (var_3: int64), (var_4: int64), (var_5: int64), (var_6: int64), (var_7: int64), (var_8: int64), (var_9: int64), (var_10: int64), (var_11: int64), (var_12: int64), (var_13: int64), (var_14: int64), (var_15: int64), (var_16: int64), (var_17: int64), (var_18: int64), (var_19: string), (var_28: int64))
    else
        let (var_38: bool) =
            if (var_21 >= 0L) then
                let (var_37: int64) = (int64 var_19.Length)
                (var_21 < var_37)
            else
                false
        if var_38 then
            let (var_39: char) = var_19.[int32 var_21]
            let (var_40: bool) = ('-' = var_39)
            let (var_41: int64) = (var_21 + 1L)
            if var_40 then
                let (var_42: bool) = false
                method_71((var_42: bool), (var_0: int64), (var_1: int64), (var_2: int64), (var_3: int64), (var_4: int64), (var_5: int64), (var_6: int64), (var_7: int64), (var_8: int64), (var_9: int64), (var_10: int64), (var_11: int64), (var_12: int64), (var_13: int64), (var_14: int64), (var_15: int64), (var_16: int64), (var_17: int64), (var_18: int64), (var_19: string), (var_41: int64))
            else
                let (var_43: bool) = true
                method_71((var_43: bool), (var_0: int64), (var_1: int64), (var_2: int64), (var_3: int64), (var_4: int64), (var_5: int64), (var_6: int64), (var_7: int64), (var_8: int64), (var_9: int64), (var_10: int64), (var_11: int64), (var_12: int64), (var_13: int64), (var_14: int64), (var_15: int64), (var_16: int64), (var_17: int64), (var_18: int64), (var_19: string), (var_41: int64))
        else
            let (var_44: bool) = true
            method_71((var_44: bool), (var_0: int64), (var_1: int64), (var_2: int64), (var_3: int64), (var_4: int64), (var_5: int64), (var_6: int64), (var_7: int64), (var_8: int64), (var_9: int64), (var_10: int64), (var_11: int64), (var_12: int64), (var_13: int64), (var_14: int64), (var_15: int64), (var_16: int64), (var_17: int64), (var_18: int64), (var_19: string), (var_21: int64))
and method_71((var_0: bool), (var_1: int64), (var_2: int64), (var_3: int64), (var_4: int64), (var_5: int64), (var_6: int64), (var_7: int64), (var_8: int64), (var_9: int64), (var_10: int64), (var_11: int64), (var_12: int64), (var_13: int64), (var_14: int64), (var_15: int64), (var_16: int64), (var_17: int64), (var_18: int64), (var_19: int64), (var_20: string), (var_21: int64)): unit =
    let (var_23: bool) =
        if (var_21 >= 0L) then
            let (var_22: int64) = (int64 var_20.Length)
            (var_21 < var_22)
        else
            false
    if var_23 then
        let (var_24: char) = var_20.[int32 var_21]
        let (var_25: bool) =
            if (var_24 >= '0') then
                (var_24 <= '9')
            else
                false
        let (var_26: int64) = (var_21 + 1L)
        if var_25 then
            let (var_27: int64) = System.Convert.ToInt64(var_24)
            let (var_28: int64) = System.Convert.ToInt64('0')
            let (var_29: int64) = (var_27 - var_28)
            let (var_30: int64) = (0L + var_29)
            method_72((var_0: bool), (var_1: int64), (var_2: int64), (var_3: int64), (var_4: int64), (var_5: int64), (var_6: int64), (var_7: int64), (var_8: int64), (var_9: int64), (var_10: int64), (var_11: int64), (var_12: int64), (var_13: int64), (var_14: int64), (var_15: int64), (var_16: int64), (var_17: int64), (var_18: int64), (var_19: int64), (var_20: string), (var_30: int64), (var_26: int64))
        else
            (failwith "pint64")
    else
        (failwith "pint64")
and method_72((var_0: bool), (var_1: int64), (var_2: int64), (var_3: int64), (var_4: int64), (var_5: int64), (var_6: int64), (var_7: int64), (var_8: int64), (var_9: int64), (var_10: int64), (var_11: int64), (var_12: int64), (var_13: int64), (var_14: int64), (var_15: int64), (var_16: int64), (var_17: int64), (var_18: int64), (var_19: int64), (var_20: string), (var_21: int64), (var_22: int64)): unit =
    let (var_24: bool) =
        if (var_22 >= 0L) then
            let (var_23: int64) = (int64 var_20.Length)
            (var_22 < var_23)
        else
            false
    if var_24 then
        let (var_25: char) = var_20.[int32 var_22]
        let (var_26: bool) =
            if (var_25 >= '0') then
                (var_25 <= '9')
            else
                false
        let (var_27: int64) = (var_22 + 1L)
        if var_26 then
            let (var_28: int64) = System.Convert.ToInt64(var_25)
            let (var_29: int64) = System.Convert.ToInt64('0')
            let (var_30: int64) = (var_28 - var_29)
            let (var_31: bool) =
                if (var_21 = 922337203685477580L) then
                    (var_30 <= 7L)
                else
                    false
            let (var_32: bool) =
                if var_31 then
                    true
                else
                    (var_21 < 922337203685477580L)
            if var_32 then
                let (var_33: int64) = (var_21 * 10L)
                let (var_34: int64) = (var_33 + var_30)
                method_72((var_0: bool), (var_1: int64), (var_2: int64), (var_3: int64), (var_4: int64), (var_5: int64), (var_6: int64), (var_7: int64), (var_8: int64), (var_9: int64), (var_10: int64), (var_11: int64), (var_12: int64), (var_13: int64), (var_14: int64), (var_15: int64), (var_16: int64), (var_17: int64), (var_18: int64), (var_19: int64), (var_20: string), (var_34: int64), (var_27: int64))
            else
                (failwith "integer overflow")
        else
            let (var_35: int64) =
                if var_0 then
                    var_21
                else
                    (-var_21)
            let (var_36: int64) = 0L
            method_73((var_35: int64), (var_1: int64), (var_2: int64), (var_3: int64), (var_4: int64), (var_5: int64), (var_6: int64), (var_7: int64), (var_8: int64), (var_9: int64), (var_10: int64), (var_11: int64), (var_12: int64), (var_13: int64), (var_14: int64), (var_15: int64), (var_16: int64), (var_17: int64), (var_18: int64), (var_19: int64), (var_20: string), (var_36: int64), (var_27: int64))
    else
        let (var_37: int64) =
            if var_0 then
                var_21
            else
                (-var_21)
        let (var_38: int64) = 0L
        method_73((var_37: int64), (var_1: int64), (var_2: int64), (var_3: int64), (var_4: int64), (var_5: int64), (var_6: int64), (var_7: int64), (var_8: int64), (var_9: int64), (var_10: int64), (var_11: int64), (var_12: int64), (var_13: int64), (var_14: int64), (var_15: int64), (var_16: int64), (var_17: int64), (var_18: int64), (var_19: int64), (var_20: string), (var_38: int64), (var_22: int64))
and method_73((var_0: int64), (var_1: int64), (var_2: int64), (var_3: int64), (var_4: int64), (var_5: int64), (var_6: int64), (var_7: int64), (var_8: int64), (var_9: int64), (var_10: int64), (var_11: int64), (var_12: int64), (var_13: int64), (var_14: int64), (var_15: int64), (var_16: int64), (var_17: int64), (var_18: int64), (var_19: int64), (var_20: string), (var_21: int64), (var_22: int64)): unit =
    let (var_23: int64) = (var_21 + 1L)
    let (var_25: bool) =
        if (var_22 >= 0L) then
            let (var_24: int64) = (int64 var_20.Length)
            (var_22 < var_24)
        else
            false
    if var_25 then
        let (var_26: char) = var_20.[int32 var_22]
        let (var_28: bool) =
            if (var_26 = ' ') then
                true
            else
                if (var_26 = '\n') then
                    true
                else
                    (var_26 = '\r')
        let (var_29: int64) = (var_22 + 1L)
        if var_28 then
            method_73((var_0: int64), (var_1: int64), (var_2: int64), (var_3: int64), (var_4: int64), (var_5: int64), (var_6: int64), (var_7: int64), (var_8: int64), (var_9: int64), (var_10: int64), (var_11: int64), (var_12: int64), (var_13: int64), (var_14: int64), (var_15: int64), (var_16: int64), (var_17: int64), (var_18: int64), (var_19: int64), (var_20: string), (var_23: int64), (var_29: int64))
        else
            let (var_30: int64) = (0L + var_19)
            let (var_31: int64) = (var_30 + var_18)
            let (var_32: int64) = (var_31 + var_17)
            let (var_33: int64) = (var_32 + var_16)
            let (var_34: int64) = (var_33 + var_15)
            let (var_35: int64) = (var_34 + var_14)
            let (var_36: int64) = (var_35 + var_13)
            let (var_37: int64) = (var_36 + var_12)
            let (var_38: int64) = (var_37 + var_11)
            let (var_39: int64) = (var_38 + var_10)
            let (var_40: int64) = (var_39 + var_9)
            let (var_41: int64) = (var_40 + var_8)
            let (var_42: int64) = (var_41 + var_7)
            let (var_43: int64) = (var_42 + var_6)
            let (var_44: int64) = (var_43 + var_5)
            let (var_45: int64) = (var_44 + var_4)
            let (var_46: int64) = (var_45 + var_3)
            let (var_47: int64) = (var_46 + var_2)
            let (var_48: int64) = (var_47 + var_1)
            let (var_49: int64) = (var_48 + var_0)
            System.Console.WriteLine(var_49)
    else
        let (var_50: int64) = (0L + var_19)
        let (var_51: int64) = (var_50 + var_18)
        let (var_52: int64) = (var_51 + var_17)
        let (var_53: int64) = (var_52 + var_16)
        let (var_54: int64) = (var_53 + var_15)
        let (var_55: int64) = (var_54 + var_14)
        let (var_56: int64) = (var_55 + var_13)
        let (var_57: int64) = (var_56 + var_12)
        let (var_58: int64) = (var_57 + var_11)
        let (var_59: int64) = (var_58 + var_10)
        let (var_60: int64) = (var_59 + var_9)
        let (var_61: int64) = (var_60 + var_8)
        let (var_62: int64) = (var_61 + var_7)
        let (var_63: int64) = (var_62 + var_6)
        let (var_64: int64) = (var_63 + var_5)
        let (var_65: int64) = (var_64 + var_4)
        let (var_66: int64) = (var_65 + var_3)
        let (var_67: int64) = (var_66 + var_2)
        let (var_68: int64) = (var_67 + var_1)
        let (var_69: int64) = (var_68 + var_0)
        System.Console.WriteLine(var_69)
let (var_0: System.IO.Stream) = System.Console.OpenStandardInput()
let (var_1: System.IO.StreamReader) = System.IO.StreamReader(var_0)
let (var_2: string) = var_1.ReadToEnd()
let (var_3: int64) = 0L
let (var_5: bool) =
    if (var_3 >= 0L) then
        let (var_4: int64) = (int64 var_2.Length)
        (var_3 < var_4)
    else
        false
if var_5 then
    let (var_6: char) = var_2.[int32 var_3]
    let (var_7: bool) = ('-' = var_6)
    let (var_8: int64) = (var_3 + 1L)
    if var_7 then
        let (var_9: bool) = false
        method_14((var_9: bool), (var_2: string), (var_8: int64))
    else
        let (var_10: bool) = true
        method_14((var_10: bool), (var_2: string), (var_8: int64))
else
    let (var_11: bool) = true
    method_14((var_11: bool), (var_2: string), (var_3: int64))

