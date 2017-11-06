module SpiralExample
let cuda_kernels = """
extern "C" {
}
"""

let rec method_0((var_0: string), (var_1: int64)): unit =
    let (var_3: bool) =
        if (var_1 >= 0L) then
            let (var_2: int64) = (int64 var_0.Length)
            (var_1 < var_2)
        else
            false
    if var_3 then
        let (var_4: char) = var_0.[int32 var_1]
        let (var_5: bool) =
            if (var_4 >= '0') then
                (var_4 <= '9')
            else
                false
        let (var_6: int64) = (var_1 + 1L)
        if var_5 then
            let (var_7: int64) = System.Convert.ToInt64(var_4)
            let (var_8: int64) = System.Convert.ToInt64('0')
            let (var_9: int64) = (var_7 - var_8)
            method_1((var_0: string), (var_9: int64), (var_6: int64))
        else
            (failwith "pint64")
    else
        (failwith "pint64")
and method_1((var_0: string), (var_1: int64), (var_2: int64)): unit =
    let (var_4: bool) =
        if (var_2 >= 0L) then
            let (var_3: int64) = (int64 var_0.Length)
            (var_2 < var_3)
        else
            false
    if var_4 then
        let (var_5: char) = var_0.[int32 var_2]
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
            let (var_11: bool) =
                if (var_1 = 922337203685477580L) then
                    (var_10 <= 7L)
                else
                    false
            let (var_12: bool) =
                if var_11 then
                    true
                else
                    (var_1 < 922337203685477580L)
            if var_12 then
                let (var_13: int64) = (var_1 * 10L)
                let (var_14: int64) = (var_13 + var_10)
                method_1((var_0: string), (var_14: int64), (var_7: int64))
            else
                (failwith "integer overflow")
        else
            let (var_15: int64) = 0L
            method_2((var_1: int64), (var_0: string), (var_15: int64), (var_2: int64))
    else
        let (var_16: int64) = 0L
        method_2((var_1: int64), (var_0: string), (var_16: int64), (var_2: int64))
and method_2((var_0: int64), (var_1: string), (var_2: int64), (var_3: int64)): unit =
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
            method_2((var_0: int64), (var_1: string), (var_4: int64), (var_10: int64))
        else
            method_3((var_0: int64), (var_1: string), (var_3: int64))
    else
        method_3((var_0: int64), (var_1: string), (var_3: int64))
and method_3((var_0: int64), (var_1: string), (var_2: int64)): unit =
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
            method_4((var_0: int64), (var_1: string), (var_10: int64), (var_7: int64))
        else
            (failwith "pint64")
    else
        (failwith "pint64")
and method_4((var_0: int64), (var_1: string), (var_2: int64), (var_3: int64)): unit =
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
                method_4((var_0: int64), (var_1: string), (var_15: int64), (var_8: int64))
            else
                (failwith "integer overflow")
        else
            let (var_16: int64) = 0L
            method_5((var_2: int64), (var_0: int64), (var_1: string), (var_16: int64), (var_3: int64))
    else
        let (var_17: int64) = 0L
        method_5((var_2: int64), (var_0: int64), (var_1: string), (var_17: int64), (var_3: int64))
and method_5((var_0: int64), (var_1: int64), (var_2: string), (var_3: int64), (var_4: int64)): unit =
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
            method_5((var_0: int64), (var_1: int64), (var_2: string), (var_5: int64), (var_11: int64))
        else
            method_6((var_0: int64), (var_1: int64), (var_2: string), (var_4: int64))
    else
        method_6((var_0: int64), (var_1: int64), (var_2: string), (var_4: int64))
and method_6((var_0: int64), (var_1: int64), (var_2: string), (var_3: int64)): unit =
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
            method_7((var_0: int64), (var_1: int64), (var_2: string), (var_11: int64), (var_8: int64))
        else
            (failwith "pint64")
    else
        (failwith "pint64")
and method_7((var_0: int64), (var_1: int64), (var_2: string), (var_3: int64), (var_4: int64)): unit =
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
                method_7((var_0: int64), (var_1: int64), (var_2: string), (var_16: int64), (var_9: int64))
            else
                (failwith "integer overflow")
        else
            let (var_17: int64) = 0L
            method_8((var_3: int64), (var_0: int64), (var_1: int64), (var_2: string), (var_17: int64), (var_4: int64))
    else
        let (var_18: int64) = 0L
        method_8((var_3: int64), (var_0: int64), (var_1: int64), (var_2: string), (var_18: int64), (var_4: int64))
and method_8((var_0: int64), (var_1: int64), (var_2: int64), (var_3: string), (var_4: int64), (var_5: int64)): unit =
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
            method_8((var_0: int64), (var_1: int64), (var_2: int64), (var_3: string), (var_6: int64), (var_12: int64))
        else
            method_9((var_0: int64), (var_1: int64), (var_2: int64), (var_3: string), (var_5: int64))
    else
        method_9((var_0: int64), (var_1: int64), (var_2: int64), (var_3: string), (var_5: int64))
and method_9((var_0: int64), (var_1: int64), (var_2: int64), (var_3: string), (var_4: int64)): unit =
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
            method_10((var_0: int64), (var_1: int64), (var_2: int64), (var_3: string), (var_12: int64), (var_9: int64))
        else
            (failwith "pint64")
    else
        (failwith "pint64")
and method_10((var_0: int64), (var_1: int64), (var_2: int64), (var_3: string), (var_4: int64), (var_5: int64)): unit =
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
                method_10((var_0: int64), (var_1: int64), (var_2: int64), (var_3: string), (var_17: int64), (var_10: int64))
            else
                (failwith "integer overflow")
        else
            let (var_18: int64) = 0L
            method_11((var_4: int64), (var_0: int64), (var_1: int64), (var_2: int64), (var_3: string), (var_18: int64), (var_5: int64))
    else
        let (var_19: int64) = 0L
        method_11((var_4: int64), (var_0: int64), (var_1: int64), (var_2: int64), (var_3: string), (var_19: int64), (var_5: int64))
and method_11((var_0: int64), (var_1: int64), (var_2: int64), (var_3: int64), (var_4: string), (var_5: int64), (var_6: int64)): unit =
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
            method_11((var_0: int64), (var_1: int64), (var_2: int64), (var_3: int64), (var_4: string), (var_7: int64), (var_13: int64))
        else
            method_12((var_0: int64), (var_1: int64), (var_2: int64), (var_3: int64), (var_4: string), (var_6: int64))
    else
        method_12((var_0: int64), (var_1: int64), (var_2: int64), (var_3: int64), (var_4: string), (var_6: int64))
and method_12((var_0: int64), (var_1: int64), (var_2: int64), (var_3: int64), (var_4: string), (var_5: int64)): unit =
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
            method_13((var_0: int64), (var_1: int64), (var_2: int64), (var_3: int64), (var_4: string), (var_13: int64), (var_10: int64))
        else
            (failwith "pint64")
    else
        (failwith "pint64")
and method_13((var_0: int64), (var_1: int64), (var_2: int64), (var_3: int64), (var_4: string), (var_5: int64), (var_6: int64)): unit =
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
                method_13((var_0: int64), (var_1: int64), (var_2: int64), (var_3: int64), (var_4: string), (var_18: int64), (var_11: int64))
            else
                (failwith "integer overflow")
        else
            let (var_19: int64) = 0L
            method_14((var_5: int64), (var_0: int64), (var_1: int64), (var_2: int64), (var_3: int64), (var_4: string), (var_19: int64), (var_6: int64))
    else
        let (var_20: int64) = 0L
        method_14((var_5: int64), (var_0: int64), (var_1: int64), (var_2: int64), (var_3: int64), (var_4: string), (var_20: int64), (var_6: int64))
and method_14((var_0: int64), (var_1: int64), (var_2: int64), (var_3: int64), (var_4: int64), (var_5: string), (var_6: int64), (var_7: int64)): unit =
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
            method_14((var_0: int64), (var_1: int64), (var_2: int64), (var_3: int64), (var_4: int64), (var_5: string), (var_8: int64), (var_14: int64))
        else
            method_15((var_0: int64), (var_1: int64), (var_2: int64), (var_3: int64), (var_4: int64), (var_5: string), (var_7: int64))
    else
        method_15((var_0: int64), (var_1: int64), (var_2: int64), (var_3: int64), (var_4: int64), (var_5: string), (var_7: int64))
and method_15((var_0: int64), (var_1: int64), (var_2: int64), (var_3: int64), (var_4: int64), (var_5: string), (var_6: int64)): unit =
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
            method_16((var_0: int64), (var_1: int64), (var_2: int64), (var_3: int64), (var_4: int64), (var_5: string), (var_14: int64), (var_11: int64))
        else
            (failwith "pint64")
    else
        (failwith "pint64")
and method_16((var_0: int64), (var_1: int64), (var_2: int64), (var_3: int64), (var_4: int64), (var_5: string), (var_6: int64), (var_7: int64)): unit =
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
                method_16((var_0: int64), (var_1: int64), (var_2: int64), (var_3: int64), (var_4: int64), (var_5: string), (var_19: int64), (var_12: int64))
            else
                (failwith "integer overflow")
        else
            let (var_20: int64) = 0L
            method_17((var_6: int64), (var_0: int64), (var_1: int64), (var_2: int64), (var_3: int64), (var_4: int64), (var_5: string), (var_20: int64), (var_7: int64))
    else
        let (var_21: int64) = 0L
        method_17((var_6: int64), (var_0: int64), (var_1: int64), (var_2: int64), (var_3: int64), (var_4: int64), (var_5: string), (var_21: int64), (var_7: int64))
and method_17((var_0: int64), (var_1: int64), (var_2: int64), (var_3: int64), (var_4: int64), (var_5: int64), (var_6: string), (var_7: int64), (var_8: int64)): unit =
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
            method_17((var_0: int64), (var_1: int64), (var_2: int64), (var_3: int64), (var_4: int64), (var_5: int64), (var_6: string), (var_9: int64), (var_15: int64))
        else
            method_18((var_0: int64), (var_1: int64), (var_2: int64), (var_3: int64), (var_4: int64), (var_5: int64), (var_6: string), (var_8: int64))
    else
        method_18((var_0: int64), (var_1: int64), (var_2: int64), (var_3: int64), (var_4: int64), (var_5: int64), (var_6: string), (var_8: int64))
and method_18((var_0: int64), (var_1: int64), (var_2: int64), (var_3: int64), (var_4: int64), (var_5: int64), (var_6: string), (var_7: int64)): unit =
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
            method_19((var_0: int64), (var_1: int64), (var_2: int64), (var_3: int64), (var_4: int64), (var_5: int64), (var_6: string), (var_15: int64), (var_12: int64))
        else
            (failwith "pint64")
    else
        (failwith "pint64")
and method_19((var_0: int64), (var_1: int64), (var_2: int64), (var_3: int64), (var_4: int64), (var_5: int64), (var_6: string), (var_7: int64), (var_8: int64)): unit =
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
                method_19((var_0: int64), (var_1: int64), (var_2: int64), (var_3: int64), (var_4: int64), (var_5: int64), (var_6: string), (var_20: int64), (var_13: int64))
            else
                (failwith "integer overflow")
        else
            let (var_21: int64) = 0L
            method_20((var_7: int64), (var_0: int64), (var_1: int64), (var_2: int64), (var_3: int64), (var_4: int64), (var_5: int64), (var_6: string), (var_21: int64), (var_8: int64))
    else
        let (var_22: int64) = 0L
        method_20((var_7: int64), (var_0: int64), (var_1: int64), (var_2: int64), (var_3: int64), (var_4: int64), (var_5: int64), (var_6: string), (var_22: int64), (var_8: int64))
and method_20((var_0: int64), (var_1: int64), (var_2: int64), (var_3: int64), (var_4: int64), (var_5: int64), (var_6: int64), (var_7: string), (var_8: int64), (var_9: int64)): unit =
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
            method_20((var_0: int64), (var_1: int64), (var_2: int64), (var_3: int64), (var_4: int64), (var_5: int64), (var_6: int64), (var_7: string), (var_10: int64), (var_16: int64))
        else
            method_21((var_0: int64), (var_1: int64), (var_2: int64), (var_3: int64), (var_4: int64), (var_5: int64), (var_6: int64), (var_7: string), (var_9: int64))
    else
        method_21((var_0: int64), (var_1: int64), (var_2: int64), (var_3: int64), (var_4: int64), (var_5: int64), (var_6: int64), (var_7: string), (var_9: int64))
and method_21((var_0: int64), (var_1: int64), (var_2: int64), (var_3: int64), (var_4: int64), (var_5: int64), (var_6: int64), (var_7: string), (var_8: int64)): unit =
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
            method_22((var_0: int64), (var_1: int64), (var_2: int64), (var_3: int64), (var_4: int64), (var_5: int64), (var_6: int64), (var_7: string), (var_16: int64), (var_13: int64))
        else
            (failwith "pint64")
    else
        (failwith "pint64")
and method_22((var_0: int64), (var_1: int64), (var_2: int64), (var_3: int64), (var_4: int64), (var_5: int64), (var_6: int64), (var_7: string), (var_8: int64), (var_9: int64)): unit =
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
                method_22((var_0: int64), (var_1: int64), (var_2: int64), (var_3: int64), (var_4: int64), (var_5: int64), (var_6: int64), (var_7: string), (var_21: int64), (var_14: int64))
            else
                (failwith "integer overflow")
        else
            let (var_22: int64) = 0L
            method_23((var_8: int64), (var_0: int64), (var_1: int64), (var_2: int64), (var_3: int64), (var_4: int64), (var_5: int64), (var_6: int64), (var_7: string), (var_22: int64), (var_9: int64))
    else
        let (var_23: int64) = 0L
        method_23((var_8: int64), (var_0: int64), (var_1: int64), (var_2: int64), (var_3: int64), (var_4: int64), (var_5: int64), (var_6: int64), (var_7: string), (var_23: int64), (var_9: int64))
and method_23((var_0: int64), (var_1: int64), (var_2: int64), (var_3: int64), (var_4: int64), (var_5: int64), (var_6: int64), (var_7: int64), (var_8: string), (var_9: int64), (var_10: int64)): unit =
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
            method_23((var_0: int64), (var_1: int64), (var_2: int64), (var_3: int64), (var_4: int64), (var_5: int64), (var_6: int64), (var_7: int64), (var_8: string), (var_11: int64), (var_17: int64))
        else
            method_24((var_0: int64), (var_1: int64), (var_2: int64), (var_3: int64), (var_4: int64), (var_5: int64), (var_6: int64), (var_7: int64), (var_8: string), (var_10: int64))
    else
        method_24((var_0: int64), (var_1: int64), (var_2: int64), (var_3: int64), (var_4: int64), (var_5: int64), (var_6: int64), (var_7: int64), (var_8: string), (var_10: int64))
and method_24((var_0: int64), (var_1: int64), (var_2: int64), (var_3: int64), (var_4: int64), (var_5: int64), (var_6: int64), (var_7: int64), (var_8: string), (var_9: int64)): unit =
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
            method_25((var_0: int64), (var_1: int64), (var_2: int64), (var_3: int64), (var_4: int64), (var_5: int64), (var_6: int64), (var_7: int64), (var_8: string), (var_17: int64), (var_14: int64))
        else
            (failwith "pint64")
    else
        (failwith "pint64")
and method_25((var_0: int64), (var_1: int64), (var_2: int64), (var_3: int64), (var_4: int64), (var_5: int64), (var_6: int64), (var_7: int64), (var_8: string), (var_9: int64), (var_10: int64)): unit =
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
                method_25((var_0: int64), (var_1: int64), (var_2: int64), (var_3: int64), (var_4: int64), (var_5: int64), (var_6: int64), (var_7: int64), (var_8: string), (var_22: int64), (var_15: int64))
            else
                (failwith "integer overflow")
        else
            let (var_23: int64) = 0L
            method_26((var_9: int64), (var_0: int64), (var_1: int64), (var_2: int64), (var_3: int64), (var_4: int64), (var_5: int64), (var_6: int64), (var_7: int64), (var_8: string), (var_23: int64), (var_10: int64))
    else
        let (var_24: int64) = 0L
        method_26((var_9: int64), (var_0: int64), (var_1: int64), (var_2: int64), (var_3: int64), (var_4: int64), (var_5: int64), (var_6: int64), (var_7: int64), (var_8: string), (var_24: int64), (var_10: int64))
and method_26((var_0: int64), (var_1: int64), (var_2: int64), (var_3: int64), (var_4: int64), (var_5: int64), (var_6: int64), (var_7: int64), (var_8: int64), (var_9: string), (var_10: int64), (var_11: int64)): unit =
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
            method_26((var_0: int64), (var_1: int64), (var_2: int64), (var_3: int64), (var_4: int64), (var_5: int64), (var_6: int64), (var_7: int64), (var_8: int64), (var_9: string), (var_12: int64), (var_18: int64))
        else
            method_27((var_0: int64), (var_1: int64), (var_2: int64), (var_3: int64), (var_4: int64), (var_5: int64), (var_6: int64), (var_7: int64), (var_8: int64), (var_9: string), (var_11: int64))
    else
        method_27((var_0: int64), (var_1: int64), (var_2: int64), (var_3: int64), (var_4: int64), (var_5: int64), (var_6: int64), (var_7: int64), (var_8: int64), (var_9: string), (var_11: int64))
and method_27((var_0: int64), (var_1: int64), (var_2: int64), (var_3: int64), (var_4: int64), (var_5: int64), (var_6: int64), (var_7: int64), (var_8: int64), (var_9: string), (var_10: int64)): unit =
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
            method_28((var_0: int64), (var_1: int64), (var_2: int64), (var_3: int64), (var_4: int64), (var_5: int64), (var_6: int64), (var_7: int64), (var_8: int64), (var_9: string), (var_18: int64), (var_15: int64))
        else
            (failwith "pint64")
    else
        (failwith "pint64")
and method_28((var_0: int64), (var_1: int64), (var_2: int64), (var_3: int64), (var_4: int64), (var_5: int64), (var_6: int64), (var_7: int64), (var_8: int64), (var_9: string), (var_10: int64), (var_11: int64)): unit =
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
                method_28((var_0: int64), (var_1: int64), (var_2: int64), (var_3: int64), (var_4: int64), (var_5: int64), (var_6: int64), (var_7: int64), (var_8: int64), (var_9: string), (var_23: int64), (var_16: int64))
            else
                (failwith "integer overflow")
        else
            let (var_24: int64) = 0L
            method_29((var_10: int64), (var_0: int64), (var_1: int64), (var_2: int64), (var_3: int64), (var_4: int64), (var_5: int64), (var_6: int64), (var_7: int64), (var_8: int64), (var_9: string), (var_24: int64), (var_11: int64))
    else
        let (var_25: int64) = 0L
        method_29((var_10: int64), (var_0: int64), (var_1: int64), (var_2: int64), (var_3: int64), (var_4: int64), (var_5: int64), (var_6: int64), (var_7: int64), (var_8: int64), (var_9: string), (var_25: int64), (var_11: int64))
and method_29((var_0: int64), (var_1: int64), (var_2: int64), (var_3: int64), (var_4: int64), (var_5: int64), (var_6: int64), (var_7: int64), (var_8: int64), (var_9: int64), (var_10: string), (var_11: int64), (var_12: int64)): unit =
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
            method_29((var_0: int64), (var_1: int64), (var_2: int64), (var_3: int64), (var_4: int64), (var_5: int64), (var_6: int64), (var_7: int64), (var_8: int64), (var_9: int64), (var_10: string), (var_13: int64), (var_19: int64))
        else
            let (var_20: int64) = (var_9 + var_8)
            let (var_21: int64) = (var_20 + var_7)
            let (var_22: int64) = (var_21 + var_6)
            let (var_23: int64) = (var_22 + var_5)
            let (var_24: int64) = (var_23 + var_4)
            let (var_25: int64) = (var_24 + var_3)
            let (var_26: int64) = (var_25 + var_2)
            let (var_27: int64) = (var_26 + var_1)
            let (var_28: int64) = (var_27 + var_0)
            System.Console.WriteLine(var_28)
    else
        let (var_29: int64) = (var_9 + var_8)
        let (var_30: int64) = (var_29 + var_7)
        let (var_31: int64) = (var_30 + var_6)
        let (var_32: int64) = (var_31 + var_5)
        let (var_33: int64) = (var_32 + var_4)
        let (var_34: int64) = (var_33 + var_3)
        let (var_35: int64) = (var_34 + var_2)
        let (var_36: int64) = (var_35 + var_1)
        let (var_37: int64) = (var_36 + var_0)
        System.Console.WriteLine(var_37)
let (var_0: System.IO.Stream) = System.Console.OpenStandardInput()
let (var_1: System.IO.StreamReader) = System.IO.StreamReader(var_0)
let (var_2: string) = var_1.ReadToEnd()
let (var_3: int64) = 0L
method_0((var_2: string), (var_3: int64))

