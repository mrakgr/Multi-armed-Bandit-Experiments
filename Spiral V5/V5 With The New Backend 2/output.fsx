type Union0 =
    | Union0Case0 of Tuple2
    | Union0Case1
and Union1 =
    | Union1Case0
    | Union1Case1
and Tuple2 =
    struct
    val mem_1: Union1
    new(arg_mem_1) = {mem_1 = arg_mem_1}
    end
let rec method_23((var_0: (Union0 [])), (var_1: int64)): unit =
    if (var_1 < 101L) then
        var_0.[int32 var_1] <- Union0Case1
        let (var_2: int64) = (var_1 + 1L)
        method_23((var_0: (Union0 [])), (var_2: int64))
    else
        ()
and method_24((var_0: (Union0 [])), (var_1: int64)): unit =
    if (var_1 <= 100L) then
        let (var_2: Union1) = Union1Case0
        let (var_3: Union1) = Union1Case1
        let (var_4: Union1) = method_25((var_1: int64), (var_3: Union1), (var_2: Union1), (var_0: (Union0 [])))
        let (var_5: int64) = (var_1 + 1L)
        method_24((var_0: (Union0 [])), (var_5: int64))
    else
        ()
and method_25((var_0: int64), (var_1: Union1), (var_2: Union1), (var_3: (Union0 []))): Union1 =
    let (var_4: bool) =
        match var_2 with
        | Union1Case0 ->
            true
        | Union1Case1 ->
            false
    if var_4 then
        let (var_5: Union0) = var_3.[int32 var_0]
        match var_5 with
        | Union0Case0(var_6) ->
            var_6.mem_1
        | Union0Case1 ->
            let (var_8: Union1) = method_26((var_1: Union1), (var_0: int64), (var_2: Union1), (var_3: (Union0 [])))
            var_3.[int32 var_0] <- (Union0Case0(Tuple2(var_8)))
            var_8
    else
        method_26((var_1: Union1), (var_0: int64), (var_2: Union1), (var_3: (Union0 [])))
and method_26((var_0: Union1), (var_1: int64), (var_2: Union1), (var_3: (Union0 []))): Union1 =
    let (var_9: bool) =
        if (var_1 >= 2L) then
            let (var_4: int64) = (var_1 - 2L)
            let (var_5: Union1) = method_25((var_4: int64), (var_2: Union1), (var_0: Union1), (var_3: (Union0 [])))
            match var_5 with
            | Union1Case0 ->
                match var_2 with
                | Union1Case0 ->
                    true
                | Union1Case1 ->
                    false
            | Union1Case1 ->
                match var_2 with
                | Union1Case0 ->
                    false
                | Union1Case1 ->
                    true
        else
            false
    if var_9 then
        var_2
    else
        let (var_15: bool) =
            if (var_1 >= 3L) then
                let (var_10: int64) = (var_1 - 3L)
                let (var_11: Union1) = method_25((var_10: int64), (var_2: Union1), (var_0: Union1), (var_3: (Union0 [])))
                match var_11 with
                | Union1Case0 ->
                    match var_2 with
                    | Union1Case0 ->
                        true
                    | Union1Case1 ->
                        false
                | Union1Case1 ->
                    match var_2 with
                    | Union1Case0 ->
                        false
                    | Union1Case1 ->
                        true
            else
                false
        if var_15 then
            var_2
        else
            let (var_21: bool) =
                if (var_1 >= 5L) then
                    let (var_16: int64) = (var_1 - 5L)
                    let (var_17: Union1) = method_25((var_16: int64), (var_2: Union1), (var_0: Union1), (var_3: (Union0 [])))
                    match var_17 with
                    | Union1Case0 ->
                        match var_2 with
                        | Union1Case0 ->
                            true
                        | Union1Case1 ->
                            false
                    | Union1Case1 ->
                        match var_2 with
                        | Union1Case0 ->
                            false
                        | Union1Case1 ->
                            true
                else
                    false
            if var_21 then
                var_2
            else
                var_0
and method_27((var_0: bool), (var_1: (Union0 [])), (var_2: string), (var_3: int64)): unit =
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
            method_28((var_0: bool), (var_1: (Union0 [])), (var_2: string), (var_12: int64), (var_8: int64))
        else
            (failwith "pint64")
    else
        (failwith "pint64")
and method_28((var_0: bool), (var_1: (Union0 [])), (var_2: string), (var_3: int64), (var_4: int64)): unit =
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
                method_28((var_0: bool), (var_1: (Union0 [])), (var_2: string), (var_16: int64), (var_9: int64))
            else
                (failwith "integer overflow")
        else
            let (var_17: int64) =
                if var_0 then
                    var_3
                else
                    (-var_3)
            let (var_18: int64) = 0L
            method_29((var_17: int64), (var_1: (Union0 [])), (var_2: string), (var_18: int64), (var_4: int64))
    else
        let (var_19: int64) =
            if var_0 then
                var_3
            else
                (-var_3)
        let (var_20: int64) = 0L
        method_29((var_19: int64), (var_1: (Union0 [])), (var_2: string), (var_20: int64), (var_4: int64))
and method_29((var_0: int64), (var_1: (Union0 [])), (var_2: string), (var_3: int64), (var_4: int64)): unit =
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
            method_29((var_0: int64), (var_1: (Union0 [])), (var_2: string), (var_5: int64), (var_11: int64))
        else
            let (var_12: int64) = 0L
            method_30((var_2: string), (var_12: int64), (var_0: int64), (var_1: (Union0 [])), (var_4: int64))
    else
        let (var_13: int64) = 0L
        method_30((var_2: string), (var_13: int64), (var_0: int64), (var_1: (Union0 [])), (var_4: int64))
and method_30((var_0: string), (var_1: int64), (var_2: int64), (var_3: (Union0 [])), (var_4: int64)): unit =
    let (var_5: bool) = (var_1 < var_2)
    if var_5 then
        let (var_6: int64) = (var_1 + 1L)
        let (var_8: bool) =
            if (var_4 >= 0L) then
                let (var_7: int64) = (int64 var_0.Length)
                (var_4 < var_7)
            else
                false
        if var_8 then
            let (var_9: char) = var_0.[int32 var_4]
            let (var_10: bool) = ('-' = var_9)
            let (var_11: int64) = (var_4 + 1L)
            if var_10 then
                let (var_12: bool) = false
                method_31((var_12: bool), (var_3: (Union0 [])), (var_6: int64), (var_2: int64), (var_0: string), (var_11: int64))
            else
                let (var_13: bool) = true
                method_31((var_13: bool), (var_3: (Union0 [])), (var_6: int64), (var_2: int64), (var_0: string), (var_4: int64))
        else
            let (var_14: bool) = true
            method_31((var_14: bool), (var_3: (Union0 [])), (var_6: int64), (var_2: int64), (var_0: string), (var_4: int64))
    else
        ()
and method_31((var_0: bool), (var_1: (Union0 [])), (var_2: int64), (var_3: int64), (var_4: string), (var_5: int64)): unit =
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
            method_32((var_0: bool), (var_1: (Union0 [])), (var_2: int64), (var_3: int64), (var_4: string), (var_14: int64), (var_10: int64))
        else
            (failwith "pint64")
    else
        (failwith "pint64")
and method_32((var_0: bool), (var_1: (Union0 [])), (var_2: int64), (var_3: int64), (var_4: string), (var_5: int64), (var_6: int64)): unit =
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
                method_32((var_0: bool), (var_1: (Union0 [])), (var_2: int64), (var_3: int64), (var_4: string), (var_18: int64), (var_11: int64))
            else
                (failwith "integer overflow")
        else
            let (var_19: int64) =
                if var_0 then
                    var_5
                else
                    (-var_5)
            let (var_20: int64) = 0L
            method_33((var_19: int64), (var_1: (Union0 [])), (var_2: int64), (var_3: int64), (var_4: string), (var_20: int64), (var_6: int64))
    else
        let (var_21: int64) =
            if var_0 then
                var_5
            else
                (-var_5)
        let (var_22: int64) = 0L
        method_33((var_21: int64), (var_1: (Union0 [])), (var_2: int64), (var_3: int64), (var_4: string), (var_22: int64), (var_6: int64))
and method_33((var_0: int64), (var_1: (Union0 [])), (var_2: int64), (var_3: int64), (var_4: string), (var_5: int64), (var_6: int64)): unit =
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
            method_33((var_0: int64), (var_1: (Union0 [])), (var_2: int64), (var_3: int64), (var_4: string), (var_7: int64), (var_13: int64))
        else
            let (var_14: Union0) = var_1.[int32 var_0]
            match var_14 with
            | Union0Case0(var_15) ->
                let (var_16: Union1) = var_15.mem_1
                match var_16 with
                | Union1Case0 ->
                    System.Console.WriteLine("First")
                | Union1Case1 ->
                    System.Console.WriteLine("Second")
            | Union0Case1 ->
                (failwith "Solution not found.")
            method_30((var_4: string), (var_2: int64), (var_3: int64), (var_1: (Union0 [])), (var_6: int64))
    else
        let (var_17: Union0) = var_1.[int32 var_0]
        match var_17 with
        | Union0Case0(var_18) ->
            let (var_19: Union1) = var_18.mem_1
            match var_19 with
            | Union1Case0 ->
                System.Console.WriteLine("First")
            | Union1Case1 ->
                System.Console.WriteLine("Second")
        | Union0Case1 ->
            (failwith "Solution not found.")
        method_30((var_4: string), (var_2: int64), (var_3: int64), (var_1: (Union0 [])), (var_6: int64))
let (var_0: (Union0 [])) = Array.zeroCreate<Union0> (System.Convert.ToInt32(101L))
let (var_1: int64) = 0L
method_23((var_0: (Union0 [])), (var_1: int64))
let (var_2: int64) = 0L
method_24((var_0: (Union0 [])), (var_2: int64))
let (var_3: System.IO.Stream) = System.Console.OpenStandardInput()
let (var_4: System.IO.StreamReader) = System.IO.StreamReader(var_3)
let (var_5: string) = var_4.ReadToEnd()
let (var_6: int64) = 0L
let (var_8: bool) =
    if (var_6 >= 0L) then
        let (var_7: int64) = (int64 var_5.Length)
        (var_6 < var_7)
    else
        false
if var_8 then
    let (var_9: char) = var_5.[int32 var_6]
    let (var_10: bool) = ('-' = var_9)
    let (var_11: int64) = (var_6 + 1L)
    if var_10 then
        let (var_12: bool) = false
        method_27((var_12: bool), (var_0: (Union0 [])), (var_5: string), (var_11: int64))
    else
        let (var_13: bool) = true
        method_27((var_13: bool), (var_0: (Union0 [])), (var_5: string), (var_6: int64))
else
    let (var_14: bool) = true
    method_27((var_14: bool), (var_0: (Union0 [])), (var_5: string), (var_6: int64))

