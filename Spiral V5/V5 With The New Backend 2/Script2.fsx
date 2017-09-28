// The minimalist HackerRank example.

// It is compiled from the following Spiral code:

//open Console
//open Parsing
//
//inl n = dyn 100000
//inl p = 
//    inm ar = parse_n_array {parser=pint64 .>> spaces; typ=int64} n 
//    writeline 2
//    |> succ
//
//run_with_unit_ret (readall()) p

// What it does is generate 100k integers, parser them and then prints 2. Past 60k iteration it segmentation faults on HackerRank's servers.

let rec method_14((var_0: (int64 [])), (var_1: string), (var_2: int64), (var_3: int64), (var_4: int64)): unit =
    let (var_5: bool) = (var_2 < var_3)
    if var_5 then
        method_15((var_0: (int64 [])), (var_2: int64), (var_3: int64), (var_1: string), (var_4: int64))
    else
        System.Console.WriteLine(2L)
and method_15((var_0: (int64 [])), (var_1: int64), (var_2: int64), (var_3: string), (var_4: int64)): unit =
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
            method_16((var_0: (int64 [])), (var_1: int64), (var_2: int64), (var_3: string), (var_13: int64), (var_9: int64))
        else
            (failwith "pint64")
    else
        (failwith "pint64")
and method_16((var_0: (int64 [])), (var_1: int64), (var_2: int64), (var_3: string), (var_4: int64), (var_5: int64)): unit =
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
                method_16((var_0: (int64 [])), (var_1: int64), (var_2: int64), (var_3: string), (var_17: int64), (var_10: int64))
            else
                (failwith "integer overflow")
        else
            let (var_18: int64) = 0L
            method_17((var_4: int64), (var_0: (int64 [])), (var_1: int64), (var_2: int64), (var_3: string), (var_18: int64), (var_5: int64))
    else
        let (var_19: int64) = 0L
        method_17((var_4: int64), (var_0: (int64 [])), (var_1: int64), (var_2: int64), (var_3: string), (var_19: int64), (var_5: int64))
and method_17((var_0: int64), (var_1: (int64 [])), (var_2: int64), (var_3: int64), (var_4: string), (var_5: int64), (var_6: int64)): unit =
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
            method_17((var_0: int64), (var_1: (int64 [])), (var_2: int64), (var_3: int64), (var_4: string), (var_7: int64), (var_13: int64))
        else
            var_1.[int32 var_2] <- var_0
            let (var_14: int64) = (var_2 + 1L)
            method_14((var_1: (int64 [])), (var_4: string), (var_14: int64), (var_3: int64), (var_6: int64))
    else
        var_1.[int32 var_2] <- var_0
        let (var_15: int64) = (var_2 + 1L)
        method_14((var_1: (int64 [])), (var_4: string), (var_15: int64), (var_3: int64), (var_6: int64))
let (var_0: int64) = 100000L
let (var_1: bool) = (var_0 > 0L)
let (var_4: string) = 
    let n = 100000
    let e = String.replicate n "100 "
    sprintf "%i\n%s" n e
let (var_5: int64) = 0L
if var_1 then
    let (var_6: (int64 [])) = Array.zeroCreate<int64> (System.Convert.ToInt32(var_0))
    let (var_7: int64) = 0L
    method_14((var_6: (int64 [])), (var_4: string), (var_7: int64), (var_0: int64), (var_5: int64))
else
    (failwith "n in parse array must be > 0")

