open System.Collections.Generic

let inline test dict_create dict_add dict_clear m m' =
    let d = dict_create()
    let l = Array.init 20000 m
    let l' = Array.init 20000 m'

    let s = System.Diagnostics.Stopwatch.StartNew()
    Array.iter (fun x -> dict_add x 0 d) l
    let e1 = s.Elapsed

    dict_clear d

    s.Restart()
    Array.iter (fun x -> dict_add x 0 d) l'
    let e2 = s.Elapsed
    e1,e2

let sorted_dict_int_list_timmings =
    test ((fun _ -> SortedDictionary())) (fun k v d -> d.Add(k,v)) (fun d -> d.Clear())
        (fun x -> x :: List.replicate 30 1) (fun x -> List.replicate 30 1 @ [x])

let sorted_dict_string_timmings =
    test ((fun _ -> SortedDictionary())) (fun k v d -> d.Add(k,v)) (fun d -> d.Clear())
        (fun x -> string x + String.replicate 30 " ") (fun x -> String.replicate 30 " " + string x)

let dict_int_list_timmings =
    test ((fun _ -> Dictionary())) (fun k v d -> d.Add(k,v)) (fun d -> d.Clear())
        (fun x -> x :: List.replicate 30 1) (fun x -> List.replicate 30 1 @ [x])

let dict_string_timmings =
    test ((fun _ -> Dictionary())) (fun k v d -> d.Add(k,v)) (fun d -> d.Clear())
        (fun x -> string x + String.replicate 30 " ") (fun x -> String.replicate 30 " " + string x)

