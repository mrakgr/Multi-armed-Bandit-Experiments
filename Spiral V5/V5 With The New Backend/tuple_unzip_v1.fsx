// 4/24/2017.

// Now I need unzipping for the backwards map.

// How do I do that? No idea. I'll have to experiment here.

type T =
| S of string
| R of T list

type Result<'a,'b> = Succ of 'a | Fail of 'b

let case_s l on_fatalfail on_fail on_succ =
    let rec loop acc = function
        | (S _ as x) :: xs -> loop (x :: acc) xs
        | [] -> on_succ (List.rev acc)
        | _ when acc.IsEmpty = false -> on_fatalfail()
        | _ -> on_fail ()
    loop [] l

let case_r l on_fatalfail on_fail on_succ =
    let rec loop acc_head acc_tail = function
        | (R (h :: t)) :: xs -> loop (h :: acc_head) (R t :: acc_tail) xs
        | [] -> on_succ (List.rev acc_head, List.rev acc_tail)
        | _ when acc_head.IsEmpty = false -> on_fatalfail()
        | _ -> on_fail ()
    loop [] [] l

let case_r_empty l on_fail on_succ =
    let rec loop = function
        | R [] :: xs -> loop xs
        | [] -> on_succ()
        | _ -> on_fail ()
    loop l

let rec zip_all l =
    let fatalfail acc _ = List.rev acc @ l |> R
    case_s l (fatalfail [])
        (fun _ ->
            let rec loop acc l =
                case_r l 
                    (fatalfail acc)
                    (fun _ -> 
                        case_r_empty l 
                            (fatalfail acc)
                            (fun _ -> List.rev acc |> R))
                    (fun (head, tail) ->
                        let r = zip_all head
                        loop (r :: acc) tail)
            loop [] l)
        R

let q = R [S "q"; S "w"]
let w = R [S "1"; S "2"]
let e = R [S "a"; S "s"]

let f x = zip_all x

let t1 = f [q;w]
let t2 = f [t1;e]

let c = R [R [S "z"; S "v"]; R [S "x"; S "b"]; R [S "c"; S "n"]]

let rec zip2 a b =
    match a,b with
    | S _, S _ -> R [a;b]
    | R a, R b -> List.map2 (fun a b -> zip2 a b) a b |> R
    | _ -> failwith "Incorrect pattern"

let rec unzip2 x =
    match x with
    | R [S a;S b] -> S a, S b
    | R x -> List.map unzip2 x |> List.unzip |> fun (a,b) -> R a, R b
    | _ -> failwith "Irregular tuple."
        
let a = R [R [S "a"; S "q"]; R [S "b"; S "w"]; R [S "c"; S "e"]]
let b = R [R [S "1"; S "4"]; R [S "2"; S "5"]; R [S "3"; S "6"]]

let tt = zip2 a b |> unzip2 |> fun x -> x = (a,b)
