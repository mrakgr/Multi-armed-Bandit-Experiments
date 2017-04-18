// 4/18/2017.

// Let me do one last perfect version of this.

type T =
| S of string
| R of T list

// On if there are items in the acc, there is no way the other cases will trigger so it skips directly to 
// the last failure.

let case_s l on_fatalfail on_fail ret =
    let rec loop acc = function
        | (S _ as x) :: xs -> loop (x :: acc) xs
        | [] -> ret (List.rev acc)
        | _ when acc.IsEmpty = false -> on_fatalfail()
        | _ -> on_fail ()
    loop [] l

let case_r l on_fatalfail on_fail ret =
    let rec loop acc_head acc_tail = function
        | (R (h :: t)) :: xs -> loop (h :: acc_head) (R t :: acc_tail) xs
        | [] -> ret (List.rev acc_head, List.rev acc_tail)
        | _ when acc_head.IsEmpty = false -> on_fatalfail()
        | _ -> on_fail ()
    loop [] [] l

let case_r_empty l on_fail ret =
    let rec loop = function
        | R [] :: xs -> loop xs
        | [] -> ret()
        | _ -> on_fail ()
    loop l

// If everything apart from the recursive calls were inlined, this would be very efficient code,
// much more so than what I would get with using the Result type for control flow.

// It depends on the F#'s optimizer, but inlining function is probably an easier problem
// than deforestation. With the Spiral backend language, the following code would be a snap
// to optimize to perfection. I'd just make the higher order functions Inlineables and the magic
// would happen on its own.

// I have no doubt that this is the way zip_all should be written.

let rec zip_all l ret =
    let fatalfail acc _ = List.rev acc @ l |> R |> ret
    case_s l 
        (fatalfail [])
        (fun _ ->
            let rec loop acc l =
                case_r l 
                    (fatalfail acc)
                    (fun _ -> 
                        case_r_empty l 
                            (fatalfail acc)
                            (fun _ -> List.rev acc |> R |> ret))
                    (fun (head, tail) ->
                        zip_all head <| fun r -> loop (r :: acc) tail)
            loop [] l)
        (R >> ret)

let q = R [S "q"; S "w"]
let w = R [S "1"; S "2"]
let e = R [S "a"; S "s"]

let f x = zip_all x id 

let t1 = f [q;w]
let t2 = f [t1;e]

let a = R [R [S "a"; S "q"]; R [S "b"; S "w"]; R [S "c"; S "e"]]
let b = R [R [S "1"; S "4"]; R [S "2"; S "5"]; R [S "3"; S "6"]]
let c = R [R [S "z"; S "v"]; R [S "x"; S "b"]; R [S "c"; S "n"]]

let t3 = f [a;b]
let t4 = f [t3;c]