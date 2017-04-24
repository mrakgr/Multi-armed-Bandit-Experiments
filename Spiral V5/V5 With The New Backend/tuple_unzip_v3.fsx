// 4/24/2017.

// I have an idea now. And I know how to do it for irregular arrays as well.
// Doing it all in one loop will be tricky though...

// I guess I will have to experiment with it for a while longer.

type T =
| S of string
| R of T list

type Result<'a,'b> = Succ of 'a | Fail of 'b

let rec zip_all l =
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

let f x = zip_all x

let a = R [R [S "a"; S "q"]; R [S "b"; S "w"]; R [S "c"; S "e"]]
let b = R [R [S "1"; S "4"]; R [S "2"; S "5"]; R [S "3"; S "6"]]
let c = R [R [S "z"; S "v"]; R [S "x"; S "b"]; R [S "c"; S "n"]]

let rec unzip_all x =
    let rec transpose = function
        | (_::_)::_ as M -> List.map List.head M :: transpose (List.map List.tail M)
        | _ -> []
    let is_all_r = List.forall (function S _ -> false | R _ -> true)
    match x with
    | R x when is_all_r x -> List.map unzip_all x |> transpose |> List.map R
    | R x -> x
    | S _ -> failwith "Don't call on this."

let t3 = f [a;b]
let t4 = f [t3;c]
let tt = unzip_all t4 = [t3;c]