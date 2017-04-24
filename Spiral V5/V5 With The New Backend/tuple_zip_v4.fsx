// 4/21/2017.

// I ran into trouble when I wanted to use CPS in the main typechecker. The problem is that CPS is
// just a convention and nothing is stoping me from slipping back into normal programming.

// The typechecker won't warn me of errors making it pretty much impossible to convert select parts of the
// library to that style and it is both hard to read an non-idiomatic.

// Passing in higher order functions for control flow is a great idea with the single exception of using
// them for returns from function. That is a really bad idea.

// Parser combinators which I am trying to emulate here use the Result type, so I should do it as well.

// Edit: What a ripoff. I guess I did not even need CPS or the Result type for the zip function.

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
    let fatalfail acc _ = List.rev acc @ l |> R |> Succ
    case_s l (fatalfail [])
        (fun _ ->
            let rec loop acc l =
                case_r l 
                    (fatalfail acc)
                    (fun _ -> 
                        case_r_empty l 
                            (fatalfail acc)
                            (fun _ -> List.rev acc |> R |> Succ))
                    (fun (head, tail) ->
                        match zip_all head with
                        | Succ r -> loop (r :: acc) tail
                        | er -> er)
            loop [] l)
        (R >> Succ)

let rec zip_all' l =
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
                        let r = zip_all' head
                        loop (r :: acc) tail)
            loop [] l)
        R

let q = R [S "q"; S "w"]
let w = R [S "1"; S "2"]
let e = R [S "a"; S "s"]

let f x = zip_all' x

let t1 = f [q;w]
let t2 = f [t1;e]

let a = R [R [S "a"; S "q"]; R [S "b"; S "w"]; R [S "c"; S "e"]]
let b = R [R [S "1"; S "4"]; R [S "2"; S "5"]; R [S "3"; S "6"]]
let c = R [R [S "z"; S "v"]; R [S "x"; S "b"]; R [S "c"; S "n"]]

let t3 = f [a;b]
let t4 = f [t3;c]
