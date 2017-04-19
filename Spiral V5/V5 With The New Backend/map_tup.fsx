// 4/19/2017

// Somehow this is making me feel that the way tuples are designed in my language is too convoluted.

type T =
| S of string
| R of T list

let q = R [R [R [S "a"; S "b"]; R [S "a"; S "b"]; R [S "a"; S "b"]]; R [R [S "a"; S "b"]; R [S "a"; S "b"]; R [S "a"; S "b"]]; R [S "a"; S "b"]]

let cons a b = 
    match b with
    | R x -> R (a :: x)
    | _ -> failwith "error"

let rec map_tup2 q = 
    let r = map_tup2
    match q with
    | R (R x :: xs) -> cons (r (R x)) (r (R xs))
    | R [] -> R []
    | R [S "a"; S "b"] -> S "mset"

let rec map_tup_all f q = 
    let r q = map_tup_all f q
    let on_fail () =
        match q with
        | R (R x :: xs) -> cons (r (R x)) (r (R xs))
        | R [] -> R []
    f on_fail q

let f on_fail = function
    | R [S "a"; S "b"] -> S "mset"
    | _ -> on_fail ()

let t = map_tup_all f q

