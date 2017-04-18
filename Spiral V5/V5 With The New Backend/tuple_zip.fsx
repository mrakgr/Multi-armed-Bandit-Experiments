// 4/17/2017:

// At first glance, how to zip a tuple seems trivial, but it is fact a multidimensional transpose
// that will take me time to figure out how to do properly.

type T =
| S of string
| R of T list

let rec zip2 a b =
    match a,b with
    | S _, S _ -> R [a;b]
    | R a, R b -> List.map2 (fun a b -> zip2 a b) a b |> R
    | _ -> failwith "Incorrect pattern"

let rec zip2' a b =
    match a,b with
    | R a, R b -> List.map2 (fun a b -> zip2 a b) a b |> R
    | _ -> R [a;b]

let rec zip_all l =
    match l with
    | R [R a; R b] -> List.map2 (fun a b -> zip_all (R [a; b])) a b |> R
    | R [R a; R b; R c] -> List.map3 (fun a b c -> zip_all (R [a; b; c])) a b c |> R
    | _ -> l
    
let a = R [R [S "a"; S "q"]; R [S "b"; S "w"]; R [S "c"; S "e"]]
let b = R [R [S "1"; S "4"]; R [S "2"; S "5"]; R [S "3"; S "6"]]
let c = R [R [S "z"; S "v"]; R [S "x"; S "b"]; R [S "c"; S "n"]]

type Result<'a,'b> = Succ of 'a | Fail of 'b

let fold_er f x =
    let rec loop state = function
        | x :: xs as l -> 
            match f state x with
            | Succ state ->
                match loop state xs with
                | Succ state -> Succ state
                | Fail _ as er -> er
            | Fail er -> Fail (er, l)
        | [] -> Succ state
    loop x

let rec transpose = function
    | (_::_)::_ as M -> List.map List.head M :: transpose (List.map List.tail M)
    | _ -> []

let r_strip = 
    function
    | R x -> 
        match fold_er (fun s x -> 
            match x, s with
            | R x, (None, s) -> Succ (Some (List.length x), x :: s) 
            | R x, (Some l, s) -> 
                if l = List.length x then Succ (Some l, x :: s) 
                else Fail s
            | _, (_, s) -> Fail s) (None, []) x with
        | Succ (_,x) -> Succ (List.rev x)
        | Fail (suc,rest) -> Fail (List.rev suc, rest)
    | S x -> Fail([],[])

// This is for regular multidimensional tuples.
let rec zip_all' l =
    match r_strip l with
    | Succ l -> let l = transpose l in R (List.map (R >> zip_all') l)
    | Fail _ -> l

// This is for irregular multidimensional tuples.
let rec zip_all'' l =
    let f l = 
        let l = transpose l
        List.map (R >> zip_all'') l
    match r_strip l with
    | Succ l -> f l |> R
    | Fail(suc,rest) -> (f suc) @ rest |> R
        
let t1 = zip_all (R [a; b; c])
let t2 = zip_all' (R [a; b; c])
let t3 = zip_all'' (R [a; b; c])
let t1_eq_t2 = t1 = t2 // true
let t2_eq_t3 = t2 = t3 // true

let d = R [R [S "r"; S "u"]; R [S "t"; (*S "i"*)]; R [S "y"; S "o"]]
let t4 = zip_all' (R [a;b;c;d])
let t5 = zip_all'' (R [a;b;c;d])

