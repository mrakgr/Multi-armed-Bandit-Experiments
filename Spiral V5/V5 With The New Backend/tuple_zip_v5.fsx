// 4/24/2017.

// Seeing how concise the unzip came out inspired me to give the zip one more try.

// Edit: Success. The new code is 4x shorter than the old one which was incredibly convoluted.
// This particular bit is a masterpiece of software engineering.

type T =
| S of string
| R of T list

let rec zip_all (l: T list) =
    let is_all_r_empty x = List.forall (function R [] -> true | _ -> false) x
    let rec loop acc_total acc_head acc_tail x =
        match x with
        | S _ :: _ -> R l
        | R [] :: ys -> if is_all_r_empty ys then List.rev acc_total |> R else R l
        | R (x :: xs) :: ys -> loop acc_total (x :: acc_head) (R xs :: acc_tail) ys
        | [] -> 
            match acc_tail with
            | _ :: _ -> loop ((List.rev acc_head |> zip_all) :: acc_total) [] [] (List.rev acc_tail)
            | _ -> List.rev acc_total |> R
    loop [] [] [] l

let a = R [R [S "a"; S "q"]; R [S "b"; S "w"]; R [S "c"; S "e"]]
let b = R [R [S "1"; S "4"]; R [S "2"; S "5"]; R [S "3"; S "6"]]
let c = R [R [S "z"; S "v"]; R [S "x"; S "b"]; R [S "c"; S "n"]]

let t3 = zip_all [a;b]
let t4 = zip_all [t3;c]