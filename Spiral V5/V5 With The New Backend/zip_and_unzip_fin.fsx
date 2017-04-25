// 4/24/2017.

// I managed to make the zip function a lot better, but all the unsafe operations in the unzip make me nervous.
// I will do it properly.

// Edit: I am finally done. It took me the whole day, but now both operations work only on tuples with regular
// dimensions. Originally I did not intend this, but now that I've resolved to put in unzip into the library,
// I need this rule so the two can be duals of each other.

// The transpose function in unzip is generic and works on any kind of _ list list.

// Zipping and unzipping in general can be expressed as a dimensional shift or a series of transposes.
// That is all these two functions are doing.

type T =
| S of string
| R of T list

let rec zip l =
    let is_all_r_empty x = List.forall (function R [] -> true | _ -> false) x
    let rec loop acc_total acc_head acc_tail x =
        match x with
        | S _ :: _ -> R l
        | R [] :: ys -> 
            if List.isEmpty acc_head && is_all_r_empty ys then List.rev acc_total |> R 
            else R l
        | R (x :: xs) :: ys -> loop acc_total (x :: acc_head) (R xs :: acc_tail) ys
        | [] -> 
            match acc_tail with
            | _ :: _ -> loop ((List.rev acc_head |> zip) :: acc_total) [] [] (List.rev acc_tail)
            | _ -> List.rev acc_total |> R
    loop [] [] [] l

let rec unzip l =
    let transpose l =
        let is_all_empty x = List.forall (function _ :: _ -> false | _ -> true) x
        let rec loop acc_total acc_head acc_tail = function
            | (x :: xs) :: ys -> loop acc_total (x :: acc_head) (xs :: acc_tail) ys
            | [] :: ys -> 
                if List.isEmpty acc_head && is_all_empty ys then loop acc_total acc_head acc_tail ys 
                else l
            | [] ->
                match acc_tail with
                | _ :: _ -> loop (List.rev acc_head :: acc_total) [] [] (List.rev acc_tail)
                | _ -> List.rev acc_total
        loop [] [] [] l
    let is_all_r x = List.forall (function R _ -> true | _ -> false) x
    match l with
    | R x when is_all_r x -> List.map unzip x |> transpose |> List.map R
    | R x -> x
    | S _ -> failwith "Unzip called on S."

//let a = R [R [S "a"; S "t"]; R [S "b"; S "w"]; R [S "c"; S "e"]]
//let b = R [R [S "1"; S "4"]; R [S "5"; S "r"]; R [S "3"; S "6"]]
//let c = R [R [S "z"; S "v"]; R [S "x"; S "b"]; R [S "c"; S "2"]]
//
//let t3 = zip [a;b]
//let t4 = zip [t3;c]
//let u1 = unzip t4
//let r1 = u1 = [t3;c]
//let u2 = unzip t3
//let r2 = u2 = [a;b] // The above works fine on tuples with regular dimensions.

let a = R [R [S "q"; S "w"; S "e"]]
let b = R [R [S "a"; S "s"]; R [S "z"]; S "wqe"]
let ab = [a;b]
let t = zip ab
let t' = unzip t
ab = t' // This is false, but I would like the ziping and then unziping to be reversible if possible.