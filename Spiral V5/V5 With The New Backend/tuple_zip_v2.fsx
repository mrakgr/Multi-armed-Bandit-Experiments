// 4/18/2017.

// As expected this is quite far from trivial. I am continuing where I left off yesterday.
// I will take this chance to experiment writing the code in CPS style instead of using the
// Result DU which would be quite inefficient.

// I'll rewrite the pattern matcher in this style as well.

// Edit: It took me like 3 times, but I did it.
// The final version is not too bad, with some syntactic sugar, writing programs
// in this form would be viable.

type T =
| S of string
| R of T list

let rec zip_all_cps_v0 on_succ_done on_succ on_fail x = 
    match x with
    | R x ->
        let rec loop_r_flat transposed_acc = function
            | R [] :: ys -> loop_r_flat transposed_acc ys
            | _ :: ys -> on_fail (List.rev transposed_acc)
            | [] -> on_succ (List.rev transposed_acc)
        and loop_r transposed_acc acc_head acc_tail = function
            | R (x :: xs) :: ys -> loop_r transposed_acc (x :: acc_head) (R xs :: acc_tail) ys
            | _ :: ys -> on_fail (List.rev transposed_acc)
            | [] -> loop_start (List.rev acc_head :: transposed_acc) (List.rev acc_tail)
        and loop_s transposed_acc acc = function
            | S x :: ys -> loop_s transposed_acc (S x :: acc) ys
            | _ :: ys -> on_fail (List.rev transposed_acc)
            | [] -> on_succ_done (List.rev acc :: transposed_acc |> List.rev)
        and loop_start transposed_acc = 
            function
            | R [] :: ys -> loop_r_flat transposed_acc ys
            | R x :: ys as l -> loop_r transposed_acc [] [] l
            | S x :: ys as l -> loop_s transposed_acc [] l
            | [] -> on_succ (List.rev transposed_acc)
        loop_start [] x
    | S _ -> failwith "Don't call on this."

let unstrip acc = R (List.map R acc)

let on_succ_done acc = unstrip acc
let rec on_succ acc = zip_all_cps_v0 on_succ_done on_succ on_fail (unstrip acc)
and on_fail acc = failwith "The tuple is irregular"    

let a = R [R [S "a"; S "q"]; R [S "b"; S "w"]; R [S "c"; S "e"]]
let b = R [R [S "1"; S "4"]; R [S "2"; S "5"]; R [S "3"; S "6"]]
let c = R [R [S "z"; S "v"]; R [S "x"; S "b"]; R [S "c"; S "n"]]

//let t1 = zip_all_cps_v0 on_succ_done on_succ on_fail (R [a; b]) // Infinite loop

// You know, I am a fool. If I was going to write things in CPS style, the foremost 
// concern in my mind should be how to emulate stacks properly and yet I did shit 
// in the respect. It is clear I do not understand it.

//let rec zip2 a b =
//    match a,b with
//    | S _, S _ -> R [a;b]
//    | R a, R b -> List.map2 (fun a b -> zip2 a b) a b |> R
//    | _ -> failwith "Incorrect pattern"
//
// Let me rewrite the above on CPS style first. The we'll talk

let rec zip2_cps a b ret =
    match a,b with
    | S _, S _ -> R [a;b] |> ret
    | R a, R b -> 
        let rec loop acc l = 
            match l with
            | a :: aa, b :: bb -> zip2_cps a b <| fun x -> loop (x :: acc) (aa, bb)
            | [], [] -> List.rev acc |> R |> ret
            | _ -> failwith "Incorrect pattern"
        loop [] (a, b)
    | _ -> failwith "Incorrect pattern"

//zip2_cps a b <| fun r -> printfn "%A" r
//
//let zip_all_cps_v1 l ret =
//    let recurse l ret = zip_all_cps_v1 l ret
//    let rec loop_s acc l' =
//        match l' with
//        | S l :: ls -> loop_s (S l :: acc) ls
//        | [] -> R (List.rev acc) |> ret
//        | R (x :: xs) :: ls -> loop_r [] l
//        | R [] :: ls -> loop_r_flat 
//    and loop_r acc_result acc_head acc_tail l' =
//        match l' with
//        | R (x::xs) :: ls -> loop_r acc_result (x :: acc_head) (xs :: acc_tail) ls
//        | R [] :: ls -> loop_r_flat acc_result l
//        | [] -> recurse (List.rev acc_head) <| fun r ->
//            loop_r (r :: acc_result) [] acc_tail

// This is impossible like the above.

let case_s l on_fail ret =
    let rec loop acc = function
        | (S _ as x) :: xs -> loop (x :: acc) xs
        | [] -> ret (List.rev acc)
        | _ -> on_fail()
    loop [] l

let case_r l on_fail ret =
    let rec loop acc_head acc_tail = function
        | (R (h :: t)) :: xs -> loop (h :: acc_head) (R t :: acc_tail) xs
        | [] -> ret (List.rev acc_head, List.rev acc_tail)
        | _ -> on_fail()
    loop [] [] l

let case_r_empty l on_fail ret =
    let rec loop = function
        | R [] :: xs -> loop xs
        | [] -> ret()
        | _ -> on_fail()
    loop l


let rec zip_all l zip_fail ret =
    case_s l 
        (fun _ ->
            let rec loop acc l =
                case_r l 
                    (fun _ -> 
                        case_r_empty l 
                            (fun _ -> zip_fail acc l |> R |> ret) 
                            (fun _ -> List.rev acc |> R |> ret))
                    (fun (head, tail) ->
                        zip_all head zip_fail <| fun r -> loop (r :: acc) tail)
            loop [] l)
        (R >> ret)

let zip_regular _ = failwith "Irregular tuples not allowed"
let zip_irregular acc l = List.rev acc @ l
    
//zip_all [a;b;c] zip_irregular id

let q = R [S "q"; S "w"]
let w = R [S "1"; S "2"]
let e = R [S "a"; S "s"]

let f x = zip_all x zip_irregular id 

let t1 = f [q;w]
let t2 = f [t1;e]

/// The zip function for irregular or regularly sized tuples.
let zip = function
    | R x -> zip_all x zip_irregular id
    | x -> x

/// The zip function for regularly sized tuples. Throws an exceptions if dimensions are mismatched.
let zip' = function
    | R x -> zip_all x zip_regular id
    | x -> failwith "Not a tuple."