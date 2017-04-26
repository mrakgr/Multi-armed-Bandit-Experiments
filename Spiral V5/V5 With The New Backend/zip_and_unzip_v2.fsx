// 4/25/2017.

// I am still wrestling with this. Let me factor out the transpose and I will wipe my hands of this.
// Though that is not the problem, I opened a question on SO asking whether this stuff is isomorphic.

// http://stackoverflow.com/questions/43603903/on-heterogenous-lists-is-it-possible-to-make-the-zip-and-then-unzip-equal-to-th

// Edit: No, no matter what I try I cannot figure out how to make the pair isomorphic for irregularly sized arrays.
// On the upside, based on the tests here, I am close to 100% sure that it should be isomorphic for all regularly sizes tuples.

// I've tightened the code up as much as I could, but I have no idea anymore. Maybe there is some another condition in 
// unzip that I am missing in unzip, but at any rate, I can't see it right now.

#r "../../packages/FsCheck.2.8.0/lib/net452/FsCheck.dll"

type T =
| S of string
| VV of T list

let transpose l on_fail on_succ =
    let is_all_vv_empty x = List.forall (function VV [] -> true | _ -> false) x
    let rec loop acc_total acc_head acc_tail = function
        | VV [] :: ys -> 
            if List.isEmpty acc_head && is_all_vv_empty ys then 
                if List.isEmpty acc_total then failwith "Empty inputs in the inner dimension to transpose are invalid."
                else List.rev acc_total |> on_succ
            else on_fail ()
        | VV (x :: xs) :: ys -> loop acc_total (x :: acc_head) (VV xs :: acc_tail) ys
        | _ :: _ -> on_fail ()
        | [] -> 
            match acc_tail with
            | _ :: _ -> loop (VV (List.rev acc_head) :: acc_total) [] [] (List.rev acc_tail)
            | _ -> List.rev acc_total |> on_succ
    loop [] [] [] l

let rec zip l = 
    match l with
    | _ :: _ -> transpose l (fun _ -> l) (List.map (function VV x -> zip x | x -> x)) |> VV
    | _ -> failwith "Empty input to zip is invalid."

let rec unzip l =
    let is_all_vv x = List.forall (function VV _ -> true | _ -> false) x
    match l with
    | VV x ->
        match x with
        | _ :: _ when is_all_vv x -> let t = List.map (unzip >> VV) x in transpose t (fun _ -> x) id
        | _ :: _ -> x
        | _ -> failwith "Empty inputs to unzip are invalid."
    | S _ -> failwith "Unzip called on S."

open FsCheck
open System

let gen_t =
    let mutable gen_t = None
    let gen_s () = Gen.map S Arb.generate<string>
    let gen_vv size = Gen.nonEmptyListOf (gen_t.Value size) |> Gen.map VV
    gen_t <- 
        fun size -> 
            match size with
            | 0 -> gen_s()
            | _ when size > 0 -> Gen.oneof [gen_s (); gen_vv (size-1)] 
            | _ -> failwith "impossible" 
        |> Some
    gen_t.Value
    |> Gen.sized

let gen_t_list_irregular = Gen.nonEmptyListOf gen_t
let gen_t_list_regular = Gen.map2 List.replicate (Gen.choose(1,10)) gen_t

type MyGenerators =
    static member Tuple() = Arb.fromGen gen_t
    static member TupleList() = Arb.fromGen gen_t_list_regular
Arb.register<MyGenerators>()

let zip_and_unzip orig = zip orig |> unzip
let zip_and_unzip_eq_orig orig = zip_and_unzip orig = orig

// For regular tuples it passes with flying colors.

Check.One ({Config.Quick with EndSize = 2}, zip_and_unzip_eq_orig)

// I can't get it to be isomorphic for irregularly sized arrays as expected.

let f x = 
    let x' = zip x
    printfn "after_zip=%A" x'
    printfn "after_zip_unzip=%A" (unzip x')
    printfn "zip_and_unzip_eq_orig x=%A" (zip_and_unzip_eq_orig x)

let fail_case = [VV [VV [S "12"; S "qwe"]]; VV [VV [S ""; S "ug"]; VV [S ""]]]
let fail_after_zip = zip fail_case
let fail_after_zip_unzip = unzip fail_after_zip
let fail_fater_zip_unzip_eq_orig = fail_case = fail_after_zip_unzip

let succ_case = [VV [VV [S "12"; S "qwe"]]; VV [VV [S ""; S "ug"]]]
let succ_after_zip = zip succ_case
let succ_after_zip_unzip = unzip succ_after_zip
let succ_fater_zip_unzip_eq_orig = succ_case = succ_after_zip_unzip

f [VV [VV [S ""; S ""]; VV [S "v"; S ""]]]
