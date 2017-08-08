open System
open System.Collections.Generic

let inline test dict_create dict_add dict_clear m m' =
    let d = dict_create()
    let l = Array.init 20000 m
    let l' = Array.init 20000 m'

    let s = System.Diagnostics.Stopwatch.StartNew()
    Array.iter (fun x -> dict_add x 0 d) l
    let e1 = s.Elapsed

    dict_clear d

    s.Restart()
    Array.iter (fun x -> dict_add x 0 d) l'
    let e2 = s.Elapsed
    e1,e2

//let sorted_dict_int_list_timings =
//    test ((fun _ -> SortedDictionary())) (fun k v d -> d.Add(k,v)) (fun d -> d.Clear())
//        (fun x -> x :: List.replicate 30 1) (fun x -> List.replicate 30 1 @ [x])
//
//let sorted_dict_string_timings =
//    test ((fun _ -> SortedDictionary())) (fun k v d -> d.Add(k,v)) (fun d -> d.Clear())
//        (fun x -> string x + String.replicate 30 " ") (fun x -> String.replicate 30 " " + string x)
//
//let dict_int_list_timings =
//    test ((fun _ -> Dictionary())) (fun k v d -> d.Add(k,v)) (fun d -> d.Clear())
//        (fun x -> x :: List.replicate 30 1) (fun x -> List.replicate 30 1 @ [x])
//
//let dict_string_timings =
//    test ((fun _ -> Dictionary())) (fun k v d -> d.Add(k,v)) (fun d -> d.Clear())
//        (fun x -> string x + String.replicate 30 " ") (fun x -> String.replicate 30 " " + string x)

type Node<'a>(expr:'a, symbol:int) = 
    member x.Expression = expr
    member x.Symbol = symbol
    override x.GetHashCode() = symbol
    override x.Equals(y) = 
        match y with 
        | :? Node<'a> as y -> symbol = y.Symbol
        | _ -> failwith "Invalid equality for Node."

    interface IComparable with
        member x.CompareTo(y) = 
            match y with
            | :? Node<'a> as y -> compare symbol y.Symbol
            | _ -> failwith "Invalid comparison for Node."

type HashedList<'a> =
    | Nil
    | Cons of Node<'a * HashedList<'a>>

type NodeDict<'a> = Dictionary<'a,Node<'a>>

let get_tag =
    let mutable i = 0
    fun () -> i <- i+1; i

let nodify (dict: NodeDict<_>) x =
    match dict.TryGetValue x with
    | true, x -> x
    | false, _ ->
        let x' = Node(x,get_tag())
        dict.[x] <- x'
        x'

let nodify_int_list = nodify <| Dictionary()

let nil = Nil
let cons (x: int) xs = Cons <| nodify_int_list (x,xs)

let (|N|) (x: Node<_>) = x.Expression

let rec replicate n x = if n > 0 then cons x (replicate (n-1) x) else Nil
let rec append a b =
    match a with
    | Nil -> b
    | Cons(N(x,xs)) -> cons x (append xs b)

let dict_int_list_timings =
    test ((fun _ -> Dictionary())) (fun k v d -> d.Add(k,v)) (fun d -> d.Clear())
        (fun x -> x :: List.replicate 30 1) (fun x -> List.replicate 30 1 @ [x])

let dict_hashed_int_list_timings =
    test ((fun _ -> Dictionary())) (fun k v d -> d.Add(k,v)) (fun d -> d.Clear())
        (fun x -> cons x (replicate 30 1)) (fun x -> append (replicate 30 1) (cons x Nil))