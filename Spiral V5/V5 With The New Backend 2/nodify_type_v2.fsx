﻿open System
open System.Collections.Generic

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

type Ty =
    | Int
    | String
    | Tuple of Ty list
    | Rec of Node<Ty>
    | Union of Ty list
    | Delayed of Lazy<Ty>

type NodeDict<'a> = Dictionary<'a,Node<'a>>

let get_nodify_tag =
    let mutable i = 0
    fun () -> i <- i+1; i

let nodify (dict: NodeDict<_>) x =
    match dict.TryGetValue x with
    | true, x -> x
    | false, _ ->
        let x' = Node(x,get_nodify_tag())
        dict.[x] <- x'
        x'

let d = Dictionary(HashIdentity.Structural)
let nodify_ty x = nodify d x

let rec int_string_stream = 
    Delayed <| lazy
        Union 
            [
            Tuple [Int; Rec (nodify_ty (int_string_stream))]
            Tuple [String; Rec (nodify_ty (int_string_stream))]
            ]

let rec int_string_stream2 = 
    Delayed <| lazy
        Union 
            [
            Tuple [Int; Rec (nodify_ty (int_string_stream2))]
            Tuple [String; Rec (nodify_ty (int_string_stream2))]
            ]
    
let x =
    match int_string_stream2 with
    | Delayed x -> 
        match x.Value with
        | Union [_;Tuple [_; Rec x]] -> x

match x.Expression with
| Delayed x -> x.Value

