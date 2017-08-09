open System
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

let rec int_string_stream () = 
    Union 
        [
        Tuple [Int; Rec (nodify_ty (int_string_stream ()))]
        Tuple [String; Rec (nodify_ty (int_string_stream ()))]
        ]
    
