open System
open System.Collections.Generic

type Ty = PrimT

type Node<'a>(expr:'a, symbol:int) = 
    member x.Expression = expr
    member x.Symbol = symbol

type TyTag = Node<int * Ty>
type TypedExpr = TyTag of TyTag

let d0() = Dictionary(HashIdentity.Structural)

let nodify (dict: Dictionary<_,_>) x =
    match dict.TryGetValue x with
    | true, id -> Node(x,id)
    | false, _ ->
        let id = dict.Count
        let x' = Node(x,id)
        dict.[x] <- id
        x'

let nodify_tytag = nodify <| d0()
let tytag x = 
    let x: TyTag = nodify_tytag x
    TyTag(x)