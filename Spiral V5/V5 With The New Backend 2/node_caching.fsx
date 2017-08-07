// https://stackoverflow.com/questions/45531771/how-do-i-cache-hash-codes-for-an-ast
// Based on the example by Tomas Petricek.

open System.Collections.Generic

type ListDictionaryNode<'K, 'T> = 
  { mutable Result : 'T option
    Nested : Dictionary<'K, ListDictionaryNode<'K, 'T>> }

type ListDictionary<'K, 'V> = Dictionary<'K, ListDictionaryNode<'K, 'V>>

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module ListDictionary = 
  let tryFind ks dict = 
    let rec loop ks node =
      match ks, node with
      | [], { Result = Some r } -> Some r
      | k::ks, { Nested = d } when d.ContainsKey k -> loop ks (d.[k])
      | _ -> None
    loop ks { Nested = dict; Result = None }

  let set ks v dict =
    let rec loop ks (dict:ListDictionary<_, _>) = 
      match ks with
      | [] -> failwith "Empty key not supported"
      | k::ks ->
          if not (dict.ContainsKey k) then 
            dict.[k] <- { Nested = Dictionary<_, _>(); Result = None }
          if List.isEmpty ks then dict.[k].Result <- Some v
          else loop ks (dict.[k].Nested)
    loop ks dict

let nextId = 
  let mutable id = 0
  fun () -> id <- id + 1; id

type Expr =
  | Add of ExprNode * ExprNode
  | Lit of int

and ExprNode(expr:Expr, symbol:int) = 
  member x.Expression = expr
  member x.Symbol = symbol
  override x.GetHashCode() = symbol
  override x.Equals(y) = 
    match y with 
    | :? ExprNode as y -> y.Symbol = x.Symbol
    | _ -> false

let node expr ctx =  
  // Get the key from the kind of the expression
  // and symbols of all nested node in this expression
  let key = 
    match expr with 
    | Lit n -> [0; n]
    | Add(e1, e2) -> [1; e1.Symbol; e2.Symbol]
  // Return either a node from cache or create a new one
  match ListDictionary.tryFind key ctx with
  | Some res -> res
  | None ->
      let res = ExprNode(expr, nextId())
      ListDictionary.set key res ctx
      res
