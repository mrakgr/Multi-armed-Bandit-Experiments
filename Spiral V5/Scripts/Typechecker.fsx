// http://cs.brown.edu/courses/cs173/2012/book/types.html

type Number = int
type Symbol = string

type Type = 
| NumT
| FunT of Type * Type

type TyExprC =
| NumC of Number
| IdC of Symbol
| AppC of f: TyExprC * arg: TyExprC
| PlusC of TyExprC * TyExprC
| MultC of TyExprC * TyExprC
| LamC of arg: Symbol * argT: Type * retT: Type * body: TyExprC

type Env = Map<Symbol, Value>

and Value =
| NumV of Number
| ClosV of Symbol * TyExprC * Env

type TyEnv = Map<Symbol, Type>

let get_type_of n tenv =
    match Map.tryFind n tenv with
    | Some v -> v
    | None -> failwith "Identifier not found in the type environment."

let rec tc expr tenv = 
    match expr with
    | NumC _ -> NumT
    | IdC n -> get_type_of n tenv
    | PlusC (l,r) | MultC (l,r) -> 
        match tc l tenv, tc r tenv with
        | NumT, NumT -> NumT
        | _ -> failwith "not both numbers"
    | AppC (f,a) ->
        let ft, at = tc f tenv, tc a tenv
        match ft,at with
        | FunT(ft',at'), at -> if at' = at then ft else failwith "app arg mismatch"
        | _, _ -> failwith "not a function"
    | LamC (name,argT,retT,body) ->
        if retT = tc body (tenv.Add (name, argT)) then FunT(argT,retT)
        else failwith "lam type mismatch"

