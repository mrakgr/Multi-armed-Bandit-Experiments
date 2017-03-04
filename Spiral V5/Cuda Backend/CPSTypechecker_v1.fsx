type Ty =
    | Unit
    | Int
    | Float
    | Bool
    | TVar of string

type TyV = int64 * string * Ty

type Expr = 
    | V of string
    | If of Expr * Expr * Expr
    | Inlineable of string list * Expr * EnvType option
    | LitUnit
    | LitInt of int
    | LitFloat of float
    | LitBool of bool
    | Apply of Expr * args: Expr list

// This is being compiled to STLC, not System F, so no type variables are allowed in the processed AST.
and TypedExpr =
    | TyV of TyV
    | TyIf of TypedExpr * TypedExpr * TypedExpr * Ty
    | TyLet of TyV * TypedExpr * TypedExpr * Ty
    | TyUnit
    | TyLitInt of int
    | TyLitFloat of float
    | TyLitBool of bool

and ReturnCases =
    | RTypedExpr of TypedExpr
    | RExpr of Expr
    | RError of string

and EnvType = Map<string,ReturnCases>

let rec get_type = function
    | TyV(_,_,t) | TyIf(_,_,_,t) | TyLet(_,_,_,t) -> t
    | TyLitInt _ -> Int
    | TyLitFloat _ -> Float
    | TyLitBool _ -> Bool
    | TyUnit -> Unit

let get_tag =
    let mutable x = 0L
    fun () -> 
        let x' = x
        x <- x + 1L
        x'

type ArgCases = Expr list * EnvType

open System.Collections.Generic
type Data =
    {
    // Immutable
    env : EnvType
    args : ArgCases list
    }

type Result<'a,'b> = Succ of 'a | Fail of 'b

let d0() = {env=Map.empty;args=[]}

let inl x y = Inlineable(x,y,None)
let ap x y = Apply(x,y)

let term0 =
    let snd = inl ["a";"b"] (V "b")
    ap (inl ["x";"y";"z";"r"] (ap (V "r") [V "y";V "z"])) ([LitUnit;LitBool true;LitInt 5;snd])

let l v b e = Apply(Inlineable(v,e,None),b)

let term1 =
    let snd = inl ["a";"b"] (V "b")
    l ["x";"y";"z"] [LitUnit;LitBool true;LitInt 5] 
        (l ["q"] [snd] (ap (V "q") [V "y";V "z"]))