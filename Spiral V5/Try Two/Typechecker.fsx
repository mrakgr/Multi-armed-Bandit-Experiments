type Ty = 
    | Int
    | Float
    | Bool

type Expr = 
    | Var of string
    | App of Expr * Expr
    | If of Expr * Expr * Expr
    | Lam of string * Expr
    | Let of string * Expr * Expr
    | LitInt of int
    | LitFloat of float
    | LitBool of bool

type TyEvalResult =
    | Ty of Ty
    | Expr of Expr // Supposed to be only Lam. Otherwise it would lead to the language having unhigienic macros.

type EnvType = Map<string,TyEvalResult>

let rec teval (env: EnvType) exp =
    match exp with
    | Var x -> 
        match Map.tryFind x env with
        | Some x -> x
        | None -> failwith "Variable not bound."
    | App(l,r) ->
        let l,r = teval env l, teval env r
        match l,r with
        | Expr(Lam(x,b)), (Ty _ as t) -> teval (Map.add x t env) b
        | _ -> failwithf "Expected: lambda, type\nGot: %A and %A instead" l r
    | If(cond,tr,fl) ->
        match teval env cond with
        | Ty Bool -> ()
        | cond -> failwithf "Expected: Bool\nGot: %A" cond
        match teval env tr, teval env fl with
        | Ty tr, Ty fl when tr = fl -> Ty tr
        | tr, fl -> failwithf "Expected: Ty tr = Ty fl\nGot: %A = %A" tr fl
    | Let(x,body,in_) ->
        let body = teval env body
        teval (Map.add x body env) in_
    | LitInt _ -> Ty Int
    | LitFloat _ -> Ty Float
    | LitBool _ -> Ty Bool
    | Lam _ as x -> Expr x

let lam x y = Lam(x,y)
let app x y = App(y,x)

// Some tests
let term1 = lam "x" (lam "y" (Var "x")) |> app (LitInt 1) |> app (LitInt 2)
let t1 = teval Map.empty term1