type Ty = 
    | Int
    | Float
    | Bool
    | Tuple of Ty list

type Expr = 
    | V of string
    | Inline of Expr * Expr list 
    | If of Expr * Expr * Expr
    | Inlineable of string list * Expr
    | Let of string * Expr * Expr // TODO: This should be a statement rather than an expression.
    | LitInt of int
    | LitFloat of float
    | LitBool of bool
    | T of Expr list // T stands for tuple

type TyV = string * Ty

type TypedExpr =
    | TyV of TyV
    | TyIf of TypedExpr * TypedExpr * TypedExpr * Ty
    | TyLet of TyV * TypedExpr * TypedExpr * Ty
    | TyLitInt of int
    | TyLitFloat of float
    | TyLitBool of bool
    | TyT of TypedExpr list

let rec get_type = function
    | TyV(_,t) | TyIf(_,_,_,t) | TyLet(_,_,_,t) -> t
    | TyLitInt _ -> Int
    | TyLitFloat _ -> Float
    | TyLitBool _ -> Bool
    | TyT l -> List.map get_type l |> Tuple

type ReturnCases =
    | RTypedExpr of TypedExpr
    | RExpr of Expr * EnvType
    | RError of string
and EnvType = Map<string,ReturnCases>

let rec teval (env: EnvType) inline_args exp: ReturnCases =
    match exp with
    | V x -> 
        match Map.tryFind x env with
        | Some (RTypedExpr _ as v) -> v
        | Some (RExpr(v,env)) -> teval env inline_args v
        | Some (RError _ as e) -> e
        | None -> RError "Variable not bound."
    | Inline(expr,args) ->
        teval env (args :: inline_args) expr
    | If(cond,tr,fl) as orig ->
        match teval env inline_args cond with
        | RTypedExpr cond' when get_type cond' = Bool ->
            match teval env inline_args tr, teval env inline_args fl with
            | RTypedExpr tr, RTypedExpr fl -> RTypedExpr <| TyIf(cond',tr,fl,get_type tr)
            | RExpr(tr',_), RExpr(fl',_) -> RExpr(If(cond,tr',fl'),env) // tr = tr', fl = fl'
            | a, b -> RError <| sprintf "Expected either both equal types or non-evaluated in conditional.\nGot true: %A\nGot false: %A" a b
        | x -> RError <| sprintf "Expected bool in conditionalal.\nGot: %A" x
    | Let(v,b,e) as orig ->
        match teval env [] b with
        | RTypedExpr b' as b'' ->
            let b'_type = get_type b'
            let v' = TyV(v,b'_type)
            match teval (Map.add v (RTypedExpr v') env) inline_args e with
            | RTypedExpr e' -> RTypedExpr(TyLet((v,b'_type),b',e',get_type e'))
            | RExpr(e',_) -> RExpr(Let(v,b,e'),env) // e = e'
            | RError er -> RError er
        | RExpr _ as b -> teval (Map.add v b env) inline_args e
        | RError _ as e -> e