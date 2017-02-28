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
        | None -> RError <| sprintf "Variable %A not bound." x
    | Inline(expr,args) ->
        teval env ((args,env) :: inline_args) expr
    | If(cond,tr,fl) as orig ->
        match teval env inline_args cond with
        | RTypedExpr cond' when get_type cond' = Bool ->
            match teval env inline_args tr, teval env inline_args fl with
            | RTypedExpr tr, RTypedExpr fl -> 
                if get_type tr = get_type fl then
                    RTypedExpr <| TyIf(cond',tr,fl,get_type tr)
                else
                    RError "Types in branches of if do not match."
            | RExpr(_,_), RExpr(_,_) -> RExpr(orig,env)
            | a, b -> RError <| sprintf "Expected either both equal types or non-evaluated in conditional.\nGot true: %A\nGot false: %A" a b
        | x -> RError <| sprintf "Expected bool in conditional.\nGot: %A" x
    | Inlineable(args,body) as inl ->
        match inline_args with
        | (cur_args,env'') :: other_args ->
            if args.Length = cur_args.Length then
                let env =
                    (args, cur_args) ||>
                    List.fold2 (fun env' arg cur_arg -> 
                        Map.add arg (teval env'' [] cur_arg) env'
                        ) env
                teval env other_args body
            else
                RError "args.Length = cur_args.Length failed in the Inlineable case"
        | [] -> RExpr(inl,env)
    | Let(v,b,e) as orig ->
        match teval env [] b with
        | RTypedExpr b' ->
            let b'_type = get_type b'
            let v' = TyV(v,b'_type)
            match teval (Map.add v (RTypedExpr v') env) inline_args e with
            | RTypedExpr e' -> RTypedExpr(TyLet((v,b'_type),b',e',get_type e'))
            | RExpr(_,_) -> RExpr(orig,env)
            | RError er -> RError er
        | RExpr _ as b -> teval (Map.add v b env) inline_args e
        | RError _ as e -> e
    | LitInt x -> RTypedExpr (TyLitInt x)
    | LitFloat x -> RTypedExpr (TyLitFloat x)
    | LitBool x -> RTypedExpr (TyLitBool x)
    | T ls as orig ->
        let rec loop acc = function
            | l::ls ->
                match teval env inline_args l with
                | RTypedExpr x -> loop (x::acc) ls
                | RExpr _ as x -> RExpr (orig, env)
                | RError _ as x -> x
            | [] -> RTypedExpr (TyT acc)
        loop [] ls

let inl x y = Inlineable(x,y)
let inap x y = Inline(x,y)
let l x b i = Let(x,b,i)

// Some tests
let term1 = inap (inl ["x";"y"] (V "x")) [LitInt 1; LitInt 2] 
let t1 = teval Map.empty [] term1

let term2 =
    l "inlineable" 
        (inl ["x";"y"] (T [V "x";V "y"]))
        (T [(inap (V "inlineable") [LitInt 1; LitInt 2]); 
            (inap (V "inlineable") [LitFloat 1.5; LitInt 2])])
let t2 = teval Map.empty [] term2

let term3 =
    l "inlineable" 
        (inl ["x";"y"] (T [V "x";V "y"]))
        (l "fun" 
            (inl ["inl";"a";"b";"c";"d"] 
                (T [inap (V "inl") [V "a";V "b"];
                    inap (V "inl") [V "c";V "d"]]))
            (inap (V "fun") [V "inlineable"; LitInt 1; LitInt 2; LitFloat 1.5; LitInt 2]))
let t3 = teval Map.empty [] term3