type Ty = 
    | Unit
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
    | Method of string list * Expr
    | Annotate of Expr * Expr list

type TyV = int64 * string * Ty

type TypedExpr =
    | TyV of TyV
    | TyIf of TypedExpr * TypedExpr * TypedExpr * Ty
    | TyLet of TyV * TypedExpr * TypedExpr * Ty
    | TyLitInt of int
    | TyLitFloat of float
    | TyLitBool of bool
    | TyT of TypedExpr list
    | TyMethod of TyV list * TypedExpr * Ty

let rec get_type = function
    | TyMethod(_,_,t) | TyV(_,_,t) | TyIf(_,_,_,t) | TyLet(_,_,_,t) -> t
    | TyLitInt _ -> Int
    | TyLitFloat _ -> Float
    | TyLitBool _ -> Bool
    | TyT l -> List.map get_type l |> Tuple
    
type ReturnCases =
    | RTypedExpr of TypedExpr
    | RExpr of Expr * EnvType
    | RError of string
and EnvType = Map<string,ReturnCases>

let get_tag =
    let mutable x = 0L
    fun () -> 
        let x' = x
        x <- x + 1L
        x'

let rec teval (env: EnvType) inline_args (apply_args: (string list * EnvType) list) exp: ReturnCases =
    match exp with
    | V x -> 
        match Map.tryFind x env with
        | Some (RTypedExpr _ as v) -> v
        | Some (RExpr(v,env)) -> teval env inline_args apply_args v
        | Some (RError _ as e) -> e
        | None -> RError <| sprintf "Variable %A not bound." x
    | Inline(expr,args) ->
        teval env ((args,env) :: inline_args) apply_args expr
    | If(cond,tr,fl) as orig ->
        match teval env inline_args apply_args cond with
        | RTypedExpr cond' when get_type cond' = Bool ->
            match teval env inline_args apply_args tr, teval env inline_args apply_args fl with
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
                        Map.add arg (teval env'' [] apply_args cur_arg) env'
                        ) env
                teval env other_args apply_args body
            else
                RError "args.Length = cur_args.Length failed in the Inlineable case"
        | [] -> RExpr(inl,env)
    | Let(v,b,e) as orig ->
        match teval env [] apply_args b with
        | RTypedExpr b' ->
            let b'_type = get_type b'
            let v' = get_tag(),v,b'_type
            match teval (Map.add v (RTypedExpr <| TyV(v')) env) inline_args apply_args e with
            | RTypedExpr e' -> RTypedExpr(TyLet(v',b',e',get_type e'))
            | RExpr(_,_) -> RExpr(orig,env)
            | RError er -> RError er
        | RExpr _ as b -> teval (Map.add v b env) inline_args apply_args e
        | RError _ as e -> e
    | LitInt x -> RTypedExpr (TyLitInt x)
    | LitFloat x -> RTypedExpr (TyLitFloat x)
    | LitBool x -> RTypedExpr (TyLitBool x)
    | T ls as orig ->
        let rec loop acc = function
            | l::ls ->
                match teval env inline_args apply_args l with
                | RTypedExpr x -> loop (x::acc) ls
                | RExpr _ as x -> RExpr (orig, env)
                | RError _ as x -> x
            | [] -> RTypedExpr (TyT (List.rev acc))
        loop [] ls
    | Method(args,body) ->
        match apply_args with
        | (cur_args,env'') :: other_args ->
            if args.Length = cur_args.Length then
                let rec loop acc = function
                    | l::ls ->
                        match Map.tryFind l env'' with
                        | Some(RTypedExpr _ as x) -> loop (Map.add l x acc) ls
                        | _ -> None
                    | [] -> Some acc
                match loop Map.empty cur_args with
                | Some env -> 
                    let body = teval env [] [] body // This is a significant restriction...
                    RTypedExpr(TyMethod(cur_args,body,get_type body))
            else
                RError "args.Length = cur_args.Length failed in the Method case"
        | [] -> RError "No arguments to apply to the Method"

let inl x y = Inlineable(x,y)
let inap x y = Inline(x,y)
let l x b i = Let(x,b,i)

// Assorted tests
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

let term4 = 
    l "a" (LitInt 2)
        (l "b" (LitBool true)
            (T [V "a"; V "b"]))
let t4 = teval Map.empty [] term4

let term5 = // If test
    l "if" (inl ["cond";"tr";"fl"] (If(V "cond",V "tr",V "fl")))
        (l "cond" (LitBool true)
            (l "tr" (LitFloat 3.33)
                (l "fl" (LitFloat 4.44)
                    (inap (V "if") [V "cond";V "tr";V "fl"]))))
let t5 = teval Map.empty [] term5

let term6 = // Error in conditional
    l "if" (inl ["cond";"tr";"fl"] (If(V "cond",V "tr",V "fl")))
        (l "cond" (LitInt 2)
            (l "tr" (LitFloat 3.33)
                (l "fl" (LitFloat 4.44)
                    (inap (V "if") [V "cond";V "tr";V "fl"]))))
let t6 = teval Map.empty [] term6

let term7 = // Error in branches
    l "if" (inl ["cond";"tr";"fl"] (If(V "cond",V "tr",V "fl")))
        (l "cond" (LitBool true)
            (l "tr" (LitInt 3)
                (l "fl" (LitFloat 4.44)
                    (inap (V "if") [V "cond";V "tr";V "fl"]))))
let t7 = teval Map.empty [] term7

let term8 = // Hygiene test
    l "f" 
        (inl ["g"] 
            (l "a" (LitInt 2)
                (l "b" (LitBool true)
                    (T [(inap (V "g") []);V "a";V "b"]))))
        (l "g" (inl [] 
            (l "a" (LitFloat 4.4)
                (l "b" (LitInt 4) (T [V "a"; V "b"]))))
            (inap (V "f") [V "g"]))
                
let t8 = teval Map.empty [] term8