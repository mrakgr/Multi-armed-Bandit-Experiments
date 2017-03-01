type Ty = 
    | Unit
    | Int
    | Float
    | Bool
    | Tuple of Ty list
    | TArr of Ty list * Ty

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
    | Apply of Expr * Expr list

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
    | TyApply of TypedExpr * TypedExpr list * Ty

let rec get_type = function
    | TyApply(_,_,t) | TyMethod(_,_,t) | TyV(_,_,t) | TyIf(_,_,_,t) | TyLet(_,_,_,t) -> t
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

open System.Collections.Generic
type Data =
    {
    // Immutable
    env : EnvType
    inline_args : (Expr list * EnvType) list
    apply_args : (Expr list * EnvType) list
    // Mutable
    global_methods : Dictionary<int64 * Ty list,string * TypedExpr> // For hoisted out global methods.
    }

type Result<'a,'b> = Succ of 'a | Fail of 'b

let d0() = {env=Map.empty;inline_args=[];apply_args=[];global_methods=Dictionary()}

let rec teval (d: Data) exp: ReturnCases =
    match exp with
    | V x -> 
        match Map.tryFind x d.env with
        | Some (RTypedExpr _ as v) -> v
        | Some (RExpr(v,env)) -> teval d v
        | Some (RError _ as e) -> e
        | None -> RError <| sprintf "Variable %A not bound." x
    | Inline(expr,args) ->
        teval {d with inline_args = (args,d.env) :: d.inline_args} expr
    | If(cond,tr,fl) as orig ->
        match teval d cond with
        | RTypedExpr cond' when get_type cond' = Bool ->
            match teval d tr, teval d fl with
            | RTypedExpr tr, RTypedExpr fl -> 
                if get_type tr = get_type fl then
                    RTypedExpr <| TyIf(cond',tr,fl,get_type tr)
                else
                    RError "Types in branches of if do not match."
            | RExpr(_,_), RExpr(_,_) -> RExpr(orig,d.env)
            | a, b -> RError <| sprintf "Expected either both equal types or non-evaluated in conditional.\nGot true: %A\nGot false: %A" a b
        | x -> RError <| sprintf "Expected bool in conditional.\nGot: %A" x
    | Inlineable(args,body) as inl ->
        match d.inline_args with
        | (cur_args,env'') :: other_args ->
            if args.Length = cur_args.Length then
                let env =
                    (args, cur_args) ||>
                    List.fold2 (fun env' arg cur_arg -> 
                        Map.add arg (teval {d with env=env''; inline_args=[]} cur_arg) env'
                        ) d.env
                teval {d with env=env; inline_args=other_args} body
            else
                RError "args.Length = cur_args.Length failed in the Inlineable case"
        | [] -> RExpr(inl,d.env)
    | Let(v,b,e) as orig ->
        match teval {d with inline_args=[]; apply_args=[]} b with
        | RTypedExpr b' ->
            let b'_type = get_type b'
            let v' = get_tag(),v,b'_type
            match teval {d with env=Map.add v (RTypedExpr <| TyV(v')) d.env} e with
            | RTypedExpr e' -> RTypedExpr(TyLet(v',b',e',get_type e'))
            | RExpr(e,_) -> RError <| sprintf "Got expression %A on the right side of a Let" e
            | RError er -> RError er
        | RExpr _ as b -> teval {d with env=Map.add v b d.env} e
        | RError _ as e -> e
    | LitInt x -> RTypedExpr (TyLitInt x)
    | LitFloat x -> RTypedExpr (TyLitFloat x)
    | LitBool x -> RTypedExpr (TyLitBool x)
    | T ls as orig ->
        let rec loop acc = function
            | l::ls ->
                match teval d l with
                | RTypedExpr x -> loop (x::acc) ls
                | RExpr _ as x -> RExpr (orig, d.env)
                | RError _ as x -> x
            | [] -> RTypedExpr (TyT (List.rev acc))
        loop [] ls

//    | Method of string list * Expr
//    | Apply of Expr * Expr list

//    | TyMethod of TyV list * TypedExpr * Ty
//    | TyApply of TypedExpr * TypedExpr list * Ty

let inl x y = Inlineable(x,y)
let inap x y = Inline(x,y)
let l x b i = Let(x,b,i)

let teval1 x = teval (d0()) x

// Assorted tests
let term1 = inap (inl ["x";"y"] (V "x")) [LitInt 1; LitInt 2] 
let t1 = teval1 term1

let term2 =
    l "inlineable" 
        (inl ["x";"y"] (T [V "x";V "y"]))
        (T [(inap (V "inlineable") [LitInt 1; LitInt 2]); 
            (inap (V "inlineable") [LitFloat 1.5; LitInt 2])])
let t2 = teval1 term2

let term3 =
    l "inlineable" 
        (inl ["x";"y"] (T [V "x";V "y"]))
        (l "fun" 
            (inl ["inl";"a";"b";"c";"d"] 
                (T [inap (V "inl") [V "a";V "b"];
                    inap (V "inl") [V "c";V "d"]]))
            (inap (V "fun") [V "inlineable"; LitInt 1; LitInt 2; LitFloat 1.5; LitInt 2]))
let t3 = teval1 term3

let term4 = 
    l "a" (LitInt 2)
        (l "b" (LitBool true)
            (T [V "a"; V "b"]))
let t4 = teval1 term4

let term5 = // If test
    l "if" (inl ["cond";"tr";"fl"] (If(V "cond",V "tr",V "fl")))
        (l "cond" (LitBool true)
            (l "tr" (LitFloat 3.33)
                (l "fl" (LitFloat 4.44)
                    (inap (V "if") [V "cond";V "tr";V "fl"]))))
let t5 = teval1 term5

let term6 = // Error in conditional
    l "if" (inl ["cond";"tr";"fl"] (If(V "cond",V "tr",V "fl")))
        (l "cond" (LitInt 2)
            (l "tr" (LitFloat 3.33)
                (l "fl" (LitFloat 4.44)
                    (inap (V "if") [V "cond";V "tr";V "fl"]))))
let t6 = teval1 term6

let term7 = // Error in branches
    l "if" (inl ["cond";"tr";"fl"] (If(V "cond",V "tr",V "fl")))
        (l "cond" (LitBool true)
            (l "tr" (LitInt 3)
                (l "fl" (LitFloat 4.44)
                    (inap (V "if") [V "cond";V "tr";V "fl"]))))
let t7 = teval1 term7

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
                
let t8 = teval1 term8