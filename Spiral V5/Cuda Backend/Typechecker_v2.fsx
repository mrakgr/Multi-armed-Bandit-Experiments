type Ty =
    | Unit
    | Int
    | Float
    | Bool
    | Tuple of Ty list
    | TArr of arg_types: Ty list * return_type: Ty // No type variables allowed in instantiated types. Prenex polymorhism only for now.
    | TVar of string

type TyV = int64 * string * Ty

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
    | Method of tag: int64 option * type_vars: Ty list * args: (string * Ty) list * body: Expr * return_type: Ty
    | ApplyType of Expr * type_vars: Ty list
    | Apply of Expr * args: Expr list
    // Better to have these than backtracking.
    | SuspendedLet of TyV * TypedExpr * Expr
    | SuspendedIf of TypedExpr * Expr * Expr
    | SuspendedInlineable of string list * Expr * EnvType

// This is being compiled to STLC, not System F, so no type variables are allowed in the processed AST.
and TypedExpr =
    | TyV of TyV
    | TyIf of TypedExpr * TypedExpr * TypedExpr * Ty
    | TyLet of TyV * TypedExpr * TypedExpr * Ty
    | TyLitInt of int
    | TyLitFloat of float
    | TyLitBool of bool
    | TyT of TypedExpr list
    | TyMethod of tag: int64 * TyV list * TypedExpr * whole_type: Ty // Not just the return type. The TypedExpr body has the return type.
    | TyApply of TypedExpr * TypedExpr list * Ty

and ReturnCases =
    | RTypedExpr of TypedExpr
    | RExpr of Expr
    | RError of string

and EnvType = Map<string,ReturnCases>

type TypeEnvType = Map<string,Ty>

let rec get_type = function
    | TyApply(_,_,t) | TyMethod(_,_,_,t) | TyV(_,_,t) | TyIf(_,_,_,t) | TyLet(_,_,_,t) -> t
    | TyLitInt _ -> Int
    | TyLitFloat _ -> Float
    | TyLitBool _ -> Bool
    | TyT l -> List.map get_type l |> Tuple

let get_tag =
    let mutable x = 0L
    fun () -> 
        let x' = x
        x <- x + 1L
        x'

type ArgCases =
    | InlineArgs of Expr list * EnvType
    | ApplyTypeArgs of Ty list * TypeEnvType

open System.Collections.Generic
type Data =
    {
    // Immutable
    env : EnvType
    type_env: TypeEnvType
    args : ArgCases list
    // Mutable
    memoized_functions : Dictionary<int64 * Ty list,TypedExpr> // For hoisted out global methods.
    }

type Result<'a,'b> = Succ of 'a | Fail of 'b

let d0() = {env=Map.empty;type_env=Map.empty;args=[];memoized_functions=Dictionary()}

let rec teval (d: Data) exp: ReturnCases =
    let eval_other_let v' b' e d =
        match teval d e with
        | RTypedExpr e' -> RTypedExpr(TyLet(v',b',e',get_type e'))
        // If there aren't enough arguments to apply to the final expression, just save it for later evaluation.
        | RExpr e -> RExpr(SuspendedLet(v',b',e))
        | RError er -> RError er

    let eval_if_branches cond' tr fl d =
        match teval d tr, teval d fl with
        | RTypedExpr tr, RTypedExpr fl -> 
            if get_type tr = get_type fl then
                RTypedExpr <| TyIf(cond',tr,fl,get_type tr)
            else
                RError "Types in branches of if do not match."
        | RExpr tr, RExpr fl -> RExpr(SuspendedIf(cond',tr,fl))
        | a, b -> RError <| sprintf "Expected either both equal types or non-evaluated in conditional.\nGot true: %A\nGot false: %A" a b

    let eval_inlineable (args: string list) body env =
        match d.args with
        | InlineArgs (cur_args,env'') :: other_args ->
            if args.Length = cur_args.Length then
                let env =
                    (args, cur_args) ||>
                    List.fold2 (fun env' arg cur_arg -> 
                        Map.add arg (teval {d with env=env''; args=[]} cur_arg) env'
                        ) env
                teval {d with env=env; args=other_args} body
            else
                RError "args.Length = cur_args.Length failed in the Inlineable case"
        | x :: other_args -> RError "Unexpected arguments in Inlineable."
        | [] -> RExpr(SuspendedInlineable(args,body,d.env))

    match exp with
    | V x -> 
        match Map.tryFind x d.env with
        | Some (RTypedExpr _ as v) -> v
        | Some (RExpr v) -> teval d v
        | Some (RError _ as e) -> e
        | None -> RError <| sprintf "Variable %A not bound." x
    | Inline(expr,args) ->
        teval {d with args = InlineArgs (args,d.env) :: d.args} expr
    | If(cond,tr,fl) ->
        match teval d cond with
        | RTypedExpr cond' when get_type cond' = Bool -> 
            eval_if_branches cond' tr fl d
        | x -> RError <| sprintf "Expected bool in conditional.\nGot: %A" x
    | SuspendedIf(cond',tr,fl) -> eval_if_branches cond' tr fl d
    | Inlineable(args,body) -> eval_inlineable args body d.env
    | SuspendedInlineable(args,body,env) -> eval_inlineable args body env
    | Let(v,b,e) ->
        match teval {d with args=[]} b with
        | RTypedExpr b' ->
            let b'_type = get_type b'
            let v' = get_tag(),v,b'_type
            eval_other_let v' b' e {d with env=Map.add v (RTypedExpr <| TyV v') d.env}
        | RExpr _ as b -> teval {d with env=Map.add v b d.env} e
        | RError _ as e -> e
    | SuspendedLet(v',b',e) -> eval_other_let v' b' e d
    | LitInt x -> 
        match d.args with
        | [] -> RTypedExpr (TyLitInt x)
        | _ -> RError "Cannot apply a int literal."
    | LitFloat x -> 
        match d.args with
        | [] -> RTypedExpr (TyLitFloat x)
        | _ -> RError "Cannot apply a float literal."
    | LitBool x -> 
        match d.args with
        | [] -> RTypedExpr (TyLitBool x)
        | _ -> RError "Cannot apply a bool literal."
    | T ls as orig ->
        match d.args with
        | [] ->
            let rec loop acc = function
                | l::ls ->
                    match teval d l with
                    | RTypedExpr x -> loop (x::acc) ls
                    | RExpr _ as x -> RError "Only typed expressions are allowed in tuples."
                    | RError _ as x -> x
                | [] -> RTypedExpr (TyT (List.rev acc))
            loop [] ls
        | _ -> RError "Tuples can't be applied."
    | Method(None, type_vars, args, body, return_type) ->
        teval d (Method(Some <| get_tag(), type_vars, args, body, return_type))
    | Method(Some tag, type_vars, args, body, return_type) as orig ->
        match d.args with
        | [] -> RExpr orig
        | ApplyTypeArgs (cur_vars,tenv'') :: other_args ->
            let tag_and_cur_vars = tag,cur_vars
            match d.memoized_functions.TryGetValue tag_and_cur_vars with
            | true, v -> RTypedExpr v
            | false, _ ->
                let rec create_map_from_tvars_to_cvars acc = function
                    | TVar tv :: tvs, cv :: cvs -> create_map_from_tvars_to_cvars (Map.add tv cv acc) (tvs,cvs)
                    | _, cv :: cvs -> Fail "Only type variable declarations are allowed in the type_vars field of the Method definition."
                    | [], [] -> Succ acc
                    | _ -> Fail "Declaration and application argument lengths do not match in the Method case."
                 
                match create_map_from_tvars_to_cvars tenv'' (type_vars,cur_vars) with
                | Fail er -> RError er
                | Succ tenv -> 
                    let rec substitute_tvars_with_cvars l = 
                        let rest t n xs =
                            match substitute_tvars_with_cvars xs with
                            | Some y -> Some ((get_tag(),n,t) :: y)
                            | None -> None
                        match l with
                        | (n, TVar t) :: xs ->
                            match Map.tryFind t tenv with
                            | Some t -> rest t n xs
                            | None -> None
                        | (n, t) :: xs -> rest t n xs
                        | [] -> Some []

                    let return_type = 
                        match return_type with
                        | TVar t -> Map.tryFind t tenv
                        | t -> Some t

                    match substitute_tvars_with_cvars args, return_type with
                    | Some (args : TyV list), Some return_type ->
                        let whole_type =
                            let args = List.map (fun (_,_,x) -> x) args
                            TArr(args,return_type)
                        match teval {d with type_env=tenv} body with
                        | RTypedExpr body when get_type body = return_type -> 
                            let x = TyMethod(get_tag(),args,body,whole_type)
                            d.memoized_functions.Add(tag_and_cur_vars,x)
                            RTypedExpr x
                        | RTypedExpr body -> RError "Body's return type in the method evaluation must equal the annotated type."
                        | RExpr _ -> RError "The method must be fully evaluated."
                        | RError _ as er -> er
                    | None, _ -> RError "Argument substitution failed in the Method evaluation."
                    | _, None -> RError "Return type substitution failed in the Method evaluation."
        | _ -> RError "Incorrect argument type on the argument stack."
    | ApplyType(body,type_vars) ->
        teval {d with args=ApplyTypeArgs(type_vars,d.type_env) :: d.args} body
    | Apply(body,vars) ->
        let rec get_typed_vars = function
            | v :: vs ->
                match teval {d with args=[]} v with
                | RTypedExpr v -> 
                    match get_typed_vars vs with
                    | Succ vs -> Succ <| v :: vs
                    | Fail _ as er -> er
                | RExpr _ -> Fail "An argument in Apply returned an Expr."
                | RError er -> Fail er
            | [] -> Succ []
        let get_typedvar_types x = List.map get_type x
        
        match teval d body with
        | RError _ as er -> er
        | RExpr expr -> 
            match get_typed_vars vars with
            | Succ vars ->  teval d (ApplyType(expr,get_typedvar_types vars))
            | Fail er -> RError er
        | RTypedExpr f ->
            match get_type f with
            | TArr(arg_types,ret) ->
                match get_typed_vars vars with
                | Succ vars -> 
                    let var_types = get_typedvar_types vars 
                    let rec loop = function
                        | ar :: ars, v :: vs when ar = v -> loop (ars,vs)
                        | ar :: ars, v :: vs -> 
                            Fail "An argument in Apply is not of correct type."
                        | _ -> 
                            Fail "The number of arguments in a function does match the number being applied."
                        | [], [] -> Succ ()
                    match loop (arg_types,var_types) with
                    | Succ () -> RTypedExpr(TyApply(f,vars,ret))
                    | Fail er -> RError er
                | Fail er -> RError er
            | _ -> RError "The expression being applied is not a function."

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