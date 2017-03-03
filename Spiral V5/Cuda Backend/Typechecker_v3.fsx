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
    | Inlineable of string list * Expr
    | Let of string list * Expr * Expr
    | LitInt of int
    | LitFloat of float
    | LitBool of bool
    | T of Expr list // T stands for tuple
    | Method of tag: int64 option * args: Expr list * body: Expr * return_type: Ty option
    | Apply of Expr * args: Expr list

// This is being compiled to STLC, not System F, so no type variables are allowed in the processed AST.
type TypedExpr =
    | TyV of TyV
    | TyIf of TypedExpr * TypedExpr * TypedExpr * Ty
    | TyLet of TyV * TypedExpr * TypedExpr * Ty
    | TyLitInt of int
    | TyLitFloat of float
    | TyLitBool of bool
    | TyMethod of tag: int64 * TyV list * TypedExpr * whole_type: Ty // Not just the return type. The TypedExpr body has the return type.
    | TyApply of TypedExpr * TypedExpr list * Ty

type ReturnCases =
    | RTypedExpr of TypedExpr
    | RDual of TypedExpr * Expr // Having Dual in the environment is considered an error.
    | RExpr of Expr
    | RError of string

type EnvType = Map<string,ReturnCases>

let rec get_type = function
    | TyApply(_,_,t) | TyMethod(_,_,_,t) | TyV(_,_,t) | TyIf(_,_,_,t) | TyLet(_,_,_,t) -> t
    | TyLitInt _ -> Int
    | TyLitFloat _ -> Float
    | TyLitBool _ -> Bool

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
    // Mutable
    memoized_functions : Dictionary<int64 * int64 list * Ty list * Ty, Expr * TypedExpr option> // For hoisted out global methods.
    }

type Result<'a,'b> = Succ of 'a | Fail of 'b

let d0() = {env=Map.empty;args=[];memoized_functions=Dictionary()}

let rec teval (d: Data) exp: ReturnCases =
    match exp with
    | V x -> 
        match Map.tryFind x d.env with
        | Some (RTypedExpr _ as v) -> v
        | Some (RDual _) -> RError "A Dual in the environment is an error."
        | Some (RExpr v) -> teval d v
        | Some (RError _ as e) -> e
        | None -> RError <| sprintf "Variable %A not bound." x
    | If(cond,tr,fl) ->
        match teval d cond with
        | RTypedExpr cond' when get_type cond' = Bool -> 
            match teval d tr, teval d fl with
            | RTypedExpr tr, RTypedExpr fl -> 
                if get_type tr = get_type fl then
                    RTypedExpr <| TyIf(cond',tr,fl,get_type tr)
                else
                    RError "Types in branches of if do not match."
            | a, b -> RError <| sprintf "Expected both sides to be types and to be equal types.\nGot true: %A\nGot false: %A" a b
        | x -> RError <| sprintf "Expected bool in conditional.\nGot: %A" x
    | Inlineable(args,body) as orig -> 
        match d.args with
        | (cur_args,env'') :: other_args ->
            let rec loop acc = function
                | (arg :: ars, cur_arg :: crs) ->
                    let cur_arg = teval {d with env=env''; args=[]} cur_arg
                    match cur_arg with
                    | RTypedExpr _ | RExpr _ -> loop (Map.add arg cur_arg acc) (ars,crs)
                    | RDual _ -> RError "Duals cannot pass through Inlineable calls."
                    | RError _ -> cur_arg
                | [], [] -> teval {d with env=acc; args=other_args} body
                | _ -> RError "The number of arguments in Inlineable does not match the number of arguments being applied."
            loop env'' (args, cur_args)
        | [] -> RExpr orig
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
let ap x y = Apply(x,y)
let l x b i = Let(x,b,i)

let teval1 x = teval (d0()) x


