open System.Collections.Generic

type Ty =
    | UnitT
    | IntT
    | FloatT
    | BoolT
    | VarsT of Ty list

type TyV = int64 * string * Ty

type MethodArgs =
    | MTy of Ty
    | MTag of int64
    | MVV of MethodArgs list

// No return type polymorphism like in Haskell for now. Local type inference only.
type TyMethodKey = int64 * MethodArgs // The key does not need to know the free variables.

and Expr = 
    | V of string // standard variable
    | If of Expr * Expr * Expr
    | Inlineable of Expr * Expr * Env option
    | LitUnit
    | LitInt of int
    | LitFloat of float
    | LitBool of bool
    | Apply of Expr * args: Expr
    | Method of (int64 * Env) option * args: Expr * body: Expr * return_type: Ty option
    | VV of Expr list // immediately destructure
    | Vars of Expr list // variable arguments
    | ET of Expr list // expression tuple

// This is being compiled to STLC, not System F, so no type variables are allowed in the processed AST.
and TypedExpr =
    | TyV of TyV
    | TyIf of TypedExpr * TypedExpr * TypedExpr * Ty
    | TyLet of TyV * TypedExpr * TypedExpr * Ty
    | TyUnit
    | TyLitInt of int
    | TyLitFloat of float
    | TyLitBool of bool
    | TyMethodCall of TyMethodKey * TypedExpr list * Ty
    | TyVars of TypedExpr list * Ty

and ReturnCases =
    | RTypedExpr of TypedExpr
    | RExpr of Expr
    | RError of string

and Env = Map<string,ReturnCases>

let rec get_type = function
    | TyV(_,_,t) | TyIf(_,_,_,t) | TyLet(_,_,_,t) -> t
    | TyLitInt _ -> IntT
    | TyLitFloat _ -> FloatT
    | TyLitBool _ -> BoolT
    | TyUnit -> UnitT
    | TyMethodCall(_,_,t) -> t
    | TyVars(_,t) -> t

let get_tag =
    let mutable x = 0L
    fun () -> 
        let x' = x
        x <- x + 1L
        x'

// method key * method body * bound variables * used variables
type MethodDict = Dictionary<TyMethodKey, TypedExpr * Set<TyV> * Set<TyV>>
// method key * method body * implicit arguments
type MethodImplDict = Dictionary<TyMethodKey, TypedExpr * Set<TyV>>

type ArgCases = Expr * Env
type Data =
    {
    // Immutable
    env : Env
    args : ArgCases list
    // Mutable
    memoized_methods : MethodDict // For hoisted out global methods.
    sequences : Stack<TyV * TypedExpr>
    used_variables : HashSet<TyV>
    }

type Result<'a,'b> = Succ of 'a | Fail of 'b

let d0() = {env=Map.empty;args=[];sequences=Stack();memoized_methods=Dictionary(HashIdentity.Structural);used_variables=HashSet(HashIdentity.Structural)}

let sequences_to_typed_expr (sequences: Stack<_>) final_expr =
    let type_fin = get_type final_expr
    Seq.fold (fun exp (v,body) -> TyLet(v,body,exp,type_fin)) final_expr sequences

let get_bound_variables (env: Env) =
    env
    |> Seq.choose (fun kv -> 
        match kv.Value with
        | RTypedExpr(TyV v) -> Some v
        | _ -> None)
    |> Set

let mapFold2Er f state (x,y) =
    if List.length x = List.length y then
        let rec loop f state = function
            | x :: xs, y :: ys ->
                match f state (x,y) with
                | Succ(r,state) ->
                    match loop f state (xs,ys) with
                    | Succ(rs,state) -> Succ (r::rs,state)
                    | Fail _ as er -> er
                | Fail er -> Fail er
            | [], [] -> Succ ([], state)
            | x -> failwith "Argument size mismatch in mapFoldEr2."
        loop f state (x,y)
    else
        Fail <| sprintf "Argument size mismatch in mapFoldEr2. Args: %A" (x,y)

let mapResult f = function
    | Succ x -> Succ <| f x
    | Fail er -> Fail er

let mapFst f (a,b) = (f a, b)
let mapResultFst f = mapResult (mapFst f)

let rec fold2Er f state (x,y) =
    if List.length x = List.length y then
        let rec loop f state = function
            | x :: xs, y :: ys ->
                match f state (x,y) with
                | Succ state ->
                    match loop f state (xs,ys) with
                    | Succ state -> Succ state
                    | Fail _ as er -> er
                | Fail er -> Fail er
            | [], [] -> Succ state
            | x -> failwith "Argument size mismatch in fold2Er."
        loop f state (x,y)
    else
        Fail <| sprintf "Argument size mismatch in foldEr2. Args: %A" (x,y)

// Does macros expansion, sequtialization and takes note of the bound and 
// used variables in the method dictionary for the following passes.
let rec exp_and_seq (d: Data) exp: ReturnCases =
    let tev d exp = exp_and_seq d exp
    
    let add_bound_variable env arg_name ty_arg =
        let v = TyV ty_arg
        v, Map.add arg_name (RTypedExpr v) env

    let bind_expr_fail acc (arg_name, exp) =
        Fail "Cannot bind untyped expressions in value structures like Vars."
    let bind_expr acc (arg_name, exp) =
        Succ (Map.add arg_name exp acc)
    let bind_typedexpr_fail acc (arg_name, ty_exp) =
        Fail "Cannot bind typed expressions in expression tuples."
    let bind_typedexpr' acc (arg_name, ty_exp) =
        let b'_type = get_type ty_exp
        let ty_arg: TyV = get_tag(),arg_name,b'_type
        // Pushes the sequence onto the stack
        d.sequences.Push(ty_arg,ty_exp)
        // Binds the name to the said sequence's name and loops to the next argument
        add_bound_variable acc arg_name ty_arg
    let bind_typedexpr acc (arg_name, ty_exp) =
        bind_typedexpr' acc (arg_name, ty_exp) |> snd |> Succ
    let bind_template bind_expr bind_typedexpr eval_env acc (arg_name, right_arg) =
        match tev {d with env=eval_env; args=[]} right_arg with
        | RError er -> Fail er
        | RExpr _ as exp -> bind_expr acc (arg_name, exp)
        | RTypedExpr ty_exp -> bind_typedexpr acc (arg_name, ty_exp)

    let bind_any = bind_template bind_expr bind_typedexpr
    let bind_expr_only = bind_template bind_expr bind_typedexpr_fail
    let bind_typedexpr_only = bind_template bind_expr_fail bind_typedexpr

    let match_v match_vv bind (eval_env: Env) (acc: Env) = function
        | V arg_name, right_arg ->
            bind acc (arg_name, right_arg)
        | VV args, right_arg ->
            match_vv eval_env acc (args, right_arg)
        | x -> Fail <| sprintf "Unexpected arguments in match_v.\n%A" x

    let rec match_vars (eval_env: Env) (acc: Env) (l,r) = 
        match r with
        | Vars r -> fold2Er (match_v match_vars (bind_typedexpr_only eval_env) eval_env) acc (l,r)
        | x -> Fail <| sprintf "Unexpected arguments in match_vars.\n%A" x

    let rec match_et (eval_env: Env) (acc: Env) (l,r) = 
        match r with
        | ET r -> fold2Er (match_v match_et (bind_expr_only eval_env) eval_env) acc (l,r)
        | x -> Fail <| sprintf "Unexpected arguments in match_et.\n%A" x

    let rec match_tyvars eval_env (acc: Env) (l,r) = 
        match r with
        | TyVars(r,t) -> fold2Er (match_v match_tyvars bind_typedexpr eval_env) acc (l,r)
        | x -> Fail <| sprintf "Unexpected arguments in match_tyvars.\n%A" x

    let bind_tyvars eval_env = bind_template bind_expr_fail (match_tyvars eval_env) eval_env

    let rec match_vv (eval_env: Env) (acc: Env) (l,r) = 
        match r with
        | VV r -> fold2Er (match_v match_vv (bind_any eval_env) eval_env) acc (l,r)
        | Vars _ as r -> match_vars eval_env acc (l,r)
        | ET _ as r -> match_et eval_env acc (l,r)
        | r -> bind_tyvars eval_env acc (l,r)

// -----

    let bind_expr_any_method acc (arg_name, exp) =
        let x = 
            match exp with
            | Method(Some(tag,_), _, _, _) -> MTag tag
            | 

        //Succ (Map.add arg_name exp acc)
    let bind_typedexpr' acc (arg_name, ty_exp) =
        let b'_type = get_type ty_exp
        let ty_arg: TyV = get_tag(),arg_name,b'_type
        // Pushes the sequence onto the stack
        d.sequences.Push(ty_arg,ty_exp)
        // Binds the name to the said sequence's name and loops to the next argument
        add_bound_variable acc arg_name ty_arg
    let bind_typedexpr acc (arg_name, ty_exp) =
        bind_typedexpr' acc (arg_name, ty_exp) |> snd |> Succ


    let rec match_vv_method (eval_env: Env) (acc: Env) (l,r) = 
        match r with
        | VV r -> mapFold2Er (match_v match_vv_method (bind_any eval_env) eval_env) acc (l,r)
//        | Vars _ as r -> match_vars eval_env acc (l,r)
//        | ET _ as r -> match_et eval_env acc (l,r)
//        | r -> bind_tyvars eval_env acc (l,r)

    match exp with
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
    | LitUnit -> 
        match d.args with
        | [] -> RTypedExpr TyUnit
        | _ -> RError "Cannot apply a bool literal."
    | V x -> 
        match Map.tryFind x d.env with
        | Some (RTypedExpr (TyV v) as v') -> d.used_variables.Add v |> ignore; v'
        | Some (RTypedExpr _ as v) -> v
        | Some (RExpr v) -> exp_and_seq d v
        | Some (RError _ as e) -> e
        | None -> RError <| sprintf "Variable %A not bound." x
    | Apply(expr,args) ->
        exp_and_seq {d with args = (args,d.env) :: d.args} expr
    | If(cond,tr,fl) ->
        match exp_and_seq {d with args=[]} cond with
        | RTypedExpr cond' when get_type cond' = BoolT -> 
            match exp_and_seq d tr, exp_and_seq d fl with // TODO: Fix this. tr and fl need their own sequentialization.
            | RTypedExpr tr, RTypedExpr fl -> 
                let type_tr, type_fl = get_type tr, get_type fl
                if type_tr = type_fl then
                    RTypedExpr <| TyIf(cond',tr,fl,type_tr)
                else
                    RError <| sprintf "Types in branches of if do not match.\nGot: %A and %A" type_tr type_fl
            | a, b -> RError <| sprintf "Expected both sides to be types and to be equal types.\nGot true: %A\nGot false: %A" a b
        | x -> RError <| sprintf "Expected bool in conditional.\nGot: %A" x
    | Inlineable(args, body, None) as orig -> 
        exp_and_seq d (Inlineable(args, body, Some d.env))
    | Inlineable(args, body, Some env) as orig -> 
        match d.args with
        | (cur_args,env'') :: other_args ->
            
        | [] -> RExpr orig

