open System.Collections.Generic

type Ty =
    | UnitT
    | IntT
    | FloatT
    | BoolT
    | VarsT of Ty list

type TyV = int64 * string * Ty

type TyOrTag =
    | Ty of Ty
    | Tag of int64

// No return type polymorphism like in Haskell for now. Local type inference only.
type TyMethodKey = int64 * TyOrTag list // The key does not need to know the free variables.
// tag * higher order function tags * argument types * method body * outside bound variables * used variables
//type TyMethod = int64 * int64 list * Ty list * TypedExpr * Set<Ty> * Set<TyV> 

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

// Does macros expansion, sequtialization and takes note of the bound and 
// used variables in the method dictionary for the following passes.
let rec exp_and_seq (d: Data) exp: ReturnCases =
    let tev d exp = exp_and_seq d exp
    
    let add_bound_variable env arg_name ty_arg =
        let v = TyV ty_arg
        v, Map.add arg_name (RTypedExpr v) env

    let bind_er er = Fail er
    let bind_expr_fail acc arg_name exp =
        Fail "Cannot bind untyped expressions in value structures like Vars."
    let bind_expr acc arg_name exp =
//        let rec loop acc = function
//            | x :: xs ->
//                match tev {d with args=[]} x with
//                | RExpr expr -> loop (expr :: acc) xs
//                | RTypedExpr ty_expr -> Fail "Typed Expressions not allowed in Expression Tuples."
//                | RError er -> Fail er
//            | [] -> List.rev acc |> Succ
//        match loop [] exprs with
//        | Succ args -> RExpr <| ET args
//        | Fail er -> RError er
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
        Succ (Map.add arg_name exp acc)
    let bind_typedexpr_fail acc arg_name ty_exp =
        Fail "Cannot bind typed expressions in expression tuples."
    let bind_typedexpr' acc arg_name ty_exp =
        let b'_type = get_type ty_exp
        let ty_arg: TyV = get_tag(),arg_name,b'_type
        // Pushes the sequence onto the stack
        d.sequences.Push(ty_arg,ty_exp)
        // Binds the name to the said sequence's name and loops to the next argument
        add_bound_variable acc arg_name ty_arg
    let bind_typedexpr acc arg_name ty_exp =
        bind_typedexpr' acc arg_name ty_exp |> snd |> Succ

    let rec bind_typed_vars acc = function
        | V x :: left_rest, exp :: right_rest ->
            let x,acc = bind_typedexpr' acc x exp
            match bind_typed_vars acc (left_rest, right_rest) with
            | Succ(exp,acc) -> Succ(x :: exp,acc)
            | Fail _ as er -> er
        | VV x :: left_rest, TyVars(exp,t) :: right_rest ->
            match bind_typed_vars acc (x,exp) with
            | Succ(x,acc) ->
                match bind_typed_vars acc (left_rest, right_rest) with
                | Succ(exp,acc) -> Succ(TyVars(x,t) :: exp,acc)
                | Fail _ as er -> er
            | Fail _ as er -> er
        | [],[] -> Succ([],acc)
        | x -> Fail <| sprintf "Unexpected expression in bind_typed_vars: %A" x

    let rec vars_to_tyvars acc = function
        | x :: xs ->
            match tev {d with args=[]} x with
            | RExpr _ -> Fail "Expressions not allowed in Vars. Vars is only used to represented typed variable sized arguments."
            | RTypedExpr ty_expr -> 
                vars_to_tyvars (ty_expr :: acc) xs
            | RError er -> Fail er
        | [] -> List.rev acc |> Succ

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
            match exp_and_seq d tr, exp_and_seq d fl with
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
            let bind_template bind_er bind_expr bind_typedexpr acc (arg_name, right_arg) =
                match tev {d with env=env''; args=[]} right_arg with
                | RError er -> bind_er er
                | RExpr _ as exp -> bind_expr acc arg_name exp
                | RTypedExpr ty_exp -> bind_typedexpr acc arg_name ty_exp

            let rec parse bind acc = function
                | V arg_name, right_arg -> bind acc (arg_name, right_arg)
                | left_arg, right_arg -> Fail <| sprintf "Something is wrong. Got: %A and %A" left_arg right_arg

            match parse bind_any env (args,cur_args) with
            | Succ env -> exp_and_seq {d with env=env} body
            | Fail er -> RError er
        | [] -> RExpr orig
    // Note: All the cases here can only be accessed through a V. 
    // The actual evaluation of them should be happening Inlineable and Method cases.
    | VV _ -> RError "Typechecking should never be called on VV. VV is only for immediate destructuring."
    | Vars vars -> RError "Vars should have been evaluated already."
    | ET exprs as orig -> RExpr orig
