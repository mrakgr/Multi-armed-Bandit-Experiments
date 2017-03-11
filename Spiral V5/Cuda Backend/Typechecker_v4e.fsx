open System.Collections.Generic

type Ty =
    | UnitT
    | IntT
    | FloatT
    | BoolT
    | VarsT of Ty list

type TyV = int64 * string * Ty

type MethodArgs =
    | MATy of Ty
    | MATag of int64
    | MAVV of MethodArgs list
    | MAET of MethodArgs list
    | MAVars of MethodArgs list

type MethodCall =
    | MCTypedExpr of TypedExpr
    | MCTag of int64
    | MCVV of MethodCall list
    | MCET of MethodCall list
    | MCVars of MethodCall list

// No return type polymorphism like in Haskell for now. Local type inference only.
and TyMethodKey = int64 * MethodArgs // The key does not need to know the free variables.

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
    | TyMethodCall of TyMethodKey * MethodCall * Ty
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

let rec call_to_args = function
    | MCVV x -> MAVV (List.map call_to_args x)
    | MCTag x -> MATag x
    | MCTypedExpr x -> MATy (get_type x)
    | MCVars x -> MAVars (List.map call_to_args x)
    | MCET x -> MAET (List.map call_to_args x)

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

let rec with_empty_seq (d: Data) expr =
    let d' = {d with sequences = Stack()}
    match exp_and_seq d' expr with
    | RExpr _ as x -> x
    | RTypedExpr x -> RTypedExpr <| sequences_to_typed_expr d'.sequences x
    | RError _ as er -> er

// Does macros expansion, sequtialization and takes note of the bound and 
// used variables in the method dictionary for the following passes.
and exp_and_seq (d: Data) exp: ReturnCases =
    let tev d exp = exp_and_seq d exp
    
    let add_bound_variable env arg_name ty_arg =
        let v = TyV ty_arg |> RTypedExpr
        Map.add arg_name v env

    let bind_expr_fail acc (arg_name, exp) =
        Fail "Cannot bind untyped expressions in value structures like Vars."
    let bind_expr acc (arg_name, exp) =
        let exp = RExpr exp
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
        bind_typedexpr' acc (arg_name, ty_exp) |> Succ
    let bind_template bind_expr bind_typedexpr eval_env acc (arg_name, right_arg) =
        match tev {d with env=eval_env; args=[]} right_arg with
        | RError er -> Fail er
        | RExpr exp -> bind_expr acc (arg_name, exp)
        | RTypedExpr ty_exp -> bind_typedexpr acc (arg_name, ty_exp)

    let bind_any = bind_template bind_expr bind_typedexpr
    let bind_expr_only = bind_template bind_expr bind_typedexpr_fail
    let bind_typedexpr_only = bind_template bind_expr_fail bind_typedexpr

    let match_v_template match_vv bind (eval_env: Env) (acc: Env) = function
        | V arg_name, right_arg ->
            bind acc (arg_name, right_arg)
        | VV args, right_arg ->
            match_vv eval_env acc (args, right_arg)
        | x -> Fail <| sprintf "Unexpected arguments in match_v.\n%A" x

    let bind_method acc (arg_name, exp) =
        match exp with
        | Method(Some(tag,_),_,_,_) -> 
            let exp' = MCTag tag
            Succ (exp', Map.add arg_name (RExpr exp) acc)
        | x -> Fail <| sprintf "Expected: method.\nGot: %A" x
    
    let bind_ty acc (arg_name, ty_exp) =
        let acc = bind_typedexpr' acc (arg_name, ty_exp)
        Succ (MCTypedExpr ty_exp, acc)

    let bind_method_only = bind_template bind_method bind_typedexpr_fail
    let bind_ty_only = bind_template bind_expr_fail bind_ty
    let bind_any_method = bind_template bind_method bind_ty

    let traverse_generic tuple_constructor f s l = mapFold2Er f s l |> mapResultFst tuple_constructor

    let rec match_vars_template traverse bind (eval_env: Env) (acc: Env) (l,r) = 
        let match_vv = match_vars_template traverse bind
        match r with
        | Vars r -> traverse (match_v_template match_vv (bind eval_env) eval_env) acc (l,r)
        | x -> Fail <| sprintf "Unexpected arguments in match_vars.\n%A" x

    let match_vars = match_vars_template fold2Er bind_typedexpr_only
    let match_vars_method = match_vars_template (traverse_generic MCVars) bind_ty_only

    let rec match_et_template traverse bind (eval_env: Env) (acc: Env) (l,r) = 
        let match_vv = match_et_template traverse bind
        match r with
        | ET r -> traverse (match_v_template match_vv (bind eval_env) eval_env) acc (l,r)
        | x -> Fail <| sprintf "Unexpected arguments in match_et.\n%A" x

    let match_et = match_et_template fold2Er bind_expr_only
    let match_et_method = match_et_template (traverse_generic MCET) bind_method_only

    let rec match_tyvars_template traverse bind (eval_env: Env) (acc: Env) (l,r) = 
        let match_vv = match_tyvars_template traverse bind
        match r with
        | TyVars(r,t) -> traverse (match_v_template match_vv bind eval_env) acc (l,r)
        | x -> Fail <| sprintf "Unexpected arguments in match_tyvars.\n%A" x

    let match_tyvars = match_tyvars_template fold2Er bind_typedexpr
    let match_tyvars_method = match_tyvars_template (traverse_generic MCVars) bind_ty

    let bind_tyvars eval_env = bind_template bind_expr_fail (match_tyvars eval_env) eval_env
    let bind_tyvars_method eval_env = bind_template bind_expr_fail (match_tyvars_method eval_env) eval_env

    let rec match_vv_template traverse bind_vv match_vars match_et bind_tyvars (eval_env: Env) (acc: Env) (l,r) = 
        let match_vv = match_vv_template traverse bind_vv match_vars match_et bind_tyvars
        match r with
        | VV r -> traverse (match_v_template match_vv (bind_vv eval_env) eval_env) acc (l,r)
        | Vars _ as r -> match_vars eval_env acc (l,r)
        | ET _ as r -> match_et eval_env acc (l,r)
        | r -> bind_tyvars eval_env acc (l,r)

    let match_vv = match_vv_template fold2Er bind_any match_vars match_et bind_tyvars
    let match_vv_method = match_vv_template (traverse_generic MCVV) bind_any_method match_vars_method match_et_method bind_tyvars_method

    let match_v eval_env = match_v_template match_vv (bind_any eval_env) eval_env
    let match_v_method eval_env = match_v_template match_vv_method (bind_any_method eval_env) eval_env

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
            match with_empty_seq d tr, with_empty_seq d fl with
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
        | [] -> RExpr orig
        | (cur_args,env'') :: other_args ->
            match match_v env'' env (args, cur_args) with
            | Fail er -> RError er
            | Succ env -> exp_and_seq {d with env=env;args=other_args} body
    | Method(None,args,body,return_type) ->
        exp_and_seq d (Method(Some(get_tag(),d.env),args,body,return_type))
    | Method(Some(tag,initial_env),args,body,return_type) as orig -> 
        match d.args with
        | [] -> RExpr orig
        | (cur_args,env'') :: other_args ->
            match match_v_method env'' initial_env (args, cur_args) with
            | Fail er -> RError er
            | Succ(evaled_cur_args,env) -> 
                let method_key: TyMethodKey = tag, call_to_args evaled_cur_args

                let make_method_call body =
                    let x = RTypedExpr(TyMethodCall(method_key,evaled_cur_args,get_type body))
                    match return_type with
                    | None -> x
                    | Some return_type when return_type = get_type body -> x
                    | Some _ -> RError "The evaluated return type does not match the one given in Method evaluation."

                match d.memoized_methods.TryGetValue method_key with
                | false, _ ->
                    let d = {d with env=env; args=[]; used_variables=HashSet(HashIdentity.Structural)}
                    match with_empty_seq d body with
                    | RError _ as er -> er
                    | RExpr x -> RError "Only TypedExprs are allowed as returns from a Method's body evaluation."
                    | RTypedExpr body ->
                        let bound_variables = get_bound_variables initial_env
                        d.memoized_methods.Add(method_key, (body, bound_variables, Set d.used_variables))
                        make_method_call body
                | true, (body, bound_variables, used_variables) ->
                    make_method_call body
    | VV _ -> RError "Typechecking should never be called on VV. VV is only for immediate destructuring."
    | ET exprs ->
        let rec loop acc = function
            | x :: xs ->
                match tev {d with args=[]} x with
                | RExpr expr -> loop (expr :: acc) xs
                | RTypedExpr ty_expr -> Fail "Typed Expressions not allowed in Expression Tuples."
                | RError er -> Fail er
            | [] -> List.rev acc |> Succ
        match loop [] exprs with
        | Succ args -> RExpr <| ET args
        | Fail er -> RError er
    | Vars vars as orig ->
        let empty_names = List.map (fun _ -> V "") vars
        match match_vars_method d.env d.env (empty_names, orig) with
        | Succ(MCVars(evaled_vars),env) ->
            let fields = List.map (function
                | (MCTypedExpr x) -> x
                | _ -> failwith "Impossible") evaled_vars
            let ty = List.map get_type fields |> VarsT
            RTypedExpr <| TyVars(fields,ty)
        | Succ _ -> failwith "Impossible"
        | Fail er -> RError er
            
// Unions the free variables from top to bottom of the call chain.
let rec closure_conv (imemo: MethodImplDict) (memo: MethodDict) (exp: TypedExpr) =
    let c x = closure_conv imemo memo x
    let rec grab_implicit_args = function
        | MCVars x | MCVV x -> Set.unionMany (List.map grab_implicit_args x)
        | MCTag _ | MCET _ -> Set.empty
        | MCTypedExpr x -> c x
    match exp with
    | TyV(_,_,t) -> Set.empty
    | TyIf(cond,tr,fl,t) ->
        let cond, tr, fl = c cond, c tr, c fl
        Set.unionMany [|cond; tr; fl|]
    | TyLet(_,body,e,t) ->
        let body = c body
        let e = c e
        Set.union body e
    | TyLitInt _ -> Set.empty
    | TyLitFloat _ -> Set.empty
    | TyLitBool _ -> Set.empty
    | TyUnit -> Set.empty
    | TyVars(vars,_) -> Set.unionMany (List.map c vars)
    | TyMethodCall(m,ar,t) ->
        let method_implicit_args =
            match imemo.TryGetValue m with
            | true, (_,impl_args) -> impl_args
            | false, _ ->
                let m', bound_variables, used_variables = memo.[m]
                let impl_args = Set.union (c m') used_variables |> Set.intersect bound_variables // union the free vars from top to bottom
                imemo.Add(m,(m',impl_args))
                impl_args
        Set.union method_implicit_args (grab_implicit_args ar)

let inl x y = Inlineable(x,y,None)
let ap x y = Apply(x,y)

let l v b e = Apply(Inlineable(v,e,None),b)

let exp_and_seq0 x = exp_and_seq (d0()) x
let exp_and_seq1 x = 
    let d = d0() 
    exp_and_seq d x, d.memoized_methods
let eval x = 
    let d = d0()
    match exp_and_seq d x with
    | RTypedExpr exp ->
        let imemo = Dictionary(HashIdentity.Structural)
        let s = closure_conv imemo d.memoized_methods exp
        printfn "set=%A" s
        //if closure_conv imemo d.memoized_methods exp <> Set.empty then failwith "Set should be empty at the end of this call"
        Succ (exp, imemo)
    | RExpr exp ->
        Fail <| sprintf "Expected: typed expression.\nGot: expression %A" exp
    | RError er ->
        Fail er

let term0 =
    let snd = inl (VV [V "a";V "b"]) (V "b")
    ap (inl (VV [V "x";V "y";V "z";V "r"]) (ap (V "r") (VV [V "y";V "z"]))) (VV [LitUnit;LitBool true;LitInt 5;snd])

let t0 = exp_and_seq0 term0

let term1 =
    let fst = inl (VV [V "a";V "b"]) (V "a")
    let snd = inl (VV [V "a";V "b"]) (V "b")
    l (VV [V "x";V "y";V "z"]) (VV [LitUnit;LitBool true;LitInt 5])
        (l (V "q") (fst) (ap (V "q") (VV [V "y";V "z"])))

let t1 = exp_and_seq0 term1
    
let term2 =
    let fst = inl (VV [V "a";V "b"]) (V "a")
    let snd = inl (VV [V "a";V "b"]) (V "b")
    l (VV [V "a";V "b"]) (VV [LitInt 2;LitFloat 3.3]) (ap (If(LitBool true,snd,snd)) (VV [V "a";V "b"]))

let t2 = exp_and_seq0 term2

let term3 =
    l (V "inlineable") (VV [inl (VV [V "a";V "b"]) (V "b")])
        (l (V "fun")
            (inl (VV [V "inl";V "a";V "b";V "c";V "d"]) (ap (V "inl") (VV [V "b";V "c"])))
            (ap (V "fun") (VV [V "inlineable"; LitBool true; LitInt 2; LitFloat 1.5; LitInt 2])))
let t3 = exp_and_seq0 term3

let term3' =
    l (V "inlineable") (inl (VV [V "a";V "b"]) (V "b"))
        (l (V "fun")
            (inl (VV [V "inl";V "a";V "b";V "c";V "d"]) (ap (V "inl") (VV [V "b";V "c"])))
            (ap (V "fun") (VV [V "inlineable"; LitBool true; LitInt 2; LitFloat 1.5; LitInt 2])))
let t3' = exp_and_seq0 term3'

let term4 = // If test
    l (V "if") (inl (VV [V "cond";V "tr";V "fl"]) (If(V "cond",V "tr",V "fl")))
        (l (V "cond") (LitBool true)
            (l (V "tr") (LitFloat 3.33)
                (l (V "fl") (LitFloat 4.44)
                    (ap (V "if") (VV [V "cond";V "tr";V "fl"])))))
let t4 = exp_and_seq0 term4

let meth x y = Method(None,x,y,None)

let meth1 =
    l (VV [V "fun";V "id"])
        (VV [meth (VV [V "x";V "y";V "z";V "f"])
                (l (V "t") (LitInt 3)
                    (l (V "u") (LitBool false) (ap (V "f") (V "z"))))
             meth (V "x") (V "x")])
        (ap (V "fun") (VV [LitBool true; LitInt 2; LitFloat 4.4;V "id"]))
let m1 = eval meth1

let meth2 = // closure conversion test
    l (V "m") 
        (meth (VV [V "a";V "n";V "qwe"]) 
            (l (V "loop") 
                (meth (VV [V "acc";V "q"]) 
                    (l (V "loop_method") (meth (VV [V "a";V "n"]) (If(LitBool true,V "a",V "n")))
                        (ap (V "loop_method") (VV [V "a"; V "n"]))))
                (ap (V "loop") (VV [LitInt 1; V "n"]))))
        (ap (V "m") (VV [LitInt 3;LitInt 2;LitUnit]))
let ip = eval meth2
