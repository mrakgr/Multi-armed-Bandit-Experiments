open System.Collections.Generic

type Ty =
    | Unit
    | Int
    | Float
    | Bool
    | TVar of string

type TyV = int64 * string * Ty
// No return type polymorphism like in Haskell for now. Local type inference only.
type TyMethodKey = int64 * int64 list * Ty list  // The key does not need to know the free variables.
// tag * higher order function tags * free variables * argument types
type TyMethod = int64 * int64 list * Ty list * TypedExpr * Set<TyV> 

and Expr = 
    | V of string
    | If of Expr * Expr * Expr
    | Inlineable of string list * Expr * Env option
    | LitUnit
    | LitInt of int
    | LitFloat of float
    | LitBool of bool
    | Apply of Expr * args: Expr list
    | Method of (int64 * Env) option * args: string list * body: Expr * return_type: Ty option

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

and ReturnCases =
    | RTypedExpr of TypedExpr
    | RExpr of Expr
    | RError of string

and Env = Map<string,ReturnCases>

let rec get_type = function
    | TyV(_,_,t) | TyIf(_,_,_,t) | TyLet(_,_,_,t) -> t
    | TyLitInt _ -> Int
    | TyLitFloat _ -> Float
    | TyLitBool _ -> Bool
    | TyUnit -> Unit
    | TyMethodCall(_,_,t) -> t

let get_tag =
    let mutable x = 0L
    fun () -> 
        let x' = x
        x <- x + 1L
        x'

type ArgCases = Expr list * Env
type Data =
    {
    // Immutable
    env : Env
    args : ArgCases list
    // Mutable
    memoized_methods : Dictionary<TyMethodKey, TypedExpr * Set<TyV>> // For hoisted out global methods.
    sequences : Stack<TyV * TypedExpr>
    used_variables : Set<TyV> ref
    }

type Result<'a,'b> = Succ of 'a | Fail of 'b

let d0() = {env=Map.empty;args=[];sequences=Stack();memoized_methods=Dictionary();used_variables=ref Set.empty}

let sequences_to_typed_expr (sequences: Stack<_>) final_expr =
    let type_fin = get_type final_expr
    Seq.fold (fun exp (v,body) -> TyLet(v,body,exp,type_fin)) final_expr sequences

let get_free_variables (env: Env) (used_variables: Set<TyV>) =
    printfn "Getting free variables."
    printfn "env=%A, used_variables=%A" env used_variables
    env
    |> Seq.choose (fun kv -> 
        match kv.Value with
        | RTypedExpr(TyV v) -> Some v
        | _ -> None)
    |> Set
    // Bound outside the method's scope that is.
    |> fun bound_variables ->
        Set.intersect bound_variables used_variables
        

let rec teval (d: Data) exp: ReturnCases =
    let add_bound_variable env arg_name ty_arg =
        Map.add arg_name (RTypedExpr(TyV ty_arg)) env

    match exp with
    | V x -> 
        match Map.tryFind x d.env with
        | Some (RTypedExpr (TyV v) as v') -> d.used_variables := (!d.used_variables).Add v; v'
        | Some (RTypedExpr _ as v) -> v
        | Some (RExpr v) -> teval d v
        | Some (RError _ as e) -> e
        | None -> RError <| sprintf "Variable %A not bound." x
    | Apply(expr,args) ->
        teval {d with args = (args,d.env) :: d.args} expr
    | If(cond,tr,fl) ->
        match teval {d with args=[]} cond with
        | RTypedExpr cond' when get_type cond' = Bool -> 
            match teval d tr, teval d fl with
            | RTypedExpr tr, RTypedExpr fl -> 
                let type_tr, type_fl = get_type tr, get_type fl
                if type_tr = type_fl then
                    RTypedExpr <| TyIf(cond',tr,fl,type_tr)
                else
                    RError <| sprintf "Types in branches of if do not match.\nGot: %A and %A" type_tr type_fl
            | a, b -> RError <| sprintf "Expected both sides to be types and to be equal types.\nGot true: %A\nGot false: %A" a b
        | x -> RError <| sprintf "Expected bool in conditional.\nGot: %A" x
    | Inlineable(args, body, None) as orig -> 
        teval d (Inlineable(args, body, Some d.env))
    | Inlineable(args, body, Some env) as orig -> 
        match d.args with
        | (cur_args,env'') :: other_args ->
            let rec loop acc = function
                | arg_name :: ars, arg_expr :: crs ->
                    match teval {d with env=env''; args=[]} arg_expr with
                    | RTypedExpr ty_exp ->
                        let b'_type = get_type ty_exp
                        let ty_arg: TyV = get_tag(),arg_name,b'_type
                        // Pushes the sequence onto the stack
                        d.sequences.Push(ty_arg,ty_exp)
                        // Binds the name to the said sequence's name and loops to the next argument
                        loop (add_bound_variable acc arg_name ty_arg) (ars,crs)
                    | RExpr _ as exp ->
                        loop (Map.add arg_name exp acc) (ars,crs)
                    | RError er -> Fail er
                | [], [] -> Succ acc
                | _ -> Fail "Incorrect number of arguments in Inlineable application."
            match loop env (args,cur_args) with
            | Succ env -> teval {d with env=env} body
            | Fail er -> RError er
        | [] -> RExpr orig
    | Method(None, args, body, return_type) ->
        teval d (Method(Some(get_tag(),d.env), args, body, return_type))
    | Method(Some(tag,initial_env), arg_names, body, return_type) as orig ->
        printfn "I am in Method(%i)." tag
        printfn "initial_env=%A" initial_env

        match d.args with
        | [] -> RExpr orig
        | (cur_args,env'') :: other_args ->
            let rec loop method_tags typed_exprs acc = function
                | arg_name :: ars, arg_expr :: crs ->
                    match teval {d with env=env''; args=[]} arg_expr with
                    | RTypedExpr ty_exp ->
                        let b'_type = get_type ty_exp
                        let ty_arg: TyV = get_tag(),arg_name,b'_type
                        loop method_tags (ty_exp :: typed_exprs) (add_bound_variable acc arg_name ty_arg) (ars,crs)
                    | RExpr(Method(Some(tag',_),_,_,_) as met) as exp ->
                        loop (tag' :: method_tags) typed_exprs (Map.add arg_name (RExpr met) acc) (ars,crs)
                    | RExpr _ -> Fail "In Method application the only Expr type allowed to be passed via an argument is another Method."
                    | RError er -> Fail er
                | [], [] -> Succ(List.rev method_tags,List.rev typed_exprs,acc)
                | _ -> Fail "Incorrect number of arguments in Method application."

            match loop [] [] initial_env (arg_names,cur_args) with
            | Succ(method_tags,typed_exprs,env) ->
                let arg_types = List.map get_type typed_exprs
                let method_key: TyMethodKey = tag, method_tags, arg_types
                let make_method_call body =
                    let x = RTypedExpr(TyMethodCall(method_key,typed_exprs,get_type body))
                    match return_type with
                    | None -> x
                    | Some return_type when return_type = get_type body -> x
                    | Some _ -> RError "The evaluated return type does not match the one given in Method evaluation."
                match d.memoized_methods.TryGetValue method_key with
                | false, _ ->
                    let sequences' = Stack()
                    printfn "Haven't evaled body. !d.used_variables=%A" !d.used_variables
                    let d = {d with env=env; sequences=sequences'; args=[]; used_variables=ref !d.used_variables}
                    match teval d body with
                    | RError _ as er -> er
                    | RExpr x -> RError "Only TypedExprs are allowed as returns from a Method's body evaluation."
                    | RTypedExpr body ->
                        // All the intermediate expressions get sequenced in the Inlineable case.
                        // The body here is just the final return hence the call to sequences_to_typed_expr.
                        let body = sequences_to_typed_expr sequences' body
                        let free_variables = get_free_variables initial_env !d.used_variables
                        d.memoized_methods.Add(method_key, (body, free_variables))
                        make_method_call body
                | true, (body, free_variables) ->
                    make_method_call body
            | Fail er -> RError er
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

let inl x y = Inlineable(x,y,None)
let ap x y = Apply(x,y)

let term0 =
    let snd = inl ["a";"b"] (V "b")
    ap (inl ["x";"y";"z";"r"] (ap (V "r") [V "y";V "z"])) ([LitUnit;LitBool true;LitInt 5;snd])

let l v b e = Apply(Inlineable(v,e,None),b)

let teval0 x = teval (d0()) x

let term1 =
    let fst = inl ["a";"b"] (V "a")
    let snd = inl ["a";"b"] (V "b")
    l ["x";"y";"z"] [LitUnit;LitBool true;LitInt 5] 
        (l ["q"] [fst] (ap (V "q") [V "y";V "z"]))

let t1 = teval0 term1
    
let term2 =
    let fst = inl ["a";"b"] (V "a")
    let snd = inl ["a";"b"] (V "b")
    l ["a";"b"] [LitInt 2;LitFloat 3.3] (ap (If(LitBool true,snd,snd)) [V "a";V "b"])

let t2 = teval0 term2

let term3 =
    l ["inlineable"]
        [inl ["a";"b"] (V "b")]
        (l ["fun"] 
            [inl ["inl";"a";"b";"c";"d"] (ap (V "inl") [V "b";V "c"])]
            (ap (V "fun") [V "inlineable"; LitBool true; LitInt 2; LitFloat 1.5; LitInt 2]))
let t3 = teval0 term3

let term4 = // If test
    l ["if"] [inl ["cond";"tr";"fl"] (If(V "cond",V "tr",V "fl"))]
        (l ["cond"] [LitBool true]
            (l ["tr"] [LitFloat 3.33]
                (l ["fl"] [LitFloat 4.44]
                    (ap (V "if") [V "cond";V "tr";V "fl"]))))
let t4 = teval0 term4

let teval1 x = 
    let d = d0() 
    teval d x, d.memoized_methods
let meth x y = Method(None,x,y,None)

let meth1 =
    l ["fun";"id"] 
        [meth ["x";"y";"z";"f"] 
            (l ["t"] [LitInt 3] 
                (l ["u"] [LitBool false] (ap (V "f") [V "z"])))
         meth ["x"] (V "x")]
        (ap (V "fun") [LitBool true; LitInt 2; LitFloat 4.4;V "id"])
let m1 = teval1 meth1

let intpow =
    l ["intpow"] 
        [meth ["a";"n"] (
            l ["loop"] [meth ["acc";"q"] (If(LitBool true,LitInt 1,LitInt 2))]
                (ap (V "loop") [LitInt 1; V "n"])
            )]
        (ap (V "intpow") [LitInt 3;LitInt 2])
let ip = teval1 intpow