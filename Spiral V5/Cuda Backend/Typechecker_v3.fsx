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
// tag * higher order function tags * argument types * method body * outside bound variables * used variables
//type TyMethod = int64 * int64 list * Ty list * TypedExpr * Set<Ty> * Set<TyV> 

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

// method key * method body * bound variables * used variables
type MethodDict = Dictionary<TyMethodKey, TypedExpr * Set<TyV> * Set<TyV>>
// method key * method body * implicit arguments
type MethodImplDict = Dictionary<TyMethodKey, TypedExpr * Set<TyV>>

type ArgCases = Expr list * Env
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
    let add_bound_variable env arg_name ty_arg =
        Map.add arg_name (RTypedExpr(TyV ty_arg)) env

    match exp with
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
        | RTypedExpr cond' when get_type cond' = Bool -> 
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
            let rec loop acc = function
                | arg_name :: ars, arg_expr :: crs ->
                    match exp_and_seq {d with env=env''; args=[]} arg_expr with
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
            | Succ env -> exp_and_seq {d with env=env} body
            | Fail er -> RError er
        | [] -> RExpr orig
    | Method(None, args, body, return_type) ->
        exp_and_seq d (Method(Some(get_tag(),d.env), args, body, return_type))
    | Method(Some(tag,initial_env), arg_names, body, return_type) as orig ->
//        printfn "I am in Method(%i)." tag
//        printfn "initial_env=%A" initial_env
        match d.args with
        | [] -> RExpr orig
        | (cur_args,env'') :: other_args ->
            let rec loop method_tags typed_exprs acc = function
                | arg_name :: ars, arg_expr :: crs ->
                    match exp_and_seq {d with env=env''; args=[]} arg_expr with
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
//                    printfn "Haven't evaled body. !d.used_variables=%A" !d.used_variables
                    let d = {d with env=env; sequences=sequences'; args=[]; used_variables=HashSet(HashIdentity.Structural)}
                    match exp_and_seq d body with
                    | RError _ as er -> er
                    | RExpr x -> RError "Only TypedExprs are allowed as returns from a Method's body evaluation."
                    | RTypedExpr body ->
                        // All the intermediate expressions get sequenced in the Inlineable case.
                        // The body here is just the final return hence the call to sequences_to_typed_expr.
                        let body = sequences_to_typed_expr sequences' body
                        let bound_variables = get_bound_variables initial_env
                        d.memoized_methods.Add(method_key, (body, bound_variables, Set d.used_variables))
                        make_method_call body
                | true, (body, bound_variables, used_variables) ->
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

// Unions the free variables from top to bottom of the call chain.
let rec closure_conv (imemo: MethodImplDict) (memo: MethodDict) (exp: TypedExpr) =
    let c x = closure_conv imemo memo x
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
    | TyMethodCall(m,ar,t) ->
        let method_implicit_args =
            match imemo.TryGetValue m with
            | true, (_,impl_args) -> impl_args
            | false, _ ->
                let m', bound_variables, used_variables = memo.[m]
                let impl_args = Set.union (c m') used_variables |> Set.intersect bound_variables // union the free vars from top to bottom
                imemo.Add(m,(m',impl_args))
                impl_args
        Set.union method_implicit_args (Set.unionMany <| List.map c ar)

let inl x y = Inlineable(x,y,None)
let ap x y = Apply(x,y)

let term0 =
    let snd = inl ["a";"b"] (V "b")
    ap (inl ["x";"y";"z";"r"] (ap (V "r") [V "y";V "z"])) ([LitUnit;LitBool true;LitInt 5;snd])

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

let term1 =
    let fst = inl ["a";"b"] (V "a")
    let snd = inl ["a";"b"] (V "b")
    l ["x";"y";"z"] [LitUnit;LitBool true;LitInt 5] 
        (l ["q"] [fst] (ap (V "q") [V "y";V "z"]))

let t1 = exp_and_seq0 term1
    
let term2 =
    let fst = inl ["a";"b"] (V "a")
    let snd = inl ["a";"b"] (V "b")
    l ["a";"b"] [LitInt 2;LitFloat 3.3] (ap (If(LitBool true,snd,snd)) [V "a";V "b"])

let t2 = exp_and_seq0 term2

let term3 =
    l ["inlineable"]
        [inl ["a";"b"] (V "b")]
        (l ["fun"] 
            [inl ["inl";"a";"b";"c";"d"] (ap (V "inl") [V "b";V "c"])]
            (ap (V "fun") [V "inlineable"; LitBool true; LitInt 2; LitFloat 1.5; LitInt 2]))
let t3 = exp_and_seq0 term3

let term4 = // If test
    l ["if"] [inl ["cond";"tr";"fl"] (If(V "cond",V "tr",V "fl"))]
        (l ["cond"] [LitBool true]
            (l ["tr"] [LitFloat 3.33]
                (l ["fl"] [LitFloat 4.44]
                    (ap (V "if") [V "cond";V "tr";V "fl"]))))
let t4 = exp_and_seq0 term4

let meth x y = Method(None,x,y,None)

let meth1 =
    l ["fun";"id"] 
        [meth ["x";"y";"z";"f"] 
            (l ["t"] [LitInt 3] 
                (l ["u"] [LitBool false] (ap (V "f") [V "z"])))
         meth ["x"] (V "x")]
        (ap (V "fun") [LitBool true; LitInt 2; LitFloat 4.4;V "id"])
let m1 = eval meth1

let meth2 =
    l ["m"] 
        [meth ["a";"n";"qwe"] (
            l ["loop"] [meth ["acc";"q"] 
                (l ["loop_method"] [meth ["a";"n"] (If(LitBool true,V "a",V "n"))]
                    (ap (V "loop_method") [V "a"; V "n"]))]
                (ap (V "loop") [LitInt 1; V "n"]))]
        (ap (V "m") [LitInt 3;LitInt 2;LitUnit])

let ip = eval meth2
