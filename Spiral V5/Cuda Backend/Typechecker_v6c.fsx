open System.Collections.Generic

type Ty =
    | UnitT
    | UInt32T
    | UInt64T
    | Int32T
    | Int64T
    | Float32T
    | Float64T
    | BoolT
    | VVT of Ty list
    | NominalT of string // for classes and such
    | GlobalArrayT of TyV list * Ty
    | SharedArrayT of TyV list * Ty
    | LocalArrayT of TyV list * Ty
    | ArrT of Ty list * Ty
    | TagT of int64
and TyV = int64 * string * Ty

// No return type polymorphism like in Haskell for now. Local type inference only.
and TyMethodKey = int64 * Ty // The key does not need to know the free variables.

and Expr = 
    | V of string // standard variable
    | If of Expr * Expr * Expr
    | HoistedIf of Expr * Expr * Expr
    | Inlineable of Expr * Expr
    | LitUnit
    | LitInt of int
    | LitFloat of float
    | LitBool of bool
    | Apply of Expr * args: Expr
    | Method of name: string * args: Expr * body: Expr

    // Tuple cases
    | IndexVV of Expr * Expr
    | VV of Expr list // tuple

    // Array cases
    | IndexArray of Expr * Expr list
    | CreateSharedArray of Expr list * Ty
    | CreateLocalArray of Expr list * Ty

    // Primitive operations on expressions.
    | Add of Expr * Expr
    | Sub of Expr * Expr
    | Mult of Expr * Expr
    | Div of Expr * Expr
    | Mod of Expr * Expr
    | LT of Expr * Expr
    | LTE of Expr * Expr
    | EQ of Expr * Expr
    | GT of Expr * Expr
    | GTE of Expr * Expr
    | LeftShift of Expr * Expr
    | RightShift of Expr * Expr
    | Syncthreads
    | ShuffleXor of Expr * Expr
    | ShuffleUp of Expr * Expr
    | ShuffleDown of Expr * Expr
    | ShuffleIndex of Expr * Expr
    | Log of Expr
    | Exp of Expr
    | Tanh of Expr
    | Neg of Expr
    // Cuda kernel constants
    | ThreadIdxX | ThreadIdxY | ThreadIdxZ
    | BlockIdxX | BlockIdxY | BlockIdxZ
    | BlockDimX | BlockDimY | BlockDimZ
    | GridDimX | GridDimY | GridDimZ
    // Mutable operations.
    | MSet of Expr * Expr * Expr
    | AtomicAdd of out: Expr * in_: Expr
    // Loops
    | While of Expr * Expr * Expr
    // Cub operations
    | CubBlockReduce of Expr * Expr option

    static member (+)(a,b) = Add(a,b)
    static member (-)(a,b) = Sub(a,b)
    static member (*)(a,b) = Mult(a,b)
    static member (/)(a,b) = Div(a,b)
    static member (%)(a,b) = Mod(a,b)

    static member (.=)(a,b) = EQ(a,b)
    static member (.<)(a,b) = LT(a,b)
    static member (.<=)(a,b) = LTE(a,b)
    static member (.>)(a,b) = GT(a,b)
    static member (.>=)(a,b) = GTE(a,b)

// This is being compiled to STLC, not System F, so no type variables are allowed in the processed AST.
and TypedExpr =
    // These two will not get code gen'd.
    // The difference from the past version of the typechecker is that now the TagT type exists.
    | Inlineable' of Expr * Expr * Env * Ty
    | Method' of name: string * args: Expr * body: Expr * Env * Ty
    
    | TyV of TyV
    | TyIf of TypedExpr * TypedExpr * TypedExpr * Ty
    | TyLet of TyV * TypedExpr // expression (in tuples) / statement (in seqs)
    | TyUnit
    | TyLitInt of int
    | TyLitFloat of float
    | TyLitBool of bool
    | TyMethodCall of TyMethodKey * TypedExpr * Ty
    
    // Tuple cases
    | TyIndexVV of TypedExpr * TypedExpr * Ty
    | TyVV of TypedExpr list * Ty

    // Seq
    | TySeq of TypedExpr * TypedExpr * Ty
        
    // Array cases
    | TyIndexArray of TypedExpr * TypedExpr list * Ty
    | TyCreateSharedArray of TypedExpr list * Ty
    | TyCreateLocalArray of TypedExpr list * Ty

    // Cuda kernel constants
    | TyThreadIdxX | TyThreadIdxY | TyThreadIdxZ
    | TyBlockIdxX | TyBlockIdxY | TyBlockIdxZ
    | TyBlockDimX | TyBlockDimY | TyBlockDimZ
    | TyGridDimX | TyGridDimY | TyGridDimZ
   
    // Primitive operations on expressions.
    | TyAdd of TypedExpr * TypedExpr * Ty
    | TySub of TypedExpr * TypedExpr * Ty
    | TyMult of TypedExpr * TypedExpr * Ty
    | TyDiv of TypedExpr * TypedExpr * Ty
    | TyMod of TypedExpr * TypedExpr * Ty
    | TyLT of TypedExpr * TypedExpr
    | TyLTE of TypedExpr * TypedExpr
    | TyEQ of TypedExpr * TypedExpr
    | TyGT of TypedExpr * TypedExpr
    | TyGTE of TypedExpr * TypedExpr
    | TyLeftShift of TypedExpr * TypedExpr * Ty
    | TyRightShift of TypedExpr * TypedExpr * Ty
    | TySyncthreads
    | TyShuffleXor of TypedExpr * TypedExpr * Ty
    | TyShuffleUp of TypedExpr * TypedExpr * Ty
    | TyShuffleDown of TypedExpr * TypedExpr * Ty
    | TyShuffleIndex of TypedExpr * TypedExpr * Ty
    | TyLog of TypedExpr * Ty
    | TyExp of TypedExpr * Ty
    | TyTanh of TypedExpr * Ty
    | TyNeg of TypedExpr * Ty
    // Mutable operations.
    | TyMSet of TypedExpr * TypedExpr // statement
    | TyAtomicAdd of TypedExpr * TypedExpr * Ty
    | TyWhile of TypedExpr * TypedExpr // statement
    // Cub operations
    | TyCubBlockReduce of TypedExpr * TypedExpr option * Ty

and Env = Map<string, TypedExpr>
// method key * method body * bound variables * used variables
and MethodCases =
    | MethodInEvaluation of Ty option * Stack<unit -> TypedExpr>
    | MethodDone of TyV list * TypedExpr * Set<TyV> * Set<TyV>
and MethodDict = Dictionary<TyMethodKey, MethodCases>
and TaggedDict = Dictionary<int64,TypedExpr>
// method key * method body * implicit arguments
and MethodImplDict = Dictionary<TyMethodKey, TyV list * TypedExpr * Set<TyV>>
and Data =
    {
    // Immutable
    env : Env
    // Mutable
    tagged_vars : TaggedDict // For looking up the the unapplied Inlineables and Methods
    memoized_methods : MethodDict // For hoisted out global methods.
    sequences : Stack<TypedExpr>
    used_variables : HashSet<TyV>
    current_stack : Stack<unit -> TypedExpr>
    }

type Result<'a,'b> = Succ of 'a | Fail of 'b

let rec get_type = function
    | Inlineable'(_,_,_,t) | Method'(_,_,_,_,t) -> t

    | TyV(_,_,t) | TyIf(_,_,_,t) | TyLet((_,_,t),_) -> t
    | TyLitInt _ -> Int32T
    | TyLitFloat _ -> Float32T
    | TyLitBool _ -> BoolT
    | TyUnit -> UnitT
    | TyMethodCall(_,_,t) -> t

    // Cuda kernel constants
    | TyThreadIdxX | TyThreadIdxY | TyThreadIdxZ
    | TyBlockIdxX | TyBlockIdxY | TyBlockIdxZ
    | TyBlockDimX | TyBlockDimY | TyBlockDimZ
    | TyGridDimX | TyGridDimY | TyGridDimZ -> Int32T

    // Tuple cases
    | TyVV(_,t) | TyIndexVV(_,_, t) -> t

    // Seq
    | TySeq(_,_,t) -> t

    // Array cases
    | TyIndexArray(_,_,t) | TyCreateLocalArray(_,t) | TyCreateSharedArray(_,t) -> t

    // Primitive operations on expressions.
    | TyAdd(_,_,t) | TySub(_,_,t) | TyMult(_,_,t)
    | TyDiv(_,_,t) | TyMod(_,_,t) -> t
    | TyLT _ | TyLTE _ | TyEQ _ | TyGT _
    | TyGTE _ -> BoolT
    | TyLeftShift(_,_,t) | TyRightShift(_,_,t) -> t
    | TySyncthreads -> UnitT
    | TyShuffleXor(_,_,t) | TyShuffleUp(_,_,t)
    | TyShuffleDown(_,_,t) | TyShuffleIndex(_,_,t) -> t
    | TyLog(_,t) | TyExp(_,t) | TyTanh(_,t)
    | TyNeg(_,t) -> t
    // Mutable operations.
    | TyAtomicAdd(_,_,t) -> t
    | TyMSet(_,_) -> UnitT
    // Loops
    | TyWhile(_,_) -> UnitT
    // Cub operations
    | TyCubBlockReduce(_,_,t) -> t

let rec is_simple' = function
    | UnitT | UInt32T | UInt64T | Int32T | Int64T | Float32T 
    | Float64T | BoolT -> true
    | VVT x -> List.forall is_simple' x
    | _ -> false
let is_simple a = is_simple' (get_type a)

let rec is_numeric' = function
    | UInt32T | UInt64T | Int32T | Int64T 
    | Float32T | Float64T -> true
    | _ -> false
let is_numeric a = is_numeric' (get_type a)

let is_atomic_add_supported' = function
    | UInt32T | UInt64T | Int32T
    | Float32T | Float64T -> true
    | _ -> false
let is_atomic_add_supported a = is_atomic_add_supported' (get_type a)

let rec is_float' = function
    | Float32T | Float64T -> true
    | _ -> false
let is_float a = is_float' (get_type a)

let rec is_bool' = function
    | BoolT -> true
    | _ -> false
let is_bool a = is_bool' (get_type a)

let rec is_int' = function
    | UInt32T | UInt64T | Int32T | Int64T -> true
    | _ -> false
let is_int a = is_int' (get_type a)

let is_vv' = function
    | VVT _ -> true
    | _ -> false
let is_vv a = is_vv' (get_type a)

let get_tag =
    let mutable x = 0L
    fun () -> 
        let x' = x
        x <- x + 1L
        x'

let get_bound_variables (env: Env) =
    env
    |> Seq.choose (fun kv -> 
        match kv.Value with
        | TyV v -> Some v
        | _ -> None)
    |> Set

let map_fold_2_Er f state x y =
    let rec loop f state = function
        | x :: xs, y :: ys -> 
            let r, state = f state x y
            let rs, state = loop f state (xs,ys) 
            (r :: rs, state)
        | [], [] -> [], state
        | x -> failwith "Argument size mismatch in map_fold_2_Er."
    loop f state (x,y)

let get_body_from (stack: Stack<unit -> TypedExpr>) = stack.Peek()()

let filter_simple_vars evaled_cur_args =
    let rec loop = function
        | TyLet((_,_,t as v),_) when is_simple' t -> [v]
        | TyLet(_,_) -> []
        | TyVV(x,_) -> List.collect loop x
        | x -> failwithf "Expected: TyVV or TyLet.\nGot: %A" x
    loop evaled_cur_args

let rec with_empty_seq (d: Data) expr =
    let d' = {d with sequences = Stack()}
    let expr = exp_and_seq d' expr
    let seq = Seq.toList d.sequences
    let seq_types = seq |> List.map get_type |> VVT
    TySeq(TyVV(seq, seq_types),expr,get_type expr)

// Does macro expansion and takes note of the bound and 
// used variables in the method dictionary for the following passes.
and exp_and_seq (d: Data) exp: TypedExpr =
    let tev d exp = exp_and_seq d exp

    let dup_name_check (name_checker: HashSet<string>) arg_name f =
        match name_checker.Add arg_name || arg_name = "" || arg_name = "_" with
        | true -> f()
        | false -> failwithf "%s is a duplicate name in pattern matching." arg_name

    let make_tyv arg_name ty_exp = get_tag(), arg_name, get_type ty_exp
    let make_tyv_and_push arg_name ty_exp =
        let v = make_tyv arg_name ty_exp
        d.sequences.Push(TyLet(v,ty_exp))
        v

    let bind_typedexpr_inl name_checker acc arg_name ty_exp =
        dup_name_check name_checker arg_name <| fun _ ->
            let v = make_tyv_and_push arg_name ty_exp
            Map.add arg_name (TyV v) acc
    
    let bind_typedexpr_method name_checker acc arg_name ty_exp =
        dup_name_check name_checker arg_name <| fun _ -> 
            let v = make_tyv arg_name ty_exp
            TyLet(v,ty_exp), Map.add arg_name (TyV v) acc

    let traverse_inl t = List.fold2
    let traverse_method t f s a b = map_fold_2_Er f s a b |> fun (l,s) -> TyVV(l,t), s

    let destructure traverse r =
        match get_type r with
        | VVT x as t -> 
            let r = List.mapi (fun i t -> TyIndexVV(r,TyLitInt i,t)) x
            traverse t r
        | x -> failwithf "Unexpected arguments in destructuring.\nGot: %A" x

    let rec match_vv traverse bind acc l r =
        match l,r with
        | V x, r -> bind acc x r
        | VV l, TyVV(r,t) -> traverse t (match_vv traverse bind) acc l r
        | VV l, r -> destructure (fun t r -> traverse t (match_vv traverse bind) acc l r) r
        | l, r -> failwithf "Expected V or VV on the left side.\nGot: %A" l

    let match_vv_inl dup_name_checker = match_vv traverse_inl (bind_typedexpr_inl dup_name_checker)
    let match_vv_method dup_name_checker = match_vv traverse_method (bind_typedexpr_method dup_name_checker)

    let rec mset l r =
        match l,r with
        | VV l, TyVV(r,_) -> List.iter2 mset l r
        | VV _ as l, r -> destructure (fun t r -> mset l (TyVV(r,t))) r
        | l, r ->
            match tev d l with
            | (TyIndexArray(_,_,lt) as v) | (TyV(_,_,lt) as v) when lt = get_type r -> d.sequences.Push(TyMSet(v,r))
            | x -> failwithf "Expected `(TyIndexArray(_,_,lt) as v) | (TyV(_,_,lt) as v) when lt = get_type r`.\nGot: %A" x

    // Primitive functions
    let append_typeof_fst k a b =
        k (a, b, (get_type a))

    let prim_bin_op_template check_error is_check k a b t =
        let constraint_both_eq_numeric f k =
            let a,b = tev d a, tev d b
            if is_check a b then k a b
            else f (check_error a b)

        constraint_both_eq_numeric failwith (k t)


    let prim_arith_op = 
        let er = sprintf "`is_numeric a && get_type a = get_type b` is false.\na=%A, b=%A"
        let check a b = is_numeric a && get_type a = get_type b
        prim_bin_op_template er check append_typeof_fst

    let prim_atomic_add_op = 
        let er = sprintf "`is_atomic_add_supported a && get_type a = get_type b` is false.\na=%A, b=%A"
        let check a b = is_atomic_add_supported a && get_type a = get_type b
        prim_bin_op_template er check append_typeof_fst

    let prim_bool_op = 
        let er = sprintf "`(is_numeric a || is_bool a) && get_type a = get_type b` is false.\na=%A, b=%A"
        let check a b = (is_numeric a || is_bool a) && get_type a = get_type b
        prim_bin_op_template er check (fun t a b -> t (a,b))

    let prim_shift_op =
        let er = sprintf "`is_int a && is_int b` is false.\na=%A, b=%A"
        let check a b = is_int a && is_int b
        prim_bin_op_template er check append_typeof_fst

    let prim_shuffle_op =
        let er = sprintf "`is_int b` is false.\na=%A, b=%A"
        let check a b = is_int b
        prim_bin_op_template er check append_typeof_fst

    let prim_un_op_template check_error is_check k a t =
        let constraint_numeric f k =
            let a = tev d a
            if is_check a then k a
            else f (check_error a)

        constraint_numeric failwith (k t)

    let prim_un_floating = 
        let er = sprintf "`is_float a` is false.\na=%A"
        let check a = is_float a
        prim_un_op_template er check (fun t a -> t (a, get_type a))

    let prim_un_numeric = 
        let er = sprintf "`true` is false.\na=%A"
        let check a = true
        prim_un_op_template er check (fun t a -> t (a, get_type a))

    let create_array con args =
        let args, args' = 
            List.map (tev d) args
            |> fun args -> 
                if List.forall (fun x -> is_int x) args then args 
                else failwithf "One of the args in CreateArray is not of Type int.\nGot: %A" args
            |> List.map (fun x -> let v = make_tyv_and_push "" x in v, TyV v)
            |> List.unzip
        make_tyv_and_push "" (con args args') |> TyV

    let add_tagged f =
        let t = get_tag()
        let x = f t
        d.tagged_vars.Add(t,x)
        x

    let h0 () = HashSet(HashIdentity.Structural)

    match exp with
    | LitInt x -> TyLitInt x
    | LitFloat x -> TyLitFloat x
    | LitBool x -> TyLitBool x
    | LitUnit -> TyUnit
    | V x -> 
        match Map.tryFind x d.env with
        | Some ((TyV v) as v') -> d.used_variables.Add v |> ignore; v'
        | Some v -> v
        | None -> failwithf "Variable %A not bound." x
    | Inlineable(args, body) -> add_tagged (fun t -> Inlineable'(args, body, d.env, TagT t))
    | Method(name, args, body) -> add_tagged (fun t -> Method'(name, args, body, d.env, TagT t))
    | Apply(expr,args) ->
        let expr = tev d expr
        match get_type expr with
        | TagT t ->
            let ra = tev d args
            match d.tagged_vars.TryGetValue t with
            | true, (Inlineable'(la,body,env,_)) -> tev {d with env = match_vv_inl (h0()) env la ra} body
            | true, (Method'(name,la,body,initial_env,_) as orig) ->
                let bound_args, env = match_vv_method (h0()) initial_env la ra
                let method_key = t, get_type bound_args

                let make_method_call body = TyMethodCall(method_key, body, get_type body)

                match d.memoized_methods.TryGetValue method_key with
                | false, _ ->
                    let s = 
                        // The Stack is used for inferring the types of recursive function. 
                        // See the `RealityAlgorithm.fsx` for a more info.
                        Stack() 
                    s.Push <| fun _ -> failwith "The method is divergent."
                    d.memoized_methods.[method_key] <- MethodInEvaluation(None,s)

                    let d = {d with env = if name <> "" then env.Add(name, orig) else env
                                    used_variables=h0(); current_stack=s}
                    let body = with_empty_seq d body
                    let sole_arguments = filter_simple_vars bound_args
                    let bound_variables = get_bound_variables initial_env
                        
                    s.Clear()
                    d.memoized_methods.[method_key] <- MethodDone(sole_arguments, body, bound_variables, Set d.used_variables)

                    if is_simple body then make_method_call body
                    else failwithf "Expected a simple type as the function return.\nGot: %A" (get_type body)
                | true, MethodInEvaluation (None, stack) ->
                    let body = get_body_from stack
                    let t = get_type body
                    d.memoized_methods.[method_key] <- MethodInEvaluation (Some t,stack)
                    make_method_call body
                | true, MethodInEvaluation (Some t', stack) ->
                    let body = get_body_from stack
                    let t = get_type body
                    if t' <> t then failwithf "Unification failed. %A <> %A" t' t
                    else make_method_call body
                | true, MethodDone(_,body,_,_) ->
                    make_method_call body
            | _ -> failwith "impossible"
        | _ -> failwithf "Expected: Inlineable or Method.\nGot: %A" expr
    | If(cond,tr,fl) ->
        tev d (Apply(Method("",VV [],HoistedIf(cond,tr,fl)),VV []))
    | HoistedIf(cond,tr,fl) ->
        let cond = tev d cond
        if get_type cond <> BoolT then failwithf "Expected a bool in conditional.\nGot: %A" (get_type cond)
        else
            let tev e f =
                d.current_stack.Push f
                let x = with_empty_seq d e
                d.current_stack.Pop |> ignore
                x

            let mutable fl_result = None
            
            let tr = tev tr (fun _ -> 
                let fl = tev fl (fun _ -> failwith "Method is divergent.") 
                fl_result <- Some fl
                fl)

            let fl = 
                match fl_result with
                | Some fl -> fl
                | None -> tev fl (fun _ -> tr)

            let type_tr, type_fl = get_type tr, get_type fl
            if type_tr = type_fl then TyIf(cond,tr,fl,type_tr)
            else failwithf "Types in branches of If do not match.\nGot: %A and %A" type_tr type_fl

    | VV vars ->
        let vv = List.map (tev d) vars
        let vv_type = List.map get_type vv
        TyVV(vv,VVT(vv_type))