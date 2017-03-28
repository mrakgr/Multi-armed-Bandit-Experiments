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
    | GlobalArrayT of TyV list * Ty
    | SharedArrayT of TyV list * Ty
    | LocalArrayT of TyV list * Ty
    | TagT of int64
and TyV = int64 * Ty

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
    | ApplyAsIfMain of main_method: Expr * args: (string * (TyV list)) list * macros: (string * Expr) list
    | Method of name: string * args: Expr * body: Expr

    // Tuple cases
    | IndexVV of Expr * Expr
    | VV of Expr list // tuple

    // Array cases
    | IndexArray of Expr * Expr list
    | CreateSharedArray of Expr list * typeof: Expr
    | CreateLocalArray of Expr list * typeof: Expr

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
    | CubBlockReduce of Expr * Expr * Expr option

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
    | TySeq of TypedExpr list * TypedExpr * Ty
        
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
    | TyCubBlockReduce of input: TypedExpr * method_: TypedExpr * num_valid: TypedExpr option * Ty

and Env = Map<string, TypedExpr>
// method key * method body * bound variables
and MethodCases =
    | MethodInEvaluation of Ty option * Stack<unit -> TypedExpr>
    | MethodDone of TyV list * TypedExpr * Set<TyV>
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
    current_stack : Stack<unit -> TypedExpr>
    }

type Result<'a,'b> = Succ of 'a | Fail of 'b

let rec get_type = function
    | Inlineable'(_,_,_,t) | Method'(_,_,_,_,t) -> t

    | TyV(_,t) | TyIf(_,_,_,t) | TyLet((_,t),_) -> t
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
    | TyVV(_,t) | TyIndexVV(_,_,t) -> t

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
    | TyCubBlockReduce(_,_,_,t) -> t

let rec is_simple' = function
    | UnitT | UInt32T | UInt64T | Int32T | Int64T | Float32T 
    | Float64T | BoolT | GlobalArrayT _ -> true
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

let filter_simple_vars_template f evaled_cur_args =
    let rec loop = function
        | TyLet((_,t as v),x) when is_simple' t -> [f (v, x)]
        | TyLet(_,_) -> []
        | TyVV(x,_) -> List.collect loop x
        | x -> failwithf "Expected: TyVV or TyLet.\nGot: %A" x
    loop evaled_cur_args

let filter_simple_vars x = filter_simple_vars_template fst x

let rec with_empty_seq (d: Data) expr =
    let d = {d with sequences = Stack()}
    let expr = exp_and_seq d expr
    let seq = Seq.toList d.sequences |> List.rev
    TySeq(seq,expr,get_type expr)

// Does macro expansion and takes note of the bound and 
// used variables in the method dictionary for the following passes.
and exp_and_seq (d: Data) exp: TypedExpr =
    let tev d exp = exp_and_seq d exp

    let dup_name_check (name_checker: HashSet<string>) arg_name f =
        match name_checker.Add arg_name || arg_name = "" || arg_name = "_" with
        | true -> f()
        | false -> failwithf "%s is a duplicate name in pattern matching." arg_name

    let make_tyv arg_name ty_exp = get_tag(), get_type ty_exp
    let make_tyv_and_push arg_name ty_exp =
        let v = make_tyv arg_name ty_exp
        d.sequences.Push(TyLet(v,ty_exp))
        v

    let bind_typedexpr_inl name_checker acc arg_name ty_exp =
        dup_name_check name_checker arg_name <| fun _ ->
            match ty_exp with
            | TyV _ as v -> Map.add arg_name v acc
            | Inlineable' _ | Method' _ -> // This case is just so the typed AST is not so noisy when printed.
                let v = make_tyv arg_name ty_exp
                Map.add arg_name (TyV v) acc
            | _ ->
                let v = make_tyv_and_push arg_name ty_exp
                Map.add arg_name (TyV v) acc
    
    let bind_typedexpr_method name_checker acc arg_name ty_exp =
        dup_name_check name_checker arg_name <| fun _ -> 
            match ty_exp with
            | TyV v -> TyLet(v,ty_exp), Map.add arg_name ty_exp acc
            | _ ->
                let v = make_tyv arg_name ty_exp
                TyLet(v,ty_exp), Map.add arg_name (TyV v) acc

    let traverse_inl t = List.fold2
    let traverse_method t f s a b = map_fold_2_Er f s a b |> fun (l,s) -> TyVV(l,t), s

    let destructure traverse r =
        match get_type r with
        | VVT x as t -> 
            let r = List.mapi (fun i t -> TyIndexVV(r,TyLitInt i,t)) x
            traverse t r
        | x -> failwithf "Unexpected arguments in destructuring.\nGot: %A\nExp: %A" x exp

    let rec match_vv traverse bind acc l r =
        match l,r with
        | V x, r -> bind acc x r
        | VV l, TyVV(r,t) -> traverse t (match_vv traverse bind) acc l r
        | VV l, r -> destructure (fun t r -> traverse t (match_vv traverse bind) acc l r) r
        | l, r -> failwithf "Expected V or VV on the left side.\nGot: %A" l

    let match_vv_inl' dup_name_checker = match_vv traverse_inl (bind_typedexpr_inl dup_name_checker)
    let match_vv_method' dup_name_checker = match_vv traverse_method (bind_typedexpr_method dup_name_checker)

    let h0 () = HashSet(HashIdentity.Structural)

    let match_vv_inl = match_vv_inl' (h0())
    let match_vv_method = match_vv_method' (h0())

    let rec mset l r =
        match l,r with
        | VV l, TyVV(r,_) -> List.iter2 mset l r
        | VV _ as l, r -> destructure (fun t r -> mset l (TyVV(r,t))) r
        | l, r ->
            match tev d l with
            | (TyIndexArray(_,_,lt) as v) when lt = get_type r -> d.sequences.Push(TyMSet(v,r))
            | x -> failwithf "Expected `(TyIndexArray(_,_,lt) as v) when lt = get_type r`.\nGot: %A" x

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

    let create_array con args t =
        let t = get_type (tev d t)
        let args, args' = 
            List.map (tev d) args
            |> fun args -> 
                if List.forall (fun x -> is_int x) args then args 
                else failwithf "An arg in CreateArray is not of Type int.\nGot: %A" args
            |> List.map (fun ty_exp -> 
                match ty_exp with
                | TyV v' as v -> v, v'
                | x -> let v = make_tyv_and_push "" x in TyV v, v)
            |> List.unzip
        make_tyv_and_push "" (con args args' t) |> TyV

    let add_tagged f =
        let t = get_tag()
        let x = f t
        d.tagged_vars.Add(t,x)
        x

    let make_vvt x = List.map get_type x |> VVT

    let apply_first expr =
        let expr = tev d expr
        match get_type expr with
        | TagT t ->
            match d.tagged_vars.TryGetValue t with
            | true, v -> v
            | _ -> failwith "impossible"
        | _ -> failwithf "Expected: Inlineable or Method.\nGot: %A" expr

    let apply_inlineable (la,body,env,_) ra =
        tev {d with env = match_vv_inl env la ra} body

    let apply_method match_vv (name,la,body,initial_env,t as orig) ra =
        let t = match t with TagT t -> t | _ -> failwith "impossible"
        let bound_args, env = match_vv initial_env la ra
        let method_key = t, get_type bound_args

        let make_method_call body_type = TyMethodCall(method_key, bound_args, body_type)

        match d.memoized_methods.TryGetValue method_key with
        | false, _ ->
            let s = 
                // The Stack is used for inferring the types of recursive function. 
                // See the `RealityAlgorithm.fsx` for a more info.
                Stack() 
            s.Push <| fun _ -> failwith "The method is divergent."
            d.memoized_methods.[method_key] <- MethodInEvaluation(None,s)

            let d = {d with env = if name <> "" then Map.add name (Method' orig) env else env
                            current_stack=s}
            let body = with_empty_seq d body
            let sole_arguments = filter_simple_vars bound_args
            let bound_variables = get_bound_variables initial_env
                        
            s.Clear()
            d.memoized_methods.[method_key] <- MethodDone(sole_arguments, body, bound_variables)

            if is_simple body then make_method_call (get_type body)
            else failwithf "Expected a simple type as the function return.\nGot: %A" (get_type body)
        | true, MethodInEvaluation (None, stack) ->
            let body = get_body_from stack
            let t = get_type body
            d.memoized_methods.[method_key] <- MethodInEvaluation (Some t,stack)
            make_method_call (get_type body)
        | true, MethodInEvaluation (Some t', stack) ->
            let body = get_body_from stack
            let t = get_type body
            if t' <> t then failwithf "Unification failed. %A <> %A" t' t
            else make_method_call (get_type body)
        | true, MethodDone(_,body,_) ->
            make_method_call (get_type body)

    let apply_second apply_inl apply_method la ra =
        match la with
        | Inlineable'(a,b,c,d) -> apply_inl (a,b,c,d) ra
        | Method'(a,b,c,d,e) -> apply_method (a,b,c,d,e) ra
        | _ -> failwith "impossible"

    let apply_both = apply_second apply_inlineable (apply_method match_vv_method)
    let apply_method_only match_vv = apply_second (fun _ -> failwith "Inlineable not supported.") (apply_method match_vv)

    let apply expr args =
        let la = apply_first expr
        apply_both la (tev d args)

    match exp with
    | LitInt x -> TyLitInt x
    | LitFloat x -> TyLitFloat x
    | LitBool x -> TyLitBool x
    | LitUnit -> TyUnit
    | V x -> 
        match Map.tryFind x d.env with
        | Some v -> v
        | None -> failwithf "Variable %A not bound." x
    | Inlineable(args, body) -> add_tagged (fun t -> Inlineable'(args, body, d.env, TagT t))
    | Apply(expr,args) -> apply expr args
    | Method(name, args, body) -> add_tagged (fun t -> Method'(name, args, body, d.env, TagT t))
    | ApplyAsIfMain(exp,args,macros) ->
        // Hack so it flattens the arguments.
        let match_vv acc _ r = r, acc
        let make_tyv r = let tyv = List.map TyV r in TyVV(tyv, make_vvt tyv)
        let flattened_args = 
            List.collect snd args 
            |> List.map (fun v -> TyLet(v, TyV v)) 
            |> fun x -> TyVV(x,make_vvt x)

        // Hack so it puts the macros in the the env
        let env = 
            List.fold (fun m (n,r) -> Map.add n (make_tyv r) m) d.env args
            |> fun env -> List.fold (fun m (n,r) -> Map.add n (tev {d with env=m} r) m) env macros

        let main_method = tev {d with env=env} (Method("",VV [],exp))
        // Hack so closure conversion does not give incorrect arguments.
        match apply_method_only match_vv main_method flattened_args with
        | TyMethodCall(k,_,t) -> TyMethodCall(k,TyVV([], VVT []),t)
        | _ -> failwith "impossible"
    | If(cond,tr,fl) -> tev d (Apply(Method("",VV [],HoistedIf(cond,tr,fl)),VV []))
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
        TyVV(vv,make_vvt vv)

    | IndexVV(v,i) ->
        match tev d v, tev d i with
        | v, (TyLitInt i as i') ->
            match get_type v with
            | VVT ts -> 
                if i >= 0 || i < List.length ts then TyIndexVV(v,i',ts.[i])
                else failwith "(i >= 0 || i < List.length ts) = false in IndexVT"
            | x -> failwithf "Type of a evaluated expression in IndexVT is not VTT.\nGot: %A" x
        | v, i ->
            failwithf "Index into a tuple must be a natural number less than the size of the tuple.\nGot: %A" i

    // Array cases
    | IndexArray(exp,args) ->
        let exp,args = tev d exp, List.map (tev d) args
        match get_type exp with
        | LocalArrayT(vs, t) | SharedArrayT(vs, t) | GlobalArrayT(vs, t) when List.forall is_int args && List.length vs = List.length args ->
            TyIndexArray(exp,args,t)
        | _ -> failwithf "Something is wrong in IndexArray.\nexp=%A, args=%A" exp args

    | CreateLocalArray(args,t) ->
        create_array (fun args args' t -> TyCreateLocalArray(args,LocalArrayT(args', t))) args t
    | CreateSharedArray(args,t) ->
        create_array (fun args args' t -> TyCreateSharedArray(args,SharedArrayT(args', t))) args t

    | ThreadIdxX -> TyThreadIdxX 
    | ThreadIdxY -> TyThreadIdxY 
    | ThreadIdxZ -> TyThreadIdxZ
    | BlockIdxX -> TyBlockIdxX 
    | BlockIdxY -> TyBlockIdxY 
    | BlockIdxZ -> TyBlockIdxZ
    | BlockDimX -> TyBlockDimX 
    | BlockDimY -> TyBlockDimY 
    | BlockDimZ -> TyBlockDimZ
    | GridDimX -> TyGridDimX 
    | GridDimY -> TyGridDimY 
    | GridDimZ -> TyGridDimZ

    // Primitive operations on expressions.
    | Add(a,b) -> prim_arith_op a b TyAdd
    | Sub(a,b) -> prim_arith_op a b TySub
    | Mult(a,b) -> prim_arith_op a b TyMult
    | Div(a,b) -> prim_arith_op a b TyDiv
    | Mod(a,b) -> prim_arith_op a b TyMod
        
    | LT(a,b) -> prim_bool_op a b TyLT
    | LTE(a,b) -> prim_bool_op a b TyLTE
    | EQ(a,b) -> prim_bool_op a b TyEQ
    | GT(a,b) -> prim_bool_op a b TyGT
    | GTE(a,b) -> prim_bool_op a b TyGTE

    | Syncthreads -> TySyncthreads

    | LeftShift(a,b) -> prim_shift_op a b TyLeftShift
    | RightShift(a,b) -> prim_shift_op a b TyRightShift
    
    | ShuffleXor(a,b) -> prim_shuffle_op a b TyShuffleXor
    | ShuffleUp(a,b) -> prim_shuffle_op a b TyShuffleUp
    | ShuffleDown(a,b) -> prim_shuffle_op a b TyShuffleDown
    | ShuffleIndex(a,b) -> prim_shuffle_op a b TyShuffleIndex

    | Log a -> prim_un_floating a TyLog
    | Exp a -> prim_un_floating a TyExp
    | Tanh a -> prim_un_floating a TyTanh
    | Neg a -> prim_un_numeric a TyNeg
    // Mutable operations.
    | MSet(a,b,rest) -> mset a (tev d b); tev d rest
    | AtomicAdd(a,b) -> prim_atomic_add_op a b TyAtomicAdd
    | While(cond,body,e) ->
        let cond, body = tev d cond, with_empty_seq d body
        match get_type cond, get_type body with
        | BoolT, UnitT -> d.sequences.Push(TyWhile(cond,body)); tev d e
        | BoolT, _ -> failwith "Expected UnitT as the type of While's body."
        | _ -> failwith "Expected BoolT as the type of While's conditional."
    | CubBlockReduce(input, method_, num_valid) ->
        let evaled_input = tev d input
        let method_ =
            match get_type evaled_input with
            | LocalArrayT(_,t) | SharedArrayT(_,t) -> 
                let arg = IndexArray(input,[LitInt 0])
                tev d (VV [arg;arg])
            | x -> tev d (VV [input; input])
            |> apply_method_only match_vv_method (apply_first method_)

        let num_valid = Option.map (tev d) num_valid
        TyCubBlockReduce(evaled_input,method_,num_valid,get_type method_)
            

// Unions the free variables from top to bottom of the call chain.
let rec closure_conv (imemo: MethodImplDict) (memo: MethodDict) (exp: TypedExpr) =
    let c x = closure_conv imemo memo x
    match exp with
    | Method' _ | Inlineable' _ -> Set.empty
    | TyV v -> Set.singleton v
    | TyIf(cond,tr,fl,t) ->
        let cond, tr, fl = c cond, c tr, c fl
        Set.unionMany [|cond; tr; fl|]
    | TyLet(_,body) -> c body
    | TyLitInt _ | TyLitFloat _ | TyLitBool _ | TyUnit -> Set.empty
    | TyVV(vars,t) -> Set.unionMany (List.map c vars)
    | TyIndexVV(t,i,_) -> Set.union (c t) (c i)
    | TyMethodCall(m,ar,_) ->
        let method_implicit_args =
            match imemo.TryGetValue m with
            | true, (_,_,impl_args) -> impl_args
            | false, _ ->
                match memo.[m] with
                | MethodDone(sol_arg, body, bound_variables) ->
                    // union the free vars from top to bottom
                    let impl_args = Set.intersect bound_variables (c body) - Set(sol_arg)
                    imemo.Add(m,(sol_arg,body,impl_args))
                    impl_args
                | _ -> failwith "impossible"
        Set.union method_implicit_args (c ar)
    | TyCubBlockReduce(inp,(TyMethodCall(key,_,_) as m),num_valid,t) ->
        ignore <| c m // This is so it gets added to the env.

        match imemo.[key] with
        | _,_,impl_args when impl_args.IsEmpty ->
            match num_valid with
            | Some num_valid -> Set.union (c num_valid) (c inp)
            | None -> c inp
        | _ -> 
            failwithf "The method passed to Cub should have no implicit arguments.\nm=%A" m
            
    // Array cases
    | TyIndexArray(a,b,t) -> 
        let a = 
            match get_type a with
            | LocalArrayT(x,_) | SharedArrayT(x,_) | GlobalArrayT(x,_) ->
                Set.union (c a) (Set(x))
            | _ -> failwith "impossible"
        Set.union a (Set.unionMany (List.map c b))
    | TyCreateLocalArray(b,_) | TyCreateSharedArray(b,_) -> Set.unionMany (List.map c b)
    // Cuda kernel constants
    | TyThreadIdxX | TyThreadIdxY | TyThreadIdxZ
    | TyBlockIdxX | TyBlockIdxY | TyBlockIdxZ
    | TyBlockDimX | TyBlockDimY | TyBlockDimZ
    | TyGridDimX | TyGridDimY | TyGridDimZ -> Set.empty
    // Primitive operations on expressions.
    | TySyncthreads -> Set.empty
    | TyLog(a,_) | TyExp(a,_) | TyTanh(a,_) | TyNeg(a,_) -> c a
    | TyAdd(a,b,_) | TySub(a,b,_) | TyMult(a,b,_) | TyDiv(a,b,_) | TyMod(a,b,_)
    | TyLT(a,b) | TyLTE(a,b) | TyEQ(a,b) | TyGT(a,b) | TyGTE(a,b) 
    | TyLeftShift(a,b,_) | TyRightShift(a,b,_) | TyShuffleXor(a,b,_)
    | TyShuffleUp(a,b,_) | TyShuffleDown(a,b,_) | TyShuffleIndex(a,b,_) 
    | TyAtomicAdd(a,b,_) | TyWhile(a,b) | TyMSet(a,b) -> Set.union (c a) (c b)
    | TySeq(a,b,_) -> Set.union (Set.unionMany (List.map c a)) (c b)


let l v b e = Apply(Inlineable(v,e),b)

let d0() = Dictionary(HashIdentity.Structural)
let data_empty() = 
    {memoized_methods=d0();tagged_vars=d0();
     env=Map.empty;sequences=Stack();current_stack=Stack()}

let typecheck body inputs macros = 
    try
        let main_method, memo = 
            let d = data_empty()
            exp_and_seq d (ApplyAsIfMain(body,inputs,macros)), d.memoized_methods
        let imemo = Dictionary(HashIdentity.Structural)
        closure_conv imemo memo main_method |> ignore
        Succ imemo
    with e -> Fail (e.Message, e.StackTrace)

let typecheck0 program = typecheck program []

let inl x y = Inlineable(x,y)
let ap x y = Apply(x,y)

let term0 =
    let snd = inl (VV [V "a";V "b"]) (V "b")
    ap (inl (VV [V "x";V "y";V "z";V "r"]) (ap (V "r") (VV [V "y";V "z"]))) (VV [LitUnit;LitBool true;LitInt 5;snd])

let t0 = typecheck0 term0

let term1 =
    let fst = inl (VV [V "a";V "b"]) (V "a")
    let snd = inl (VV [V "a";V "b"]) (V "b")
    l (VV [V "x";V "y";V "z"]) (VV [LitUnit;LitBool true;LitInt 5])
        (l (V "q") (fst) (ap (V "q") (VV [V "y";V "z"])))

let t1 = typecheck0 term1
    
// This particular test evolved during the course of the development.
// Previously it would inline the terms completely, now I've simplified it
// and it just gives an error.
let term2 = 
    let fst = inl (VV [V "a";V "b"]) (V "a")
    let snd = inl (VV [V "a";V "b"]) (V "b")
    l (VV [V "a";V "b"]) (VV [LitInt 2;LitFloat 3.3]) (ap (If(LitBool true,snd,snd)) (VV [V "a";V "b"]))

let t2 = typecheck0 term2

let term3 = // Error in the first line.
    l (V "inlineable") (VV [inl (VV [V "a";V "b"]) (V "b")])
        (l (V "fun")
            (inl (VV [V "inl";V "a";V "b";V "c";V "d"]) (ap (V "inl") (VV [V "b";V "c"])))
            (ap (V "fun") (VV [V "inlineable"; LitBool true; LitInt 2; LitFloat 1.5; LitInt 2])))
let t3 = typecheck0 term3

let term3' =
    l (V "inlineable") (inl (VV [V "a";V "b"]) (V "b"))
        (l (V "fun")
            (inl (VV [V "inl";V "a";V "b";V "c";V "d"]) (ap (V "inl") (VV [V "b";V "c"])))
            (ap (V "fun") (VV [V "inlineable"; LitBool true; LitInt 2; LitFloat 1.5; LitInt 2])))
let t3' = typecheck0 term3'

let term4 = // If test
    l (V "if") (inl (VV [V "cond";V "tr";V "fl"]) (If(V "cond",V "tr",V "fl")))
        (l (V "cond") (LitBool true)
            (l (V "tr") (LitFloat 3.33)
                (l (V "fl") (LitFloat 4.44)
                    (ap (V "if") (VV [V "cond";V "tr";V "fl"])))))

let t4 = typecheck0 term4

let meth x y = Method("",x,y)

let meth1 = 
    l (VV [V "fun";V "id"])
        (VV [meth (VV [V "x";V "y";V "z";V "f"])
                (l (V "t") (LitInt 3)
                    (l (V "u") (LitBool false) (ap (V "f") (V "z"))))
             meth (V "x") (V "x")])
        (ap (V "fun") (VV [LitBool true; LitInt 2; LitFloat 4.4;V "id"]))
let m1 = typecheck0 meth1

let meth2 = // closure conversion test
    l (V "m")
        (meth (VV [V "a";V "n";V "qwe"]) 
            (l (V "loop") 
                (meth (VV [V "acc";V "q"]) 
                    (l (V "loop_method") (meth (VV [V "a";V "n"]) (If(LitBool true,V "a",V "n")))
                        (ap (V "loop_method") (VV [V "a"; V "n"]))))
                (ap (V "loop") (VV [LitInt 1; V "n"]))))
        (ap (V "m") (VV [LitInt 3;LitInt 2;LitFloat 3.5]))
let m2 = typecheck0 meth2

let meth3 = // vv test
    l (V "m") (meth (VV [V "a"; V "b"; V "c"]) (V "c"))
        (ap (V "m") (VV [LitInt 2; LitFloat 3.3; LitBool true]))

let m3 = typecheck0 meth3

let meth4 = // vv test 2
    l (V "m") (meth (V "vars") (l (VV [V "a"; V "b"; V "c"]) (V "vars") (V "c"))) 
        (ap (V "m") (VV [LitInt 2; LitFloat 3.3; LitBool true]))
let m4 = typecheck0 meth4

