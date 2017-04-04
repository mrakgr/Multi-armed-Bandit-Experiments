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
    | GlobalArrayT of TypedExpr list * Ty
    | SharedArrayT of TypedExpr list * Ty
    | LocalArrayT of TypedExpr list * Ty
    | TagT of int64
and TyV = int64 * Ty

// No return type polymorphism like in Haskell for now. Local type inference only.
and TyMethodKey = int64 * TypedExpr // The key does not need to know the free variables.

and Expr = 
    | V of string // standard variable
    | V' of TyV // given variable
    | T of TypedExpr
    | B // blank
    | If of Expr * Expr * Expr
    | Inlineable of Expr * Expr
    | LitInt of int
    | LitFloat of float
    | LitBool of bool
    | Apply of Expr * args: Expr
    | Method of name: string * args: Expr * body: Expr

    // Tuple cases
    | IndexVV of Expr * Expr
    | VV of Expr list // tuple
    | MapVV of Expr * Expr

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
    | TyMethodCall of TyMethodKey * implicit_args: (TyV list) option ref * Ty
    
    // Tuple cases
    | TyIndexVV of TypedExpr * TypedExpr * Ty
    | TyVV of TypedExpr list * Ty

    // Seq
    | TySeq of TypedExpr list * TypedExpr * Ty
        
    // Array cases
    | TyIndexArray of TypedExpr * TypedExpr list * Ty
    | TyCreateArray of Ty

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
    | MethodInEvaluation of Ty option
    | MethodDone of TypedExpr
and MethodDict = ResizeArray<TyMethodKey * MethodCases>
and TaggedDict = Dictionary<int64,TypedExpr>
// method key * method body * implicit arguments
and MethodImplDict = ResizeArray<TyMethodKey * (TyV list * TypedExpr)>
and Data =
    {
    // Immutable
    env : Env
    // Mutable
    tagged_vars : TaggedDict // For looking up the the unapplied Inlineables and Methods
    memoized_methods : MethodDict // For hoisted out global methods.
    sequences : Stack<TypedExpr> // For sequencing statements.
    recursive_methods_stack : Stack<unit -> TypedExpr> // For typechecking recursive calls.
    }

type Result<'a,'b> = Succ of 'a | Fail of 'b

let rec get_type = function
    | Inlineable'(_,_,_,t) | Method'(_,_,_,_,t) -> t

    | TyV(_,t) | TyIf(_,_,_,t) -> t
    | TyLitInt _ -> Int32T
    | TyLitFloat _ -> Float32T
    | TyLitBool _ -> BoolT
    | TyLet((_,_),_) | TyUnit -> UnitT
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
    | TyIndexArray(_,_,t) | TyCreateArray(t) -> t

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

let map_fold_2_Er f state x y =
    let rec loop f state = function
        | x :: xs, y :: ys -> 
            let r, state = f state x y
            let rs, state = loop f state (xs,ys) 
            (r :: rs, state)
        | [], [] -> [], state
        | x -> failwith "Argument size mismatch in map_fold_2_Er."
    loop f state (x,y)

let get_body_from (stack: Stack<unit -> TypedExpr>) = 
    if stack.Count > 0 then stack.Peek()()
    else failwith "The program is divergent."

let filter_simple_vars evaled_cur_args =
    let rec loop = function
        | TyV(_,t as v) when is_simple' t -> [v]
        | TyVV(x,_) -> List.collect loop x
        | _ -> []
    loop evaled_cur_args

let h0() = HashSet(HashIdentity.Structural)
let d0() = Dictionary(HashIdentity.Structural)

type DualRenameMap = HashSet<int64> * HashSet<int64> * HashSet<int64 * int64>
let dual_eq_helper ((a,b,ab): DualRenameMap) a' b' = 
    (a.Contains a' && b.Contains b' && ab.Contains(a',b'))
    || (a.Add a' = false && b.Add b' = false && ab.Add(a',b') = false)

let rec dual_eq_typedexpr (map: DualRenameMap) a b =
    match a,b with
    | TyV(a1,t1), TyV(a2,t2) -> 
        let a, b = get_type a, get_type b
        dual_eq_type map a b 
        && dual_eq_helper map a1 a2
    | TyV _, _ | _, TyV _ -> false
    | TyVV (a,_), TyVV (b,_) -> List.forall2 (dual_eq_typedexpr map) a b
    | TyLitInt a, TyLitInt b -> a = b
    | TyLitFloat a, TyLitFloat b -> a = b
    | TyLitBool a, TyLitBool b -> a = b
    | a,b -> dual_eq_type map (get_type a) (get_type b)

and dual_eq_type (map: DualRenameMap) a b =
    match a,b with
    | GlobalArrayT (a1,t1), GlobalArrayT (a2,t2) 
    | SharedArrayT (a1,t1), SharedArrayT (a2,t2) 
    | LocalArrayT (a1,t1), LocalArrayT (a2,t2) ->
        List.forall2 (dual_eq_typedexpr map) a1 a2
        && dual_eq_type map t1 t2
    | a,b -> a = b

let dual_eq_make_map a b =
    let m = h0(), h0(), h0()
    if dual_eq_typedexpr m a b then
        Some m
    else
        None

let dual_eq_a_to_b ((_,_,map_ab): DualRenameMap) a = 
    let map_ab = dict map_ab
    List.map (function
        | (tag,t) as x ->
            match map_ab.TryGetValue tag with
            | true, v -> (v,t)
            | false, _ -> x) a

let find_key_template find (key,args) =
    find (fun ((key',args'),_) -> key = key' && Option.isSome (dual_eq_make_map args args'))

let set_key_template find (ar: ResizeArray<_>) k v =
    match find k with
    | None -> ar.Add (k,v)
    | Some i -> ar.[i] <- (k, v)

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

    let make_tyv ty_exp = 
        let v = get_tag(), get_type ty_exp
        v
    let make_tyv_and_push' ty_exp =
        let v = make_tyv ty_exp
        d.sequences.Push(TyLet(v,ty_exp))
        v
    let make_tyv_and_push ty_exp = 
        make_tyv_and_push' ty_exp |> TyV

    let bind map_add name_checker acc arg_name ty_exp =
        dup_name_check name_checker arg_name <| fun _ ->
            map_add arg_name ty_exp acc

    let bind_inl = bind (fun arg_name x acc -> Map.add arg_name x acc)
    let bind_method = bind (fun arg_name x acc -> x, Map.add arg_name x acc)
    
    let traverse_inl t = List.fold2
    let traverse_method t f s a b = map_fold_2_Er f s a b |> fun (l,s) -> TyVV(l,t), s

    // for a shallow version, take a look at `alternative_destructure_v6e.fsx`. 
    // It can be straightforwardly derived from this using the Y combinator.
    let rec destructure_deep r = 
        match r with
        | TyLitBool _ | TyLitFloat _ | TyLitBool _ | TyUnit
        | Inlineable' _ | Method' _ | TyV _ -> r
        | TyVV(l,t) -> List.map destructure_deep l |> fun x -> TyVV(x,t)
        | _ -> 
            match get_type r with
            | VVT tuple_types -> 
                let indexed_tuple_args = List.mapi (fun i typ -> 
                    destructure_deep <| TyIndexVV(r,TyLitInt i,typ)) tuple_types
                TyVV(indexed_tuple_args, VVT tuple_types)
            | _ -> make_tyv_and_push r

    let rec match_vv traverse bind acc l r =
        let recurse acc l r = match_vv traverse bind acc l r
        match l, r with // destructure_deep is called in apply_method and apply_inlineable
        | B, r -> bind acc "" r
        | V x, r -> bind acc x r
        | VV l, TyVV(r,t) -> traverse t recurse acc l r
        | VV l, r -> failwithf "Cannot destructure %A." r
        | l, r -> failwithf "Expected V or VV on the left side.\nGot: %A" l

    let rec match_vv_inl' dup_name_checker = match_vv traverse_inl (bind_inl dup_name_checker)
    let rec match_vv_method' dup_name_checker = match_vv traverse_method (bind_method dup_name_checker)

    let match_vv_inl = match_vv_inl' (h0())
    let match_vv_method = match_vv_method' (h0())

    let rec mset l r =
        match l, r with // destructure_deep is called in the MSet case.
        | VV l, TyVV(r,_) -> List.iter2 mset l r
        | VV l, r -> failwithf "Cannot destructure %A." r
        | l, r ->
            match tev d l with
            | (TyIndexArray(_,_,lt) as v) when lt = get_type r -> d.sequences.Push(TyMSet(v,r))
            | x -> failwithf "Expected `(TyIndexArray(_,_,lt) as v) when lt = get_type r`.\nGot: %A" x

    let rec map_tyvv f = function
        | TyVV(r,_) -> VV (List.map (map_tyvv f) r)
        | x -> Apply(f,T x)
        
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

    let process_create_array args typeof_expr =
        let typ = get_type (tev d typeof_expr)
        let args = 
            List.map (tev d) args
            |> fun args -> 
                if List.forall is_int args then args 
                else failwithf "An arg in CreateArray is not of Type int.\nGot: %A" args
            |> List.map destructure_deep
        args, typ

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
        tev {d with env = match_vv_inl env la (destructure_deep ra)} body

    let find_key = find_key_template (fun f -> Seq.tryFind f d.memoized_methods) >> Option.map snd
    let find_key_index = find_key_template (fun f -> Seq.tryFindIndex f d.memoized_methods)
    let set_key = set_key_template find_key_index d.memoized_methods

    let apply_method match_vv (name,la,body,initial_env,t as orig) ra =
        let t = match t with TagT t -> t | _ -> failwith "impossible"
        let bound_args, env = match_vv initial_env la (destructure_deep ra)
        let method_key = t, bound_args

        let make_method_call body_type = TyMethodCall(method_key, ref None, body_type)

        match find_key method_key with
        | None ->
            set_key method_key (MethodInEvaluation(None))

            let d = {d with env = if name <> "" then Map.add name (Method' orig) env else env}
            let body = with_empty_seq d body
                        
            set_key method_key (MethodDone body)

            if is_simple body then make_method_call (get_type body)
            else failwithf "Expected a simple type as the function return.\nGot: %A" (get_type body)
        | Some (MethodInEvaluation None) ->
            let body = get_body_from d.recursive_methods_stack
            let t = get_type body
            set_key method_key (MethodInEvaluation (Some t))
            make_method_call t
        | Some (MethodInEvaluation (Some t)) ->
            make_method_call t
        | Some (MethodDone body) ->
            make_method_call (get_type body)

    let apply_second apply_inl apply_method la ra =
        match la with
        | Inlineable'(a,b,c,d) -> apply_inl (a,b,c,d) ra
        | Method'(a,b,c,d,e) -> apply_method (a,b,c,d,e) ra
        | _ -> failwith "impossible"

    let apply_both = apply_second apply_inlineable (apply_method match_vv_method)
    let apply_method_only match_vv = apply_second (fun _ -> failwith "Inlineable not supported.") (apply_method match_vv)

    let apply expr args = apply_both (apply_first expr) (tev d args)

    match exp with
    | T x -> x // To assist in CubBlockReduce so evaled cases do not have to be evaluated twice.
    | V' x -> TyV x // To assist in interfacing with the outside.
    | LitInt x -> TyLitInt x
    | LitFloat x -> TyLitFloat x
    | LitBool x -> TyLitBool x
    | B -> TyUnit
    | V x -> 
        match Map.tryFind x d.env with
        | Some v -> v
        | None -> failwithf "Variable %A not bound." x
    | Inlineable(args, body) -> add_tagged (fun t -> Inlineable'(args, body, d.env, TagT t))
    | Method(name, args, body) -> add_tagged (fun t -> Method'(name, args, body, d.env, TagT t))
    | Apply(expr,args) -> apply expr args
    | If(cond,tr,fl) -> //tev d (Apply(Method("",VV [],HoistedIf(cond,tr,fl)),VV []))
        let cond = tev d cond
        if get_type cond <> BoolT then failwithf "Expected a bool in conditional.\nGot: %A" (get_type cond)
        else
            let tev' e f =
                d.recursive_methods_stack.Push f
                let x = with_empty_seq d e
                d.recursive_methods_stack.Pop |> ignore
                x

            let mutable fl_result = None
            
            let tr = tev' tr (fun _ -> 
                let fl = tev d fl
                fl_result <- Some fl
                fl)

            let fl = 
                match fl_result with
                | Some fl -> fl
                | None -> tev' fl (fun _ -> tr)

            let type_tr, type_fl = get_type tr, get_type fl
            if type_tr = type_fl then tev d (Apply(Method("",VV [],T (TyIf(cond,tr,fl,type_tr))),VV []))
            else failwithf "Types in branches of If do not match.\nGot: %A and %A" type_tr type_fl
    | MapVV(a,b) ->
        let a,b = tev d a, tev d b
        tev d (map_tyvv (T a) (destructure_deep b))
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

    | CreateLocalArray(args,t) -> let args,t = process_create_array args t in TyCreateArray(LocalArrayT(args,t))
    | CreateSharedArray(args,t) -> let args,t = process_create_array args t in TyCreateArray(SharedArrayT(args,t))

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
    | MSet(a,b,rest) -> mset a (destructure_deep (tev d b)); tev d rest
    | AtomicAdd(a,b) -> prim_atomic_add_op a b TyAtomicAdd
    | While(cond,body,e) ->
        let cond, body = tev d (Apply(Method("",VV [],cond),VV [])), with_empty_seq d body
        match get_type cond, get_type body with
        | BoolT, UnitT -> d.sequences.Push(TyWhile(cond,body)); tev d e
        | BoolT, _ -> failwith "Expected UnitT as the type of While's body."
        | _ -> failwith "Expected BoolT as the type of While's conditional."
    | CubBlockReduce(input, method_, num_valid) ->
        let evaled_input = tev d input
        let method_ =
            match get_type evaled_input with
            | LocalArrayT(_,t) | SharedArrayT(_,t) -> 
                let arg = IndexArray(T evaled_input,[LitInt 0])
                tev d (VV [arg;arg])
            | x -> tev d (VV [T evaled_input; T evaled_input])
            |> apply_method_only match_vv_method (apply_first method_)

        let num_valid = Option.map (tev d) num_valid
        TyCubBlockReduce(evaled_input,method_,num_valid,get_type method_)
            
// Unions the free variables from top to bottom of the call chain.
let closure_conv (imemo: MethodImplDict) (memo: MethodDict) (exp: TypedExpr) =
    let find_key ar = find_key_template (fun f -> Seq.tryFind f ar)
    let find_key_index ar = find_key_template (fun f -> Seq.tryFindIndex f ar)
    let set_key ar = set_key_template (find_key_index ar) ar

    let rec closure_conv (bound_vars: HashSet<TyV>) (exp: TypedExpr) =
        let add_var v = bound_vars.Add v |> ignore
        let c x = closure_conv bound_vars x
        match exp with
        | Method' _ | Inlineable' _ -> Set.empty
        | TyV v -> Set.singleton v
        | TyIf(cond,tr,fl,t) ->
            let cond, tr, fl = c cond, c tr, c fl
            Set.unionMany [|cond; tr; fl|]
        | TyLet(v,body) -> 
            add_var v
            c body
        | TyLitInt _ | TyLitFloat _ | TyLitBool _ | TyUnit -> Set.empty
        | TyVV(vars,t) -> Set.unionMany (List.map c vars)
        | TyIndexVV(t,i,_) -> Set.union (c t) (c i)
        | TyMethodCall((_,expl_args' as key'),impl_ref,_) ->
            let method_implicit_args =
                let make_iml_args' expl_args expl_args' impl_args =
                    match dual_eq_make_map expl_args expl_args' with
                    | Some map -> dual_eq_a_to_b map impl_args
                    | None -> failwith "Compiler error in closure conversion. Mapping creation for implicit arguments failed."
                let fill_ref_and_conv_to_set expl_args expl_args' impl_args =
                    let impl_args' = make_iml_args' expl_args expl_args' impl_args
                    impl_ref := Some impl_args'
                    Set impl_args'
                match find_key imemo key' with
                | Some ((_,expl_args),(impl_args, _)) -> 
                    fill_ref_and_conv_to_set expl_args expl_args' impl_args
                | None ->
                    match find_key memo key' with
                    | Some ((tag,expl_args as key),MethodDone body) -> // union the free vars from top to bottom
                        let sol_args = filter_simple_vars expl_args
                        // Without this line the main function would not propagate implicit arguments correctly.
                        for x in sol_args do bound_vars.Add x |> ignore
                        // Copies the HashSet as it goes into a new scope.
                        let impl_args_set = Set.intersect (closure_conv (HashSet(bound_vars)) body) (Set(bound_vars)) - Set(sol_args)
                        let impl_args = impl_args_set |> Set.toList
                        set_key imemo key (impl_args,body)
                        fill_ref_and_conv_to_set expl_args expl_args' impl_args
                    | _ -> failwith "impossible"
            Set.union method_implicit_args (c expl_args')
        | TyCubBlockReduce(inp,(TyMethodCall(key',impl_ref,_) as m),num_valid,t) ->
            ignore <| c m // This is so it gets added to the env.
            match impl_ref.Value.Value with
            | [] ->
                match num_valid with
                | Some num_valid -> Set.union (c num_valid) (c inp)
                | None -> c inp
            | _ ->
                failwithf "The method passed to Cub should have no implicit arguments.\nm=%A" m
        | TyCubBlockReduce _ -> failwith "impossible"
        // Array caseshat 
        | TyIndexArray(a,b,t) -> 
            let a = 
                match get_type a with
                | LocalArrayT(x,_) | SharedArrayT(x,_) | GlobalArrayT(x,_) ->
                    let b = 
                        if x.IsEmpty then [] 
                        else List.choose (function TyV v -> Some v | _ -> None) (List.tail x)
                    Set.union (c a) (Set(b))
                | _ -> failwith "impossible"
            Set.union a (Set.unionMany (List.map c b))
        | TyCreateArray t -> 
            match t with
            | LocalArrayT(x,_) | SharedArrayT(x,_) | GlobalArrayT(x,_) ->
                Set.unionMany (List.map c x)
            | _ -> failwith "impossible"
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
    closure_conv (h0()) exp

let l v b e = Apply(Inlineable(v,e),b)

let data_empty() = 
    {memoized_methods=ResizeArray();tagged_vars=d0()
     env=Map.empty;sequences=Stack();recursive_methods_stack=Stack()}

let typecheck body inputs = 
    try
        let main_method, memo = 
            let d = data_empty()
            exp_and_seq d (Apply(body,inputs)), d.memoized_methods
        let imemo = ResizeArray()
        closure_conv imemo memo main_method |> ignore
        Succ imemo
    with e -> Fail (e.Message, e.StackTrace)

let typecheck0 program = typecheck program (VV [])

let inl x y = Inlineable(x,y)
let ap x y = Apply(x,y)
let meth x y = Method("",x,y)

let term0 =
    let snd = inl (VV [V "a";V "b"]) (V "b")
    meth (VV [])
        (ap (inl (VV [V "x";V "y";V "z";V "r"]) (ap (V "r") (VV [V "y";V "z"]))) (VV [B;LitBool true;LitInt 5;snd]))

let t0 = typecheck0 term0

let term1 =
    let fst = inl (VV [V "a";V "b"]) (V "a")
    let snd = inl (VV [V "a";V "b"]) (V "b")
    meth (VV [])
        (l (VV [V "x";V "y";V "z"]) (VV [B;LitBool true;LitInt 5])
            (l (V "q") (fst) (ap (V "q") (VV [V "y";V "z"]))))

let t1 = typecheck0 term1
    
// This particular test evolved during the course of the development.
// Previously it would inline the terms completely, now I've simplified it
// and it just gives an error.
let term2 = 
    let fst = inl (VV [V "a";V "b"]) (V "a")
    let snd = inl (VV [V "a";V "b"]) (V "b")
    meth (VV [])
        (l (VV [V "a";V "b"]) (VV [LitInt 2;LitFloat 3.3]) (ap (If(LitBool true,snd,snd)) (VV [V "a";V "b"])))

let t2 = typecheck0 term2

let term3 = // Error in the first line.
    meth (VV [])
        (l (V "inlineable") (VV [inl (VV [V "a";V "b"]) (V "b")])
            (l (V "fun")
                (inl (VV [V "inl";V "a";V "b";V "c";V "d"]) (ap (V "inl") (VV [V "b";V "c"])))
                (ap (V "fun") (VV [V "inlineable"; LitBool true; LitInt 2; LitFloat 1.5; LitInt 2]))))
let t3 = typecheck0 term3

let term3' =
    meth (VV [])
        (l (V "inlineable") (inl (VV [V "a";V "b"]) (V "b"))
            (l (V "fun")
                (inl (VV [V "inl";V "a";V "b";V "c";V "d"]) (ap (V "inl") (VV [V "b";V "c"])))
                (ap (V "fun") (VV [V "inlineable"; LitBool true; LitInt 2; LitFloat 1.5; LitInt 2]))))
let t3' = typecheck0 term3'

let term4 = // If test
    meth (VV [])
        (l (V "if") (inl (VV [V "cond";V "tr";V "fl"]) (If(V "cond",V "tr",V "fl")))
            (l (V "cond") (LitBool true)
                (l (V "tr") (LitFloat 3.33)
                    (l (V "fl") (LitFloat 4.44)
                        (ap (V "if") (VV [V "cond";V "tr";V "fl"]))))))

let t4 = typecheck0 term4

let meth1 = 
    meth (VV [])
        (l (VV [V "fun";V "id"])
            (VV [meth (VV [V "x";V "y";V "z";V "f"])
                    (l (V "t") (LitInt 3)
                        (l (V "u") (LitBool false) (ap (V "f") (V "z"))))
                 meth (V "x") (V "x")])
            (ap (V "fun") (VV [LitBool true; LitInt 2; LitFloat 4.4;V "id"])))
let m1 = typecheck0 meth1

let meth2 = // closure conversion test
    meth (VV [])
        (l (V "m")
            (meth (VV [V "a";V "n";V "qwe"]) 
                (l (V "loop") 
                    (meth (VV [V "acc";V "q"]) 
                        (l (V "loop_method") (meth (VV [V "a";V "n"]) (If(LitBool true,V "a",V "n")))
                            (ap (V "loop_method") (VV [V "a"; V "n"]))))
                    (ap (V "loop") (VV [LitInt 1; V "n"]))))
            (ap (V "m") (VV [LitInt 3;LitInt 2;LitFloat 3.5])))
let m2 = typecheck0 meth2

let meth3 = // vv test
    meth (VV [])
        (l (V "m") (meth (VV [V "a"; V "b"; V "c"]) (V "c"))
            (ap (V "m") (VV [LitInt 2; LitFloat 3.3; LitBool true])))

let m3 = typecheck0 meth3

let meth4 = // vv test 2
    meth (VV [])
        (l (V "m") (meth (V "vars") (l (VV [V "a"; V "b"; V "c"]) (V "vars") (V "c"))) 
            (ap (V "m") (VV [LitInt 2; LitFloat 3.3; LitBool true])))
let m4 = typecheck0 meth4

printfn "%A" m4