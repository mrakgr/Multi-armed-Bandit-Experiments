// TODO: Tuple only has 10 variants and no zero case. Fix that.

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
    | VTT of Ty list // represents the value tuple
    | NominalT of string // for classes and such
    | ConstT of Ty
    | SharedT of Ty
    | ArrayT of TyV list * Ty
    | ArrT of Ty list * Ty
and TyV = int64 * string * Ty

type MethodArgs =
    | MATy of Ty
    | MATag of int64
    | MAVV of MethodArgs list
    | MAET of MethodArgs list
    | MAVT of MethodArgs list

type MethodCall =
    | MCTypedExpr of TyV * TypedExpr
    | MCTag of int64
    | MCVV of MethodCall list
    | MCET of MethodCall list
    | MCVT of MethodCall list

// No return type polymorphism like in Haskell for now. Local type inference only.
and TyMethodKey = int64 * MethodArgs // The key does not need to know the free variables.

and Expr = 
    | V of string // standard variable
    | If of Expr * Expr * Expr
    | HoistedIf of Expr * Expr * Expr
    | Inlineable of Expr * Expr * Env option
    | LitUnit
    | LitInt of int
    | LitFloat of float
    | LitBool of bool
    | Apply of Expr * args: Expr
    | Method of args: Expr * body: Expr * return_type: Ty option
    | HoistedMethod of int64 * Env * args: Expr * body: Expr * return_type: Ty option
    | VV of Expr list // immediately destructure
    | ET of Expr list // expression tuple

    // Value tuple cases
    | IndexVT of Expr * Expr
    | VT of Expr list // value tuple

    // Array cases
    | IndexArray of Expr * Expr list * Ty
    | CreateArray of Expr list * Ty

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
    // Magic
    | Typecase of (Data -> Expr -> ReturnCases) * Expr

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
    | TyMethod of TyMethodKey * MethodCall * Ty
    
    // Value tuple cases
    | TyIndexVT of TypedExpr * TypedExpr * Ty
    | TyVT of TypedExpr list * Ty // value tuple

    // Array cases
    | TyIndexArray of TypedExpr * TypedExpr list * Ty
    | TyCreateArray of TypedExpr list * Ty

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
    | TyMSet of TyV * TypedExpr * TypedExpr * Ty
    | TyAtomicAdd of TypedExpr * TypedExpr * Ty
    | TyWhile of TypedExpr * TypedExpr * TypedExpr * Ty

and ReturnCases =
    | RTypedExpr of TypedExpr
    | RExpr of Expr
    | RError of string

and Env = Map<string,ReturnCases>
// method key * method body * bound variables * used variables
and MethodDict = Dictionary<TyMethodKey, TyV list * TypedExpr * Set<TyV> * Set<TyV>>
// method key * method body * implicit arguments
and MethodImplDict = Dictionary<TyMethodKey, TyV list * TypedExpr * Set<TyV>>

and Sequence =
    | SeqLet of TyV * TypedExpr
    | SeqWhile of TypedExpr * TypedExpr
    | SeqMSet of TyV * TypedExpr

and ArgCases = Expr * Env
and Data =
    {
    // Immutable
    env : Env
    args : ArgCases list
    // Mutable
    memoized_methods : MethodDict // For hoisted out global methods.
    sequences : Stack<Sequence>
    used_variables : HashSet<TyV>
    }

type Result<'a,'b> = Succ of 'a | Fail of 'b

let rec get_type = function
    | TyV(_,_,t) | TyIf(_,_,_,t) | TyLet(_,_,_,t) -> t
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

    // Value tuple cases
    | TyVT(_,t) | TyIndexVT(_,_, t) -> t

    // Array cases
    | TyIndexArray(_,_,t) | TyCreateArray(_,t) -> t

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
    | TyMSet(_,_,_,t) -> t
    | TyAtomicAdd(_,_,t) -> t
    // Loops
    | TyWhile(_,_,_,t) -> t
    | TyMethod(_,_,t) -> t
    


let is_numeric a =
    match get_type a with
    | UInt32T | UInt64T | Int32T | Int64T 
    | Float32T | Float64T -> true
    | _ -> false

let is_atomic_add_supported a =
    match get_type a with
    | UInt32T | UInt64T | Int32T
    | Float32T | Float64T -> true
    | _ -> false

let is_float a =
    match get_type a with
    | Float32T | Float64T -> true
    | _ -> false

let is_bool a =
    match get_type a with
    | BoolT -> true
    | _ -> false

let is_int a =
    match get_type a with
    | UInt32T | UInt64T | Int32T | Int64T -> true
    | _ -> false

let is_vt a =
    match get_type a with
    | VTT _ -> true
    | _ -> false

let is_const a =
    let rec loop = function
        | ConstT _ -> true
        | SharedT x -> loop x
        | _ -> false
    loop (get_type a)

let get_tag =
    let mutable x = 0L
    fun () -> 
        let x' = x
        x <- x + 1L
        x'

let rec call_to_args = function
    | MCVV x -> MAVV (List.map call_to_args x)
    | MCTag x -> MATag x
    | MCTypedExpr(_,x) -> MATy (get_type x)
    | MCVT x -> MAVT (List.map call_to_args x)
    | MCET x -> MAET (List.map call_to_args x)

let sequences_to_typed_expr (sequences: Stack<Sequence>) final_expr =
    let type_fin = get_type final_expr
    Seq.fold (fun rest -> function 
        | SeqLet(v,body) -> TyLet(v,body,rest,type_fin)
        | SeqWhile(cond,body) -> TyWhile(cond,body,rest,type_fin)
        | SeqMSet(a,b) -> TyMSet(a,b,rest,type_fin)) final_expr sequences

let get_bound_variables (env: Env) =
    env
    |> Seq.choose (fun kv -> 
        match kv.Value with
        | RTypedExpr(TyV v) -> Some v
        | _ -> None)
    |> Set

let map_fold_2_Er f state (x,y) =
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
            | x -> failwith "Argument size mismatch in map_fold_2_Er."
        loop f state (x,y)
    else
        Fail <| sprintf "Argument size mismatch in map_fold_2_Er. Args: %A" (x,y)

let map_typed f x =
    let rec loop = function
        | x :: xs -> 
            match f x with
            | RTypedExpr x ->
                match loop xs with
                | Succ xs -> Succ (x :: xs)
                | Fail _ as er -> er
            | RExpr x -> Fail <| sprintf "Expected: typed expression.\nGot: %A" x
            | RError er -> Fail er
        | [] -> Succ []
    loop x
            

let mapResult f = function
    | Succ x -> Succ <| f x
    | Fail er -> Fail er

let mapFst f (a,b) = (f a, b)
let mapResultFst f = mapResult (mapFst f)

let rec fold_2_er f state (x,y) =
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
            | x -> failwith "Argument size mismatch in fold_2_er."
        loop f state (x,y)
    else
        Fail <| sprintf "Argument size mismatch in fold_2_er. Args: %A" (x,y)

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

    /// Patterm matching functions
    let add_bound_variable env arg_name ty_arg =
        let v = TyV ty_arg
        v, Map.add arg_name (RTypedExpr v) env

    let dup_name_check (name_checker: HashSet<string>) arg_name f =
        match name_checker.Add arg_name || arg_name = "" || arg_name = "_" with
        | true -> f()
        | false -> Fail <| sprintf "%s is a duplicate name in pattern matching." arg_name

    let bind_expr_fail acc (arg_name, exp) =
        Fail "Cannot bind untyped expressions in value structures like Vars."
    let bind_expr (name_checker: HashSet<string>) acc (arg_name, exp) =
        dup_name_check name_checker arg_name <| fun _ ->
            let exp = RExpr exp
            Succ (Map.add arg_name exp acc)
        
    let bind_typedexpr_fail acc (arg_name, ty_exp) =
        Fail "Cannot bind typed expressions in expression tuples."
    let bind_typedexpr'' acc (arg_name, ty_exp) =
        let b'_type = get_type ty_exp
        let ty_arg: TyV = get_tag(),arg_name,b'_type
        // Pushes the sequence onto the stack
        d.sequences.Push(SeqLet(ty_arg,ty_exp))
        // Binds the name to the said sequence's name and loops to the next argument
        add_bound_variable acc arg_name ty_arg
    let bind_typedexpr' a b = bind_typedexpr'' a b |> snd
    let bind_typedexpr (name_checker: HashSet<string>) acc (arg_name, ty_exp) =
        dup_name_check name_checker arg_name <| fun _ ->
            Succ (bind_typedexpr' acc (arg_name, ty_exp))
    let bind_template bind_expr bind_typedexpr eval_env acc (arg_name, right_arg) =
        match tev {d with env=eval_env; args=[]} right_arg with
        | RError er -> Fail er
        | RExpr exp -> bind_expr acc (arg_name, exp)
        | RTypedExpr ty_exp -> bind_typedexpr acc (arg_name, ty_exp)

    let bind_any name_checker = bind_template (bind_expr name_checker) (bind_typedexpr name_checker)
    let bind_expr_only name_checker = bind_template (bind_expr name_checker) bind_typedexpr_fail
    let bind_typedexpr_only name_checker = bind_template bind_expr_fail (bind_typedexpr name_checker)

    let match_v_template match_vv bind (acc: Env) = function
        | V arg_name, right_arg ->
            bind acc (arg_name, right_arg)
        | VV args, right_arg ->
            match_vv acc (args, right_arg)
        | x -> Fail <| sprintf "Unexpected arguments in match_v.\n%A" x

    let bind_method name_checker acc (arg_name, exp) =
        dup_name_check name_checker arg_name <| fun _ ->
            match exp with
            | HoistedMethod(tag,_,_,_,_) -> 
                let exp' = MCTag tag
                Succ (exp', Map.add arg_name (RExpr exp) acc)
            | x -> Fail <| sprintf "Expected: method.\nGot: %A" x
    
    let bind_typedexpr_method name_checker acc (arg_name, ty_exp: TypedExpr) =
        dup_name_check name_checker arg_name <| fun _ ->
            let v = get_tag(),arg_name,get_type ty_exp
            let ty_arg = TyV v
            let acc = Map.add arg_name (RTypedExpr ty_arg) acc
            Succ (MCTypedExpr(v,ty_exp), acc)

    let bind_method_only name_checker = bind_template (bind_method name_checker) bind_typedexpr_fail
    let bind_ty_only name_checker = bind_template bind_expr_fail (bind_typedexpr_method name_checker)
    let bind_any_method name_checker = bind_template (bind_method name_checker) (bind_typedexpr_method name_checker)

    let traverse_generic tuple_constructor f s l = map_fold_2_Er f s l |> mapResultFst tuple_constructor

    let rec match_vt_template traverse bind bind_tyvars (acc: Env) (l,r) = 
        let match_vv = match_vt_template traverse bind bind_tyvars
        match r with
        | VT r -> traverse (match_v_template match_vv bind) acc (l,r)
        | r -> bind_tyvars acc (l,r)

    let rec match_et_template traverse bind (acc: Env) (l,r) = 
        let match_vv = match_et_template traverse bind
        match r with
        | ET r -> traverse (match_v_template match_vv bind) acc (l,r)
        | x -> Fail <| sprintf "Unexpected arguments in match_et.\n%A" x

    let match_et name_checker eval_env = match_et_template fold_2_er (bind_expr_only name_checker eval_env)
    let match_et_method name_checker eval_env = match_et_template (traverse_generic MCET) (bind_method_only name_checker eval_env)

    let rec match_tyvt_template traverse bind (acc: Env) (l,r) = 
        let match_vv = match_tyvt_template traverse bind
        match get_type r with
        | VTT x -> 
            let r = List.mapi (fun i t -> TyIndexVT(r,TyLitInt i,t)) x
            traverse (match_v_template match_vv bind) acc (l,r)
        | x -> Fail <| sprintf "Unexpected arguments in match_tyvt.\n%A" x
    
    let match_tyvt name_checker = match_tyvt_template fold_2_er (bind_typedexpr name_checker)
    let match_tyvt_method name_checker = match_tyvt_template (traverse_generic MCVT) (bind_typedexpr_method name_checker)

    let bind_tyvt name_checker eval_env = bind_template bind_expr_fail (match_tyvt name_checker) eval_env
    let bind_tyvt_method name_checker eval_env = bind_template bind_expr_fail (match_tyvt_method name_checker) eval_env

    let match_vt name_checker eval_env = 
        match_vt_template fold_2_er (bind_typedexpr_only name_checker eval_env) 
            (bind_tyvt name_checker eval_env)

    let match_vt_method name_checker eval_env = 
        match_vt_template (traverse_generic MCVT) (bind_ty_only name_checker eval_env) 
            (bind_tyvt_method name_checker eval_env)

    let bind_mset_template eval_env acc (arg_name, r) =
        let d = {d with env=eval_env; args=[]}
        match tev d (V arg_name) with
        | RTypedExpr (TyV(_,_,lt as v)) when lt = get_type r ->
            d.sequences.Push(SeqMSet(v,r))
            Succ acc
        | x -> Fail <| sprintf "Expected: `RTypedExpr (TyV(_,_,lt as v)) when lt = get_type r`.\nGot: %A" x

    let match_tyvt_mset eval_env = match_tyvt_template fold_2_er (bind_mset_template eval_env)
    let bind_tyvt_mset eval_env = bind_template bind_expr_fail (match_tyvt_mset eval_env) eval_env
    let bind_mset eval_env = bind_template bind_expr_fail (bind_mset_template eval_env) eval_env
    let match_vt_mset eval_env = match_vt_template fold_2_er (bind_mset eval_env) (bind_tyvt_mset eval_env)

    let rec match_vv_template traverse bind_vv match_vars match_et bind_tyvars (acc: Env) (l,r) = 
        let match_vv = match_vv_template traverse bind_vv match_vars match_et bind_tyvars
        match r with
        | VV r -> traverse (match_v_template match_vv bind_vv) acc (l,r)
        | VT _ as r -> match_vars acc (l,r)
        | ET _ as r -> match_et acc (l,r)
        | r -> bind_tyvars acc (l,r)

    let match_vv name_checker eval_env = 
        match_vv_template fold_2_er (bind_any name_checker eval_env) (match_vt name_checker eval_env) 
            (match_et name_checker eval_env) (bind_tyvt name_checker eval_env)
    let match_vv_method name_checker eval_env = 
        match_vv_template (traverse_generic MCVV) (bind_any_method name_checker eval_env) (match_vt_method name_checker eval_env) 
            (match_et_method name_checker eval_env) (bind_tyvt_method name_checker eval_env)

    let match_v name_checker eval_env = match_v_template (match_vv name_checker eval_env) (bind_any name_checker eval_env)
    let match_v_method name_checker eval_env = match_v_template (match_vv_method name_checker eval_env) (bind_any_method name_checker eval_env)
    let match_v_mset eval_env = match_v_template (match_vt_mset eval_env) (bind_mset eval_env)

    // Primitive functions

    let append_typeof_fst k a b =
        k (a, b, (get_type a))

    let prim_bin_op_template check_error is_check k a b t =
        let constraint_both_eq_numberic f k =
            match tev d a, tev d b with
            | RTypedExpr a, RTypedExpr b ->
                if is_check a b then k a b |> RTypedExpr
                else f (check_error a b)
            | x -> f <| sprintf "Expected numeric expression.\nGot: %A" x

        constraint_both_eq_numberic RError (k t)

    let prim_mset_op =
        let er = sprintf "`get_type a = get_type b` is false.\na=%A, b=%A"
        let check a b = get_type a = get_type b
        prim_bin_op_template er check (fun t a b -> t (a,b))

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
        let constraint_numberic f k =
            match tev d a with
            | RTypedExpr a ->
                if is_check a then k a |> RTypedExpr
                else f (check_error a)
            | x -> f <| sprintf "Expected numeric expression.\nGot: %A" x

        constraint_numberic RError (k t)

    let prim_un_floating = 
        let er = sprintf "`is_float a` is false.\na=%A"
        let check a = is_float a
        prim_un_op_template er check (fun t a -> t (a, get_type a))

    let prim_un_numeric = 
        let er = sprintf "`true` is false.\na=%A"
        let check a = true
        prim_un_op_template er check (fun t a -> t (a, get_type a))

    let prim_atomic_add_op = 
        let er = sprintf "`is_numeric a && get_type a = get_type b` is false.\na=%A, b=%A"
        let check a b = is_numeric a && get_type a = get_type b
        prim_bin_op_template er check append_typeof_fst

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
        tev d (Apply(Method(V "cond",HoistedIf(V "cond",tr,fl),None),cond))
    | HoistedIf(cond,tr,fl) ->
        match tev {d with args=[]} cond with
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
            match match_v (HashSet(HashIdentity.Structural)) env'' env (args, cur_args) with
            | Fail er -> RError er
            | Succ env -> exp_and_seq {d with env=env;args=other_args} body
    | Method(args,body,return_type) ->
        exp_and_seq d (HoistedMethod(get_tag(),d.env,args,body,return_type))
    | HoistedMethod(tag,initial_env,args,body,return_type) as orig -> 
        match d.args with
        | [] -> RExpr orig
        | (cur_args,env'') :: other_args ->
            match match_v_method (HashSet(HashIdentity.Structural)) env'' initial_env (args, cur_args) with
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
                        let sole_arguments = 
                            let rec loop = function
                                | MCET _ | MCTag _ -> []
                                | MCTypedExpr(v,_) -> [v]
                                | MCVT x | MCVV x -> List.collect loop x
                            loop evaled_cur_args
                            
                        let bound_variables = get_bound_variables initial_env
                        d.memoized_methods.Add(method_key, (sole_arguments, body, bound_variables, Set d.used_variables))
                        make_method_call body
                | true, (sole_arguments, body, bound_variables, used_variables) ->
                    make_method_call body
    | VV _ as x -> 
        RError <| sprintf "Typechecking should never be called on VV. VV is only for immediate destructuring.\nGot: %A" x
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
    | VT vars as orig ->
        let empty_names = List.map (fun _ -> V "") vars
        match match_vt_method (HashSet(HashIdentity.Structural)) d.env d.env (empty_names, orig) with
        | Succ(MCVT(evaled_vars),env) ->
            let fields = List.map (function
                | (MCTypedExpr(_,x)) -> x
                | _ -> failwith "Impossible") evaled_vars
            let ty = List.map get_type fields |> VTT
            RTypedExpr <| TyVT(fields,ty)
        | Succ _ -> failwith "Impossible"
        | Fail er -> RError er
    | IndexVT(v,i) ->
        match tev d v, tev d i with
        | RTypedExpr v, RTypedExpr (TyLitInt i as i') ->
            match get_type v with
            | VTT ts -> 
                if i >= 0 || i < List.length ts then RTypedExpr (TyIndexVT(v,i',ts.[i]))
                else RError "(i >= 0 || i < List.length ts) = false in IndexVT"
            | x -> RError <| sprintf "Type of a evaluated expression in IndexVT is not VTT.\nGot: %A" x
        | RTypedExpr v, RTypedExpr i ->
            RError <| sprintf "Index into a tuple must be a natural number less than the size of the tuple.\nGot: %A" i
        | x -> RError <| sprintf "Expected: typed expression of type VTT and index into a tuple.\nGot: %A" x

    // Array cases
    | IndexArray(exp,args,t) ->
        match tev d exp, map_typed (tev d) args with
        | RTypedExpr exp, Succ args ->
            match get_type exp with
            | ArrayT(vs, t) when List.forall is_int args && List.length vs = List.length args ->
                List.iter (d.used_variables.Add >> ignore) vs
                RTypedExpr <| TyIndexArray(exp,args,t)
            | _ -> RError <| sprintf "Something is wrong in IndexArray.\nexp=%A, args=%A" exp args
        | x -> RError <| sprintf "Something is wrong in IndexArray.\n%A" x

    | CreateArray(args,t) ->
        match map_typed (tev d) args with
        | Succ args when List.forall is_int args ->
            let args = List.map (fun x -> bind_typedexpr'' d.env ("",x) |> fst) args
            let args' = List.map (function (TyV x) -> x | _ -> failwith "impossible") args
            bind_typedexpr'' d.env ("",TyCreateArray(args,ArrayT(args', t)))
            |> fst |> RTypedExpr
        | x -> RError <| sprintf "Something is wrong in CreateArray.\n%A" x

    | ThreadIdxX -> RTypedExpr TyThreadIdxX 
    | ThreadIdxY -> RTypedExpr TyThreadIdxY 
    | ThreadIdxZ -> RTypedExpr TyThreadIdxZ
    | BlockIdxX -> RTypedExpr TyBlockIdxX 
    | BlockIdxY -> RTypedExpr TyBlockIdxY 
    | BlockIdxZ -> RTypedExpr TyBlockIdxZ
    | BlockDimX -> RTypedExpr TyBlockDimX 
    | BlockDimY -> RTypedExpr TyBlockDimY 
    | BlockDimZ -> RTypedExpr TyBlockDimZ
    | GridDimX -> RTypedExpr TyGridDimX 
    | GridDimY -> RTypedExpr TyGridDimY 
    | GridDimZ -> RTypedExpr TyGridDimZ

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

    | Syncthreads -> RTypedExpr TySyncthreads

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
    | MSet(a,b,rest) -> 
        match match_v_mset d.env d.env (a,b) with
        | Fail er -> RError er
        | Succ env -> tev d rest
    | AtomicAdd(a,b) -> prim_atomic_add_op a b TyAtomicAdd
    | While(cond,body,e) ->
        match tev d cond, tev d body with
        | RTypedExpr cond, RTypedExpr body ->
            match get_type cond, get_type body with
            | BoolT, UnitT -> d.sequences.Push(SeqWhile(cond,body)); tev d e
            | BoolT, _ -> RError "Expected UnitT as the type of While's body."
            | _ -> RError "Expected BoolT as the type of While's conditional."
        | x -> RError <| sprintf "Expected both body and cond of While to be typed expressions.\nGot: %A" x
    // Magic
    | Typecase(f,x) -> f d x

let methodcall_to_method_type (tag,args) t =
    let rec get_arg_type = function
            | MAET _ | MATag _ -> []
            | MAVT x | MAVV x -> List.map get_arg_type x |> List.concat
            | MATy x -> [x]
    let r = get_arg_type args
    ArrT(r,t)
           
// Unions the free variables from top to bottom of the call chain.
let rec closure_conv (imemo: MethodImplDict) (memo: MethodDict) (exp: TypedExpr) =
    let c x = closure_conv imemo memo x
    let rec grab_implicit_args = function
        | MCVT x | MCVV x -> Set.unionMany (List.map grab_implicit_args x)
        | MCTag _ | MCET _ -> Set.empty
        | MCTypedExpr(_,x) -> c x
    match exp with
    | TyV(_,_,t) -> Set.empty
    | TyIf(cond,tr,fl,t) ->
        let cond, tr, fl = c cond, c tr, c fl
        Set.unionMany [|cond; tr; fl|]
    | TyLet(_,body,e,t) ->
        let body = c body
        let e = c e
        Set.union body e
    | TyLitInt _ | TyLitFloat _ | TyLitBool _ | TyUnit -> Set.empty
    | TyVT(vars,_) -> Set.unionMany (List.map c vars)
    | TyIndexVT(t,i,_) -> Set.union (c t) (c i)
    | TyMethod(m,ar,_) | TyMethodCall(m,ar,_) ->
        let method_implicit_args =
            match imemo.TryGetValue m with
            | true, (_,_,impl_args) -> impl_args
            | false, _ ->
                let sol_arg, body, bound_variables, used_variables = memo.[m]
                let impl_args = Set.union (c body) used_variables |> Set.intersect bound_variables // union the free vars from top to bottom
                imemo.Add(m,(sol_arg,body,impl_args))
                impl_args
        Set.union method_implicit_args (grab_implicit_args ar)
    // Array cases
    | TyIndexArray(a,b,_) -> Set.union (c a) (Set.unionMany (List.map c b))
    | TyCreateArray(b,_) -> Set.unionMany (List.map c b)
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
    | TyAtomicAdd(a,b,_) -> Set.union (c a) (c b)
    | TyWhile(a,b,c',_) -> Set.union (c a) (c b) |> Set.union (c c')
    | TyMSet(a,b,c',d) -> Set.unionMany [Set.singleton a; c b; c c']

let l v b e = Apply(Inlineable(v,e,None),b)

let data_empty() = {env=Map.empty;args=[];sequences=Stack();memoized_methods=Dictionary(HashIdentity.Structural);used_variables=HashSet(HashIdentity.Structural)}
let typecheck program inputs = 
    let d = data_empty()

    let rec rename i =
        List.mapFold (fun i -> function
            | ET _ | VV _ -> failwith "Only value tuples allowed as the inputs to the main function."
            | VT x -> 
                let r, i = rename i x
                VV r, i
            | x -> V <| sprintf "global_%i" i, i+1
            ) i
    let args = rename 0 [inputs] |> fst |> List.head
    let args' = args |> function VV x -> VT x | _ -> failwith "impossible"
    let program = 
        l (V "m1") (Method(V "global",program,None))
            (Apply(Method(args,Apply(V "m1",args'),None),inputs))
    match exp_and_seq d program with
    | RTypedExpr exp ->
        let imemo = Dictionary(HashIdentity.Structural)
        closure_conv imemo d.memoized_methods exp |> ignore
        Succ (exp, imemo)
    | RExpr exp ->
        Fail <| sprintf "Expected: typed expression.\nGot: expression %A" exp
    | RError er ->
        Fail er

let typecheck0 program = typecheck program (VT [])

let inl x y = Inlineable(x,y,None)
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
    
let term2 =
    let fst = inl (VV [V "a";V "b"]) (V "a")
    let snd = inl (VV [V "a";V "b"]) (V "b")
    l (VV [V "a";V "b"]) (VV [LitInt 2;LitFloat 3.3]) (ap (If(LitBool true,snd,snd)) (VV [V "a";V "b"]))

let t2 = typecheck0 term2

let term3 =
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

let meth x y = Method(x,y,None)

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
        (ap (V "m") (VV [LitInt 3;LitInt 2;LitUnit]))
let m2 = typecheck0 meth2

let meth3 = // vars test
    l (V "m") (meth (VV [V "a"; V "b"; V "c"]) (V "c"))
        (ap (V "m") (VT [LitInt 2; LitFloat 3.3; LitBool true]))

let m3 = typecheck0 meth3

let meth4 = // vars test 2
    l (V "m") (meth (V "vars") (l (VV [V "a"; V "b"; V "c"]) (V "vars") (V "c"))) 
        (ap (V "m") (VT [LitInt 2; LitFloat 3.3; LitBool true]))
let m4 = typecheck meth4 (VT [LitInt 3; LitBool true])


