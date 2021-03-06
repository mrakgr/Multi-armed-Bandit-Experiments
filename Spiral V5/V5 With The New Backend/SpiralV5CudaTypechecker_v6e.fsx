﻿#load "load-project-release.fsx"

open ManagedCuda.VectorTypes
open System.Collections.Generic

/// The dynamic device variable type.
type SpiralDeviceVarType =
| UInt8T
| UInt16T
| UInt32T
| UInt64T
| Int8T
| Int16T
| Int32T
| Int64T
| Float32T
| Float64T
| BoolT

type CudaTy =
    | UnitT
    | PrimT of SpiralDeviceVarType
    | VVT of CudaTy list
    | GlobalArrayT of TypedCudaExpr list * CudaTy
    | SharedArrayT of TypedCudaExpr list * CudaTy
    | LocalArrayT of TypedCudaExpr list * CudaTy
    | TagT of int64
and TyV = int64 * CudaTy

// No return type polymorphism like in Haskell for now. Local type inference only.
and TyMethodKey = int64 * CudaTy // The key does not need to know the free variables.

and CudaExpr = 
    | V of string // standard variable
    | V' of TyV // given variable
    | T of TypedCudaExpr
    | B // blank
    | If of CudaExpr * CudaExpr * CudaExpr
    | Inlineable of CudaExpr * CudaExpr
    | LitUInt32 of uint32
    | LitUInt64 of uint64
    | LitInt32 of int
    | LitInt64 of int64
    | LitFloat32 of float32
    | LitFloat64 of float
    | LitBool of bool
    | Apply of CudaExpr * args: CudaExpr
    | Method of name: string * args: CudaExpr * body: CudaExpr

    // Tuple cases
    | VVIndex of CudaExpr * CudaExpr
    | VV of CudaExpr list // tuple
    | VVMap of CudaExpr * CudaExpr

    // Array cases
    | ArrayIndex of CudaExpr * CudaExpr list
    | ArraySize of CudaExpr * CudaExpr
    | ArrayCreateShared of CudaExpr list * typeof: CudaExpr
    | ArrayCreateLocal of CudaExpr list * typeof: CudaExpr

    // Primitive operations on expressions.
    | Add of CudaExpr * CudaExpr
    | Sub of CudaExpr * CudaExpr
    | Mult of CudaExpr * CudaExpr
    | Div of CudaExpr * CudaExpr
    | Mod of CudaExpr * CudaExpr
    | LT of CudaExpr * CudaExpr
    | LTE of CudaExpr * CudaExpr
    | EQ of CudaExpr * CudaExpr
    | GT of CudaExpr * CudaExpr
    | GTE of CudaExpr * CudaExpr
    | LeftShift of CudaExpr * CudaExpr
    | RightShift of CudaExpr * CudaExpr
    | Syncthreads
    | ShuffleXor of CudaExpr * CudaExpr
    | ShuffleUp of CudaExpr * CudaExpr
    | ShuffleDown of CudaExpr * CudaExpr
    | ShuffleIndex of CudaExpr * CudaExpr
    | Log of CudaExpr
    | Exp of CudaExpr
    | Tanh of CudaExpr
    | Neg of CudaExpr
    // Cuda kernel constants
    | ThreadIdxX | ThreadIdxY | ThreadIdxZ
    | BlockIdxX | BlockIdxY | BlockIdxZ
    | BlockDimX | BlockDimY | BlockDimZ
    | GridDimX | GridDimY | GridDimZ
    // Mutable operations.
    | MSet of CudaExpr * CudaExpr * CudaExpr
    | AtomicAdd of out: CudaExpr * in_: CudaExpr
    // Loops
    | While of CudaExpr * CudaExpr * CudaExpr
    // Cub operations
    | CubBlockReduce of CudaExpr * CudaExpr * CudaExpr option

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
and TypedCudaExpr =
    // These two will not get code gen'd.
    // The difference from the past version of the typechecker is that now the TagT type exists.
    | Inlineable' of CudaExpr * CudaExpr * Env * CudaTy
    | Method' of name: string * args: CudaExpr * body: CudaExpr * Env * CudaTy
    
    | TyV of TyV
    | TyIf of TypedCudaExpr * TypedCudaExpr * TypedCudaExpr * CudaTy
    | TyLet of TyV * TypedCudaExpr * TypedCudaExpr * CudaTy
    | TyUnit
    | TyLitUInt32 of uint32
    | TyLitUInt64 of uint64
    | TyLitInt32 of int
    | TyLitInt64 of int64
    | TyLitFloat32 of float32
    | TyLitFloat64 of float
    | TyLitBool of bool
    | TyMethodCall of TyMethodKey * TypedCudaExpr * CudaTy
    
    // Tuple cases
    | TyIndexVV of TypedCudaExpr * TypedCudaExpr * CudaTy
    | TyVV of TypedCudaExpr list * CudaTy

    // Array cases
    | TyArrayIndex of TypedCudaExpr * TypedCudaExpr list * CudaTy
    | TyArrayCreate of CudaTy

    // Cuda kernel constants
    | TyThreadIdxX | TyThreadIdxY | TyThreadIdxZ
    | TyBlockIdxX | TyBlockIdxY | TyBlockIdxZ
   
    // Primitive operations on expressions.
    | TyAdd of TypedCudaExpr * TypedCudaExpr * CudaTy
    | TySub of TypedCudaExpr * TypedCudaExpr * CudaTy
    | TyMult of TypedCudaExpr * TypedCudaExpr * CudaTy
    | TyDiv of TypedCudaExpr * TypedCudaExpr * CudaTy
    | TyMod of TypedCudaExpr * TypedCudaExpr * CudaTy
    | TyLT of TypedCudaExpr * TypedCudaExpr
    | TyLTE of TypedCudaExpr * TypedCudaExpr
    | TyEQ of TypedCudaExpr * TypedCudaExpr
    | TyGT of TypedCudaExpr * TypedCudaExpr
    | TyGTE of TypedCudaExpr * TypedCudaExpr
    | TyLeftShift of TypedCudaExpr * TypedCudaExpr * CudaTy
    | TyRightShift of TypedCudaExpr * TypedCudaExpr * CudaTy
    | TySyncthreads
    | TyShuffleXor of TypedCudaExpr * TypedCudaExpr * CudaTy
    | TyShuffleUp of TypedCudaExpr * TypedCudaExpr * CudaTy
    | TyShuffleDown of TypedCudaExpr * TypedCudaExpr * CudaTy
    | TyShuffleIndex of TypedCudaExpr * TypedCudaExpr * CudaTy
    | TyLog of TypedCudaExpr * CudaTy
    | TyExp of TypedCudaExpr * CudaTy
    | TyTanh of TypedCudaExpr * CudaTy
    | TyNeg of TypedCudaExpr * CudaTy
    // Mutable operations.
    | TyMSet of TypedCudaExpr * TypedCudaExpr * TypedCudaExpr * CudaTy
    | TyAtomicAdd of TypedCudaExpr * TypedCudaExpr * CudaTy
    | TyWhile of TypedCudaExpr * TypedCudaExpr * TypedCudaExpr * CudaTy
    // Cub operations
    | TyCubBlockReduce of dim: TypedCudaExpr * input: TypedCudaExpr * method_: TypedCudaExpr * num_valid: TypedCudaExpr option * CudaTy

and Env = Map<string, TypedCudaExpr>
// method key * method body * bound variables
and MethodCases =
    | MethodInEvaluation of CudaTy option
    | MethodDone of TyV list * TypedCudaExpr
and MethodDict = Dictionary<TyMethodKey, MethodCases>
and TaggedDict = Dictionary<int64,TypedCudaExpr>
// method key * method body * implicit arguments
and MethodImplDict = Dictionary<TyMethodKey, TyV list * TypedCudaExpr * Set<TyV>>
and CudaTypecheckerEnv =
    {
    // Immutable
    env : Env
    // Mutable
    tagged_vars : TaggedDict // For looking up the the unapplied Inlineables and Methods
    memoized_methods : MethodDict // For hoisted out global methods.
    sequences : (TypedCudaExpr -> TypedCudaExpr) option ref // For sequencing statements.
    recursive_methods_stack : Stack<unit -> TypedCudaExpr> // For typechecking recursive calls.
    blockDim : dim3 // since Cub needs hard constants, I need to have them during typechecking.
    gridDim : dim3
    }

type Result<'a,'b> = Succ of 'a | Fail of 'b

let rec get_type = function
    | Inlineable'(_,_,_,t) | Method'(_,_,_,_,t) -> t

    | TyV(_,t) | TyIf(_,_,_,t) -> t
    | TyLitUInt32 _ -> PrimT UInt32T
    | TyLitUInt64 _ -> PrimT UInt64T
    | TyLitInt32 _ -> PrimT Int32T
    | TyLitInt64 _ -> PrimT Int64T
    | TyLitFloat32 _ -> PrimT Float64T
    | TyLitFloat64 _ -> PrimT Float32T   
    | TyLitBool _ -> PrimT BoolT
    | TyMethodCall(_,_,t) -> t

    // Cuda kernel constants
    | TyThreadIdxX | TyThreadIdxY | TyThreadIdxZ
    | TyBlockIdxX | TyBlockIdxY | TyBlockIdxZ -> PrimT UInt64T

    // Tuple cases
    | TyVV(_,t) | TyIndexVV(_,_,t) -> t

    // Array cases
    | TyArrayIndex(_,_,t) | TyArrayCreate(t) -> t

    // Primitive operations on expressions.
    | TyAdd(_,_,t) | TySub(_,_,t) | TyMult(_,_,t)
    | TyDiv(_,_,t) | TyMod(_,_,t) -> t
    | TyLT _ | TyLTE _ | TyEQ _ | TyGT _
    | TyGTE _ -> PrimT BoolT
    | TyLeftShift(_,_,t) | TyRightShift(_,_,t) -> t
    | TySyncthreads -> UnitT
    | TyShuffleXor(_,_,t) | TyShuffleUp(_,_,t)
    | TyShuffleDown(_,_,t) | TyShuffleIndex(_,_,t) -> t
    | TyLog(_,t) | TyExp(_,t) | TyTanh(_,t)
    | TyNeg(_,t) -> t
    // Mutable operations.
    | TyAtomicAdd(_,_,t) -> t
    
    // Statements
    | TyUnit -> UnitT
    | TyMSet(_,_,_,t) | TyWhile(_,_,_,t) | TyLet((_,_),_,_,t)  -> t
    // Cub operations
    | TyCubBlockReduce(_,_,_,_,t) -> t

let rec is_simple' = function
    | PrimT x -> 
        match x with
        | UInt8T | UInt16T | UInt32T | UInt64T 
        | Int8T | Int16T | Int32T | Int64T 
        | Float32T | Float64T | BoolT -> true
    | UnitT | GlobalArrayT _ -> true
    | VVT x -> List.forall is_simple' x
    | _ -> false
let is_simple a = is_simple' (get_type a)

let rec is_numeric' = function
    | PrimT x -> 
        match x with
        | UInt8T | UInt16T | UInt32T | UInt64T 
        | Int8T | Int16T | Int32T | Int64T 
        | Float32T | Float64T -> true
        | BoolT -> false
    | _ -> false
let is_numeric a = is_numeric' (get_type a)

let is_atomic_add_supported' = function
    | PrimT x -> 
        match x with
        | UInt32T | UInt64T | Int32T
        | Float32T | Float64T -> true
        | _ -> false
    | _ -> false
let is_atomic_add_supported a = is_atomic_add_supported' (get_type a)

let rec is_float' = function
    | PrimT x -> 
        match x with
        | Float32T | Float64T -> true
        | _ -> false
    | _ -> false
let is_float a = is_float' (get_type a)

let rec is_bool' = function
    | PrimT x -> 
        match x with
        | BoolT -> true
        | _ -> false
    | _ -> false
let is_bool a = is_bool' (get_type a)

let rec is_int' = function
    | PrimT x -> 
        match x with
        | UInt32T | UInt64T | Int32T | Int64T -> true
        | _ -> false
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

let get_body_from (stack: Stack<unit -> TypedCudaExpr>) = 
    if stack.Count > 0 then stack.Pop()()
    else failwith "The program is divergent."

let filter_tyvs evaled_cur_args =
    let rec loop = function
        | TyV(_,t as v) when is_simple' t -> [v]
        | TyVV(x,_) -> List.collect loop x
        | _ -> []
    loop evaled_cur_args

let filter_simple_vars evaled_cur_args =
    let rec loop = function
        | TyVV(x,_) -> List.collect loop x
        | x when is_simple x -> [x]
        | _ -> []
    loop evaled_cur_args

let h0() = HashSet(HashIdentity.Structural)
let d0() = Dictionary(HashIdentity.Structural)

let apply_sequences sequences x =
    match sequences with
    | Some sequences -> sequences x
    | None -> x

let rec with_empty_seq (d: CudaTypecheckerEnv) expr =
    let d = {d with sequences = ref None}
    let expr = exp_and_seq d expr
    apply_sequences !d.sequences expr


// Does macro expansion and takes note of the bound and 
// used variables in the method dictionary for the following passes.
and exp_and_seq (d: CudaTypecheckerEnv) exp: TypedCudaExpr =
    let tev d exp = exp_and_seq d exp

    let dup_name_check (name_checker: HashSet<string>) arg_name f =
        match name_checker.Add arg_name || arg_name = "" || arg_name = "_" with
        | true -> f()
        | false -> failwithf "%s is a duplicate name in pattern matching." arg_name

    let inline push_sequence x = 
        let f current_sequence rest = apply_sequences current_sequence (x rest)
        d.sequences := Some (f !d.sequences)

    let make_tyv ty_exp = get_tag(), get_type ty_exp

    let make_tyv_and_push' ty_exp =
        let v = make_tyv ty_exp
        push_sequence (fun rest -> TyLet(v,ty_exp,rest,get_type rest))
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
    // The deep version can also be straightforwardly derived from a template of this using the Y combinator.
    let rec destructure_deep_template nonseq_push r = 
        let destructure_deep r = destructure_deep_template nonseq_push r
        match r with
        | TyV _ | TyUnit | Inlineable' _ | Method' _ -> r // Won't be passed into method as arguments apart from TyV.
        | TyVV(l,t) -> List.map destructure_deep l |> fun x -> TyVV(x,t)
        | TyLitUInt32 _ | TyLitUInt64 _ | TyLitInt32 _ | TyLitInt64 _ 
        | TyLitFloat32 _ | TyLitFloat64 _ | TyLitBool _ 
        | TyThreadIdxX | TyThreadIdxY | TyThreadIdxZ
        | TyBlockIdxX | TyBlockIdxY | TyBlockIdxZ -> nonseq_push r // Will not be evaluated except at method calls.
        | _ -> 
            match get_type r with
            | VVT tuple_types -> 
                let indexed_tuple_args = List.mapi (fun i typ -> 
                    destructure_deep <| TyIndexVV(r,TyLitInt32 i,typ)) tuple_types
                TyVV(indexed_tuple_args, VVT tuple_types)
            | _ -> 
                match r with
                | TyIndexVV _ -> nonseq_push r
                | _ -> make_tyv_and_push r

    let destructure_deep r = destructure_deep_template id r
    let destructure_deep_method r = destructure_deep_template (make_tyv >> TyV) r

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

    let match_vv_inl x = match_vv_inl' (h0()) x
    let match_vv_method x = match_vv_method' (h0()) x

    let rec mset l r =
        match l, r with // destructure_deep is called in the MSet case.
        | (TyArrayIndex(_,_,lt) as v), r when lt = get_type r -> push_sequence (fun rest -> TyMSet(v,r,rest,get_type rest))
        | TyVV(l,_), TyVV(r,_) -> List.iter2 mset l r
        | _ -> failwithf "Error in mset. Expected: (TyArrayIndex(_,_,lt) as v), r when lt = get_type r or TyVV(l,_), TyVV(r,_).\nGot: %A and %A" l r

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

    let filter_duplicate_vars x =
        let h = h0()
        let rec loop = function
            | TyV(tag,t) as var -> if h.Add tag then Some var else None
            | TyVV(l,t) -> TyVV(List.choose loop l, t) |> Some
            | x -> Some x
        (loop x).Value

    let apply_method (name,la,body,initial_env,t as orig) ra =
        let t = match t with TagT t -> t | _ -> failwith "impossible"
        
        let bound_outside_args, bound_inside_args, env = 
            let bound_outside_args = destructure_deep ra |> filter_duplicate_vars
            let bound_inside_args, env = match_vv_method initial_env la (destructure_deep_method bound_outside_args)
            bound_outside_args, bound_inside_args, env

        let method_key = t, get_type bound_inside_args

        let make_method_call body_type = TyMethodCall(method_key, bound_outside_args, body_type)

        match d.memoized_methods.TryGetValue method_key with
        | false, _ ->
            d.memoized_methods.[method_key] <- MethodInEvaluation(None)

            let d = {d with env = if name <> "" then Map.add name (Method' orig) env else env}
            let body = with_empty_seq d body
            let sole_arguments = filter_tyvs bound_inside_args
                        
            d.memoized_methods.[method_key] <- MethodDone(sole_arguments, body)

            if is_simple body then make_method_call (get_type body)
            else failwithf "Expected a simple type as the function return.\nGot: %A" (get_type body)
        | true, MethodInEvaluation None ->
            let body = get_body_from d.recursive_methods_stack
            let t = get_type body
            d.memoized_methods.[method_key] <- MethodInEvaluation (Some t)
            make_method_call t
        | true, MethodInEvaluation (Some t) ->
            make_method_call t
        | true, MethodDone(_,body) ->
            make_method_call (get_type body)

    let apply_second apply_inl apply_method la ra =
        match la with
        | Inlineable'(a,b,c,d) -> apply_inl (a,b,c,d) ra
        | Method'(a,b,c,d,e) -> apply_method (a,b,c,d,e) ra
        | _ -> failwith "impossible"

    let apply_both = apply_second apply_inlineable apply_method
    let apply_method_only = apply_second (fun _ -> failwith "Inlineable not supported.") apply_method

    let apply expr args = apply_both (apply_first expr) (tev d args)

    match exp with
    | T x -> x // To assist in CubBlockReduce so evaled cases do not have to be evaluated twice.
    | V' x -> TyV x // To assist in interfacing with the outside.
    | LitInt32 x -> TyLitInt32 x
    | LitInt64 x -> TyLitInt64 x
    | LitUInt32 x -> TyLitUInt32 x
    | LitUInt64 x -> TyLitUInt64 x
    | LitFloat32 x -> TyLitFloat32 x
    | LitFloat64 x -> TyLitFloat64 x
    | LitBool x -> TyLitBool x
    | B -> TyUnit
    | V x -> 
        match Map.tryFind x d.env with
        | Some v -> v
        | None -> failwithf "Variable %A not bound." x
    | Inlineable(args, body) -> add_tagged (fun t -> Inlineable'(args, body, d.env, TagT t))
    | Method(name, args, body) -> add_tagged (fun t -> Method'(name, args, body, d.env, TagT t))
    | Apply(expr,args) -> apply expr args
    | If(cond,tr,fl) ->
        let cond = tev d cond
        if is_bool cond = false then failwithf "Expected a bool in conditional.\nGot: %A" (get_type cond)
        else
            let tev' e f =
                let mutable is_popped = false
                d.recursive_methods_stack.Push (fun _ -> is_popped <- true; f())
                let x = with_empty_seq d e
                if is_popped = false then d.recursive_methods_stack.Pop() |> ignore
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
    | VVMap(a,b) ->
        let a,b = tev d a, tev d b
        tev d (map_tyvv (T a) (destructure_deep b))
    | VV vars ->
        let vv = List.map (tev d) vars
        TyVV(vv,make_vvt vv)
    | VVIndex(v,i) ->
        match tev d v, tev d i with
        | v, (TyLitInt32 i as i') ->
            match get_type v with
            | VVT ts -> 
                if i >= 0 || i < List.length ts then TyIndexVV(v,i',ts.[i])
                else failwith "(i >= 0 || i < List.length ts) = false in IndexVT"
            | x -> failwithf "Type of a evaluated expression in IndexVT is not VTT.\nGot: %A" x
        | v, i ->
            failwithf "Index into a tuple must be a natural number less than the size of the tuple.\nGot: %A" i

    // Array cases
    | ArrayIndex(exp,args) ->
        let exp,args = tev d exp, List.map (tev d) args
        match get_type exp with
        | LocalArrayT(vs, t) | SharedArrayT(vs, t) | GlobalArrayT(vs, t) when List.forall is_int args && List.length vs = List.length args ->
            TyArrayIndex(exp,args,t)
        | _ -> failwithf "Something is wrong in ArrayIndex.\nexp=%A, args=%A" exp args
    | ArraySize(ar,ind) ->
        let ar, ind = tev d ar, tev d ind
        match get_type ar, ind with
        | (LocalArrayT(vs, t) | SharedArrayT(vs, t) | GlobalArrayT(vs, t)), TyLitInt32 i ->
            if i < vs.Length then vs.[i]
            else failwith "Array size not available."
        | _ -> failwithf "Something is wrong in ArraySize.\nar=%A, ind=%A" ar ind

    | ArrayCreateLocal(args,t) -> let args,t = process_create_array args t in TyArrayCreate(LocalArrayT(args,t))
    | ArrayCreateShared(args,t) -> let args,t = process_create_array args t in TyArrayCreate(SharedArrayT(args,t))

    | ThreadIdxX -> TyThreadIdxX 
    | ThreadIdxY -> TyThreadIdxY 
    | ThreadIdxZ -> TyThreadIdxZ
    | BlockIdxX -> TyBlockIdxX 
    | BlockIdxY -> TyBlockIdxY 
    | BlockIdxZ -> TyBlockIdxZ
    | BlockDimX -> TyLitUInt64 (uint64 d.blockDim.x) 
    | BlockDimY -> TyLitUInt64 (uint64 d.blockDim.y) 
    | BlockDimZ -> TyLitUInt64 (uint64 d.blockDim.z)
    | GridDimX -> TyLitUInt64 (uint64 d.gridDim.x)
    | GridDimY -> TyLitUInt64 (uint64 d.gridDim.y) 
    | GridDimZ -> TyLitUInt64 (uint64 d.gridDim.z)

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
    | MSet(a,b,rest) -> mset (tev d a) (destructure_deep (tev d b)); tev d rest
    | AtomicAdd(a,b) -> prim_atomic_add_op a b TyAtomicAdd
    | While(cond,body,e) ->
        let cond, body = tev d (Apply(Method("",VV [],cond),VV [])), with_empty_seq d body
        match get_type cond, get_type body with
        | PrimT BoolT, UnitT -> push_sequence (fun rest -> TyWhile(cond,body,rest,get_type rest)); tev d e
        | PrimT BoolT, _ -> failwith "Expected UnitT as the type of While's body."
        | _ -> failwith "Expected BoolT as the type of While's conditional."
    | CubBlockReduce(input, method_, num_valid) ->
        let dim = 
            if int d.blockDim.y <> 1 || int d.blockDim.z <> 1 then // TODO: Remove this restriction.
                failwith "int d.blockDim.y <> 1 || int d.blockDim.z <> 1.\nTODO: Remove this restriction."
            tev d BlockDimX
        let evaled_input = tev d input
        let method_ =
            match get_type evaled_input with
            | LocalArrayT(_,t) | SharedArrayT(_,t) -> 
                let arg = ArrayIndex(T evaled_input,[LitInt32 0])
                tev d (VV [arg;arg])
            | x -> 
                let arg() = evaled_input |> make_tyv |> TyV
                tev d (VV [T (arg()); T (arg())])
            |> apply_method_only (apply_first method_)

        let num_valid = Option.map (tev d) num_valid
        TyCubBlockReduce(dim, evaled_input,method_,num_valid,get_type method_)
            
/// Propagates the free variables downwards.
/// The closures are stack allocated hence not allowed to return from functions.
let rec closure_conv (imemo: MethodImplDict) (memo: MethodDict) (exp: TypedCudaExpr) =
    let c x = closure_conv imemo memo x
    match exp with
    | Method' _ | Inlineable' _ -> Set.empty
    | TyV v -> Set.singleton v
    | TyIf(cond,tr,fl,t) -> c cond + c tr + c fl
    | TyLitUInt32 _ | TyLitUInt64 _ | TyLitInt32 _ | TyLitInt64 _ 
    | TyLitFloat32 _ | TyLitFloat64 _ | TyLitBool _ | TyUnit -> Set.empty
    | TyVV(vars,t) -> Set.unionMany (List.map c vars)
    | TyIndexVV(t,i,_) -> Set.union (c t) (c i)
    | TyMethodCall(m,ar,_) ->
        let method_implicit_args =
            match imemo.TryGetValue m with
            | true, (_,_,impl_args) -> impl_args
            | false, _ ->
                match memo.[m] with
                | MethodDone(sol_arg, body) -> 
                    let impl_args = c body - Set(sol_arg)
                    imemo.Add(m,(sol_arg,body,impl_args))
                    impl_args
                | _ -> failwith "impossible"
        Set.union method_implicit_args (c ar)
    | TyCubBlockReduce(_,inp,(TyMethodCall(key,_,_) as m),num_valid,t) ->
        ignore <| c m // This is so it gets added to the env.

        match imemo.[key] with
        | _,_,impl_args when impl_args.IsEmpty ->
            match num_valid with
            | Some num_valid -> Set.union (c num_valid) (c inp)
            | None -> c inp
        | impl_args -> 
            failwithf "The method passed to Cub should have no implicit arguments.\nm=%A\nimpl_args=%A" m impl_args
    | TyCubBlockReduce _ -> failwith "impossible"
    // Array cases
    | TyArrayIndex(a,b,t) -> 
        let a = 
            match get_type a with
            | LocalArrayT(x,_) | SharedArrayT(x,_) | GlobalArrayT(x,_) ->
                let b = 
                    match x with
                    | [] -> []
                    | x -> List.choose (function TyV v -> Some v | _ -> None) (List.tail x)
                Set.union (c a) (Set(b))
            | _ -> failwith "impossible"
        Set.union a (Set.unionMany (List.map c b))
    | TyArrayCreate t -> 
        match t with
        | LocalArrayT(x,_) | SharedArrayT(x,_) | GlobalArrayT(x,_) ->
            Set.unionMany (List.map c x)
        | _ -> failwith "impossible"
    // Cuda kernel constants
    | TyThreadIdxX | TyThreadIdxY | TyThreadIdxZ
    | TyBlockIdxX | TyBlockIdxY | TyBlockIdxZ -> Set.empty
    // Primitive operations on expressions.
    | TySyncthreads -> Set.empty
    | TyLog(a,_) | TyExp(a,_) | TyTanh(a,_) | TyNeg(a,_) -> c a
    | TyAdd(a,b,_) | TySub(a,b,_) | TyMult(a,b,_) | TyDiv(a,b,_) | TyMod(a,b,_)
    | TyLT(a,b) | TyLTE(a,b) | TyEQ(a,b) | TyGT(a,b) | TyGTE(a,b) 
    | TyLeftShift(a,b,_) | TyRightShift(a,b,_) | TyShuffleXor(a,b,_)
    | TyShuffleUp(a,b,_) | TyShuffleDown(a,b,_) | TyShuffleIndex(a,b,_) 
    | TyAtomicAdd(a,b,_) -> Set.union (c a) (c b)
    | TyMSet(a, b, rest, _) | TyWhile(a, b, rest, _) -> c a + c b + c rest
    | TyLet(v, body, rest, _) -> c body + c rest |> Set.remove v

let l v b e = Apply(Inlineable(v,e),b)

let data_empty (blockDim, gridDim) = 
    {memoized_methods=d0();tagged_vars=d0();blockDim=blockDim;gridDim=gridDim
     env=Map.empty;sequences=ref None;recursive_methods_stack=Stack()}

let typecheck dims body inputs = 
    try
        let main_method, memo = 
            let d = data_empty dims
            exp_and_seq d (Apply(body,inputs)), d.memoized_methods
        let imemo = Dictionary(HashIdentity.Structural)
        closure_conv imemo memo main_method |> ignore
        Succ imemo
    with e -> Fail (e.Message, e.StackTrace)

/// Reasonable default for the dims.
let default_dims = dim3(256), dim3(20)

let typecheck0 program = typecheck default_dims program (VV [])

let inl x y = Inlineable(x,y)
let ap x y = Apply(x,y)
let meth x y = Method("",x,y)

//let term0 =
//    let snd = inl (VV [V "a";V "b"]) (V "b")
//    meth (VV [])
//        (ap (inl (VV [V "x";V "y";V "z";V "r"]) (ap (V "r") (VV [V "y";V "z"]))) (VV [B;LitBool true;LitInt 5;snd]))
//
//let t0 = typecheck0 term0
//
//let term1 =
//    let fst = inl (VV [V "a";V "b"]) (V "a")
//    let snd = inl (VV [V "a";V "b"]) (V "b")
//    meth (VV [])
//        (l (VV [V "x";V "y";V "z"]) (VV [B;LitBool true;LitInt 5])
//            (l (V "q") (fst) (ap (V "q") (VV [V "y";V "z"]))))
//
//let t1 = typecheck0 term1
//    
//// This particular test evolved during the course of the development.
//// Previously it would inline the terms completely, now I've simplified it
//// and it just gives an error.
//let term2 = 
//    let fst = inl (VV [V "a";V "b"]) (V "a")
//    let snd = inl (VV [V "a";V "b"]) (V "b")
//    meth (VV [])
//        (l (VV [V "a";V "b"]) (VV [LitInt 2;LitFloat 3.3]) (ap (If(LitBool true,snd,snd)) (VV [V "a";V "b"])))
//
//let t2 = typecheck0 term2
//
//let term3 = // Error in the first line.
//    meth (VV [])
//        (l (V "inlineable") (VV [inl (VV [V "a";V "b"]) (V "b")])
//            (l (V "fun")
//                (inl (VV [V "inl";V "a";V "b";V "c";V "d"]) (ap (V "inl") (VV [V "b";V "c"])))
//                (ap (V "fun") (VV [V "inlineable"; LitBool true; LitInt 2; LitFloat 1.5; LitInt 2]))))
//let t3 = typecheck0 term3
//
//let term3' =
//    meth (VV [])
//        (l (V "inlineable") (inl (VV [V "a";V "b"]) (V "b"))
//            (l (V "fun")
//                (inl (VV [V "inl";V "a";V "b";V "c";V "d"]) (ap (V "inl") (VV [V "b";V "c"])))
//                (ap (V "fun") (VV [V "inlineable"; LitBool true; LitInt 2; LitFloat 1.5; LitInt 2]))))
//let t3' = typecheck0 term3'
//
//let term4 = // If test
//    meth (VV [])
//        (l (V "if") (inl (VV [V "cond";V "tr";V "fl"]) (If(V "cond",V "tr",V "fl")))
//            (l (V "cond") (LitBool true)
//                (l (V "tr") (LitFloat 3.33)
//                    (l (V "fl") (LitFloat 4.44)
//                        (ap (V "if") (VV [V "cond";V "tr";V "fl"]))))))
//
//let t4 = typecheck0 term4
//
//let meth1 = 
//    meth (VV [])
//        (l (VV [V "fun";V "id"])
//            (VV [meth (VV [V "x";V "y";V "z";V "f"])
//                    (l (V "t") (LitInt 3)
//                        (l (V "u") (LitBool false) (ap (V "f") (V "z"))))
//                 meth (V "x") (V "x")])
//            (ap (V "fun") (VV [LitBool true; LitInt 2; LitFloat 4.4;V "id"])))
//let m1 = typecheck0 meth1
//
//let meth2 = // closure conversion test
//    meth (VV [])
//        (l (V "m")
//            (meth (VV [V "a";V "n";V "qwe"]) 
//                (l (V "loop") 
//                    (meth (VV [V "acc";V "q"]) 
//                        (l (V "loop_method") (meth (VV [V "a";V "n"]) (If(LitBool true,V "a",V "n")))
//                            (ap (V "loop_method") (VV [V "a"; V "n"]))))
//                    (ap (V "loop") (VV [LitInt 1; V "n"]))))
//            (ap (V "m") (VV [LitInt 3;LitInt 2;LitFloat 3.5])))
//let m2 = typecheck0 meth2
//
//let meth3 = // vv test
//    meth (VV [])
//        (l (V "m") (meth (VV [V "a"; V "b"; V "c"]) (V "c"))
//            (ap (V "m") (VV [LitInt 2; LitFloat 3.3; LitBool true])))
//
//let m3 = typecheck0 meth3
//
//let meth4 = // vv test 2
//    meth (VV [])
//        (l (V "m") (meth (V "vars") (l (VV [V "a"; V "b"; V "c"]) (V "vars") (V "c"))) 
//            (ap (V "m") (VV [LitInt 2; LitFloat 3.3; LitBool true])))
//let m4 = typecheck0 meth4

//let term5 = // For debugging the sequencing infinite loop bug.
//    meth (VV [])
//        (l (V "x") (LitInt 2) (V "x"))
//
//let t5 = typecheck0 term5