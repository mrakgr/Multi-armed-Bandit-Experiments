#load "load-project-release.fsx"

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
    | InlineableT of TyInlineableKey
    | MethodT of TyMethodKey

and TyV = int64 * CudaTy
and Env = Map<string, TypedCudaExpr>
and TyEnv = Map<string,CudaTy>
and InlineableAlias = string * (CudaPattern * CudaExpr) list
and MethodAlias = string * (CudaPattern * CudaExpr) list 
and TyMethodKey = MethodAlias * TyEnv
and TyInlineableKey = InlineableAlias * TyEnv

and CudaPattern =
    | S of string
    | S' of string // match if not tuple
    | R of CudaPattern list * CudaPattern option // Tuple
    | F of string * CudaExpr // Functiona application with retracing.

and CudaExpr = 
    | V of string // standard variable
    | V' of TyV // given variable
    | T of TypedCudaExpr
    | B // blank
    | If of CudaExpr * CudaExpr * CudaExpr
    | LitUInt32 of uint32
    | LitUInt64 of uint64
    | LitInt32 of int
    | LitInt64 of int64
    | LitFloat32 of float32
    | LitFloat64 of float
    | LitBool of bool
    | Apply of CudaExpr * args: CudaExpr
    | Inlineable of InlineableAlias
    | Method of MethodAlias

    // Tuple cases
    | VVIndex of CudaExpr * CudaExpr
    | VV of CudaExpr list // tuple
    | VVCons of CudaExpr * CudaExpr
    | VVZip of CudaExpr

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
    | MSet of CudaExpr * CudaExpr
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
//    | Inlineable' of CudaExpr * CudaExpr * Env * CudaTy
//    | Method' of name: string * args: CudaExpr * body: CudaExpr * Env * CudaTy
    | TyType of CudaTy
    
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

// method key * method body * bound variables
and MethodCases =
    | MethodInEvaluation of CudaTy option
    | MethodDone of TyV list * TypedCudaExpr * int64
and MethodDict = Dictionary<TyMethodKey, MethodCases>
and MethodDictEnvs = Dictionary<TyMethodKey, Env>
and InlineableDictEnvs = Dictionary<TyInlineableKey, Env>
// method key * method body * implicit arguments
and MethodImplDict = Dictionary<TyMethodKey, TyV list * TypedCudaExpr * int64 * Set<TyV>>
and CudaTypecheckerEnv =
    {
    // Immutable
    env : Env
    // Mutable
    memoized_methods : MethodDict // For typechecking methods.
    memoized_method_envs : MethodDictEnvs // For storing their environments.
    memoized_inlineable_envs : InlineableDictEnvs // Ditto for Inlineables.
    sequences : (TypedCudaExpr -> TypedCudaExpr) option ref // For sequencing statements.
    recursive_methods_stack : Stack<unit -> TypedCudaExpr> // For typechecking recursive calls.
    blockDim : dim3 // since Cub needs hard constants, I need to have them during typechecking.
    gridDim : dim3
    }

type Result<'a,'b> = Succ of 'a | Fail of 'b

let rec get_type = function
    | TyType t
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

let rec is_returnable' = function
    | PrimT x -> 
        match x with
        | UInt8T | UInt16T | UInt32T | UInt64T 
        | Int8T | Int16T | Int32T | Int64T 
        | Float32T | Float64T | BoolT -> true
    | UnitT | GlobalArrayT _ -> true
    | VVT x -> List.forall is_returnable' x
    | _ -> false
let is_returnable a = is_returnable' (get_type a)

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

let get_body_from (stack: Stack<unit -> TypedCudaExpr>) = 
    if stack.Count > 0 then stack.Pop()()
    else failwith "The program is divergent."

let filter_tyvs evaled_cur_args =
    let rec loop = function
        | TyV v -> [v]
        | TyVV(x,_) -> List.collect loop x
        | _ -> []
    loop evaled_cur_args

let is_arg = function TyUnit | TyType _ -> false | _ -> true

let filter_vars check evaled_cur_args =
    let rec loop = function
        | TyVV(x,_) -> List.collect loop x
        | x when check x -> [x]
        | _ -> []
    loop evaled_cur_args

let h0() = HashSet(HashIdentity.Structural)
let d0() = Dictionary(HashIdentity.Structural)

let apply_sequences sequences x =
    match sequences with
    | Some sequences -> sequences x
    | None -> x

let inl x y = Inlineable("",[x,y])
let inlr name x y = Inlineable(name,[x,y])
let ap x y = Apply(x,y)
let meth x y = Method("",[x,y])

let E = S ""
/// Matches tuples without a tail.
let SS x = R (x, None) 
/// Opposite of S', matches only a tuple.
let SS' x = R ([], Some (S x)) 
/// Matches tuples with a tail.
let SSS a b = R(a, Some (S b)) 

let cons a b = VVCons(a,b)

let l v b e = Apply(inl v e,b)

let dref x = ArrayIndex(x,[])
let cref x = l (S "ref") (ArrayCreateLocal([],x)) (l E (MSet(dref (V "ref"),x)) (V "ref"))

let rec ap' f = function
    | x :: xs -> ap' (ap f x) xs
    | [] -> f

let match_ x pat = ap (Inlineable("",pat)) x

let rec inl' args body = 
    match args with
    | x :: xs -> inl x (inl' xs body)
    | [] -> body

let rec inlr' name args body = 
    match args with
    | x :: xs -> inlr name x (inl' xs body)
    | [] -> body

/// The tuple map function. Pass it a pattern matching function that takes in on_fail and q as arguments.
let cuda_map =
    let recurse x = ap' (V "rec") [(V "f"); (V x)]
    inlr' "rec" [S "f"; S "q"]
        (l (S "on_fail")
            (inl (SS []) 
                (match_ (V "q")
                        [
                        SSS [SS' "v1"] "rest", cons (recurse "v1") (recurse "rest")
                        SS [], VV []
                        ])) 
        (ap' (V "f") [V "on_fail"; V "q"]))

/// Template for two member zip_map operations. Apply the first argument to curry the necessary operation (such as MSet or AtomicAdd)
/// and then pass it to cuda_zip_map.
let cuda_op2 = 
    inl' ([S "f"; S "on_fail"; S "l"]) 
        (match_ (V "l")
            [
            SS [S' "in"; S' "out"], ap (V "f") (VV [V "in"; V "out"])
            E, ap (V "on_fail") (VV [])
            ])

let cuda_op1 = 
    inl' ([S "f"; S "on_fail"; S "l"]) 
        (match_ (V "l")
            [
            S' "in", ap (V "f") (V "in")
            E, ap (V "on_fail") (VV [])
            ])

/// Maps over a tuple of 2-pairs tuples.
let cuda_map2 =
    inl' [S "f"; S "a"; S "b"] 
        (l (S "f") (ap cuda_op2 (V "f"))
        (ap' cuda_map [V "f"; VVZip <| VV [V "a"; V "b"]]))

// Maps over a tuple of singleton values.
let cuda_map1 =
    inl' [S "f"; S "a"] 
        (l (S "f") (ap cuda_op1 (V "f"))
        (ap' cuda_map [V "f"; V "a"]))

let data_empty (blockDim, gridDim) = 
    {memoized_method_envs=d0(); memoized_inlineable_envs=d0()
     memoized_methods=d0();blockDim=blockDim;gridDim=gridDim
     env=Map.empty;sequences=ref None;recursive_methods_stack=Stack()}

let typechecker_env_copy (d: CudaTypecheckerEnv) =
    {
    memoized_method_envs = d.memoized_method_envs |> Dictionary
    memoized_inlineable_envs = d.memoized_inlineable_envs |> Dictionary
    memoized_methods = d.memoized_methods |> Dictionary
    blockDim = d.blockDim
    gridDim = d.gridDim
    env = d.env
    sequences = ref !d.sequences
    recursive_methods_stack = d.recursive_methods_stack |> Stack
    }

let typechecker_env_set (t: CudaTypecheckerEnv) (d: CudaTypecheckerEnv) =
    t.memoized_method_envs.Clear(); d.memoized_method_envs |> Seq.iter (fun x -> t.memoized_method_envs.Add(x.Key,x.Value))
    t.memoized_inlineable_envs.Clear(); d.memoized_inlineable_envs |> Seq.iter (fun x -> t.memoized_inlineable_envs.Add(x.Key,x.Value))
    t.memoized_methods.Clear(); d.memoized_methods |> Seq.iter (fun x -> t.memoized_methods.Add(x.Key,x.Value))
    t.sequences := !d.sequences
    t.recursive_methods_stack.Clear(); d.recursive_methods_stack |> Seq.iter t.recursive_methods_stack.Push
    

let rec with_empty_seq (d: CudaTypecheckerEnv) expr =
    let d = {d with sequences = ref None}
    let expr = exp_and_seq d expr
    apply_sequences !d.sequences expr

// Does macro expansion and takes note of the bound and 
// used variables in the method dictionary for the following passes.
and exp_and_seq (_: CudaTypecheckerEnv) exp: TypedCudaExpr =
    let tev d exp = exp_and_seq d exp

    let append_typeof_fst k a b =
        k (a, b, (get_type a))

    let prim_bin_op_template d check_error is_check k a b t =
        let constraint_both_eq_numeric f k =
            let a,b = tev d a, tev d b
            if is_check a b then k a b
            else f (check_error a b)

        constraint_both_eq_numeric failwith (k t)

    let prim_arith_op d = 
        let er = sprintf "`is_numeric a && get_type a = get_type b` is false.\na=%A, b=%A"
        let check a b = is_numeric a && get_type a = get_type b
        prim_bin_op_template d er check append_typeof_fst

    let prim_atomic_add_op d = 
        let er = sprintf "`is_atomic_add_supported a && get_type a = get_type b` is false.\na=%A, b=%A"
        let check a b = is_atomic_add_supported a && get_type a = get_type b
        prim_bin_op_template d er check append_typeof_fst

    let prim_bool_op d = 
        let er = sprintf "`(is_numeric a || is_bool a) && get_type a = get_type b` is false.\na=%A, b=%A"
        let check a b = (is_numeric a || is_bool a) && get_type a = get_type b
        prim_bin_op_template d er check (fun t a b -> t (a,b))

    let prim_shift_op d =
        let er = sprintf "`is_int a && is_int b` is false.\na=%A, b=%A"
        let check a b = is_int a && is_int b
        prim_bin_op_template d er check append_typeof_fst

    let prim_shuffle_op d =
        let er = sprintf "`is_int b` is false.\na=%A, b=%A"
        let check a b = is_int b
        prim_bin_op_template d er check append_typeof_fst

    let prim_un_op_template d check_error is_check k a t =
        let constraint_numeric f k =
            let a = tev d a
            if is_check a then k a
            else f (check_error a)

        constraint_numeric failwith (k t)

    let prim_un_floating d = 
        let er = sprintf "`is_float a` is false.\na=%A"
        let check a = is_float a
        prim_un_op_template d er check (fun t a -> t (a, get_type a))

    let prim_un_numeric d = 
        let er = sprintf "`true` is false.\na=%A"
        let check a = true
        prim_un_op_template d er check (fun t a -> t (a, get_type a))

    let make_vvt x = List.map get_type x |> VVT

    let filter_duplicate_vars x =
        let h = h0()
        let rec loop = function
            | TyV(tag,t) as var -> if h.Add tag then Some var else None
            | TyVV(l,t) -> TyVV(List.choose loop l, t) |> Some
            | x -> Some x
        (loop x).Value

    let inline push_sequence d x = 
        let f current_sequence rest = apply_sequences current_sequence (x rest)
        d.sequences := Some (f !d.sequences)

    let make_tyv ty_exp = get_tag(), get_type ty_exp

    let make_tyv_and_push' d ty_exp =
        let v = make_tyv ty_exp
        push_sequence d (fun rest -> TyLet(v,ty_exp,rest,get_type rest))
        v

    let make_tyv_and_push d ty_exp = make_tyv_and_push' d ty_exp |> TyV

    // for a shallow version, take a look at `alternative_destructure_v6e.fsx`. 
    // The deep version can also be straightforwardly derived from a template of this using the Y combinator.
    let rec destructure_deep_template d nonseq_push r = 
        let destructure_deep d r = destructure_deep_template d nonseq_push r
        match r with
        | TyV _ | TyUnit | TyType _ -> r // Won't be passed into method as arguments apart from TyV.
        | TyVV(l,t) -> List.map (destructure_deep d) l |> fun x -> TyVV(x,t)
        | TyLitUInt32 _ | TyLitUInt64 _ | TyLitInt32 _ | TyLitInt64 _ 
        | TyLitFloat32 _ | TyLitFloat64 _ | TyLitBool _ 
        | TyThreadIdxX | TyThreadIdxY | TyThreadIdxZ
        | TyBlockIdxX | TyBlockIdxY | TyBlockIdxZ -> nonseq_push r // Will not be evaluated except at method calls.
        | _ -> 
            match get_type r with
            | VVT tuple_types -> 
                let indexed_tuple_args = List.mapi (fun i typ -> 
                    destructure_deep d <| TyIndexVV(r,TyLitInt32 i,typ)) tuple_types
                TyVV(indexed_tuple_args, VVT tuple_types)
            | _ -> 
                match r with
                | TyIndexVV _ -> nonseq_push r
                | _ -> make_tyv_and_push d r

    let destructure_deep d r = destructure_deep_template d id r
    let destructure_deep_method d r = destructure_deep_template d (make_tyv >> TyV) r

    let process_create_array d args typeof_expr =
        let typ = get_type (tev d typeof_expr)
        let args = 
            List.map (tev d) args
            |> fun args -> 
                if List.forall is_int args then args 
                else failwithf "An arg in CreateArray is not of Type int.\nGot: %A" args
            |> List.map (destructure_deep d)
        args, typ

    let dup_name_check (name_checker: HashSet<string>) arg_name f =
        match name_checker.Add arg_name || arg_name = "" || arg_name = "_" with
        | true -> f()
        | false -> failwithf "%s is a duplicate name in pattern matching." arg_name

    let case_bind_template map_add name_checker acc arg_name ty_exp ret =
        dup_name_check name_checker arg_name <| fun _ ->
            map_add arg_name ty_exp acc
        |> ret

    let case_bind_inl = case_bind_template (fun arg_name x acc -> 
        match arg_name with
        | "" | "_" -> acc
        | _ -> Map.add arg_name x acc)
        
    let case_bind_method = case_bind_template (fun arg_name x acc -> 
        match arg_name with
        | "" | "_" -> x, acc
        | _ -> x, Map.add arg_name x acc)
    
    let case_r_inl recurse acc (l,ls,ls') (r,rs) (t,ts) ret =
        recurse acc l r <| fun x_acc ->
            recurse x_acc (R(ls,ls')) (TyVV(rs,VVT ts)) ret

    let case_r_method recurse acc (l,ls,ls') (r,rs) (t,ts) ret =
        recurse acc l r <| fun (x, x_acc) ->
            recurse x_acc (R(ls,ls')) (TyVV(rs,VVT ts)) <| function
                | (TyVV(xs,VVT ts), xs_acc) -> ret (TyVV(x :: xs, VVT <| t :: ts), xs_acc)
                | _ -> failwith "impossible"

    let case_f d apply on_fail acc body args ret =
        let d' = typechecker_env_copy d
        let d = {d with env = acc}
        apply d (fun _ -> 
            typechecker_env_set d d'
            on_fail() // <| "Function application in pattern matcher failed to match a pattern."
            ) body args (make_tyv_and_push d >> ret)

    let rec match_single case_f case_r case_bind (acc: Env) l r on_fail ret =
        let recurse acc l r ret = match_single case_f case_r case_bind acc l r on_fail ret
        match l,r with // destructure_deep is called in apply_method and apply_inlineable
        | F (name,args) , body -> case_f on_fail acc body args (fun r -> case_bind acc name r ret)
        | R([],None), TyVV([], _) -> case_bind acc "" r ret
        | S' x, TyVV _ -> on_fail () //<| "S' matched a tuple."
        | S' x, _ | S x, _ -> case_bind acc x r ret
        | R([],Some ls), TyVV _ -> recurse acc ls r ret
        | R(l::ls,ls'), TyVV(r :: rs,VVT (t :: ts)) -> case_r recurse acc (l,ls,ls') (r,rs) (t,ts) ret
        | R([],None), TyVV(_, _) -> on_fail () // <| sprintf "More arguments than can be matched in R."
        | R _, _ -> on_fail () //<| sprintf "Cannot destructure %A." r
    
    let match_single_inl' case_f dup_name_checker = match_single case_f case_r_inl (case_bind_inl dup_name_checker)
    let match_single_method' case_f dup_name_checker = match_single case_f case_r_method (case_bind_method dup_name_checker)

    let match_single_inl case_f = match_single_inl' case_f (h0())
    let match_single_method case_f = match_single_method' case_f (h0())

    let match_all match_single env l args on_fail ret =
        let rec loop = function
            | (pattern, body) :: xs ->
                match_single env pattern args
                    (fun _ -> loop xs)
                    (fun x -> ret (x, body))
            | [] -> on_fail () //"All patterns in matcher failed to match."
        loop l

    let apply_inlineable d case_f on_fail ((name,l),_ as inlineable_key) args ret =
        let env = d.memoized_inlineable_envs.[inlineable_key]
        let args = destructure_deep d args
        match_all (match_single_inl case_f) env l args
            on_fail
            (fun (env, body) -> 
                let d = {d with env = if name <> "" then Map.add name (TyType <| InlineableT inlineable_key) env else env}
                tev d body |> ret)

    let apply_method d case_f on_fail ((name,l), _ as method_key) args ret =
        let initial_env = d.memoized_method_envs.[method_key]
        let bound_outside_args = destructure_deep d args
        match_all (match_single_method case_f) initial_env l (destructure_deep_method d bound_outside_args) 
            on_fail
            (fun ((bound_inside_args, env), body) ->
                let bound_outside_args, bound_inside_args, env, body = 
                    filter_duplicate_vars bound_outside_args, filter_duplicate_vars bound_inside_args, env, body

                let make_method_call body_type = TyMethodCall(method_key, bound_outside_args, body_type)

                match d.memoized_methods.TryGetValue method_key with
                | false, _ ->                         
                    d.memoized_methods.[method_key] <- MethodInEvaluation(None)

                    let d = {d with env = if name <> "" then Map.add name (TyType <| MethodT method_key) env else env}
                    let body = with_empty_seq d body
                    let sole_arguments = filter_tyvs bound_inside_args

                    d.memoized_methods.[method_key] <- MethodDone(sole_arguments, body, get_tag())

                    if is_returnable body then make_method_call (get_type body)
                    else failwithf "Expected a simple type as the function return.\nGot: %A" (get_type body)
                | true, MethodInEvaluation None ->
                    let body = get_body_from d.recursive_methods_stack
                    let t = get_type body
                    d.memoized_methods.[method_key] <- MethodInEvaluation (Some t)
                    make_method_call t
                | true, MethodInEvaluation (Some t) ->
                    make_method_call t
                | true, MethodDone(_,body,_) ->
                    make_method_call (get_type body)
                |> ret)

    let rec apply_template d on_fail apply_inlineable apply_method la (ra: TypedCudaExpr) =
        match get_type (tev d la) with
        | InlineableT(x,env) -> apply_inlineable d (case_f d apply_both) on_fail (x,env) ra
        | MethodT(x,env) -> apply_method d (case_f d apply_both) on_fail (x,env) ra
        | _ -> failwith "impossible"

    and apply_both d on_fail la (ra : TypedCudaExpr ) ret = //apply_template d on_fail apply_inlineable apply_method
        match get_type (tev d la) with
        | InlineableT(x,env) -> //apply_inlineable d (case_f d apply_both) on_fail (x,env) ra
            let case_f = case_f d apply_both
            let (name,l),_ as inlineable_key = x, env
            let args = ra 
            let env = d.memoized_inlineable_envs.[inlineable_key]
            let args = destructure_deep d args
            match_all (match_single_inl case_f) env l args
                on_fail
                (fun (env, body) -> 
                    let d = {d with env = if name <> "" then Map.add name (TyType <| InlineableT inlineable_key) env else env}
                    tev d body |> ret)
//        | MethodT(x,env) -> apply_method d (case_f d apply_both) on_fail (x,env) ra
//        | _ -> failwith "impossible"
    //and apply_method_only d on_fail = apply_template d on_fail (fun _ -> failwith "Inlineable not supported.") apply_method

    failwith "x"

//    let apply_standard d expr args = apply_both d failwith expr (tev d args)
//    let apply_match d on_fail expr args = apply_both d on_fail expr (tev d args)
//
//    match exp with
//    | T x -> x // To assist in CubBlockReduce so evaled cases do not have to be evaluated twice.
//    | V' x -> TyV x // To assist in interfacing with the outside.
//    | LitInt32 x -> TyLitInt32 x
//    | LitInt64 x -> TyLitInt64 x
//    | LitUInt32 x -> TyLitUInt32 x
//    | LitUInt64 x -> TyLitUInt64 x
//    | LitFloat32 x -> TyLitFloat32 x
//    | LitFloat64 x -> TyLitFloat64 x
//    | LitBool x -> TyLitBool x
//    | B -> TyUnit
//    | V x -> 
//        match Map.tryFind x d.env with
//        | Some v -> v
//        | None -> failwithf "Variable %A not bound." x
//    | Inlineable l -> 
//        let k = l, Map.map (fun _ -> get_type) d.env
//        d.memoized_inlineable_envs.[k] <- d.env
//        InlineableT k |> TyType
//    | Method(name, l) -> 
//        let k = (name, l), Map.map (fun _ -> get_type) d.env
//        d.memoized_method_envs.[k] <- d.env
//        MethodT k |> TyType
//    | Apply(expr,args) -> apply_standard expr args
//    | If(cond,tr,fl) ->
//        let cond = tev d cond
//        if is_bool cond = false then failwithf "Expected a bool in conditional.\nGot: %A" (get_type cond)
//        else
//            let tev' e f =
//                let mutable is_popped = false
//                d.recursive_methods_stack.Push (fun _ -> is_popped <- true; f())
//                let x = with_empty_seq d e
//                if is_popped = false then d.recursive_methods_stack.Pop() |> ignore
//                x
//
//            let mutable fl_result = None
//            
//            let tr = tev' tr (fun _ -> 
//                let fl = tev d fl
//                fl_result <- Some fl
//                fl)
//
//            let fl = 
//                match fl_result with
//                | Some fl -> fl
//                | None -> tev' fl (fun _ -> tr)
//
//            let type_tr, type_fl = get_type tr, get_type fl
//            if type_tr = type_fl then tev d (Apply(Method("",[E,T (TyIf(cond,tr,fl,type_tr))]),B))
//            else failwithf "Types in branches of If do not match.\nGot: %A and %A" type_tr type_fl
//    | VVZip(a) ->
//        let case_s l on_fatalfail on_fail ret =
//            let rec loop (acc: CudaExpr list) = function
//                | VV _ :: _ when acc.IsEmpty = false -> on_fatalfail()
//                | VV _ :: _ -> on_fail ()
//                | x :: xs -> loop (x :: acc) xs
//                | [] -> ret (List.rev acc)
//            loop [] l
//
//        let case_r l on_fatalfail on_fail ret =
//            let rec loop (acc_head: CudaExpr list) (acc_tail: CudaExpr list) = function
//                | (VV (h :: t)) :: xs -> loop (h :: acc_head) (VV t :: acc_tail) xs
//                | _ :: _ when acc_head.IsEmpty = false -> on_fatalfail()
//                | _ :: _ -> on_fail ()
//                | [] -> ret (List.rev acc_head, List.rev acc_tail)
//            loop [] [] l
//
//        let case_r_empty l on_fail ret =
//            let rec loop = function
//                | VV [] :: xs -> loop xs
//                | [] -> ret()
//                | _ -> on_fail ()
//            loop l
//
//        let rec zip_all l ret =
//            let fatalfail acc _ = List.rev acc @ l |> VV |> ret
//            case_s l 
//                (fatalfail [])
//                (fun _ ->
//                    let rec loop acc l =
//                        case_r l 
//                            (fatalfail acc)
//                            (fun _ -> 
//                                case_r_empty l 
//                                    (fatalfail acc)
//                                    (fun _ -> List.rev acc |> VV |> ret))
//                            (fun (head, tail) ->
//                                zip_all head <| fun r -> loop (r :: acc) tail)
//                    loop [] l)
//                (VV >> ret)
//
//        let rec zip_remap = function
//            | TyVV(a,_) -> VV (List.map zip_remap a)
//            | a -> T a
//
//        match tev d a |> destructure_deep |> zip_remap with
//        | VV x -> zip_all x id |> tev d
//        | x -> x |> tev d
//    | VV vars ->
//        let vv = List.map (tev d) vars
//        TyVV(vv,make_vvt vv)
//    | VVIndex(v,i) ->
//        match tev d v, tev d i with
//        | v, (TyLitInt32 i as i') ->
//            match get_type v with
//            | VVT ts -> 
//                if i >= 0 || i < List.length ts then TyIndexVV(v,i',ts.[i])
//                else failwith "(i >= 0 || i < List.length ts) = false in IndexVT"
//            | x -> failwithf "Type of a evaluated expression in IndexVT is not VTT.\nGot: %A" x
//        | v, i ->
//            failwithf "Index into a tuple must be a natural number less than the size of the tuple.\nGot: %A" i
//    | VVCons(a,b) ->
//        let a = tev d a
//        let b = tev d b |> destructure_deep
//        match b with
//        | TyVV(b, VVT bt) -> TyVV(a::b, VVT (get_type a :: bt))
//        | _ -> failwith "Expected a tuple on the right is in VVCons."
//    // Array cases
//    | ArrayIndex(exp,args) ->
//        let exp,args = tev d exp, List.map (tev d) args
//        match get_type exp with
//        | LocalArrayT(vs, t) | SharedArrayT(vs, t) | GlobalArrayT(vs, t) when List.forall is_int args && List.length vs = List.length args ->
//            TyArrayIndex(exp,args,t)
//        | _ -> failwithf "Something is wrong in ArrayIndex.\nexp=%A, args=%A" exp args
//    | ArraySize(ar,ind) ->
//        let ar, ind = tev d ar, tev d ind
//        match get_type ar, ind with
//        | (LocalArrayT(vs, t) | SharedArrayT(vs, t) | GlobalArrayT(vs, t)), TyLitInt32 i ->
//            if i < vs.Length then vs.[i]
//            else failwith "Array size not available."
//        | _ -> failwithf "Something is wrong in ArraySize.\nar=%A, ind=%A" ar ind
//
//    | ArrayCreateLocal(args,t) -> let args,t = process_create_array args t in TyArrayCreate(LocalArrayT(args,t))
//    | ArrayCreateShared(args,t) -> let args,t = process_create_array args t in TyArrayCreate(SharedArrayT(args,t))
//
//    | ThreadIdxX -> TyThreadIdxX 
//    | ThreadIdxY -> TyThreadIdxY 
//    | ThreadIdxZ -> TyThreadIdxZ
//    | BlockIdxX -> TyBlockIdxX 
//    | BlockIdxY -> TyBlockIdxY 
//    | BlockIdxZ -> TyBlockIdxZ
//    | BlockDimX -> TyLitUInt64 (uint64 d.blockDim.x) 
//    | BlockDimY -> TyLitUInt64 (uint64 d.blockDim.y) 
//    | BlockDimZ -> TyLitUInt64 (uint64 d.blockDim.z)
//    | GridDimX -> TyLitUInt64 (uint64 d.gridDim.x)
//    | GridDimY -> TyLitUInt64 (uint64 d.gridDim.y) 
//    | GridDimZ -> TyLitUInt64 (uint64 d.gridDim.z)
//
//    // Primitive operations on expressions.
//    | Add(a,b) -> prim_arith_op a b TyAdd
//    | Sub(a,b) -> prim_arith_op a b TySub
//    | Mult(a,b) -> prim_arith_op a b TyMult
//    | Div(a,b) -> prim_arith_op a b TyDiv
//    | Mod(a,b) -> prim_arith_op a b TyMod
//        
//    | LT(a,b) -> prim_bool_op a b TyLT
//    | LTE(a,b) -> prim_bool_op a b TyLTE
//    | EQ(a,b) -> prim_bool_op a b TyEQ
//    | GT(a,b) -> prim_bool_op a b TyGT
//    | GTE(a,b) -> prim_bool_op a b TyGTE
//
//    | Syncthreads -> TySyncthreads
//
//    | LeftShift(a,b) -> prim_shift_op a b TyLeftShift
//    | RightShift(a,b) -> prim_shift_op a b TyRightShift
//    
//    | ShuffleXor(a,b) -> prim_shuffle_op a b TyShuffleXor
//    | ShuffleUp(a,b) -> prim_shuffle_op a b TyShuffleUp
//    | ShuffleDown(a,b) -> prim_shuffle_op a b TyShuffleDown
//    | ShuffleIndex(a,b) -> prim_shuffle_op a b TyShuffleIndex
//
//    | Log a -> prim_un_floating a TyLog
//    | Exp a -> prim_un_floating a TyExp
//    | Tanh a -> prim_un_floating a TyTanh
//    | Neg a -> prim_un_numeric a TyNeg
//    // Mutable operations.
//    | MSet(a,b) -> 
//        let l = tev d a
//        let r = destructure_deep (tev d b)
//        match l, r with 
//        | TyArrayIndex(_,_,lt), r when lt = get_type r -> push_sequence (fun rest -> TyMSet(l,r,rest,get_type rest)); TyUnit
//        | _ -> failwithf "Error in mset. Expected: (TyArrayIndex(_,_,lt) as v), r when lt = get_type r or TyVV(l,_), TyVV(r,_).\nGot: %A and %A" l r
//    | AtomicAdd(a,b) -> prim_atomic_add_op a b TyAtomicAdd
//    | While(cond,body,e) ->
//        let cond, body = tev d (Apply(Method("",[E,cond]),B)), with_empty_seq d body
//        match get_type cond, get_type body with
//        | PrimT BoolT, UnitT -> push_sequence (fun rest -> TyWhile(cond,body,rest,get_type rest)); tev d e
//        | PrimT BoolT, _ -> failwith "Expected UnitT as the type of While's body."
//        | _ -> failwith "Expected BoolT as the type of While's conditional."
//    | CubBlockReduce(input, method_, num_valid) ->
//        let dim = 
//            if int d.blockDim.y <> 1 || int d.blockDim.z <> 1 then // TODO: Remove this restriction.
//                failwith "int d.blockDim.y <> 1 || int d.blockDim.z <> 1.\nTODO: Remove this restriction."
//            tev d BlockDimX
//        let evaled_input = tev d input
//        let method_ =
//            match get_type evaled_input with
//            | LocalArrayT(_,t) | SharedArrayT(_,t) -> 
//                let arg = ArrayIndex(T evaled_input,[LitInt32 0])
//                tev d (VV [arg;arg])
//            | x -> 
//                let arg() = evaled_input |> make_tyv |> TyV
//                tev d (VV [T (arg()); T (arg())])
//            |> apply_method_only failwith method_
//
//        let num_valid = Option.map (tev d) num_valid
//        TyCubBlockReduce(dim, evaled_input,method_,num_valid,get_type method_)
//            
///// Propagates the free variables downwards.
///// The closures are stack allocated hence not allowed to return from functions.
//let rec closure_conv (imemo: MethodImplDict) (memo: MethodDict) (exp: TypedCudaExpr) =
//    let c x = closure_conv imemo memo x
//    match exp with
//    | TyType t -> Set.empty // TODO: When I go with full out type based metaprogramming, fix this so closure conv propagates variables through types as well.
//    | TyV v -> Set.singleton v
//    | TyIf(cond,tr,fl,t) -> c cond + c tr + c fl
//    | TyLitUInt32 _ | TyLitUInt64 _ | TyLitInt32 _ | TyLitInt64 _ 
//    | TyLitFloat32 _ | TyLitFloat64 _ | TyLitBool _ | TyUnit -> Set.empty
//    | TyVV(vars,t) -> Set.unionMany (List.map c vars)
//    | TyIndexVV(t,i,_) -> Set.union (c t) (c i)
//    | TyMethodCall(m,ar,_) ->
//        let method_implicit_args =
//            match imemo.TryGetValue m with
//            | true, (_,_,_,impl_args) -> impl_args
//            | false, _ ->
//                match memo.[m] with
//                | MethodDone(sol_arg, body, tag) -> 
//                    let impl_args = c body - Set(sol_arg)
//                    imemo.Add(m,(sol_arg,body,tag,impl_args))
//                    impl_args
//                | _ -> failwith "impossible"
//        Set.union method_implicit_args (c ar)
//    | TyCubBlockReduce(_,inp,(TyMethodCall(key,_,_) as m),num_valid,t) ->
//        ignore <| c m // This is so it gets added to the env.
//
//        match imemo.[key] with
//        | _,_,_,impl_args when impl_args.IsEmpty ->
//            match num_valid with
//            | Some num_valid -> Set.union (c num_valid) (c inp)
//            | None -> c inp
//        | impl_args -> 
//            failwithf "The method passed to Cub should have no implicit arguments.\nm=%A\nimpl_args=%A" m impl_args
//    | TyCubBlockReduce _ -> failwith "impossible"
//    // Array cases
//    | TyArrayIndex(a,b,_) -> 
//        let a = 
//            match get_type a with
//            | LocalArrayT(x,_) | SharedArrayT(x,_) | GlobalArrayT(x,_) ->
//                let b = 
//                    match x with
//                    | [] -> []
//                    | x -> List.choose (function TyV v -> Some v | _ -> None) (List.tail x)
//                Set.union (c a) (Set(b))
//            | _ -> failwith "impossible"
//        Set.union a (Set.unionMany (List.map c b))
//    | TyArrayCreate t -> 
//        match t with
//        | LocalArrayT(x,_) | SharedArrayT(x,_) | GlobalArrayT(x,_) ->
//            Set.unionMany (List.map c x)
//        | _ -> failwith "impossible"
//    // Cuda kernel constants
//    | TyThreadIdxX | TyThreadIdxY | TyThreadIdxZ
//    | TyBlockIdxX | TyBlockIdxY | TyBlockIdxZ -> Set.empty
//    // Primitive operations on expressions.
//    | TySyncthreads -> Set.empty
//    | TyLog(a,_) | TyExp(a,_) | TyTanh(a,_) | TyNeg(a,_) -> c a
//    | TyAdd(a,b,_) | TySub(a,b,_) | TyMult(a,b,_) | TyDiv(a,b,_) | TyMod(a,b,_)
//    | TyLT(a,b) | TyLTE(a,b) | TyEQ(a,b) | TyGT(a,b) | TyGTE(a,b) 
//    | TyLeftShift(a,b,_) | TyRightShift(a,b,_) | TyShuffleXor(a,b,_)
//    | TyShuffleUp(a,b,_) | TyShuffleDown(a,b,_) | TyShuffleIndex(a,b,_) 
//    | TyAtomicAdd(a,b,_) -> Set.union (c a) (c b)
//    | TyMSet(a, b, rest, _) | TyWhile(a, b, rest, _) -> c a + c b + c rest
//    | TyLet(v, body, rest, _) -> c body + c rest |> Set.remove v
//
//let typecheck dims body inputs = 
//    try
//        let main_method, memo = 
//            let d = data_empty dims
//            exp_and_seq d (Apply(body,inputs)), d.memoized_methods
//        let imemo = Dictionary(HashIdentity.Structural)
//        closure_conv imemo memo main_method |> ignore
//        Succ imemo
//    with e -> Fail (e.Message, e.StackTrace)
//
///// Reasonable default for the dims.
//let default_dims = dim3(256), dim3(20)
//
//let typecheck0 program = typecheck default_dims program (VV [])
//
