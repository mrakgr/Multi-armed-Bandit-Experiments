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
    | PrimT of SpiralDeviceVarType
    | VVT of CudaTy list
    | VVNamedT of CudaTy list * string
    | GlobalArrayT of TypedCudaExpr * CudaTy
    | SharedArrayT of TypedCudaExpr * CudaTy
    | LocalArrayT of TypedCudaExpr * CudaTy
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
    | A of CudaPattern * CudaExpr // Type annotation case
    | A' of CudaPattern * CudaTy
    | S of string
    | S' of string // match if not tuple
    | R of CudaPattern list * CudaPattern option // Tuple
    | F of CudaPattern * CudaExpr // Functiona application with retracing.
    | N of string * CudaPattern // matches a tuple name and proceeds onto the pattern on a hit.

and CudaExpr = 
    | TypeError of string
    | V of string // standard variable
    | V' of TyV // given variable
    | T of TypedCudaExpr
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
    | VVNamed of CudaExpr list * string // named tuple
    | VVCons of CudaExpr * CudaExpr
    | VVZipReg of CudaExpr
    | VVZipIrreg of CudaExpr
    | VVUnzipReg of CudaExpr
    | VVUnzipIrreg of CudaExpr

    // Array cases
    | ArrayIndex of CudaExpr * CudaExpr
    | ArraySize of CudaExpr * CudaExpr
    | ArrayCreateShared of CudaExpr * typeof: CudaExpr
    | ArrayCreateLocal of CudaExpr * typeof: CudaExpr

    // Primitive operations on expressions.
    | Add of CudaExpr * CudaExpr
    | Sub of CudaExpr * CudaExpr
    | Mult of CudaExpr * CudaExpr
    | Div of CudaExpr * CudaExpr
    | Mod of CudaExpr * CudaExpr
    | LTE of CudaExpr * CudaExpr
    | LT of CudaExpr * CudaExpr
    | EQ of CudaExpr * CudaExpr
    | NEQ of CudaExpr * CudaExpr
    | GT of CudaExpr * CudaExpr
    | GTE of CudaExpr * CudaExpr
    | And of CudaExpr * CudaExpr
    | Or of CudaExpr * CudaExpr
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
    | TyType of CudaTy
    
    | TyV of TyV
    | TyIf of TypedCudaExpr * TypedCudaExpr * TypedCudaExpr * CudaTy
    | TyLet of TyV * TypedCudaExpr * TypedCudaExpr * CudaTy
    | TyLitUInt32 of uint32
    | TyLitUInt64 of uint64
    | TyLitInt32 of int
    | TyLitInt64 of int64
    | TyLitFloat32 of float32
    | TyLitFloat64 of float
    | TyLitBool of bool
    | TyMethodCall of TyMethodKey * TypedCudaExpr * CudaTy
    
    // Tuple cases
    | TyVVIndex of TypedCudaExpr * TypedCudaExpr * CudaTy
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
    | TyNEQ of TypedCudaExpr * TypedCudaExpr
    | TyGT of TypedCudaExpr * TypedCudaExpr
    | TyGTE of TypedCudaExpr * TypedCudaExpr
    | TyAnd of TypedCudaExpr * TypedCudaExpr
    | TyOr of TypedCudaExpr * TypedCudaExpr
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
    | TyVV(_,t) | TyVVIndex(_,_,t) -> t

    // Array cases
    | TyArrayIndex(_,_,t) | TyArrayCreate(t) -> t

    // Primitive operations on expressions.
    | TyAdd(_,_,t) | TySub(_,_,t) | TyMult(_,_,t)
    | TyDiv(_,_,t) | TyMod(_,_,t) -> t
    | TyLT _ | TyLTE _ | TyEQ _ | TyNEQ _ | TyGT _
    | TyGTE _ | TyAnd _ | TyOr _ -> PrimT BoolT
    | TyLeftShift(_,_,t) | TyRightShift(_,_,t) -> t
    | TySyncthreads -> VVT []
    | TyShuffleXor(_,_,t) | TyShuffleUp(_,_,t)
    | TyShuffleDown(_,_,t) | TyShuffleIndex(_,_,t) -> t
    | TyLog(_,t) | TyExp(_,t) | TyTanh(_,t)
    | TyNeg(_,t) -> t
    // Mutable operations.
    | TyAtomicAdd(_,_,t) -> t
    
    // Statements
    | TyMSet(_,_,_,t) | TyWhile(_,_,_,t) | TyLet((_,_),_,_,t)  -> t
    // Cub operations
    | TyCubBlockReduce(_,_,_,_,t) -> t

let rec is_returnable' = function
    | PrimT x -> 
        match x with
        | UInt8T | UInt16T | UInt32T | UInt64T 
        | Int8T | Int16T | Int32T | Int64T 
        | Float32T | Float64T | BoolT -> true
    | GlobalArrayT _ -> true
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

let is_arg = function TyType _ -> false | _ -> true

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
let methr name x y = Method(name,[x,y])

let E = S ""
let B = VV []
let TyB = TyVV([], VVT [])
/// Matches tuples without a tail.
let SS x = R (x, None) 
/// Opposite of S', matches only a tuple.
let SS' x = R ([], Some (S x)) 
/// Matches tuples with a tail.
let SSS a b = R(a, Some (S b)) 

let cons a b = VVCons(a,b)

let l v b e = Apply(inl v e,b)

let while_ cond body rest = While(cond,body,rest)
let s l fin = List.foldBack (fun x rest -> x rest) l fin

let dref x = ArrayIndex(x,VV [])
let cref x = 
    s [l (S "x") x
       l (S "ref") (ArrayCreateLocal(VV [],V "x")) 
       l E (MSet(dref (V "ref"),V "x"))] (V "ref")
   
let for_template end_ init cond body =
    l (S "init") (cref init)
        (while_ (ap cond (dref <| V "init"))
            (MSet(dref (V "init"), ap body (dref <| V "init"))) end_)

let for' init cond body = for_template (dref <| V "init") init cond body
let for_ init cond body = l E (for_template B init cond body)

let rec ap' f = function
    | x :: xs -> ap' (ap f x) xs
    | [] -> f

let match_ x pat = ap (Inlineable("",pat)) x
let function_ pat = Inlineable("",pat)

let rec inlr' name args body = 
    match args with
    | x :: xs -> inlr name x (inlr' "" xs body)
    | [] -> body

let rec inl' args body = inlr' "" args body

let rec methr' name args body = 
    match args with
    | [x] -> methr name x body
    | x :: xs -> inlr name x (methr' "" xs body)
    | [] -> body

let meth' args body = methr' "" args body

/// The tuple map function. Goes over the tuple scanning for a pattern and triggers only if it finds it.
let tuple_map =
    let recurse x = ap (V "rec") (VV [V "f"; V x])
    inlr "rec" (SS [S "f"; S "q"])
        (match_ (V "q")
            [
            F(S "x",V "f"), V "x"
            SSS [SS' "v1"] "rest", cons (recurse "v1") (recurse "rest")
            SS [], VV []
            E, TypeError <| sprintf "tuple .Map failed to match."
            ])

let tuple_library =
    function_
        [
        N("Map", S "x"), ap tuple_map (V "x")
        N("ZipReg", S "x"), VVZipReg (V "x")
        N("ZipIrreg", S "x"), VVZipIrreg (V "x")
        N("UnzipReg", S "x"), VVUnzipReg (V "x")
        N("UnzipIrreg", S "x"), VVUnzipIrreg (V "x")
        E, TypeError "Call to non-existent case in the tuple function."
        ]

let cuda_library =
    function_
        [
        N("BlockDimX",S ""), BlockDimX
        N("BlockDimY",S ""), BlockDimY
        N("BlockDimZ",S ""), BlockDimZ
        N("GridDimX",S ""), GridDimX
        N("GridDimY",S ""), GridDimY
        N("GridDimZ",S ""), GridDimZ
        N("ThreadIdxX",S ""), ThreadIdxX
        N("ThreadIdxY",S ""), ThreadIdxY
        N("ThreadIdxZ",S ""), ThreadIdxZ
        N("BlockIdxX",S ""), BlockIdxX
        N("BlockIdxY",S ""), BlockIdxY
        N("BlockIdxZ",S ""), BlockIdxZ
        N("CubBlockReduce",SS [S "value"; S "method"]), CubBlockReduce(V "value",V "method",None)
        N("CubBlockReduce",SS [S "value"; S "method"; S "valid_threads"]), CubBlockReduce(V "value",V "method",Some <| V "valid_threads")
        N("ArrayCreateShared",SS [S "size"; S "typeof"]), ArrayCreateShared(V "size",V "x")
        N("ArrayCreateLocal",SS [S "size"; S "typeof"]), ArrayCreateLocal(V "size",V "x")
        E, TypeError "Call to non-existent case in the cuda function."
        ]

           
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
and exp_and_seq (d: CudaTypecheckerEnv) exp: TypedCudaExpr =
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
    let destructure_deep_template d nonseq_push r = 
        let rec destructure_deep r = 
            let destructure_tuple r on_non_tuple on_tuple =
                match get_type r with
                | VVT tuple_types -> 
                    let indexed_tuple_args = List.mapi (fun i typ -> 
                        destructure_deep <| TyVVIndex(r,TyLitInt32 i,typ)) tuple_types
                    TyVV(indexed_tuple_args, VVT tuple_types) |> on_tuple
                | _ -> on_non_tuple r
            match r with
            | TyType _ -> r // Won't be passed into method as arguments apart from TyV.
            | TyVV(l,t) -> TyVV(List.map destructure_deep l,t)
            | TyLitUInt32 _ | TyLitUInt64 _ | TyLitInt32 _ | TyLitInt64 _ 
            | TyLitFloat32 _ | TyLitFloat64 _ | TyLitBool _ 
            | TyThreadIdxX | TyThreadIdxY | TyThreadIdxZ
            | TyBlockIdxX | TyBlockIdxY | TyBlockIdxZ -> nonseq_push r // Will not be assigned to a variable except at method calls.
            | TyV _ -> destructure_tuple r id nonseq_push
            | TyVVIndex _ | TyArrayIndex(_,[],_) -> destructure_tuple r nonseq_push nonseq_push
            | _ -> make_tyv_and_push d r |> destructure_deep
        destructure_deep r

    let destructure_deep d r = destructure_deep_template d id r
    let destructure_deep_method d r = destructure_deep_template d (make_tyv >> TyV) r

    let process_create_array d args typeof_expr =
        let typ = get_type (tev d typeof_expr)
        let args = 
            tev d args
            |> destructure_deep d
            |> function 
                | TyVV(args,_) as x -> 
                    if List.forall is_int args then x
                    else failwithf "An arg in CreateArray is not of Type int.\nGot: %A" args
                | args -> 
                    if is_int args then args
                    else failwithf "The arg in CreateArray is not of Type int.\nGot: %A" args
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

    let case_f d apply match_single acc (pattern: CudaPattern) args meth on_fail ret =
        let d' = typechecker_env_copy d
        let d = {d with env = acc}
        apply d meth args
            (fun _ -> 
                typechecker_env_set d d'
                on_fail()) // <| "Function application in pattern matcher failed to match a pattern."
            (fun r -> 
                match_single acc pattern (destructure_deep d r) 
                    (fun _ -> failwith "The subpattern in F failed to match.")
                    ret)

    let case_a' annot args on_fail ret = if annot = get_type args then ret() else on_fail()
    let case_a d annot = case_a' (tev d annot |> get_type) 

    let match_single case_a case_a' case_f case_r case_bind (acc: Env) l r on_fail ret =
        let rec recurse acc l r ret = //match_single case_f case_r case_bind acc l r on_fail ret
            match l,r with // destructure_deep is called in apply_method and apply_inlineable
            | A (pattern,annot), _ -> case_a annot r on_fail (fun _ -> recurse acc pattern r ret) 
            | A' (pattern,annot), _ -> case_a' annot r on_fail (fun _ -> recurse acc pattern r ret) 
            | F (pattern, meth), args -> case_f acc pattern args meth on_fail ret
            | R([],None), TyVV([], _) -> case_bind acc "" r ret
            | S' x, TyVV _ -> on_fail () //<| "S' matched a tuple."
            | S' x, _ | S x, _ -> case_bind acc x r ret
            | R([],Some ls), TyVV _ -> recurse acc ls r ret
            | R(l::ls,ls'), TyVV(r :: rs,VVT (t :: ts)) -> case_r recurse acc (l,ls,ls') (r,rs) (t,ts) ret
            | R([],None), TyVV(_, _) -> on_fail () // <| sprintf "More arguments than can be matched in R."
            | R _, _ -> on_fail () //<| sprintf "Cannot destructure %A." r
            | N (name, next), TyVV(x, VVNamedT(t, name')) ->
                if name = name' then recurse acc next (TyVV(x,VVT t)) ret
                else on_fail() // <| sprintf "Cannot pattern match %s against %s" name name'
            | N _, _ -> on_fail() // "Cannot match name against a non-named argument."
        recurse acc l r ret
    
    let rec match_single_inl' case_a case_a' case_f dup_name_checker = 
        let case_f x = case_f (match_single_inl' case_a case_a' case_f dup_name_checker) x
        match_single case_a case_a' case_f case_r_inl (case_bind_inl dup_name_checker)
    let rec match_single_method' case_a case_a' case_f dup_name_checker = 
        let case_f x = case_f (match_single_method' case_a case_a' case_f dup_name_checker) x
        match_single case_a case_a' case_f case_r_method (case_bind_method dup_name_checker)

    let match_single_inl case_a case_a' case_f = match_single_inl' case_a case_a' case_f (h0())
    let match_single_method case_a case_a' case_f = match_single_method' case_a case_a' case_f (h0())

    let match_all match_single (env: Env) l (args: TypedCudaExpr) on_fail ret =
        let rec loop = function
            | (pattern: CudaPattern, body: CudaExpr) :: xs ->
                match_single env pattern args
                    (fun _ -> loop xs)
                    (fun x -> ret (x, body))
            | [] -> on_fail (l,args) //"All patterns in the matcher failed to match."
        loop l

    let apply_inlineable d case_a case_a' case_f ((name,l),_ as inlineable_key) args on_fail ret =
        let env = d.memoized_inlineable_envs.[inlineable_key]
        let args = destructure_deep d args
        match_all (match_single_inl case_a case_a' case_f) env l args
            on_fail
            (fun (env, body) -> 
                let d = {d with env = if name <> "" then Map.add name (TyType <| InlineableT inlineable_key) env else env}
                tev d body |> ret)

    let apply_method d case_a case_a' case_f ((name,l), _ as method_key) args on_fail ret =
        let initial_env = d.memoized_method_envs.[method_key]
        let bound_outside_args = destructure_deep d args
        match_all (match_single_method case_a case_a' case_f) initial_env l (destructure_deep_method d bound_outside_args) 
            on_fail
            (fun ((bound_inside_args, env), body) ->
                printfn "I am inside method."
                let bound_outside_args, bound_inside_args, env, body = 
                    filter_duplicate_vars bound_outside_args, filter_duplicate_vars bound_inside_args, env, body

                let make_method_call body_type = TyMethodCall(method_key, bound_outside_args, body_type)

                match d.memoized_methods.TryGetValue method_key with
                | false, _ ->                         
                    d.memoized_methods.[method_key] <- MethodInEvaluation(None)

                    let d = {d with env = if name <> "" then Map.add name (TyType <| MethodT method_key) env else env}
                    printfn "I am going to call with_empty_seq."
                    let body = with_empty_seq d body
                    printfn "Done with with_empty_seq."
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

    let rec apply_template d apply_inlineable apply_method (la: CudaExpr) ra on_fail ret =
        let la = tev d la
        match get_type la with
        | InlineableT(x,env) -> apply_inlineable d (case_a d) case_a' (case_f d apply_both) (x,env) ra on_fail ret
        | MethodT(x,env) -> apply_method d (case_a d) case_a' (case_f d apply_both) (x,env) ra on_fail ret
        | LocalArrayT _ | SharedArrayT _ | GlobalArrayT _ -> 
            // This case is a bit of a hack to make the indexing syntax nicer. 
            // In the full language, I'd use closures for arrays instead. 
            // That is some ways off, and I do not feel like implementing them just yet.
            ArrayIndex(T la, T ra) |> tev d 
        | _ -> failwith "Trying to apply a type other than InlineableT or MethodT."

    and apply_both d (la: CudaExpr) ra = apply_template d apply_inlineable apply_method la ra
    and apply_method_only d = apply_template d (fun _ -> failwith "Inlineable not supported.") apply_method

    let apply d expr args = apply_both d expr (tev d args) (failwithf "Pattern matching cases failed to match.\n%A")

    let transpose l on_fail on_succ =
        let is_all_vv_empty x = List.forall (function VV [] -> true | _ -> false) x
        let rec loop acc_total acc_head acc_tail = function
            | VV [] :: ys -> 
                if List.isEmpty acc_head && is_all_vv_empty ys then 
                    if List.isEmpty acc_total then failwith "Empty inputs in the inner dimension to transpose are invalid."
                    else List.rev acc_total |> on_succ
                else on_fail ()
            | VV (x :: xs) :: ys -> loop acc_total (x :: acc_head) (VV xs :: acc_tail) ys
            | _ :: _ -> on_fail ()
            | [] -> 
                match acc_tail with
                | _ :: _ -> loop (VV (List.rev acc_head) :: acc_total) [] [] (List.rev acc_tail)
                | _ -> List.rev acc_total |> on_succ
        loop [] [] [] l

    let zip_template on_ireg l = 
        let rec zip l = 
            match l with
            | _ :: _ -> transpose l (fun _ -> on_ireg l) (List.map (function VV x -> zip x | x -> x)) |> VV
            | _ -> failwith "Empty input to zip is invalid."
        zip l

    let zip_reg_guard l =
        if List.forall (function VV _ -> false | _ -> true) l then l
        else failwith "Irregular inputs in zip."
    let zip_reg = zip_template id
    let zip_irreg = zip_template zip_reg_guard

    let rec unzip_template on_irreg l = 
        let is_all_vv x = List.forall (function VV _ -> true | _ -> false) x
        let rec unzip l =
            match l with
            | VV x ->
                match x with
                | _ :: _ when is_all_vv x -> let t = List.map (unzip >> VV) x in transpose t (fun _ -> on_irreg x) id
                | _ :: _ -> x
                | _ -> failwith "Empty inputs to unzip are invalid."
            | _ -> failwith "Unzip called on a non-VV."
        unzip l

    let unzip_reg = unzip_template zip_reg_guard
    let unzip_irreg = unzip_template id

    let zip_op d f a = 
        let rec zip_remap = function
            | TyVV(a,_) -> VV (List.map zip_remap a)
            | a -> T a

        tev d a |> destructure_deep d |> zip_remap |> f

    let vv_make vars name =
        let vv = List.map (tev d) vars
        match name with
        | null | "" -> TyVV(vv, List.map get_type vv |> VVT)
        | _ -> TyVV(vv, VVNamedT(List.map get_type vv, name))

    match exp with
    | TypeError er -> failwith er
    | T x -> x // To assist in CubBlockReduce so evaled cases do not have to be evaluated twice.
    | V' x -> TyV x // To assist in interfacing with the outside.
    | LitInt32 x -> TyLitInt32 x
    | LitInt64 x -> TyLitInt64 x
    | LitUInt32 x -> TyLitUInt32 x
    | LitUInt64 x -> TyLitUInt64 x
    | LitFloat32 x -> TyLitFloat32 x
    | LitFloat64 x -> TyLitFloat64 x
    | LitBool x -> TyLitBool x
    | V x -> 
        match Map.tryFind x d.env with
        | Some v -> v
        | None -> failwithf "Variable %A not bound." x
    | Inlineable l -> 
        let k = l, Map.map (fun _ -> get_type) d.env
        d.memoized_inlineable_envs.[k] <- d.env
        InlineableT k |> TyType
    | Method(name, l) -> 
        let k = (name, l), Map.map (fun _ -> get_type) d.env
        d.memoized_method_envs.[k] <- d.env
        MethodT k |> TyType
    | Apply(expr,args) -> apply d expr args id
    | If(cond,tr,fl) ->
        printfn "I am in If."
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

            printfn "Evaluating the true branch."
            let tr = tev' tr (fun _ -> 
                let fl = tev d fl
                fl_result <- Some fl
                fl)
            printfn "Done with the true branch."
            printfn "Evaluating the false branch."
            let fl = 
                match fl_result with
                | Some fl -> fl
                | None -> tev' fl (fun _ -> tr)
            printfn "Done with the false branch."

            let type_tr, type_fl = get_type tr, get_type fl
            if type_tr = type_fl then 
                //tev d (Apply(Method("",[E,T (TyIf(cond,tr,fl,type_tr))]),B))
                TyIf(cond,tr,fl,type_tr)
            else failwithf "Types in branches of If do not match.\nGot: %A and %A" type_tr type_fl
    | VVZipReg a -> zip_op d (function VV x -> zip_reg x |> tev d | x -> tev d x) a
    | VVZipIrreg a -> zip_op d (function VV x -> zip_irreg x |> tev d | x -> tev d x) a
    | VVUnzipReg a -> zip_op d (unzip_reg >> VV >> tev d) a
    | VVUnzipIrreg a -> zip_op d (unzip_irreg >> VV >> tev d) a
    | VV vars -> vv_make vars null
    | VVNamed (vars, name) -> vv_make vars name
    | VVIndex(v,i) ->
        match tev d v, tev d i with
        | v, (TyLitInt32 i as i') ->
            match get_type v with
            | VVT ts -> 
                if i >= 0 || i < List.length ts then TyVVIndex(v,i',ts.[i])
                else failwith "(i >= 0 || i < List.length ts) = false in IndexVT"
            | x -> failwithf "Type of a evaluated expression in IndexVT is not VTT.\nGot: %A" x
        | v, i ->
            failwithf "Index into a tuple must be a natural number less than the size of the tuple.\nGot: %A" i
    | VVCons(a,b) ->
        let a = tev d a
        let b = tev d b |> destructure_deep d
        match b with
        | TyVV(b, VVT bt) -> TyVV(a::b, VVT (get_type a :: bt))
        | _ -> failwith "Expected a tuple on the right is in VVCons."
    // Array cases
    | ArrayIndex(exp,args) ->
        let exp,args = tev d exp, tev d args
        match get_type exp, args with
        | (LocalArrayT(TyVV(vs,_), t) | SharedArrayT(TyVV(vs,_), t) | GlobalArrayT(TyVV(vs,_), t)), TyVV(args,_) when List.forall is_int args-> 
            if List.length vs = List.length args then TyArrayIndex(exp,args,t) else failwith "The index lengths in ArrayIndex do not match"
        | (LocalArrayT(TyVV(vs,_), t) | SharedArrayT(TyVV(vs,_), t) | GlobalArrayT(TyVV(vs,_), t)), args when is_int args -> 
            if List.isEmpty vs = false then TyArrayIndex(exp,[args],t) else failwith "The index lengths in ArrayIndex do not match"
        | _ -> failwithf "Something is wrong in ArrayIndex.\nexp=%A, args=%A" exp args
    | ArraySize(ar,ind) ->
        let ar, ind = tev d ar, tev d ind
        match get_type ar, ind with
        | (LocalArrayT(TyVV(vs,_), t) | SharedArrayT(TyVV(vs,_), t) | GlobalArrayT(TyVV(vs,_), t)), TyLitInt32 i ->
            if i < vs.Length then vs.[i] else failwith "The index into array exceeds the number of its dimensions."
        | _ -> failwithf "Something is wrong in ArraySize.\nar=%A, ind=%A" ar ind

    | ArrayCreateLocal(args,t) -> let args,t = process_create_array d args t in TyArrayCreate(LocalArrayT(args,t)) |> make_tyv_and_push d
    | ArrayCreateShared(args,t) -> let args,t = process_create_array d args t in TyArrayCreate(SharedArrayT(args,t)) |> make_tyv_and_push d

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
    | Add(a,b) -> prim_arith_op d a b TyAdd
    | Sub(a,b) -> prim_arith_op d a b TySub
    | Mult(a,b) -> prim_arith_op d a b TyMult
    | Div(a,b) -> prim_arith_op d a b TyDiv
    | Mod(a,b) -> prim_arith_op d a b TyMod
        
    | LT(a,b) -> prim_bool_op d a b TyLT
    | LTE(a,b) -> prim_bool_op d a b TyLTE
    | EQ(a,b) -> prim_bool_op d a b TyEQ
    | NEQ(a,b) -> prim_bool_op d a b TyNEQ
    | GT(a,b) -> prim_bool_op d a b TyGT
    | GTE(a,b) -> prim_bool_op d a b TyGTE
    | And(a,b) -> prim_bool_op d a b TyAnd
    | Or(a,b) -> prim_bool_op d a b TyOr

    | Syncthreads -> TySyncthreads

    | LeftShift(a,b) -> prim_shift_op d a b TyLeftShift
    | RightShift(a,b) -> prim_shift_op d a b TyRightShift
    
    | ShuffleXor(a,b) -> prim_shuffle_op d a b TyShuffleXor
    | ShuffleUp(a,b) -> prim_shuffle_op d a b TyShuffleUp
    | ShuffleDown(a,b) -> prim_shuffle_op d a b TyShuffleDown
    | ShuffleIndex(a,b) -> prim_shuffle_op d a b TyShuffleIndex

    | Log a -> prim_un_floating d a TyLog
    | Exp a -> prim_un_floating d a TyExp
    | Tanh a -> prim_un_floating d a TyTanh
    | Neg a -> prim_un_numeric d a TyNeg
    // Mutable operations.
    | MSet(a,b) ->
        let l = tev d a
        let r = destructure_deep d (tev d b)
        match l, r with
        | TyArrayIndex(_,_,lt), r when lt = get_type r -> push_sequence d (fun rest -> TyMSet(l,r,rest,get_type rest)); TyB
        | _ -> failwithf "Error in mset. Expected: TyArrayIndex(_,_,lt), r when lt = get_type r.\nGot: %A and %A" l r
    | AtomicAdd(a,b) -> prim_atomic_add_op d a b TyAtomicAdd
    | While(cond,body,e) ->
        let cond, body = tev d (Apply(Method("",[E,cond]),B)), with_empty_seq d body
        match get_type cond, get_type body with
        | PrimT BoolT, VVT [] -> push_sequence d (fun rest -> TyWhile(cond,body,rest,get_type rest)); tev d e
        | PrimT BoolT, _ -> failwith "Expected VVT [] as the type of While's body."
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
                let arg = ArrayIndex(T evaled_input,VV [LitInt32 0])
                tev d (VV [arg;arg])
            | x -> 
                let arg() = evaled_input |> make_tyv |> TyV
                tev d (VV [T (arg()); T (arg())])
            |> fun x -> apply_method_only d method_ x (failwithf "Failed to pattern match the method in CubBlockReduce.\n%A") id

        let num_valid = Option.map (tev d) num_valid
        TyCubBlockReduce(dim, evaled_input,method_,num_valid,get_type method_)
            
/// Propagates the free variables downwards.
/// The closures are stack allocated hence not allowed to return from functions.
let rec closure_conv (imemo: MethodImplDict) (memo: MethodDict) (exp: TypedCudaExpr) =
    let c x = closure_conv imemo memo x
    match exp with
    | TyType t -> Set.empty // TODO: When I go with full out type based metaprogramming, fix this so closure conv propagates variables through types as well.
    | TyV v -> Set.singleton v
    | TyIf(cond,tr,fl,t) -> c cond + c tr + c fl
    | TyLitUInt32 _ | TyLitUInt64 _ | TyLitInt32 _ | TyLitInt64 _ 
    | TyLitFloat32 _ | TyLitFloat64 _ | TyLitBool _ -> Set.empty
    | TyVV(vars,t) -> Set.unionMany (List.map c vars)
    | TyVVIndex(t,i,_) -> Set.union (c t) (c i)
    | TyMethodCall(m,ar,_) ->
        let method_implicit_args =
            match imemo.TryGetValue m with
            | true, (_,_,_,impl_args) -> impl_args
            | false, _ ->
                match memo.[m] with
                | MethodDone(sol_arg, body, tag) -> 
                    let impl_args = c body - Set(sol_arg)
                    imemo.Add(m,(sol_arg,body,tag,impl_args))
                    impl_args
                | _ -> failwith "impossible"
        Set.union method_implicit_args (c ar)
    | TyCubBlockReduce(_,inp,(TyMethodCall(key,_,_) as m),num_valid,t) ->
        ignore <| c m // This is so it gets added to the env.

        match imemo.[key] with
        | _,_,_,impl_args when impl_args.IsEmpty ->
            match num_valid with
            | Some num_valid -> Set.union (c num_valid) (c inp)
            | None -> c inp
        | impl_args -> 
            failwithf "The method passed to Cub should have no implicit arguments.\nm=%A\nimpl_args=%A" m impl_args
    | TyCubBlockReduce _ -> failwith "impossible"
    // Array cases
    | TyArrayIndex(a,b,_) -> 
        let a = 
            match get_type a with
            | LocalArrayT(TyVV(x,_),_) | SharedArrayT(TyVV(x,_),_) | GlobalArrayT(TyVV(x,_),_) ->
                let b = 
                    match x with
                    | [] -> []
                    | x -> List.choose (function TyV v -> Some v | _ -> None) (List.tail x)
                Set.union (c a) (Set(b))
            | _ -> failwith "impossible"
        Set.union a (Set.unionMany (List.map c b))
    | TyArrayCreate t -> 
        match t with
        | LocalArrayT(TyVV(x,_),_) | SharedArrayT(TyVV(x,_),_) | GlobalArrayT(TyVV(x,_),_) ->
            Set.unionMany (List.map c x)
        | _ -> failwith "impossible"
    // Cuda kernel constants
    | TyThreadIdxX | TyThreadIdxY | TyThreadIdxZ
    | TyBlockIdxX | TyBlockIdxY | TyBlockIdxZ -> Set.empty
    // Primitive operations on expressions.
    | TySyncthreads -> Set.empty
    | TyLog(a,_) | TyExp(a,_) | TyTanh(a,_) | TyNeg(a,_) -> c a
    | TyAdd(a,b,_) | TySub(a,b,_) | TyMult(a,b,_) | TyDiv(a,b,_) | TyMod(a,b,_)
    | TyLT(a,b) | TyLTE(a,b) | TyEQ(a,b) | TyNEQ(a,b) | TyGT(a,b) 
    | TyGTE(a,b) | TyAnd(a,b) | TyOr(a,b)
    | TyLeftShift(a,b,_) | TyRightShift(a,b,_) | TyShuffleXor(a,b,_)
    | TyShuffleUp(a,b,_) | TyShuffleDown(a,b,_) | TyShuffleIndex(a,b,_) 
    | TyAtomicAdd(a,b,_) -> Set.union (c a) (c b)
    | TyMSet(a, b, rest, _) | TyWhile(a, b, rest, _) -> c a + c b + c rest
    | TyLet(v, body, rest, _) -> c body + c rest |> Set.remove v

let typecheck dims body inputs = 
    try
        let main_method, memo = 
            let d = data_empty dims
            let body =
                (l (S "cuda") (cuda_library)
                (l (S "tuple") (tuple_library) body))
            exp_and_seq d (Apply(body,inputs)), d.memoized_methods
        let imemo = Dictionary(HashIdentity.Structural)
        closure_conv imemo memo main_method |> ignore
        Succ imemo
    with e -> Fail (e.Message, e.StackTrace)

/// Reasonable default for the dims.
let default_dims = dim3(256), dim3(20)

let typecheck0 program = typecheck default_dims program (VV [])
