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
    | InlineableT of CudaPattern * CudaExpr * Env
    | MethodT of name: string * args: CudaPattern * body: CudaExpr * Env

and TyV = int64 * CudaTy

// No return type polymorphism like in Haskell for now. Local type inference only.
and TyMethodKey = Env * CudaExpr * CudaTy // The key does not need to know the free variables.

and CudaPattern =
    | E // empty case
    | S of string
    // Tuple cases
    | SS of string list // A tuple of predetermined size.
    | R of CudaPattern list // The final variable of R gets bound to the cons of the list.

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
    | Inlineable of CudaPattern * CudaExpr
    | Method of name: string * args: CudaPattern * body: CudaExpr

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

and Env = Map<string, TypedCudaExpr>
// method key * method body * bound variables
and MethodCases =
    | MethodInEvaluation of CudaTy option
    | MethodDone of TyV list * TypedCudaExpr * int64
and MethodDict = Dictionary<TyMethodKey, MethodCases>
// method key * method body * implicit arguments
and MethodImplDict = Dictionary<TyMethodKey, TyV list * TypedCudaExpr * int64 * Set<TyV>>
and CudaTypecheckerEnv =
    {
    // Immutable
    env : Env
    // Mutable
    memoized_methods : MethodDict // For hoisted out global methods.
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

    let make_tyv_and_push ty_exp = make_tyv_and_push' ty_exp |> TyV

    let bind_template map_add name_checker acc arg_name ty_exp =
        dup_name_check name_checker arg_name <| fun _ ->
            map_add arg_name ty_exp acc

    let bind_inl = bind_template (fun arg_name x acc -> 
        match arg_name with
        | "" | "_" -> acc
        | _ -> Map.add arg_name x acc)

    let bind_method = bind_template (fun arg_name x acc -> 
        match arg_name with
        | "" | "_" -> x, acc
        | _ -> x, Map.add arg_name x acc)
    
    let traverse_inl t = List.fold2
    let traverse_method t f s a b = map_fold_2_Er f s a b |> fun (l,s) -> TyVV(l,t), s

    // for a shallow version, take a look at `alternative_destructure_v6e.fsx`. 
    // The deep version can also be straightforwardly derived from a template of this using the Y combinator.
    let rec destructure_deep_template nonseq_push r = 
        let destructure_deep r = destructure_deep_template nonseq_push r
        match r with
        | TyV _ | TyUnit | TyType _ -> r // Won't be passed into method as arguments apart from TyV.
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

    let rec match_vv (acc: Env) l r =
        let bind acc arg_name x = Map.add arg_name x acc
        let traverse = traverse_inl
        match l,r with // destructure_deep is called in apply_method and apply_inlineable
        | E, r -> bind acc "" r
        | S x, r -> bind acc x r
        | SS l, TyVV(r,t) -> traverse t match_vv acc (List.map S l) r
        | SS l, r -> failwithf "Cannot destructure %A." r
        | R ([l]), r -> match_vv acc l r
        | R (l :: ls), TyVV(r :: rs,VVT (t :: ts)) ->
            let x_acc = match_vv acc l r
            match_vv x_acc (R ls) (TyVV(rs,VVT ts))
        | R (l :: ls), r -> failwithf "Cannot destructure %A." r
        | R [], _ -> failwith "Empty R not allowed."

    let rec match_vv_method (acc: Env) l r =
        let match_vv = match_vv_method
        let bind acc arg_name x = x, Map.add arg_name x acc
        let traverse = traverse_method
        match l,r with // destructure_deep is called in apply_method and apply_inlineable
        | E, r -> bind acc "" r
        | S x, r -> bind acc x r
        | SS l, TyVV(r,t) -> traverse t match_vv acc (List.map S l) r
        | SS l, r -> failwithf "Cannot destructure %A." r
        | R ([l]), r -> match_vv acc l r
        | R (l :: ls), TyVV(r :: rs,VVT (t :: ts)) ->
            let x, x_acc = match_vv acc l r
            match match_vv x_acc (R ls) (TyVV(rs,VVT ts)) with
            | TyVV(xs,VVT ts), xs_acc -> TyVV(x :: xs, VVT <| t :: ts), xs_acc
            | _ -> failwith "impossible"
        | R (l :: ls), r -> failwithf "Cannot destructure %A." r
        | R [], _ -> failwith "Empty R not allowed."

    failwith "placeholder"

