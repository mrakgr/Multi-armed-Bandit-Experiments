module Spiral.Lang

open ManagedCuda.VectorTypes
open System.Collections.Generic

/// The dynamic device variable type.
type PrimitiveType =
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
    | StringT
    | CharT

type ArrayType =
| DotNetHeap
| DotNetStack
| DotNetReference
| CudaGlobal
| CudaShared
| CudaLocal
| CudaReference

type Ty =
    | PrimT of PrimitiveType
    | VVT of Ty list
    | LitT of Value
    | FunctionT of EnvTy * FunctionCore // Type level function. Can also be though of as a procedural macro.
    | RecFunctionT of EnvTy * FunctionCore * string 
    | ModuleT of EnvTy
    | UnionT of Set<Ty>
    | RecT of Tag
    | TypeConstructorT of Ty
    | ClosureT of Ty * Ty
    | ArrayT of ArrayType * Ty
    | ForCastT of Ty // For casting type level function to term (ClosureT) level ones.
    | DotNetTypeRuntimeT of Tag // Since Type does not support the Comparable interface, I map it to int.
    | DotNetTypeInstanceT of Tag
    | DotNetAssemblyT of Tag

and Tag = int64
and TyTag = Tag * Ty
and EnvTerm = Map<string, TypedExpr>
and EnvTy = Map<string, Ty>
and FunctionCore = string * Expr
and MemoKey = EnvTerm * Expr

and Value = 
    | LitUInt8 of uint8
    | LitUInt16 of uint16
    | LitUInt32 of uint32
    | LitUInt64 of uint64
    | LitInt8 of int8
    | LitInt16 of int16
    | LitInt32 of int32
    | LitInt64 of int64
    | LitFloat32 of float32
    | LitFloat64 of float
    | LitBool of bool
    | LitString of string
    | LitChar of char

and Op =
    // DotNetOps
    | DotNetLoadAssembly
    | DotNetTypeConstruct
    | DotNetTypeCallMethod

    // Case
    | Case

    // TriOps
    | If
    | IfStatic

    // BinOps
    | Add
    | Sub
    | Mult 
    | Div 
    | Mod 
    | LTE 
    | LT 
    | EQ 
    | NEQ 
    | GT 
    | GTE 
    | And 
    | Or 

    | Fix
    | Apply
    | ForCast
    | MethodMemoize
    | StructCreate
    | VVIndex
    | VVSliceFrom
    | VVCons
    | VVLength
    | VVIs
    | TypeAnnot
    | ModuleWith
    | ModuleWithExtend
    | EnvUnseal
    | TypeConstructorCreate
    | TypeConstructorUnion
    | EqType

    | ArrayCreate
    | ReferenceCreate
    | ArrayIndex
    | ArraySet
   
    | ShiftLeft
    | ShiftRight
    | ShuffleXor
    | ShuffleUp
    | ShuffleDown
    | ShuffleIndex

    // Static unary operations
    | PrintStatic
    | ErrorNonUnit
    | ErrorType
    | ModuleOpen
    | TypeLitCreate
    | Dynamize

    // UnOps
    | Neg
    | Log
    | Exp
    | Tanh

    // Constants
    | ModuleCreate

    | Syncthreads
    | BlockDimX | BlockDimY | BlockDimZ
    | GridDimX | GridDimY | GridDimZ

and Pattern =
    | E
    | PatVar of string
    | PatTuple of Pattern list
    | PatCons of Pattern list
    | PatType of Pattern * Pattern
    | PatActive of string * Pattern
    | PatOr of Pattern list
    | PatAnd of Pattern list
    | PatClauses of (Pattern * Expr) list
    | PatTypeName of string
    | PatPos of PosKey * Pattern

and PosKey = string * int64 * int64

and Expr = 
    | V of string
    | Lit of Value
    | Pattern of Pattern
    | Function of FunctionCore
    | FunctionFilt of Set<string> * FunctionCore
    | VV of Expr list
    | Op of Op * Expr list
    | Pos of PosKey * Expr

and Arguments = Set<TyTag> ref
and Renamer = Map<Tag,Tag>

and MemoExprType =
| MemoClosure of Set<TyTag>
| MemoMethod

and LetType =
| LetStd
| LetInvisible

and TypedExpr =
    | TyTag of TyTag
    | TyV of TypedExpr * Ty
    | TyLet of LetType * TyTag * TypedExpr * TypedExpr * Ty
    | TyLit of Value
    
    | TyVV of TypedExpr list * Ty
    | TyEnv of EnvTerm * Ty
    | TyOp of Op * TypedExpr list * Ty
    | TyMemoizedExpr of MemoExprType * Arguments * Renamer * Tag * Ty

and MemoCases =
    | MemoMethodInEvaluation of Tag
    | MemoMethodDone of MemoExprType * TypedExpr * Tag * Arguments
    | MemoTypeInEvaluation
    | MemoType of Ty

// This key is for functions without arguments. It is intended that the arguments be passed in through the Environment.
and MemoDict = Dictionary<MemoKey, MemoCases>
and ClosureDict = Dictionary<Tag, TypedExpr> 
// For Common Subexpression Elimination. I need it not for its own sake, but to enable other PE based optimizations.
and CSEDict = Map<TypedExpr,TypedExpr> ref

type Result<'a,'b> = Succ of 'a | Fail of 'b

let flip f a b = f b a

let get_type_of_value = function
    | LitUInt8 _ -> PrimT UInt8T
    | LitUInt16 _ -> PrimT UInt16T
    | LitUInt32 _ -> PrimT UInt32T
    | LitUInt64 _ -> PrimT UInt64T
    | LitInt8 _ -> PrimT Int8T
    | LitInt16 _ -> PrimT Int16T
    | LitInt32 _ -> PrimT Int32T
    | LitInt64 _ -> PrimT Int64T
    | LitFloat32 _ -> PrimT Float32T
    | LitFloat64 _ -> PrimT Float64T   
    | LitBool _ -> PrimT BoolT
    | LitString _ -> PrimT StringT
    | LitChar _ -> PrimT CharT

let get_type = function
    | TyLit x -> get_type_of_value x
    | TyTag(_,t) | TyV (_,t) | TyLet(_,_,_,_,t) | TyMemoizedExpr(_,_,_,_,t)
    | TyVV(_,t) | TyEnv(_,t) | TyOp(_,_,t) -> t

let get_subtype x = 
    match get_type x with
    | ForCastT x -> x
    | x -> x

/// Wraps the argument in a list if not a tuple.
let tuple_field = function 
    | TyVV(args,_) -> args
    | x -> [x]

let (|TyTuple|) x = tuple_field x

/// Wraps the argument in a set if not a UnionT.
let set_field = function
    | UnionT t -> t
    | t -> Set.singleton t

/// Wraps the argument in a list if not a tuple type.
let tuple_field_ty = function 
    | VVT x -> x
    | x -> [x]

let (|TyType|) x = get_type x
let (|TypeLit|_|) = function
    | TyType (LitT x) -> Some x
    | _ -> None
let (|TypeString|_|) = function
    | TypeLit (LitString x) -> Some x
    | _ -> None

let rec is_returnable' _ = true
let is_returnable a = is_returnable' (get_type a)

let is_numeric' = function
    | PrimT (UInt8T | UInt16T | UInt32T | UInt64T 
        | Int8T | Int16T | Int32T | Int64T 
        | Float32T | Float64T) -> true
    | _ -> false
let is_numeric a = is_numeric' (get_type a)

let is_primt' = function
    | PrimT x -> true
    | _ -> false
let is_primt a = is_primt' (get_type a)

let is_float' = function
    | PrimT (Float32T | Float64T) -> true
    | _ -> false
let is_float a = is_float' (get_type a)

let rec is_bool' = function
    | PrimT BoolT -> true
    | _ -> false
let is_bool a = is_bool' (get_type a)

let rec is_int' = function
    | PrimT (UInt32T | UInt64T | Int32T | Int64T) -> true
    | _ -> false
let is_int a = is_int' (get_type a)

let rec is_int64' = function
    | PrimT Int64T -> true
    | _ -> false
let is_int64 a = is_int64' (get_type a)

let h0() = HashSet(HashIdentity.Structural)
let d0() = Dictionary(HashIdentity.Structural)

let lit_int i = Lit (LitInt32 i)
let lit_string x = Lit (LitString x)
let fix name x =
    match name with
    | "" -> x
    | _ -> Op(Fix,[lit_string name; x])
let inl x y = Function(x,y)
let inl_pat x y = Pattern(PatClauses([x,y]))
let ap x y = Op(Apply,[x;y])
let for_cast x = Op(ForCast,[x])
let lp v b e = ap (inl_pat v e) b
let l v b e = ap (inl v e) b
let l_rec v b e = ap (inl v e) (fix v b)

let inl' args body = List.foldBack inl args body
    
let meth_memo y = Op(MethodMemoize,[y])
let meth x y = inl x (meth_memo y)

let module_create l = Op(ModuleCreate,[l])
let module_open a b = Op(ModuleOpen,[a;b])

let B = VV ([])
let BVVT = VVT []
let TyB = TyVV ([], BVVT)

let v x = V(x)
let cons a b = Op(VVCons,[a;b])

let s l fin = List.foldBack (fun x rest -> x rest) l fin

let rec ap' f l = List.fold ap f l

let vv x = VV(x)
let tuple_index v i = Op(VVIndex,[v; lit_int i])
let tuple_length v = Op(VVLength,[v])
let tuple_slice_from v i = Op(VVSliceFrom,[v; lit_int i])
let tuple_is v = Op(VVIs,[v])

let error_type x = Op(ErrorType, [x])
let print_static x = Op(PrintStatic,[x])
let dynamize x = Op(Dynamize,[x])

let if_static cond tr fl = Op(IfStatic,[cond;tr;fl])
let case arg case = Op(Case,[arg;case])
let private binop op a b = Op(op,[a;b])
let eq_type a b = binop EqType a b
let eq a b = binop EQ a b
let lt a b = binop LT a b

let error_non_unit x = Op(ErrorNonUnit, [x])
let type_lit_create x = Op(TypeLitCreate,[Lit x])
let pos pos x = Pos(pos,x)

let get_tag =
    let mutable i = 0
    fun () -> i <- i+1; i

let rec pattern_compile arg pat =
    let rec pattern_compile flag_is_var_type arg pat (on_succ: Lazy<_>) (on_fail: Lazy<_>) =
        let inline cp' arg pat on_succ on_fail = pattern_compile flag_is_var_type arg pat on_succ on_fail
        let inline cp arg pat on_succ on_fail = lazy cp' arg pat on_succ on_fail

        let pat_foldbacki f s l =
            let mutable len = 0
            let rec loop i l =
                match l with
                | x :: xs -> f (x,i) (loop (i+1) xs)
                | [] -> len <- i; s
            loop 0 l, len
            
        let pat_tuple l =
            pat_foldbacki
                (fun (pat,i) on_succ ->
                    let arg = tuple_index arg i
                    cp arg pat on_succ on_fail)
                on_succ
                l
            |> fun (on_succ,len) -> 
                if_static (eq (tuple_length arg) (lit_int len)) on_succ.Value on_fail.Value
                |> fun on_succ -> if_static (tuple_is arg) on_succ on_fail.Value
                |> case arg

        let pat_cons l = 
            pat_foldbacki
                (fun (pat,i) (on_succ, tuple_index') ->
                    let arg = tuple_index' arg i
                    cp arg pat on_succ on_fail, tuple_index)
                (on_succ, tuple_slice_from)
                l
            |> fun ((on_succ,_),len) -> 
                if_static (lt (lit_int len) (tuple_length arg)) on_succ.Value on_fail.Value
                |> fun on_succ -> if_static (tuple_is arg) on_succ on_fail.Value
                |> case arg

        let inline force (x: Lazy<_>) = x.Value

        match pat with
        | E -> on_succ.Value
        | PatVar x -> 
            if flag_is_var_type then if_static (eq_type arg (V x)) on_succ.Value on_fail.Value
            else l x arg on_succ.Value
        | PatTuple l -> pat_tuple l
        | PatCons l -> pat_cons l
        | PatType (exp,typ) ->
            let on_succ = cp arg exp on_succ on_fail
            pattern_compile true arg typ on_succ on_fail
            |> case arg
        | PatActive (a,b) ->
            let pat_var = sprintf " pat_var_%i" (get_tag())
            l pat_var (ap (V a) arg) (cp' (v pat_var) b on_succ on_fail)
        | PatOr l -> List.foldBack (fun pat on_fail -> cp arg pat on_succ on_fail) l on_fail |> force
        | PatAnd l -> List.foldBack (fun pat on_succ -> cp arg pat on_succ on_fail) l on_succ |> force
        | PatClauses l -> List.foldBack (fun (pat, exp) on_fail -> cp arg pat (lazy (expr_prepass exp |> snd)) on_fail) l on_fail |> force
        | PatTypeName x ->
            let x = type_lit_create (LitString x)
            if_static (eq_type arg x) on_succ.Value on_fail.Value |> case arg
        | PatPos (p, pat) -> pos p (cp' arg pat on_succ on_fail)

    let pattern_compile_def_on_succ = lazy failwith "Missing a clause."
    let pattern_compile_def_on_fail = lazy error_type (Lit(LitString <| "Pattern matching cases are inexhaustive."))
    pattern_compile false arg pat pattern_compile_def_on_succ pattern_compile_def_on_fail

and pattern_compile_single pat =
    let main_arg = " main_arg"
    inl main_arg (pattern_compile (V main_arg) pat) |> expr_prepass

and expr_prepass e =
    let f e = expr_prepass e
    match e with
    | V n -> Set.singleton n, e
    | Op(op,l) ->
        let l,l' = List.map f l |> List.unzip
        Set.unionMany l, Op(op,l')
    | VV l -> 
        let l,l' = List.map f l |> List.unzip
        Set.unionMany l, VV l'
    | FunctionFilt(vars,(name,body)) ->
        Set.remove name vars, e
    | Function(name,body) ->
        let vars,body = f body
        Set.remove name vars, FunctionFilt(vars,(name,body))
    | Lit _ -> Set.empty, e
    | Pos(pos,body) -> 
        let vars, body = f body
        vars, Pos(pos,body)
    | Pattern pat -> pattern_compile_single pat


let renamer_make s = Set.fold (fun (s,i) (tag,ty) -> Map.add tag i s, i+1L) (Map.empty,0L) s |> fst
let renamer_apply_pool r s = Set.map (fun (tag,ty) -> Map.find tag r, ty) s
let renamer_apply_renamer r m = Map.map (fun _ v -> Map.find v r) m

let renamer_reverse r = 
    Map.fold (fun s k v -> Map.add v k s) Map.empty r
    |> fun x -> if r.Count <> x.Count then failwith "The renamer is not bijective." else x

let rec renamer_apply_env r e = Map.map (fun _ v -> renamer_apply_typedexpr r v) e
and renamer_apply_typedexpr r e =
    let f e = renamer_apply_typedexpr r e
    match e with
    | TyTag (n,t) -> TyTag (Map.find n r,t)
    | TyV (n,t) -> TyV(f n,t)
    | TyLit _ -> e
    | TyVV(l,t) -> TyVV(List.map f l,t)
    | TyEnv(l,t) -> TyEnv(renamer_apply_env r l, t)
    | TyMemoizedExpr(typ,used_vars,renamer,tag,t) -> 
        let renamer = renamer_apply_renamer r renamer
        let used_vars = ref <| renamer_apply_pool r !used_vars
        TyMemoizedExpr(typ,used_vars,renamer,tag,t)
    | TyOp(o,l,t) -> TyOp(o,List.map f l,t)
    | TyLet(le,(n,t),a,b,t') -> TyLet(le,(Map.find n r,t),f a,f b,t')

let inline vars_union' init f l = List.fold (fun s x -> Set.union s (f x)) init l
let inline vars_union f l = vars_union' Set.empty f l

let rec typed_expr_free_variables_template on_memo e =
    let inline f e = typed_expr_free_variables_template on_memo e
    match e with
    | TyV (n,t) -> f n
    | TyTag (n,t) -> Set.singleton (n, t)
    | TyLit _ -> Set.empty
    | TyVV(l,_) | TyOp(_,l,_) -> vars_union f l
    | TyEnv(l,_) -> env_free_variables_template on_memo l
    | TyMemoizedExpr(typ,used_vars,renamer,tag,ty) -> on_memo (typ,used_vars,renamer,tag)
    // Note, this is different from `Set.remove x (f b) + f a` because let statements are also used to instantiate a variable to themselves.
    // For example `let x = x`. In the typed language that is being compiled to, I want the x's tag to be blocked from being propagated.
    | TyLet(_,x,a,b,_) -> Set.remove x (f b + f a)

and env_free_variables_template on_memo env = 
    Map.fold (fun s _ v -> typed_expr_free_variables_template on_memo v + s) Set.empty env

let private typed_expr_std_pass (typ,used_vars,renamer,tag) = !used_vars
let rec typed_expr_free_variables e = typed_expr_free_variables_template typed_expr_std_pass e
and env_free_variables env = env_free_variables_template typed_expr_std_pass env

/// Optimizes the free variables for the sake of tuple deforestation.
/// It needs at least two passes to converge properly. And probably exactly two.
let typed_expr_optimization_pass num_passes (memo: MemoDict) typed_exp =
    let rec on_method_call_optimization_pass (memo: (Set<Tag * Ty> * int) []) (expr_map: Map<Tag,TypedExpr * Arguments>) (_, r, renamer, tag) =
        let vars,counter = memo.[int tag]
        let set_vars vars = renamer_apply_pool renamer vars |> fun x -> r := x; x
        if counter < num_passes then
            let counter = counter + 1
            memo.[int tag] <- vars, counter
            let ty_expr, arguments = expr_map.[tag]
            let vars = typed_expr_free_variables_template (on_method_call_optimization_pass memo expr_map) ty_expr
            arguments := vars
            memo.[int tag] <- vars, counter
            set_vars vars
        else
            set_vars vars

    let memo = 
        Seq.choose (function
            | MemoMethodDone (_, e, tag, args) -> Some (tag,(e,args))
            | MemoType t -> None
            | _ -> failwith "impossible") memo.Values |> Map

    typed_expr_free_variables_template (on_method_call_optimization_pass (Array.init memo.Count (fun _ -> Set.empty,0)) memo) typed_exp
    |> ignore

let env_to_ty env = Map.map (fun _ -> get_type) env
let env_num_args env = 
    Map.fold (fun s k v -> 
        let f = typed_expr_free_variables v
        if Set.isEmpty f then s else s+1) 0 env

let map_dotnet (d: Dictionary<_,Tag>, dr: Dictionary<Tag,_>) x =
    match d.TryGetValue x with
    | true, x -> x
    | false, _ ->
        let v = d.Count |> int64
        d.Add(x,v); dr.Add(v,x)
        v

let map_rev_dotnet (d: Dictionary<_,Tag>, dr: Dictionary<Tag,_>) x = dr.[x]

let rec dotnet_type_to_ty memoized_dotnet_types (x: System.Type) =
    if x = typeof<int8> then PrimT Int8T
    elif x = typeof<int16> then PrimT Int16T
    elif x = typeof<int32> then PrimT Int32T
    elif x = typeof<int64> then PrimT Int64T

    elif x = typeof<uint8> then PrimT UInt8T
    elif x = typeof<uint16> then PrimT UInt16T
    elif x = typeof<uint32> then PrimT UInt32T
    elif x = typeof<uint64> then PrimT UInt64T

    elif x = typeof<float32> then PrimT Float32T
    elif x = typeof<float> then PrimT Float64T
    elif x = typeof<string> then PrimT StringT
    elif x = typeof<unit> then BVVT
    elif x.IsArray then ArrayT(DotNetHeap,dotnet_type_to_ty memoized_dotnet_types (x.GetElementType()))
    // Note: The F# compiler doing implicit conversions on refs really screws with me here. I won't bother trying to make this sound.
    elif x.IsByRef then ArrayT(DotNetReference, dotnet_type_to_ty memoized_dotnet_types (x.GetElementType())) // Incorrect, but useful
    else map_dotnet memoized_dotnet_types x |> DotNetTypeRuntimeT

let rec dotnet_ty_to_type memoized_dotnet_types (x: Ty) =
    match x with
    | PrimT Int8T -> typeof<int8>
    | PrimT Int16T -> typeof<int16>
    | PrimT Int32T -> typeof<int32>
    | PrimT Int64T -> typeof<int64>

    | PrimT UInt8T -> typeof<uint8>
    | PrimT UInt16T -> typeof<uint16>
    | PrimT UInt32T -> typeof<uint32>
    | PrimT UInt64T -> typeof<uint64>

    | PrimT Float32T -> typeof<float32>
    | PrimT Float64T -> typeof<float>
    | PrimT StringT -> typeof<string>
    | ArrayT(DotNetHeap,t) -> (dotnet_ty_to_type memoized_dotnet_types t).MakeArrayType()
    | ArrayT(DotNetReference,t) -> (dotnet_ty_to_type memoized_dotnet_types t).MakeByRefType() // Incorrect, but useful
    | DotNetTypeInstanceT x | DotNetTypeRuntimeT x -> map_rev_dotnet memoized_dotnet_types x
    | _ -> failwithf "Type %A not supported for conversion into .NET SystemType." x

type Trace = PosKey list

type RecursiveBehavior =
| AnnotationDive
| AnnotationReturn

type LangEnv =
    {
    rbeh: RecursiveBehavior
    ltag : int64 ref
    seq : (TypedExpr -> TypedExpr) ref
    env : EnvTerm
    cse_env : CSEDict
    trace : Trace
    }

type LangGlobals =
    {
    method_tag: Tag ref
    memoized_methods: MemoDict
    type_tag: Tag ref
    memoized_types: Dictionary<Tag,Ty>
    memoized_dotnet_assemblies: Dictionary<System.Reflection.Assembly,Tag> * Dictionary<Tag,System.Reflection.Assembly>
    memoized_dotnet_types: Dictionary<System.Type,Tag> * Dictionary<Tag,System.Type>
    }

let (|TyEnvT|_|) = function
    | ModuleT env | RecFunctionT (env,_,_) | FunctionT(env,_) -> Some env
    | _ -> None

let is_all_int64 size = List.forall is_int64 (tuple_field size)
let (|TyArray|_|) = function
    | TyTuple [size; ar & TyType (ArrayT (ar_type,ret_type))] when is_all_int64 size -> Some (size,ar,ar_type,ret_type)
    | _ -> None

exception TypeError of Trace * string

let rec expr_typecheck (globals: LangGlobals) (d : LangEnv) (expr: Expr) =
    let inline tev d expr = expr_typecheck globals d expr
    let inline apply_seq d x = !d.seq x
    let inline tev_seq d expr = let d = {d with seq=ref id; cse_env=ref !d.cse_env} in tev d expr |> apply_seq d
    let inline tev_assume cse_env d expr = let d = {d with seq=ref id; cse_env=ref cse_env} in tev d expr |> apply_seq d
    let inline tev_method d expr = let d = {d with seq=ref id; cse_env=ref Map.empty} in tev d expr |> apply_seq d
    let inline tev_rec d expr = tev_method {d with rbeh=AnnotationReturn} expr
    let on_type_er trace message = TypeError(trace,message) |> raise

    let tev2 d a b = tev d a, tev d b
    let tev3 d a b c = tev d a, tev d b, tev d c

    let v_find env x on_fail = 
        match Map.tryFind x env with
        | Some v -> v
        | None -> on_fail()

    let get_tag d = 
        let t = !d.ltag
        d.ltag := t + 1L
        t

    let inline make_tyv_ty d ty = get_tag d, ty
    let inline make_tyv_typed_expr d ty_exp = make_tyv_ty d (get_type ty_exp)

    let make_tyv_and_push_typed_expr d ty_exp =
        let v = make_tyv_typed_expr d ty_exp
        let seq = !d.seq
        d.seq := fun rest -> TyLet(LetStd,v,ty_exp,rest,get_type rest) |> seq
        TyTag v

    let make_tyv_and_push_ty d ty =
        let v = make_tyv_ty d ty
        let v' = TyTag v
        let seq = !d.seq
        d.seq := fun rest -> TyLet(LetInvisible,v,v',rest,get_type rest) |> seq
        v'

    let cse_add' d r x = let e = !d.cse_env in if r <> x then Map.add r x e else e
    let cse_add d r x = d.cse_env := cse_add' d r x

    // for a shallow version, take a look at `alternative_destructure_v6e.fsx`.
    // The deep version can also be straightforwardly derived from a template of this using the Y combinator.
    let rec destructure d r = 
        let inline destructure r = destructure d r

        let chase_cse on_succ on_fail r = 
            match Map.tryFind r !d.cse_env with
            | Some x -> on_succ x
            | None -> on_fail r
        let chase_recurse r = chase_cse destructure id r

        let destructure_var r =
            let index_tuple_args tuple_types = 
                List.mapi (fun i typ -> 
                    destructure <| TyOp(VVIndex,[r;TyLit <| LitInt32 i],typ)) tuple_types
            let env_unseal x =
                let unseal k v = destructure <| TyOp(EnvUnseal,[r; TyLit (LitString k)], v)
                Map.map unseal x
            let r_ty = get_type r
            match r_ty with
            | VVT tuple_types -> TyVV(index_tuple_args tuple_types, r_ty)
            | TyEnvT env -> TyEnv(env_unseal env, r_ty)
            | _ -> chase_recurse r
           
        let destructure_cse r = 
            chase_cse 
                chase_recurse
                (fun r ->
                    let x = make_tyv_and_push_typed_expr d r
                    cse_add d r x
                    x)
                r
            
        match r with
        | TyLit _ -> r
        | TyTag _ | TyV _ -> destructure_var r
        | TyVV(l,ty) -> TyVV(List.map destructure l, ty)
        | TyEnv(l,ty) -> TyEnv(Map.map (fun _ -> destructure) l, ty)
        | TyMemoizedExpr _ | TyLet _ | TyOp _ -> destructure_cse r

    let if_is_returnable (TyType r & x) =
        if is_returnable' r then x
        else on_type_er d.trace <| sprintf "The following is not a type that can be returned from a if statement. Got: %A" r

    let if_body d cond tr fl =
        let b x = (cse_add' d cond (TyLit(LitBool x)))
        let tr = tev_assume (b true) d tr
        let fl = tev_assume (b false) d fl
        let type_tr, type_fl = get_type tr, get_type fl
        if type_tr = type_fl then
            match cond with
            | TyLit(LitBool true) -> tr
            | TyLit(LitBool false) -> fl
            | _ -> TyOp(If,[cond;tr;fl],type_tr)
            |> if_is_returnable
        else on_type_er d.trace <| sprintf "Types in branches of If do not match.\nGot: %A and %A" type_tr type_fl

    let if_cond d tr fl cond =
        if is_bool cond = false then on_type_er d.trace <| sprintf "Expected a bool in conditional.\nGot: %A" (get_type cond)
        else if_body d cond tr fl

    let if_static d cond tr fl =
        match tev d cond with
        | TyLit (LitBool cond) -> 
            let branch = if cond then tr else fl
            tev d branch |> if_is_returnable
        | cond -> if_cond d tr fl cond

    let if_ d cond tr fl = tev d cond |> if_cond d tr fl

    let tag r =
        let tag = !r
        r := tag + 1L
        tag

    let method_tag () = tag globals.method_tag
    let type_tag () = tag globals.type_tag

    let eval_method memo_type used_vars d expr =
        let key_args = d.env, expr
        let memoized_methods = globals.memoized_methods

        match memoized_methods.TryGetValue key_args with
        | false, _ ->
            let tag = method_tag ()

            memoized_methods.[key_args] <- MemoMethodInEvaluation tag
            let typed_expr = tev_method d expr
            memoized_methods.[key_args] <- MemoMethodDone (memo_type, typed_expr, tag, used_vars)
            typed_expr, tag
        | true, MemoMethodInEvaluation tag -> 
            tev_rec d expr, tag
        | true, MemoMethodDone (memo_type, typed_expr, tag, used_vars) -> 
            typed_expr, tag
        | true, (MemoTypeInEvaluation | MemoType _) ->
            failwith "Expected a method, not a recursive type."

    let eval_renaming memo_type d expr =
        let env = d.env
        let fv = env_free_variables env
        let renamer = renamer_make fv
        let renamed_env = renamer_apply_env renamer env

        let memo_type = memo_type renamer
        let typed_expr, tag = eval_method memo_type (renamer_apply_pool renamer fv |> ref) {d with env=renamed_env; ltag=ref <| int64 renamer.Count} expr
        let typed_expr_ty = get_type typed_expr
        if is_returnable' typed_expr_ty = false then on_type_er d.trace <| sprintf "The following is not a type that can be returned from a method. Consider using Inlineable instead. Got: %A" typed_expr
        else memo_type, ref fv, renamer_reverse renamer, tag, typed_expr_ty

    let inline memoize_helper memo_type k d x = eval_renaming memo_type d x |> k |> make_tyv_and_push_typed_expr d
    let memoize_method d x = 
        let memo_type _ = MemoMethod
        memoize_helper memo_type (fun (memo_type,args,rev_renamer,tag,ret_ty) -> 
            TyMemoizedExpr(memo_type,args,rev_renamer,tag,ret_ty)) d x

    let memoize_closure arg d x =
        let fv, arg_ty = typed_expr_free_variables arg, get_type arg
        let memo_type r = MemoClosure (renamer_apply_pool r fv)
        memoize_helper memo_type (fun (memo_type,args,rev_renamer,tag,ret_ty) -> 
            TyMemoizedExpr(memo_type,args,rev_renamer,tag,ClosureT(arg_ty,ret_ty))) d x

    let apply_typec d typec ra =
        let rec apply_typec d ty ra on_fail ret =
            let substitute_ty = function 
                | TyVV(l,_) -> TyVV(l,ty) |> ret
                | x when get_type x <> ty -> TyV(x,ty) |> ret
                | x -> ret x
            match ty, ra with
            | TypeConstructorT _, _ -> failwith "Type constructors should never be nested."
            | x, TyType r when x = r -> ret ra
            | x, TyV(n,_) -> apply_typec d x n on_fail ret
            | RecT tag, _ -> apply_typec d globals.memoized_types.[tag] ra on_fail substitute_ty
            | UnionT tys, _ ->
                let rec loop = function
                    | x :: xs -> apply_typec d x ra (fun _ -> loop xs) substitute_ty
                    | [] -> on_fail()
                loop (Set.toList tys)
            | VVT t, TyVV(l,_) ->
                let rec loop ret = function
                    | x :: xs, r :: rs ->
                        apply_typec d x r on_fail <| fun v ->
                            loop (fun vs -> ret (v :: vs)) (xs, rs)
                    | [], [] -> ret []
                    | _ -> on_fail()
                loop (fun l -> TyVV(l,ty)) (t,l)
            | _ -> on_fail()

        let er _ = on_type_er d.trace <| sprintf "Type constructor application failed. %A does not intersect %A." typec (get_type ra)
        apply_typec d typec ra er id
            
    let typec_union d a b =
        let a, b = tev2 d a b
        match get_type a, get_type b with
        | TypeConstructorT a, TypeConstructorT b -> set_field a + set_field b |> UnionT |> TypeConstructorT |> make_tyv_and_push_ty d
        | a, b -> on_type_er d.trace <| sprintf "In type constructor union expected both types to be type constructors. Got: %A and %A" a b

    let rec strip_map x = Map.map (fun _ -> typec_strip) x
    and typec_strip = function // Implements #10.
        | DotNetAssemblyT _ | DotNetTypeRuntimeT _ | DotNetTypeInstanceT _
        | PrimT _ | LitT _ | RecT _ as x -> x
        | TypeConstructorT x -> x
        | VVT l -> VVT (List.map typec_strip l)
        | FunctionT (e, b) -> FunctionT(strip_map e, b)
        | RecFunctionT (e, b, name) -> RecFunctionT (strip_map e, b, name)
        | ModuleT e -> ModuleT (strip_map e)
        | ForCastT x -> typec_strip x |> ForCastT
        | ClosureT (a,b) -> ClosureT (typec_strip a, typec_strip b)
        | UnionT s -> UnionT (Set.map typec_strip s)
        | ArrayT (a,b) -> ArrayT (a, typec_strip b)

    let typec_create d x =
        let key = d.env, x
        let ret_tyv x = TypeConstructorT x |> make_tyv_and_push_ty d

        let add_to_memo_dict x = 
            globals.memoized_methods.[key] <- MemoType x
            x

        let add_to_type_dict x =
            match globals.memoized_methods.TryGetValue key with
            | true, MemoType (RecT tag) -> globals.memoized_types.[tag] <- x; RecT tag
            | _ -> x

        match globals.memoized_methods.TryGetValue key with
        | true, MemoType ty -> ret_tyv ty
        | true, MemoTypeInEvaluation -> type_tag() |> RecT |> add_to_memo_dict |> ret_tyv
        | true, _ -> failwith "Expected a type in the dictionary."
        | false, _ -> 
            globals.memoized_methods.[key] <- MemoTypeInEvaluation
            // After the evaluation, if the type is recursive the dictionary should have its key.
            // If present it will return that instead.
            tev_seq d x |> get_type |> typec_strip |> add_to_type_dict |> add_to_memo_dict |> ret_tyv

    let inline wrap_exception d f =
        try f()
        with 
        | :? TypeError as e -> reraise()
        | e -> on_type_er d.trace e.Message

    let dotnet_load_assembly d x =
        match tev d x with
        | TypeString x ->
            wrap_exception d <| fun _ ->
                System.Reflection.Assembly.Load(x) |> map_dotnet globals.memoized_dotnet_assemblies 
                |> DotNetAssemblyT |> make_tyv_and_push_ty d
        | _ -> on_type_er d.trace "Expected a type level string."

    let (|TyAssembly|_|) = function
        | TyType (DotNetAssemblyT x) -> map_rev_dotnet globals.memoized_dotnet_assemblies x |> Some
        | _ -> None

    let (|TyDotNetTypeRuntime|_|) = function
        | TyType (DotNetTypeRuntimeT x) -> map_rev_dotnet globals.memoized_dotnet_types x |> Some
        | _ -> None

    let (|TyDotNetTypeInstance|_|) = function
        | TyType (DotNetTypeInstanceT x) -> map_rev_dotnet globals.memoized_dotnet_types x |> Some
        | _ -> None

    let (|TyDotNetType|_|) = function
        | TyType (DotNetTypeRuntimeT x | DotNetTypeInstanceT x) -> map_rev_dotnet globals.memoized_dotnet_types x |> Some
        | _ -> None

    let dotnet_type_to_ty (x: System.Type) = dotnet_type_to_ty globals.memoized_dotnet_types x
    let dotnet_ty_to_type (x: Ty) = dotnet_ty_to_type globals.memoized_dotnet_types x

    let (|TySystemTypeArgs|) args = 
        let typec_strip = function TypeConstructorT x -> x | x -> x
        List.toArray args |> Array.map (get_type >> typec_strip >> dotnet_ty_to_type)

    let array_index' d = function
        | ar, idx when is_all_int64 idx ->
            match ar with
            | TyArray (size,ar,_,t) ->
                if List.length (tuple_field size) = List.length (tuple_field idx) then TyOp(ArrayIndex,[size;ar;idx],t)
                else on_type_er d.trace "Array index does not match the number of dimensions in the array."
            | _ -> on_type_er d.trace "Trying to index into a non-array."
        | _ -> on_type_er d.trace "One of the index arguments in array index is not an int64."
    
    let array_index d ar idx = array_index' d (tev2 d ar idx)

    let rec apply tev d = function
        | closure & TyEnv(env_term,(FunctionT(env_ty,x) | RecFunctionT(env_ty,x,_))), TyType (ForCastT args_ty) -> 
            let instantiate_type_as_variable d args_ty =
                let f x = make_tyv_and_push_ty d x
                match args_ty with
                | VVT l -> TyVV(List.map f l, args_ty)
                | x -> f x

            let args = instantiate_type_as_variable d args_ty
//            let tev d x =
//                let r = memoize_closure args d x
//                if is_returnable r then r // This is always true in the F# segment.
//                else on_type_er d.trace "Closure does not have a returnable type."
                    
            apply (memoize_closure args) d (closure, args)
        | x, TyType (ForCastT t) -> on_type_er d.trace <| sprintf "Expected a function in type application. Got: %A" x
        | ar & TyArray _, idx -> array_index' d (ar, idx)
        | TyEnv(env_term,ModuleT env_ty), TypeString n -> v_find env_term n (fun () -> on_type_er d.trace <| sprintf "Cannot find a function named %s inside the module." n)
        | TyEnv(env_term,ModuleT env_ty), _ -> on_type_er d.trace "Expected a type level string in module application."
        | recf & TyEnv(env_term,RecFunctionT (env_ty, (pat,body), name)), args -> 
            let env = if pat <> "" then Map.add pat args env_term else env_term
            tev {d with env = Map.add name recf env} body
        | TyEnv(env_term,FunctionT (env_ty, (pat,body))), args -> 
            tev {d with env = if pat <> "" then Map.add pat args env_term else env_term} body
        | TyAssembly a, TypeString name -> 
                wrap_exception d <| fun _ ->
                    match a.GetType(name) with
                    | null -> on_type_er d.trace "A type cannot be found inside the assembly."
                    | x -> 
                        if x.IsPublic then
                            x |> map_dotnet globals.memoized_dotnet_types
                            |> DotNetTypeRuntimeT |> make_tyv_and_push_ty d
                        else
                            on_type_er d.trace "Cannot load a private type from an assembly."
        | TyAssembly _, _ -> on_type_er d.trace "Expected a type level string as the second argument."
        | dotnet_type & TyDotNetType _, method_name & TypeString _ ->
            let lam = inl' ["instance";"method_name";"args"] (ap (V "instance") (VV [V "method_name"; V "args"]))
                      |> expr_prepass |> snd |> tev d
            let ap a b = apply tev d (a, b)
            ap (ap lam dotnet_type) method_name
        | dotnet_type & TyDotNetType typ, args & TyTuple [TypeString method_name; TyTuple(TySystemTypeArgs method_args)] ->
            wrap_exception d <| fun _ ->
                match typ.GetMethod(method_name, method_args) with
                | null -> on_type_er d.trace "Cannot find a method with matching arguments."
                | meth -> 
                    if meth.IsPublic then
                        TyOp(DotNetTypeCallMethod,[dotnet_type;args],meth.ReturnType |> dotnet_type_to_ty)
                        |> make_tyv_and_push_typed_expr d
                    else
                        on_type_er d.trace "Cannot call a private method."
        | TyDotNetTypeRuntime runtime_type, args & TyTuple (TySystemTypeArgs system_type_args) ->
            wrap_exception d <| fun _ ->
                if runtime_type.ContainsGenericParameters then // instantiate generic type params
                    runtime_type.MakeGenericType system_type_args |> map_dotnet globals.memoized_dotnet_types
                    |> DotNetTypeRuntimeT |> make_tyv_and_push_ty d
                else // construct the type
                    match runtime_type.GetConstructor system_type_args with
                    | null -> on_type_er d.trace "Cannot find a constructor with matching arguments."
                    | con -> 
                        if con.IsPublic then
                            let instance_type = map_dotnet globals.memoized_dotnet_types runtime_type |> DotNetTypeInstanceT
                            TyOp(DotNetTypeConstruct,[args],instance_type) |> make_tyv_and_push_typed_expr d
                        else
                            on_type_er d.trace "Cannot call a private constructor."    
        | TyType(DotNetTypeInstanceT tag), _ -> on_type_er d.trace "Expected a type level string as the first argument for a method call."
        | TyType(TypeConstructorT uniont), args -> apply_typec d uniont args
        | closure & TyType(ClosureT(clo_arg_ty,clo_ret_ty)), args -> 
            let arg_ty = get_type args
            if arg_ty <> clo_arg_ty then on_type_er d.trace <| sprintf "Cannot apply an argument of type %A to closure (%A -> %A)." arg_ty clo_arg_ty clo_ret_ty
            else TyOp(Apply,[closure;args],clo_ret_ty) |> make_tyv_and_push_typed_expr d
        | a,b -> on_type_er d.trace <| sprintf "Invalid use of apply. %A and %A" a b

    let apply_tev d expr args = 
        let expr,args = tev2 d expr args
        apply tev d (expr, args)

    let (|LitIndex|_|) = function
        | TyLit (LitInt32 i) -> Some i
        | TyLit (LitInt64 i) -> Some (int i)
        | TyLit (LitUInt32 i) -> Some (int i)
        | TyLit (LitUInt64 i) -> Some (int i)
        | _ -> None

    let vv_index_template f d v i =
        let v,i = tev2 d v i
        match v, i with
        | TyVV(l,_), LitIndex i ->
            if i >= 0 || i < List.length l then f l i
            else on_type_er d.trace "Tuple index not within bounds."
        | v & TyType (VVT ts), LitIndex i -> failwith "The tuple should ways be destructured."
        | v, LitIndex i -> on_type_er d.trace <| sprintf "Type of an evaluated expression in tuple index is not a tuple.\nGot: %A" v
        | v, i -> on_type_er d.trace <| sprintf "Index into a tuple must be an at least a i32 less than the size of the tuple.\nGot: %A" i

    let vv_index d v i = vv_index_template (fun l i -> l.[i]) d v i
    let vv_slice_from d v i = vv_index_template (fun l i -> let l = l.[i..] in TyVV(l,VVT (List.map get_type l))) d v i

    let inline vv_unop_template on_succ on_fail d v =
        match tev d v with
        | TyVV(l,_) -> on_succ l
        | v & TyType (VVT ts) -> failwith "The tuple should ways be destructured."
        | v -> on_fail()

    let vv_length = 
        vv_unop_template (fun l -> TyLit (LitInt32 l.Length)) 
            (fun _ -> on_type_er d.trace <| sprintf "Type of an evaluated expression in tuple index is not a tuple.\nGot: %A" v)
    let vv_is = vv_unop_template (fun _ -> TyLit (LitBool true)) (fun _ -> TyLit (LitBool false))

    let eq_type d a b =
        let f x = match get_type x with TypeConstructorT x -> x | x -> x
        let a, b = tev2 d a b 
        LitBool (f a = f b) |> TyLit
    
    let vv_cons d a b =
        let a, b = tev2 d a b
        match b with
        | TyVV(b, VVT bt) -> TyVV(a::b, VVT (get_type a :: bt))
        | _ -> on_type_er d.trace "Expected a tuple on the right is in VVCons."

    let type_lit_create' d x = LitT x |> make_tyv_and_push_ty d

    let module_open d a b =
        let a = tev d a
        match a with
        | TyEnv(env_term, ModuleT env_ty) -> 
            let env = Map.fold (fun s k v -> Map.add k v s) d.env env_term
            tev {d with env = env} b
        | x -> on_type_er d.trace <| sprintf "The open expected a module type as input. Got: %A" x

    let module_with_f_extend d (module_,module_type) name arg =
        let x = Map.add name arg module_
        let x_ty = Map.add name (get_type arg) module_type
        TyEnv(x, ModuleT x_ty)

    let module_with_f d (module_,module_type) name arg =
        match Map.tryFind name module_ with
        | Some arg' ->
            if get_type arg = get_type arg' then module_with_f_extend d (module_,module_type) name arg
            else on_type_er d.trace <| sprintf "Cannot extend module with %s due to difference in types. Use the extensible `with` if that is the desired behavior." name
        | None -> on_type_er d.trace <| sprintf "Cannot extend module with %s due to it being missing in the module. Use the extensible `with` if that is the desired behavior." name

    let module_with_template f d module_ name arg =
        let module_, name, arg = tev3 d module_ name arg
        match module_, name with
        | TyEnv(module_,ModuleT module_type), TypeString name -> f d (module_,module_type) name arg
        | TyEnv(module_,ModuleT module_type), _ -> on_type_er d.trace "Expected a type level string as the second argument."
        | _ -> on_type_er d.trace "Expected a module as the first argument."

    let module_with = module_with_template module_with_f
    let module_with_extend = module_with_template module_with_f_extend

    let type_annot d a b =
        match d.rbeh with
        | AnnotationReturn -> tev d b |> get_type |> make_tyv_and_push_ty d
        | AnnotationDive ->
            let a, b = tev d a, tev_seq d b
            let ta, tb = get_type a, get_type b
            if ta = tb then a else on_type_er d.trace <| sprintf "%A <> %A" ta tb

    let prim_bin_op_template d check_error is_check k a b t =
        let a, b = tev2 d a b
        if is_check a b then k t a b
        else on_type_er d.trace (check_error a b)

    let prim_bin_op_helper t a b = TyOp(t,[a;b],get_type a)
    let prim_un_op_helper t a = TyOp(t,[a],get_type a)
    let bool_helper t a b = TyOp(t,[a;b],PrimT BoolT)

    let prim_arith_op d = 
        let er = sprintf "`is_numeric a && get_type a = get_type b` is false.\na=%A, b=%A"
        let check a b = is_numeric a && get_type a = get_type b
        prim_bin_op_template d er check (fun t a b ->
            let inline op_arith a b =
                match t with
                | Add -> a + b
                | Sub -> a - b
                | Mult -> a * b
                | Div -> a / b
                | Mod -> a % b
                | _ -> failwith "Expected an arithmetic operation."
            match a, b with
            | TyLit a', TyLit b' ->
                match a', b' with
                | LitInt8 a, LitInt8 b -> op_arith a b |> LitInt8 |> TyLit
                | LitInt16 a, LitInt16 b -> op_arith a b |> LitInt16 |> TyLit
                | LitInt32 a, LitInt32 b -> op_arith a b |> LitInt32 |> TyLit
                | LitInt64 a, LitInt64 b -> op_arith a b |> LitInt64 |> TyLit
                | LitUInt8 a, LitUInt8 b -> op_arith a b |> LitUInt8 |> TyLit
                | LitUInt16 a, LitUInt16 b -> op_arith a b |> LitUInt16 |> TyLit
                | LitUInt32 a, LitUInt32 b -> op_arith a b |> LitUInt32 |> TyLit
                | LitUInt64 a, LitUInt64 b -> op_arith a b |> LitUInt64 |> TyLit
                | LitFloat32 a, LitFloat32 b -> op_arith a b |> LitFloat32 |> TyLit
                | LitFloat64 a, LitFloat64 b -> op_arith a b |> LitFloat64 |> TyLit
                | _ -> prim_bin_op_helper t a b
            | _ -> prim_bin_op_helper t a b
            )
            

    let prim_comp_op d = 
        let er = sprintf "`(is_numeric a || is_bool a) && get_type a = get_type b` is false.\na=%A, b=%A"
        let check a b = (is_numeric a || is_bool a) && get_type a = get_type b
        prim_bin_op_template d er check (fun t a b ->
            let inline op a b =
                match t with
                | LT -> a < b
                | LTE -> a <= b
                | EQ -> a = b
                | GT -> a > b
                | GTE -> a >= b 
                | _ -> failwith "Expected a comparison operation."
            match a, b with
            | TyLit a', TyLit b' ->
                match a', b' with
                | LitInt8 a, LitInt8 b -> op a b |> LitBool |> TyLit
                | LitInt16 a, LitInt16 b -> op a b |> LitBool |> TyLit
                | LitInt32 a, LitInt32 b -> op a b |> LitBool |> TyLit
                | LitInt64 a, LitInt64 b -> op a b |> LitBool |> TyLit
                | LitUInt8 a, LitUInt8 b -> op a b |> LitBool |> TyLit
                | LitUInt16 a, LitUInt16 b -> op a b |> LitBool |> TyLit
                | LitUInt32 a, LitUInt32 b -> op a b |> LitBool |> TyLit
                | LitUInt64 a, LitUInt64 b -> op a b |> LitBool |> TyLit
                | LitFloat32 a, LitFloat32 b -> op a b |> LitBool |> TyLit
                | LitFloat64 a, LitFloat64 b -> op a b |> LitBool |> TyLit
                | _ -> bool_helper t a b
            | _ -> bool_helper t a b
            )

    let prim_bool_op d = 
        let er = sprintf "`is_bool a && get_type a = get_type b` is false.\na=%A, b=%A"
        let check a b = is_bool a && get_type a = get_type b
        prim_bin_op_template d er check (fun t a b ->
            let inline op a b =
                match t with
                | And -> a && b
                | Or -> a || b
                | _ -> failwith "Expected a comparison operation."
            match a, b with
            | TyLit (LitBool a), TyLit (LitBool b) -> op a b |> LitBool |> TyLit
            | _ -> bool_helper t a b            
            )

    let prim_shift_op d =
        let er = sprintf "`is_int a && is_int b` is false.\na=%A, b=%A"
        let check a b = is_int a && is_int b
        prim_bin_op_template d er check prim_bin_op_helper

    let prim_shuffle_op d =
        let er = sprintf "`is_int b` is false.\na=%A, b=%A"
        let check a b = is_int b
        prim_bin_op_template d er check prim_bin_op_helper

    let prim_un_op_template d check_error is_check k a t =
        let a = tev d a
        if is_check a then k t a
        else on_type_er d.trace (check_error a)

    let prim_un_floating d =
        let er = sprintf "`is_float a` is false.\na=%A"
        let check a = is_float a
        prim_un_op_template d er check prim_un_op_helper

    let prim_un_numeric d =
        let er = sprintf "`is_numeric a` is false.\na=%A"
        let check a = is_numeric a
        prim_un_op_template d er check (fun t a -> 
            let inline op a = 
                match t with
                | Neg -> -a
                | _ -> failwithf "Unexpected operation %A." t

            match a with
            | TyLit a' ->
                match a' with
                | LitInt8 a -> op a |> LitInt8 |> TyLit
                | LitInt16 a -> op a |> LitInt16 |> TyLit
                | LitInt32 a -> op a |> LitInt32 |> TyLit
                | LitInt64 a -> op a |> LitInt64 |> TyLit
                | LitFloat32 a -> op a |> LitFloat32 |> TyLit
                | LitFloat64 a -> op a |> LitFloat64 |> TyLit
                | _ -> prim_un_op_helper t a
            | _ -> prim_un_op_helper t a
            )

    let for_cast d x = tev_seq d x |> get_type |> typec_strip |> ForCastT |> make_tyv_and_push_ty d
    let error_non_unit d a =
        let x = tev d a 
        if get_type x <> BVVT then on_type_er d.trace "Only the last expression of a block is allowed to be unit. Use `ignore` if it intended to be such."
        else x

    let type_lit_create d a =
        match tev d a with
        | TyLit a -> type_lit_create' d a
        | _ -> on_type_er d.trace "Expected a literal in type literal create."

    let case_tuple_template d v case =
        let assume d v x branch = tev_assume (cse_add' d v x) d branch
        match tev d v with
        | TyTag(_, t & (UnionT _ | RecT _)) as v ->
            let rec case_destructure d args_ty =
                let f x = make_tyv_and_push_ty d x
                let union_case = function
                    | UnionT l -> Set.toList l |> List.collect (case_destructure d)
                    | _ -> [f args_ty]
                match args_ty with
                | RecT tag -> union_case globals.memoized_types.[tag]
                | x -> union_case x

            let rec map_cases l =
                match l with
                | x :: xs -> (x, assume d v x case) :: map_cases xs
                | _ -> []
                            
            match map_cases (case_destructure d t) with
            | (_, TyType p) :: _ as cases -> 
                if List.forall (fun (_, TyType x) -> x = p) cases then TyOp(Case,v :: List.collect (fun (a,b) -> [a;b]) cases, p)
                else on_type_er d.trace "All the cases in pattern matching clause with dynamic data must have the same type."
            | _ -> failwith "There should always be at least one clause here."
        | a & TyV(b,_) -> assume d a b case
        | _ -> tev d case

    let dynamize d a =
        match tev d a with
        | TyLit _ as a -> make_tyv_and_push_typed_expr d a
        | a -> a

    let module_create d l =
        let rec loop acc = function
            | V x -> x :: acc
            | VV l -> List.fold loop acc l
            | Pos(_,x) -> loop acc x
            | _ -> on_type_er d.trace "Only variable names are allowed in module create."
        let er _ = on_type_er d.trace "In module create, the variable was not found."
        let env = List.map (fun n -> n, v_find d.env n er) (loop [] l) |> Map
        TyEnv(env, ModuleT <| env_to_ty env)

    let array_create d size typ =
        let typ = tev_seq d typ |> function 
            | TyType (TypeConstructorT x) -> x 
            | TyType x -> x

        let size, array_type =
            match tev d size with
            | size when is_all_int64 size -> size, ArrayT(DotNetHeap,typ)
            | size -> on_type_er d.trace <| sprintf "An size argument in CreateArray is not of type int64.\nGot: %A" size

        let array = TyOp(ArrayCreate,[size],array_type) |> make_tyv_and_push_typed_expr d
        let l = [size;array] in TyVV(l,VVT <| List.map get_type l)

    let reference_create d x =
        let x = tev d x
        let size, array_type = TyB, ArrayT(DotNetReference, get_type x)
        let array = TyOp(ReferenceCreate,[x],array_type) |> make_tyv_and_push_typed_expr d
        let l = [size;array] in TyVV(l,VVT <| List.map get_type l)

    let array_set d ar idx r =
        match array_index d ar idx, tev d r with
        | l, r when get_type l = get_type r -> make_tyv_and_push_typed_expr d (TyOp(ArraySet,[l;r],BVVT))
        | _ -> on_type_er d.trace "The two sides in array set have different types."

    let add_trace d x = {d with trace = x :: d.trace}

    match expr with
    | Lit value -> TyLit value
    | V x -> v_find d.env x (fun () -> on_type_er d.trace <| sprintf "Variable %A not bound." x)
    | FunctionFilt(vars, (pat, body)) -> 
        let env = Map.filter (fun k _ -> Set.contains k vars) d.env
        let env_ty = env_to_ty env
        let pat = if vars.Contains pat then pat else ""
        TyEnv(env, FunctionT(env_ty, (pat, body)))
    | Function core -> failwith "Function not allowed in this phase as it tends to cause stack overflows in recursive scenarios."
    | Pattern pat -> failwith "Pattern not allowed in this phase as it tends to cause stack overflows when prepass is triggered in the match case."
    | Pos (pos, body) -> tev (add_trace d pos) body
    | VV vars -> 
        let vv = List.map (tev d) vars 
        TyVV(vv, VVT(List.map get_type vv))
    | Op(op,vars) ->
        match op, vars with
        | DotNetLoadAssembly,[a] -> dotnet_load_assembly d a
        | Fix,[Lit (LitString name); body] ->
            match tev d body with
            | TyEnv(env_term,FunctionT(env_ty,core)) -> TyEnv(env_term,RecFunctionT(env_ty,core,name))  
            | x -> failwithf "Invalid use of Fix. Got: %A" x
        | Case,[v;case] -> case_tuple_template d v case
        | IfStatic,[cond;tr;fl] -> if_static d cond tr fl
        | If,[cond;tr;fl] -> if_ d cond tr fl
        | Apply,[a;b] -> apply_tev d a b
        | MethodMemoize,[a] -> memoize_method d a
        | ForCast,[x] -> for_cast d x
        
        | PrintStatic,[a] -> tev d a |> fun r -> printfn "%A" r; TyB
        | ModuleOpen,[a;b] -> module_open d a b
        | ModuleCreate,[l] -> module_create d l
        | ModuleWith,[a;b;c] -> module_with d a b c
        | ModuleWithExtend,[a;b;c] -> module_with_extend d a b c
        | TypeLitCreate,[a] -> type_lit_create d a
        | Dynamize,[a] -> dynamize d a

        | ArrayCreate,[a;b] -> array_create d a b
        | ReferenceCreate,[a] -> reference_create d a
        | ArrayIndex,[a;b] -> array_index d a b
        | ArraySet,[a;b;c] -> array_set d a b c

        // Primitive operations on expressions.
        | Add,[a;b] -> prim_arith_op d a b Add
        | Sub,[a;b] -> prim_arith_op d a b Sub 
        | Mult,[a;b] -> prim_arith_op d a b Mult
        | Div,[a;b] -> prim_arith_op d a b Div
        | Mod,[a;b] -> prim_arith_op d a b Mod

        | LT,[a;b] -> prim_comp_op d a b LT
        | LTE,[a;b] -> prim_comp_op d a b LTE
        | EQ,[a;b] -> prim_comp_op d a b EQ
        | NEQ,[a;b] -> prim_comp_op d a b NEQ 
        | GT,[a;b] -> prim_comp_op d a b GT
        | GTE,[a;b] -> prim_comp_op d a b GTE
    
        | And,[a;b] -> prim_bool_op d a b And
        | Or,[a;b] -> prim_bool_op d a b Or

        | ShiftLeft,[a;b] -> prim_shift_op d a b ShiftLeft
        | ShiftRight,[a;b] -> prim_shift_op d a b ShiftRight

        | ShuffleXor,[a;b] -> prim_shuffle_op d a b ShuffleXor
        | ShuffleUp,[a;b] -> prim_shuffle_op d a b ShuffleUp
        | ShuffleDown,[a;b] -> prim_shuffle_op d a b ShuffleDown
        | ShuffleIndex,[a;b] -> prim_shuffle_op d a b ShuffleIndex

        | VVIndex,[a;b] -> vv_index d a b
        | VVLength,[a] -> vv_length d a
        | VVIs,[a] -> vv_is d a
        | VVSliceFrom,[a;b] -> vv_slice_from d a b
        | VVCons,[a;b] -> vv_cons d a b

        | TypeAnnot,[a;b] -> type_annot d a b
        | TypeConstructorUnion,[a;b] -> typec_union d a b
        | EqType,[a;b] -> eq_type d a b

        | Neg,[a] -> prim_un_numeric d a Neg
        | ErrorType,[a] -> tev d a |> fun a -> on_type_er d.trace <| sprintf "%A" a
        | ErrorNonUnit,[a] -> error_non_unit d a

        | Log,[a] -> prim_un_floating d a Log
        | Exp,[a] -> prim_un_floating d a Exp
        | Tanh,[a] -> prim_un_floating d a Tanh

        // Constants
        | TypeConstructorCreate,[a] -> typec_create d a
        | _ -> failwith "Missing Op case."
    |> destructure d


/// Reasonable default for the dims.
let default_dims = dim3(256), dim3(20)

let print_type_error (code: Dictionary<string, string []>) (trace: Trace) message = 
    let error = System.Text.StringBuilder(1024)
    error.AppendLine message |> ignore
    let rec loop prev_file prev_line i (trace: _[]) = 
        if i > 0 then
            let (file, line: int64, col: int64) = trace.[i-1]
            if prev_file <> file || prev_line <> line then
                let er_code = code.[file].[int line - 1]
                let er_file = if file <> "" then sprintf " in file \"%s\"." file else file
                error.AppendLine <| sprintf "Error trace on line: %i, column: %i%s" line col er_file |> ignore
                error.AppendLine er_code |> ignore
                let col = int (col - 1L)
                for i=1 to col do error.Append(' ') |> ignore
                error.AppendLine "^" |> ignore
            loop file line (i-1) trace
        else
            error.ToString()
    let trace = List.toArray trace
    loop "" -1L trace.Length trace

let data_empty () =
    {ltag = ref 0L; seq=ref id; trace=[]; rbeh=AnnotationDive
     env = Map.empty
     cse_env = ref Map.empty
     }

let globals_empty (): LangGlobals =
    {
    method_tag = ref 0L
    memoized_methods = d0()
    type_tag = ref 0L
    memoized_types = d0()
    memoized_dotnet_assemblies = d0(), d0()
    memoized_dotnet_types = d0(), d0()
    }

let array_index op a b = Op(op,[a;b])
let type_create a = Op(TypeConstructorCreate,[a])
let type_union a b = Op(TypeConstructorUnion,[a;b])

let core_functions =
    let p f = inl "x" (f (v "x"))
    let p2 f = inl' ["x"; "y"] (f (v "x") (v "y"))
    let p3 f = inl' ["x"; "y"; "z"] (f (v "x") (v "y") (v "z"))
    let binop' op a b = Op(op,[a;b])
    let binop op = p2 (binop' op)
    let b str op = l str (binop op)
    let apply a b = binop' Apply a b
    let compose a b c = apply a (apply b c)
    let con x = Op(x,[])
    let lit x = Lit (x)
    s  [l "error_type" (p error_type)
        l "print_static" (p print_static)
        l "dyn" (p dynamize)
        l "union" (p2 type_union)

        l "int64" (Op(TypeConstructorCreate,[Lit <| LitInt64 0L]))
        l "int32" (Op(TypeConstructorCreate,[Lit <| LitInt32 0]))
        l "int16" (Op(TypeConstructorCreate,[Lit <| LitInt16 0s]))
        l "int8" (Op(TypeConstructorCreate,[Lit <| LitInt8 0y]))
        l "uint64" (Op(TypeConstructorCreate,[Lit <| LitUInt64 0UL]))
        l "uint32" (Op(TypeConstructorCreate,[Lit <| LitUInt32 0u]))
        l "uint16" (Op(TypeConstructorCreate,[Lit <| LitUInt16 0us]))
        l "uint8" (Op(TypeConstructorCreate,[Lit <| LitUInt8 0uy]))
        l "float64" (Op(TypeConstructorCreate,[Lit <| LitFloat64 0.0]))
        l "float32" (Op(TypeConstructorCreate,[Lit <| LitFloat32 0.0f]))
        l "string" (Op(TypeConstructorCreate,[Lit <| LitString ""]))
        l "char" (Op(TypeConstructorCreate,[Lit <| LitChar ' ']))
        l "unit" (Op(TypeConstructorCreate,[B]))

        l "lit_lift" (p <| fun x -> Op(TypeLitCreate,[x]))
        l "for_cast" (p for_cast)
        l "negate" (p <| fun x -> Op(Neg,[x]))
        
        l "load_assembly" (p <| fun x -> Op(DotNetLoadAssembly,[x]))
        l "mscorlib" (ap (V "load_assembly") (ap (V "lit_lift") (lit_string "mscorlib")))
        l "ignore" (inl "" B)
        l "ref" (p <| fun x -> Op(ReferenceCreate,[x]))
        l "array_create" (p2 <| fun size typ -> Op(ArrayCreate,[size;typ]))

        b "+" Add; b "-" Sub; b "*" Mult; b "/" Div
        b "<|" Apply; l "|>" (p2 (flip apply)); l "<<" (p3 compose); l ">>" (p3 (flip compose))

        b "<=" LTE; b "<" LT; b "=" EQ; b ">" GT; b ">=" GTE
        b "||" Or; b "&&" And; b "::" VVCons
        ]

let spiral_typecheck code body on_fail ret = 
    let globals = globals_empty()
    let d = data_empty()
    let input = core_functions body
    try
        let x = !d.seq (expr_typecheck globals d (expr_prepass input |> snd))
        typed_expr_optimization_pass 2 globals.memoized_methods x // Is mutable
        ret (x, globals)
    with 
    | :? TypeError as e -> 
        let trace, message = e.Data0, e.Data1
        on_fail <| print_type_error code trace message
