#load "../Scripts/load-project-release.fsx"

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

type Ty =
    | PrimT of SpiralDeviceVarType
    | VVT of Ty list
    | NameT of string
    | FunctionT of EnvTy * FunctionCore // Type level function. Can also be though of as a procedural macro.
    | ModuleT of EnvTy
    | UnionT of Set<Ty>
    | RecT of Tag
    | TypeConstructorT of Ty
    | LocalPointerT of Ty
    | SharedPointerT of Ty
    | GlobalPointerT of Ty
    | ClosureT of Ty * Ty
    | ForCastT of Ty // For casting type level function to term (ClosureT) level ones.

and Tag = int64
and TyV = Tag * Ty
and EnvTerm = Map<string, TypedExpr>
and EnvTy = Map<string, Ty>
and FunctionCore = string * string * Expr
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

    | ThreadIdxX | ThreadIdxY | ThreadIdxZ
    | BlockIdxX | BlockIdxY | BlockIdxZ

and Op =
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
    | MSet 

    | Apply
    | ApplyType
    | MethodMemoize
    | StructCreate
    | VVIndex
    | VVCons
    | TypeAnnot
    | ModuleWith
    | ModuleWithExtend
    | EnvUnseal
    | TypeConstructorCreate
    | TypeConstructorUnion

    | ArrayCreate
    | ArrayCreateShared
    | ArrayIndex
    | ArrayUnsafeIndex
   
    | ShiftLeft
    | ShiftRight
    | ShuffleXor
    | ShuffleUp
    | ShuffleDown
    | ShuffleIndex

    // Static unary operations
    | StaticPrint
    | ErrorNonUnit
    | ErrorType
    | ModuleOpen

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

and PosKey = string * int64 * int64
and Pos = PosKey option

and Expr = 
    | V of string * Pos
    | Lit of Value * Pos
    | Function of FunctionCore * Set<string> ref * Pos
    | VV of Expr list * Pos // named tuple
    | Op of Op * Expr list * Pos

and Arguments = Set<TyV> ref
and Renamer = Map<Tag,Tag>

and MemoExprType =
| MemoClosure
| MemoMethod

and LetType =
| LetStd
| LetInvisible

and TypedExpr =
    | TyV of TyV
    | TyLet of LetType * TyV * TypedExpr * TypedExpr * Ty
    | TyLit of Value
    
    | TyVV of TypedExpr list * Ty
    | TyEnv of EnvTerm * Ty
    | TyOp of Op * TypedExpr list * Ty
    | TyMemoizedExpr of MemoExprType * Arguments * Renamer * Tag * Ty

and MemoCases =
    | MemoMethodInEvaluation of Tag
    | MemoMethodDone of TypedExpr * Tag * Arguments
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
    | LitString _ -> LocalPointerT <| PrimT UInt8T
    | ThreadIdxX | ThreadIdxY | ThreadIdxZ 
    | BlockIdxX | BlockIdxY | BlockIdxZ -> PrimT UInt64T

let get_type = function
    | TyLit x -> get_type_of_value x
    | TyV (_,t) | TyLet(_,_,_,_,t) | TyMemoizedExpr(_,_,_,_,t)
    | TyVV(_,t) | TyEnv(_,t) | TyOp(_,_,t) -> t

let get_subtype x = 
    match get_type x with
    | ForCastT x -> x
    | x -> x

/// Wraps the argument in a list if not a tuple.
let tuple_field = function 
    | TyVV(args,_) -> args
    | x -> [x]

/// Wraps the argument in a set if not a UnionT.
let set_field = function
    | UnionT t -> t
    | t -> Set.singleton t

/// Wraps the argument in a list if not a tuple type.
let tuple_field_ty = function 
    | VVT x -> x
    | x -> [x]

type ArrayType = Local | Shared | Global

let get_type_pointer x =
    match get_type x with
    | LocalPointerT x -> x, Local
    | SharedPointerT x -> x, Shared
    | GlobalPointerT x -> x, Global
    | _ -> failwith "Expected a pointer"

let (|TyType|_|) = function
    | TyV(_,x) -> Some x
    | _ -> None

let (|Array|_|) = function
    | TyVV([size;typ;TyType(NameT "Array")],_) ->
        let typ, array_type = get_type_pointer typ
        Some(array_type,tuple_field size,typ)
    | _ -> None

let rec is_returnable' = function
    | VVT x -> List.forall is_returnable' x
    | RecT _ | LocalPointerT _ | SharedPointerT _ | GlobalPointerT _ -> false
    | TypeConstructorT x -> is_returnable' x
    | UnionT env -> Set.forall is_returnable' env
    | FunctionT (env, _) | ModuleT env -> Map.forall (fun _ -> is_returnable') env
    | NameT _ | ForCastT _ | ClosureT _ | NameT _ | PrimT _ -> true
let is_returnable a = is_returnable' (get_type a)

let is_numeric' = function
    | PrimT x -> 
        match x with
        | UInt8T | UInt16T | UInt32T | UInt64T 
        | Int8T | Int16T | Int32T | Int64T 
        | Float32T | Float64T -> true
        | BoolT -> false
    | _ -> false
let is_numeric a = is_numeric' (get_type a)

let is_primt' = function
    | PrimT x -> true
    | _ -> false
let is_primt a = is_primt' (get_type a)

let is_float' = function
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

let is_full_name = function
    | "" | "_" -> false
    | _ -> true

let is_arg = function TyV _ -> true | _ -> false

let h0() = HashSet(HashIdentity.Structural)
let d0() = Dictionary(HashIdentity.Structural)

let fun_ name (pat,body) pos = Function((name,pat,body),ref Set.empty,pos)
let inlr name x y = fun_ name (x,y)
let inl x y = inlr "" x y
let ap pos x y = Op(Apply,[x;y],pos)
let ap_ty pos x = Op(ApplyType,[x],pos)
let l v b pos e = ap pos (inl v e pos) b
    
let meth_memo y = Op(MethodMemoize,[y],None)
let methr name x y pos = inlr name x (meth_memo y) pos
let meth x y = methr "" x y

let module_create pos = Op(ModuleCreate,[],pos)
let module_open pos a b = Op(ModuleOpen,[a;b],pos)

let B = VV ([], None)
let BVVT = VVT []
let TyB = TyVV ([], BVVT)

let v x = V(x,None)

let cons a b pos = Op(VVCons,[a;b],pos)

let s l fin = List.foldBack (fun x rest -> x rest) l fin

let rec ap' f l pos =
    match l with
    | x :: xs -> ap' (ap pos f x) xs pos
    | [] -> f

let match_ x pat pos = ap pos (fun_ "" pat pos) x
let function_ pat = fun_ "" pat

let rec inlr' name args body pos =
    match args with
    | x :: xs -> inlr name x (inlr' "" xs body None) pos
    | [] -> body

let rec inl' args body = inlr' "" args body

let rec methr' name args body pos =
    match args with
    | [x] -> methr name x body pos
    | x :: xs -> inlr name x (methr' "" xs body None) pos
    | [] -> body

let meth' args body = methr' "" args body

let vv pos x = VV(x,pos)

let error_type x = Op(ErrorType, [x], None)
let static_print x = Op(StaticPrint,[x], None)

let get_pos = function
    | Lit(_,pos) | V(_,pos) | Function(_,_,pos) | VV(_,pos) | Op(_,_,pos) -> pos

let error_non_unit x = Op(ErrorNonUnit, [x], get_pos x)

let inline vars_union' init f l = List.fold (fun s x -> Set.union s (f x)) init l
let inline vars_union f l = vars_union' Set.empty f l

let expr_free_variables e = 
    let rec expr_free_variables vars_for_module e =
        let f e = expr_free_variables vars_for_module e
        let f' bound_vars e = expr_free_variables bound_vars e
        match e with
        | Op(ModuleCreate,[],_) -> vars_for_module
    
        | V (n,_) -> Set.singleton n
        | Op(Apply,[a;b],_) -> f a + f' Set.empty b // This is so modules only capture their local scope, not their entire lexical scope.
        | Op(_,l,_) | VV(l,_) -> vars_union f l
        | Function((name,pat,body),free_var_set,_) ->
            let fv = 
                let g f x = x |> f name |> f pat
                let add_name_pat x = g Set.add x
                let remove_name_pat x = g Set.remove x
                remove_name_pat (f' (add_name_pat vars_for_module) body)
            free_var_set := fv
            fv
        | Lit _ -> Set.empty

    expr_free_variables Set.empty e


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
    | TyV (n,t) -> TyV (Map.find n r,t)
    | TyType _ | TyLit _ -> e
    | TyVV(l,t) -> TyVV(List.map f l,t)
    | TyEnv(l,t) -> TyEnv(renamer_apply_env r l, t)
    | TyMemoizedExpr(typ,used_vars,renamer,tag,t) -> 
        let renamer = renamer_apply_renamer r renamer
        let used_vars = ref <| renamer_apply_pool r !used_vars
        TyMemoizedExpr(typ,used_vars,renamer,tag,t)
    | TyOp(o,l,t) -> TyOp(o,List.map f l,t)
    | TyLet(le,(n,t),a,b,t') -> TyLet(le,(Map.find n r,t),f a,f b,t')

let rec typed_expr_free_variables_template on_memo e =
    let inline f e = typed_expr_free_variables_template on_memo e
    match e with
    | TyV (n,t) -> Set.singleton (n, t)
    | TyType _ | TyLit _ -> Set.empty
    | TyVV(l,_) | TyOp(_,l,_) -> vars_union f l
    | TyEnv(l,_) -> env_free_variables_template on_memo l
    | TyMemoizedExpr(typ,used_vars,renamer,tag,ty) -> on_memo (typ,used_vars,renamer,tag)
    | TyLet(_,x,a,b,_) -> Set.remove x (f b) + f a

and env_free_variables_template on_memo env = 
    Map.fold (fun s _ v -> typed_expr_free_variables_template on_memo v + s) Set.empty env

let private typed_expr_std_pass (typ,used_vars,renamer,tag) = !used_vars
let rec typed_expr_free_variables e = typed_expr_free_variables_template typed_expr_std_pass e
and env_free_variables env = env_free_variables_template typed_expr_std_pass env

let memo_value = function
    | MemoMethodDone (e, tag, args) -> e, tag, args
    | _ -> failwith "impossible"

/// Optimizes the free variables for the sake of tuple deforestation.
/// It needs at least two passes to converge properly. And probably exactly two.
let typed_expr_optimization_pass num_passes (memo: MemoDict) typed_exp =
    let rec on_method_call_optimization_pass (memo: (Set<Tag * Ty> * int) []) (expr_map: Map<Tag,TypedExpr * Arguments>) (typ, r, renamer, tag) =
        match typ with
        | MemoMethod ->
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
        | MemoClosure -> 
            !r

    let memo = Seq.map (memo_value >> (fun (e,tag,args) -> tag,(e,args))) memo.Values |> Map
    typed_expr_free_variables_template (on_method_call_optimization_pass (Array.init memo.Count (fun _ -> Set.empty,0)) memo) typed_exp
    |> ignore

let env_to_ty env = Map.map (fun _ -> get_type) env
let env_num_args env = 
    Map.fold (fun s k v -> 
        let f = typed_expr_free_variables v
        if Set.isEmpty f then s else s+1) 0 env

type Trace = PosKey list

type RecursiveBehavior =
| AnnotationDive
| AnnotationReturn

type LangEnv<'a,'c,'q,'e> =
    {
    rbeh: RecursiveBehavior
    ltag : int64 ref
    seq : (TypedExpr -> TypedExpr) ref
    env : EnvTerm
    cse_env : CSEDict
    trace : Trace
    on_type_er : Trace -> 'c -> 'e
    }

let rec expr_typecheck (gridDim: dim3, blockDim: dim3 as dims) (method_tag, memoized_methods: MemoDict, type_tag, memoized_types: Dictionary<_,_> as globals)
        (d : LangEnv<_,_,_,_>) (expr: Expr) ret =
    let inline tev d expr ret = expr_typecheck dims globals d expr ret
    let inline apply_seq d x = !d.seq x
    let inline tev_seq d expr ret = let d = {d with seq=ref id; cse_env=ref !d.cse_env} in tev d expr (apply_seq d >> ret)
    let inline tev_method d expr ret = let d = {d with seq=ref id; cse_env=ref Map.empty} in tev d expr (apply_seq d >> ret)
    let inline tev_rec d expr ret = tev_method {d with rbeh=AnnotationReturn} expr ret

    let v_find env x on_fail ret = 
        match Map.tryFind x env with
        | Some v -> ret v
        | None -> on_fail()

    let rec vars_map d l ret =
        match l with
        | x :: xs -> tev d x (fun x -> vars_map d xs (fun x' -> ret (x :: x')))
        | [] -> ret []

    let inline tev2 d a b ret =
        tev d a <| fun l ->
            tev d b <| fun r ->
                ret l r

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
        TyV v

    let make_tyv_and_push_ty d ty =
        let v = make_tyv_ty d ty
        let v' = TyV v
        let seq = !d.seq
        d.seq := fun rest -> TyLet(LetInvisible,v,v',rest,get_type rest) |> seq
        v'

    // for a shallow version, take a look at `alternative_destructure_v6e.fsx`.
    // The deep version can also be straightforwardly derived from a template of this using the Y combinator.
    let rec destructure d r = 
        let inline destructure r = destructure d r
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
            | ModuleT env | FunctionT (env, _) -> TyEnv(env_unseal env, r_ty)
            | _ -> r

        let destructure_cse = 
            let rec chase f r = 
                match Map.tryFind r !d.cse_env with
                | None -> f r
                | Some x -> chase id x

            chase (fun r ->
                let x = make_tyv_and_push_typed_expr d r |> destructure
                if r <> x then d.cse_env := !d.cse_env |> Map.add r x
                chase id x)
            
        match r with
        | TyLit _ -> r
        | TyV _ -> destructure_var r
        | TyVV(l,ty) -> TyVV(List.map destructure l, ty)
        | TyEnv(l,ty) -> TyEnv(Map.map (fun _ -> destructure) l,ty)
        | TyMemoizedExpr _ | TyLet _ | TyOp _ -> destructure_cse r

    let if_ d cond tr fl ret =
        tev d cond <| fun cond ->
            if is_bool cond = false then d.on_type_er d.trace <| sprintf "Expected a bool in conditional.\nGot: %A" (get_type cond)
            else
                tev_seq d tr <| fun tr ->
                    tev_seq d fl <| fun fl ->
                        let type_tr, type_fl = get_type tr, get_type fl
                        if type_tr = type_fl then 
                            if is_returnable' type_tr then
                                match cond with
                                | TyLit(LitBool true) -> tr
                                | TyLit(LitBool false) -> fl
                                | _ -> TyOp(If,[cond;tr;fl],type_tr)
                                |> ret
                            else d.on_type_er d.trace <| sprintf "The following is not a type that can be returned from a if statement. Consider using Inlineable instead. Got: %A" type_tr
                        else d.on_type_er d.trace <| sprintf "Types in branches of If do not match.\nGot: %A and %A" type_tr type_fl

    let tag r =
        let tag = !r
        r := tag + 1L
        tag

    let method_tag () = tag method_tag
    let type_tag () = tag type_tag

    let eval_method used_vars d expr ret =
        let key_args = d.env, expr
        
        match memoized_methods.TryGetValue key_args with
        | false, _ ->
            let tag = method_tag ()

            memoized_methods.[key_args] <- MemoMethodInEvaluation tag
            tev_method d expr <| fun typed_expr ->
                memoized_methods.[key_args] <- MemoMethodDone (typed_expr, tag, used_vars)
                ret (typed_expr, tag)
        | true, MemoMethodInEvaluation tag -> 
            tev_rec d expr <| fun r -> ret (r, tag)
        | true, MemoMethodDone (typed_expr, tag, used_vars) -> 
            ret (typed_expr, tag)
        | true, (MemoTypeInEvaluation | MemoType _) ->
            failwith "Expected a method, not a recursive type."

    let eval_renaming d expr ret =
        let env = d.env
        let fv = env_free_variables env
        let renamer = renamer_make fv
        let renamed_env = renamer_apply_env renamer env

        eval_method (renamer_apply_pool renamer fv |> ref) {d with env=renamed_env; ltag=ref <| int64 renamer.Count} expr <| fun (typed_expr, tag) ->
            let typed_expr_ty = get_type typed_expr
            if is_returnable' typed_expr_ty = false then d.on_type_er d.trace <| sprintf "The following is not a type that can be returned from a method. Consider using Inlineable instead. Got: %A" typed_expr
            else ret (ref fv, renamer_reverse renamer, tag, typed_expr_ty)

    let inline memoize_helper k d x ret = eval_renaming d x (k >> make_tyv_and_push_typed_expr d >> ret)
    let memoize_method d x ret = 
        memoize_helper (fun (args,renamer,tag,ret_ty) -> 
            TyMemoizedExpr(MemoMethod,args,renamer,tag,ret_ty)) d x ret

    let memoize_closure arg_ty d x ret =
        memoize_helper (fun (args,renamer,tag,ret_ty) -> 
            TyMemoizedExpr(MemoClosure,args,renamer,tag,ClosureT(arg_ty,ret_ty))) d x ret

    let apply_module d env_term b ret = 
        v_find env_term b (fun () -> d.on_type_er d.trace <| sprintf "Cannot find a function named %s inside the module." b) ret

//    Ground rules for type constructor application:
//    1) Nested type constructors are disallowed. If a type constructor is in a type constructor then it is to be stripped. That means that TypeConstructorT can only appear on the outermost level.
//    2) A tuple is matched directly against the tuple type and returns a tuple. This is straightforward.
//    3) A tuple will always returned from the union case if the input to it is a tuple.
//    4) Variables will always be wrapped in a tuple when they are a part of the union to indicate they are statically determined.
//    5) A variable that has a union type will always mean a sealed union type.
//    6) Union type can only be destructured via pattern matching.
//    7) Corollary to #3, the tuple returned from a union apply case will have its type replaced (generalized) into the union's type.
//    8) The tuple returned from a type constructor will of course not have the type constructor type.
//    9) Lastly, tuples returned from a recursive type case will have their type replaced with the recursive type much like when returned from a union case.
//    10) Singleton tuples returned from a type constructor case whose item's type is equal to the tuple's outer type, 
//        meaning the tuple is not a recursive or a union type will get implicitly converted to a variable. 
//        This so type construction application meshes with type constructor creation.

//    Note: It is intended that the inputs to type constructors be destructured and hence contain no free floating expressions.
//          The functions should work regardless.
    
    let apply_typec d uniont ra ret =
        let rec apply_typec d uniont ra on_fail ret =
            match uniont, ra with
            | TypeConstructorT _, _ -> failwith "Type constructors should never be nested." // Checks whether #1 is implemented correctly
            | VVT x, r -> apply_typec_tuple d x (tuple_field r) on_fail <| fun v -> TyVV(v,uniont) |> ret // Implements #2
            | UnionT x, TyVV(_,_) ->
                apply_typec_union_tuple d x ra on_fail <| function 
                    | (TyVV(v,_)) -> TyVV(v,uniont) |> ret // Implements #7
                    | _ -> failwith "A tuple should be returned from the union tuple case call." // Implements #3
            | UnionT _, _ -> if uniont <> get_type ra then on_fail() else TyVV([ra],uniont) |> ret // Implements #4, #5, #6
            | RecT _, _ -> failwith "implement later" // Should implement #9
            | x, r -> if x = get_type r then ret r else on_fail()

        and apply_typec_union_tuple d x r on_fail ret =
            let rec loop = function
                | x :: xs -> apply_typec d x r (fun _ -> loop xs) ret
                | [] -> on_fail()
            loop (Set.toList x)

        and apply_typec_tuple d uniont ra on_fail ret =
            match uniont, ra with
            | x :: xs, r :: rs ->
                apply_typec d x r on_fail <| fun v ->
                    apply_typec_tuple d xs rs on_fail <| fun vs ->
                        ret (v :: vs)
            | [], [] -> ret []
            | _ -> on_fail()
    
        let er _ = d.on_type_er d.trace <| sprintf "Type constructor application failed. %A does not intersect %A." uniont (get_type ra)
        apply_typec d uniont ra er <| function
            | TyVV([x],VVT [t]) when get_type x = t -> ret x // Implements rule #10 here.
            | x -> ret x // Both cases implement #8.
            
    let typec_union d a b ret =
        tev2 d a b <| fun a b ->
            match get_type a, get_type b with
            | TypeConstructorT a, TypeConstructorT b -> set_field a + set_field b |> UnionT |> TypeConstructorT |> ret
            | _ -> d.on_type_er d.trace <| sprintf "In type constructor union expected both types to be type constructors."

    let typec_create d x ret =
        let key = d.env, x
        let ret_tyv x = TypeConstructorT x |> make_tyv_and_push_ty d |> ret
        let add_to_memo_dict x = 
            memoized_methods.[key] <- MemoType x
            ret_tyv x
        let add_to_type_dict x =
            match memoized_methods.TryGetValue key with
            | true, MemoType (RecT tag) -> memoized_types.[tag] <- x; RecT tag
            | _ -> x
            
        match memoized_methods.TryGetValue key with
        | true, MemoType ty -> ret_tyv ty
        | true, MemoTypeInEvaluation -> type_tag() |> RecT |> add_to_memo_dict
        | true, _ -> failwith "Expected a type in the dictionary."
        | false, _ -> 
            memoized_methods.[key] <- MemoTypeInEvaluation
            // After the evaluation, if the type is recursive the dictionary should have its key.
            // If present it will return that instead.
            tev_seq d x (get_type >> add_to_type_dict >> add_to_memo_dict)

   
    let rec apply_closuret d clo (clo_arg_ty,clo_ret_ty) arg ret =
        let arg_ty = get_type arg
        if arg_ty <> clo_arg_ty then d.on_type_er d.trace <| sprintf "Cannot apply an argument of type %A to closure %A" arg_ty clo
        else TyOp(Apply,[clo;arg],clo_ret_ty) |> ret

    and apply_functiont tev d env_term (env_ty,(name,pat,body) as fun_key) args ret =
        let env = Map.add pat args env_term
        let fv = TyEnv (env, FunctionT fun_key)
        let d = {d with env = if name <> "" then Map.add name fv env else env}
        tev d body ret

    and apply tev d la ra ret =
        match la, get_type ra with
        | TyEnv(env_term,FunctionT(env_ty,x)), ForCastT t -> apply_cast d env_term (env_ty,x) t ret
        | x, ForCastT t -> d.on_type_er d.trace <| sprintf "Expected a function in type application. Got: %A" x
        | TyEnv(env_term,ModuleT env_ty), NameT n -> apply_module d env_term n ret
        | TyType(TypeConstructorT uniont), _ -> apply_typec d uniont ra ret
        | x, NameT n -> d.on_type_er d.trace <| sprintf "Expected a module or a type constructor in application. Got: %A" x
        | TyEnv(env_term,FunctionT(env_ty,x)),_ -> apply_functiont tev d env_term (env_ty,x) ra ret
        | _ ->
            match get_type la with
            | ClosureT(a,r) -> apply_closuret d la (a,r) ra ret
            | _ -> d.on_type_er d.trace "Invalid use of apply."

    and apply_cast d env_term (env_ty,core as fun_key) args_ty ret =
        let instantiate_type_as_variable d args_ty =
            let f x = make_tyv_and_push_ty d x
            match args_ty with
            | VVT l -> TyVV(List.map f l, args_ty)
            | x -> f x

        if env_num_args env_term > 0 then d.on_type_er d.trace <| sprintf "The number of implicit + explicit arguments to a closure exceeds one. Implicit args: %A" env_term
        else
            let args = instantiate_type_as_variable d args_ty
            let closure = TyEnv(env_term, FunctionT fun_key)

            let tev d x ret = 
                memoize_closure (get_type args) d x <| fun r ->
                    if is_returnable r then ret r
                    else d.on_type_er d.trace "Closure does not have a returnable type."
                    
            apply tev d closure args ret

    let apply_tev d expr args ret = 
        tev2 d expr args <| fun expr args ->
            apply tev d expr args ret

    let mset d a b ret =
        tev2 d a b <| fun l r ->
            match l, r with
            | TyOp((ArrayUnsafeIndex | ArrayIndex),[_;_],lt), r when lt = get_type r -> make_tyv_and_push_typed_expr d (TyOp(MSet,[l;r],BVVT)) |> ret
            | _ -> d.on_type_er d.trace <| sprintf "Error in mset. Expected: TyBinOp((ArrayUnsafeIndex | ArrayIndex),_,_,lt), r when lt = get_type r.\nGot: %A and %A" l r

    let vv_index d v i ret =
        tev2 d v i <| fun v i ->
            match v, i with
            | v, TyLit (LitInt32 i as i') ->
                match get_type v with
                | VVT ts -> 
                    if i >= 0 || i < List.length ts then TyOp(VVIndex,[v;TyLit i'],ts.[i]) |> ret
                    else d.on_type_er d.trace "(i >= 0 || i < List.length ts) = false in IndexVT"
                | x -> d.on_type_er d.trace <| sprintf "Type of a evaluated expression in IndexVT is not VTT.\nGot: %A" x
            | v, i -> d.on_type_er d.trace <| sprintf "Index into a tuple must be a natural number less than the size of the tuple.\nGot: %A" i
    
    let vv_cons d a b ret =
        tev2 d a b <| fun a b ->
            match b with
            | TyVV(b, VVT bt) -> TyVV(a::b, VVT (get_type a :: bt)) |> ret
            | _ -> d.on_type_er d.trace "Expected a tuple on the right is in VVCons."

    let guard_is_int d args ret = 
        if List.forall is_int args = false then d.on_type_er d.trace <| sprintf "An size argument in CreateArray is not of type int.\nGot: %A" args
        else ret()

    let array_create d pointer size typeof_expr ret =
        tev d size <| fun size -> 
            tev_seq d typeof_expr <| fun typ ->
                guard_is_int d (tuple_field size) <| fun () ->
                    let l = [size
                             get_type typ |> pointer |> make_tyv_and_push_ty d 
                             NameT "Array" |> make_tyv_and_push_ty d]
                    let x = TyVV (l, VVT (List.map get_type l))
                    TyOp(ArrayCreate,[x],get_type x) |> ret

    let array_index d op_index ar args ret =
        tev2 d ar args <| fun ar args ->
            let fargs = tuple_field args
            guard_is_int d fargs <| fun () ->
                match ar with
                | Array(_,size,typ) ->
                    let lar, largs = size.Length, fargs.Length
                    if lar = largs then TyOp(op_index,[ar;args],typ) |> ret
                    else d.on_type_er d.trace <| sprintf "The index lengths in %A do not match. %i <> %i" op_index lar largs
                | _ -> d.on_type_er d.trace <| sprintf "Array index needs the Array.Got: %A" ar

    let module_open d a b ret =
        tev d a <| fun a ->
            match a with
            | TyEnv(env_term, ModuleT env_ty) -> 
                let env = Map.fold (fun s k v -> Map.add k v s) d.env env_term
                tev {d with env = env} b ret
            | x -> d.on_type_er d.trace <| sprintf "The open expected a module type as input. Got: %A" x

    let module_missing_key_error x y ret =
        let mutable missing_key = None
        let env = Map.fold (fun s k v ->
            if Map.containsKey k s = false && missing_key.IsNone then missing_key <- Some k
            Map.add k v s) x y
        match missing_key with
        | Some k -> d.on_type_er d.trace <| sprintf "The key %s cannot be added to a module using `with` as it is not in it originally. Use the extensible `with'` if such behavior is desired." k
        | _ -> ret env

    let module_missing_key_ignore x y ret =
        Map.fold (fun s k v -> Map.add k v s) x y |> ret

    let module_with module_missing_key d a b c ret =
        tev2 d a b <| fun a b ->
            match get_type a, get_type b with
            | ModuleT x, ModuleT y -> module_missing_key x y <| fun env -> tev {d with env = env} c ret
            | _ -> d.on_type_er d.trace <| sprintf "The open expected a module type as input. Got: %A" (get_type a)

    let type_annot d a b ret =
        match d.rbeh with
        | AnnotationReturn -> tev d b (get_type >> make_tyv_and_push_ty d >> ret)
        | AnnotationDive ->
            tev d a <| fun a -> tev_seq d b <| fun b ->
                let ta, tb = get_type a, get_type b
                if ta = tb then ret a else d.on_type_er d.trace <| sprintf "%A <> %A" ta tb

    let prim_bin_op_template d check_error is_check k a b t ret =
        tev2 d a b <| fun a b ->
            if is_check a b then k t a b |> ret
            else d.on_type_er d.trace (check_error a b)

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

    let prim_un_op_template d check_error is_check k a t ret =
        tev d a <| fun a ->
            if is_check a then k t a |> ret
            else d.on_type_er d.trace (check_error a)

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

    let for_cast d x ret = tev_seq d x (get_type >> ForCastT >> make_tyv_and_push_ty d >> ret)
    let error_non_unit d a ret =
        tev d a <| fun x ->
            if get_type x <> BVVT then
                d.on_type_er d.trace <| "Only the last expression of a block is allowed to be unit. Use `ignore` if it intended to be such."
            else
                ret x

    let add_trace d pos =
        match pos with
        | Some x -> {d with trace = x :: d.trace}
        | None -> d
      
    let ret x = destructure d x |> ret
    match expr with
    | Lit (value,_) -> TyLit value |> ret
    | V (x, pos) -> v_find d.env x (fun () -> d.on_type_er (add_trace d pos).trace <| sprintf "Variable %A not bound." x) ret
    | Function (core, free_var_set, _) -> 
        let env_term = Set.fold (fun s x -> 
            match d.env.TryFind x with
            | Some ex -> Map.add x ex s
            | None -> s) Map.empty !free_var_set
        TyEnv(env_term, FunctionT(env_to_ty env_term, core)) |> ret
    | VV(vars, pos) -> vars_map (add_trace d pos) vars (fun vv -> TyVV(vv, VVT(List.map get_type vv)) |> ret)
    | Op(op,vars,pos) ->
        let d = add_trace d pos
        match op, vars with
        | If,[cond;tr;fl] -> if_ d cond tr fl ret
        | Apply,[a;b] -> apply_tev d a b ret
        | MethodMemoize,[a] -> memoize_method d a ret
        | ApplyType,[x] -> for_cast d x ret
        
        | ModuleOpen,[a;b] -> module_open d a b ret
        | StaticPrint,[a] -> tev d a <| fun r -> printfn "%A" r; ret TyB

        // Primitive operations on expressions.
        | Add,[a;b] -> prim_arith_op d a b Add ret
        | Sub,[a;b] -> prim_arith_op d a b Sub ret 
        | Mult,[a;b] -> prim_arith_op d a b Mult ret
        | Div,[a;b] -> prim_arith_op d a b Div ret
        | Mod,[a;b] -> prim_arith_op d a b Mod ret

        | LT,[a;b] -> prim_comp_op d a b LT ret
        | LTE,[a;b] -> prim_comp_op d a b LTE ret
        | EQ,[a;b] -> prim_comp_op d a b EQ ret
        | NEQ,[a;b] -> prim_comp_op d a b NEQ ret 
        | GT,[a;b] -> prim_comp_op d a b GT ret
        | GTE,[a;b] -> prim_comp_op d a b GTE ret
    
        | And,[a;b] -> prim_bool_op d a b And ret
        | Or,[a;b] -> prim_bool_op d a b Or ret

        | ShiftLeft,[a;b] -> prim_shift_op d a b ShiftLeft ret
        | ShiftRight,[a;b] -> prim_shift_op d a b ShiftRight ret

        | ShuffleXor,[a;b] -> prim_shuffle_op d a b ShuffleXor ret
        | ShuffleUp,[a;b] -> prim_shuffle_op d a b ShuffleUp ret
        | ShuffleDown,[a;b] -> prim_shuffle_op d a b ShuffleDown ret
        | ShuffleIndex,[a;b] -> prim_shuffle_op d a b ShuffleIndex ret

        | VVIndex,[a;b] -> vv_index d a b ret
        | VVCons,[a;b] -> vv_cons d a b ret

        | ArrayCreate,[a;b] -> array_create d LocalPointerT a b ret
        | ArrayCreateShared,[a;b] -> array_create d SharedPointerT a b ret
        | (ArrayUnsafeIndex | ArrayIndex),[a;b] -> array_index d op a b ret
        | MSet,[a;b] -> mset d a b ret
        | TypeAnnot,[a;b] -> type_annot d a b ret

        | Neg,[a] -> prim_un_numeric d a Neg ret
        | ErrorType,[a] -> tev d a <| fun a -> d.on_type_er d.trace <| sprintf "%A" a
        | ErrorNonUnit,[a] -> error_non_unit d a ret

        | Log,[a] -> prim_un_floating d a Log ret
        | Exp,[a] -> prim_un_floating d a Exp ret
        | Tanh,[a] -> prim_un_floating d a Tanh ret

        // Constants
        | BlockDimX,[] -> uint64 blockDim.x |> LitUInt64 |> TyLit |> ret
        | BlockDimY,[] -> uint64 blockDim.y |> LitUInt64 |> TyLit |> ret
        | BlockDimZ,[] -> uint64 blockDim.z |> LitUInt64 |> TyLit |> ret
        | GridDimX,[] -> uint64 gridDim.x |> LitUInt64 |> TyLit |> ret
        | GridDimY,[] -> uint64 gridDim.y |> LitUInt64 |> TyLit |> ret
        | GridDimZ,[] -> uint64 gridDim.z |> LitUInt64 |> TyLit |> ret

        | Syncthreads,[] -> TyOp(Syncthreads,[],BVVT) |> ret

        | ModuleCreate,[] -> TyEnv(d.env, ModuleT <| env_to_ty d.env) |> ret
        //| TypeCreate,[] -> env_to_ty d.env |> UnionT |> make_tyv_and_push_ty d |> ret // TODO: Work on this more.
        | _ -> failwith "Missing Op case."


/// Reasonable default for the dims.
let default_dims = dim3(256), dim3(20)

let print_type_error (code: Dictionary<string, string []>) (trace: Trace) message = 
    let error = System.Text.StringBuilder(1024)
    error.AppendLine message |> ignore
    let rec loop prev_file prev_line = function
        | (file, line: int64, col: int64) :: xs ->
            if prev_file <> file || prev_line <> line then
                let er_code = code.[file].[int line - 1]
                let er_file = if file <> "" then sprintf " in file \"%s\"." file else file
                error.AppendLine <| sprintf "Error trace on line: %i, column: %i%s" line col er_file |> ignore
                error.AppendLine er_code |> ignore
                let col = int (col - 1L)
                for i=1 to col do error.Append(' ') |> ignore
                error.AppendLine "^" |> ignore
            loop file line xs
        | [] -> error.ToString()
    loop "" -1L trace

let data_empty on_fail code =
    {ltag = ref 0L; seq=ref id; trace=[]; rbeh=AnnotationDive
     env = Map.empty
     cse_env = ref Map.empty
     on_type_er = fun trace message -> on_fail <| print_type_error code trace message}

let array_index op a b = Op(op,[a;b],None)

let core_functions =
    let p f = inl "x" (f (v "x")) None
    let p2 f = inl' ["x"; "y"] (f (v "x") (v "y")) None
    let con x = Op(x,[],None)
    let lit x = Lit (x, None)
    let l a b = l a b None
    s  [l "errortype" (p error_type)
        l "static_print" (p static_print)
        l "overload_ap_Array" (p2 (array_index ArrayIndex))
        l "unsafe_index" (p2 (array_index ArrayUnsafeIndex))

        l "threadIdxX" (lit ThreadIdxX)
        l "threadIdxY" (lit ThreadIdxY)
        l "threadIdxZ" (lit ThreadIdxZ)
        l "blockIdxX" (lit BlockIdxX)
        l "blockIdxY" (lit BlockIdxY)
        l "blockIdxZ" (lit BlockIdxZ)

        l "blockDimX" (con BlockDimX)
        l "blockDimY" (con BlockDimY)
        l "blockDimZ" (con BlockDimZ)
        l "gridDimX" (con GridDimX)
        l "gridDimY" (con GridDimY)
        l "gridDimZ" (con GridDimZ)
        ]

let spiral_typecheck code dims body on_fail ret = 
    let method_tag, memoized_methods = ref 0L, d0()
    let type_tag, memoized_type = ref 0L, d0()
    let d = data_empty on_fail code
    let input = core_functions body
    expr_free_variables input |> ignore // Is mutable
    let ret x = 
        typed_expr_optimization_pass 2 memoized_methods x // Is mutable
        ret (x,memoized_methods)
    expr_typecheck dims (method_tag, memoized_methods, type_tag, memoized_type) d input ret