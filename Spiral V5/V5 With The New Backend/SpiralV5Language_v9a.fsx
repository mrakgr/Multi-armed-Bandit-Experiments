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
    | StringT

type CudaTy =
    | PrimT of SpiralDeviceVarType
    | VVT of CudaTy list * string
    | FunctionT of FunctionKey
    | StructT of TypedExpr // Structs here are pretty much the opposite of what they are in C. They are a type level feature.
    | ClosureT of CudaTy * CudaTy // For now since I am compiling only to Cuda, closures will be not be allowed to capture variables.
    | ModuleT of Env
    | ForApplyT of CudaTy 
    | ForModuleT of string

and Tag = int64
and TyV = Tag * CudaTy
and Env = Map<string, TypedExpr>
and FunctionCore = string * (Pattern * Expr) list
and FunctionKey = Env * FunctionCore
and MemoKey = Env * Expr

and Pattern =
    | A of Pattern * string // Type annotation case
    | A' of Pattern * CudaTy
    | S of string
    | S' of string // match if not tuple
    | R of Pattern list * Pattern option // Tuple
    | F of Pattern * string // Functiona application with retracing.
    | N of string * Pattern // matches a tuple name and proceeds onto the pattern on a hit.

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

and Op =
    // TriOps
    | If

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
    | ApplyModule
    | MethodMemoize
    | StructCreate
    | VVIndex
    | VVCons

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
    | ThreadIdxX | ThreadIdxY | ThreadIdxZ
    | BlockIdxX | BlockIdxY | BlockIdxZ
    | BlockDimX | BlockDimY | BlockDimZ
    | GridDimX | GridDimY | GridDimZ

and PosKey = string * int64 * int64
and Pos = PosKey option

and Expr = 
    | V of string * Pos
    | T of TypedExpr * Pos
    | Lit of Value * Pos
    | Function of FunctionCore * Set<string> ref * Pos
    | VV of Expr list * string * Pos // named tuple
    | Op of Op * Expr list * Pos

and Arguments = Set<TyV> ref
and Renamer = Map<Tag,Tag>

and MemoExprType =
| MemoClosure
| MemoMethod

and LetType =
| LetStd
| LetInvisible

// This is being compiled to STLC, not System F, so no type variables are allowed in the processed AST.
and TypedExpr =
    | TyV of TyV
    | TyLet of LetType * TyV * TypedExpr * TypedExpr * CudaTy
    | TyLit of Value
    
    | TyVV of TypedExpr list * CudaTy
    | TyOp of Op * TypedExpr list * CudaTy
    | TyType of CudaTy
    | TyMemoizedExpr of MemoExprType * Arguments * Renamer * Tag * CudaTy

and MemoCases =
    | MethodInEvaluation of TypedExpr option
    | MethodDone of TypedExpr
// This key is for functions without arguments. It is intended that the arguments be passed in through the Environment.
and MemoDict = Dictionary<MemoKey, MemoCases * Tag * Arguments>
and ClosureDict = Dictionary<Tag, TypedExpr> 

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

let get_type = function
    | TyLit x -> get_type_of_value x
    | TyV (_,t) | TyLet(_,_,_,_,t) | TyMemoizedExpr(_,_,_,_,t)
    | TyVV(_,t) | TyOp(_,_,t) | TyType t -> t

let for_apply_unwrap x = 
    match get_type x with
    | ForApplyT x -> x
    | x -> x

/// Returns an empty string if not a tuple.
let tuple_name = function
    | TyVV(_,VVT (_,name)) -> name
    | _ -> ""

/// Wraps the argument in a list if not a tuple.
let tuple_field = function 
    | TyVV(args,_) -> args
    | x -> [x]

let tuple_field_ty = function 
    | VVT(x,_) -> x
    | x -> [x]

type ArrayType = Local | Shared | Global

let (|Array|_|) = function
    | TyVV([size;typ],VVT (_,name)) ->
        let f array_type = Some(array_type,tuple_field size,get_type typ)
        match name with
        | "Array" -> f Local
        | "ArrayShared" -> f Shared
        | "ArrayGlobal" -> f Global
        | _ -> None
    | _ -> None

let rec is_returnable' = function
    | VVT (x,name) -> List.forall is_returnable' x
    | ForModuleT _ | ForApplyT _ | ModuleT _ | StructT _ | FunctionT _ -> false
    | ClosureT (a,b) -> is_returnable' a && is_returnable' b
    | PrimT _ -> true
and is_returnable a = is_returnable' (get_type a)

let is_numeric' = function
    | PrimT x -> 
        match x with
        | UInt8T | UInt16T | UInt32T | UInt64T 
        | Int8T | Int16T | Int32T | Int64T 
        | Float32T | Float64T -> true
        | StringT | BoolT -> false
    | _ -> false
let is_numeric a = is_numeric' (get_type a)

let is_primt' = function
    | PrimT x -> true
    | _ -> false
let is_primt a = is_primt' (get_type a)

let rec is_comparable' = function
    | PrimT _ -> true
    | VVT (x,_) -> List.forall is_comparable' x
    | ClosureT(a,b) -> is_comparable' a && is_comparable' b
    | ForModuleT _ | ForApplyT _ | ModuleT _  | StructT _ | FunctionT _ -> false

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

let fun_ name pat pos = Function((name,pat),ref Set.empty,pos)
let inlr name x y = fun_ name [x,y]
let inl x y = inlr "" x y
let ap pos x y = Op(Apply,[x;y],pos)
let ap_ty pos x = Op(ApplyType,[x],pos)
let ap_mod pos x = Op(ApplyModule,[V (x, pos)], pos)
let l v b pos e = ap pos (inl v e pos) b
    
let meth_memo y = Op(MethodMemoize,[y],None)
let methr name x y pos = inlr name x (meth_memo y) pos
let meth x y = methr "" x y

let module_create pos = Op(ModuleCreate,[],pos)
let module_open pos a b = Op(ModuleOpen,[a;b],pos)

let E = S ""
let B = VV ([], "", None)
let BVVT = VVT ([], "")
let TyB = TyVV([], BVVT)
/// Matches tuples without a tail.
let SS x = R (x, None) 
/// Opposite of S', matches only a tuple.
let SS' x = R ([], Some (S x)) 
/// Matches tuples with a tail.
let SSS a b = R(a, Some (S b)) 

let t x = T(x,None)
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

let vv pos x = VV(x,"",pos)

let error_type x = Op(ErrorType, [x], None)
let static_print x = Op(StaticPrint,[x], None)

let get_pos = function
    | Lit(_,pos) | T(_,pos) | V(_,pos) | Function(_,_,pos) | VV(_,_,pos) | Op(_,_,pos) -> pos

let error_non_unit x = Op(ErrorNonUnit, [x], get_pos x)

let inline vars_union' init f l = List.fold (fun s x -> Set.union s (f x)) init l
let inline vars_union f l = vars_union' Set.empty f l

let expr_free_variables e = 
    let rec expr_free_variables vars_for_module e =
        let f e = expr_free_variables vars_for_module e
        let f' bound_vars e = expr_free_variables bound_vars e
        match e with
        | Op(ModuleCreate,[],_) -> vars_for_module
        | Op(ApplyModule,_,_) -> Set.empty
    
        | V (n,_) -> Set.singleton n
        | Op(_,l,_) | VV(l,_,_) -> vars_union f l
        | Function((name,l),free_var_set,_) ->
            let rec pat_template on_name on_expr p = 
                let g p = pat_template on_name on_expr p
                match p with
                | F (pat,var) | A (pat,var) -> on_expr (g pat) var
                | N (_,pat) | A' (pat,_) -> g pat
                | S x | S' x -> on_name x
                | R (l,Some o) -> vars_union' (g o) g l
                | R (l,None) -> vars_union g l

            let pat_free_vars p = pat_template (fun _ -> Set.empty) (fun s var -> Set.add var s) p
            let pat_bound_vars p = pat_template Set.singleton (fun s _ -> s) p

            let fv = vars_union (fun (pat,body) -> 
                let bound_vars = pat_bound_vars pat
                let vars_for_module = vars_for_module + bound_vars |> Set.add name
                pat_free_vars pat + (f' vars_for_module body - bound_vars |> Set.remove name)) l
            free_var_set := fv |> Set.remove ""
            fv
        | T _ | Lit _ -> Set.empty

    expr_free_variables Set.empty e


let renamer_make s = Set.fold (fun (s,i) (tag,ty) -> Map.add tag i s, i+1L) (Map.empty,0L) s |> fst
let renamer_apply_pool r s = Set.map (fun (tag,ty) -> Map.find tag r, ty) s

let renamer_reverse r = 
    Map.fold (fun s k v -> Map.add v k s) Map.empty r
    |> fun x -> if r.Count <> x.Count then failwith "The renamer is not bijective." else x

let rec renamer_apply_env r e = Map.map (fun _ v -> renamer_apply_typedexpr r v) e
and renamer_apply_typedexpr r e =
    let f e = renamer_apply_typedexpr r e
    let g e = renamer_apply_ty r e
    match e with
    | TyV (n,t) -> TyV (Map.find n r,g t)
    | TyType t -> TyType (g t)
    | TyVV(l,t) -> TyVV(List.map f l,g t)
    | TyMemoizedExpr(typ,used_vars,renamer,tag,t) -> 
        let renamer = Map.map (fun _ v -> Map.find v r) renamer
        let used_vars = ref <| renamer_apply_pool r !used_vars
        TyMemoizedExpr(typ,used_vars,renamer,tag,g t)
    | TyOp(o,l,t) -> TyOp(o,List.map f l,g t)
    | TyLet(le,(n,t),a,b,t') -> TyLet(le,(Map.find n r,g t),f a,f b,g t')
    | TyLit _ -> e
and renamer_apply_ty r e = 
    let f e = renamer_apply_ty r e
    match e with
    | FunctionT(e,t) -> FunctionT(renamer_apply_env r e,t)
    | StructT e -> StructT(renamer_apply_typedexpr r e)
    | ClosureT(a,b) -> ClosureT(f a, f b)
    | ForApplyT t -> ForApplyT (f t)
    | PrimT _ | ForModuleT _ -> e
    | ModuleT e -> ModuleT (renamer_apply_env r e)
    | VVT (l,n) -> VVT (List.map f l, n)

let rec typed_expr_free_variables on_method_call e =
    let inline f e = typed_expr_free_variables on_method_call e
    let inline g e = ty_free_variables on_method_call e
    match e with
    | TyV (n,t) -> g t |> Set.add (n,t)
    | TyType t -> g t
    | TyOp(_,l,t) -> vars_union f l + g t
    | TyVV(l,t) -> vars_union f l
    | TyMemoizedExpr(typ,used_vars,renamer,tag,t) -> on_method_call typ used_vars renamer tag + g t
    | TyLet(_,x,a,b,t) -> Set.remove x (f b) + f a
    | TyLit _ -> Set.empty

and ty_free_variables on_method_call x = 
    let f x = ty_free_variables on_method_call x
    match x with
    | ModuleT env | FunctionT(env,_) -> env_free_variables on_method_call env
    | StructT e -> typed_expr_free_variables on_method_call e
    | ClosureT(a,b) -> f a + f b
    | ForApplyT t -> f t
    | PrimT _ | ForModuleT _ -> Set.empty
    | VVT (l,n) -> vars_union f l

and env_free_variables on_method_call env = 
    Map.fold (fun s _ v -> typed_expr_free_variables on_method_call v + s) Set.empty env

let on_method_call_typechecking_pass _ used_vars _ _ = !used_vars
let env_num_args env = 
    Map.fold (fun s k v -> 
        let f = typed_expr_free_variables on_method_call_typechecking_pass v
        if Set.isEmpty f then s else s+1) 0 env

let memo_value = function
    | MethodDone e, tag, args -> e, tag, args
    | _ -> failwith "impossible"

/// Optimizes the free variables for the sake of tuple deforestation.
/// It needs at least two passes to converge properly. And probably exactly two.
let typed_expr_optimization_pass num_passes (memo: MemoDict) =
    let rec on_method_call_optimization_pass (memo: (Set<Tag * CudaTy> * int) []) (expr_map: Map<Tag,TypedExpr * Arguments>) typ r renamer tag =
        match typ with
        | MemoMethod ->
            let vars,counter = memo.[int tag]
            let set_vars vars = r := renamer_apply_pool renamer vars; !r
            if counter < num_passes then
                let counter = counter + 1
                memo.[int tag] <- vars, counter
                let ty_expr, arguments = expr_map.[tag]
                let vars = typed_expr_free_variables (on_method_call_optimization_pass memo expr_map) ty_expr
                arguments := vars
                memo.[int tag] <- vars, counter
                set_vars vars
            else
                set_vars vars
        | MemoClosure -> 
            let ty_expr, arguments = expr_map.[tag]
            if Set.isEmpty !r = false && Set.isEmpty !arguments then 
                arguments := renamer_apply_pool (renamer_reverse renamer) !r
            !r

    let memo = Seq.map (memo_value >> (fun (e,tag,args) -> tag,(e,args))) memo.Values |> Map
    typed_expr_free_variables (on_method_call_optimization_pass (Array.init memo.Count (fun _ -> Set.empty,0)) memo)

type Trace = PosKey list

type LangEnv<'a,'c,'d,'q,'e,'r> =
    {
    ltag : int64 ref
    seq : (TypedExpr -> TypedExpr) ref
    env : Map<string,TypedExpr>
    trace : Trace
    on_match_break : 'a -> 'q
    on_type_er : Trace -> 'c -> 'e
    on_rec : 'd -> 'r
    }

let rec expr_typecheck (gridDim: dim3, blockDim: dim3 as dims) method_tag (memoized_methods: Dictionary<_,_>) 
        (d : LangEnv<_,_,_,_,_,_>) (expr: Expr) ret =
    let inline tev d expr ret = expr_typecheck dims method_tag memoized_methods d expr ret
    let inline apply_seq d ret x = ret (!d.seq x)
    let inline tev_seq d expr ret = tev {d with seq=ref id} expr (apply_seq d ret)
    let inline tev_if d expr on_rec ret = tev_seq {d with on_rec=on_rec} expr ret
    let inline tev_method d expr ret = tev_seq {d with ltag=ref 0L} expr ret
    let inline tev_match_f on_match_break d expr ret = tev_seq {d with on_match_break=on_match_break} expr ret

    let v_find env x on_fail ret = 
        match Map.tryFind x env with
        | Some v -> ret v
        | None -> on_fail()

    let rec vars_map d l ret =
        match l with
        | x :: xs ->
            tev d x (fun x ->
                vars_map d xs (fun x' -> ret (x :: x'))
                )
        | [] -> ret []

    let if_ d cond tr fl ret =
        tev d cond <| fun cond ->
            if is_bool cond = false then d.on_type_er d.trace <| sprintf "Expected a bool in conditional.\nGot: %A" (get_type cond)
            else
                let fin (tr,fl) =
                    let type_tr, type_fl = get_type tr, get_type fl
                    if type_tr = type_fl then 
                        match cond with
                        | TyLit(LitBool true) -> tr
                        | TyLit(LitBool false) -> fl
                        | _ -> TyOp(If,[cond;tr;fl],type_tr)
                        |> ret
                    else d.on_type_er d.trace <| sprintf "Types in branches of If do not match.\nGot: %A and %A" type_tr type_fl

                let mutable fl' = None
                tev_if d tr 
                    (fun ret' ->
                        tev_if d fl 
                            d.on_rec
                            (fun fl -> 
                                fl' <- Some fl
                                ret' (get_type fl))
                        )
                    (fun tr ->
                        match fl' with
                        | Some fl -> fin (tr,fl)
                        | None ->
                            tev_if d fl 
                                (fun ret' -> ret' (get_type tr))
                                (fun fl -> fin (tr,fl))
                        )

    let get_tag d = 
        let t = !d.ltag
        d.ltag := t + 1L
        t

    let make_tyv d ty_exp = get_tag d, get_type ty_exp

    let make_tyv_and_push' le d ty_exp =
        let v = make_tyv d ty_exp
        let seq = !d.seq
        d.seq := fun rest -> TyLet(le,v,ty_exp,rest,get_type rest) |> seq
        v

    let make_tyv_and_push d ty_exp = make_tyv_and_push' LetStd d ty_exp |> TyV
    let make_tyv_and_push_inv d ty_exp = make_tyv_and_push' LetInvisible d ty_exp |> TyV

    let struct_create d ty_exp = TyOp(StructCreate, [ty_exp], StructT ty_exp) |> make_tyv_and_push d

    // for a shallow version, take a look at `alternative_destructure_v6e.fsx`.
    // The deep version can also be straightforwardly derived from a template of this using the Y combinator.
    let destructure_deep d r = 
        let rec destructure_deep r = 
            let destructure_tuple r =
                match get_type r with
                | VVT (tuple_types, name) -> 
                    let indexed_tuple_args = List.mapi (fun i typ -> 
                        destructure_deep <| TyOp(VVIndex,[r;TyLit <| LitInt32 i],typ)) tuple_types
                    TyVV(indexed_tuple_args, VVT (tuple_types, name))
                | _ -> r
            match r with
            | TyType _ | TyLit _ -> r
            | TyOp((ArrayUnsafeIndex | ArrayIndex),[Array(_,[],_);_],_)
            | TyV _ | TyOp (VVIndex,[_;_],_) -> destructure_tuple r
            | TyVV(l,t) -> TyVV(List.map destructure_deep l,t)
            | TyMemoizedExpr _ | TyLet _ | TyOp _ -> make_tyv_and_push d r |> destructure_deep
        destructure_deep r

    let method_tag () =
        let tag = !method_tag
        method_tag := tag + 1L
        tag

    let eval_method d expr ret =
        let key_args = d.env, expr
        match memoized_methods.TryGetValue key_args with
        | false, _ ->
            let tag = method_tag ()

            memoized_methods.[key_args] <- (MethodInEvaluation(None), tag, ref Set.empty)
            tev_method d expr <| fun typed_expr ->
                memoized_methods.[key_args] <- (MethodDone(typed_expr), tag, ref Set.empty)
                ret (typed_expr, tag)
        | true, (MethodInEvaluation None, tag, args) ->
            d.on_rec <| fun ty ->
                let typed_expr = TyType ty
                memoized_methods.[key_args] <- (MethodInEvaluation (Some typed_expr), tag, args)
                ret (typed_expr, tag)
        | true, (MethodInEvaluation (Some typed_expr), tag, _) -> ret (typed_expr, tag)
        | true, (MethodDone typed_expr, tag, _) -> ret (typed_expr, tag)

    let eval_renaming d expr ret = 
        let env = d.env

        let fv = env_free_variables on_method_call_typechecking_pass env
        let renamer = renamer_make fv
        let renamed_env = renamer_apply_env renamer env

        eval_method {d with env=renamed_env; ltag=ref <| int64 renamer.Count} expr <| fun (typed_expr, tag) ->
            let typed_expr_ty = get_type typed_expr
            if is_returnable' typed_expr_ty = false then d.on_type_er d.trace <| sprintf "The following is not a type that can be returned from a method. Consider using Inlineable instead. Got: %A" typed_expr
            else ret (ref fv, renamer_reverse renamer, tag, typed_expr_ty)

    let memoize_method d x ret = 
        eval_renaming d x <| fun (a,b,c,d) -> 
            TyMemoizedExpr(MemoMethod,a,b,c,d)
            |> ret

    let memoize_closure arg_ty d x ret =
        eval_renaming d x <| fun (a,b,c,ret_ty) ->
            TyMemoizedExpr(MemoClosure,a,b,c,ClosureT(arg_ty,ret_ty))
            |> make_tyv_and_push d
            |> ret

    let inline match_bind acc arg_name x = if is_full_name arg_name then Map.add arg_name x acc else acc
    let inline match_r recurse acc (l,ls,ls') (r,rs) (t,ts) ret =
        recurse acc l r <| fun x_acc ->
            recurse x_acc (R(ls,ls')) (TyVV(rs,VVT (ts, ""))) ret
    let inline match_a' annot args on_fail ret = if annot = get_type args then ret() else on_fail()

    let apply_module d module_ b ret = 
        v_find module_ b (fun () -> d.on_type_er d.trace <| sprintf "Cannot find a function named %s inside the module." b) ret
    
    let rec match_a d annot args on_fail ret = 
        tev d (V (annot,None)) <| fun r ->
            match_a' (get_type r) args on_fail ret

    and match_single d (acc: Env) l r on_fail ret =
        let rec recurse acc l r ret = 
            match l,r with
            | A (pattern,annot), _ -> match_a d annot r on_fail (fun _ -> recurse acc pattern r ret) 
            | A' (pattern,annot), _ -> match_a' annot r on_fail (fun _ -> recurse acc pattern r ret) 
            | F (pattern, meth), args -> match_f d acc pattern args meth on_fail ret
            | R([],None), TyVV([], _) -> match_bind acc "" r |> ret
            | S' x, TyVV _ -> on_fail () //<| "S' matched a tuple."
            | S' x, _ | S x, _ -> match_bind acc x r |> ret
            | R([],Some ls), TyVV _ -> recurse acc ls r ret
            | R(l::ls,ls'), TyVV(r :: rs,VVT (t :: ts, "")) -> match_r recurse acc (l,ls,ls') (r,rs) (t,ts) ret
            //| R(l::ls,ls'), TyVV(r :: rs,VVT (t :: ts, _)) -> on_fail() // <| "Expecting an unnamed argument."
            | R([],None), TyVV(_, _) -> on_fail () // <| sprintf "More arguments than can be matched in R."
            | R _, _ -> on_fail () //<| sprintf "Cannot destructure %A." r
            | N (name, next), TyVV(x, VVT(t, name')) ->
                if name = name' then recurse acc next (TyVV(x,VVT (t, ""))) ret
                else on_fail() // <| sprintf "Cannot pattern match %s against %s" name name'
            | N _, _ -> on_fail() // "Cannot match name against a non-named argument."
        recurse acc l r ret

    and match_all d (env: Env) l (args: TypedExpr) on_fail ret =
        let rec loop = function
            | (pattern: Pattern, body: Expr) :: xs ->
                match_single d env pattern args
                    (fun _ -> loop xs)
                    (fun acc -> ret (acc, body))
            | [] -> on_fail (l,args) //"All patterns in the matcher failed to match."
        loop l

    and match_f d acc (pattern: Pattern) args meth on_fail ret =
        tev d (v meth) <| fun r ->
            apply (tev_match_f on_fail) d r args
                (fun _ -> on_fail()) // <| "Function application in pattern matcher failed to match a pattern."
                (fun r -> 
                    match_single d acc pattern (destructure_deep d r)
                        (fun _ -> d.on_type_er d.trace "The function call subpattern in the matcher failed to match.")
                        ret)

    and apply_closuret d clo (clo_arg_ty,clo_ret_ty) arg ret =
        let arg_ty = get_type arg
        if arg_ty <> clo_arg_ty then d.on_type_er d.trace <| sprintf "Cannot apply an argument of type %A to closure %A" arg_ty clo
        else TyOp(Apply,[clo;arg],clo_ret_ty) |> ret

    and apply_functiont tev d (initial_env,(name,l) as fun_key) args on_fail ret =
        match_all d initial_env l args
            on_fail
            (fun (env, body) ->
                let fv = TyType (FunctionT fun_key)
                let d = {d with env = if name <> "" then Map.add name fv env else env}
                tev d body ret)

    and apply_named_tuple tev d name ra on_fail ret =
        let oname = "overload_ap_" + name
        v_find d.env oname (fun () -> d.on_type_er d.trace <| sprintf "Cannot find an application overload for the tuple %s. %s is not in scope." name oname) <| fun la ->
            apply tev d la ra on_fail ret

    and apply tev d la ra on_fail ret =
        match get_type la, get_type ra with
        | FunctionT x, ForApplyT t -> apply_type d x t on_fail ret
        | x, ForApplyT t -> d.on_type_er d.trace <| sprintf "Expected a function type in type application. Got: %A" x
        | ModuleT x, ForModuleT n -> apply_module d x n ret
        | x, ForModuleT n -> d.on_type_er d.trace <| sprintf "Expected a module type in module application. Got: %A" x
        | FunctionT x,_ -> apply_functiont tev d x ra on_fail ret
        | ClosureT(a,r),_ -> apply_closuret d la (a,r) ra ret
        | (VVT(_,name) | StructT(TyVV(_,VVT(_,name)))),_ when name <> "" -> apply_named_tuple tev d name ra on_fail ret
        | _ -> d.on_type_er d.trace "Trying to apply a type other than InlineableT or MethodT."

    and apply_type d (env,core as fun_key) args_ty on_fail ret =
        if env_num_args env > 0 then d.on_type_er d.trace <| sprintf "The number of implicit + explicit arguments to a closure exceeds one. Implicit args: %A" env
        else
            let args =
                let f x = TyType x |> make_tyv_and_push_inv d
                match args_ty with
                | VVT(l,n) -> TyVV(List.map f l, args_ty)
                | x -> f x
            let f = FunctionT fun_key |> TyType |> t
            let g = FunctionT(env,("",[S " ",ap None f (v " ")])) |> TyType
            let tev d x = memoize_closure (get_type args) d x
            apply tev d g args on_fail ret

    let apply_tev d expr args apply_fail ret = 
        tev d expr <| fun expr ->
            tev d args <| fun args ->
                let args = destructure_deep d args
                apply tev d expr args apply_fail ret

    let inline tev2 d a b ret =
        tev d a <| fun l ->
            tev d b <| fun r ->
                ret l r

    let mset d a b ret =
        tev2 d a b <| fun l r ->
            let r = destructure_deep d r
            match l, r with
            | TyOp((ArrayUnsafeIndex | ArrayIndex),[_;_],lt), r when lt = get_type r -> make_tyv_and_push d (TyOp(MSet,[l;r],BVVT)) |> ret
            | _ -> d.on_type_er d.trace <| sprintf "Error in mset. Expected: TyBinOp((ArrayUnsafeIndex | ArrayIndex),_,_,lt), r when lt = get_type r.\nGot: %A and %A" l r

    let vv_index d v i ret =
        tev2 d v i <| fun v i ->
            match v, i with
            | v, TyLit (LitInt32 i as i') ->
                match get_type v with
                | VVT (ts,"") -> 
                    if i >= 0 || i < List.length ts then TyOp(VVIndex,[v;TyLit i'],ts.[i]) |> ret
                    else d.on_type_er d.trace "(i >= 0 || i < List.length ts) = false in IndexVT"
                | VVT (ts, name) -> d.on_type_er d.trace <| sprintf "Named tuples (%s) can't be indexed directly. They must be pattern matched on the name first." name
                | x -> d.on_type_er d.trace <| sprintf "Type of a evaluated expression in IndexVT is not VTT.\nGot: %A" x
            | v, i -> d.on_type_er d.trace <| sprintf "Index into a tuple must be a natural number less than the size of the tuple.\nGot: %A" i
    
    let vv_cons d a b ret =
        tev2 d a b <| fun a b ->
            let b = destructure_deep d b
            match b with
            | TyVV(b, VVT (bt, "")) -> TyVV(a::b, VVT (get_type a :: bt, "")) |> ret
            | TyVV(_, VVT (_, name)) -> d.on_type_er d.trace <| sprintf "Named tuples (%s) can't be cons'd directly. They must be pattern matched on the name first." name 
            | _ -> d.on_type_er d.trace "Expected a tuple on the right is in VVCons."

    let guard_is_int d args ret = 
        if List.forall is_int args = false then d.on_type_er d.trace <| sprintf "An size argument in CreateArray is not of type int.\nGot: %A" args
        else ret()

    let array_create d name size typeof_expr ret =
        tev2 d size typeof_expr <| fun size typ ->
            destructure_deep d size
            |> fun x -> 
                guard_is_int d (tuple_field x) <| fun () ->
                    let l = [size; TyType(get_type typ)]
                    TyVV (l, VVT (List.map get_type l, name))
                    |> struct_create d
                    |> ret

    let array_index d safe_or_not ar args ret =
        tev2 d ar args <| fun ar args ->
            let fargs = tuple_field args
            guard_is_int d fargs <| fun () ->
                match get_type ar with
                | StructT (Array(_,size,typ)) ->
                    let lar, largs = size.Length, fargs.Length
                    if lar = largs then TyOp(safe_or_not,[ar;args],typ) |> ret
                    else d.on_type_er d.trace <| sprintf "The index lengths in %A do not match. %i <> %i" safe_or_not lar largs
                | _ -> d.on_type_er d.trace <| sprintf "Array index needs the Array.Got: %A" ar

    let module_open d a b ret =
        tev d a <| fun a ->
            match get_type a with
            | ModuleT x -> 
                let env = Map.fold (fun s k v -> Map.add k v s) d.env x
                tev {d with env = env} b ret
            | x -> d.on_type_er d.trace <| sprintf "The open expected a module type as input. Got: %A" x

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
        prim_bin_op_template d er check prim_bin_op_helper

    let prim_comp_op d = 
        let er = sprintf "`(is_numeric a || is_bool a) && get_type a = get_type b` is false.\na=%A, b=%A"
        let check a b = (is_numeric a || is_bool a) && get_type a = get_type b
        prim_bin_op_template d er check bool_helper

    let prim_bool_op d = 
        let er = sprintf "`is_bool a && get_type a = get_type b` is false.\na=%A, b=%A"
        let check a b = is_bool a && get_type a = get_type b
        prim_bin_op_template d er check bool_helper

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
        prim_un_op_template d er check prim_un_op_helper

    let for_apply_type d x ret =
        tev_seq d x (get_type >> ForApplyT >> TyType >> ret)

    let error_non_unit d a ret =
        tev d a <| fun x ->
            if get_type x <> BVVT then
                d.on_type_er d.trace <| sprintf "Only the last expression of a block is allowed to be unit. Use `ignore` if it intended to be such. %A" x
            else
                ret x

    let add_trace d pos = 
        match pos with
        | Some x -> {d with trace = x :: d.trace}
        | None -> d
        
    match expr with
    | Lit (value,_) -> TyLit value |> ret
    | T (x, _) -> x |> ret
    | V (x, pos) -> v_find d.env x (fun () -> d.on_type_er (add_trace d pos).trace <| sprintf "Variable %A not bound." x) ret
    | Function (core, free_var_set, _) -> 
        let env = Map.filter (fun k _ -> Set.contains k !free_var_set) d.env
        TyType(FunctionT(env,core)) |> ret
    | VV(vars,name, pos) -> vars_map (add_trace d pos) vars (fun vv -> TyVV(vv, VVT(List.map get_type vv, name)) |> ret)

    | Op(op,vars,pos) ->
        let d = add_trace d pos
        match op, vars with
        | If,[cond;tr;fl] -> if_ d cond tr fl ret
        | Apply,[a;b] -> apply_tev d a b (fun _ -> d.on_type_er d.trace "All the match cases were exhausted.") ret
        | MethodMemoize,[a] -> memoize_method d a ret
        | ApplyType,[x] -> for_apply_type d x ret
        
        | ApplyModule,[V (x,_)] -> x |> ForModuleT |> TyType |> ret
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

        | ArrayCreate,[a;b] -> array_create d "Array" a b ret
        | ArrayCreateShared,[a;b] -> array_create d "ArrayShared" a b ret
        | (ArrayUnsafeIndex | ArrayIndex),[a;b] -> array_index d op a b ret
        | MSet,[a;b] -> mset d a b ret

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
        | (ThreadIdxX | ThreadIdxY 
               | ThreadIdxZ | BlockIdxX | BlockIdxY 
               | BlockIdxZ as constant),[] -> TyOp(constant,[],PrimT UInt64T) |> ret

        | ModuleCreate,[] -> TyType(ModuleT d.env) |> ret
        | _ -> failwith "Missing Op case."


/// Reasonable default for the dims.
let default_dims = dim3(256), dim3(20)

let print_type_error (code: string) (trace: Trace) message = 
    let code = code.Split [|'\n'|]
    let error = System.Text.StringBuilder(1024)
    error.AppendLine message |> ignore
    let rec loop prev_file prev_line = function
        | (file, line: int64, col: int64) :: xs ->
            if prev_file <> file || prev_line <> line then
                let er_code = code.[int line - 1]
                let er_file = if file <> "" then sprintf " in file \"%s\"." file else file
                error.AppendLine <| sprintf "Error trace on line: %i, column: %i%s" line col er_file |> ignore
                error.AppendLine er_code |> ignore
                let col = int (col - 1L)
                for i=1 to col do error.Append(' ') |> ignore
                error.AppendLine "^" |> ignore
            loop file line xs
        | [] -> error.ToString()
    loop "" -1L trace

let data_empty code =
    let on_match_break _ = Fail "The match broke to the top level."
    let on_type_er trace message = Fail <| print_type_error code trace message
    let on_rec _ = Fail "The method is divergent"
    {ltag=ref 0L;seq=ref id;trace=[];env=Map.empty;
     on_match_break=on_match_break;on_type_er=on_type_er;on_rec=on_rec}

let array_index op a b = Op(op,[a;b],None)

let core_functions =
    let p f = inl (S "x") (f (v "x")) None
    let p2 f = inl' [S "x"; S "y"] (f (v "x") (v "y")) None
    let con x = Op(x,[],None)
    let l a b = l a b None
    s  [l (S "errortype") (p error_type)
        l (S "static_print") (p static_print)
        l (S "overload_ap_Array") (p2 (array_index ArrayIndex))
        l (S "unsafe_index") (p2 (array_index ArrayUnsafeIndex))

        l (S "threadIdxX") (con ThreadIdxX)
        l (S "threadIdxY") (con ThreadIdxY)
        l (S "threadIdxZ") (con ThreadIdxZ)
        l (S "blockIdxX") (con BlockIdxX)
        l (S "blockIdxY") (con BlockIdxY)
        l (S "blockIdxZ") (con BlockIdxZ)

        l (S "blockDimX") (con BlockDimX)
        l (S "blockDimY") (con BlockDimY)
        l (S "blockDimZ") (con BlockDimZ)
        l (S "gridDimX") (con GridDimX)
        l (S "gridDimY") (con GridDimY)
        l (S "gridDimZ") (con GridDimZ)
        ]
   
let spiral_typecheck code dims body = 
    let method_tag = ref 0L
    let memoized_methods = d0()
    let d = data_empty code
    let input = core_functions body
    expr_free_variables input |> ignore // Is mutable
    let deforest_tuples x = 
        typed_expr_optimization_pass 2 memoized_methods x |> ignore // Is mutable
        Succ(x,memoized_methods)
    expr_typecheck dims method_tag memoized_methods d input deforest_tuples
