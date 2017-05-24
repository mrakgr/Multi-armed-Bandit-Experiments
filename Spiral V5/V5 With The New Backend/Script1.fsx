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
and FunctionCore = string * (CudaPattern * Expr) list
and FunctionKey = Env * FunctionCore
and MemoKey = Env * Expr

and CudaPattern =
    | A of CudaPattern * string // Type annotation case
    | A' of CudaPattern * CudaTy
    | S of string
    | S' of string // match if not tuple
    | R of CudaPattern list * CudaPattern option // Tuple
    | F of CudaPattern * string // Functiona application with retracing.
    | N of string * CudaPattern // matches a tuple name and proceeds onto the pattern on a hit.

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
   
    | ShiftLeft
    | ShiftRight
    | ShuffleXor
    | ShuffleUp
    | ShuffleDown
    | ShuffleIndex

    // UnOps
    | Neg
    | TypeError
    | ModuleOpen
    
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

//    | VVZipReg
//    | VVZipIrreg
//    | VVUnzipReg
//    | VVUnzipIrreg

and Expr = 
    | V of string
    | T of TypedExpr
    | Lit of Value
    | Function of FunctionCore * Set<string> ref
    | VV of Expr list * string // named tuple
    | Op of Op * Expr list

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
    | LitFloat32 _ -> PrimT Float64T
    | LitFloat64 _ -> PrimT Float32T   
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
    | ForModuleT _ | ForApplyT _ | ModuleT _ | ClosureT _ | StructT _ | FunctionT _ -> false

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

let get_body_from (stack: Stack<unit -> TypedExpr>) = 
    if stack.Count > 0 then stack.Pop()()
    else failwith "The program is divergent."

let is_arg = function TyV _ -> true | _ -> false

let h0() = HashSet(HashIdentity.Structural)
let d0() = Dictionary(HashIdentity.Structural)

let fun_ name pat = Function((name,pat),ref Set.empty)
let inlr name x y = fun_ name [x,y]
let inl x y = inlr "" x y
let ap x y = Op(Apply,[x;y])
let ap_ty x = Op(ApplyType,[x])
let ap_mod x = Op(ApplyModule,[V x])
let l v b e = ap (inl v e) b
    
let meth_memo y = Op(MethodMemoize, [y])
let methr name x y = inlr name x (meth_memo y) 
let meth x y = methr "" x y

let module_create = Op(ModuleCreate,[])
let module_open a b = Op(ModuleOpen,[a;b])

let E = S ""
let B = VV ([], "")
let BVVT = VVT ([], "")
let TyB = TyVV([], BVVT)
/// Matches tuples without a tail.
let SS x = R (x, None) 
/// Opposite of S', matches only a tuple.
let SS' x = R ([], Some (S x)) 
/// Matches tuples with a tail.
let SSS a b = R(a, Some (S b)) 

let cons a b = Op(VVCons,[a;b])

let s l fin = List.foldBack (fun x rest -> x rest) l fin

let rec ap' f = function
    | x :: xs -> ap' (ap f x) xs
    | [] -> f

let match_ x pat = ap (fun_ "" pat) x
let function_ pat = fun_ "" pat

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

let vv x = VV(x,"")

let type_error x = Op(TypeError, [Lit <| LitString x])

let inline vars_union' init f l = List.fold (fun s x -> Set.union s (f x)) init l
let inline vars_union f l = vars_union' Set.empty f l

let expr_free_variables e = 
    let rec expr_free_variables vars_for_module e =
        let f e = expr_free_variables vars_for_module e
        let f' bound_vars e = expr_free_variables bound_vars e
        match e with
        | Op(ModuleCreate,[]) -> vars_for_module
        | Op(ApplyModule,_) -> Set.empty
    
        | V n -> Set.singleton n
        | Op(_,l) | VV(l,_) -> vars_union f l
        | Function((name,l),free_var_set) ->
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

let rec expr_typecheck' (gridDim, blockDim as dims) memoized_methods d expr ret =
    let tev d expr ret = expr_typecheck' dims memoized_methods d expr ret

    let mtag (mtag,ltag,seq,env,on_rec,on_match_fail,on_type_er) = mtag
    let mtag_with f (mtag,ltag,seq,env,on_rec,on_match_fail,on_type_er) = 
        (f mtag,ltag,seq,env,on_rec,on_match_fail,on_type_er)
    let ltag (mtag,ltag,seq,env,on_rec,on_match_fail,on_type_er) = ltag
    let ltag_with f (mtag,ltag,seq,env,on_rec,on_match_fail,on_type_er) =
        (mtag,f ltag,seq,env,on_rec,on_match_fail,on_type_er)
    let seq (mtag,ltag,seq,env,on_rec,on_match_fail,on_type_er) = seq
    let seq_with_empty (mtag,ltag,seq,env,on_rec,on_match_fail,on_type_er) = 
        (mtag,ltag,id,env,on_rec,on_match_fail,on_type_er)
    let env (mtag,ltag,seq,env,on_rec,on_match_fail,on_type_er) = env
    let env_with f (mtag,ltag,seq,env,on_rec,on_match_fail,on_type_er) = 
        (mtag,ltag,seq,f env,on_rec,on_match_fail,on_type_er)
    let on_rec (mtag,ltag,seq,env,on_rec,on_match_fail,on_type_er) = on_rec
    let on_rec_with f (mtag,ltag,seq,env,on_rec,on_match_fail,on_type_er) = 
        (mtag,ltag,seq,env,f on_rec,on_match_fail,on_type_er)
    let on_match_fail (mtag,ltag,seq,env,on_rec,on_match_fail,on_type_er) = on_match_fail
    let on_match_fail_with f (mtag,ltag,seq,env,on_rec,on_match_fail,on_type_er) = 
        (mtag,ltag,seq,env,on_rec,f on_match_fail,on_type_er)
    let on_type_er (mtag,ltag,seq,env,on_rec,on_match_fail,on_type_er) = on_type_er
    
    let v env x on_fail ret = 
        match Map.tryFind x env with
        | Some v -> ret v
        | None -> on_fail()

    let rec vars_map d l ret =
        match l with
        | x :: xs ->
            tev d x (fun (d,x) ->
                vars_map d xs (fun (d,x') -> ret (d, x :: x'))
                )
        | [] -> ret (d,[])

    let if_ d cond tr fl ret =
        tev d cond <| fun (d,cond) ->
            if is_bool cond = false then failwithf "Expected a bool in conditional.\nGot: %A" (get_type cond)
            else
                let on_rec' = on_rec d
                let env' = env d
                let restore d = 
                    on_rec_with (fun _ -> on_rec') d
                    |> env_with (fun _ -> env')

                let fin (d,(tr,fl)) =
                    let type_tr, type_fl = get_type tr, get_type fl
                    if type_tr = type_fl then 
                        match cond with
                        | TyLit(LitBool true) -> tr
                        | TyLit(LitBool false) -> fl
                        | _ -> TyOp(If,[cond;tr;fl],type_tr)
                        |> fun x -> ret (d,x)
                    else on_type_er d <| sprintf "Types in branches of If do not match.\nGot: %A and %A" type_tr type_fl

//                let d =
//                    on_rec_with (fun (d, ret) ->
//                        tev d fl (fun (d,fl) ->
//                            ret (d,fl,fun (d, tr) -> fin (restore d, (tr,fl)))
//                            )
//                        ) d

                tev d tr (fun (d,tr) ->
                    tev (restore d) fl (fun (d,fl) ->
                        fin (d,(tr,fl))  
                        )
                    ) 


    match expr with
    | Lit value -> (d,TyLit value) |> ret
    | T x -> (d,x) |> ret
    | V x -> v (env d) x (fun () -> on_type_er d <| sprintf "Variable %A not bound." x) (fun r -> ret (d,r))
    | Function (core, free_var_set) -> 
        let env = Map.filter (fun k _ -> Set.contains k !free_var_set) (env d)
        (d,TyType(FunctionT(env,core))) |> ret
    | VV(vars,name) ->
        vars_map d vars (fun (d,vv) -> (d, TyVV(vv, VVT(List.map get_type vv, name))) |> ret)


/// Reasonable default for the dims.
let default_dims = dim3(256), dim3(20)

//(mtag,ltag,seq,env,on_rec,on_match_fail,on_type_er as d) 

let data_empty () = 
    let on_rec _ = Fail "The method is divergent"
    let on_match_fail _ = Fail "All the match cases were exhausted."
    let on_type_er x = Fail x
    
    0L,0L,id,Map.empty,on_rec,on_match_fail,on_type_er
    
let ret (_,x) = Succ x
        
let t = expr_typecheck' default_dims (d0()) (data_empty()) (V "") ret

