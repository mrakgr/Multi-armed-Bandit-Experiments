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

let fun_tag = -1L

type CudaTy =
    | PrimT of SpiralDeviceVarType
    | VVT of CudaTy list * string
    | FunctionT of FunctionKey
    | StructT of TypedCudaExpr

and TyV = int64 * CudaTy
and Env = Map<string, TypedCudaExpr>
and FunctionCore = string * (CudaPattern * CudaExpr) list
and FunctionKey = Env * FunctionCore
and MemoKey = Env * CudaExpr

and CudaPattern =
    | A of CudaPattern * string // Type annotation case
    | A' of CudaPattern * CudaTy
    | S of string
    | S' of string // match if not tuple
    | R of CudaPattern list * CudaPattern option // Tuple
    | F of CudaPattern * string // Functiona application with retracing.
    | N of string * CudaPattern // matches a tuple name and proceeds onto the pattern on a hit.

and Value = 
    | LitUInt32 of uint32
    | LitUInt64 of uint64
    | LitInt32 of int
    | LitInt64 of int64
    | LitFloat32 of float32
    | LitFloat64 of float
    | LitBool of bool
    | LitString of string

and BinOp =
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
    | VVIndex
    | VVCons

    | ArrayCreate
    | ArrayCreateShared
    | ArrayIndex

and UnOp = 
    | Neg
    | CallAsMethod
    | TypeError
    | StructCreate
//    | VVZipReg
//    | VVZipIrreg
//    | VVUnzipReg
//    | VVUnzipIrreg

and CudaExpr = 
    | V of string
    | Lit of Value
    | Function of FunctionCore * Set<string> ref
    | VV of CudaExpr list * string // named tuple
    | If of CudaExpr * CudaExpr * CudaExpr
    | BinOp of BinOp * CudaExpr * CudaExpr
    | UnOp of UnOp * CudaExpr

// This is being compiled to STLC, not System F, so no type variables are allowed in the processed AST.
and TypedCudaExpr =
    | TyV of TyV
    | TyIf of TypedCudaExpr * TypedCudaExpr * TypedCudaExpr * CudaTy
    | TyLet of TyV * TypedCudaExpr * TypedCudaExpr * CudaTy
    | TyLit of Value
    
    | TyVV of TypedCudaExpr list * CudaTy
    | TyBinOp of BinOp * TypedCudaExpr * TypedCudaExpr * CudaTy
    | TyUnOp of UnOp * TypedCudaExpr * CudaTy
    | TyMethodCall of Set<int64> * int64 * CudaTy

and MemoCases =
    | MethodInEvaluation of TypedCudaExpr option
    | MethodDone of TypedCudaExpr
// This key is for functions without arguments. It is intended that the arguments be passed in through the Environment.
and MemoDict = Dictionary<MemoKey, MemoCases * int64> 

and LangEnv =
    {
    // Immutable
    env : Env
    // Mutable
    memoized_methods : MemoDict // For typechecking recursive functions.
    sequences : (TypedCudaExpr -> TypedCudaExpr) option ref // For sequencing statements.
    recursive_methods_stack : Stack<unit -> TypedCudaExpr> // For typechecking recursive calls.
    method_tag : int64 ref
    mutable let_tag : int64
    }

type Result<'a,'b> = Succ of 'a | Fail of 'b

let get_type_of_value = function
    | LitUInt32 _ -> PrimT UInt32T
    | LitUInt64 _ -> PrimT UInt64T
    | LitInt32 _ -> PrimT Int32T
    | LitInt64 _ -> PrimT Int64T
    | LitFloat32 _ -> PrimT Float64T
    | LitFloat64 _ -> PrimT Float32T   
    | LitBool _ -> PrimT BoolT
    | LitString _ -> PrimT StringT

let get_type = function
    | TyLit x -> get_type_of_value x
    | TyV (_,t) | TyIf(_,_,_,t) | TyLet(_,_,_,t) | TyMethodCall(_,_,t)
    | TyVV(_,t) | TyBinOp(_,_,_,t) | TyUnOp(_,_,t) -> t

/// Returns an empty string if not a tuple.
let tuple_name = function
    | TyVV(_,VVT (_,name)) -> name
    | _ -> ""

/// Wraps the argument in a list if not a tuple.
let tuple_field = function 
    | TyVV(args,_) -> args
    | x -> [x]

let is_returnable_tuple_blacklist = [|"Array";"ArrayShared"|]

let rec is_returnable' = function
    | VVT (x,name) -> Array.contains name is_returnable_tuple_blacklist = false && List.forall is_returnable' x
    | StructT e -> is_returnable e
    | FunctionT _ -> false
    | _ -> true
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

let is_comparable' = function
    | PrimT _ | VVT _ -> true
    | StructT _ | FunctionT _ -> false

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

let get_body_from (stack: Stack<unit -> TypedCudaExpr>) = 
    if stack.Count > 0 then stack.Pop()()
    else failwith "The program is divergent."

let is_arg = function TyV _ -> true | _ -> false

let h0() = HashSet(HashIdentity.Structural)
let d0() = Dictionary(HashIdentity.Structural)

let apply_sequences sequences x =
    match sequences with
    | Some sequences -> sequences x
    | None -> x


let fun_ name pat = Function((name,pat),ref Set.empty)
let inlr name x y = fun_ name [x,y]
let inl x y = inlr "" x y
let ap x y = BinOp(Apply,x,y)
let methr name x y = inlr name x <| UnOp(CallAsMethod, y)
let meth x y = methr "" x y

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

let cons a b = BinOp(VVCons,a,b)

let l v b e = BinOp(Apply,inl v e,b)
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

let type_error x = UnOp(TypeError, Lit <| LitString x)

/// The tuple map function. Goes over the tuple scanning for a pattern and triggers only if it finds it.
let tuple_map =
    let recurse x = ap (V "rec") (vv [V "f"; V x])
    inlr "rec" (SS [S "f"; S "q"])
        (match_ (V "q")
            [
            F(S "x", "f"), V "x"
            SSS [SS' "v1"] "rest", cons (recurse "v1") (recurse "rest")
            SS [], vv []
            E, type_error "tuple .Map failed to match."
            ])

let tuple_library =
    function_
        [
        N("Map", S "x"), ap tuple_map (V "x")
//        N("ZipReg", S "x"), UnOp(VVZipReg, V "x")
//        N("ZipIrreg", S "x"), UnOp(VVZipIrreg, V "x")
//        N("UnzipReg", S "x"), UnOp(VVUnzipReg, V "x")
//        N("UnzipIrreg", S "x"), UnOp(VVUnzipIrreg, V "x")
        E, type_error "Call to non-existent case in the tuple function."
        ]

let data_empty () = 
    {memoized_methods=d0()
     method_tag=ref 0L
     let_tag=0L
     env=Map.empty
     sequences=ref None
     recursive_methods_stack=Stack()}

let inline vars_union' init f l = List.fold (fun s x -> Set.union s (f x)) init l
let inline vars_union f l = vars_union' Set.empty f l

let rec expr_free_variables e =
    let f e = expr_free_variables e
    match e with
    | V n -> Set.singleton n
    | VV(l,_) -> vars_union f l
    | If(a,b,c) -> f a + f b + f c
    | BinOp(_,a,b) -> f a + f b
    | UnOp(_,a) -> f a
    | Function((name,l),free_var_set) ->
        let rec pat_template on_name on_expr p = 
            let g p = pat_template on_name on_expr p
            match p with
            | F (pat,var) | A (pat,var) -> on_expr (g pat) var
            | N (_,pat) | A' (pat,_) -> g pat
            | S x | S' x -> on_name x
            | R (l,Some o) -> vars_union' (g o) g l
            | R (l,None) -> vars_union g l

        let pat_vars p = pat_template (fun _ -> Set.empty) (fun s var -> Set.add var s) p
        let pat_names p = pat_template Set.singleton (fun s _ -> s) p

        let fv = vars_union (fun (pat,body) -> pat_vars pat + (f body - pat_names pat |> Set.remove name)) l
        free_var_set := fv
        fv
        
    | Lit _ -> Set.empty

let rec expr_typecheck_with_empty_seq (d: LangEnv) expr =
    let d = {d with sequences = ref None}
    let expr = expr_typecheck d expr
    apply_sequences !d.sequences expr

and expr_typecheck (d: LangEnv) exp: TypedCudaExpr =
    let tev d exp = expr_typecheck d exp

    let prim_bin_op_template d check_error is_check k a b t =
        let constraint_both_eq_numeric f k =
            let a,b = tev d a, tev d b
            if is_check a b then k a b
            else f (check_error a b)

        constraint_both_eq_numeric failwith (k t)

    let prim_bin_op_helper t a b = TyBinOp(t,a,b,get_type a)
    let prim_un_op_helper t a = TyUnOp(t,a,get_type a)

    let prim_arith_op d = 
        let er = sprintf "`is_numeric a && get_type a = get_type b` is false.\na=%A, b=%A"
        let check a b = is_numeric a && get_type a = get_type b
        prim_bin_op_template d er check prim_bin_op_helper

    let prim_comp_op d = 
        let er = sprintf "`(is_numeric a || is_bool a) && get_type a = get_type b` is false.\na=%A, b=%A"
        let check a b = (is_numeric a || is_bool a) && get_type a = get_type b
        prim_bin_op_template d er check (fun t a b -> TyBinOp(t,a,b,PrimT BoolT))

    let prim_bool_op d = 
        let er = sprintf "`is_bool a && get_type a = get_type b` is false.\na=%A, b=%A"
        let check a b = is_bool a && get_type a = get_type b
        prim_bin_op_template d er check (fun t a b -> TyBinOp(t,a,b,PrimT BoolT))

    let prim_shift_op d =
        let er = sprintf "`is_int a && is_int b` is false.\na=%A, b=%A"
        let check a b = is_int a && is_int b
        prim_bin_op_template d er check prim_bin_op_helper

    let prim_shuffle_op d =
        let er = sprintf "`is_int b` is false.\na=%A, b=%A"
        let check a b = is_int b
        prim_bin_op_template d er check prim_bin_op_helper

    let prim_un_op_template d check_error is_check k a t =
        let constraint_numeric f k =
            let a = tev d a
            if is_check a then k a
            else f (check_error a)

        constraint_numeric failwith (k t)

    let prim_un_floating d = 
        let er = sprintf "`is_float a` is false.\na=%A"
        let check a = is_float a
        prim_un_op_template d er check prim_un_op_helper

    let prim_un_numeric d = 
        let er = sprintf "`is_numeric a` is false.\na=%A"
        let check a = is_numeric a
        prim_un_op_template d er check prim_un_op_helper

    let push_sequence d x = 
        let f current_sequence rest = apply_sequences current_sequence (x rest)
        d.sequences := Some (f !d.sequences)

    let get_tag d = 
        let t = d.let_tag
        d.let_tag <- d.let_tag + 1L
        t

    let make_tyv d ty_exp = get_tag d, get_type ty_exp

    let make_tyv_and_push' d ty_exp =
        let v = make_tyv d ty_exp
        push_sequence d (fun rest -> TyLet(v,ty_exp,rest,get_type rest))
        v

    let make_tyv_and_push d ty_exp = make_tyv_and_push' d ty_exp |> TyV

    let struct_create d ty_exp = TyUnOp(StructCreate,ty_exp,StructT ty_exp) |> make_tyv_and_push d

    // for a shallow version, take a look at `alternative_destructure_v6e.fsx`.
    // The deep version can also be straightforwardly derived from a template of this using the Y combinator.
    let destructure_deep d r = 
        let rec destructure_deep r = 
            let destructure_tuple r =
                match get_type r with
                | VVT (tuple_types, name) -> 
                    let indexed_tuple_args = List.mapi (fun i typ -> 
                        destructure_deep <| TyBinOp(VVIndex,r,TyLit <| LitInt32 i,typ)) tuple_types
                    TyVV(indexed_tuple_args, VVT (tuple_types, name))
                | _ -> r
            match r with
            | TyLit _ -> r // Literals are propagated.
            | TyV _ | TyBinOp (VVIndex,_,_,_) -> destructure_tuple r
            | TyVV(l,t) -> TyVV(List.map destructure_deep l,t)
            | _ -> make_tyv_and_push d r |> destructure_deep
        destructure_deep r

    let inline case_bind acc arg_name x = if is_full_name arg_name then Map.add arg_name x acc else acc

    let inline case_r recurse acc (l,ls,ls') (r,rs) (t,ts) ret =
        recurse acc l r <| fun x_acc ->
            recurse x_acc (R(ls,ls')) (TyVV(rs,VVT (ts, ""))) ret

    let case_f d apply match_single acc (pattern: CudaPattern) args meth on_fail ret =
        apply {d with env = acc} (V meth) args
            (fun _ -> on_fail()) // <| "Function application in pattern matcher failed to match a pattern."
            (fun r -> 
                match_single acc pattern (destructure_deep d r) 
                    (fun _ -> failwith "The function call subpattern in the matcher failed to match.")
                    ret)

    let case_a' annot args on_fail ret = if annot = get_type args then ret() else on_fail()
    let case_a d annot = case_a' (tev d (V annot) |> get_type) 

    let match_single' case_a case_f (acc: Env) l r on_fail ret =
        let rec recurse acc l r ret = //match_single case_f case_r case_bind acc l r on_fail ret
            match l,r with // destructure_deep is called in apply_method and apply_inlineable
            | A (pattern,annot), _ -> case_a annot r on_fail (fun _ -> recurse acc pattern r ret) 
            | A' (pattern,annot), _ -> case_a' annot r on_fail (fun _ -> recurse acc pattern r ret) 
            | F (pattern, meth), args -> case_f acc pattern args meth on_fail ret
            | R([],None), TyVV([], _) -> case_bind acc "" r |> ret
            | S' x, TyVV _ -> on_fail () //<| "S' matched a tuple."
            | S' x, _ | S x, _ -> case_bind acc x r |> ret
            | R([],Some ls), TyVV _ -> recurse acc ls r ret
            | R(l::ls,ls'), TyVV(r :: rs,VVT (t :: ts, "")) -> case_r recurse acc (l,ls,ls') (r,rs) (t,ts) ret
            //| R(l::ls,ls'), TyVV(r :: rs,VVT (t :: ts, _)) -> on_fail() // <| "Expecting an unnamed argument."
            | R([],None), TyVV(_, _) -> on_fail () // <| sprintf "More arguments than can be matched in R."
            | R _, _ -> on_fail () //<| sprintf "Cannot destructure %A." r
            | N (name, next), TyVV(x, VVT(t, name')) ->
                if name = name' then recurse acc next (TyVV(x,VVT (t, ""))) ret
                else on_fail() // <| sprintf "Cannot pattern match %s against %s" name name'
            | N _, _ -> on_fail() // "Cannot match name against a non-named argument."
        recurse acc l r ret

    let rec match_single case_a case_f = 
        let case_f x = case_f (match_single case_a case_f) x
        match_single' case_a case_f

    let match_all match_single (env: Env) l (args: TypedCudaExpr) on_fail ret =
        let rec loop = function
            | (pattern: CudaPattern, body: CudaExpr) :: xs ->
                match_single env pattern args
                    (fun _ -> loop xs)
                    (fun x -> ret (x, body))
            | [] -> on_fail (l,args) //"All patterns in the matcher failed to match."
        loop l

    let rec typed_expr_free_variables e =
        let f e = typed_expr_free_variables e
        match e with
        | TyV (n,t) -> (if n <> fun_tag then Set.singleton n else Set.empty) + ty_free_variables t
        | TyVV(l,_) -> vars_union f l
        | TyIf(a,b,c,_) -> f a + f b + f c
        | TyBinOp(_,a,b,_) -> f a + f b
        | TyMethodCall(used_vars,_,_) -> used_vars
        | TyUnOp(_,a,_) -> f a
        | TyLet((n,_),a,b,_) -> Set.remove n (f b) + f a
        | TyLit _ -> Set.empty

    and ty_free_variables x = 
        match x with
        | FunctionT(env,_) -> env_free_variables env
        | StructT e -> typed_expr_free_variables e
        | _ -> Set.empty

    and env_free_variables env = Map.fold (fun s _ v -> typed_expr_free_variables v + s) Set.empty env

    let renamer_make s =
        Set.toArray s
        |> Array.mapi (fun i x -> x, int64 i)
        |> Map
        
    let renamer_apply_pool r s = Set.map (fun x -> Map.find x r) s
    let renamer_reverse r = 
        Map.fold (fun s k v -> Map.add v k s) Map.empty r
        |> fun x -> if r.Count <> x.Count then failwith "The renamer is not bijective." else x

    let rec renamer_apply_env r e = Map.map (fun _ v -> renamer_apply_typedexpr r v) e
    and renamer_apply_typedexpr r e =
        let f e = renamer_apply_typedexpr r e
        let g e = renamer_apply_ty r e
        match e with
        | TyV (n,t) -> if n <> fun_tag then TyV (Map.find n r,g t) else TyV(n,g t)
        | TyVV(l,t) -> TyVV(List.map f l,t)
        | TyIf(a,b,c,t) -> TyIf(f a,f b,f c,t)
        | TyBinOp(o,a,b,t) -> TyBinOp(o,f a,f b,t)
        | TyMethodCall(used_vars,tag,t) -> TyMethodCall(renamer_apply_pool r used_vars,tag,t)
        | TyUnOp(o,a,t) -> TyUnOp(o,f a,t)
        | TyLet((n,t),a,b,t') -> TyLet((Map.find n r,t),f a,f b,t')
        | TyLit _ -> e
    and renamer_apply_ty r = function
        | FunctionT(e,t) -> FunctionT(renamer_apply_env r e,t)
        | StructT e -> StructT(renamer_apply_typedexpr r e)
        | e -> e

    let apply_inlineable tev d case_a case_f (initial_env,(name,l) as fun_key) args on_fail ret =
        match_all (match_single case_a case_f) initial_env l (destructure_deep d args)
            on_fail
            (fun (env, body) ->
                let fv = TyV (fun_tag, FunctionT fun_key)
                let d = {d with env = if name <> "" then Map.add name fv env else env}
                tev d body |> ret)

    let rec apply_template tev d (la: CudaExpr) ra on_fail ret =
        let la = tev d la
        match get_type la with
        | FunctionT x -> apply_inlineable tev d (case_a d) (case_f d (apply_template expr_typecheck_with_empty_seq)) x ra on_fail ret
        | _ -> failwith "Trying to apply a type other than InlineableT or MethodT."

    let apply d expr args = apply_template tev d expr (tev d args) (failwithf "Pattern matching cases failed to match.\n%A")

    let memoizing_eval d expr =
        let key_args = d.env, expr
        match d.memoized_methods.TryGetValue key_args with
        | false, _ ->                         
            let tag = !d.method_tag
            d.method_tag := tag + 1L

            d.memoized_methods.[key_args] <- (MethodInEvaluation(None), tag)
            let typed_expr = expr_typecheck_with_empty_seq d expr
            d.memoized_methods.[key_args] <- (MethodDone(typed_expr), tag)
            typed_expr, tag
        | true, (MethodInEvaluation None, tag) ->
            let typed_expr = get_body_from d.recursive_methods_stack
            d.memoized_methods.[key_args] <- (MethodInEvaluation (Some typed_expr), tag)
            typed_expr, tag
        | true, (MethodInEvaluation (Some typed_expr), tag) -> typed_expr, tag
        | true, (MethodDone typed_expr, tag) -> typed_expr, tag

    let call_as_method d expr = 
        let env = d.env

        let renamer = renamer_make (env_free_variables env)
        let renamed_env = renamer_apply_env renamer env

        let typed_expr, tag = memoizing_eval {d with env=renamed_env; let_tag=int64 renamer.Count} expr
        let typed_expr_ty = get_type typed_expr

        if is_returnable' typed_expr_ty = false then failwithf "%A is not a type that can be returned from a method. Consider using Inlineable instead." typed_expr

        let typed_expr_fv_pool = 
            typed_expr_free_variables typed_expr
            |> renamer_apply_pool (renamer_reverse renamer)

        TyMethodCall(typed_expr_fv_pool, tag, typed_expr_ty)

    let if_ cond tr fl =
        let cond = tev d cond
        if is_bool cond = false then failwithf "Expected a bool in conditional.\nGot: %A" (get_type cond)
        else
            let tev' e f =
                let mutable is_popped = false
                d.recursive_methods_stack.Push (fun _ -> is_popped <- true; f())
                let x = expr_typecheck_with_empty_seq d e
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
            if type_tr = type_fl then 
                //tev d (Apply(Method("",[E,T (TyIf(cond,tr,fl,type_tr))]),B))
                TyIf(cond,tr,fl,type_tr)
            else failwithf "Types in branches of If do not match.\nGot: %A and %A" type_tr type_fl

    let mset d a b =
        let l = tev d a
        let r = destructure_deep d (tev d b)
        match l, r with
        | TyBinOp(ArrayIndex,_,_,lt), r when lt = get_type r -> make_tyv_and_push d <| TyBinOp(MSet,l,r,BVVT)
        | _ -> failwithf "Error in mset. Expected: TyBinOp(ArrayIndex,_,_,lt), r when lt = get_type r.\nGot: %A and %A" l r

    let vv_index v i =
        match tev d v, tev d i with
        | v, TyLit (LitInt32 i as i') ->
            match get_type v with
            | VVT (ts,"") -> 
                if i >= 0 || i < List.length ts then TyBinOp(VVIndex,v,TyLit i',ts.[i])
                else failwith "(i >= 0 || i < List.length ts) = false in IndexVT"
            | VVT (ts, name) -> failwithf "Named tuples (%s) can't be indexed directly. They must be pattern matched on the name first." name
            | x -> failwithf "Type of a evaluated expression in IndexVT is not VTT.\nGot: %A" x
        | v, i ->
            failwithf "Index into a tuple must be a natural number less than the size of the tuple.\nGot: %A" i
    
    let vv_cons a b =
        let a = tev d a
        let b = tev d b |> destructure_deep d
        match b with
        | TyVV(b, VVT (bt, "")) -> TyVV(a::b, VVT (get_type a :: bt, ""))
        | TyVV(_, VVT (_, name)) -> failwithf "Named tuples (%s) can't be cons'd directly. They must be pattern matched on the name first." name 
        | _ -> failwith "Expected a tuple on the right is in VVCons."

    let guard_is_int args = if List.forall is_int args = false then failwithf "An size argument in CreateArray is not of type int.\nGot: %A" args
    let array_create d name size typeof_expr =
        let typ = tev d typeof_expr
        let size = 
            tev d size
            |> destructure_deep d
            |> fun x -> guard_is_int (tuple_field x); x

        let l = [size; typ]
        TyVV (l, VVT (List.map get_type l, name))
        |> struct_create d

    let array_index d ar args =
        let ar, args = tev d ar, tev d args
        let fargs = tuple_field args
        guard_is_int fargs
        match get_type ar with
        | StructT (TyVV([size;typ], VVT (_, ("Array" | "ArrayShared")))) ->
            let fsize = tuple_field size
            let lar, largs = fsize.Length, fargs.Length
            if lar = largs then size, args, get_type typ
            else failwithf "The index lengths in ArrayIndex do not match. %i <> %i" lar largs
        | StructT (TyVV(_, VVT (_, "Array"))) -> failwith "Incorrectly formed Array tuple."
        | _ -> failwithf "Array index needs the Array tuple.Got: %A" ar
            
    match exp with
    | Lit value -> TyLit value
    | V x -> 
        match Map.tryFind x d.env with
        | Some v -> v
        | None -> failwithf "Variable %A not bound." x
    | Function (core, free_var_set) -> 
        let env = Map.filter (fun k _ -> Set.contains k !free_var_set) d.env
        TyV(fun_tag,FunctionT(env,core))
    | If(cond,tr,fl) -> if_ cond tr fl
    | VV(vars,name) -> let vv = List.map (tev d) vars in TyVV(vv, VVT(List.map get_type vv, name))
    | BinOp (op,a,b) -> 
        match op with
        | Apply -> apply d a b id
        // Primitive operations on expressions.
        | Add -> prim_arith_op d a b Add
        | Sub -> prim_arith_op d a b Sub
        | Mult -> prim_arith_op d a b Mult
        | Div -> prim_arith_op d a b Div
        | Mod -> prim_arith_op d a b Mod
        
        | LT -> prim_comp_op d a b LT
        | LTE -> prim_comp_op d a b LTE
        | EQ -> prim_comp_op d a b EQ
        | NEQ -> prim_comp_op d a b NEQ
        | GT -> prim_comp_op d a b GT
        | GTE -> prim_comp_op d a b GTE
        | And -> prim_bool_op d a b And
        | Or -> prim_bool_op d a b Or

        | VVIndex -> vv_index a b
        | VVCons -> vv_cons a b

        | ArrayCreate -> array_create d "Array" a b
        | ArrayCreateShared -> array_create d "ArrayShared" a b
        | ArrayIndex -> let size,args,typ = array_index d a b in TyBinOp(ArrayIndex,size,args,typ)
        | MSet -> mset d a b
            
    | UnOp (op,a) -> 
        match op with
        | Neg -> prim_un_numeric d a Neg
        | CallAsMethod -> call_as_method d a
        | TypeError -> failwithf "%A" a
        | StructCreate -> failwith "StructCreate is not supposed to be typechecked. It is only for the benefit of the code generator."
