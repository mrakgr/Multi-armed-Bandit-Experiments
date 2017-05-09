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

and TyV = int64 * CudaTy
and Env = Map<string, TypedCudaExpr>
and FunctionCore = string * (CudaPattern * CudaExpr) list
and FunctionKey = Env * FunctionCore

and CudaPattern =
    | A of CudaPattern * CudaExpr // Type annotation case
    | A' of CudaPattern * CudaTy
    | S of string
    | S' of string // match if not tuple
    | R of CudaPattern list * CudaPattern option // Tuple
    | F of CudaPattern * CudaExpr // Functiona application with retracing.
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

and UnOp = 
    | Neg
    | CallAsMethod
    | TypeError
    | VVZipReg
    | VVZipIrreg
    | VVUnzipReg
    | VVUnzipIrreg

and CudaExpr = 
    | V of string
    | Lit of Value
    | Function of FunctionCore
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

and MemoCases =
    | MethodInEvaluation of CudaTy option
    | MethodDone of Set<TyV> * TypedCudaExpr
and MemoDict = Dictionary<FunctionKey, MemoCases>

and LangEnv =
    {
    // Immutable
    env : Env
    // Mutable
    memoized_methods : MemoDict // For typechecking recursive functions.
    sequences : (TypedCudaExpr -> TypedCudaExpr) option ref // For sequencing statements.
    recursive_methods_stack : Stack<unit -> TypedCudaExpr> // For typechecking recursive calls.
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
    | TyV (_,t) | TyIf(_,_,_,t) | TyLet(_,_,_,t)
    | TyVV(_,t) | TyBinOp(_,_,_,t) | TyUnOp(_,_,t) -> t

let rec is_returnable' = function
    | VVT (x,_) -> List.forall is_returnable' x
    | FunctionT _ -> false
    | _ -> true
let is_returnable a = is_returnable' (get_type a)

let rec is_numeric' = function
    | PrimT x -> 
        match x with
        | UInt8T | UInt16T | UInt32T | UInt64T 
        | Int8T | Int16T | Int32T | Int64T 
        | Float32T | Float64T -> true
        | StringT | BoolT -> false
    | _ -> false
let is_numeric a = is_numeric' (get_type a)

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

let is_full_name = function
    | null | "" | "_" -> false
    | _ -> true

let get_body_from (stack: Stack<unit -> TypedCudaExpr>) = 
    if stack.Count > 0 then stack.Pop()()
    else failwith "The program is divergent."

let is_arg = function TyV _ -> true | _ -> false

let rec typedexpr_iter f x = 
    let loop x = typedexpr_iter f x
    match x with
    | TyVV(l,_) -> f x; List.iter loop l
    | TyIf(a,b,c,_) -> f x; loop a; loop b; loop c
    | TyLet(_,a,b,_) | TyBinOp(_,a,b,_) -> f x; loop a; loop b
    | TyUnOp(_,a,_) -> f x; loop a
    | TyLit _ | TyV _ -> f x

let rec expr_iter f x = 
    let loop x = expr_iter f x
    match x with
    | VV(l,_) -> f x; List.iter loop l
    | If(a,b,c) -> f x; loop a; loop b; loop c
    | BinOp(_,a,b) -> f x; loop a; loop b
    | UnOp(_,a) -> f x; loop a
    | Lit _ | V _ | Function _ -> f x


let h0() = HashSet(HashIdentity.Structural)
let d0() = Dictionary(HashIdentity.Structural)

let apply_sequences sequences x =
    match sequences with
    | Some sequences -> sequences x
    | None -> x


let inlr name x y = Function(name,[x,y])
let inl x y = inlr "" x y
let ap x y = BinOp(Apply,x,y)
let methr name x y = UnOp(CallAsMethod,inlr name x y)
let meth x y = methr "" x y

let E = S ""
let B = VV ([], "")
let TyB = TyVV([], VVT ([], ""))
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

let match_ x pat = ap (Function("",pat)) x
let function_ pat = Function("",pat)

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
            F(S "x",V "f"), V "x"
            SSS [SS' "v1"] "rest", cons (recurse "v1") (recurse "rest")
            SS [], vv []
            E, type_error "tuple .Map failed to match."
            ])

let tuple_library =
    function_
        [
        N("Map", S "x"), ap tuple_map (V "x")
        N("ZipReg", S "x"), UnOp(VVZipReg, V "x")
        N("ZipIrreg", S "x"), UnOp(VVZipIrreg, V "x")
        N("UnzipReg", S "x"), UnOp(VVUnzipReg, V "x")
        N("UnzipIrreg", S "x"), UnOp(VVUnzipIrreg, V "x")
        E, type_error "Call to non-existent case in the tuple function."
        ]

let data_empty () = 
    {memoized_methods=d0()
     let_tag=0L
     env=Map.empty
     sequences=ref None
     recursive_methods_stack=Stack()}

let typechecker_env_copy (d: LangEnv) =
    {
    memoized_methods = d.memoized_methods |> Dictionary
    env = d.env
    sequences = ref !d.sequences
    recursive_methods_stack = d.recursive_methods_stack |> Stack
    let_tag=d.let_tag
    }

let typechecker_env_set (t: LangEnv) (d: LangEnv) =
    t.memoized_methods.Clear(); d.memoized_methods |> Seq.iter (fun x -> t.memoized_methods.Add(x.Key,x.Value))
    t.sequences := !d.sequences
    t.recursive_methods_stack.Clear(); d.recursive_methods_stack |> Seq.iter t.recursive_methods_stack.Push
    t.let_tag <- d.let_tag

let rec with_empty_seq (d: LangEnv) expr =
    let d = {d with sequences = ref None}
    let expr = exp_and_seq d expr
    apply_sequences !d.sequences expr

and exp_and_seq (d: LangEnv) exp: TypedCudaExpr =
    let tev d exp = exp_and_seq d exp

    let append_typeof_fst t a b =
        t (a, b, (get_type a))

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

    let push_sequence d x = 
        let f current_sequence rest = apply_sequences current_sequence (x rest)
        d.sequences := Some (f !d.sequences)

    let make_tyv d ty_exp = 
        d.let_tag <- d.let_tag + 1L
        d.let_tag, get_type ty_exp

    let make_tyv_and_push' d ty_exp =
        let v = make_tyv d ty_exp
        push_sequence d (fun rest -> TyLet(v,ty_exp,rest,get_type rest))
        v

    let make_tyv_and_push d ty_exp = make_tyv_and_push' d ty_exp |> TyV

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
            | TyVV(l,t) -> TyVV(List.map destructure_deep l,t)
            | TyLit _ -> r // Literals are propagated.
            | TyV _ | TyBinOp (VVIndex,_,_,_) -> destructure_tuple r
            | _ -> make_tyv_and_push d r |> destructure_deep
        destructure_deep r

    let case_bind acc arg_name x = if is_full_name arg_name then Map.add arg_name x acc else acc

    let case_r recurse acc (l,ls,ls') (r,rs) (t,ts) ret =
        recurse acc l r <| fun x_acc ->
            recurse x_acc (R(ls,ls')) (TyVV(rs,VVT (ts, ""))) ret

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

    let match_single' case_a case_f case_r case_bind (acc: Env) l r on_fail ret =
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
        match_single' case_a case_f case_r case_bind

    let match_all match_single (env: Env) l (args: TypedCudaExpr) on_fail ret =
        let rec loop = function
            | (pattern: CudaPattern, body: CudaExpr) :: xs ->
                match_single env pattern args
                    (fun _ -> loop xs)
                    (fun x -> ret (x, body))
            | [] -> on_fail (l,args) //"All patterns in the matcher failed to match."
        loop l

    let fun_tag = -1L

    let apply_inlineable d case_a case_f (initial_env,(name,l) as fun_key) args on_fail ret =
        match_all (match_single case_a case_f) initial_env l (destructure_deep d args)
            on_fail
            (fun (env, body) ->
                let fv = TyV (fun_tag, FunctionT fun_key)
                let d = {d with env = if name <> "" then Map.add name fv env else env}
                tev d body |> ret)

    let expr_used_variables (d: LangEnv) x =
        let rec loop bound_set used_set e = 
            let f used_set x = loop bound_set used_set x
            match e with
            | V n -> if Set.contains n bound_set then Set.add n used_set else used_set
            | VV(l,_) -> List.fold (loop bound_set) used_set l
            | If(a,b,c) -> f (f (f used_set a) b) c
            | BinOp(_,a,b) -> f (f used_set a) b
            | UnOp(_,a) -> f used_set a
            | Function(name,l) ->
                let bound_set_function = bound_set
                let rec eval_pattern (bound_set,used_set as both_sets) p = 
                    match p with
                    | F (pat,expr) | A (pat,expr) -> eval_pattern (bound_set, loop bound_set_function used_set expr) pat
                    | N (_,pat) | A' (pat,_) -> eval_pattern both_sets pat
                    | S x | S' x -> Set.remove x bound_set, used_set
                    | R (l,Some o) -> List.fold eval_pattern (eval_pattern both_sets o) l
                    | R (l,None) -> List.fold eval_pattern both_sets l

                let bound_set = Set.remove name bound_set
                l |> List.fold (fun used_set (pat,body) ->
                    let bound_set,used_set = eval_pattern (bound_set,used_set) pat
                    loop bound_set used_set body
                    ) used_set
            | Lit _ -> used_set
        
        let bound_set = d.env |> Map.fold (fun s k v -> Set.add k s) Set.empty
        loop bound_set Set.empty x

    let call_as_method d x = failwith "x"


    failwith "x"