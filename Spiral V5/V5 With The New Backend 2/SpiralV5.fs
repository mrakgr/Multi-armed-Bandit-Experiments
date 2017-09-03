module Spiral.Main

// Global open
open System
open System.Collections.Generic
open Iesi.Collections.Generic

let mutable total_time = TimeSpan()

// Parser open
open FParsec

// Codegen open
open System.Text

// Language types
type Node<'a>(expr:'a, symbol:int) = 
    member x.Expression = expr
    member x.Symbol = symbol
    override x.ToString() = sprintf "%A" expr
    override x.GetHashCode() = symbol
    override x.Equals(y) = 
        match y with 
        | :? Node<'a> as y -> symbol = y.Symbol
        | _ -> failwith "Invalid equality for Node."

    interface IComparable with
        member x.CompareTo(y) = 
            match y with
            | :? Node<'a> as y -> compare symbol y.Symbol
            | _ -> failwith "Invalid comparison for Node."

let inline n (x: Node<_>) = x.Expression 
let (|N|) x = n x
let (|S|) (x: Node<_>) = x.Symbol

type ModuleName = string
type ModuleCode = string
type ModuleDescription = string
type Module = Module of Node<ModuleName * Module list * ModuleDescription * ModuleCode>

type PosKey = Module * int64 * int64

let h0() = HashSet(HashIdentity.Structural)
let lh0() = LinkedHashSet()
let d0() = Dictionary(HashIdentity.Structural)

let inline memoize (memo_dict: Dictionary<_,_>) k f =
    match memo_dict.TryGetValue k with
    | true, v -> v
    | false, _ -> let v = f() in memo_dict.[k] <- v; v

let nodify (dict: Dictionary<_,_>) x = memoize dict x (fun () -> Node(x,dict.Count))
let nodify_module = nodify <| d0()
let module_ x = nodify_module x |> Module

type Pos<'a when 'a: equality and 'a: comparison>(pos:PosKey, expr:'a) = 
    member x.Expression = expr
    member x.Pos = pos
    override x.ToString() = sprintf "%A" expr
    override x.GetHashCode() = expr.GetHashCode()
    override x.Equals(y) = 
        match y with 
        | :? Pos<'a> as y -> expr = y.Expression
        | x -> failwithf "Invalid equality for Pos. Got: %A" x

    interface IComparable with
        member x.CompareTo(y) = 
            match y with
            | :? Pos<'a> as y -> compare expr y.Expression
            | x -> failwithf "Invalid comparison for Pos. Got: %A" x

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

type Value = 
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

type Op =
    // StringOps
    | StringLength
    | StringIndex
    | StringSlice

    // DotNetOps
    | DotNetLoadAssembly
    | DotNetTypeConstruct
    | DotNetTypeCallMethod
    | DotNetTypeGetField

    // Module
    | ModuleCreate
    | ModuleWith
    | ModuleWithExtend
    | ModuleCreateAlt
    | ModuleWithAlt
    | ModuleIs

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
    | JoinPoint
    | StructCreate
    | VVIndex
    | VVSliceFrom
    | VVCons
    | VVLength
    | VVIs
    | TypeAnnot
    | EnvUnseal
    | TypeConstructorCreate
    | TypeConstructorUnion
    | EqType
    | ModuleHasMember

    | ArrayCreate
    | ReferenceCreate
    | ArrayIndex
    | ArraySet
    | ArrayLength
   
    | ShiftLeft
    | ShiftRight
    | ShuffleXor
    | ShuffleUp
    | ShuffleDown
    | ShuffleIndex

    // Static unary operations
    | PrintStatic
    | PrintEnv
    | PrintExpr
    | ErrorNonUnit
    | ErrorType
    | ModuleOpen
    | TypeLitCreate
    | Dynamize
    | IsStatic

    // UnOps
    | Neg
    | Log
    | Exp
    | Tanh
    | FailWith
    
    // Constants
    | Syncthreads
    | BlockDimX | BlockDimY | BlockDimZ
    | GridDimX | GridDimY | GridDimZ

type FunctionCore = string * Expr

and FunType =
    | FunTypeFunction of FunctionCore // Type level function. Can also be though of as a procedural macro.
    | FunTypeRecFunction of FunctionCore * string
    | FunTypeModule

    override t.ToString() =
        match t with
        | FunTypeModule -> "<module>"
        | FunTypeFunction (str, expr) -> "<function>"
        | FunTypeRecFunction (_, name) -> sprintf "<rec_function: %s>" name

and Pattern =
    | E
    | PatVar of string
    | PatTuple of Pattern list
    | PatCons of Pattern list
    | PatType of Pattern * Expr
    | PatActive of string * Pattern
    | PatPartActive of string * Pattern
    | PatExtActive of string * Pattern
    | PatOr of Pattern list
    | PatAnd of Pattern list
    | PatClauses of (Pattern * Expr) list
    | PatTypeLit of Value
    | PatLit of Value
    | PatWhen of Pattern * Expr
    | PatModuleBinding of string * Pattern option
    | PatModuleInner of string * Pattern list
    | PatModuleOuter of string option * Pattern list
    | PatPos of Pos<Pattern>

and Expr = 
    | V of Node<string>
    | Lit of Node<Value>
    | Pattern of Node<Pattern>
    | Function of Node<FunctionCore>
    | FunctionFilt of Node<Set<string> * Node<FunctionCore>>
    | VV of Node<Expr list>
    | Op of Node<Op * Expr list>
    | ExprPos of Pos<Expr>

and Ty =
    | PrimT of PrimitiveType
    | VVT of Node<Ty list>
    | LitT of Value
    | FunT of Node<EnvTy * FunType>
    | UnionT of Node<Set<Ty>>
    | RecT of int
    | TypeConstructorT of Node<Ty>
    | ClosureT of Node<Ty * Ty>
    | ArrayT of Node<ArrayType * Ty>
    | ForCastT of Node<Ty> // For casting type level function to term (ClosureT) level ones.
    | DotNetTypeRuntimeT of Node<Type> 
    | DotNetTypeInstanceT of Node<Type>
    | DotNetAssemblyT of Node<System.Reflection.Assembly>

and TypedExpr =
    | TyV of TyTag
    | TyVV of Node<TypedExpr list>
    | TyFun of Node<EnvTerm * FunType>
    | TyBox of Node<TypedExpr * Ty>

    | TyLet of LetType * TyTag * TypedExpr * TypedExpr * Ty
    | TyLit of Value
    | TyOp of Op * TypedExpr list * Ty
    | TyJoinPoint of JoinPointKey * Ty

and JoinPointType =
    | JoinPointClosure of Arguments
    | JoinPointMethod
and JoinPointKey = MemoKey * Tag
and JoinPointValue = JoinPointType * Arguments * Renamer

and MemoCases =
    | MemoMethodInEvaluation
    | MemoMethod of JoinPointType * Arguments * TypedExpr
    | MemoTypeInEvaluation of Ty
    | MemoType of Ty

and Tag = int
and TyTag = Tag * Ty
and EnvTerm = Node<Map<string, TypedExpr>>
and EnvTy = Node<Map<string, Ty>>
and MemoKey = Node<Expr * EnvTerm>

and Arguments = LinkedHashSet<TyTag>
and Renamer = Dictionary<Tag,Tag>

and LetType =
    | LetStd
    | LetInvisible

// This key is for functions without arguments. It is intended that the arguments be passed in through the Environment.
and MemoDict = Dictionary<MemoKey, MemoCases>
// For Common Subexpression Elimination. I need it not for its own sake, but to enable other PE based optimizations.
and CSEDict = Map<TypedExpr,TypedExpr> ref

type Trace = PosKey list

type RecursiveBehavior =
    | AnnotationDive
    | AnnotationReturn

type LangEnv =
    {
    rbeh: RecursiveBehavior
    ltag : int ref
    seq : (TypedExpr -> TypedExpr) ref
    env : EnvTerm
    cse_env : CSEDict
    trace : Trace
    }

exception TypeError of Trace * string

type Result<'a,'b> = Succ of 'a | Fail of 'b

// Parser types
type Userstate = 
    {
    ops : Dictionary<string, int * Associativity>
    ignore_semicolon : bool
    }

type ParserExpr =
| ParserStatement of PosKey * (Expr -> Expr)
| ParserExpr of PosKey * Expr

// Codegen types
type Buf = ResizeArray<ProgramNode>
and ProgramNode =
    | Statement of string
    | Indent
    | Dedent
    | Statements of Buf

// #Main
let spiral_peval module_main output_path = 
    let inline force (x: Lazy<_>) = x.Value
    let memoized_methods: MemoDict = d0()
    let join_point_dict: Dictionary<JoinPointKey,JoinPointValue> = d0()

    let ty_join_point memo_key value t =
        let new_subtag = join_point_dict.Count
        let key = memo_key,new_subtag
        join_point_dict.Add(key,value)
        TyJoinPoint(key,t)

    let nodify_expr (dict: Dictionary<_,_>) x =
        match dict.TryGetValue x with
        | true, id -> Node(x,id)
        | false, _ ->
            let id = dict.Count
            let x' = Node(x,id)
            dict.[x] <- id
            x'
 
    // #Smart constructors
    
    // nodify_expr variants.
    let nodify_v = nodify_expr <| d0()
    let nodify_lit = nodify_expr <| d0()
    let nodify_pattern = nodify_expr <| d0()
    let nodify_func = nodify_expr <| d0()
    let nodify_func_filt = nodify_expr <| d0()
    let nodify_vv = nodify_expr <| d0()
    let nodify_op = nodify_expr <| d0()

    let v x = nodify_v x |> V
    let lit x = nodify_lit x |> Lit
    let op x = nodify_op x |> Op
    let pattern x = nodify_pattern x |> Pattern
    let func x = nodify_func x |> Function
    let func_filt x = nodify_func_filt x |> FunctionFilt
    let vv x = nodify_vv x |> VV

    // nodify_ty variants
    let env_ty_dict = d0()
    let nodify_env_ty = nodify env_ty_dict
    let uniont_dict = d0()
    let nodify_uniont = nodify uniont_dict
    let rect_dict = d0()
    let (|TyRec|_|) = function
        | RecT x -> Some rect_dict.[x]
        | _ -> None
    let nodify_typect = nodify <| d0()
    let nodify_closuret = nodify <| d0()
    let nodify_arrayt = nodify <| d0()
    let nodify_for_castt = nodify <| d0()
    let nodify_dotnet_type_runtimet = nodify <| d0()
    let nodify_dotnet_type_instancet = nodify <| d0()
    let nodify_dotnet_assemblyt = nodify <| d0()
    let nodify_vvt = nodify <| d0()
    let nodify_funt = nodify <| d0()

    let vvt x = nodify_vvt x |> VVT
    let litt x = LitT x
    let funt (x, core) = nodify_funt (x, core) |> FunT
    let uniont x = nodify_uniont x |> UnionT
    let typect x = nodify_typect x |> TypeConstructorT
    let closuret x = nodify_closuret x |> ClosureT
    let arrayt x = nodify_arrayt x |> ArrayT
    let for_castt x = nodify_for_castt x |> ForCastT
    let dotnet_type_runtimet x = nodify_dotnet_type_runtimet x |> DotNetTypeRuntimeT
    let dotnet_type_instancet x = nodify_dotnet_type_instancet x |> DotNetTypeInstanceT
    let dotnet_assemblyt x = nodify_dotnet_assemblyt x |> DotNetAssemblyT

    let nodify_memo_key = nodify <| d0()
    let nodify_env_term = nodify <| d0()

//    let nodify_tyv = nodify <| d0()
    let nodify_tyvv = nodify <| d0()
    let nodify_tyfun = nodify <| d0()
    let nodify_tybox = nodify <| d0()
    
    let tyv x = x |> TyV
    let tyvv x = nodify_tyvv x |> TyVV
    let tyfun (a,t) = nodify_tyfun (a,t) |> TyFun
    let tybox x = nodify_tybox x |> TyBox

    let lit_int i = LitInt64 i |> lit
    let lit_string x = LitString x |> lit

    let fix name x =
        match name with
        | "" -> x
        | _ -> (Fix,[lit_string name; x]) |> op
    let inl x y = (x,y) |> func
    let inl_pat x y = (PatClauses([x,y])) |> pattern
    let ap x y = (Apply,[x;y]) |> op
    let for_cast x = (ForCast,[x]) |> op
    let lp v b e = ap (inl_pat v e) b
    let inmp v' b e = ap (ap (v ">>=") b) (inl_pat v' e)
    let l v b e = ap (inl v e) b
    let l_rec v b e = ap (inl v e) (fix v b)

    let inl' args body = List.foldBack inl args body
    
    let meth_memo y = (JoinPoint,[y]) |> op
    let meth x y = inl x (meth_memo y)

    let module_create l = (ModuleCreate,[l]) |> op
    let module_open a b = (ModuleOpen,[a;b]) |> op

    let B = vv []
    let BVVT = vvt []
    let TyB = tyvv []

    let cons a b = (VVCons,[a;b]) |> op

    let s l fin = List.foldBack (fun x rest -> x rest) l fin

    let rec ap' f l = List.fold ap f l

    let tuple_index' v i = (VVIndex,[v; i]) |> op
    let tuple_index v i = tuple_index' v (lit_int i)
    let tuple_length v = (VVLength,[v]) |> op
    let tuple_slice_from v i = (VVSliceFrom,[v; lit_int i]) |> op
    let tuple_is v = (VVIs,[v]) |> op

    let error_type x = (ErrorType,[x]) |> op
    let print_static x = (PrintStatic,[x]) |> op
    let print_env x = (PrintEnv,[x]) |> op
    let print_expr x = (PrintExpr,[x]) |> op
    let dynamize x = (Dynamize,[x]) |> op

    let module_is x = op (ModuleIs,[x])
    let module_has_member a b = op (ModuleHasMember,[a;b])

    let if_static cond tr fl = (IfStatic,[cond;tr;fl]) |> op
    let case arg case = (Case,[arg;case]) |> op
    let binop op' a b = (op',[a;b]) |> op
    let eq_type a b = binop EqType a b
    let eq a b = binop EQ a b
    let lt a b = binop LT a b
    let gte a b = binop GTE a b

    let error_non_unit x = (ErrorNonUnit, [x]) |> op
    let type_lit_create x = (TypeLitCreate,[lit x]) |> op
    let expr_pos pos x = ExprPos(Pos(pos,x))
    let pat_pos pos x = PatPos(Pos(pos,x))

    let type_create a = op(TypeConstructorCreate,[a])
    let type_union a b = op(TypeConstructorUnion,[a;b])

    // Aux outer functions
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

    let rec env_to_ty env = Map.map (fun _ -> get_type) env |> nodify_env_ty
    and get_type = function
        | TyLit x -> get_type_of_value x
        | TyVV (N l) -> List.map get_type l |> vvt
        | TyFun (N(N l, t)) -> funt (env_to_ty l, t)

        | TyV(_,t)
        | TyBox(N(_,t))
        | TyLet(_,_,_,_,t) | TyJoinPoint(_,t)
        | TyOp(_,_,t) -> t

    let (|TyTypeC|_|) x =
        match x with
        | TypeConstructorT x -> Some x
        | _ -> None

    /// Wraps the argument in a list if not a tuple.
    let tuple_field = function 
        | TyVV (N args) -> args
        | x -> [x]

    let (|TyTuple|) x = tuple_field x

    /// Wraps the argument in a set if not a UnionT.
    let set_field = function
        | UnionT (N t) -> t
        | t -> Set.singleton t

    let (|TySet|) x = set_field x

    /// Wraps the argument in a list if not a tuple type.
    let tuple_field_ty = function 
        | VVT (N x) -> x
        | x -> [x]

    let (|TyType|) x = get_type x
    let (|TypeLit|_|) = function
        | TyType (LitT x) -> Some x
        | _ -> None
    let (|TypeString|_|) = function
        | TypeLit (LitString x) -> Some x
        | _ -> None

    let inline is_returnable' _ = true
    let inline is_returnable a = is_returnable' (get_type a)

    let is_numeric' = function
        | PrimT (UInt8T | UInt16T | UInt32T | UInt64T 
            | Int8T | Int16T | Int32T | Int64T 
            | Float32T | Float64T) -> true
        | _ -> false
    let inline is_numeric a = is_numeric' (get_type a)

    let is_string' = function
        | PrimT StringT -> true
        | _ -> false
    let inline is_string a = is_string' (get_type a)

    let is_char' = function
        | PrimT CharT -> true
        | _ -> false
    let inline is_char a = is_char' (get_type a)

    let is_primt' = function
        | PrimT x -> true
        | _ -> false
    let inline is_primt a = is_primt' (get_type a)

    let is_float' = function
        | PrimT (Float32T | Float64T) -> true
        | _ -> false
    let inline is_float a = is_float' (get_type a)

    let rec is_bool' = function
        | PrimT BoolT -> true
        | _ -> false
    let inline is_bool a = is_bool' (get_type a)

    let rec is_int' = function
        | PrimT (UInt32T | UInt64T | Int32T | Int64T) -> true
        | _ -> false
    let inline is_int a = is_int' (get_type a)

    let rec is_int64' = function
        | PrimT Int64T -> true
        | _ -> false
    let inline is_int64 a = is_int64' (get_type a)

    // #Prepass
    let get_pattern_tag =
        let mutable i = 0
        fun () -> i <- i+1; i

    let rec pattern_compile arg pat =
        let rec pattern_compile arg pat (on_succ: Lazy<_>) (on_fail: Lazy<_>) =
            let inline cp' arg pat on_succ on_fail = pattern_compile arg pat on_succ on_fail
            let inline cp arg pat on_succ on_fail = lazy cp' arg pat on_succ on_fail

            let inline pat_foldbacki f s l =
                let mutable len = 0L
                let rec loop i l =
                    match l with
                    | x :: xs -> f (x,i) (loop (i+1L) xs)
                    | [] -> len <- i; s
                loop 0L l, len
            
            let pat_tuple l' =
                pat_foldbacki
                    (fun (pat,i) on_succ ->
                        let arg = tuple_index arg i
                        cp arg pat on_succ on_fail)
                    on_succ
                    l'
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
                    if_static (gte (tuple_length arg) (lit_int (len-1L))) on_succ.Value on_fail.Value
                    |> fun on_succ -> if_static (tuple_is arg) on_succ on_fail.Value
                    |> case arg

            let pat_part_active a pat on_fail arg =
                let pat_var = sprintf " pat_var_%i" (get_pattern_tag())
                let on_succ = inl pat_var (cp (v pat_var) pat on_succ on_fail |> force)
                let on_fail = inl "" on_fail.Value
                ap' (v a) [arg; on_fail; on_succ]

            let pat_module_bindings pat_var bindings on_succ = 
                List.foldBack (fun x on_succ -> cp pat_var x on_succ on_fail) bindings on_succ |> force 
            let pat_module_is_module on_succ = if_static (module_is arg) on_succ (force on_fail)

            match pat with
            | E -> on_succ.Value
            | PatVar x -> l x arg on_succ.Value
            | PatType (exp,typ) ->
                let on_succ = cp' arg exp on_succ on_fail
                if_static (eq_type arg typ) on_succ on_fail.Value
                |> case arg
            | PatTuple l -> pat_tuple l
            | PatCons l -> pat_cons l
            | PatActive (a,b) ->
                let pat_var = sprintf " pat_var_%i" (get_pattern_tag())
                l pat_var (ap (v a) arg) (cp' (v pat_var) b on_succ on_fail)
            | PatPartActive (a,pat) -> pat_part_active a pat on_fail arg
            | PatExtActive (a,pat) ->
                let rec f pat' on_fail = function
                    | PatAnd _ as pat -> op(ErrorType,[lit_string "And patterns are not allowed in extension patterns."]) |> pat_part_active a (pat' pat) on_fail
                    | PatOr l -> List.foldBack (fun pat on_fail -> lazy f pat' on_fail pat) l on_fail |> force
                    | PatCons l as pat -> vv [type_lit_create (LitString "cons"); l.Length |> int64 |> LitInt64 |> lit; arg] |> pat_part_active a (pat' pat) on_fail
                    | PatTuple l as pat -> vv [type_lit_create (LitString "tup"); l.Length |> int64 |> LitInt64 |> lit; arg] |> pat_part_active a (pat' pat) on_fail
                    | PatType (a,typ) -> f (fun a -> PatType(a,typ) |> pat') on_fail a
                    | PatWhen (pat, e) -> f (fun pat -> PatWhen(pat,e) |> pat') on_fail pat
                    | PatClauses _ -> failwith "Clauses should not appear inside other clauses."
                    | pat -> vv [type_lit_create (LitString "var"); arg] |> pat_part_active a (pat' pat) on_fail
                f id on_fail pat
            | PatOr l -> List.foldBack (fun pat on_fail -> cp arg pat on_succ on_fail) l on_fail |> force
            | PatAnd l -> List.foldBack (fun pat on_succ -> cp arg pat on_succ on_fail) l on_succ |> force
            | PatClauses l -> List.foldBack (fun (pat, exp) on_fail -> cp arg pat (lazy (expr_prepass exp |> snd)) on_fail) l on_fail |> force
            | PatTypeLit x -> 
                if_static (eq_type arg (type_lit_create x)) on_succ.Value on_fail.Value 
                |> case arg
            | PatLit x -> 
                let x = lit x
                let on_succ = if_static (eq arg x) on_succ.Value on_fail.Value
                if_static (eq_type arg x) on_succ on_fail.Value |> case arg
            | PatWhen (p, e) -> cp' arg p (lazy if_static e on_succ.Value on_fail.Value) on_fail
            | PatModuleInner(name,bindings) ->
                let pat_var = sprintf " pat_var_%i" (get_pattern_tag())
                let pat_var' = v pat_var
                let memb = type_lit_create (LitString name)
                
                pat_module_bindings pat_var' bindings on_succ
                |> l name pat_var'
                |> l pat_var (ap arg memb)
                |> fun on_succ -> if_static (module_has_member arg memb) on_succ (force on_fail)
                |> pat_module_is_module
            | PatModuleOuter(name,bindings) ->
                let pat_var = sprintf " pat_var_%i" (get_pattern_tag())
                let pat_var' = v pat_var
                pat_module_bindings pat_var' bindings on_succ
                |> fun x -> 
                    match name with
                    | Some name -> l name pat_var' x
                    | None -> x
                |> l pat_var arg
                |> pat_module_is_module
            | PatModuleBinding (name,pat) ->
                let pat_var = sprintf " pat_var_%i" (get_pattern_tag())
                let memb = type_lit_create (LitString name)
                match pat with
                | Some pat -> l pat_var (ap arg memb) (cp' (v pat_var) pat on_succ on_fail)
                | None -> l name (ap arg memb) on_succ.Value
                |> fun on_succ -> if_static (module_has_member arg memb) on_succ (force on_fail)
            | PatPos p -> expr_pos p.Pos (cp' arg p.Expression on_succ on_fail)

        let pattern_compile_def_on_succ = lazy failwith "Missing a clause."
        let pattern_compile_def_on_fail = lazy error_type (lit (LitString <| "Pattern matching cases are inexhaustive."))
        pattern_compile arg pat pattern_compile_def_on_succ pattern_compile_def_on_fail

    and pattern_compile_single pat =
        let main_arg = "main_arg"
        inl main_arg (pattern_compile (v main_arg) pat) |> expr_prepass

    and expr_prepass e =
        let inline f e = expr_prepass e
        match e with
        | V (N n) -> Set.singleton n, e
        | Op(N(op',l)) ->
            let l,l' = List.map f l |> List.unzip
            Set.unionMany l, op(op',l')
        | VV (N l) -> 
            let l,l' = List.map f l |> List.unzip
            Set.unionMany l, vv l'
        | FunctionFilt(N (vars,N(name,body))) ->
            Set.remove name vars, e
        | Function(N(name,body)) ->
            let vars,body = f body
            Set.remove name vars, func_filt(vars,nodify_func(name,body))
        | Lit _ -> Set.empty, e
        | Pattern (N pat) -> pattern_compile_single pat
        | ExprPos p -> 
            let vars, body = f p.Expression
            vars, expr_pos p.Pos body

    // #Renaming
    let renamer_apply_pool (r: Dictionary<_,_>) (s: LinkedHashSet<_>) = 
        let s' = lh0()
        s |> Seq.iter (fun (tag,ty) -> s'.Add(r.[tag],ty) |> ignore)
        s' 

    let renamer_apply_renamer (r: Dictionary<_,_>) (m: Renamer): Renamer = 
        let m' = d0()
        m |> Seq.iter (fun kv -> m'.[kv.Key] <- r.[kv.Value])
        m'

    let rec renamer_apply_env r e = Map.map (fun k v -> renamer_apply_typedexpr r v) e |> nodify_env_term
    and renamer_apply_typedexpr
        (memo_dict: Dictionary<_,_>, renamer: Dictionary<_,_>, renamer_reversed: Dictionary<_,_>, 
         fv: LinkedHashSet<_>, renamed_fv: LinkedHashSet<_> as r) e =
        let inline f e = renamer_apply_typedexpr r e
        let inline rename (n,t as k) =
            match renamer.TryGetValue n with
            | true, _ -> failwith "Should be caught be the memoize call."
            | false, _ ->
                let n' = renamer.Count
                renamer.Add(n,n')
                renamer_reversed.Add(n',n)
                fv.Add k |> ignore
                let k' = n', t
                renamed_fv.Add (n',t) |> ignore
                k'
        memoize memo_dict e <| fun () ->
            match e with
            | TyBox (N(n,t)) -> tybox(f n,t)
            | TyVV (N l) -> tyvv(List.map f l)
            | TyFun(N(N l,t)) -> tyfun(renamer_apply_env r l, t)
            | TyV (n,t as k) -> 
                let n', _ as k' = rename k
                if n' = n then e else tyv k'
            | TyLit _ -> e
            | TyJoinPoint _ | TyOp _ | TyLet _ -> failwithf "Only data structures can be renamed. Got: %A" e

    // #Conversion
    let rec dotnet_type_to_ty (x: System.Type) =
        if x = typeof<bool> then PrimT BoolT
        elif x = typeof<int8> then PrimT Int8T
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
        elif x = typeof<char> then PrimT CharT
        elif x = typeof<unit> || x = typeof<System.Void> then BVVT
        elif x.IsArray then arrayt(DotNetHeap,dotnet_type_to_ty (x.GetElementType()))
        // Note: The F# compiler doing implicit conversions on refs really screws with me here. I won't bother trying to make this sound.
        elif x.IsByRef then arrayt(DotNetReference, dotnet_type_to_ty (x.GetElementType())) // Incorrect, but useful
        elif x.ContainsGenericParameters then dotnet_type_runtimet x
        else dotnet_type_instancet x

    let rec dotnet_ty_to_type (x: Ty) =
        match x with
        | PrimT BoolT -> typeof<bool>
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
        | PrimT CharT -> typeof<char>
        | ArrayT(N(DotNetHeap,t)) -> (dotnet_ty_to_type t).MakeArrayType()
        | ArrayT(N(DotNetReference,t)) -> (dotnet_ty_to_type t).MakeByRefType() // Incorrect, but useful
        | DotNetTypeInstanceT (N x) | DotNetTypeRuntimeT (N x) -> x
        | _ -> failwithf "Type %A not supported for conversion into .NET SystemType." x

    let is_all_int64 size = List.forall is_int64 (tuple_field size)

    // #Type directed partial evaluation
    let rec expr_peval (d: LangEnv) (expr: Expr) =
        let inline tev d expr = expr_peval d expr
        let inline apply_seq d x = !d.seq x
        let inline tev_seq d expr = let d = {d with seq=ref id; cse_env=ref !d.cse_env} in tev d expr |> apply_seq d
        let inline tev_assume cse_env d expr = let d = {d with seq=ref id; cse_env=ref cse_env} in tev d expr |> apply_seq d
        let inline tev_method d expr = let d = {d with seq=ref id; cse_env=ref Map.empty} in tev d expr |> apply_seq d
        let inline tev_rec d expr = let d = {d with seq=ref id; cse_env=ref Map.empty; rbeh=AnnotationReturn} in tev d expr |> apply_seq d
        let on_type_er trace message = TypeError(trace,message) |> raise

        let inline tev2 d a b = tev d a, tev d b
        let inline tev3 d a b c = tev d a, tev d b, tev d c

        let inline inner_compile x = expr_prepass x |> snd |> tev d

        let inline v_find env x on_fail = 
            match Map.tryFind x env with
            | Some v -> v
            | None -> on_fail()

        let get_tag d = 
            let t = !d.ltag
            d.ltag := t + 1
            t

        let make_tyv_ty d ty = get_tag d, ty
        let make_tyv_typed_expr d ty_exp = make_tyv_ty d (get_type ty_exp)

        let make_tyv_and_push_typed_expr d ty_exp =
            let v = make_tyv_typed_expr d ty_exp
            let seq = !d.seq
            d.seq := fun rest -> TyLet(LetStd,v,ty_exp,rest,get_type rest) |> seq
            tyv v

        let make_tyv_and_push_ty d ty =
            let v = make_tyv_ty d ty
            let v' = tyv v
            let seq = !d.seq
            d.seq := fun rest -> TyLet(LetInvisible,v,v',rest,get_type rest) |> seq
            v'

        let cse_add' d r x = let e = !d.cse_env in if r <> x then Map.add r x e else e
        let cse_add d r x = d.cse_env := cse_add' d r x

        // for a shallow version, take a look at `alternative_destructure_v6e.fsx`.
        // The deep version can also be straightforwardly derived from a template of this using the Y combinator.
        let rec destructure d r = 
            let inline destructure r = destructure d r

            let inline chase_cse on_succ on_fail r = 
                match Map.tryFind r !d.cse_env with
                | Some x -> on_succ x
                | None -> on_fail r
            let inline chase_recurse r = chase_cse destructure id r

            let destructure_var r =
                let index_tuple_args tuple_types = 
                    List.mapi (fun i typ -> 
                        destructure <| TyOp(VVIndex,[r;TyLit <| LitInt64 (int64 i)],typ)) tuple_types
                let env_unseal x =
                    let unseal k v = destructure <| TyOp(EnvUnseal,[r; TyLit (LitString k)], v)
                    Map.map unseal x
                match get_type r with
                | VVT (N tuple_types) -> tyvv(index_tuple_args tuple_types)
                | FunT (N (N env,t)) -> tyfun(env_unseal env |> nodify_env_term, t)
                | _ -> chase_recurse r
           
            let inline destructure_cse r = 
                chase_cse 
                    chase_recurse
                    (fun r ->
                        let x = make_tyv_and_push_typed_expr d r
                        cse_add d r x
                        x)
                    r
            
            match r with
            | TyFun _ | TyVV _ | TyLit _ -> r
            | TyBox _ | TyV _ -> destructure_var r
            | TyJoinPoint _ | TyLet _ | TyOp _ -> destructure_cse r

        let inline if_is_returnable ty_x f =
            if is_returnable' ty_x then f()
            else on_type_er d.trace <| sprintf "The following is not a type that can be returned from a if statement. Got: %A" ty_x

        let if_body d cond tr fl =
            let b x = cse_add' d cond (TyLit <| LitBool x)
            let tr = 
                match cond with
                | TyOp(EQ,[b & TyLit _; a & TyV _],_) | TyOp(EQ,[a & TyV _; b & TyLit _],_) -> tev_assume (cse_add' d a b) d tr
                | _ -> tev_assume (b true) d tr
            let fl = tev_assume (b false) d fl
            let type_tr, type_fl = get_type tr, get_type fl
            if type_tr = type_fl then
                if_is_returnable type_tr <| fun () ->
                    match cond with
                    | TyLit(LitBool true) -> tr
                    | TyLit(LitBool false) -> fl
                    | _ -> TyOp(If,[cond;tr;fl],type_tr)
            else on_type_er d.trace <| sprintf "Types in branches of If do not match.\nGot: %A and %A" type_tr type_fl

        let if_cond d tr fl cond =
            if is_bool cond = false then on_type_er d.trace <| sprintf "Expected a bool in conditional.\nGot: %A" (get_type cond)
            else if_body d cond tr fl

        let if_static d cond tr fl =
            match tev d cond with
            | TyLit (LitBool cond) -> 
                let branch = if cond then tr else fl
                tev d branch
            | cond -> if_cond d tr fl cond

        let if_ d cond tr fl = tev d cond |> if_cond d tr fl

        let eval_method memo_type used_vars d expr =
            let key_args = nodify_memo_key (expr, d.env) 

            match memoized_methods.TryGetValue key_args with
            | false, _ ->
                memoized_methods.[key_args] <- MemoMethodInEvaluation
                let typed_expr = tev_method d expr
                memoized_methods.[key_args] <- MemoMethod (memo_type, used_vars, typed_expr)
                typed_expr, key_args
            | true, MemoMethodInEvaluation -> 
                tev_rec d expr, key_args
            | true, MemoMethod (memo_type, used_vars, typed_expr) -> 
                typed_expr, key_args
            | true, _->
                failwith "Expected a method."

        let inline renamables0() = d0(), d0(), d0(), lh0(), lh0()
        let inline eval_renaming memo_type d expr =
            let env = d.env.Expression
            let stopwatch = Diagnostics.Stopwatch.StartNew()
            let _, renamer, renamer_reversed, fv, renamed_fv as k = renamables0()
            let renamed_env = renamer_apply_env k env
            total_time <- total_time + stopwatch.Elapsed

            let memo_type = memo_type renamer
            let typed_expr, memo_key = eval_method memo_type renamed_fv {d with env=renamed_env; ltag=ref renamer.Count} expr
            let typed_expr_ty = get_type typed_expr
            if is_returnable' typed_expr_ty = false then on_type_er d.trace <| sprintf "The following is not a type that can be returned from a method. Consider using Inlineable instead. Got: %A" typed_expr
            else memo_key, fv, renamer_reversed, typed_expr_ty

        let inline memoize_helper memo_type k d x = eval_renaming memo_type d x |> k |> make_tyv_and_push_typed_expr d
        let memoize_method d x = 
            let memo_type = JoinPointMethod
            memoize_helper (fun _ -> memo_type) (fun (memo_key,args,rev_renamer,ret_ty) -> 
                ty_join_point memo_key (memo_type,args,rev_renamer) ret_ty) d x

        let memoize_closure arg d x =
            let _,_,_,fv,_ as r = renamables0()
            let arg_ty = renamer_apply_typedexpr r arg |> get_type
            let memo_type renamer = JoinPointClosure <| renamer_apply_pool renamer fv
            memoize_helper memo_type (fun (memo_key,args,rev_renamer,ret_ty) -> 
                ty_join_point memo_key (JoinPointClosure fv,args,rev_renamer) (closuret(arg_ty,ret_ty))) d x

        let case_ d v case =
            let inline assume d v x branch = tev_assume (cse_add' d v x) d branch
            match tev d v with
            | TyV(_, t & (UnionT _ | RecT _)) as v ->
                let rec case_destructure d args_ty =
                    let inline f x = make_tyv_and_push_ty d x
                    let union_case = function
                        | UnionT (N l) -> Set.toList l |> List.map f
                        | _ -> [f args_ty]
                    match args_ty with
                    | TyRec t -> union_case t
                    | x -> union_case x

                let rec map_cases l =
                    match l with
                    | x :: xs -> (x, assume d v x case) :: map_cases xs
                    | _ -> []
                            
                match map_cases (case_destructure d t) with
                | (_, TyType p) :: _ as cases -> 
                    if List.forall (fun (_, TyType x) -> x = p) cases then TyOp(Case,v :: List.collect (fun (a,b) -> [a;b]) cases, p)
                    else 
                        let l = List.map (snd >> get_type) cases
                        on_type_er d.trace <| sprintf "All the cases in pattern matching clause with dynamic data must have the same type.\n%A" l
                | _ -> failwith "There should always be at least one clause here."
            | a & TyBox(N(b,_)) -> assume d a b case
            | _ -> tev d case
           
        let typec_union d a b =
            let a, b = tev2 d a b
            match get_type a, get_type b with
            | TyTypeC (N a), TyTypeC (N b) -> set_field a + set_field b |> uniont |> typect |> make_tyv_and_push_ty d
            | a, b -> on_type_er d.trace <| sprintf "In type constructor union expected both types to be type constructors. Got: %A and %A" a b

        let rec typec_strip = function 
            | TyTypeC (N x) -> x
            | VVT (N l) -> vvt (List.map typec_strip l)
            | x -> x

        let typec_create d x = 
            let key = nodify_memo_key (x, d.env)
            let inline ret x = make_tyv_and_push_ty d (typect x)

            let inline add_to_memo_dict x = 
                memoized_methods.[key] <- MemoType x
                x

            let inline if_recursive_type_add_to_type_dict x =
                match memoized_methods.TryGetValue key with
                | true, MemoType (RecT tag as ty) -> rect_dict.[tag] <- x; ty
                | _ -> x

            match memoized_methods.TryGetValue key with
            | true, MemoType ty -> ret ty
            | true, MemoTypeInEvaluation ty -> add_to_memo_dict ty |> ret
            | true, _ -> failwith "Expected a type in the dictionary."
            | false, _ -> 
                memoized_methods.[key] <- MemoTypeInEvaluation (RecT memoized_methods.Count)
                tev_seq d x |> get_type |> typec_strip |> if_recursive_type_add_to_type_dict |> add_to_memo_dict |> ret
                
        let inline wrap_exception d f =
            try f()
            with 
            | :? TypeError as e -> reraise()
            | e -> on_type_er d.trace ("This is a .NET exception.\n"+e.Message)

        let dotnet_load_assembly d x =
            match tev d x with
            | TypeString x ->
                wrap_exception d <| fun _ ->
                    System.Reflection.Assembly.Load(x) |> dotnet_assemblyt |> make_tyv_and_push_ty d
            | _ -> on_type_er d.trace "Expected a type level string."

        let (|TyDotNetType|_|) = function
            | TyType (DotNetTypeRuntimeT (N x) | DotNetTypeInstanceT (N x)) -> Some x
            | _ -> None

        let (|TySystemTypeArgs|) args = List.toArray args |> Array.map (get_type >> typec_strip >> dotnet_ty_to_type)

        let (|TyLitIndex|_|) = function
            | TyLit (LitInt32 i) -> Some i
            | TyLit (LitInt64 i) -> Some (int i)
            | TyLit (LitUInt32 i) -> Some (int i)
            | TyLit (LitUInt64 i) -> Some (int i)
            | _ -> None

        let string_length d a = 
            match tev d a with
            | TyLit (LitString str) -> TyLit (LitInt64 (int64 str.Length))
            | TyType(PrimT StringT) & str -> TyOp(StringLength,[str],PrimT Int64T)
            | _ -> on_type_er d.trace "Expected a string."

        let rec apply d a b = 
            match destructure d a, destructure d b with
            // It might make more sense to use TyType rather than TyTag here, but I do not want get_type to be triggered on everything here
            // as it is an expensive operation.

            // apply_for_cast
            | recf & TyFun(N(N env_term,fun_type)), TyV (_,ForCastT (N args_ty)) -> 
                let instantiate_type_as_variable d args_ty =
                    let f x = make_tyv_and_push_ty d x
                    match args_ty with
                    | VVT (N l) -> tyvv(List.map f l)
                    | x -> f x
            
                let args = instantiate_type_as_variable d args_ty
                let inline tev d body = memoize_closure args d body
                match fun_type with
                | FunTypeModule -> on_type_er d.trace <| sprintf "Expected a function in term casting application. Got: %A" fun_type
                // Usually it is against my principles to do inlining by hand, but apply is directly on the hot path 
                // and without me doing this, it would need to take higher order functions.
                | FunTypeRecFunction ((pat,body),name) ->
                    let env = if pat <> "" then Map.add pat args env_term else env_term
                    tev {d with env = Map.add name recf env |> nodify_env_term} body
                | FunTypeFunction (pat,body) -> 
                    tev {d with env = nodify_env_term <| if pat <> "" then Map.add pat args env_term else env_term} body
            // apply_function
            | recf & TyFun(N(N env_term,FunTypeRecFunction ((pat,body),name))), args -> 
                let env = if pat <> "" then Map.add pat args env_term else env_term
                tev {d with env = Map.add name recf env |> nodify_env_term} body
            | TyFun(N(N env_term,FunTypeFunction (pat,body))), args -> 
                tev {d with env = nodify_env_term <| if pat <> "" then Map.add pat args env_term else env_term} body
            // apply_module
            | TyFun(N(N env_term,FunTypeModule)), TypeString n -> v_find env_term n (fun () -> on_type_er d.trace <| sprintf "Cannot find a function named %s inside the module." n)
            | TyFun(N(env_term,FunTypeModule)), _ -> on_type_er d.trace "Expected a type level string in module application."
            // apply_string_static
            | TyLit (LitString str), TyVV(N [TyLitIndex a; TyLitIndex b]) -> 
                let f x = x >= 0 && x < str.Length
                if f a && f b then TyLit(LitString str.[a..b])
                else on_type_er d.trace "The slice into a string literal is out of bounds."
            | TyLit (LitString str), TyLitIndex x -> 
                if x >= 0 && x < str.Length then TyLit(LitChar str.[x])
                else on_type_er d.trace "The index into a string literal is out of bounds."
            // apply_array
            | ar & TyType(ArrayT(N(array_ty,elem_ty))), idx ->
                match array_ty, idx with
                | DotNetHeap, idx when is_int idx -> TyOp(ArrayIndex,[ar;idx],elem_ty) |> make_tyv_and_push_typed_expr d
                | DotNetHeap, idx -> on_type_er d.trace <| sprintf "The index into an array is not an int. Got: %A" idx
                | DotNetReference, TyVV(N []) -> TyOp(ArrayIndex,[ar;idx],elem_ty) |> make_tyv_and_push_typed_expr d
                | DotNetReference, _ -> on_type_er d.trace <| sprintf "The index into a reference is not a unit. Got: %A" idx
                | _ -> failwith "Not implemented."
            // apply_dotnet_type
            | TyType (DotNetAssemblyT (N a)), TypeString name -> 
                    wrap_exception d <| fun _ ->
                        match a.GetType(name) with
                        | null -> on_type_er d.trace "A type cannot be found inside the assembly."
                        | x -> 
                            if x.IsPublic then
                                x |> dotnet_type_runtimet |> make_tyv_and_push_ty d
                            else
                                on_type_er d.trace "Cannot load a private type from an assembly."
            | TyType (DotNetAssemblyT _), _ -> on_type_er d.trace "Expected a type level string as the second argument."
            | dotnet_type & TyType (DotNetTypeRuntimeT (N t) | DotNetTypeInstanceT (N t)), method_name & TypeString name ->
                match t.GetField name with
                | null ->
                    let lam = inl' ["instance";"method_name";"args"] (ap (v "instance") (vv [v "method_name"; v "args"]))
                              |> inner_compile
                    apply d (apply d lam dotnet_type) method_name
                | field ->
                    if field.IsPublic then
                        TyOp(DotNetTypeGetField,[dotnet_type;method_name],field.FieldType |> dotnet_type_to_ty)
                        |> make_tyv_and_push_typed_expr d
                    else
                        on_type_er d.trace "Cannot get a private field."            
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
            | TyType (DotNetTypeRuntimeT (N runtime_type)), args & TyTuple (TySystemTypeArgs system_type_args) ->
                wrap_exception d <| fun _ ->
                    if runtime_type.ContainsGenericParameters then // instantiate generic type params
                        runtime_type.MakeGenericType system_type_args 
                        |> dotnet_type_runtimet |> make_tyv_and_push_ty d
                    else // construct the type
                        match runtime_type.GetConstructor system_type_args with
                        | null -> on_type_er d.trace "Cannot find a constructor with matching arguments."
                        | con ->
                            if con.IsPublic then
                                let instance_type = dotnet_type_instancet runtime_type
                                TyOp(DotNetTypeConstruct,[args],instance_type) |> make_tyv_and_push_typed_expr d
                            else
                                on_type_er d.trace "Cannot call a private constructor."    
            | TyType(DotNetTypeInstanceT _), _ -> on_type_er d.trace "Expected a type level string as the first argument for a method call."
            // apply_typec
            | typec & TyType(TyTypeC (N ty)), args ->
                let substitute_ty = function
                    | TyBox(N(x,_)) -> tybox(x,ty)
                    | x -> tybox(x,ty)

                let (|TyRecUnion|_|) = function
                    | UnionT (N ty') -> Some ty'
                    | TyRec t -> Some (set_field t)
                    | _ -> None

                match ty, args with
                | x, TyType r when x = r -> args
                | TyRecUnion ty', TyType (UnionT (N ty_args)) when Set.isSubset ty_args ty' ->
                    let lam = inl' ["typec"; "args"] (op(Case,[v "args"; ap (v "typec") (v "args")])) |> inner_compile
                    apply d (apply d lam typec) args
                | TyRecUnion ty', TyType x when Set.contains x ty' -> substitute_ty args
                | _ -> on_type_er d.trace <| sprintf "Type constructor application failed. %A does not intersect %A." ty (get_type args)
            // apply_string
            | TyType(PrimT StringT) & str, TyVV(N [a;b]) -> 
                if is_int a && is_int b then TyOp(StringSlice,[str;a;b],PrimT StringT) |> destructure d
                else on_type_er d.trace "Expected an int as the second argument to string index."
            | TyType(PrimT StringT) & str, idx -> 
                if is_int idx then TyOp(StringIndex,[str;idx],PrimT CharT) |> destructure d
                else on_type_er d.trace "Expected an int as the second argument to string index."
            // apply_closure
            | closure & TyType(ClosureT(N (clo_arg_ty,clo_ret_ty))), args -> 
                let arg_ty = get_type args
                if arg_ty <> clo_arg_ty then on_type_er d.trace <| sprintf "Cannot apply an argument of type %A to closure (%A -> %A)." arg_ty clo_arg_ty clo_ret_ty
                else TyOp(Apply,[closure;args],clo_ret_ty) |> make_tyv_and_push_typed_expr d
            | a,b -> on_type_er d.trace <| sprintf "Invalid use of apply. %A and %A" a b

        let apply_tev d expr args = apply d (tev d expr) (tev d args)

        let inline vv_index_template f d v i =
            let v,i = tev2 d v i
            match v, i with
            | TyVV (N l), TyLitIndex i ->
                if i >= 0 || i < List.length l then f l i
                else on_type_er d.trace "Tuple index not within bounds."
            | v & TyType (VVT ts), TyLitIndex i -> failwith "The tuple should ways be destructured."
            | v, TyLitIndex i -> on_type_er d.trace <| sprintf "Type of an evaluated expression in tuple index is not a tuple.\nGot: %A" v
            | v, i -> on_type_er d.trace <| sprintf "Index into a tuple must be an at least a i32 less than the size of the tuple.\nGot: %A" i

        let vv_index d v i = vv_index_template (fun l i -> l.[i]) d v i |> destructure d
        let vv_slice_from d v i = vv_index_template (fun l i -> tyvv l.[i..]) d v i

        let inline vv_unop_template on_succ on_fail d v =
            match tev d v with
            | TyVV (N l) -> on_succ l
            | v & TyType (VVT ts) -> failwith "The tuple should ways be destructured."
            | v -> on_fail()

        let vv_length x = 
            vv_unop_template (fun l -> l.Length |> int64 |> LitInt64 |> TyLit) 
                (fun _ -> on_type_er d.trace <| sprintf "Type of an evaluated expression in tuple index is not a tuple.\nGot: %A" v) x
                
        let vv_is x = vv_unop_template (fun _ -> TyLit (LitBool true)) (fun _ -> TyLit (LitBool false)) x

        let eq_type d a b =
            let f a = typec_strip (get_type a) 
            let a, b = tev2 d a b 
            LitBool (f a = f b) |> TyLit
    
        let vv_cons d a b =
            let a, b = tev2 d a b
            match b with
            | TyVV (N b) -> tyvv(a::b)
            | _ -> on_type_er d.trace "Expected a tuple on the right in VVCons."

        let type_lit_create' d x = litt x |> make_tyv_and_push_ty d

        let module_open d a b =
            let a = tev d a
            match a with
            | TyFun(N(N env_term, FunTypeModule)) -> 
                let env = Map.fold (fun s k v -> Map.add k v s) d.env.Expression env_term
                tev {d with env = env |> nodify_env_term} b
            | x -> on_type_er d.trace <| sprintf "The open expected a module type as input. Got: %A" x

        let module_with_f_extend d module_ name arg =
            let x = Map.add name arg module_ |> nodify_env_term
            tyfun(x, FunTypeModule)

        let module_with_f d module_ name arg =
            match Map.tryFind name module_ with
            | Some arg' ->
                if get_type arg = get_type arg' then module_with_f_extend d module_ name arg
                else on_type_er d.trace <| sprintf "Cannot extend module with %s due to difference in types. Use the extensible `upon'` if that is the desired behavior." name
            | None -> on_type_er d.trace <| sprintf "Cannot extend module with %s due to it being missing in the module. Use the extensible `upon'` if that is the desired behavior." name

        let inline module_with_template f d module_ name arg =
            let module_, name, arg = tev3 d module_ name arg
            match module_, name with
            | TyFun(N(N module_,FunTypeModule)), TypeString name -> f d module_ name arg
            | TyFun(N(N module_,FunTypeModule)), _ -> on_type_er d.trace "Expected a type level string as the second argument."
            | x -> on_type_er d.trace (sprintf "Expected a module as the first argument. Got: %A" x)

        let module_with x = module_with_template module_with_f x
        let module_with_extend x = module_with_template module_with_f_extend x

        let type_annot d a b =
            match d.rbeh with
            | AnnotationReturn -> tev d b |> get_type |> typec_strip |> make_tyv_and_push_ty d
            | AnnotationDive ->
                let f a = typec_strip (get_type a) 
                let a, b = tev d a, tev_seq d b
                let ta, tb = f a, f b
                if ta = tb then a else on_type_er d.trace <| sprintf "Type annotation mismatch.\n%A <> %A" ta tb

        let inline prim_bin_op_template d check_error is_check k a b t =
            let a, b = tev2 d a b
            if is_check a b then k t a b
            else on_type_er d.trace (check_error a b)

        let inline prim_bin_op_helper t a b = TyOp(t,[a;b],get_type a)
        let inline prim_un_op_helper t a = TyOp(t,[a],get_type a)
        let inline bool_helper t a b = TyOp(t,[a;b],PrimT BoolT)

        let prim_arith_op d a b t = 
            let er a b = sprintf "`is_numeric a && get_type a = get_type b` is false.\na=%A, b=%A" a b
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
                ) a b t

        let prim_comp_op d a b t = 
            let er a b = sprintf "(is_char a || is_string a || is_numeric a || is_bool a) && get_type a = get_type b` is false.\na=%A, b=%A" a b
            let check a b = (is_char a || is_string a || is_numeric a || is_bool a) && get_type a = get_type b
            prim_bin_op_template d er check (fun t a b ->
                let inline eq_op a b = LitBool (a = b) |> TyLit
                match t, a, b with
                | EQ, TyV(a,_), TyV(b,_) when a = b -> LitBool true |> TyLit
                | EQ, TyLit (LitBool a), TyLit (LitBool b) -> eq_op a b
                | EQ, TyLit (LitString a), TyLit (LitString b) -> eq_op a b
                | _ ->
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
                        | LitString a, LitString b -> op a b |> LitBool |> TyLit
                        | LitChar a, LitChar b -> op a b |> LitBool |> TyLit
                        | _ -> bool_helper t a b
                    | _ -> bool_helper t a b
                    ) a b t

        let prim_bool_op d a b t = 
            let er a b = sprintf "`is_bool a && get_type a = get_type b` is false.\na=%A, b=%A" a b
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
                ) a b t

        let prim_shift_op d a b t =
            let er a b = sprintf "`is_int a && is_int b` is false.\na=%A, b=%A" a b
            let check a b = is_int a && is_int b
            prim_bin_op_template d er check prim_bin_op_helper a b t

        let prim_shuffle_op d a b t =
            let er a b = sprintf "`is_int b` is false.\na=%A, b=%A" a b
            let check a b = is_int b
            prim_bin_op_template d er check prim_bin_op_helper a b t

        let prim_un_op_template d check_error is_check k a t =
            let a = tev d a
            if is_check a then k t a
            else on_type_er d.trace (check_error a)

        let prim_un_floating d a t =
            let er a = sprintf "`is_float a` is false.\na=%A" a
            let check a = is_float a
            prim_un_op_template d er check prim_un_op_helper a t

        let prim_un_numeric d a t =
            let er a = sprintf "`is_numeric a` is false.\na=%A" a
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
                ) a t

        let for_cast d x = tev_seq d x |> get_type |> typec_strip |> for_castt |> make_tyv_and_push_ty d
        let error_non_unit d a =
            let x = tev d a 
            if get_type x <> BVVT then on_type_er d.trace "Only the last expression of a block is allowed to be unit. Use `ignore` if it intended to be such."
            else x

        let type_lit_create d a =
            match tev d a with
            | TyLit a -> type_lit_create' d a
            | _ -> on_type_er d.trace "Expected a literal in type literal create."

        let dynamize d a =
            match tev d a with
            | TyBox(N(_, (UnionT _ | RecT _))) | TyLit _ as a -> make_tyv_and_push_typed_expr d a
            | a -> a

        let module_create d l =
            let rec loop acc = function
                | V (N x) -> x :: acc
                | VV (N l) -> List.fold loop acc l
                | ExprPos p -> loop acc p.Expression
                | x -> on_type_er d.trace <| sprintf "Only variable names are allowed in module create. Got: %A" x
            let er n _ = on_type_er d.trace <| sprintf "In module create, the variable %s was not found." n
            let env = List.map (fun n -> n, v_find d.env.Expression n (er n)) (loop [] l) |> Map |> nodify_env_term
            tyfun(env, FunTypeModule)

        let array_create d size typ =
            let typ = tev_seq d typ |> get_type |> typec_strip

            let size,array_type =
                match tev d size with
                | size when is_int size -> size,arrayt(DotNetHeap,typ)
                | size -> on_type_er d.trace <| sprintf "An size argument in CreateArray is not of type int64.\nGot: %A" size

            TyOp(ArrayCreate,[size],array_type) |> make_tyv_and_push_typed_expr d

        let reference_create d x =
            let x = tev d x
            let array_type = arrayt(DotNetReference, get_type x)
            TyOp(ReferenceCreate,[x],array_type) |> make_tyv_and_push_typed_expr d

        let array_set d ar idx r =
            match tev3 d ar idx r with
            | ar & TyType (ArrayT(N(DotNetHeap,t))), idx, r when is_int idx && t = get_type r -> 
                make_tyv_and_push_typed_expr d (TyOp(ArraySet,[ar;idx;r],BVVT))
            | ar & TyType (ArrayT(N(DotNetReference,t))), idx & TyVV(N []), r when t = get_type r -> 
                make_tyv_and_push_typed_expr d (TyOp(ArraySet,[ar;idx;r],BVVT))
            | x -> on_type_er d.trace <| sprintf "The two sides in array set have different types. %A" x

        let array_length d ar =
            match tev d ar with
            | ar & TyType (ArrayT(N(DotNetHeap,t)))-> 
                make_tyv_and_push_typed_expr d (TyOp(ArrayLength,[ar],PrimT Int64T))
            | ar & TyType (ArrayT(N(DotNetReference,t)))-> 
                TyLit (LitInt64 1L)
            | x -> on_type_er d.trace <| sprintf "ArrayLength is only supported for .NET arrays. Got: %A" x

        let is_static d x = 
            match tev d x with
            | TyLit _ -> TyLit <| LitBool true
            | _ -> TyLit <| LitBool false

        let module_is d a =
            match tev d a with
            | TyFun(N(_,FunTypeModule)) -> TyLit (LitBool true)
            | _ -> TyLit (LitBool false)

        let module_has_member d a b =
            match tev2 d a b with
            | TyFun(N(N env,FunTypeModule)), TypeString b -> TyLit (LitBool <| Map.containsKey b env)
            | TyFun(N(N env,FunTypeModule)), _ -> on_type_er d.trace "Expecting a type literals as the second argument to ModuleHasMember."
            | x -> on_type_er d.trace <| sprintf "Expecting a module as the first argument to ModuleHasMember. Got: %A" x

        let module_create_alt d l =
            List.fold (fun env -> function
                | VV(N [Lit(N(LitString n)); e]) -> Map.add n (tev d e |> destructure d) env
                | _ -> failwith "impossible"
                ) Map.empty l
            |> fun x -> tyfun(nodify_env_term x, FunTypeModule)

        let module_with_alt (d: LangEnv) l =
            let names, bindings =
                match l with
                | VV (N l) :: bindings -> l, bindings
                | V _ as x :: bindings -> [x], bindings
                | x -> failwithf "Malformed ModuleWithAlt. %A" x

            let rec loop cur_env names = 
                let inline f name f =
                    match Map.tryFind name cur_env with
                    | Some (TyFun(N(N env,FunTypeModule)) as recf) -> f env
                    | Some _ -> on_type_er d.trace <| sprintf "Variable %s is not a module." name
                    | _ -> on_type_er d.trace <| sprintf "Module %s is not bound in the environment." name
                match names with
                | V(N name) :: names -> f name (fun env -> loop env names)
                | Lit(N(LitString name)) :: names -> f name (fun env -> 
                    tyfun (Map.add name (loop env names) cur_env |> nodify_env_term, FunTypeModule)
                    )
                | [] ->
                    List.fold (fun env -> function
                        | VV(N [Lit(N(LitString name)); e]) ->
                            match Map.tryFind name env with
                            | Some v -> {d with env = Map.add "self" v (n d.env) |> nodify_env_term}
                            | None -> d
                            |> fun d -> Map.add name (tev d e |> destructure d) env
                        | _ -> failwith "impossible"
                        ) cur_env bindings
                    |> fun env -> tyfun(nodify_env_term env, FunTypeModule)
                | x -> failwithf "Malformed ModuleWithAlt. %A" x
            loop (n d.env) names

        let failwith_ d a =
            match tev d a with
            | TyType (PrimT StringT) as a -> TyOp(FailWith,[a],BVVT)
            | _ -> on_type_er d.trace "Expected a string as input to failwith."
            
        let inline add_trace d x = {d with trace = x :: d.trace}

        match expr with
        | Lit (N value) -> TyLit value
        | V (N x) -> v_find d.env.Expression x (fun () -> on_type_er d.trace <| sprintf "Variable %A not bound." x) |> destructure d
        | FunctionFilt(N (vars,N (pat, body))) -> 
            let env = Map.filter (fun k _ -> Set.contains k vars) d.env.Expression |> nodify_env_term
            let pat = if vars.Contains pat then pat else ""
            tyfun(env, FunTypeFunction (pat, body))
        | Function core -> failwith "Function not allowed in this phase as it tends to cause stack overflows in recursive scenarios."
        | Pattern pat -> failwith "Pattern not allowed in this phase as it tends to cause stack overflows when prepass is triggered in the match case."
        | ExprPos p -> tev (add_trace d p.Pos) p.Expression
        | VV (N vars) -> List.map (tev d >> destructure d) vars |> tyvv
        | Op(N (op,vars)) ->
            match op,vars with
            | Apply,[a;b] -> apply_tev d a b
            | StringLength,[a] -> string_length d a
            | DotNetLoadAssembly,[a] -> dotnet_load_assembly d a
            | Fix,[Lit (N (LitString name)); body] ->
                match tev d body with
                | TyFun(N(env_term,FunTypeFunction core)) -> tyfun(env_term,FunTypeRecFunction(core,name))
                | x -> failwithf "Invalid use of Fix. Got: %A" x
            | Case,[v;case] -> case_ d v case
            | IfStatic,[cond;tr;fl] -> if_static d cond tr fl
            | If,[cond;tr;fl] -> if_ d cond tr fl
            | JoinPoint,[a] -> memoize_method d a
            | ForCast,[x] -> for_cast d x
            | PrintStatic,[a] -> printfn "%A" (tev d a); TyB
            | PrintEnv,[a] -> 
                Map.iter (fun k _ -> printfn "%s" k) (n d.env)
                //printfn "%A" d.env; 
                tev d a
            | PrintExpr,[a] -> printfn "%A" a; tev d a
            | ModuleOpen,[a;b] -> module_open d a b
            | ModuleCreate,[l] -> module_create d l
            | ModuleWith,[a;b;c] -> module_with d a b c
            | ModuleWithExtend,[a;b;c] -> module_with_extend d a b c
            | ModuleCreateAlt,l -> module_create_alt d l
            | ModuleWithAlt, l -> module_with_alt d l
            | TypeLitCreate,[a] -> type_lit_create d a
            | Dynamize,[a] -> dynamize d a
            | IsStatic,[a] -> is_static d a
            | ModuleIs,[a] -> module_is d a

            | ArrayCreate,[a;b] -> array_create d a b
            | ReferenceCreate,[a] -> reference_create d a
            | ArraySet,[a;b;c] -> array_set d a b c
            | ArrayLength,[a] -> array_length d a

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
            | ModuleHasMember,[a;b] -> module_has_member d a b
            | Neg,[a] -> prim_un_numeric d a Neg
            | ErrorType,[a] -> tev d a |> fun a -> on_type_er d.trace <| sprintf "%A" a
            | ErrorNonUnit,[a] -> error_non_unit d a

            | Log,[a] -> prim_un_floating d a Log
            | Exp,[a] -> prim_un_floating d a Exp
            | Tanh,[a] -> prim_un_floating d a Tanh
            | FailWith,[a] -> failwith_ d a

            // Constants
            | TypeConstructorCreate,[a] -> typec_create d a
            | x -> failwithf "Missing Op case. %A" x


    // #Parsing
    let spiral_parse (Module(N(module_name,_,_,module_code)) & module_) = 
        let pos' (s: CharStream<_>) = module_, s.Line, s.Column
        let pos expr (s: CharStream<_>) = (expr |>> expr_pos (pos' s)) s

        let patpos expr (s: CharStream<_>) = 
            let p = pos' s
            (expr |>> fun expr -> pat_pos p expr) s

        let rec spaces_template spaces s = spaces >>. optional (followedByString "//" >>. skipRestOfLine true >>. spaces_template spaces) <| s
        let spaces, spaces1 = spaces_template spaces, spaces_template spaces1
    
        let is_identifier_starting_char c = isAsciiLetter c || c = '_'
        let is_identifier_char c = is_identifier_starting_char c || c = ''' || isDigit c 

        let var_name =
            many1Satisfy2L is_identifier_starting_char is_identifier_char "identifier" .>> spaces
            >>=? function
                | "match" | "function" | "with" | "open" | "module" | "as" | "when" | "print_env" | "inl" | "met" | "inm"
                | "print_expr" | "rec" | "if" | "then" | "elif" | "else" | "true" | "false" as x -> 
                    fun _ -> Reply(Error,messageError <| sprintf "%s not allowed as an identifier." x)
                | x -> preturn x

        let between_brackets l p r = between (skipChar l .>> spaces) (skipChar r .>> spaces) p
        let rounds p = between_brackets '(' p ')'
        let curlies p = between_brackets '{' p '}'
        let quares p = between_brackets '[' p ']'

        let keywordChar x = skipChar x .>> spaces
        let keywordString x = skipString x .>> spaces
        let keywordString1 x = skipString x .>> spaces1

        let when_ = keywordString "when"
        let as_ = keywordString "as"
        let negate_ = keywordChar '-'
        let comma = keywordChar ','
        let dot = keywordChar '.'
        let grave = keywordChar '`' 
        let pp = keywordChar ':'
        let semicolon = keywordChar ';' 
        let eq = keywordChar '=' 
        let bar = keywordChar '|' 
        let amphersand = keywordChar '&'
        let barbar = keywordString "||" 
        let lam = keywordString "->"
        let set_ref = keywordString ":="
        let set_array = keywordString "<-"
        let inl_ = keywordString "inl"
        let inm_ = keywordString "inm"
        let met_ = keywordString "met"
        let inl_rec = keywordString1 "inl" .>> keywordString "rec"
        let met_rec = keywordString1 "met" .>> keywordString "rec"
        let match_ = keywordString "match"
        let function_ = keywordString "function"
        let module_ = keywordString "module"
        let with_ = keywordString "with"
        let open_ = keywordString "open"
        let cons = keywordString "::"
        let active_pat = keywordChar '!'
        let part_active_pat = keywordChar '@'
        let ext_active_pat = keywordChar '#'
        let type_' = keywordString "type"
        let wildcard = keywordChar '_'

        let pbool = (skipString "false" >>% LitBool false) <|> (skipString "true" >>% LitBool true)
        let pnumber : Parser<_,_> =
            let numberFormat =  NumberLiteralOptions.AllowFraction
                                ||| NumberLiteralOptions.AllowExponent
                                ||| NumberLiteralOptions.AllowHexadecimal
                                ||| NumberLiteralOptions.AllowBinary
                                ||| NumberLiteralOptions.AllowInfinity
                                ||| NumberLiteralOptions.AllowNaN

            let parser = numberLiteral numberFormat "number"

            let inline safe_parse f on_succ er_msg x = 
                match f x with
                | true, x -> Reply(on_succ x)
                | false, _ -> Reply(ReplyStatus.FatalError,messageError er_msg)

            let default_int x _ = safe_parse Int64.TryParse LitInt64 "default int parse failed" x
            let default_float x _ = safe_parse Single.TryParse LitFloat32 "default float parse failed" x

            let int8 x _ = safe_parse SByte.TryParse LitInt8 "int8 parse failed" x
            let int16 x _ = safe_parse Int16.TryParse LitInt16 "int16 parse failed" x
            let int32 x _ = safe_parse Int32.TryParse LitInt32 "int32 parse failed" x
            let int64 x _ = safe_parse Int64.TryParse LitInt64 "int64 parse failed" x

            let uint8 x _ = safe_parse Byte.TryParse LitUInt8 "uint8 parse failed" x
            let uint16 x _ = safe_parse UInt16.TryParse LitUInt16 "uint16 parse failed" x
            let uint32 x _ = safe_parse UInt32.TryParse LitUInt32 "uint32 parse failed" x
            let uint64 x _ = safe_parse UInt64.TryParse LitUInt64 "uint64 parse failed" x

            let float32 x _ = safe_parse Single.TryParse LitFloat32 "float32 parse failed" x
            let float64 x _ = safe_parse Double.TryParse LitFloat64 "float64 parse failed" x

            let followedBySuffix x is_x_integer =
                let f c l = 
                    let l = Array.map (fun (k,m) -> skipString k >>= fun _ -> m x) l
                    skipChar c >>. choice l
                choice
                    [|
                    f 'i'
                        [|
                        "8", int8
                        "16", int16
                        "32", int32
                        "64", int64
                        |]

                    f 'u'
                        [|
                        "8", uint8
                        "16", uint16
                        "32", uint32
                        "64", uint64
                        |]

                    f 'f'
                        [|
                        "32", float32
                        "64", float64
                        |]
                    (if is_x_integer then default_int x else default_float x)
                    |]

            fun s ->
                let reply = parser s
                if reply.Status = Ok then
                    let nl = reply.Result // the parsed NumberLiteral
                    try 
                        followedBySuffix nl.String nl.IsInteger s
                    with
                    | :? System.OverflowException as e ->
                        s.Skip(-nl.String.Length)
                        Reply(FatalError, messageError e.Message)
                else // reconstruct error reply
                    Reply(reply.Status, reply.Error)

        let quoted_char = 
            let normalChar = satisfy (fun c -> c <> '\\' && c <> ''')
            let unescape c = match c with
                             | 'n' -> '\n'
                             | 'r' -> '\r'
                             | 't' -> '\t'
                             | c   -> c
            let escapedChar = pchar '\\' >>. (anyOf "\\nrt'" |>> unescape)
            let a = (normalChar <|> escapedChar) .>> pchar ''' |>> LitChar
            let b = pstring "''" >>% LitChar '''
            pchar ''' >>. (a <|> b)

        let quoted_string =
            let normalChar = satisfy (fun c -> c <> '\\' && c <> '"')
            let unescape c = match c with
                             | 'n' -> '\n'
                             | 'r' -> '\r'
                             | 't' -> '\t'
                             | c   -> c
            let escapedChar = pchar '\\' >>. (anyOf "\\nrt\"" |>> unescape)
            between (pchar '"') (pchar '"')
                    (manyChars (normalChar <|> escapedChar))
            |>> LitString

        let lit_ s = 
            choice 
                [|
                pbool
                pnumber .>> nextCharSatisfiesNot is_identifier_char
                quoted_string
                quoted_char
                |] .>> spaces
            <| s

        let pat_e = wildcard >>% E
        let pat_var = var_name |>> PatVar
        let pat_tuple pattern = sepBy1 pattern comma |>> function [x] -> x | x -> PatTuple x
        let pat_cons pattern = sepBy1 pattern cons |>> function [x] -> x | x -> PatCons x
        let pat_rounds pattern = rounds (pattern <|>% PatTuple [])
        // TODO: That `notFollowedBy cons` is such a hack.
        // I should have made the whole parser follow the pattern of reading a whole token first and then comparing
        // instead of the thing I do now where I use skipString. It is too bad it did not occur to me that designing
        // this way would be a good idea. I am going to reimplement it like that on the next redesign.
        let pat_type expr pattern = tuple2 pattern (opt (notFollowedBy cons >>. pp >>. ((var_name |>> v) <|> rounds expr))) |>> function a,Some b as x-> PatType(a,b) | a, None -> a
        let pat_active pattern = 
            let active_pat = choice [active_pat >>% PatActive; part_active_pat >>% PatPartActive; ext_active_pat >>% PatExtActive]
            (pipe3 active_pat var_name pattern <| fun c name pat -> c (name,pat)) <|> pattern
        let pat_or pattern = sepBy1 pattern bar |>> function [x] -> x | x -> PatOr x
        let pat_and pattern = sepBy1 pattern amphersand |>> function [x] -> x | x -> PatAnd x
        let pat_type_lit = dot >>. (lit_ <|> (var_name |>> LitString)) |>> PatTypeLit
        let pat_lit = lit_ |>> PatLit
        let pat_when expr pattern = pattern .>>. (opt (when_ >>. expr)) |>> function a, Some b -> PatWhen(a,b) | a, None -> a
        let pat_as pattern = pattern .>>. (opt (as_ >>. pattern )) |>> function a, Some b -> PatAnd [a;b] | a, None -> a

        let (^<|) a b = a b // High precedence, right associative <| operator

        let rec pat_module_helper (names,bindings) = 
            match names with
            | [x] -> PatModuleInner(x,bindings)
            | x :: xs -> PatModuleInner(x,[pat_module_helper (xs,bindings)])
            | _ -> failwith "Invalid state"
        let rec pat_module_outer expr s = 
            let parse_binding = var_name .>>. opt (eq >>. patterns_template expr pat_module_outer) |>> PatModuleBinding
            curlies (opt (attempt (sepBy1 var_name dot .>> with_)) .>>. many (pat_module_inner expr <|> parse_binding)) //|>> PatModuleOuter <| s
            |>> function
                | Some (n :: (_ :: _ as ns)), bindings -> PatModuleOuter(Some n, [pat_module_helper (ns,bindings)])
                | Some [n], bindings -> PatModuleOuter(Some n,bindings)
                | Some [], _ -> failwith "impossible"
                | None, bindings -> PatModuleOuter(None,bindings)
            <| s
        and pat_module_inner expr s = 
            let parse_binding = var_name .>>. opt (eq >>. patterns_template expr pat_module_outer) |>> PatModuleBinding
            curlies ((sepBy1 var_name dot .>> with_) .>>. many (pat_module_inner expr <|> parse_binding)) 
            |>> pat_module_helper <| s

        and patterns_template expr pat_module s = // The order in which the pattern parsers are chained in determines their precedence.
            let inline recurse s = patterns_template expr pat_module s
            pat_or ^<| pat_when expr ^<| pat_as ^<| pat_tuple ^<| pat_cons ^<| pat_and ^<| pat_type expr ^<| pat_active 
            ^<| choice [|pat_e; pat_var; pat_type_lit; pat_lit; pat_rounds recurse; pat_module expr|] <| s

        let inline patterns expr s = patterns_template expr pat_module_outer s
    
        let pattern_list expr = many (patterns expr)
    
        let col (s: CharStream<_>) = s.Column

        let expr_indent i op expr (s: CharStream<_>) = if op i (col s) then expr s else pzero s
        let if_then_else expr (s: CharStream<_>) =
            let i = (col s)
            let expr_indent expr (s: CharStream<_>) = expr_indent i (<=) expr s
            let inline f' str = skipString str >>. spaces1 >>. expr
            let inline f str = expr_indent (f' str)
            pipe4 (f' "if") (f "then") (many (f "elif" .>>. f "then")) (opt (f "else"))
                (fun cond tr elifs fl -> 
                    let fl = 
                        match fl with Some x -> x | None -> B
                        |> List.foldBack (fun (cond,tr) fl -> op(If,[cond;tr;fl])) elifs
                    op(If,[cond;tr;fl]))
            <| s

        let is_operator c = (is_identifier_char c || isAnyOf [|' ';',';'\t';'\n';'\"';'(';')';'{';'}';'[';']'|] c) = false
        let poperator (s: CharStream<Userstate>) = many1Satisfy is_operator .>> spaces <| s

        let name = var_name <|> rounds poperator

        let filter_env x = ap (inl "" x) B
        let inl_pat' (args: Pattern list) body = List.foldBack inl_pat args body
        let meth_pat' args body = 
            let body = meth_memo body
            if List.isEmpty args then filter_env body else body
            |> inl_pat' args
    
        let case_inl_pat_statement expr = pipe2 (inl_ >>. patterns expr) (eq >>. expr) lp
        let case_inl_name_pat_list_statement expr = pipe3 (inl_ >>. name) (pattern_list expr) (eq >>. expr) (fun name pattern body -> l name (inl_pat' pattern body)) 
        let case_inl_rec_name_pat_list_statement expr = pipe3 (inl_rec >>. name) (pattern_list expr) (eq >>. expr) (fun name pattern body -> l_rec name (inl_pat' pattern body))
        
        let case_met_pat_statement expr = pipe2 (met_ >>. patterns expr) (eq >>. expr) <| fun pattern body -> lp pattern (filter_env (meth_memo body))
        let case_met_name_pat_list_statement expr = pipe3 (met_ >>. name) (pattern_list expr) (eq >>. expr) (fun name pattern body -> l name (meth_pat' pattern body))
        let case_met_rec_name_pat_list_statement expr = pipe3 (met_rec >>. name) (pattern_list expr) (eq >>. expr) <| fun name pattern body -> l_rec name (meth_pat' pattern body)

        let case_open expr = open_ >>. expr |>> module_open

        let case_inm_pat_statement expr = pipe2 (inm_ >>. patterns expr) (eq >>. expr) inmp

        let statements expr = 
            [case_inl_pat_statement; case_inl_name_pat_list_statement; case_inl_rec_name_pat_list_statement
             case_met_pat_statement; case_met_name_pat_list_statement; case_met_rec_name_pat_list_statement
             case_open; case_inm_pat_statement]
            |> List.map (fun x -> x expr |> attempt)
            |> choice

        let change_semicolon_ignore_to_true expr (s: CharStream<Userstate>) =
            let u = s.UserState
            if u.ignore_semicolon = false then
                s.UserState <- {u with ignore_semicolon = true}
                let x = expr s
                s.UserState <- u
                x
            else expr s

        let case_inl_pat_list_expr expr = pipe2 (inl_ >>. pattern_list expr) (lam >>. change_semicolon_ignore_to_true expr) inl_pat'
        let case_met_pat_list_expr expr = pipe2 (met_ >>. pattern_list expr) (lam >>. change_semicolon_ignore_to_true expr) meth_pat'

        let case_lit expr = lit_ |>> lit
        let case_if_then_else expr = change_semicolon_ignore_to_true (if_then_else expr)
        let case_rounds expr s = change_semicolon_ignore_to_true (rounds (expr <|>% B)) s
        let case_var expr = name |>> v

        let case_typex match_type expr (s: CharStream<_>) =
            let mutable i = None
            let expr_indent op expr (s: CharStream<_>) = expr_indent i.Value op expr s
    
            let clause = 
                let clause_template is_meth = 
                    pipe2 (many1 (patterns expr) .>> lam) expr <| fun pat body ->
                        match pat with
                        | x :: xs -> x, if is_meth then meth_pat' xs body else inl_pat' xs body
                        | _ -> failwith "impossible"

                poperator >>=? function
                    | "|" -> clause_template false
                    | "||" -> clause_template true
                    | _ -> fail "not a pattern matching clause"
                |> expr_indent (<=) 
            
            let set_col (s: CharStream<_>) = i <- Some ((col s)); Reply(())

            let pat_function l = pattern (PatClauses l)
            let pat_match x l = ap (pat_function l) x

            match match_type with
            | true -> // function
                (function_ >>. set_col >>. many1 clause
                |>> pat_function) s    
            | false -> // match
                pipe2 (match_ >>. expr .>> with_ .>> set_col)
                    (many1 clause)
                    pat_match s

        let case_typeinl expr (s: CharStream<_>) = case_typex true expr s
        let case_typecase expr (s: CharStream<_>) = case_typex false expr s

        let case_module expr = module_ >>. expr |>> module_create
        let case_for_cast expr = grave >>. expr |>> ap (v "for_cast")
        let case_lit_lift expr = 
            let var = var_name |>> (LitString >> lit >> ap (v "lit_lift"))
            let lit = expr |>> ap (v "lit_lift")
            dot >>. (var <|> lit)

        let case_print_env expr s = 
            let i = col s
            keywordString "print_env" >>. expr_indent i (<=) expr |>> print_env
            <| s

        let case_print_expr expr s = 
            let i = col s
            keywordString "print_expr" >>. expr_indent i (<=) expr |>> print_expr
            <| s

        let inline expr_indent_semicolon is_module i expr' (s: CharStream<_>) =
            let inline if_ op tr s = expr_indent i op tr s
            let mutable op = (=)
            let inline set_op op' x = op <- op'; x
            let inline semicolon s = if_ (<) (semicolon |>> set_op (<=)) s
            let inline expr s = if_ op (expr' |>> set_op (=)) s
            let inline f s = many1 (expr .>> optional semicolon) s
            let u = s.UserState
            match is_module, u.ignore_semicolon with
            | true, false | false, true -> f s
            | true, true -> 
                s.UserState <- {u with ignore_semicolon=false}
                let x = f s
                s.UserState <- u
                x
            | false, false -> many1 (if_ (=) expr') s
            
                

        let case_module_alt expr s =
            let mp_binding (n,e) = vv [lit_string n; e]
            let mp_create l = op(ModuleCreateAlt,l)
            let mp_with (n,l) = 
                match n with
                | [x] -> op(ModuleWithAlt,v x :: l)
                | x :: xs -> op(ModuleWithAlt,vv (v x :: List.map lit_string xs) :: l)
                | _ -> failwith "impossible"
                
            let parse_binding s = 
                let i = col s
                var_name .>>. (eq >>. expr_indent i (<) expr) |>> mp_binding <| s
            let module_create s = 
                let i = col s
                expr_indent_semicolon true i parse_binding s

            let module_with = 
                attempt (sepBy1 var_name dot .>> with_) >>= fun names ->
                    (module_create |>> fun l -> mp_with (names,l))
            curlies (module_with <|> (module_create |>> mp_create))
            <| s

        let rec expressions expr s =
            let unary_ops = 
                [case_for_cast; case_lit_lift]
                |> List.map (fun x -> x (expressions expr) |> attempt)
                |> choice
            let rest = 
                [case_print_env; case_print_expr
                 case_inl_pat_list_expr; case_met_pat_list_expr; case_lit; case_if_then_else
                 case_rounds; case_typecase; case_typeinl; case_var; case_module; case_module_alt]
                |> List.map (fun x -> x expr |> attempt)
                |> choice
            unary_ops <|> rest <| s
 
        let process_parser_exprs exprs = 
            let error_statement_in_last_pos _ = Reply(Error,messageError "Statements not allowed in the last position of a block.")
            let rec process_parser_exprs on_succ = function
                | [ParserExpr (p,a)] -> on_succ (expr_pos p a)
                | [ParserStatement _] -> error_statement_in_last_pos
                | ParserStatement (p,a) :: xs -> process_parser_exprs (expr_pos p >> a >> on_succ) xs
                | ParserExpr (p,a) :: xs -> process_parser_exprs (l "" (error_non_unit (expr_pos p a)) >> on_succ) xs
                | [] -> preturn B
            
            process_parser_exprs preturn exprs

        let indentations statements expressions (s: CharStream<Userstate>) =
            let i = (col s)
            let pos' s = Reply(pos' s)
            expr_indent_semicolon false i ((tuple2 pos' statements |>> ParserStatement) <|> (tuple2 pos' expressions |>> ParserExpr)) >>= process_parser_exprs <| s

        let application expr (s: CharStream<_>) =
            let i = (col s)
            let expr_up (s: CharStream<_>) = expr_indent i (<) expr s
    
            pipe2 expr (many expr_up) (List.fold ap) s

        let tuple expr (s: CharStream<_>) =
            let i = (col s)
            let expr_indent expr (s: CharStream<_>) = expr_indent i (<=) expr s
            sepBy1 (expr_indent expr) (expr_indent comma)
            |>> function [x] -> x | x -> vv x
            <| s

        let type_ expr =
            let type_parse (s: CharStream<_>) = 
                let i = (col s)
                let expr_indent expr (s: CharStream<_>) = expr_indent i (=) expr s
                many1 (expr_indent expr) |>> (List.map type_create >> List.reduce type_union >> type_create) <| s
            attempt (type_' >>. nextCharSatisfiesNot is_identifier_char >>. type_parse) <|> expr

        let mset statements expressions (s: CharStream<_>) = 
            let i = (col s)
            let expr_indent expr (s: CharStream<_>) = expr_indent i (<) expr s
            let op =
                (set_ref >>% fun l r -> op(ArraySet,[l;B;r]) |> preturn)
                <|> (set_array >>% fun l r -> 
                        let rec loop = function
                            | ExprPos p -> loop p.Expression
                            | Op(N(Apply,[a;b])) -> op(ArraySet,[a;b;r]) |> preturn
                            | _ -> fail "Expected two arguments on the left of <-."
                        loop l)

            (tuple2 expressions (opt (expr_indent op .>>. expr_indent statements))
            >>= function 
                | a,Some(f,b) -> f a b
                | a,None -> preturn a) s

        let annotations expr (s: CharStream<_>) = 
            let i = (col s)
            let expr_indent expr (s: CharStream<_>) = expr_indent i (<=) expr s
            pipe2 (expr_indent expr) (opt (expr_indent pp >>. expr_indent expr))
                (fun a -> function
                    | Some b -> op(TypeAnnot,[a;b])
                    | None -> a) s

        let inbuilt_operators =
            let dict_operator = d0()
            let add_infix_operator assoc str prec = dict_operator.Add(str, (prec, assoc))

            let left_assoc_ops = 
                let f = add_infix_operator Associativity.Left
                f "+" 60; f "-" 60; f "*" 70; f "/" 70
                f "<|" 10; f "|>" 10; f "<<" 10; f ">>" 10

            let no_assoc_ops =
                let f str = add_infix_operator Associativity.None str 40
                f "<="; f "<"; f "="; f ">"; f ">="

            let right_associative_ops =
                let f str prec = add_infix_operator Associativity.Right str prec
                f "||" 20; f "&&" 30; f "::" 50
         
            dict_operator

        let negate expr = attempt (negate_ >>. expr |>> (ap (v "negate"))) <|> expr

        let operators expr (s: CharStream<_>) =
            let poperator (s: CharStream<Userstate>) =
                let dict_operator = s.UserState.ops
                let p = pos' s
                (poperator >>=? function
                    | "->" | ":=" | "<-" -> fail "forbidden operator"
                    | orig_op -> 
                        let rec calculate on_fail op = 
                            match dict_operator.TryGetValue op with
                            | true, (prec,asoc) -> preturn (prec,asoc,fun a b -> expr_pos p (ap' (v orig_op) [a; b]))
                            | false, _ -> on_fail ()

                        let on_fail () =
                            let x = orig_op.TrimStart [|'.'|]
                            let rec on_fail i () = 
                                if i >= 0 then calculate (on_fail (i-1)) x.[0..i] 
                                else fail "unknown operator"
                            calculate (on_fail (x.Length-1)) x

                        calculate on_fail orig_op) s

            let rec led poperator term left (prec,asoc,m) =
                match asoc with
                | Associativity.Left | Associativity.None -> tdop poperator term prec |>> m left
                | Associativity.Right -> tdop poperator term (prec-1) |>> m left
                | _ -> failwith "impossible"

            and tdop poperator term rbp =
                let rec f left =
                    poperator >>= fun (prec,asoc,m as v) ->
                        if rbp < prec then led poperator term left v >>= loop
                        else pzero
                and loop left = attempt (f left) <|>% left
                term >>= loop

            let i = (col s)
            let expr_indent expr (s: CharStream<_>) = expr_indent i (<=) expr s
            let op s = expr_indent poperator s
            let term s = expr_indent expr s
            tdop op term 0 s

        let rec expr s = annotations ^<| indentations (statements ^<| change_semicolon_ignore_to_true expr) (mset expr ^<| type_ ^<| tuple ^<| negate ^<| operators ^<| application ^<| expressions expr) <| s
        runParserOnString (spaces >>. expr .>> eof) {ops=inbuilt_operators; ignore_semicolon=true} module_name module_code

    // #Codegen
    let spiral_codegen main =
        let buffer_type_definitions = ResizeArray()
        let buffer_code = ResizeArray()
        let buffer = ResizeArray()
        let exp x = String.concat "" x

        let process_statements (statements: ResizeArray<ProgramNode>) =
            let rec process_statement (code: StringBuilder,ind as state) statement =
                match statement with
                | Statement x -> [|String.replicate ind " "; x; "\n"|] |> exp |> code.Append, ind
                | Indent -> code, ind+4
                | Dedent -> code, ind-4
                | Statements x -> process_statements state x
            and process_statements state (statements: ResizeArray<ProgramNode>) =
                Seq.fold process_statement state statements
            process_statements (StringBuilder(),0) statements
            |> fun (code,ind) -> code.ToString()

        let state x = buffer.Add <| Statement x
        let enter' f = buffer.Add Indent; f(); buffer.Add Dedent
        let enter f = 
            enter' <| fun _ -> 
                match f() with
                | "" -> ()
                | s -> state s

        let rec is_unit_tuple t = List.forall is_unit t
        and is_unit_env env = Map.forall (fun _ -> is_unit) env
        and is_unit = function
            | VVT (N []) | TypeConstructorT _ | LitT _ | ForCastT _ | DotNetAssemblyT _ | DotNetTypeRuntimeT _ -> true
            | UnionT _ | RecT _ | DotNetTypeInstanceT _ | ClosureT _ | PrimT _ -> false
            | ArrayT(N(_,t)) -> is_unit t
            | FunT (N(N env,_)) -> is_unit_env env
            | VVT (N t) -> is_unit_tuple t

        let (|Unit|_|) x = if is_unit x then Some () else None

        let definitions_set = h0()
        let definitions_queue = Queue()

        let print_tag_tuple' t = sprintf "Tuple%i" t
        let print_tag_union' t = sprintf "Union%i" t
        let print_tag_rec' t = sprintf "Rec%i" t
        let print_tag_env_ty' t = sprintf "Env%i" t

        let sym = function
            | VVT (S s) | UnionT (S s) | FunT(S s) | RecT s -> s
            | _ -> failwith "Invalid input to sym."

        let def_enqueue f t =
            if definitions_set.Add t then definitions_queue.Enqueue t
            f (sym t)

        let print_tag_tuple t = def_enqueue print_tag_tuple' t
        let print_tag_union t = def_enqueue print_tag_union' t
        let print_tag_rec t = def_enqueue print_tag_rec' t
        let print_tag_env_ty t = def_enqueue print_tag_env_ty' t

        let rec print_type = function
            | Unit -> "unit"
            | FunT _ as x -> print_tag_env_ty x
            | VVT _ as x -> print_tag_tuple x
            | UnionT _ as x -> print_tag_union x
            | RecT _ as x -> print_tag_rec x
            | ArrayT(N(DotNetReference,t)) -> sprintf "(%s ref)" (print_type t)
            | ArrayT(N(DotNetHeap,t)) -> sprintf "(%s [])" (print_type t)
            | ArrayT _ -> failwith "Not implemented."
            | DotNetTypeInstanceT (N t) -> print_dotnet_instance_type t
            | ClosureT(N(a,b)) -> 
                let a = tuple_field_ty a |> List.map print_type |> String.concat " * "
                sprintf "(%s -> %s)" a (print_type b)
            | PrimT x ->
                match x with
                | Int8T -> "int8"
                | Int16T -> "int16"
                | Int32T -> "int32"
                | Int64T -> "int64"
                | UInt8T -> "uint8"
                | UInt16T -> "uint16"
                | UInt32T -> "uint32"
                | UInt64T -> "uint64"
                | Float32T -> "float32"
                | Float64T -> "float64"
                | BoolT -> "bool"
                | StringT -> "string"
                | CharT -> "char"
            | TypeConstructorT _ | LitT _ | ForCastT _ | DotNetAssemblyT _ | DotNetTypeRuntimeT _ -> 
                failwith "Should be covered in Unit."
                

        and print_dotnet_instance_type (x: System.Type) =
            if x.GenericTypeArguments.Length > 0 then
                [|
                x.Namespace
                "." 
                x.Name.Split '`' |> Array.head
                "<"
                Array.map (dotnet_type_to_ty >> print_type) x.GenericTypeArguments |> String.concat ","
                ">"
                |] |> String.concat null
            else
                [|x.Namespace; "."; x.Name|] |> String.concat null

        let print_tyv (tag,ty) = sprintf "var_%i" tag
        let print_tyv_with_type (tag,ty as v) = sprintf "(%s: %s)" (print_tyv v) (print_type ty)
        let print_method tag = sprintf "method_%i" tag

        let print_args args = 
            Seq.choose (fun (_,ty as x) ->
                if is_unit ty = false then print_tyv_with_type x |> Some
                else None) args
            |> String.concat ", "

        let get_tag =
            let mutable i = 0
            fun () -> i <- i + 1; i

        let print_case_rec x i = print_tag_rec x + sprintf "Case%i" i
        let print_case_union x i = print_tag_union x + sprintf "Case%i" i

        let rec codegen expr =
            let print_value = function
                | LitInt8 x -> sprintf "%iy" x
                | LitInt16 x -> sprintf "%is" x
                | LitInt32 x -> sprintf "%i" x
                | LitInt64 x -> sprintf "%iL" x
                | LitUInt8 x -> sprintf "%iuy" x
                | LitUInt16 x -> sprintf "%ius" x
                | LitUInt32 x -> sprintf "%iu" x
                | LitUInt64 x -> sprintf "%iUL" x
                | LitFloat32 x -> sprintf "%ff" x
                | LitFloat64 x -> sprintf "%f" x
                | LitString x -> sprintf "\"%s\"" x
                | LitChar x -> 
                    match x with
                    | '\n' -> @"\n"
                    | '\t' -> @"\t"
                    | '\r' -> @"\r"
                    | x -> string x
                    |> sprintf "'%s'"
                | LitBool x -> if x then "true" else "false"

            let print_if t f =
                match t with
                | Unit -> f (); ""
                | t ->
                    let if_var = sprintf "if_var_%i" (get_tag())
                    sprintf "let (%s: %s) =" if_var (print_type t) |> state
                    enter' <| fun _ -> f()
                    if_var
        
            let rec if_ cond tr fl =
                let enter f =
                    enter <| fun _ ->
                        let x = buffer.Count
                        match f () with
                        | "" when buffer.Count = x -> "()"
                        | x -> x
                
                print_if (get_type tr) <| fun _ ->
                    sprintf "if %s then" (codegen cond) |> state
                    enter <| fun _ -> codegen tr
                    "else" |> state
                    enter <| fun _ -> codegen fl

            let make_struct l on_empty on_rest =
                Seq.choose (fun x -> let x = codegen x in if x = "" then None else Some x) l
                |> String.concat ", "
                |> function
                    | "" -> on_empty
                    | x -> on_rest x

            let if_not_unit ty f = if is_unit ty then "" else f()

            let (|DotNetPrintedArgs|) x = List.map codegen x |> List.filter ((<>) "") |> String.concat ", "

            let array_create size = function
                | Unit -> ""
                | ArrayT(N(_,t)) -> sprintf "Array.zeroCreate<%s> (System.Convert.ToInt32(%s))" (print_type t) (codegen size)
                | _ -> failwith "impossible"

            let reference_create = function
                | TyType Unit -> ""
                | x -> sprintf "(ref %s)" (codegen x)

            let array_index ar idx =
                match ar with
                | TyType Unit -> ""
                | _ -> sprintf "%s.[int32 %s]" (codegen ar) (codegen idx)

            let array_length ar = sprintf "%s.LongLength" (codegen ar)

            let reference_index = function
                | TyType Unit -> ""
                | x -> sprintf "(!%s)" (codegen x)

            let array_set ar idx r = 
                match ar with
                | TyType Unit -> ()
                | _ -> sprintf "%s <- %s" (array_index ar idx) (codegen r) |> state
            let reference_set l r = 
                match l with
                | TyType Unit -> ()
                | _ -> sprintf "%s := %s" (codegen l) (codegen r) |> state

            let string_length str = sprintf "(int64 %s.Length)" (codegen str)
            let string_index str idx = sprintf "%s.[int32 %s]" (codegen str) (codegen idx)
            let string_slice str a b = sprintf "%s.[int32 %s..int32 %s]" (codegen str) (codegen a) (codegen b)

            match expr with
            | TyV (_, Unit) | TyBox (N(_, Unit)) -> ""
            | TyV v -> print_tyv v
            | TyOp(If,[cond;tr;fl],t) -> if_ cond tr fl
            | TyLet(LetInvisible, _, _, rest, _) -> codegen rest
            | TyLet(_,tyv,b,TyV tyv',_) when tyv = tyv' -> codegen b
            | TyLet(_,(_,Unit),b,rest,_) ->
                match b with
                | TyOp(ArraySet,[ar;idx;b],_) ->
                    match get_type ar with
                    | ArrayT(N(DotNetReference,_)) -> reference_set ar b
                    | ArrayT(N(DotNetHeap,_)) -> array_set ar idx b
                    | _ -> failwith "impossible"
                | _ ->
                    let b = codegen b
                    if b <> "" then sprintf "%s" b |> state
                codegen rest
            | TyLet(_,tyv,b,rest,_) -> sprintf "let %s = %s" (print_tyv_with_type tyv) (codegen b) |> state; codegen rest
            | TyLit x -> print_value x
            | TyJoinPoint((S method_tag,_ as key),_) ->
                let method_name = print_method method_tag
                match join_point_dict.[key] with
                | JoinPointMethod, fv, _ ->
                    sprintf "%s(%s)" method_name (print_args fv)
                | JoinPointClosure args, fv, rev_renamer ->
                    let fv = Seq.filter (fun x -> args.Contains x = false) fv //(Set fv) - (Set args)
                    if Seq.isEmpty fv then method_name
                    else sprintf "%s(%s)" method_name (print_args fv)
            | TyBox(N(x, t)) ->
                let case_name =
                    let union_idx s = Seq.findIndex ((=) (get_type x)) s
                    match t with
                    | UnionT (N s) -> print_case_union t (union_idx s)
                    | RecT tag -> 
                        match rect_dict.[tag] with
                        | VVT _ -> print_tag_rec t
                        | UnionT (N s) -> print_case_rec t (union_idx s)
                        | _ -> failwith "Only VVT and UnionT can be recursive types."
                    | _ -> failwith "Only VVT and UnionT can be boxed types."
                if is_unit (get_type x) then case_name else sprintf "%s(%s)" case_name (codegen x)
            | TyFun(N(N env_term, _)) ->
                let t = get_type expr
                Map.toArray env_term
                |> Array.map snd
                |> fun x -> make_struct x "" (fun args -> sprintf "%s(%s)" (print_tag_env_ty t) args)
            | TyVV (N l) -> let t = get_type expr in make_struct l "" (fun args -> sprintf "%s(%s)" (print_tag_tuple t) args)
            | TyOp(op,args,t) ->
                match op, args with
                | Apply,[a;b] ->
                    // Apply during codegen is only used for applying closures.
                    // There is one level of flattening in the outer arguments.
                    // The reason for this is the symmetry between the F# and the Cuda side.
                    let b = tuple_field b |> List.map codegen |> String.concat ", "
                    sprintf "%s(%s)" (codegen a) b
                | Case,v :: cases ->
                    print_if t <| fun _ ->
                        let print_case =
                            match get_type v with
                            | RecT _ as x -> print_case_rec x
                            | UnionT _ as x -> print_case_union x
                            | _ -> failwith "impossible"

                        sprintf "match %s with" (codegen v) |> state
                        let print_case i case = 
                            let case = codegen case
                            if String.IsNullOrEmpty case then sprintf "| %s ->" (print_case i) |> state
                            else sprintf "| %s(%s) ->" (print_case i) case |> state
                        let rec loop i = function
                            | case :: body :: rest -> 
                                print_case i case
                                enter <| fun _ -> 
                                    let c = buffer.Count
                                    let x = codegen body
                                    if String.IsNullOrEmpty x && buffer.Count = c then "()" else x
                                loop (i+1) rest
                            | [] -> ()
                            | _ -> failwith "The cases should always be in pairs."
                        loop 0 cases

                | ArrayCreate,[a] -> array_create a t
                | ReferenceCreate,[a] -> reference_create a
                | ArrayIndex,[b & TyType(ArrayT(N (DotNetHeap,_)));c] -> array_index b c
                | ArrayIndex,[b & TyType(ArrayT(N (DotNetReference,_)));c] -> reference_index b
                | ArrayLength,[a] -> array_length a
                | StringIndex,[str;idx] -> string_index str idx
                | StringSlice,[str;a;b] -> string_slice str a b
                | StringLength,[str] -> string_length str

                // Primitive operations on expressions.
                | Add,[a;b] -> sprintf "(%s + %s)" (codegen a) (codegen b)
                | Sub,[a;b] -> sprintf "(%s - %s)" (codegen a) (codegen b)
                | Mult,[a;b] -> sprintf "(%s * %s)" (codegen a) (codegen b)
                | Div,[a;b] -> sprintf "(%s / %s)" (codegen a) (codegen b)
                | Mod,[a;b] -> sprintf "(%s %% %s)" (codegen a) (codegen b)
                | LT,[a;b] -> sprintf "(%s < %s)" (codegen a) (codegen b)
                | LTE,[a;b] -> sprintf "(%s <= %s)" (codegen a) (codegen b)
                | EQ,[a;b] -> sprintf "(%s = %s)" (codegen a) (codegen b)
                | NEQ,[a;b] -> sprintf "(%s != %s)" (codegen a) (codegen b)
                | GT,[a;b] -> sprintf "(%s > %s)" (codegen a) (codegen b)
                | GTE,[a;b] -> sprintf "(%s >= %s)" (codegen a) (codegen b)
                | And,[a;b] -> sprintf "(%s && %s)" (codegen a) (codegen b)
                | Or,[a;b] -> sprintf "(%s || %s)" (codegen a) (codegen b)

                | ShiftLeft,[x;y] -> sprintf "(%s << %s)" (codegen x) (codegen y)
                | ShiftRight,[x;y] -> sprintf "(%s >> %s)" (codegen x) (codegen y)

        //        | ShuffleXor,[x;y],_) -> sprintf "cub::ShuffleXor(%s, %s)" (codegen x) (codegen y)
        //        | ShuffleUp,[x;y],_) -> sprintf "cub::ShuffleUp(%s, %s)" (codegen x) (codegen y)
        //        | ShuffleDown,[x;y],_) -> sprintf "cub::ShuffleDown(%s, %s)" (codegen x) (codegen y)
        //        | ShuffleIndex,[x;y],_) -> sprintf "cub::ShuffleIndex(%s, %s)" (codegen x) (codegen y)

                | Neg,[a] -> sprintf "(-%s)" (codegen a)
                | VVIndex,[a;TyLit(LitInt64 b)] -> if_not_unit t <| fun _ -> sprintf "%s.mem_%i" (codegen a) b
                | EnvUnseal,[r; TyLit (LitString k)] -> if_not_unit t <| fun _ -> sprintf "%s.mem_%s" (codegen r) k
                | Log,[x] -> sprintf "log(%s)" (codegen x)
                | Exp,[x] -> sprintf "exp(%s)" (codegen x)
                | Tanh,[x] -> sprintf "tanh(%s)" (codegen x)
                | FailWith,[x] -> sprintf "(failwith %s)" (codegen x)
                | DotNetTypeConstruct,[TyTuple (DotNetPrintedArgs args)] as x ->
                    match t with 
                    | DotNetTypeInstanceT (N instance_type) -> sprintf "%s(%s)" (print_dotnet_instance_type instance_type) args
                    | _ -> failwith "impossible"
                | DotNetTypeCallMethod,[v; TyTuple [TypeString method_name; TyTuple (DotNetPrintedArgs method_args)]] ->
                    match v with
                    | TyType (DotNetTypeRuntimeT (N t)) -> sprintf "%s.%s(%s)" (print_dotnet_instance_type t) method_name method_args
                    | _ -> sprintf "%s.%s(%s)" (codegen v) method_name method_args
                | DotNetTypeGetField,[v; TypeString name] ->
                    match v with
                    | TyType (DotNetTypeRuntimeT (N t)) -> sprintf "%s.%s" (print_dotnet_instance_type t) name
                    | _ -> sprintf "%s.%s" (codegen v) name
                // Cuda kernel constants
        //        | Syncthreads,[],_) -> state "syncthreads();"; ""

                | x -> failwithf "Missing TyOp case. %A" x

        let prefix =
            let mutable prefix = false
            fun () -> if prefix then "and" else prefix <- true; "type"

        let print_union_cases print_case tys =
            enter' <| fun _ ->
                List.iteri (fun i -> function
                    | Unit -> "| " + print_case i |> state
                    | x -> sprintf "| %s of %s" (print_case i) (print_type x) |> state) tys

        let print_struct_definition iter fold name tys =
            let args sep f =
                fold (fun s k ty -> 
                    match ty with
                    | Unit -> s
                    | _ -> f k :: s) [] tys
                |> List.rev
                |> String.concat sep

            let args_declaration = args ", " <| fun k -> sprintf "arg_mem_%s" k
            let args_mapping = args "; " <| fun k -> sprintf "mem_%s = arg_mem_%s" k k

            sprintf "%s %s =" (prefix()) name |> state
            enter' <| fun _ -> 
                "struct" |> state
                iter (fun k ty -> 
                    match ty with
                    | Unit -> ()
                    | _ -> sprintf "val mem_%s: %s" k (print_type ty) |> state) tys
            
                sprintf "new(%s) = {%s}" args_declaration args_mapping |> state
                "end" |> state

        let print_method_definition is_first tag (join_point_type, fv, body) = 
            let prefix = if is_first then "let rec" else "and"
            let method_name = print_method tag
            match join_point_type with
            | JoinPointClosure args -> 
                let printed_fv = 
                    Seq.filter (fun x -> args.Contains x = false) fv
                    |> fun fv -> if Seq.isEmpty fv then "" else sprintf "(%s) " (print_args fv)
                sprintf "%s %s %s(%s): %s =" prefix method_name printed_fv (print_args args) (print_type (get_type body))
            | JoinPointMethod -> sprintf "%s %s(%s): %s =" prefix method_name (print_args fv) (print_type (get_type body))
            |> state

            enter <| fun _ -> 
                let c = buffer.Count
                let x = codegen body
                if String.IsNullOrEmpty x && buffer.Count = c then "()" else x

        memoized_methods |> Seq.fold (fun is_first x -> 
            match x.Value with
            | MemoMethod (join_point_type, fv, body) -> print_method_definition is_first x.Key.Symbol (join_point_type, fv, body); false
            | _ -> is_first) true |> ignore
        codegen main |> state // Can't forget the non-method

        buffer_code.AddRange(buffer)
        buffer.Clear()

        while definitions_queue.Count > 0 do
            match definitions_queue.Dequeue() with
            | FunT(N (N tys, _)) as x ->
                if is_unit_env tys = false then
                    let tuple_name = print_tag_env_ty x
                    print_struct_definition Map.iter Map.fold tuple_name tys
            | RecT tag as x ->
                let tys = rect_dict.[tag]
                sprintf "%s %s =" (prefix ()) (print_tag_rec x) |> state
                match tys with
                | Unit -> "| " + print_tag_rec' tag |> state
                | VVT _ -> sprintf "| %s of %s" (print_tag_rec' tag) (print_type tys) |> state
                | UnionT (N tys) -> print_union_cases (print_case_rec x) (Set.toList tys)
                | x -> failwithf "Only VVT and UnionT are recursive types. Got: %A" x
            | UnionT (N tys) as x ->
                sprintf "%s %s =" (prefix()) (print_tag_union x) |> state
                print_union_cases (print_case_union x) (Set.toList tys)
            | VVT (N tys) as x ->
                if is_unit_tuple tys = false then
                    let tuple_name = print_tag_tuple x
                    let fold f s l = List.fold (fun (i,s) ty -> i+1, f s (string i) ty) (0,s) l |> snd
                    let iter f l = List.iteri (fun i x -> f (string i) x) l
                    print_struct_definition iter fold tuple_name tys
            | _ -> failwith "impossible"

        buffer_type_definitions.AddRange(buffer)
        buffer.Clear()

        buffer.AddRange(buffer_type_definitions)
        buffer.AddRange(buffer_code)
  
        process_statements buffer

    // #Run
    let print_type_error (trace: Trace) message = 
        let code: Dictionary<Module, ModuleCode []> = d0()
        let error = System.Text.StringBuilder(1024)
        error.AppendLine message |> ignore
        List.foldBack (fun ((file & Module(N(file_name,_,_,file_code))), line, col as trace) prev_trace ->
            let b =
                match prev_trace with
                | Some (prev_file, prev_line, col) when prev_file <> file || prev_line <> line -> true
                | None -> true
                | _ -> false
            if b then
                let er_code = 
                    memoize code file (fun () -> file_code.Split [|'\n'|])
                    |> fun x -> x.[int line - 1]

                let er_file = if file_name <> "" then sprintf " in file \"%s\"." file_name else file_name
                error.AppendLine <| sprintf "Error trace on line: %i, column: %i%s" line col er_file |> ignore
                error.AppendLine er_code |> ignore
                let col = int (col - 1L)
                for i=1 to col do error.Append(' ') |> ignore
                error.AppendLine "^" |> ignore
            Some trace
            ) trace None
        |> ignore
        error.ToString()

    let data_empty () =
        {ltag = ref 0; seq=ref id; trace=[]; rbeh=AnnotationDive
         env = Map.empty |> nodify_env_term
         cse_env = ref Map.empty
         }

    let core_functions =
        let p f = inl "x" (f (v "x"))
        let p2 f = inl' ["x"; "y"] (f (v "x") (v "y"))
        let p3 f = inl' ["x"; "y"; "z"] (f (v "x") (v "y") (v "z"))
        let binop' op' a b = op(op',[a;b])
        let binop op = p2 (binop' op)
        let b str op = l str (binop op)
        let apply a b = binop' Apply a b
        let compose a b c = apply a (apply b c)
        let con x = op(x,[])
        s  [l "error_type" (p error_type)
            l "print_static" (p print_static)
            l "dyn" (p dynamize)
            l "union" (p2 type_union)

            l "bool" (op(TypeConstructorCreate,[lit <| LitBool true]))
            l "int64" (op(TypeConstructorCreate,[lit <| LitInt64 0L]))
            l "int32" (op(TypeConstructorCreate,[lit <| LitInt32 0]))
            l "int16" (op(TypeConstructorCreate,[lit <| LitInt16 0s]))
            l "int8" (op(TypeConstructorCreate,[lit <| LitInt8 0y]))
            l "uint64" (op(TypeConstructorCreate,[lit <| LitUInt64 0UL]))
            l "uint32" (op(TypeConstructorCreate,[lit <| LitUInt32 0u]))
            l "uint16" (op(TypeConstructorCreate,[lit <| LitUInt16 0us]))
            l "uint8" (op(TypeConstructorCreate,[lit <| LitUInt8 0uy]))
            l "float64" (op(TypeConstructorCreate,[lit <| LitFloat64 0.0]))
            l "float32" (op(TypeConstructorCreate,[lit <| LitFloat32 0.0f]))
            l "string" (op(TypeConstructorCreate,[lit <| LitString ""]))
            l "char" (op(TypeConstructorCreate,[lit <| LitChar ' ']))
            l "unit" (op(TypeConstructorCreate,[B]))

            l "lit_lift" (p <| fun x -> op(TypeLitCreate,[x]))
            l "for_cast" (p for_cast)
            l "negate" (p <| fun x -> op(Neg,[x]))
        
            l "load_assembly" (p <| fun x -> op(DotNetLoadAssembly,[x]))
            l "mscorlib" (ap (v "load_assembly") (ap (v "lit_lift") (lit_string "mscorlib")))
            l "ignore" (inl "" B)
            l "id" (p <| id)
            l "ref" (p <| fun x -> op(ReferenceCreate,[x]))
            l "array_create" (p2 <| fun size typ -> op(ArrayCreate,[size;typ]))
            l "array_length" (p <| fun ar -> op(ArrayLength,[ar]))

            b "+" Add; b "-" Sub; b "*" Mult; b "/" Div
            b "<|" Apply; l "|>" (p2 (flip apply)); l "<<" (p3 compose); l ">>" (p3 (flip compose))

            b "<=" LTE; b "<" LT; b "=" EQ; b ">" GT; b ">=" GTE
            b "||" Or; b "&&" And; b "::" VVCons

            l "fst" (p <| fun x -> tuple_index x 0L)
            l "snd" (p <| fun x -> tuple_index x 1L)

            l "tuple_length" (p <| fun x -> op(VVLength,[x]))
            l "tuple_index" (p2 tuple_index')
            l "not" (p <| fun x -> eq x (lit <| LitBool false))
            l "string_length" (p <| fun x -> op(StringLength,[x]))
            l "upon" (p3 <| fun a b c -> op(ModuleWith,[a;b;c]))
            l "upon'" (p3 <| fun a b c -> op(ModuleWithExtend,[a;b;c]))
            l "is_static" (p <| fun x -> op(IsStatic,[x]))
            l "failwith" (p <| fun x -> op(FailWith,[x]))
            l "assert" (p2 <| fun c x -> if_static (eq c (lit (LitBool false))) (op(FailWith,[x])) B)
            ]

    let rec parse_modules (Module(N(_,module_auxes,_,_)) as module_main) on_fail ret =
        let h = h0()

        let inline p x ret =
            match spiral_parse x with
            | Success(r,_,_) -> ret r
            | Failure(er,_,_) -> on_fail er

        let rec loop xs ret =
            match xs with
            | (x & Module(N(name,auxes,_,_))) :: xs ->
                if h.Add x then
                    loop auxes <| fun auxes ->
                        p x <| fun x ->
                            loop xs <| fun xs ->
                                ret (auxes << l name x << xs)
                else loop xs ret
            | [] -> ret id

        loop module_auxes (fun r -> p module_main (r >> ret))

    let copy_to_temporary x =
        IO.File.WriteAllText(output_path,x)
        printfn "Copied the code to: %s" output_path
        x

    let watch = System.Diagnostics.Stopwatch.StartNew()
    parse_modules module_main Fail <| fun body -> 
        printfn "Time for parse: %A" watch.Elapsed
        watch.Restart()
        let d = data_empty()
        let input = core_functions body |> expr_prepass |> snd
        printfn "Time for prepass: %A" watch.Elapsed
        watch.Restart()
        try
            let x = !d.seq (expr_peval d input)
            printfn "Time for peval was: %A" watch.Elapsed
            watch.Restart()
            let x = Succ (spiral_codegen x |> copy_to_temporary)
            printfn "Time for codegen was: %A" watch.Elapsed
            x
        with 
        | :? TypeError as e -> 
            let trace, message = e.Data0, e.Data1
            let message = if message.Length > 300 then message.[0..299] + "..." else message
            Fail <| print_type_error trace message
