module Spiral.Main

// Global open
open System
open System.Collections.Generic
open Iesi.Collections.Generic
open HashConsing

// Parser open
open FParsec

// Codegen open
open System.Text

// Language types
type RecordType =
    | RecordIndividual
    | RecordStack
    | RecordHeap

type Node<'a>(expr:'a, symbol:int) = 
    member x.Expression = expr
    member x.Symbol = symbol
    override x.ToString() = sprintf "<tag %i>" symbol
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
let (|C|) (x: ConsedNode<_>) = x.node
let (|S|) (x: Node<_>) = x.Symbol

type ModuleName = string
type ModuleCode = string
type ModuleDescription = string
type Module = Module of Node<ModuleName * Module list * ModuleDescription * ModuleCode>

type PosKey = Module * int64 * int64

let h0() = HashSet(HashIdentity.Structural)
let lh0() = LinkedHashSet() // Linked hash set preserves the insertion order.
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
    // Pattern matching errors
    | ErrorPatMiss
    | ErrorPatClause

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
    | ModuleWithout
    | ModuleIs
    | ModuleValues

    // BoxedVariableIs
    | BoxedVariableIs

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
    | BitwiseAnd
    | BitwiseOr
    | BitwiseXor

    | Fix
    | Apply
    | TermCast
    | UnsafeConvert
    | FuncJoinPoint
    | TypeJoinPoint
    | StructCreate
    | VVIndex
    | VVSliceFrom
    | VVCons
    | VVLength
    | VVIs
    | TypeAnnot
    | RecordIndividualUnseal
    | RecordBoxedUnseal
    | RecordStackify
    | RecordHeapify
    | TypeCreate
    | TypeGet
    | TypeUnion
    | TypeSplit
    | TypeBox
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
    | TypeLitCast
    | TypeLitIs
    | Dynamize
    | LitIs
    | BoxIs

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
    | PatTypeLitBind of string
    | PatLit of Value
    | PatWhen of Pattern * Expr
    | PatModule of string option * PatternModule
    | PatPos of Pos<Pattern>

and PatternModule =
    | PatMAnd of PatternModule list
    | PatMOr of PatternModule list
    | PatMXor of PatternModule list
    | PatMNot of PatternModule
    | PatMInnerModule of string * PatternModule
    | PatMName of string
    | PatMRebind of string * Pattern
    | PatMPattern of Pattern


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
    | VVT of Ty list
    | LitT of Value
    | FunT of EnvTy * FunType
    | FunStackT of ConsedNode<EnvTerm> * FunType
    | FunHeapT of ConsedNode<EnvTerm> * FunType
    | ClosureT of Ty * Ty
    | UnionT of Set<Ty>
    | RecT of int
    | ArrayT of ArrayType * Ty
    | DotNetTypeRuntimeT of Node<Type> 
    | DotNetTypeInstanceT of Node<Type>
    | DotNetAssemblyT of Node<System.Reflection.Assembly>

and TypedExpr =
    // Data structures
    | TyT of Ty
    | TyV of TyTag
    | TyVV of TypedExpr list
    | TyFun of ConsedNode<EnvTerm> * FunType
    | TyBox of TypedExpr * Ty
    | TyLit of Value

    // Operations
    | TyLet of TyTag * TypedExpr * TypedExpr * Ty * Trace
    | TyState of TypedExpr * TypedExpr * Ty * Trace
    | TyOp of Op * TypedExpr list * Ty
    | TyJoinPoint of JoinPointKey * Ty

and JoinPointType =
    | JoinPointClosure of Arguments
    | JoinPointMethod
    | JoinPointType

and JoinPointKey = MemoKey * Tag
and JoinPointValue = JoinPointType * Arguments * Renamer

and MemoCases =
    | MemoMethodInEvaluation
    | MemoMethod of JoinPointType * Arguments * TypedExpr
    | MemoTypeInEvaluation of Ty
    | MemoType of Ty

and Tag = int
and TyTag = Tag * Ty
and EnvTy = Map<string, Ty>
and EnvTerm = Map<string, TypedExpr>
and MemoKey = Node<Expr * EnvTerm>

and Arguments = LinkedHashSet<TyTag>
and Renamer = Dictionary<Tag,Tag>

// This key is for functions without arguments. It is intended that the arguments be passed in through the Environment.
and MemoDict = Dictionary<MemoKey, MemoCases>
// For Common Subexpression Elimination. I need it not for its own sake, but to enable other PE based optimizations.
and CSEDict = Map<TypedExpr,TypedExpr> ref

and Trace = PosKey list

and TraceNode<'a when 'a:equality and 'a:comparison>(expr:'a, trace:Trace) = 
    member x.Expression = expr
    member x.Trace = trace
    override x.ToString() = sprintf "%A" expr
    override x.GetHashCode() = expr.GetHashCode()
    override x.Equals(y) = 
        match y with 
        | :? TraceNode<'a> as y -> expr = y.Expression
        | _ -> failwith "Invalid equality for TraceNode."

    interface IComparable with
        member x.CompareTo(y) = 
            match y with
            | :? TraceNode<'a> as y -> compare expr y.Expression
            | _ -> failwith "Invalid comparison for TraceNode."

let inline t (x: TraceNode<_>) = x.Expression
let (|T|) x = t x

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
    semicolon_line : int64
    }

type ParserExpr =
| ParserStatement of PosKey * (Expr -> Expr)
| ParserExpr of PosKey * Expr

// Codegen types
type BackendType = | Cuda | FSharp
type TypeOrMethod =
    | TomType of Ty
    | TomMethod of MemoKey
type Buf = ResizeArray<ProgramNode>
and ProgramNode =
    | Statement of string
    | Indent
    | Dedent

type CodegenEnv = {
    backend_type: BackendType
    buffer: ResizeArray<ProgramNode>
    trace: Trace
    }

type Renamables =
    {
    memo : Dictionary<TypedExpr,TypedExpr>
    renamer : Dictionary<Tag,Tag>
    renamer_reversed : Dictionary<Tag,Tag>
    fv : LinkedHashSet<Tag * Ty>
    renamed_fv : LinkedHashSet<Tag * Ty>
    }

// #Main
let spiral_peval (Module(N(module_name,_,_,_)) as module_main) = 
    let mutable renaming_time = TimeSpan()
    // #Smart constructors
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
    let rect_dict = d0()
    let (|TyRec|_|) = function
        | RecT x -> Some rect_dict.[x]
        | _ -> None
    let nodify_dotnet_type_runtimet = nodify <| d0()
    let nodify_dotnet_type_instancet = nodify <| d0()
    let nodify_dotnet_assemblyt = nodify <| d0()

    let vvt x = VVT x
    let litt x = LitT x
    let funt (x, core) = FunT (x, core)
    let uniont x = UnionT x
    let closuret x = ClosureT x
    let arrayt x = ArrayT x
    let dotnet_type_runtimet x = nodify_dotnet_type_runtimet x |> DotNetTypeRuntimeT
    let dotnet_type_instancet x = nodify_dotnet_type_instancet x |> DotNetTypeInstanceT
    let dotnet_assemblyt x = nodify_dotnet_assemblyt x |> DotNetAssemblyT

    let nodify_memo_key = nodify <| d0()
    let consify_env_term = hashcons_add <| hashcons_create 0

    let tyv x = TyV x
    let tyvv x = TyVV x
    let tyfun (a,t) = (consify_env_term a,t) |> TyFun
    let tybox x = TyBox x

    let lit_int i = LitInt64 i |> lit
    let lit_string x = LitString x |> lit

    let record_stackify a = op(RecordStackify,[a])
    let record_heapify a = op(RecordHeapify,[a])

    let fix name x =
        match name with
        | "" -> x
        | _ -> (Fix,[lit_string name; x]) |> op
    let inl x y = (x,y) |> func
    let inl_pat x y = (PatClauses([x,y])) |> pattern
    let ap x y = (Apply,[x;y]) |> op
    let term_cast a b = (TermCast,[a;b]) |> op
    let lp v b e = ap (inl_pat v e) b
    let inmp v' b e = ap (ap (v ">>=") b) (inl_pat v' e)
    let l v b e = ap (inl v e) b
    let l_rec v b e = ap (inl v e) (fix v b)

    let inl' args body = List.foldBack inl args body
    
    let meth_memo y = (FuncJoinPoint,[y]) |> op
    let type_memo y = (TypeJoinPoint,[y]) |> op
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
    let type_lit_lift' x = (TypeLitCreate,[x]) |> op
    let type_lit_lift x = type_lit_lift' (lit x)
    let type_lit_cast x = (TypeLitCast,[x]) |> op
    let type_lit_is x = (TypeLitIs,[x]) |> op
    let expr_pos pos x = ExprPos(Pos(pos,x))
    let pat_pos pos x = PatPos(Pos(pos,x))

    let type_create a = op(TypeCreate,[a])
    let type_get a = op(TypeGet,[a])
    let type_union a b = op(TypeUnion,[a;b])
    let type_split a = op(TypeSplit,[a])
    let type_box a b = op(TypeBox,[a;b])

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

    let rec env_to_ty env = Map.map (fun _ -> get_type) env
    and get_type = function
        | TyLit x -> get_type_of_value x
        | TyVV l -> List.map get_type l |> vvt
        | TyFun(C l, t) -> funt (env_to_ty l, t)

        | TyT t | TyV(_,t) | TyBox(_,t)
        | TyLet(_,_,_,t,_) | TyJoinPoint(_,t)
        | TyState(_,_,t,_) | TyOp(_,_,t) -> t

    let rec typed_expr_env_free_var_exists x = Map.exists (fun k v -> typed_expr_free_var_exists v) x
    and typed_expr_free_var_exists e =
        let inline f x = typed_expr_free_var_exists x
        match e with
        | TyBox (n,t) -> f n
        | TyVV l -> List.exists f l
        | TyFun(C l,t) -> typed_expr_env_free_var_exists l
        | TyV (n,t as k) -> true
        | TyT _ | TyLit _ -> false
        | TyJoinPoint _ | TyOp _ | TyState _ | TyLet _ -> failwithf "Only data structures in the TypedExpr can be tested for free variable existence. Got: %A" e

    // #Unit type tests
    let rec is_unit_tuple t = List.forall is_unit t
    and is_unit_env x = Map.forall (fun _ -> is_unit) x
    and is_unit = function
        | LitT _ | DotNetAssemblyT _ | DotNetTypeRuntimeT _ -> true
        | UnionT _ | RecT _ | DotNetTypeInstanceT _ | ClosureT _ | PrimT _ -> false
        | ArrayT (_,t) -> is_unit t
        | FunT (env,_) -> is_unit_env env
        | FunStackT (C x, _) | FunHeapT (C x, _) -> typed_expr_env_free_var_exists x = false
        | VVT t -> is_unit_tuple t

    /// Wraps the argument in a list if not a tuple.
    let tuple_field = function 
        | TyVV args -> args
        | x -> [x]

    let (|TyTuple|) x = tuple_field x

    /// Wraps the argument in a set if not a UnionT.
    let set_field = function
        | UnionT t -> t
        | t -> Set.singleton t

    let (|TySet|) x = set_field x

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

    let rec is_any_int' = function
        | PrimT (UInt8T | UInt16T | UInt32T | UInt64T 
            | Int8T | Int16T | Int32T | Int64T) -> true
        | _ -> false
    let inline is_any_int x = is_any_int' (get_type x)

    let rec is_int64' = function
        | PrimT Int64T -> true
        | _ -> false
    let inline is_int64 a = is_int64' (get_type a)

    // #Prepass
    let get_pattern_tag =
        let mutable i = 0
        fun () -> i <- i+1; i

    let rec pattern_compile arg pat =
        let rec pattern_compile arg pat on_succ on_fail =
            let inline cp arg pat on_succ on_fail = pattern_compile arg pat on_succ on_fail

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
                    if_static (eq (tuple_length arg) (lit_int len)) on_succ on_fail
                    |> fun on_succ -> if_static (tuple_is arg) on_succ on_fail
                    |> case arg

            let pat_cons l = 
                pat_foldbacki
                    (fun (pat,i) (on_succ, tuple_index') ->
                        let arg = tuple_index' arg i
                        cp arg pat on_succ on_fail, tuple_index)
                    (on_succ, tuple_slice_from)
                    l
                |> fun ((on_succ,_),len) -> 
                    if_static (gte (tuple_length arg) (lit_int (len-1L))) on_succ on_fail
                    |> fun on_succ -> if_static (tuple_is arg) on_succ on_fail
                    |> case arg

            let pat_part_active a pat on_fail arg =
                let pat_var = sprintf " pat_var_%i" (get_pattern_tag())
                let on_succ = inl pat_var (cp (v pat_var) pat on_succ on_fail)
                let on_fail = inl "" on_fail
                ap' (v a) [arg; on_fail; on_succ]

            
            let pat_module_is_module on_succ = if_static (module_is arg) on_succ on_fail

            let inline pat_or cp arg l on_succ on_fail = List.foldBack (fun pat on_fail -> cp arg pat on_succ on_fail) l on_fail
            let inline pat_and cp arg l on_succ on_fail = List.foldBack (fun pat on_succ -> cp arg pat on_succ on_fail) l on_succ

            let rec pattern_module_compile arg pat on_succ on_fail =
                let inline pat_bind name f =
                    let memb = type_lit_lift (LitString name)
                    let on_succ = f memb
                    if_static (module_has_member arg memb) on_succ on_fail

                match pat with
                | PatMName name ->
                    pat_bind name <| fun memb -> l name (ap arg memb) on_succ
                | PatMRebind(name,pat) ->
                    let pat_var = sprintf " pat_var_%i" (get_pattern_tag())
                    pat_bind name <| fun memb -> l pat_var (ap arg memb) (cp (v pat_var) pat on_succ on_fail)
                | PatMPattern pat -> cp arg pat on_succ on_fail
                | PatMAnd l -> pat_and pattern_module_compile arg l on_succ on_fail
                | PatMOr l -> pat_or pattern_module_compile arg l on_succ on_fail
                | PatMXor l ->
                    let state_var = sprintf " state_var_%i" (get_pattern_tag())
                    let state_var' = v state_var
                    let bool x = lit <| LitBool x
                    let rec just_one = function
                        | x :: xs -> 
                            let xs = just_one xs
                            inl state_var (pattern_module_compile arg x (if_static state_var' on_fail (ap xs (bool true))) (ap xs state_var'))
                        | [] -> inl state_var on_succ
                    ap (just_one l) (bool false)
                | PatMNot x -> pattern_module_compile arg x on_fail on_succ
                | PatMInnerModule(name,pat) ->
                    let pat_var = sprintf " pat_var_%i" (get_pattern_tag())
                    let pat_var' = v pat_var
                    let memb = type_lit_lift (LitString name)
                
                    pattern_module_compile pat_var' pat on_succ on_fail
                    |> l name pat_var'
                    |> l pat_var (ap arg memb)
                    |> fun on_succ -> if_static (module_has_member arg memb) on_succ on_fail
                    |> pat_module_is_module

            match pat with
            | E -> on_succ
            | PatVar x -> l x arg on_succ
            | PatType (exp,typ) ->
                let on_succ = cp arg exp on_succ on_fail
                if_static (eq_type arg typ) on_succ on_fail
                |> case arg
            | PatTuple l -> pat_tuple l
            | PatCons l -> pat_cons l
            | PatActive (a,b) ->
                let pat_var = sprintf " pat_var_%i" (get_pattern_tag())
                l pat_var (ap (v a) arg) (cp (v pat_var) b on_succ on_fail)
            | PatPartActive (a,pat) -> pat_part_active a pat on_fail arg
            | PatExtActive (a,pat) ->
                let rec f pat' on_fail = function
                    | PatAnd _ as pat -> op(ErrorType,[lit_string "And patterns are not allowed in extension patterns."]) |> pat_part_active a (pat' pat) on_fail
                    | PatOr l -> List.foldBack (fun pat on_fail -> f pat' on_fail pat) l on_fail
                    | PatCons l -> vv [type_lit_lift (LitString "cons"); vv [l.Length-1 |> int64 |> LitInt64 |> lit; arg]] |> pat_part_active a (pat' <| PatTuple l) on_fail
                    | PatTuple l as pat -> vv [type_lit_lift (LitString "tup"); vv [l.Length |> int64 |> LitInt64 |> lit; arg]] |> pat_part_active a (pat' pat) on_fail
                    | PatType (a,typ) -> f (fun a -> PatType(a,typ) |> pat') on_fail a
                    | PatWhen (pat, e) -> f (fun pat -> PatWhen(pat,e) |> pat') on_fail pat
                    | PatClauses _ -> failwith "Clauses should not appear inside other clauses."
                    | pat -> vv [type_lit_lift (LitString "var"); arg] |> pat_part_active a (pat' pat) on_fail
                f id on_fail pat
            | PatOr l -> pat_or cp arg l on_succ on_fail
            | PatAnd l -> pat_and cp arg l on_succ on_fail
            | PatClauses l -> List.foldBack (fun (pat, exp) on_fail -> cp arg pat (expr_prepass exp |> snd) on_fail) l on_fail
            | PatTypeLit x -> 
                if_static (eq_type arg (type_lit_lift x)) on_succ on_fail 
                |> case arg
            | PatTypeLitBind x -> 
                if_static (type_lit_is arg) (l x (type_lit_cast arg) on_succ) on_fail 
                |> case arg
            | PatLit x -> 
                let x = lit x
                let on_succ = if_static (eq arg x) on_succ on_fail
                if_static (eq_type arg x) on_succ on_fail |> case arg
            | PatWhen (p, e) -> cp arg p (if_static e on_succ on_fail) on_fail
            | PatModule(name,pat) ->
                let pat_var = sprintf " pat_var_%i" (get_pattern_tag())
                let pat_var' = v pat_var
                pattern_module_compile pat_var' pat on_succ on_fail
                |> fun x -> 
                    match name with
                    | Some name -> l name pat_var' x
                    | None -> x
                |> l pat_var arg
                |> pat_module_is_module
            | PatPos p -> expr_pos p.Pos (cp arg p.Expression on_succ on_fail)

        let pattern_compile_def_on_succ = op(ErrorPatClause,[])
        let pattern_compile_def_on_fail = op(ErrorPatMiss,[arg])
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
        s |> Seq.iter (fun (tag,ty) -> 
            s'.Add(r.[tag],ty) |> ignore)
        s' 

    let inline renamables0() = {memo=d0(); renamer=d0(); renamer_reversed=d0(); fv=lh0(); renamed_fv=lh0()}
    let rec renamer_apply_env r = Map.map (fun k v -> renamer_apply_typedexpr r v)
    and renamer_apply_typedexpr ({memo=memo; renamer=renamer; renamer_reversed=renamer_reversed; fv=fv; renamed_fv=renamed_fv} as r) e =
        let inline f e = renamer_apply_typedexpr r e
        let inline rename (n,t as k) =
            match renamer.TryGetValue n with
            | true, v -> v,t
            | false, _ ->
                let n' = renamer.Count
                renamer.Add(n,n')
                renamer_reversed.Add(n',n)
                fv.Add k |> ignore
                let k' = n', t
                renamed_fv.Add (n',t) |> ignore
                k'

        match memo.TryGetValue e with
        | true, e -> e
        | false, _ ->
            match e with
            | TyT _ -> e
            | TyBox (n,t) -> tybox(f n,t)
            | TyVV l -> tyvv(List.map f l)
            | TyFun(C l,t) -> tyfun(renamer_apply_env r l, t)
            | TyV (n,t as k) ->
                let n', _ as k' = rename k
                if n' = n then e else tyv k'
            | TyLit _ -> e
            | TyJoinPoint _ | TyOp _ | TyState _ | TyLet _ -> failwithf "Only data structures in the env can be renamed. Got: %A" e
            |> fun x -> memo.[e] <- x; x

    // #Recordify
    let rec record_map_env g = Map.map (fun _ -> record_map_typed_expr g)
    and record_map_typed_expr g e =
        let inline f e = record_map_typed_expr g e
        match e with
        | TyT _ -> e
        | TyBox (n,t) -> tybox(f n,t)
        | TyVV l -> tyvv(List.map f l)
        | TyFun(C l,t) as x -> g x
        | TyV _ | TyLit _ -> e
        | TyJoinPoint _ | TyOp _ | TyState _ | TyLet _ -> failwithf "Only data structures in the env can be mapped over. Got: %A" e

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
        | ArrayT(DotNetHeap,t) -> (dotnet_ty_to_type t).MakeArrayType()
        | ArrayT(DotNetReference,t) -> (dotnet_ty_to_type t).MakeByRefType() // Incorrect, but useful
        | DotNetTypeInstanceT (N x) | DotNetTypeRuntimeT (N x) -> x
        | _ -> failwithf "Type %A not supported for conversion into .NET SystemType." x

    let on_type_er trace message = TypeError(trace,message) |> raise

    // #Type directed partial evaluation
    let rec expr_peval (d: LangEnv) (expr: Expr) =
        let inline tev d expr = expr_peval d expr
        let inline apply_seq d x = !d.seq x
        let inline tev_seq d expr = let d = {d with seq=ref id; cse_env=ref !d.cse_env} in tev d expr |> apply_seq d
        let inline tev_assume cse_env d expr = let d = {d with seq=ref id; cse_env=ref cse_env} in tev d expr |> apply_seq d
        let inline tev_method d expr = let d = {d with seq=ref id; cse_env=ref Map.empty} in tev d expr |> apply_seq d
        let inline tev_rec d expr = let d = {d with seq=ref id; cse_env=ref Map.empty; rbeh=AnnotationReturn} in tev d expr |> apply_seq d

        let inline trace (d: LangEnv) = d.trace

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

        let inline tyt x = TyT x
        let make_up_vars_for_ty d ty = 
            if is_unit ty then tyt ty
            else TyV <| make_tyv_ty d ty

        let inline make_tyv_and_push_typed_expr_template even_if_unit d ty_exp =
            let ty = get_type ty_exp
            if is_unit ty then
                if even_if_unit then 
                    let seq = !d.seq
                    let trace = (trace d)
                    d.seq := fun rest -> TyState(ty_exp,rest,get_type rest,trace) |> seq
                tyt ty
            else
                let v = make_tyv_ty d ty
                let seq = !d.seq
                let trace = (trace d)
                d.seq := fun rest -> TyLet(v,ty_exp,rest,get_type rest,trace) |> seq
                tyv v
            
        let make_tyv_and_push_typed_expr_even_if_unit d ty_exp = make_tyv_and_push_typed_expr_template true d ty_exp
        let make_tyv_and_push_typed_expr d ty_exp = make_tyv_and_push_typed_expr_template false d ty_exp

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

            let inline destructure_var r un_template =
                let index_tuple_args tuple_types = 
                    let unpack i typ = un_template typ <| fun () -> destructure <| TyOp(VVIndex,[r;TyLit <| LitInt64 (int64 i)],typ)
                    List.mapi unpack tuple_types
                let env_unseal x =
                    let unseal k typ = un_template typ <| fun () -> destructure <| TyOp(RecordIndividualUnseal,[r; TyLit (LitString k)], typ)
                    Map.map unseal x
                match get_type r with
                | VVT tuple_types -> tyvv(index_tuple_args tuple_types)
                | FunT (env,t) -> tyfun(env_unseal env, t)
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
            | TyBox _ -> chase_recurse r
            | TyT _ -> 
                destructure_var r <| fun typ f ->
                    match r with
                    | TyT _ -> destructure <| tyt typ
                    | _ -> f()
            | TyV _ -> destructure_var r <| fun _ f -> f()
            | TyOp _ -> destructure_cse r
            | TyJoinPoint _ | TyLet _ | TyState _ -> failwithf "This should never appear in destructure. It should go directly into d.seq. Got: %A" r


        let inline if_is_returnable ty_x f =
            if is_returnable' ty_x then f()
            else on_type_er (trace d) <| sprintf "The following is not a type that can be returned from a if statement. Got: %A" ty_x

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
                    match tr, fl with
                    | TyLit (LitBool true), TyLit (LitBool false) -> cond
                    | _ when tr = fl -> tr
                    | _ ->
                        match cond with
                        | TyLit(LitBool true) -> tr
                        | TyLit(LitBool false) -> fl
                        | _ -> TyOp(If,[cond;tr;fl],type_tr) |> make_tyv_and_push_typed_expr_even_if_unit d
            else on_type_er (trace d) <| sprintf "Types in branches of If do not match.\nGot: %A and %A" type_tr type_fl

        let if_cond d tr fl cond =
            if is_bool cond = false then on_type_er (trace d) <| sprintf "Expected a bool in conditional.\nGot: %A" (get_type cond)
            else if_body d cond tr fl

        let if_static d cond tr fl =
            match tev d cond with
            | TyLit (LitBool cond) -> 
                let branch = if cond then tr else fl
                tev d branch
            | cond -> if_cond d tr fl cond

        let if_ d cond tr fl = tev d cond |> if_cond d tr fl

        let inline recordify is_stack d = function
            | TyFun(C env,t) as a ->
                let {fv = fv} as r = renamables0()
                let env' = renamer_apply_env r env |> consify_env_term
                if fv.Count > 1 then
                    if is_stack then TyOp(RecordStackify,[a],FunStackT(env',t))
                    else TyOp(RecordHeapify,[a],FunHeapT(env',t))
                elif fv.Count = 1 then a
                else
                    if is_stack then tyt (FunStackT(env',t))
                    else tyt (FunHeapT(env',t))
                |> destructure d
            | TyType (FunStackT _ | FunHeapT _) as a -> a
            | x -> on_type_er (trace d) <| sprintf "Cannot turn the seleted type into a record. Got: %A" x

        let inline recordify_env is_stack d = record_map_env (recordify is_stack d)

        let record_stack d a = recordify true d (tev d a)
        let record_heap d a = recordify false d (tev d a)

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

        let inline eval_renaming memo_type d expr =
            let env = d.env
            let stopwatch = Diagnostics.Stopwatch.StartNew()
            let {renamer=renamer; renamer_reversed=renamer_reversed; fv=fv; renamed_fv=renamed_fv} as k = renamables0()
            let renamed_env = renamer_apply_env k env
            renaming_time <- renaming_time + stopwatch.Elapsed

            let memo_type = memo_type renamer
            let typed_expr, memo_key = eval_method memo_type renamed_fv {d with env=renamed_env; ltag=ref renamer.Count} expr
            let typed_expr_ty = get_type typed_expr
            if is_returnable' typed_expr_ty = false then on_type_er (trace d) <| sprintf "The following is not a type that can be returned from a method. Consider using Inlineable instead. Got: %A" typed_expr
            else memo_key, fv, renamer_reversed, typed_expr_ty

        let inline memoize_helper memo_type k d x = eval_renaming memo_type d x |> k |> make_tyv_and_push_typed_expr_even_if_unit d
        let memoize_method d x = 
            let memo_type = JoinPointMethod
            memoize_helper (fun _ -> memo_type) (fun (memo_key,args,rev_renamer,ret_ty) -> 
                ty_join_point memo_key (memo_type,args,rev_renamer) ret_ty) d x

        let memoize_type d x = 
            let memo_type _ = JoinPointType
            let env = d.env |> Map.map (fun _ -> get_type >> tyt)
            let _,_,_,ret_ty = eval_renaming memo_type {d with env = env} x 
            tyt ret_ty

        let type_get d a = tev_seq d a |> get_type |> TyT
                
        let memoize_closure arg d x =
            let {fv=fv} as r = renamables0()
            let arg_ty = renamer_apply_typedexpr r arg |> get_type
            let memo_type renamer = JoinPointClosure <| renamer_apply_pool renamer fv
            memoize_helper memo_type (fun (memo_key,args,rev_renamer,ret_ty) -> 
                ty_join_point memo_key (JoinPointClosure fv,args,rev_renamer) (closuret(arg_ty,ret_ty))) d x

        let rec case_type d args_ty =
            let union_case = function
                | UnionT l -> Set.toList l
                | _ -> [args_ty]
            match args_ty with
            | TyRec t -> union_case t
            | x -> union_case x

        let type_split d x =
            tev d x |> get_type |> case_type d
            |> List.map tyt
            |> tyvv

        let case_ d v case =
            let inline assume d v x branch = tev_assume (cse_add' d v x) d branch
            match tev d v with
            | a & TyBox(b,_) -> tev {d with cse_env = ref (cse_add' d a b)} case
            | TyType(t & (UnionT _ | RecT _)) as v ->
                let rec map_cases l =
                    match l with
                    | x :: xs -> (x, assume d v x case) :: map_cases xs
                    | _ -> []
                            
                match map_cases (case_type d t |> List.map (make_up_vars_for_ty d)) with
                | (_, TyType p) :: _ as cases -> 
                    if List.forall (fun (_, TyType x) -> x = p) cases then 
                        TyOp(Case,v :: List.collect (fun (a,b) -> [a;b]) cases, p) 
                        |> make_tyv_and_push_typed_expr_even_if_unit d
                    else 
                        let l = List.map (snd >> get_type) cases
                        on_type_er (trace d) <| sprintf "All the cases in pattern matching clause with dynamic data must have the same type.\n%A" l
                | _ -> failwith "There should always be at least one clause here."
            | _ -> tev d case
           
        let type_union d a b =
            let a, b = tev2 d a b
            let f x = set_field (get_type x)
            f a + f b |> uniont |> tyt

        let type_create d x = 
            let key = nodify_memo_key (x, d.env)
            let inline ret x = tyt x

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
                tev_seq d x |> get_type |> if_recursive_type_add_to_type_dict |> add_to_memo_dict |> ret

        let inline wrap_exception d f =
            try f()
            with 
            | :? TypeError as e -> reraise()
            | e -> on_type_er (trace d) ("This is a .NET exception.\n"+e.Message)

        let dotnet_load_assembly d x =
            match tev d x with
            | TypeString x ->
                wrap_exception d <| fun _ ->
                    System.Reflection.Assembly.Load(x) |> dotnet_assemblyt |> tyt
            | _ -> on_type_er (trace d) "Expected a type level string."

        let (|TyDotNetType|_|) = function
            | TyType (DotNetTypeRuntimeT (N x) | DotNetTypeInstanceT (N x)) -> Some x
            | _ -> None

        let (|TySystemTypeArgs|) args = List.toArray args |> Array.map (get_type >> dotnet_ty_to_type)

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
            | _ -> on_type_er (trace d) "Expected a string."

        let rec record_boxed_unseal d recf x =
            let inline f x = record_boxed_unseal d recf x
            match x with
            | TyV _ as v -> TyOp(RecordBoxedUnseal,[recf;v],get_type v) |> destructure d
            | TyVV l -> tyvv (List.map f l)
            | TyBox(a,b) -> tybox (f a, b)
            | TyFun(C env, b) -> tyfun (Map.map (fun _ -> f) env, b)
            | x -> x
               
        let record_env_term_unseal d recf env = Map.map (fun _ -> record_boxed_unseal d recf) env

        let (|Func|_|) = function
            | TyFun(C env,t) -> Some (RecordIndividual,env,t)
            | TyType(FunStackT(C env,t)) -> Some (RecordStack,env,t)
            | TyType(FunHeapT(C env,t)) -> Some (RecordHeap,env,t)
            | _ -> None

        let inline apply_func is_term_cast d recf r env_term fun_type args =
            let unpack () =
                match r with
                | RecordIndividual -> env_term
                | _ -> record_env_term_unseal d recf env_term

            let inline tev x =
                if is_term_cast then memoize_closure args x
                else tev x

            match fun_type with
            | FunTypeRecFunction ((pat,body),name) ->
                let env_term = unpack()
                let env = if pat <> "" then Map.add pat args env_term else env_term
                tev {d with env = Map.add name recf env} body
            | FunTypeFunction (pat,body) -> 
                let env_term = unpack()
                tev {d with env = if pat <> "" then Map.add pat args env_term else env_term} body
            // apply_module
            | FunTypeModule when is_term_cast -> on_type_er (trace d) <| sprintf "Expected a function in term casting application. Got: %A" fun_type
            | FunTypeModule ->
                match args with
                | TypeString n ->
                    let unpack () = v_find env_term n (fun () -> on_type_er (trace d) <| sprintf "Cannot find a member named %s inside the module." n)
                    match r with
                    | RecordIndividual -> unpack()
                    | RecordStack | RecordHeap -> unpack() |> record_boxed_unseal d recf
                | x -> on_type_er (trace d) "Expected a type level string in module application." 

        let term_cast d a b =
            match tev d a, tev d b with
            | recf & Func(r,env_term,fun_type), args -> 
                let instantiate_type_as_variable d args_ty =
                    let f x = make_up_vars_for_ty d x
                    match args_ty with
                    | VVT l -> tyvv(List.map f l)
                    | x -> f x
            
                let args = instantiate_type_as_variable d (get_type args)
                apply_func true d recf r env_term fun_type args
            | x -> on_type_er (trace d) <| sprintf "Expected a function in term casting application. Got: %A" x

        let rec apply d a b =
            match destructure d a, destructure d b with
            // apply_function
            | recf & Func(r,env_term,fun_type), args -> apply_func false d recf r env_term fun_type args
            // apply_string_static
            | TyLit (LitString str), TyVV [TyLitIndex a; TyLitIndex b] -> 
                let f x = x >= 0 && x < str.Length
                if f a && f b then TyLit(LitString str.[a..b])
                else on_type_er (trace d) "The slice into a string literal is out of bounds."
            | TyLit (LitString str), TyLitIndex x -> 
                if x >= 0 && x < str.Length then TyLit(LitChar str.[x])
                else on_type_er (trace d) "The index into a string literal is out of bounds."
            // apply_array
            | ar & TyType(ArrayT(array_ty,elem_ty)), idx ->
                match array_ty, idx with
                | _, TypeString x -> 
                    if x = "elem_type" then elem_ty |> tyt
                    else failwithf "Unknown type string applied to array. Got: %s" x
                | DotNetHeap, idx when is_int idx -> TyOp(ArrayIndex,[ar;idx],elem_ty) |> make_tyv_and_push_typed_expr d
                | DotNetHeap, idx -> on_type_er (trace d) <| sprintf "The index into an array is not an int. Got: %A" idx
                | DotNetReference, TyVV [] -> TyOp(ArrayIndex,[ar;idx],elem_ty) |> make_tyv_and_push_typed_expr d
                | DotNetReference, _ -> on_type_er (trace d) <| sprintf "The index into a reference is not a unit. Got: %A" idx
                | _ -> failwith "Not implemented."
            // apply_dotnet_type
            | TyType (DotNetAssemblyT (N a)), TypeString name -> 
                    wrap_exception d <| fun _ ->
                        match a.GetType(name) with
                        | null -> on_type_er (trace d) "A type cannot be found inside the assembly."
                        | x -> 
                            if x.IsPublic then dotnet_type_runtimet x |> tyt
                            else on_type_er (trace d) "Cannot load a private type from an assembly."
            | TyType (DotNetAssemblyT _), _ -> on_type_er (trace d) "Expected a type level string as the second argument."
            | dotnet_type & TyType (DotNetTypeRuntimeT (N t) | DotNetTypeInstanceT (N t)), method_name & TypeString name ->
                match t.GetField name with
                | null ->
                    let lam = inl' ["instance";"method_name";"args"] (ap (v "instance") (vv [v "method_name"; v "args"]))
                              |> inner_compile
                    apply d (apply d lam dotnet_type) method_name
                | field ->
                    if field.IsPublic then
                        TyOp(DotNetTypeGetField,[dotnet_type;method_name],field.FieldType |> dotnet_type_to_ty)
                        |> make_tyv_and_push_typed_expr_even_if_unit d
                    else
                        on_type_er (trace d) "Cannot get a private field."            
            | dotnet_type & TyDotNetType typ, args & TyTuple [TypeString method_name; TyTuple(TySystemTypeArgs method_args)] ->
                wrap_exception d <| fun _ ->
                    match typ.GetMethod(method_name, method_args) with
                    | null -> on_type_er (trace d) "Cannot find a method with matching arguments."
                    | meth -> 
                        if meth.IsPublic then
                            TyOp(DotNetTypeCallMethod,[dotnet_type;args],meth.ReturnType |> dotnet_type_to_ty)
                            |> make_tyv_and_push_typed_expr_even_if_unit d
                        else
                            on_type_er (trace d) "Cannot call a private method."
            | TyType (DotNetTypeRuntimeT (N runtime_type)), args & TyTuple (TySystemTypeArgs system_type_args) ->
                wrap_exception d <| fun _ ->
                    if runtime_type.ContainsGenericParameters then // instantiate generic type params
                        runtime_type.MakeGenericType system_type_args 
                        |> dotnet_type_runtimet |> tyt
                    else // construct the type
                        match runtime_type.GetConstructor system_type_args with
                        | null -> on_type_er (trace d) "Cannot find a constructor with matching arguments."
                        | con ->
                            if con.IsPublic then
                                let instance_type = dotnet_type_instancet runtime_type
                                TyOp(DotNetTypeConstruct,[args],instance_type) |> make_tyv_and_push_typed_expr_even_if_unit d
                            else
                                on_type_er (trace d) "Cannot call a private constructor."    
            | TyType(DotNetTypeInstanceT _), _ -> on_type_er (trace d) "Expected a type level string as the first argument for a method call."
            // apply_string
            | TyType(PrimT StringT) & str, TyVV [a;b] -> 
                if is_int a && is_int b then TyOp(StringSlice,[str;a;b],PrimT StringT) |> destructure d
                else on_type_er (trace d) "Expected an int as the second argument to string index."
            | TyType(PrimT StringT) & str, idx -> 
                if is_int idx then TyOp(StringIndex,[str;idx],PrimT CharT) |> destructure d
                else on_type_er (trace d) "Expected an int as the second argument to string index."
            // apply_closure
            | closure & TyType(ClosureT (clo_arg_ty,clo_ret_ty)), args -> 
                let arg_ty = get_type args
                if arg_ty <> clo_arg_ty then on_type_er (trace d) <| sprintf "Cannot apply an argument of type %A to closure (%A -> %A)." arg_ty clo_arg_ty clo_ret_ty
                else TyOp(Apply,[closure;args],clo_ret_ty) |> make_tyv_and_push_typed_expr_even_if_unit d
            | a,b -> on_type_er (trace d) <| sprintf "Invalid use of apply. %A and %A" a b

        let type_box d typec args =
            let typec & TyType ty, args = tev2 d typec args
            let substitute_ty = function
                | TyBox(x,_) -> tybox(x,ty)
                | x -> tybox(x,ty)

            let (|TyRecUnion|_|) = function
                | UnionT ty' -> Some ty'
                | TyRec t -> Some (set_field t)
                | _ -> None

            match ty, args with
            | x, TyType r when x = r -> args
            | TyRecUnion ty', TyType (UnionT ty_args) when Set.isSubset ty_args ty' ->
                let lam = inl' ["typec"; "args"] (op(Case,[v "args"; type_box (v "typec") (v "args")])) |> inner_compile
                apply d (apply d lam typec) args
            | TyRecUnion ty', TyType x when Set.contains x ty' -> substitute_ty args
            | _ -> on_type_er (trace d) <| sprintf "Type constructor application failed. %A does not intersect %A." ty (get_type args)


        let apply_tev d expr args = apply d (tev d expr) (tev d args)

        let inline vv_index_template f d v i =
            let v,i = tev2 d v i
            match v, i with
            | TyVV l, TyLitIndex i ->
                if i >= 0 || i < List.length l then f l i
                else on_type_er (trace d) "Tuple index not within bounds."
            | v & TyType (VVT ts), TyLitIndex i -> failwith "The tuple should always be destructured."
            | v, TyLitIndex i -> on_type_er (trace d) <| sprintf "Type of an evaluated expression in tuple index is not a tuple.\nGot: %A" v
            | v, i -> on_type_er (trace d) <| sprintf "Index into a tuple must be an at least a i32 less than the size of the tuple.\nGot: %A" i

        let vv_index d v i = vv_index_template (fun l i -> l.[i]) d v i |> destructure d
        let vv_slice_from d v i = vv_index_template (fun l i -> tyvv l.[i..]) d v i

        let inline vv_unop_template on_succ on_fail d v =
            match tev d v with
            | TyVV l -> on_succ l
            | v & TyType (VVT ts) -> failwith "The tuple should always be destructured."
            | v -> on_fail()

        let vv_length x = 
            vv_unop_template (fun l -> l.Length |> int64 |> LitInt64 |> TyLit) 
                (fun _ -> on_type_er (trace d) <| sprintf "Type of an evaluated expression in tuple index is not a tuple.\nGot: %A" v) x
                
        let vv_is x = vv_unop_template (fun _ -> TyLit (LitBool true)) (fun _ -> TyLit (LitBool false)) x

        let eq_type d a b =
            let a, b = tev2 d a b 
            LitBool (get_type a = get_type b) |> TyLit
    
        let vv_cons d a b =
            let a, b = tev2 d a b
            match b with
            | TyVV b -> tyvv(a::b)
            | _ -> on_type_er (trace d) "Expected a tuple on the right in VVCons."

        let type_lit_create' d x = litt x |> tyt

        let module_open d a b =
            let a = tev d a
            match a with
            | Func(r,env_term, FunTypeModule) as recf -> 
                let inline opt f env =
                    let env = Map.fold (fun s k v -> Map.add k (f v) s) d.env env
                    tev {d with env = env} b
                match r with
                | RecordIndividual -> opt id env_term
                | RecordStack | RecordHeap -> opt (record_boxed_unseal d recf) env_term
            | x -> on_type_er (trace d) <| sprintf "The open expected a module type as input. Got: %A" x

        let type_annot d a b =
            match d.rbeh with
            | AnnotationReturn -> tev_seq {d with rbeh=AnnotationDive} b
            | AnnotationDive ->
                let a, b = tev d a, tev_seq {d with rbeh=AnnotationDive} b
                let ta, tb = get_type a, get_type b
                if ta = tb then a else on_type_er (trace d) <| sprintf "Type annotation mismatch.\n%A <> %A" ta tb

        let inline prim_bin_op_template d check_error is_check k a b t =
            let a, b = tev2 d a b
            if is_check a b then k t a b
            else on_type_er (trace d) (check_error a b)

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
                let op_arith_num_zero a b =
                    match t with
                    | Add | Sub -> a
                    | Mult -> b
                    | Div -> on_type_er (trace d) "Division by zero caught at compile time."
                    | Mod -> on_type_er (trace d) "Modulus by zero caught at compile time."
                    | _ -> failwith "Expected an arithmetic operation."
                let op_arith_zero_num a b =
                    match t with
                    | Add -> b
                    | Sub -> TyOp(Neg,[b],get_type b) |> destructure d
                    | Mult | Div | Mod -> a
                    | _ -> failwith "Expected an arithmetic operation."
                let op_arith_num_one a b =
                    match t with
                    | Mult | Div | Mod -> a
                    | Add | Sub -> prim_bin_op_helper t a b
                    | _ -> failwith "Expected an arithmetic operation."
                let op_arith_one_num a b =
                    match t with
                    | Mult -> b
                    | Div | Mod | Add | Sub -> prim_bin_op_helper t a b
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

                | TyLit a', _ ->
                    match a' with
                    | LitInt8 0y | LitInt16 0s | LitInt32 0 | LitInt64 0L
                    | LitUInt8 0uy | LitUInt16 0us | LitUInt32 0u | LitUInt64 0UL
                    | LitFloat32 0.0f | LitFloat64 0.0 -> op_arith_zero_num a b
                    | LitInt8 1y | LitInt16 1s | LitInt32 1 | LitInt64 1L
                    | LitUInt8 1uy | LitUInt16 1us | LitUInt32 1u | LitUInt64 1UL
                    | LitFloat32 1.0f | LitFloat64 1.0 -> op_arith_one_num a b
                    | _ -> prim_bin_op_helper t a b

                | _, TyLit b' ->
                    match b' with
                    | LitInt8 0y | LitInt16 0s | LitInt32 0 | LitInt64 0L
                    | LitUInt8 0uy | LitUInt16 0us | LitUInt32 0u | LitUInt64 0UL
                    | LitFloat32 0.0f | LitFloat64 0.0 -> op_arith_num_zero a b
                    | LitInt8 1y | LitInt16 1s | LitInt32 1 | LitInt64 1L
                    | LitUInt8 1uy | LitUInt16 1us | LitUInt32 1u | LitUInt64 1UL
                    | LitFloat32 1.0f | LitFloat64 1.0 -> op_arith_num_one a b
                    | _ -> prim_bin_op_helper t a b
                | _ -> prim_bin_op_helper t a b
                ) a b t

        let prim_comp_op d a b t = 
            let er a b = sprintf "(is_char a || is_string a || is_numeric a || is_bool a) && get_type a = get_type b` is false.\na=%A, b=%A" a b
            let check a b = (is_char a || is_string a || is_numeric a || is_bool a) && get_type a = get_type b
            prim_bin_op_template d er check (fun t a b ->
                let inline eq_op a b = LitBool (a = b) |> TyLit
                let inline neq_op a b = LitBool (a <> b) |> TyLit
                match t, a, b with
                | EQ, TyV(a,_), TyV(b,_) when a = b -> LitBool true |> TyLit
                | EQ, TyLit (LitBool a), TyLit (LitBool b) -> eq_op a b
                | EQ, TyLit (LitString a), TyLit (LitString b) -> eq_op a b
                | NEQ, TyV(a,_), TyV(b,_) when a = b -> LitBool false |> TyLit
                | NEQ, TyLit (LitBool a), TyLit (LitBool b) -> neq_op a b
                | NEQ, TyLit (LitString a), TyLit (LitString b) -> neq_op a b
                | _ ->
                    let inline op a b =
                        match t with
                        | LT -> a < b
                        | LTE -> a <= b
                        | EQ -> a = b
                        | NEQ -> a <> b
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
            match t with
            | And -> if_ d a b (lit (LitBool false))
            | Or -> if_ d a (lit (LitBool true)) b
            | _ -> failwith "impossible"

        let prim_shift_op d a b t =
            let er a b = sprintf "`is_int a && is_int b` is false.\na=%A, b=%A" a b
            let check a b = is_int a && is_int b
            prim_bin_op_template d er check prim_bin_op_helper a b t

        let prim_bitwise_op d a b t =
            let er a b = sprintf "`is_any_int a && is_any_int b` is false.\na=%A, b=%A" a b
            let check a b = is_any_int a && is_any_int b
            prim_bin_op_template d er check prim_bin_op_helper a b t

        let prim_shuffle_op d a b t =
            let er a b = sprintf "`is_int b` is false.\na=%A, b=%A" a b
            let check a b = is_int b
            prim_bin_op_template d er check prim_bin_op_helper a b t

        let prim_un_op_template d check_error is_check k a t =
            let a = tev d a
            if is_check a then k t a
            else on_type_er (trace d) (check_error a)

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

        let error_non_unit d a =
            let x = tev d a 
            if get_type x <> BVVT then on_type_er (trace d) "Only the last expression of a block is allowed to be unit. Use `ignore` if it intended to be such."
            else x

        let type_lit_create d a =
            match tev d a with
            | TyLit a -> type_lit_create' d a
            | _ -> on_type_er (trace d) "Expected a literal in type literal create."

        let type_lit_cast d a =
            match tev d a with
            | TyT (LitT x) -> TyLit x
            | _ -> on_type_er (trace d) "Expected a literal in type literal cast."

        let type_lit_is d a =
            match tev d a with
            | TyT (LitT _) -> TyLit <| LitBool true
            | _ -> TyLit <| LitBool false

        let lit_is d a =
            match tev d a with
            | TyLit _ -> TyLit <| LitBool true
            | _ -> TyLit <| LitBool false

        let box_is d a =
            match tev d a with
            | TyBox _ -> TyLit <| LitBool true
            | _ -> TyLit <| LitBool false

        let dynamize d a =
            let rec loop = function
                | TyBox(_, (UnionT _ | RecT _)) | TyLit _ as a -> make_tyv_and_push_typed_expr d a
                | TyVV l -> tyvv (List.map loop l)
                | a -> a
            
            loop (tev d a)

        let module_create d l =
            let rec loop acc = function
                | V (N x) -> x :: acc
                | VV (N l) -> List.fold loop acc l
                | ExprPos p -> loop acc p.Expression
                | x -> on_type_er (trace d) <| sprintf "Only variable names are allowed in module create. Got: %A" x
            let er n _ = on_type_er (trace d) <| sprintf "In module create, the variable %s was not found." n
            let env = List.fold (fun s n -> Map.add n (v_find d.env n (er n)) s) Map.empty (loop [] l)
            tyfun(env, FunTypeModule)

        let array_create d size typ =
            let typ = tev_seq d typ |> get_type

            let size,array_type =
                match tev d size with
                | size when is_int size -> size,arrayt(DotNetHeap,typ)
                | size -> on_type_er (trace d) <| sprintf "An size argument in CreateArray is not of type int64.\nGot: %A" size

            TyOp(ArrayCreate,[size],array_type) |> make_tyv_and_push_typed_expr d

        let reference_create d x =
            let x = tev d x
            let array_type = arrayt(DotNetReference, get_type x)
            TyOp(ReferenceCreate,[x],array_type) |> make_tyv_and_push_typed_expr d

        let array_set d ar idx r =
            match tev3 d ar idx r with
            | ar & TyType (ArrayT(DotNetHeap,t)), idx, r when is_int idx && t = get_type r ->
                if is_unit t then TyB
                else make_tyv_and_push_typed_expr_even_if_unit d (TyOp(ArraySet,[ar;idx;r],BVVT))
            | ar & TyType (ArrayT(DotNetReference,t)), idx & TyVV [], r when t = get_type r -> 
                if is_unit t then TyB
                else make_tyv_and_push_typed_expr_even_if_unit d (TyOp(ArraySet,[ar;idx;r],BVVT))
            | x -> on_type_er (trace d) <| sprintf "The two sides in array set have different types. %A" x

        let array_length d ar =
            match tev d ar with
            | ar & TyType (ArrayT(DotNetHeap,t))-> make_tyv_and_push_typed_expr d (TyOp(ArrayLength,[ar],PrimT Int64T))
            | ar & TyType (ArrayT(DotNetReference,t))-> TyLit (LitInt64 1L)
            | x -> on_type_er (trace d) <| sprintf "ArrayLength is only supported for .NET arrays. Got: %A" x

        let module_is d a =
            match tev d a with
            | Func(_,_,FunTypeModule) -> TyLit (LitBool true)
            | _ -> TyLit (LitBool false)

        let boxed_variable_is d a =
            match tev d a with
            | TyType (UnionT _ | RecT _) -> TyLit (LitBool true)
            | _ -> TyLit (LitBool false)

        let module_values d a =
            match tev d a with
            | Func(r,env,FunTypeModule) as recf ->
                let inline toList f = Map.foldBack (fun _ x s -> f x :: s) env []
                match r with
                | RecordIndividual -> toList id
                | _ -> toList (record_boxed_unseal d recf)
                |> tyvv
            | x ->
                on_type_er (trace d) <| sprintf "Expected a module. Got: %A" x

        let module_has_member d a b =
            match tev2 d a b with
            | Func(_,env,FunTypeModule), b -> 
                match b with
                | TypeString b -> TyLit (LitBool <| Map.containsKey b env)
                | _ -> on_type_er (trace d) "Expecting a type literals as the second argument to ModuleHasMember."
            | x -> on_type_er (trace d) <| sprintf "Expecting a module as the first argument to ModuleHasMember. Got: %A" x

        let module_create d l =
            List.fold (fun env -> function
                | VV(N [Lit(N(LitString n)); e]) -> Map.add n (tev d e |> destructure d) env
                | _ -> failwith "impossible"
                ) Map.empty l
            |> fun x -> tyfun(x, FunTypeModule)

        let module_with (d: LangEnv) l =
            let names, bindings =
                match l with
                | VV (N l) :: bindings -> l, bindings
                | V _ as x :: bindings -> [x], bindings
                | x -> failwithf "Malformed ModuleWith. %A" x

            let rec module_with_alt_loop cur_env names = 
                let inline unseal_record name =
                    match Map.tryFind name cur_env with
                    | Some (Func(r,env,FunTypeModule) as recf) -> 
                        match r with
                        | RecordIndividual -> r, env
                        | _ -> r, record_env_term_unseal d recf env
                    | Some _ -> on_type_er (trace d) <| sprintf "Variable %s is not a module." name
                    | _ -> on_type_er (trace d) <| sprintf "Module %s is not bound in the environment." name

                let inline re_record name f =
                    let r,env = unseal_record name
                    match r with
                    | RecordIndividual -> f env
                    | RecordStack -> recordify true d (f env)
                    | RecordHeap -> recordify false d (f env)

                match names with
                | V(N name) :: names -> re_record name <| fun env -> module_with_alt_loop env names
                | Lit(N(LitString name)) :: names -> tyfun (Map.add name (re_record name <| fun env -> module_with_alt_loop env names) cur_env, FunTypeModule)
                | [] ->
                    List.fold (fun env -> function
                        | VV(N [Lit(N(LitString name)); e]) ->
                            match Map.tryFind name env with
                            | Some v -> {d with env = Map.add "self" v d.env}
                            | None -> d
                            |> fun d -> Map.add name (tev d e |> destructure d) env
                        | Op(N(ModuleWithout,[Lit(N(LitString name))])) ->
                            Map.remove name env
                        | _ -> failwith "impossible"
                        ) cur_env bindings
                    |> fun env -> tyfun(env, FunTypeModule)
                | x -> failwithf "Malformed ModuleWith. %A" x
            module_with_alt_loop d.env names

        let failwith_ d a =
            match tev d a with
            | TyType (PrimT StringT) as a -> TyOp(FailWith,[a],BVVT) |> make_tyv_and_push_typed_expr_even_if_unit d
            | _ -> on_type_er (trace d) "Expected a string as input to failwith."

        let unsafe_convert d a b =
            let a,b = tev2 d a b
            let at,bt = get_type a, get_type b
            if at = bt then a
            else
                let inline conv_lit x =
                    match bt with
                    | PrimT Int8T -> int8 x |> LitInt8
                    | PrimT Int16T -> int16 x |> LitInt16
                    | PrimT Int32T -> int32 x |> LitInt32
                    | PrimT Int64T -> int64 x |> LitInt64
                    | PrimT UInt8T -> uint8 x |> LitUInt8
                    | PrimT UInt16T -> uint16 x |> LitUInt16
                    | PrimT UInt32T -> uint32 x |> LitUInt32
                    | PrimT UInt64T -> uint64 x |> LitUInt64
                    | PrimT CharT -> char x |> LitChar
                    | PrimT Float32T -> float32 x |> LitFloat32
                    | PrimT Float64T -> float x |> LitFloat64
                    | _ -> on_type_er (trace d) "Cannot convert the literal to the following type: %A" bt
                    |> TyLit
                match a with
                | TyLit (LitInt8 a) -> conv_lit a
                | TyLit (LitInt16 a) -> conv_lit a
                | TyLit (LitInt32 a) -> conv_lit a
                | TyLit (LitInt64 a) -> conv_lit a
                | TyLit (LitUInt8 a) -> conv_lit a
                | TyLit (LitUInt16 a) -> conv_lit a
                | TyLit (LitUInt32 a) -> conv_lit a
                | TyLit (LitUInt64 a) -> conv_lit a
                | TyLit (LitChar a) -> conv_lit a
                | TyLit (LitFloat32 a) -> conv_lit a
                | TyLit (LitFloat64 a) -> conv_lit a
                | TyLit (LitBool _) -> on_type_er (trace d) "Cannot convert the a boolean literal to the following type: %A" bt
                // The string is not supported because it can't throw an exception if the conversion fails on the Cuda side.
                | TyLit (LitString _) -> on_type_er (trace d) "Cannot convert the a string literal to the following type: %A" bt
                | _ ->
                    let is_convertible_primt x =
                        match x with
                        | PrimT BoolT | PrimT StringT -> false
                        | PrimT _ -> true
                        | _ -> false
                    if is_convertible_primt at && is_convertible_primt bt then TyOp(UnsafeConvert,[a;b],bt)
                    else on_type_er (trace d) "Cannot convert %A to the following type: %A" a bt
            
        let inline add_trace (d: LangEnv) x = {d with trace = x :: (trace d)}

        match expr with
        | Lit (N value) -> TyLit value
        | V (N x) -> v_find d.env x (fun () -> on_type_er (trace d) <| sprintf "Variable %A not bound." x) |> destructure d
        | FunctionFilt(N (vars,N (pat, body))) -> 
            let env = Map.filter (fun k _ -> Set.contains k vars) d.env
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
                | TyFun(C env_term,FunTypeFunction core) -> tyfun(env_term,FunTypeRecFunction(core,name))
                | TyV(tag,FunStackT(env_term,FunTypeFunction core)) -> tyv(tag,FunStackT(env_term,FunTypeRecFunction(core,name)))
                | TyT(FunStackT(env_term,FunTypeFunction core)) -> TyT(FunStackT(env_term,FunTypeRecFunction(core,name)))
                | TyV(tag,FunHeapT(env_term,FunTypeFunction core)) -> tyv(tag,FunHeapT(env_term,FunTypeRecFunction(core,name)))
                | TyT(FunHeapT(env_term,FunTypeFunction core)) -> TyT(FunHeapT(env_term,FunTypeRecFunction(core,name)))
                | x -> x
            | Case,[v;case] -> case_ d v case
            | IfStatic,[cond;tr;fl] -> if_static d cond tr fl
            | If,[cond;tr;fl] -> if_ d cond tr fl
            | FuncJoinPoint,[a] -> memoize_method d a
            | TypeJoinPoint,[a] -> memoize_type d a
            | TermCast,[a;b] -> term_cast d a b
            | UnsafeConvert,[a;b] -> unsafe_convert d a b
            | PrintStatic,[a] -> printfn "%A" (tev d a); TyB
            | PrintEnv,[a] -> 
                Map.iter (fun k v -> printfn "%s" k) d.env
                tev d a
            | PrintExpr,[a] -> printfn "%A" a; tev d a
            | RecordStackify,[a] -> record_stack d a
            | RecordHeapify,[a] -> record_heap d a
            | TypeLitCreate,[a] -> type_lit_create d a
            | TypeLitCast,[a] -> type_lit_cast d a
            | TypeLitIs,[a] -> type_lit_is d a
            | Dynamize,[a] -> dynamize d a
            | LitIs,[a] -> lit_is d a
            | BoxIs,[a] -> box_is d a
            | ModuleOpen,[a;b] -> module_open d a b
            | ModuleCreate,l -> module_create d l
            | ModuleWith, l -> module_with d l
            | ModuleValues, [a] -> module_values d a
            | ModuleIs,[a] -> module_is d a
            | ModuleHasMember,[a;b] -> module_has_member d a b
            | BoxedVariableIs,[a] -> boxed_variable_is d a

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

            | BitwiseAnd,[a;b] -> prim_bitwise_op d a b BitwiseAnd
            | BitwiseOr,[a;b] -> prim_bitwise_op d a b BitwiseOr
            | BitwiseXor,[a;b] -> prim_bitwise_op d a b BitwiseXor

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
            | TypeUnion,[a;b] -> type_union d a b
            | TypeBox,[a;b] -> type_box d a b
            | TypeCreate,[a] -> type_create d a
            | TypeGet,[a] -> type_get d a
            | TypeSplit,[a] -> type_split d a
            | EqType,[a;b] -> eq_type d a b
            | Neg,[a] -> prim_un_numeric d a Neg
            | ErrorType,[a] -> tev d a |> fun a -> on_type_er (trace d) <| sprintf "%A" a
            | ErrorNonUnit,[a] -> error_non_unit d a
            | ErrorPatClause,[] -> on_type_er (trace d) "Compiler error: The pattern matching clauses are malformed. PatClause is missing."
            | ErrorPatMiss,[a] -> tev d a |> fun a -> on_type_er (trace d) <| sprintf "Pattern miss error. The argument is %A" a

            | Log,[a] -> prim_un_floating d a Log
            | Exp,[a] -> prim_un_floating d a Exp
            | Tanh,[a] -> prim_un_floating d a Tanh
            | FailWith,[a] -> failwith_ d a

            // Constants
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
        let is_separator_char c = 
            let inline f x = c = x
            f ' ' || f ',' || f ';' || f '\t' || f '\n' || f '\"' || f '(' || f '{' || f '['
        let is_closing_parenth_char c = 
            let inline f x = c = x
            f ')' || f '}' || f ']'
        let is_operator_char c = (is_identifier_char c || is_separator_char c || is_closing_parenth_char c) = false

        let var_name =
            many1Satisfy2L is_identifier_starting_char is_identifier_char "identifier" .>> spaces
            >>=? function
                | "match" | "function" | "with" | "without" | "open" | "module" | "as" | "when" | "print_env" | "inl" | "met" | "inm" 
                | "type" | "print_expr" | "rec" | "if" | "if_dynamic" | "then" | "elif" | "else" | "true" | "false" as x -> 
                    fun _ -> Reply(Error,messageError <| sprintf "%s not allowed as an identifier." x)
                | x -> preturn x

        let between_brackets l p r = between (skipChar l >>. spaces) (skipChar r >>. spaces) p
        let rounds p = between_brackets '(' p ')'
        let curlies p = between_brackets '{' p '}'
        let squares p = between_brackets '[' p ']'
        
        let keywordString x = attempt (skipString x >>. nextCharSatisfiesNot is_identifier_char >>. spaces)
        let operatorChar x = attempt (skipChar x >>. nextCharSatisfiesNot is_operator_char >>. spaces)
        let prefixOperatorChar x = attempt (skipChar x >>. nextCharSatisfiesNot is_operator_char)
        let operatorString x = attempt (skipString x >>. nextCharSatisfiesNot is_operator_char >>. spaces)

        let when_ = keywordString "when"
        let as_ = keywordString "as"
        let prefix_negate = prefixOperatorChar '-'
        let comma = skipChar ',' >>. spaces
        let dot = operatorChar '.'
        let prefix_dot = prefixOperatorChar '.'
        let pp = operatorChar ':'
        let semicolon' = operatorChar ';'
        let semicolon = semicolon' >>=? fun _ s -> 
            if s.Line <> s.UserState.semicolon_line then Reply(())
            else Reply(ReplyStatus.Error, messageError "cannot parse ; on this line") 
        let eq = operatorChar '=' 
        let eq' = skipChar '=' >>. spaces
        let bar = operatorChar '|' 
        let amphersand = operatorChar '&'
        let caret = operatorChar '^'
        let barbar = operatorString "||" 
        let lam = operatorString "->"
        let set_ref = operatorString ":="
        let set_array = operatorString "<-"
        let inl_ = keywordString "inl"
        let inm_ = keywordString "inm"
        let met_ = keywordString "met"
        let inl_rec = keywordString "inl" >>. keywordString "rec"
        let met_rec = keywordString "met" >>. keywordString "rec"
        let match_ = keywordString "match"
        let function_ = keywordString "function"
        let with_ = keywordString "with"
        let without = keywordString "without"
        let open_ = keywordString "open"
        let cons = operatorString "::"
        let active_pat = prefixOperatorChar '!'
        let not_ = active_pat
        let part_active_pat = prefixOperatorChar '@'
        let ext_active_pat = prefixOperatorChar '#'
        let type_' = keywordString "type"
        let wildcard = operatorChar '_'

        let pbool = ((skipString "false" >>% LitBool false) <|> (skipString "true" >>% LitBool true)) .>> spaces
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
            let default_float x _ = safe_parse Double.TryParse LitFloat64 "default float parse failed" x

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
                    let l = Array.map (fun (k,m) -> keywordString k >>= fun _ -> m x) l
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
                    (if is_x_integer then default_int x else default_float x) .>> spaces
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
            pchar ''' >>. (a <|> b) .>> spaces

        let quoted_string =
            let normalChar = satisfy (fun c -> c <> '\\' && c <> '"')
            let unescape c = match c with
                             | 'n' -> '\n'
                             | 'r' -> '\r'
                             | 't' -> '\t'
                             | c   -> c
            let escapedChar = pchar '\\' >>. (anyOf "\\nrt\"" |>> unescape)
            between (pchar '"') (pchar '"' >>. spaces)
                    (manyChars (normalChar <|> escapedChar))
            |>> LitString

        let lit_ s = 
            choice 
                [|
                pbool
                pnumber
                quoted_string
                quoted_char
                |]
            <| s

        let pat_e = wildcard >>% E
        let pat_var = var_name |>> PatVar
        let pat_tuple pattern = sepBy1 pattern comma |>> function [x] -> x | x -> PatTuple x
        let pat_cons pattern = sepBy1 pattern cons |>> function [x] -> x | x -> PatCons x
        let pat_rounds pattern = rounds (pattern <|>% PatTuple [])
        let pat_type expr pattern = tuple2 pattern (opt (pp >>. ((var_name |>> v) <|> rounds expr))) |>> function a,Some b as x-> PatType(a,b) | a, None -> a
        let pat_active pattern = 
            let active_pat = choice [active_pat >>% PatActive; part_active_pat >>% PatPartActive; ext_active_pat >>% PatExtActive]
            pipe3 active_pat var_name pattern <| fun c name pat -> c (name,pat)
        let pat_or pattern = sepBy1 pattern bar |>> function [x] -> x | x -> PatOr x
        let pat_and pattern = sepBy1 pattern amphersand |>> function [x] -> x | x -> PatAnd x
        let lit_var = lit_ <|> (var_name |>> LitString)
        let pat_type_lit = 
            let literal = lit_var |>> PatTypeLit
            let bind = var_name |>> PatTypeLitBind
            prefix_dot >>. (literal <|> rounds bind)
        let pat_lit = lit_ |>> PatLit
        let pat_when expr pattern = pattern .>>. (opt (when_ >>. expr)) |>> function a, Some b -> PatWhen(a,b) | a, None -> a
        let pat_as pattern = pattern .>>. (opt (as_ >>. pattern )) |>> function a, Some b -> PatAnd [a;b] | a, None -> a

        let pat_named_tuple pattern =
            let pat = pipe2 lit_var (opt (pp >>. pattern)) (fun lit pat ->
                let lit = PatTypeLit lit
                match pat with
                | Some pat -> PatTuple [lit; pat]
                | None -> lit)
            squares (many pat) |>> function [x] -> x | x -> PatTuple x

        let (^<|) a b = a b // High precedence, right associative <| operator

        let rec pat_module_helper (names,bindings) = 
            match names with
            | [x] -> PatMInnerModule(x,bindings)
            | x :: xs -> PatMInnerModule(x,pat_module_helper (xs,bindings))
            | _ -> failwith "Invalid state"
        
        let rec pat_module_outer expr s = 
            curlies (opt (attempt (sepBy1 var_name dot .>> with_)) .>>. pat_module_body expr)
            |>> function
                | Some (n :: (_ :: _ as ns)), bindings -> PatModule(Some n, pat_module_helper (ns,bindings))
                | Some [n], bindings -> PatModule(Some n,bindings)
                | Some [], _ -> failwith "impossible"
                | None, bindings -> PatModule(None,bindings)
            <| s

        and pat_module_inner expr s = 
            curlies ((sepBy1 var_name dot .>> with_) .>>. pat_module_body expr) 
            |>> pat_module_helper <| s

        and pat_module_body expr s =
            let bind = eq' >>. patterns_template expr
            let pat_bind_fun (v,pat) =
                match pat with
                | Some pat -> 
                    let rec loop = function
                        | PatMName name -> PatMRebind(name,pat)
                        | PatMRebind (name,_) as v  -> PatMAnd [v; PatMRebind(name,pat)]
                        | (PatMInnerModule _ | PatMPattern _) as v -> PatMAnd [v;PatMPattern pat]
                        | PatMAnd l -> PatMAnd <| List.map loop l
                        | PatMOr l -> PatMOr <| List.map loop l
                        | PatMXor l -> PatMXor <| List.map loop l
                        | PatMNot x -> PatMNot (loop x)
                    loop v
                | None -> v
                
            let pat_name = var_name |>> PatMName
            let pat_bind pat = (pat .>>. opt bind |>> pat_bind_fun) 
            let inline pat_template sep con pat = sepBy1 pat sep |>> function [x] -> x | x -> con x
            let pat_not pat = (not_ >>. pat |>> PatMNot) <|> pat
            let pat_xor pat = pat_template caret PatMXor pat
            let pat_or pat = pat_template bar PatMOr pat
            let pat_and pat = many pat |>> PatMAnd
            pat_and ^<| pat_or ^<| pat_xor ^<| pat_not ^<| pat_bind ^<| choice [pat_name; pat_module_inner expr; rounds (pat_module_body expr)] <| s

        and patterns_template expr s = // The order in which the pattern parsers are chained in determines their precedence.
            let inline recurse s = patterns_template expr s
            pat_or ^<| pat_when expr ^<| pat_as ^<| pat_tuple ^<| pat_cons ^<| pat_and ^<| pat_type expr 
            ^<| choice [|pat_active recurse; pat_e; pat_var; pat_type_lit; pat_lit; pat_rounds recurse; pat_named_tuple recurse; pat_module_outer expr|] <| s

        let inline patterns expr s = patterns_template expr s
    
        let pattern_list expr = many (patterns expr)
    
        let col (s: CharStream<_>) = s.Column
        let line (s: CharStream<_>) = s.Line

        let set_semicolon_level_to_line line p (s: CharStream<_>) =
            let u = s.UserState
            s.UserState <- { u with semicolon_line=line }
            let r = p s
            s.UserState <- u
            r

        let reset_semicolon_level expr = attempt (set_semicolon_level_to_line -1L expr)

        let expr_indent i op expr (s: CharStream<_>) = if op i (col s) then expr s else pzero s
        let if_then_else expr (s: CharStream<_>) =
            let i = (col s)
            let expr_indent expr (s: CharStream<_>) = expr_indent i (<=) expr s
            let inline f' str = keywordString str >>. expr
            let inline f str = expr_indent (f' str)
            let if_ = f' "if" |>> fun x -> IfStatic, x
            let if_dynamic = f' "if_dynamic" |>> fun x -> If, x
            pipe4 (if_ <|> if_dynamic) (f "then") (many (f "elif" .>>. f "then")) (opt (f "else"))
                (fun (if_op,cond) tr elifs fl -> 
                    let fl = 
                        match fl with Some x -> x | None -> B
                        |> List.foldBack (fun (cond,tr) fl -> op(if_op,[cond;tr;fl])) elifs
                    op(if_op,[cond;tr;fl]))
            <| s

        let poperator (s: CharStream<Userstate>) = many1Satisfy is_operator_char .>> spaces <| s
        let var_op_name = var_name <|> rounds poperator

        let filter_env x = ap (inl "" x) B
        let inl_pat' (args: Pattern list) body = List.foldBack inl_pat args body
        let inline x_pat' memo args body = 
            let body = memo body
            if List.isEmpty args then filter_env body else body
            |> inl_pat' args
        let meth_pat' args body = x_pat' meth_memo args body
        let type_pat' args body = x_pat' type_memo args body


        let inline statement_expr expr = eq' >>. expr
        let case_inl_pat_statement expr = pipe2 (inl_ >>. patterns expr) (statement_expr expr) lp
        let case_inl_name_pat_list_statement expr = pipe3 (inl_ >>. var_op_name) (pattern_list expr) (statement_expr expr) (fun name pattern body -> l name (inl_pat' pattern body)) 
        let case_inl_rec_name_pat_list_statement expr = pipe3 (inl_rec >>. var_op_name) (pattern_list expr) (statement_expr expr) (fun name pattern body -> l_rec name (inl_pat' pattern body))
        
        let case_met_pat_statement expr = pipe2 (met_ >>. patterns expr) (statement_expr expr) <| fun pattern body -> lp pattern (filter_env (meth_memo body))
        let case_met_name_pat_list_statement expr = pipe3 (met_ >>. var_op_name) (pattern_list expr) (statement_expr expr) (fun name pattern body -> l name (meth_pat' pattern body))
        let case_met_rec_name_pat_list_statement expr = pipe3 (met_rec >>. var_op_name) (pattern_list expr) (statement_expr expr) <| fun name pattern body -> l_rec name (meth_pat' pattern body)

        let case_type_name_pat_list_statement expressions expr = 
            let type_parse (s: CharStream<_>) = 
                let i = col s
                let expr_indent expr (s: CharStream<_>) = expr_indent i (=) expr s
                many1 (expr_indent expressions) |>> (List.map type_create >> List.reduce type_union >> type_create) <| s
            pipe3 (type_' >>. var_op_name) (pattern_list expr) (eq' >>. type_parse) <| fun name pattern body -> 
                l_rec name (type_pat' pattern body)

        let case_open expr = open_ >>. expr |>> module_open
        let case_inm_pat_statement expr = pipe2 (inm_ >>. patterns expr) (eq' >>. expr) inmp

        let statements expressions expr = 
            [case_inl_pat_statement; case_inl_name_pat_list_statement; case_inl_rec_name_pat_list_statement
             case_met_pat_statement; case_met_name_pat_list_statement; case_met_rec_name_pat_list_statement
             case_open; case_inm_pat_statement; case_type_name_pat_list_statement expressions]
            |> List.map (fun x -> x expr |> attempt)
            |> choice

        let inline expression_expr expr = lam >>. reset_semicolon_level expr
        let case_inl_pat_list_expr expr = pipe2 (inl_ >>. pattern_list expr) (expression_expr expr) inl_pat'
        let case_met_pat_list_expr expr = pipe2 (met_ >>. pattern_list expr) (expression_expr expr) meth_pat'

        let case_lit expr = lit_ |>> lit
        let case_if_then_else expr = if_then_else expr
        let case_rounds expr s = (rounds (reset_semicolon_level expr <|>% B)) s
        let case_var expr = var_op_name |>> v

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
            
            let set_col (s: CharStream<_>) = i <- Some (col s); Reply(())

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

        let case_lit_lift expr = 
            let var = var_name |>> (LitString >> type_lit_lift)
            let lit = expr |>> type_lit_lift'
            prefix_dot >>. (var <|> lit)

        let case_print_env expr s = 
            let i = col s
            keywordString "print_env" >>. expr_indent i (<=) expr |>> print_env
            <| s

        let case_print_expr expr s = 
            let i = col s
            keywordString "print_expr" >>. expr_indent i (<=) expr |>> print_expr
            <| s

          
        let case_module expr s =
            let mp_binding (n,e) = vv [lit_string n; e]
            let mp_without n = op(ModuleWithout,[lit_string n])
            let mp_create l = op(ModuleCreate,l)
            let mp_with (n,l) = 
                match n with
                | [x] -> op(ModuleWith,v x :: l)
                | x :: xs -> op(ModuleWith,vv (v x :: List.map lit_string xs) :: l)
                | _ -> failwith "impossible"

            let inline parse_binding_with s =
                let i = col s
                let line = s.Line
                var_op_name .>>. opt (eq' >>. expr_indent i (<) (set_semicolon_level_to_line line expr)) 
                |>> function a, None -> mp_binding (a, v a) | a, Some b -> mp_binding (a, b)
                <| s

            let parse_binding_without s = var_op_name |>> mp_without <| s

            let module_create_with s = (parse_binding_with .>> optional semicolon') s
            let module_create_without s = (parse_binding_without .>> optional semicolon') s

            let module_with = 
                let withs s = (with_ >>. many1 module_create_with) s
                let withouts s = (without >>. many1 module_create_without) s 
                pipe2 (sepBy1 var_name dot) (many1 (withs <|> withouts))
                <| fun names l -> mp_with (names,List.concat l)

            let module_create = 
                many module_create_with |>> mp_create
                
            curlies (attempt module_with <|> module_create) <| s

        let case_type expr = type_' >>. rounds expr |>> type_get // rounds are needed to avoid collisions with the statement parser

        let case_named_tuple expr =
            let pat s = 
                let i = col s
                let line = line s
                pipe2 lit_var (opt (pp >>. expr_indent i (<) (set_semicolon_level_to_line line expr))) (fun lit expr ->
                    let tup = type_lit_lift lit
                    match expr with
                    | Some expr -> vv [tup; expr]
                    | None -> tup
                    ) s
            squares (many (pat .>> optional semicolon')) |>> function [x] -> x | x -> vv x

        let case_negate expr = previousCharSatisfiesNot (is_separator_char >> not) >>. prefix_negate >>. expr |>> (ap (v "negate"))

        let rec expressions expr s =
            let unary_ops = 
                [case_lit_lift; case_negate]
                |> List.map (fun x -> x (expressions expr) |> attempt)
                |> choice
            let expressions = 
                [case_print_env; case_print_expr; case_type
                 case_inl_pat_list_expr; case_met_pat_list_expr; case_lit; case_if_then_else
                 case_rounds; case_typecase; case_typeinl; case_var; case_module; case_named_tuple]
                |> List.map (fun x -> x expr |> attempt)
                |> choice
            expressions <|> unary_ops <| s
 
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
            let i = col s
            let pos' s = Reply(pos' s)
            let inline if_ op tr s = expr_indent i op tr s
            let inline many_indents expr = many1 (if_ (<=) (expr .>> optional semicolon))
            many_indents ((tuple2 pos' statements |>> ParserStatement) <|> (tuple2 pos' expressions |>> ParserExpr)) >>= process_parser_exprs <| s

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

        let mset statements expressions (s: CharStream<_>) = 
            let i = col s
            let line = s.Line
            let expr_indent expr (s: CharStream<_>) = expr_indent i (<) expr s
            let op =
                (set_ref >>% fun l r -> op(ArraySet,[l;B;r]) |> preturn)
                <|> (set_array >>% fun l r -> 
                        let rec loop = function
                            | ExprPos p -> loop p.Expression
                            | Op(N(Apply,[a;b])) -> op(ArraySet,[a;b;r]) |> preturn
                            | _ -> fail "Expected two arguments on the left of <-."
                        loop l)

            (tuple2 expressions (opt (expr_indent op .>>. expr_indent (set_semicolon_level_to_line line statements)))
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
                f "+" 60; f "-" 60; f "*" 70; f "/" 70; f "%" 70
                f "<|" 10; f "|>" 10; f "<<" 10; f ">>" 10

            let no_assoc_ops = // No associativity is really left associativity
                let f str = add_infix_operator Associativity.None str 40
                f "<="; f "<"; f "="; f ">"; f ">="; f "<>"
                f "<<<"; f ">>>"; f "&&&"; f "|||"

            let right_associative_ops =
                let f str prec = add_infix_operator Associativity.Right str prec
                f "||" 20; f "&&" 30; f "::" 50; f "^^^" 45
         
            dict_operator

        let operators expr (s: CharStream<_>) =
            let poperator (s: CharStream<Userstate>) =
                let dict_operator = s.UserState.ops
                let p = pos' s
                (poperator >>=? function
                    | "->" | ":=" | "<-" -> fail "forbidden operator"
                    | orig_op -> 
                        let rec calculate on_fail op' = 
                            match dict_operator.TryGetValue op' with
                            | true, (prec,asoc) -> preturn (prec,asoc,fun a b -> 
                                match orig_op with
                                | "||" -> expr_pos p (op(Or, [a; b]))
                                | "&&" -> expr_pos p (op(And, [a; b]))
                                | _ -> expr_pos p (ap' (v orig_op) [a; b]))
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

        let rec expr s = 
            let expressions s = mset expr ^<| tuple ^<| operators ^<| application ^<| expressions expr <| s
            let statements s = statements expressions expr <| s
            annotations ^<| indentations statements expressions <| s
        runParserOnString (spaces >>. expr .>> eof) {ops=inbuilt_operators; semicolon_line= -1L} module_name module_code

    // #Codegen
    let spiral_codegen main =
        let buffer_type_definitions = ResizeArray()
        let buffer_method = ResizeArray()
        let buffer_main = ResizeArray()
        let buffer_final = ResizeArray()
        let exp x = String.concat "" x

        let process_statements (statements: ResizeArray<ProgramNode>) =
            let process_statement (code: StringBuilder,ind as state) statement =
                match statement with
                | Statement x -> [|String.replicate ind " "; x; "\n"|] |> exp |> code.Append, ind
                | Indent -> code, ind+4
                | Dedent -> code, ind-4
            Seq.fold process_statement (StringBuilder(),0) statements
            |> fun (code,ind) -> code.ToString()

        let buffer (d: CodegenEnv) = d.buffer
        let trace (d: CodegenEnv) = d.trace

        let state d x = (buffer d).Add <| Statement x
        let enter' d f = (buffer d).Add Indent; f(); (buffer d).Add Dedent
        let enter d f = 
            enter' d <| fun _ -> 
                match f() with
                | "" -> ()
                | s -> state d s

        let (|Unit|_|) x = if is_unit x then Some () else None

        let fsharp_definitions_set = h0()
        let cuda_definitions_set = h0()
        let definitions_queue = Queue<TypeOrMethod>()

        let print_tag_tuple' t = sprintf "Tuple%i" t
        let print_tag_union' t = sprintf "Union%i" t
        let print_tag_rec' t = sprintf "Rec%i" t
        let print_tag_env' t = sprintf "Env%i" t
        let print_tag_env_stack' t = sprintf "EnvStack%i" t
        let print_tag_env_heap' t = sprintf "EnvHeap%i" t
        let print_tag_closuret' t = sprintf "FunPointer%i" t

        let fsharp_sym_dict = d0()
        let cuda_sym_dict = d0()
        let sym (sym_dict: Dictionary<_,_>) t =
            match sym_dict.TryGetValue t with
            | true, v -> v
            | false, _ ->
                let v = sym_dict.Count
                sym_dict.[t] <- v
                v

        let def_enqueue d f t =
            let definitions_set,sym_dict =
                match d.backend_type with
                | FSharp -> fsharp_definitions_set,fsharp_sym_dict
                | Cuda -> cuda_definitions_set,cuda_sym_dict
            if definitions_set.Add (TomType t) then definitions_queue.Enqueue (TomType t)
            f (sym sym_dict t)

        let print_tag_tuple d t = def_enqueue d print_tag_tuple' t
        let print_tag_union d t = def_enqueue d print_tag_union' t
        let print_tag_rec d t = 
            match d.backend_type with
            | Cuda -> on_type_er (trace d) "Recursive types are not allowed on the Cuda side."
            | FSharp -> def_enqueue d print_tag_rec' t
        let print_tag_env d t = def_enqueue d print_tag_env' t
        let print_tag_env_stack d t = def_enqueue d print_tag_env_stack' t
        let print_tag_env_heap d t =  
            match d.backend_type with
            | Cuda -> on_type_er (trace d) "Heap allocated types are not allowed on the Cuda side."
            | FSharp -> def_enqueue d print_tag_env_heap' t

        let rec print_type d = function
            | Unit ->
                match d.backend_type with
                | Cuda -> "void"
                | FSharp -> "unit"
            | FunT _ as x -> print_tag_env d x
            | FunStackT _ as x -> print_tag_env_stack d x
            | FunHeapT _ as x -> print_tag_env_heap d x
            | VVT _ as x -> print_tag_tuple d x
            | UnionT _ as x -> print_tag_union d x
            | RecT _ as x -> print_tag_rec d x
            | ArrayT(array_type,t) ->
                match d.backend_type with
                | Cuda ->
                    match array_type with
                    | CudaGlobal | CudaLocal -> sprintf "(%s [])" (print_type d t)
                    | CudaShared -> sprintf "(__shared__ %s [])" (print_type d t)
                    | _ -> on_type_er (trace d) "Other array types not supported on the Cuda side."
                | FSharp ->
                    match array_type with
                    | DotNetReference -> sprintf "(%s ref)" (print_type d t)
                    | DotNetHeap -> sprintf "(%s [])" (print_type d t)
                    | _ -> on_type_er (trace d) "Other array types not supported on the F# side."
            | DotNetTypeInstanceT (N t) -> 
                match d.backend_type with
                | FSharp -> print_dotnet_instance_type t
                | Cuda -> failwith "Should have been stopped at the join point."
            | ClosureT(a,b) as t ->
                match d.backend_type with
                | FSharp ->
                    let a = tuple_field_ty a |> List.map (print_type d) |> String.concat " * "
                    sprintf "(%s -> %s)" a (print_type d b)
                | Cuda ->
                    def_enqueue d print_tag_closuret' t
            | PrimT x ->
                match d.backend_type with
                | FSharp ->
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
                    | Float64T -> "float"
                    | BoolT -> "bool"
                    | StringT -> "string"
                    | CharT -> "char"
                | Cuda ->
                    match x with
                    | UInt8T -> "unsigned char"
                    | UInt16T -> "unsigned short"
                    | UInt32T -> "unsigned int"
                    | UInt64T -> "unsigned long long int"
                    | Int8T -> "char"
                    | Int16T -> "short"
                    | Int32T -> "int"
                    | Int64T -> "long long int"
                    | Float32T -> "float"
                    | Float64T -> "double"
                    | BoolT -> "int"
                    | CharT -> "unsigned short"
                    | StringT -> "unsigned short *"
            | LitT _ | DotNetAssemblyT _ | DotNetTypeRuntimeT _ -> 
                failwith "Should be covered in Unit."
                

        and print_dotnet_instance_type d (x: System.Type) =
            if x.GenericTypeArguments.Length > 0 then
                [|
                x.Namespace
                "." 
                x.Name.Split '`' |> Array.head
                "<"
                Array.map (dotnet_type_to_ty >> print_type d) x.GenericTypeArguments |> String.concat ","
                ">"
                |] |> String.concat null
            else
                [|x.Namespace; "."; x.Name|] |> String.concat null

        let print_tyv (tag,ty) = sprintf "var_%i" tag
        let print_tyv_with_type d (tag,ty as v) = sprintf "(%s: %s)" (print_tyv v) (print_type d ty)
        let print_method tag = sprintf "method_%i" tag

        let print_args d args = 
            Seq.choose (fun (_,ty as x) ->
                if is_unit ty = false then print_tyv_with_type d x |> Some
                else None) args
            |> String.concat ", "

        let get_tag =
            let mutable i = 0
            fun () -> i <- i + 1; i

        let print_case_rec d x i = print_tag_rec d x + sprintf "Case%i" i
        let print_case_union d x i = print_tag_union d x + sprintf "Case%i" i

        let inline handle_unit_in_last_position d f =
            let c = d.buffer.Count
            match f () with
            | "" ->
                match Seq.last d.buffer with
                | Statement s when s.StartsWith "let " -> "()"
                | _ when c = d.buffer.Count -> "()"
                | _ -> ""
            | x -> x

        let rec codegen' d expr =
            let inline codegen expr = codegen' d expr
            let print_value d x = 
                match d.backend_type with
                | FSharp ->
                    match x with
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
                | Cuda ->
                    match x with
                    | LitUInt8 x -> string x 
                    | LitUInt16 x -> string x 
                    | LitUInt32 x -> string x 
                    | LitUInt64 x -> string x 
                    | LitInt8 x -> string x
                    | LitInt16 x -> string x
                    | LitInt32 x -> string x
                    | LitInt64 x -> string x
                    | LitFloat32 x -> string x
                    | LitFloat64 x -> string x
                    | LitBool x -> if x then "1" else "0"
                    | LitChar x -> uint16 x |> string
                    | LitString x -> on_type_er (trace d) "String literals are not implemented on the Cuda side."

            let inline print_if_tail f = f()

            let inline print_if d (_,t as v) f =
                match t with
                | Unit -> f ()
                | _ ->
                    sprintf "let %s =" (print_tyv_with_type d v) |> state d
                    enter' d <| fun _ -> f()

            let inline if_ d v cond tr fl =
                let enter f = enter d <| fun _ -> handle_unit_in_last_position d f
                
                let inline k() = 
                    sprintf "if %s then" (codegen cond) |> state d
                    enter <| fun _ -> codegen tr
                    "else" |> state d
                    enter <| fun _ -> codegen fl
                match v with
                | Some tyv -> print_if d tyv k
                | None -> k()

            let make_struct l on_empty on_rest =
                Seq.choose (fun x -> let x = codegen x in if x = "" then None else Some x) l
                |> String.concat ", "
                |> function
                    | "" -> on_empty
                    | x -> on_rest x

            let if_not_unit ty f = if is_unit ty then "" else f()

            let (|DotNetPrintedArgs|) x = List.map codegen x |> List.filter ((<>) "") |> String.concat ", "

            let fsharp_array_create d size = function
                | ArrayT(_,t) -> sprintf "Array.zeroCreate<%s> (System.Convert.ToInt32(%s))" (print_type d t) (codegen size)
                | _ -> failwith "impossible"

            let fsharp_reference_create x = sprintf "(ref %s)" (codegen x)
            let fsharp_array_index ar idx = sprintf "%s.[int32 %s]" (codegen ar) (codegen idx)
            let fsharp_array_length ar = sprintf "%s.LongLength" (codegen ar)
            let fsharp_reference_index x = sprintf "(!%s)" (codegen x)

            let fsharp_array_set ar idx r = sprintf "%s <- %s" (fsharp_array_index ar idx) (codegen r) |> state d
            let fsharp_reference_set l r = sprintf "%s := %s" (codegen l) (codegen r) |> state d

            let cuda_array_index ar idx = sprintf "%s[%s]" (codegen ar) (codegen idx) |> state d
            let cuda_array_set ar idx r = sprintf "%s = %s" (cuda_array_index ar idx) |> state d
//
//            let string_length str = sprintf "(int64 %s.Length)" (codegen str)
//            let string_index str idx = sprintf "%s.[int32 %s]" (codegen str) (codegen idx)
//            let string_slice str a b = sprintf "%s.[int32 %s..int32 %s]" (codegen str) (codegen a) (codegen b)

            let match_with d print_if v cases =
                print_if <| fun _ ->
                    let print_case =
                        match get_type v with
                        | RecT _ as x -> print_case_rec d x
                        | UnionT _ as x -> print_case_union d x
                        | _ -> failwith "impossible"

                    sprintf "match %s with" (codegen v) |> state d
                    let print_case i case = 
                        let case = codegen case
                        if String.IsNullOrEmpty case then sprintf "| %s ->" (print_case i) |> state d
                        else sprintf "| %s(%s) ->" (print_case i) case |> state d
                    let rec loop i = function
                        | case :: body :: rest -> 
                            print_case i case
                            enter d <| fun _ -> handle_unit_in_last_position d (fun _ -> codegen body)
                            loop (i+1) rest
                        | [] -> ()
                        | _ -> failwith "The cases should always be in pairs."
                    loop 0 cases

            let unsafe_convert a t =
                let conv_func = 
                    match t with
                    | PrimT Int8T -> "int8"
                    | PrimT Int16T -> "int16"
                    | PrimT Int32T -> "int32"
                    | PrimT Int64T -> "int64"
                    | PrimT UInt8T -> "uint8"
                    | PrimT UInt16T -> "uint16"
                    | PrimT UInt32T -> "uint32"
                    | PrimT UInt64T -> "uint64"
                    | PrimT CharT -> "char"
                    | PrimT Float32T -> "float32"
                    | PrimT Float64T -> "float"
                    | _ -> failwith "impossible"
                sprintf "(%s %s)" conv_func (codegen a)

            match expr with
            | TyT Unit | TyV (_, Unit) -> ""
            | TyT t -> on_type_er (trace d) <| sprintf "Usage of naked type %A as an instance on the term level is invalid." t
            | TyV v -> print_tyv v
            | TyLet(tyv,b,TyV tyv',_,trace) when tyv = tyv' -> codegen' {d with trace=trace} b
            | TyState(b,rest,_,trace) ->
                match b with
                | TyOp(ArraySet,[ar;idx;b],_) ->
                    match get_type ar with
                    | ArrayT(DotNetReference,_) -> fsharp_reference_set ar b
                    | ArrayT(DotNetHeap,_) -> fsharp_array_set ar idx b
                    | ArrayT(CudaLocal,_) -> cuda_array_set ar idx b
                    | _ -> on_type_er d.trace "Setting Cuda arrays on the F# side is not supported."
                | _ ->
                    let b = codegen' d b
                    if b <> "" then sprintf "%s" b |> state d
                codegen' d rest
            | TyLet(tyv,TyOp(If,[cond;tr;fl],t),rest,_,trace) -> if_ (Some tyv) cond tr fl; codegen' trace rest
            | TyLet(tyv,TyOp(Case,v :: cases,t),rest,_,trace) -> match_with (print_if tyv) v cases; codegen' trace rest
            | TyLet(tyv,b,rest,_,trace) -> sprintf "let %s = %s" (print_tyv_with_type tyv) (codegen' trace b) |> state; codegen' trace rest
            | TyLit x -> print_value x
            | TyJoinPoint((S method_tag & method_key,_ as join_point_key),_) ->
                if definitions_set.Add (TomMethod method_key) then definitions_queue.Enqueue (TomMethod method_key)

                let method_name = print_method method_tag
                match join_point_dict.[join_point_key] with
                | JoinPointType, _, _ -> ""
                | JoinPointMethod, fv, _ -> sprintf "%s(%s)" method_name (print_args fv)
                | JoinPointClosure args, fv, rev_renamer ->
                    let fv = Seq.filter (fun x -> args.Contains x = false) fv //(Set fv) - (Set args)
                    if Seq.isEmpty fv then method_name
                    else sprintf "%s(%s)" method_name (print_args fv)
            | TyBox(x, t) ->
                let case_name =
                    let union_idx s = Seq.findIndex ((=) (get_type x)) s
                    match t with
                    | UnionT s -> print_case_union t (union_idx s)
                    | RecT tag -> 
                        match rect_dict.[tag] with
                        | VVT _ -> print_tag_rec t
                        | UnionT s -> print_case_rec t (union_idx s)
                        | _ -> failwith "Only VVT and UnionT can be recursive types."
                    | _ -> failwith "Only VVT and UnionT can be boxed types."
                if is_unit (get_type x) then case_name else sprintf "(%s(%s))" case_name (codegen x)
            | TyFun(C env_term, _) ->
                let t = get_type expr
                Map.toArray env_term
                |> Array.map snd
                |> fun x -> make_struct x "" (fun args -> sprintf "(%s(%s))" (print_tag_env t) args)
            | TyVV l -> let t = get_type expr in make_struct l "" (fun args -> sprintf "%s(%s)" (print_tag_tuple t) args)
            | TyOp(op,args,t) ->
                match op, args with
                | Apply,[a;b] ->
                    // Apply during codegen is only used for applying closures.
                    // There is one level of flattening in the outer arguments.
                    // The reason for this is the symmetry between the F# and the Cuda side.
                    let b = tuple_field b |> List.map codegen |> String.concat ", "
                    sprintf "%s(%s)" (codegen a) b
                | Case,v :: cases -> match_with print_if_tail v cases; ""
                | If,[cond;tr;fl] -> if_ None cond tr fl; ""
                | ArrayCreate,[a] -> array_create a t
                | ReferenceCreate,[a] -> reference_create a
                | ArrayIndex,[b & TyType(ArrayT(DotNetHeap,_));c] -> array_index b c
                | ArrayIndex,[b & TyType(ArrayT(DotNetReference,_));c] -> reference_index b
                | ArrayLength,[a] -> array_length a
                | StringIndex,[str;idx] -> string_index str idx
                | StringSlice,[str;a;b] -> string_slice str a b
                | StringLength,[str] -> string_length str
                | UnsafeConvert,[a;b] -> unsafe_convert a t

                // Primitive operations on expressions.
                | Add,[a;b] -> sprintf "(%s + %s)" (codegen a) (codegen b)
                | Sub,[a;b] -> sprintf "(%s - %s)" (codegen a) (codegen b)
                | Mult,[a;b] -> sprintf "(%s * %s)" (codegen a) (codegen b)
                | Div,[a;b] -> sprintf "(%s / %s)" (codegen a) (codegen b)
                | Mod,[a;b] -> sprintf "(%s %% %s)" (codegen a) (codegen b)
                | LT,[a;b] -> sprintf "(%s < %s)" (codegen a) (codegen b)
                | LTE,[a;b] -> sprintf "(%s <= %s)" (codegen a) (codegen b)
                | EQ,[a;b] -> sprintf "(%s = %s)" (codegen a) (codegen b)
                | NEQ,[a;b] -> sprintf "(%s <> %s)" (codegen a) (codegen b)
                | GT,[a;b] -> sprintf "(%s > %s)" (codegen a) (codegen b)
                | GTE,[a;b] -> sprintf "(%s >= %s)" (codegen a) (codegen b)
                | And,[a;b] -> sprintf "(%s && %s)" (codegen a) (codegen b)
                | Or,[a;b] -> sprintf "(%s || %s)" (codegen a) (codegen b)
                | BitwiseAnd,[a;b] -> sprintf "(%s &&& %s)" (codegen a) (codegen b)
                | BitwiseOr,[a;b] -> sprintf "(%s ||| %s)" (codegen a) (codegen b)
                | BitwiseXor,[a;b] -> sprintf "(%s ^^^ %s)" (codegen a) (codegen b)

                | ShiftLeft,[x;y] -> sprintf "(%s << %s)" (codegen x) (codegen y)
                | ShiftRight,[x;y] -> sprintf "(%s >> %s)" (codegen x) (codegen y)

        //        | ShuffleXor,[x;y],_) -> sprintf "cub::ShuffleXor(%s, %s)" (codegen x) (codegen y)
        //        | ShuffleUp,[x;y],_) -> sprintf "cub::ShuffleUp(%s, %s)" (codegen x) (codegen y)
        //        | ShuffleDown,[x;y],_) -> sprintf "cub::ShuffleDown(%s, %s)" (codegen x) (codegen y)
        //        | ShuffleIndex,[x;y],_) -> sprintf "cub::ShuffleIndex(%s, %s)" (codegen x) (codegen y)

                | Neg,[a] -> sprintf "(-%s)" (codegen a)
                | VVIndex,[a;TyLit(LitInt64 b)] -> if_not_unit t <| fun _ -> sprintf "%s.mem_%i" (codegen a) b
                | RecordIndividualUnseal,[r; TyLit (LitString k)] -> if_not_unit t <| fun _ -> sprintf "%s.mem_%s" (codegen r) k
                | RecordBoxedUnseal,[r; TyV (i,_)] -> if_not_unit t <| fun _ -> sprintf "%s.mem_%i" (codegen r) i
                | (RecordStackify | RecordHeapify),[a] ->
                    let {fv=fv} as r = renamables0()
                    renamer_apply_typedexpr r a |> ignore
                    match op with
                    | RecordStackify ->
                        let args = Seq.map print_tyv_with_type fv |> String.concat ", "
                        sprintf "%s(%s)" (print_tag_env_stack t) args
                    | RecordHeapify ->
                        let args = Seq.mapi (fun i x -> sprintf "mem_%i = %s" i (print_tyv_with_type x)) fv |> String.concat "; "
                        sprintf "({%s} : %s)" args (print_tag_env_heap t)
                    | _ -> failwith "impossible"
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

        let type_prefix =
            let mutable type_is_first = true
            fun () -> if type_is_first then type_is_first <- false; "type" else "and"
        let method_prefix =
            let mutable method_is_first = true
            fun () -> if method_is_first then method_is_first <- false; "let rec" else "and"

        let print_union_cases print_case tys =
            enter' <| fun _ ->
                List.iteri (fun i -> function
                    | Unit -> "| " + print_case i |> state
                    | x -> sprintf "| %s of %s" (print_case i) (print_type x) |> state) tys

        let print_x_definition is_struct iter fold name tys =
            let args sep f =
                fold (fun s k ty -> 
                    match ty with
                    | Unit -> s
                    | _ -> f k :: s) [] tys
                |> List.rev
                |> String.concat sep

            if is_struct then
                sprintf "%s %s =" (type_prefix()) name |> state
                enter' <| fun _ -> 
                    "struct" |> state
                    iter (fun k ty -> 
                        match ty with
                        | Unit -> ()
                        | _ -> sprintf "val mem_%s: %s" k (print_type ty) |> state
                        ) tys

                    let args_declaration = args ", " <| fun k -> sprintf "arg_mem_%s" k
                    let args_mapping = args "; " <| fun k -> sprintf "mem_%s = arg_mem_%s" k k
                    sprintf "new(%s) = {%s}" args_declaration args_mapping |> state
                    "end" |> state
            else
                sprintf "%s %s =" (type_prefix()) name |> state
                enter' <| fun _ -> 
                    "{" |> state
                    iter (fun k ty -> 
                        match ty with
                        | Unit -> ()
                        | _ -> sprintf "mem_%s: %s" k (print_type ty) |> state
                        ) tys
                    "}" |> state

        let print_method_definition tag (join_point_type, fv, body) = 
            let method_name = print_method tag

            let print_body() = enter <| fun _ -> handle_unit_in_last_position (fun _ -> codegen' [] body)

            match join_point_type with
            | JoinPointClosure args ->
                let printed_fv = 
                    Seq.filter (fun x -> args.Contains x = false) fv
                    |> fun fv -> if Seq.isEmpty fv then "" else sprintf "(%s) " (print_args fv)
                sprintf "%s %s %s(%s): %s =" (method_prefix()) method_name printed_fv (print_args args) (print_type (get_type body)) |> state
                print_body()
            | JoinPointMethod ->
                sprintf "%s %s(%s): %s =" (method_prefix()) method_name (print_args fv) (print_type (get_type body)) |> state
                print_body()
            | JoinPointType -> ()

        codegen' [] main |> state // Can't forget the non-method

        let move_to (buffer: ResizeArray<_>) (temp: ResizeArray<_>) = buffer.AddRange(temp); temp.Clear()
        move_to buffer_main buffer_temp

        while definitions_queue.Count > 0 do
            let inline print_fun_x is_stack env x =
                let {fv=fv} as r = renamables0()
                renamer_apply_env r env |> ignore
                if Map.forall (fun _ -> get_type >> is_unit) env = false then
                    let tuple_name = 
                        if is_stack then print_tag_env_stack x
                        else print_tag_env_heap x
                    let iter f = Seq.iter (fun (k,ty )-> f (string k) ty)
                    let fold f = Seq.fold (fun s (k,ty) -> f s (string k) ty)
                    print_x_definition is_stack iter fold tuple_name fv

            match definitions_queue.Dequeue() with
            | TomMethod key ->
                match memoized_methods.[key] with
                | MemoMethod (join_point_type, fv, body) -> 
                    print_method_definition key.Symbol (join_point_type, fv, body)
                    move_to buffer_method buffer_temp
                | _ -> failwith "impossible"
            | TomType ty ->
                match ty with
                | FunT(tys, _) as x ->
                    if is_unit_env tys = false then
                        let tuple_name = print_tag_env x
                        print_x_definition true Map.iter Map.fold tuple_name tys
                | FunStackT(C env, _) as x -> print_fun_x true env x
                | FunHeapT(C env, _) as x -> print_fun_x false env x
                | RecT tag as x ->
                    let tys = rect_dict.[tag]
                    sprintf "%s %s =" (type_prefix()) (print_tag_rec x) |> state
                    match tys with
                    | Unit -> "| " + print_tag_rec' tag |> state
                    | VVT _ -> sprintf "| %s of %s" (print_tag_rec' tag) (print_type tys) |> state
                    | UnionT tys -> print_union_cases (print_case_rec x) (Set.toList tys)
                    | x -> failwithf "Only VVT and UnionT are recursive types. Got: %A" x
                | UnionT tys as x ->
                    sprintf "%s %s =" (type_prefix()) (print_tag_union x) |> state
                    print_union_cases (print_case_union x) (Set.toList tys)
                | VVT tys as x ->
                    if is_unit_tuple tys = false then
                        let tuple_name = print_tag_tuple x
                        let fold f s l = List.fold (fun (i,s) ty -> i+1, f s (string i) ty) (0,s) l |> snd
                        let iter f l = List.iteri (fun i x -> f (string i) x) l
                        print_x_definition true iter fold tuple_name tys
                | _ -> failwith "impossible"
                move_to buffer_type_definitions buffer_temp

        move_to buffer_final buffer_type_definitions
        move_to buffer_final buffer_method
        move_to buffer_final buffer_main
  
        process_statements buffer_final

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

    let data_empty () = {
        ltag = ref 0; seq=ref id; trace=[]; rbeh=AnnotationDive
        env = Map.empty
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
            l "split" (p type_split)
            l "box" (p2 type_box)
            l "type_error" (type_lit_lift <| LitString "TypeConstructorError")
            l "stack" (p record_stackify)
            l "heap" (p record_heapify)

            l "bool" (op(TypeGet,[lit <| LitBool true]))
            l "int64" (op(TypeGet,[lit <| LitInt64 0L]))
            l "int32" (op(TypeGet,[lit <| LitInt32 0]))
            l "int16" (op(TypeGet,[lit <| LitInt16 0s]))
            l "int8" (op(TypeGet,[lit <| LitInt8 0y]))
            l "uint64" (op(TypeGet,[lit <| LitUInt64 0UL]))
            l "uint32" (op(TypeGet,[lit <| LitUInt32 0u]))
            l "uint16" (op(TypeGet,[lit <| LitUInt16 0us]))
            l "uint8" (op(TypeGet,[lit <| LitUInt8 0uy]))
            l "float64" (op(TypeGet,[lit <| LitFloat64 0.0]))
            l "float32" (op(TypeGet,[lit <| LitFloat32 0.0f]))
            l "string" (op(TypeGet,[lit <| LitString ""]))
            l "char" (op(TypeGet,[lit <| LitChar ' ']))
            l "unit" (op(TypeGet,[B]))

            l "type_lit_lift" (p <| fun x -> op(TypeLitCreate,[x]))
            l "type_lit_cast" (p <| fun x -> op(TypeLitCast,[x]))
            l "type_lit_is" (p <| fun x -> op(TypeLitIs,[x]))
            l "term_cast" (p2 term_cast)
            l "unsafe_convert" (p2 <| fun a b -> op(UnsafeConvert,[a;b]))
            l "negate" (p <| fun x -> op(Neg,[x]))
        
            l "load_assembly" (p <| fun x -> op(DotNetLoadAssembly,[x]))
            l "mscorlib" (ap (v "load_assembly") (ap (v "type_lit_lift") (lit_string "mscorlib")))
            l "ignore" (inl "" B)
            l "id" (p <| id)
            l "const" (p <| fun x -> inl "" x)
            l "ref" (p <| fun x -> op(ReferenceCreate,[x]))
            l "array_create" (p2 <| fun size typ -> op(ArrayCreate,[size;typ]))
            l "array_length" (p <| fun ar -> op(ArrayLength,[ar]))

            b "+" Add; b "-" Sub; b "*" Mult; b "/" Div; b "%" Mod
            b "<|" Apply; l "|>" (p2 (flip apply)); l "<<" (p3 compose); l ">>" (p3 (flip compose))

            b "<=" LTE; b "<" LT; b "=" EQ; b ">" GT; b ">=" GTE; b "<>" NEQ
            b "&&&" BitwiseAnd; b "|||" BitwiseOr; b "^^^" BitwiseXor
            b "::" VVCons; b "&&" And; b "||" Or
            b "<<<" ShiftLeft; b ">>>" ShiftRight

            l "fst" (p <| fun x -> tuple_index x 0L)
            l "snd" (p <| fun x -> tuple_index x 1L)

            l "tuple_length" (p <| fun x -> op(VVLength,[x]))
            l "tuple_index" (p2 tuple_index')
            l "not" (p <| fun x -> eq x (lit <| LitBool false))
            l "string_length" (p <| fun x -> op(StringLength,[x]))
            l "lit_is" (p <| fun x -> op(LitIs,[x]))
            l "box_is" (p <| fun x -> op(BoxIs,[x]))
            l "failwith" (p <| fun x -> op(FailWith,[x]))
            l "assert" (p2 <| fun c x -> if_static (eq c (lit (LitBool false))) (op(FailWith,[x])) B)
            l "max" (p2 <| fun a b -> if_static (lt a b) b a)
            l "min" (p2 <| fun a b -> if_static (lt a b) a b)
            b "eq_type" EqType
            l "module_values" (p <| fun x -> op(ModuleValues,[x]))
            l "boxed_variable_is" (p <| fun x -> op(BoxedVariableIs,[x]))
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

    let watch = System.Diagnostics.Stopwatch.StartNew()
    parse_modules module_main Fail <| fun body -> 
        printfn "Running %s." module_name
        printfn "Time for parse: %A" watch.Elapsed
        watch.Restart()
        let d = data_empty()
        let input = core_functions body |> expr_prepass |> snd
        printfn "Time for prepass: %A" watch.Elapsed
        watch.Restart()
        try
            let x = !d.seq (expr_peval d input)
            printfn "Time for peval was: %A" watch.Elapsed
            printfn "Time spent in renaming was: %A" renaming_time
            watch.Restart()
            let x = Succ (spiral_codegen x)
            printfn "Time for codegen was: %A" watch.Elapsed
            x
        with 
        | :? TypeError as e -> 
            let trace, message = e.Data0, e.Data1
            let message = if message.Length > 300 then message.[0..299] + "..." else message
            Fail <| print_type_error trace message
