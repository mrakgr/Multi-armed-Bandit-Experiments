module Spiral.Types

// Global open
open System
open System.Collections.Generic
open HashConsing

// Language types
type LayoutType =
    | LayoutStack
    | LayoutPackedStack
    | LayoutHeap
    | LayoutHeapMutable

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
    override x.GetHashCode() = hash expr
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
    // Closure
    | ClosureTypeCreate
    | ClosureIs
    | ClosureDomain
    | ClosureRange

    // Cast
    | UnsafeUpcastTo
    | UnsafeDowncastTo

    // Cuda
    | Syncthreads
    | CudaKernels

    | ThreadIdxX | ThreadIdxY | ThreadIdxZ
    | BlockIdxX | BlockIdxY | BlockIdxZ

    | BlockDimX | BlockDimY | BlockDimZ
    | GridDimX | GridDimY | GridDimZ

    // Pattern matching errors
    | ErrorPatMiss
    | ErrorPatClause

    // StringOps
    | StringLength
    | StringIndex
    | StringSlice

    // DotNetOps
    | DotNetAssemblyLoad
    | DotNetAssemblyLoadFile
    | DotNetTypeConstruct
    | DotNetTypeCallMethod
    | DotNetTypeGetField
    | DotNetEventAddHandler

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
    | JoinPointEntryMethod
    | JoinPointEntryType
    | JoinPointEntryCuda
    | StructCreate
    | ListIndex
    | ListSliceFrom
    | ListCons
    | ListLength
    | ListIs
    | TypeAnnot
    | MapGetField
    | LayoutToStack
    | LayoutToPackedStack
    | LayoutToHeap
    | LayoutToHeapMutable
    | TypeGet
    | TypeUnion
    | TypeSplit
    | TypeBox
    | EqType
    | ModuleHasMember

    | ArrayCreate
    | ReferenceCreate
    | ArrayIndex
    | MutableSet
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

type SSExpr = // SS are the Spiral .NET interop types. SS is short for 'S'piral 'S'ystem Type.
    | SSAp of SSExpr * SSExpr
    | SSSubst of SSExpr * SSExpr
    | SSType of Ty
    | SSVar of string
    | SSArray of SSExpr []
    | SSLam of string [] * SSExpr
    | SSCompileTypeDefinition of Type // These 3 act like unhygienic macros.
    | SSCompileMethod of Reflection.MethodInfo
    | SSCompileField of Reflection.FieldInfo

and SSTypedExpr =
    | SSTyType of Ty
    | SSTyVar of string
    | SSTyArray of SSTypedExpr []
    | SSTyLam of SSEnvTerm * string [] * SSExpr
    | SSTyClass of SSTypedExprClass

and SSMemberMap = Map<string, SSTypedExpr[]>

and SSTypedExprClass = {
    full_name : string
    assembly_name : string
    methods : SSMemberMap
    static_methods : SSMemberMap
    fields : SSMemberMap
    static_fields : SSMemberMap
    }
    
and SSEnvTerm = Map<string,SSTypedExpr>

and FunctionCore = string * Expr

and MapType =
    | MapTypeFunction of FunctionCore // Type level function. Can also be though of as a procedural macro.
    | MapTypeRecFunction of FunctionCore * string
    | MapTypeModule

and Pattern =
    | E
    | PatVar of string
    | PatTuple of Pattern list
    | PatCons of Pattern list
    | PatTypeEq of Pattern * Expr
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
    | PatTypeClosure of Pattern * Pattern

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
    | ListT of Ty list
    | LitT of Value
    | MapT of EnvTy * MapType
    | LayoutT of LayoutType * ConsedNode<EnvTerm> * MapType
    | ClosureT of Ty * Ty
    | UnionT of Set<Ty>
    | RecT of JoinPointKey
    | ArrayT of ArrayType * Ty
    | DotNetTypeT of Node<SSTypedExpr>

and TypedExpr =
    // Data structures
    | TyT of Ty
    | TyV of TyTag
    | TyList of TypedExpr list
    | TyMap of ConsedNode<EnvTerm> * MapType
    | TyBox of TypedExpr * Ty
    | TyLit of Value

    // Operations
    | TyLet of TyTag * TypedExpr * TypedExpr * Ty * Trace
    | TyState of TypedExpr * TypedExpr * Ty * Trace
    | TyOp of Op * TypedExpr list * Ty
    | TyJoinPoint of JoinPointKey * JoinPointType * Arguments * Ty

and JoinPointType =
    | JoinPointClosure
    | JoinPointMethod
    | JoinPointType
    | JoinPointCuda

and JoinPointState<'a,'b> =
    | JoinPointInEvaluation of 'a
    | JoinPointDone of 'b

and Tag = int
and TyTag = Tag * Ty
and EnvTy = Map<string, Ty>
and EnvTerm = Map<string, TypedExpr>
and JoinPointKey = Node<Expr * EnvTerm>

and Arguments = TyTag list
and Renamer = Dictionary<Tag,Tag>

// This key is for functions without arguments. It is intended that the arguments be passed in through the Environment.
and JoinPointDict<'a,'b> = Dictionary<JoinPointKey, JoinPointState<'a,'b>>
// For Common Subexpression Elimination. I need it not for its own sake, but to enable other PE based optimizations.
and CSEDict = Map<TypedExpr,TypedExpr> ref

and Trace = PosKey list

and TraceNode<'a when 'a:equality and 'a:comparison>(expr:'a, trace:Trace) = 
    member x.Expression = expr
    member x.Trace = trace
    override x.ToString() = sprintf "%A" expr
    override x.GetHashCode() = hash expr
    override x.Equals(y) = 
        match y with 
        | :? TraceNode<'a> as y -> expr = y.Expression
        | _ -> failwith "Invalid equality for TraceNode."

    interface IComparable with
        member x.CompareTo(y) = 
            match y with
            | :? TraceNode<'a> as y -> compare expr y.Expression
            | _ -> failwith "Invalid comparison for TraceNode."

type TypeOrMethod =
    | TomType of Ty
    | TomJP of JoinPointType * JoinPointKey

let inline t (x: TraceNode<_>) = x.Expression
let (|T|) x = t x

type RecursiveBehavior =
    | AnnotationDive
    | AnnotationReturn

type LangEnv = {
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
type Userstate = {
    ops : Dictionary<string, int * FParsec.Associativity>
    semicolon_line : int64
    }

type ParserExpr =
| ParserStatement of PosKey * (Expr -> Expr)
| ParserExpr of PosKey * Expr

// Codegen types
type CodegenEnv = {
    branch_return: string -> string
    trace: Trace
    }

type Buf = ResizeArray<ProgramNode>
and ProgramNode =
    | Statement of sep: string * code: string
    | Indent
    | Dedent

type Renamables = {
    memo : Dictionary<TypedExpr,TypedExpr>
    renamer : Dictionary<Tag,Tag>
    ref_call_args : TyTag list ref
    ref_method_pars : TyTag list ref
    }

type Argumentables = {
    renamer' : Dictionary<Tag,Tag>
    call_args : TyTag list
    method_pars : TyTag list
    }

let cuda_kernels_name = "cuda_kernels"

type AssemblyLoadType =
    | LoadType of Type
    | LoadMap of Map<string,AssemblyLoadType>

let string_to_op =
    let cases = Microsoft.FSharp.Reflection.FSharpType.GetUnionCases(typeof<Op>)
    let dict = d0()
    cases |> Array.iter (fun x ->
        dict.[x.Name] <- Microsoft.FSharp.Reflection.FSharpValue.MakeUnion(x,[||]) :?> Op
        )
    dict.TryGetValue

let core =
    (
    "Core",[],"The Core module.",
    """
inl type_lit_lift x = !TypeLitCreate(x)
inl assembly_load x = !DotNetAssemblyLoad(x)
inl assembly_load_file x = !DotNetAssemblyLoadFile(x)

inl mscorlib = assembly_load."mscorlib"

inl error_type x = !ErrorType(x)
inl print_static x = !PrintStatic(x)
inl dyn x = !Dynamize(x)
inl (\/) a b = !TypeUnion(a,b)
inl (=>) a b = !ClosureTypeCreate(a,b)
inl split x = !TypeSplit(x)
inl box a b = !TypeBox(a,b)
inl stack x = !LayoutToStack(x)
inl packed_stack x = !LayoutToPackedStack(x)
inl heap x = !LayoutToHeap(x)
inl heapm x = !LayoutToHeapMutable(x)

inl bool = type(true)

inl int64 = type(0i64)
inl int32 = type(0i32)
inl int16 = type(0i16)
inl int8 = type(0i8)

inl uint64 = type(0u64)
inl uint32 = type(0u32)
inl uint16 = type(0u16)
inl uint8 = type(0u8)

inl float64 = type(0f64)
inl float32 = type(0f32)

inl string = type("")
inl char = type(' ')
inl unit = type(())

inl type_lit_cast x = !TypeLitCast(x)
inl type_lit_is x = !TypeLitIs(x)
inl term_cast to from = !TermCast(to,from)
// TODO: This one might be better implemented directly in the language once the Extern module becomes operational.
inl unsafe_convert to from = !UnsafeConvert(to,from) 
inl negate x = !Neg(x)
inl ignore x = ()
inl id x = x
inl const x _ = x
inl ref x = !ReferenceCreate(x)
inl Array = {
    create = inl typ size -> !ArrayCreate(size,typ)
    length = inl ar -> !ArrayLength(ar)
    }

inl (+) a b = !Add(a,b)
inl (-) a b = !Sub(a,b)
inl (*) a b = !Mult(a,b)
inl (/) a b = !Div(a,b)
inl (%) a b = !Mod(a,b)

inl (|>) a b = b a
inl (<|) a b = a b
inl (>>) a b x = b (a x)
inl (<<) a b x = a (b x)

inl (<=) a b = !LTE(a,b)
inl (<) a b = !LT(a,b)
inl (=) a b = !EQ(a,b)
inl (<>) a b = !NEQ(a,b)
inl (>) a b = !GT(a,b)
inl (>=) a b = !GTE(a,b)

inl (&&&) a b = !BitwiseAnd(a,b)
inl (|||) a b = !BitwiseOr(a,b)
inl (^^^) a b = !BitwiseXor(a,b)

inl (::) a b = !ListCons(a,b)
inl (&&) a b = !And(a,b)
inl (||) a b = !Or(a,b)
inl (<<<) a b = !ShiftLeft(a,b)
inl (>>>) a b = !ShiftRight(a,b)
inl Tuple = {
    length = inl x -> !ListLength(x)
    index = inl v i -> !ListIndex(v,i)
    }

inl fst x :: _ = x
inl snd _ :: x :: _ = x

inl not x = x = false
inl string_length x = !StringLength(x)
inl lit_is x = !LitIs(x)
inl box_is x = !BoxIs(x)
inl failwith typ msg = !FailWith(typ,msg)
inl assert c msg = if c then () else failwith unit msg
inl max a b = if a > b then a else b
inl min a b = if a > b then b else a
inl eq_type a b = !EqType(a,b)
inl module_values x = !ModuleValues(x)
inl uncased_variable_is x = !BoxedVariableIs(x)
inl event_add_handler a b c = !DotNetEventAddHandler(a,b,c)
inl (:>) a b = !UnsafeUpcastTo(b,a)
inl (:?>) a b = !UnsafeDowncastTo(b,a)

inl prim_eq = (=)
// Structural polymorphic equality for every type in the language (apart from functions).
inl (=) a b =
    inl rec (=) a b =
        inl body = function
            | .(a), .(b) -> a = b
            | a :: as', b :: bs -> a = b && as' = bs
            | {} & a, {} & b -> module_values a = module_values b
            | (), () -> true
            | a, b when eq_type a b -> prim_eq a b // This repeat eq_type check is because unboxed union types might lead to variables of different types to be compared.
            | _ -> false
        if uncased_variable_is a && uncased_variable_is b then (met _ -> body (a, b) : bool)()
        else body (a, b)
    if eq_type a b then a = b
    else error_type ("Trying to compare variables of two different types. Got:",a,b)

{type_lit_lift assembly_load assembly_load_file mscorlib error_type print_static dyn (\/) (=>)
 split box stack packed_stack heap heapm bool int64 int32 int16 int8 uint64 uint32 uint16 uint8 float64 float32
 string char unit type_lit_cast type_lit_is term_cast unsafe_convert negate ignore id const ref Array (+) (-) (*) (/) (%)
 (|>) (<|) (>>) (<<) (<=) (<) (=) (<>) (>) (>=) (&&&) (|||) (^^^) (::) (&&) (||) (<<<) (>>>) Tuple fst snd not
 string_length lit_is box_is failwith assert max min eq_type module_values uncased_variable_is event_add_handler (:>)
 (:?>) (=)}
    """) |> module_
