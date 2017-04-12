#load "load-project-release.fsx"

open ManagedCuda.VectorTypes
open System.Collections.Generic

type CudaTy =
    | GlobalArrayT of TypedCudaExpr list * SpiralTy
    | SharedArrayT of TypedCudaExpr list * SpiralTy
    | LocalArrayT of TypedCudaExpr list * SpiralTy

and SpiralFrontTy =
    | TensorT of int * SpiralTy

and SpiralTy =
    | UnitT
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
    | VVT of SpiralTy list
    | TagT of int64

    // Backend specific types.
    | CudaTy of CudaTy
    | SpiralFrontTy of SpiralFrontTy
    
and TyV = int64 * SpiralTy

// No return type polymorphism like in Haskell for now. Local type inference only.
and TyMethodKey = int64 * SpiralTy // The key does not need to know the free variables.

and CudaExpr =
    // Array cases
    | IndexArray of SpiralExpr * SpiralExpr list
    | CreateSharedArray of SpiralExpr list * typeof: SpiralExpr
    | CreateLocalArray of SpiralExpr list * typeof: SpiralExpr

    // Primitive operations on expressions.
    | Add of SpiralExpr * SpiralExpr
    | Sub of SpiralExpr * SpiralExpr
    | Mult of SpiralExpr * SpiralExpr
    | Div of SpiralExpr * SpiralExpr
    | Mod of SpiralExpr * SpiralExpr
    | LT of SpiralExpr * SpiralExpr
    | LTE of SpiralExpr * SpiralExpr
    | EQ of SpiralExpr * SpiralExpr
    | GT of SpiralExpr * SpiralExpr
    | GTE of SpiralExpr * SpiralExpr
    | LeftShift of SpiralExpr * SpiralExpr
    | RightShift of SpiralExpr * SpiralExpr
    | Syncthreads
    | ShuffleXor of SpiralExpr * SpiralExpr
    | ShuffleUp of SpiralExpr * SpiralExpr
    | ShuffleDown of SpiralExpr * SpiralExpr
    | ShuffleIndex of SpiralExpr * SpiralExpr
    | Log of SpiralExpr
    | Exp of SpiralExpr
    | Tanh of SpiralExpr
    | Neg of SpiralExpr
    // Cuda kernel constants
    | ThreadIdxX | ThreadIdxY | ThreadIdxZ
    | BlockIdxX | BlockIdxY | BlockIdxZ
    | BlockDimX | BlockDimY | BlockDimZ
    | GridDimX | GridDimY | GridDimZ
    // Mutable operations.
    | MSet of SpiralExpr * SpiralExpr * SpiralExpr
    | AtomicAdd of out: SpiralExpr * in_: SpiralExpr
    // Loops
    | While of SpiralExpr * SpiralExpr * SpiralExpr
    // Cub operations
    | CubBlockReduce of SpiralExpr * SpiralExpr * SpiralExpr option

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

and SpiralFrontendExpr =
    | BackendCall of CudaExpr * SpiralExpr

and SpiralExpr = 
    | V of string // standard variable
    | V' of TyV // given variable
    | T of TypedCudaExpr
    | B // blank
    | If of SpiralExpr * SpiralExpr * SpiralExpr
    | Inlineable of SpiralExpr * SpiralExpr
    | LitInt of int
    | LitFloat of float
    | LitBool of bool
    | Apply of SpiralExpr * args: SpiralExpr
    | Method of name: string * args: SpiralExpr * body: SpiralExpr

    // Tuple cases
    | IndexVV of SpiralExpr * SpiralExpr
    | VV of SpiralExpr list
    | MapVV of SpiralExpr * SpiralExpr
    
    // Sublanguage cases
    | CudaExpr of CudaExpr
    | SpiralExpr of SpiralFrontendExpr

and TypedCudaExpr =
    // Array cases
    | TyIndexArray of TypedCudaExpr * TypedCudaExpr list * SpiralTy
    | TyCreateArray of SpiralTy

    // Cuda kernel constants
    | TyThreadIdxX | TyThreadIdxY | TyThreadIdxZ
    | TyBlockIdxX | TyBlockIdxY | TyBlockIdxZ
    | TyBlockDimX | TyBlockDimY | TyBlockDimZ
    | TyGridDimX | TyGridDimY | TyGridDimZ
   
    // Primitive operations on expressions.
    | TyAdd of TypedCudaExpr * TypedCudaExpr * SpiralTy
    | TySub of TypedCudaExpr * TypedCudaExpr * SpiralTy
    | TyMult of TypedCudaExpr * TypedCudaExpr * SpiralTy
    | TyDiv of TypedCudaExpr * TypedCudaExpr * SpiralTy
    | TyMod of TypedCudaExpr * TypedCudaExpr * SpiralTy
    | TyLT of TypedCudaExpr * TypedCudaExpr
    | TyLTE of TypedCudaExpr * TypedCudaExpr
    | TyEQ of TypedCudaExpr * TypedCudaExpr
    | TyGT of TypedCudaExpr * TypedCudaExpr
    | TyGTE of TypedCudaExpr * TypedCudaExpr
    | TyLeftShift of TypedCudaExpr * TypedCudaExpr * SpiralTy
    | TyRightShift of TypedCudaExpr * TypedCudaExpr * SpiralTy
    | TySyncthreads
    | TyShuffleXor of TypedCudaExpr * TypedCudaExpr * SpiralTy
    | TyShuffleUp of TypedCudaExpr * TypedCudaExpr * SpiralTy
    | TyShuffleDown of TypedCudaExpr * TypedCudaExpr * SpiralTy
    | TyShuffleIndex of TypedCudaExpr * TypedCudaExpr * SpiralTy
    | TyLog of TypedCudaExpr * SpiralTy
    | TyExp of TypedCudaExpr * SpiralTy
    | TyTanh of TypedCudaExpr * SpiralTy
    | TyNeg of TypedCudaExpr * SpiralTy
    // Mutable operations.
    | TyMSet of TypedCudaExpr * TypedCudaExpr * TypedCudaExpr * SpiralTy
    | TyAtomicAdd of TypedCudaExpr * TypedCudaExpr * SpiralTy
    | TyWhile of TypedCudaExpr * TypedCudaExpr * TypedCudaExpr * SpiralTy
    // Cub operations
    | TyCubBlockReduce of dim: TypedCudaExpr * input: TypedCudaExpr * method_: TypedCudaExpr * num_valid: TypedCudaExpr option * SpiralTy

and TypedSpiralFrontendExpr =
    | TyBackendCall of ManagedCuda.CudaKernel * TypedSpiralExpr * SpiralTy

// This is being compiled to STLC, not System F, so no type variables are allowed in the processed AST.
and TypedSpiralExpr =
    // These two will not get code gen'd.
    // The difference from the past version of the typechecker is that now the TagT type exists.
    | Inlineable' of SpiralExpr * SpiralExpr * Env * SpiralTy
    | Method' of name: string * args: SpiralExpr * body: SpiralExpr * Env * SpiralTy
    
    | TyV of TyV
    | TyIf of TypedSpiralExpr * TypedSpiralExpr * TypedSpiralExpr * SpiralTy
    | TyLet of TyV * TypedSpiralExpr * TypedSpiralExpr * SpiralTy
    | TyUnit
    | TyLitInt of int
    | TyLitFloat of float
    | TyLitBool of bool
    | TyMethodCall of TyMethodKey * TypedSpiralExpr * SpiralTy
    
    // Tuple cases
    | TyIndexVV of TypedSpiralExpr * TypedSpiralExpr * SpiralTy
    | TyVV of TypedSpiralExpr list * SpiralTy
    
    // Sublanguage cases
    | TyCudaExpr of TypedCudaExpr
    | TySpiralExpr of TypedSpiralFrontendExpr

and Env = Map<string, TypedCudaExpr>
// method key * method body * bound variables
and MethodCases =
    | MethodInEvaluation of SpiralTy option
    | MethodDone of TyV list * TypedCudaExpr
and MethodDict = Dictionary<TyMethodKey, MethodCases>
and TaggedDict = Dictionary<int64,TypedCudaExpr>
// method key * method body * implicit arguments
and MethodImplDict = Dictionary<TyMethodKey, TyV list * TypedCudaExpr * Set<TyV>>
and CudaTypecheckerEnv =
    {
    // Immutable
    env : Env
    // Mutable
    tagged_vars : TaggedDict // For looking up the the unapplied Inlineables and Methods
    memoized_methods : MethodDict // For hoisted out global methods.
    sequences : (TypedCudaExpr -> TypedCudaExpr) option ref // For sequencing statements.
    recursive_methods_stack : Stack<unit -> TypedCudaExpr> // For typechecking recursive calls.
    blockDim : dim3 // since Cub needs hard constants, I need to have them during typechecking.
    gridDim : dim3
    }

// Do I cut the language like this?