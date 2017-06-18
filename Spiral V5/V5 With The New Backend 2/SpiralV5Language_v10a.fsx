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
    | VVT of Ty list * string
    | NameT of string
    | FunctionT of EnvTy * FunctionCore // Type level function. Can also be though of as a procedural macro.
    | ModuleT of EnvTy
    | UnionT of Set<Ty>
    | TypeConstructor of Ty list * string
    | RecT of int
    | LocalPointerT of Ty
    | SharedPointerT of Ty
    | GlobalPointerT of Ty
    | ClosureT of Ty * Ty
    | ForCastT of Ty // For casting type level function to term (ClosureT) level ones.

and Tag = int64
and TyV = Tag * Ty
and EnvTerm = Map<string, TypedExpr>
and EnvTy = Map<string, Ty>
and FunctionCore = string * (Pattern * Expr) list
and MemoKey = EnvTerm * Expr

and Pattern =
    | A of Pattern * string // Type annotation case
    | A' of Pattern * Ty
    | S of string
    | S' of string // match if not tuple
    | R of Pattern list * Pattern option // Tuple
    | F of Pattern * string // Functiona application with retracing.
    | N of string * Pattern // Matches a tuple name and proceeds onto the pattern on a hit.
    | C of string * Pattern // Matches a type constructor name and proceeds onto the pattern on a hit.
    | Or of Pattern * Pattern
    | When of Pattern * Expr

and TypedPattern =
    | PatS of TyV
    | PatR of TypedPattern list * TypedPattern option
    | PatF of TypedPattern * TypedExpr
    | PatWhen of TypedPattern * TypedExpr

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
    | TypeAnnot
    | ModuleWith
    | ModuleWith'
    | EnvUnseal
    | TypeSeal

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

and TypedExpr =
    | TyType of Ty
    | TyV of TyV
    | TyLet of LetType * TyV * TypedExpr * TypedExpr * Ty
    | TyLit of Value
    
    | TyVV of TypedExpr list * Ty
    | TyEnv of EnvTerm * Ty
    | TyOp of Op * TypedExpr list * Ty
    | TyMemoizedExpr of MemoExprType * Arguments * Renamer * Tag * Ty
    | TyMatch of TypedPattern * Ty

and MemoCases =
    | MethodInEvaluation
    | MethodDone of TypedExpr
// This key is for functions without arguments. It is intended that the arguments be passed in through the Environment.
and MemoDict = Dictionary<MemoKey, MemoCases * Tag * Arguments>
and ClosureDict = Dictionary<Tag, TypedExpr> 

type Result<'a,'b> = Succ of 'a | Fail of 'b

let flip f a b = f b a
