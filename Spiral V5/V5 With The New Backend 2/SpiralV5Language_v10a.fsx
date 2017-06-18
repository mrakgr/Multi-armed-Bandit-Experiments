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

let x = function
    | a :: b :: c :: d -> 1
    | a -> 2