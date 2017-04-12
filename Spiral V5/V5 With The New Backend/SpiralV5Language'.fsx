#load "SpiralV5Ops.fsx"

open SpiralV5CudaTypechecker_v6e
open SpiralV5Ops
open SpiralV5DevVar

type SpiralExprType =
| TensorT of uint64 [] * SpiralDeviceVarType
| SST of SpiralExprType list

type SpiralExpr =
| S of string
| SS of SpiralExpr list
| Func of string * SpiralExpr * SpiralExpr
| Inl of SpiralExpr * SpiralExpr
| Ap of SpiralExpr * SpiralExpr
| BackendCall of CudaExpr * SpiralExpr

type TypedSpiralExpr =
| TyS of string * SpiralExprType
| TySS of TypedSpiralExpr list * SpiralExprType
| TyFunc of string * TypedSpiralExpr * TypedSpiralExpr * SpiralExprType
| TyLet of string * TypedSpiralExpr * TypedSpiralExpr * SpiralExprType
| TyAp of TypedSpiralExpr * TypedSpiralExpr * SpiralExprType
| TyBackendCall of ManagedCuda.CudaKernel * TypedSpiralExpr * SpiralExprType
//| FFICall of 

open System.Collections.Generic
open ManagedCuda.VectorTypes

type SpiralTypecheckerEnv =
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