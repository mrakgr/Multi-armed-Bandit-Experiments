#load "Typechecker_v5.fsx"
open Typechecker_v5
open System.Collections.Generic

let codegen (imemo: Dictionary<TyMethodKey,TypedExpr * Set<TyV>>) exp =
    
    match exp with
    | TyV of TyV
    | TyIf of TypedExpr * TypedExpr * TypedExpr * Ty
    | TyLet of TyV * TypedExpr * TypedExpr * Ty
    | TyUnit
    | TyLitInt of int
    | TyLitFloat of float
    | TyLitBool of bool
    | TyMethodCall of TyMethodKey * MethodCall * Ty
    | TyMethod of TyMethodKey * MethodCall * Ty
    | TyVars of TypedExpr list * Ty

    // Cuda kernel constants
    | TyThreadIdxX | TyThreadIdxY | TyThreadIdxZ
    | TyBlockIdxX | TyBlockIdxY | TyBlockIdxZ
    | TyBlockDimX | TyBlockDimY | TyBlockDimZ
    | TyGridDimX | TyGridDimY | TyGridDimZ

    // Array cases
    | TyIndexArray of TypedExpr * TypedExpr list * Ty
    | TyCreateArray of TypedExpr list * Ty
    
    // Primitive operations on expressions.
    | TyAdd of TypedExpr * TypedExpr * Ty
    | TySub of TypedExpr * TypedExpr * Ty
    | TyMult of TypedExpr * TypedExpr * Ty
    | TyDiv of TypedExpr * TypedExpr * Ty
    | TyMod of TypedExpr * TypedExpr * Ty
    | TyLT of TypedExpr * TypedExpr
    | TyLTE of TypedExpr * TypedExpr
    | TyEQ of TypedExpr * TypedExpr
    | TyGT of TypedExpr * TypedExpr
    | TyGTE of TypedExpr * TypedExpr
    | TyLeftShift of TypedExpr * TypedExpr * Ty
    | TyRightShift of TypedExpr * TypedExpr * Ty
    | TySyncthreads
    | TyShuffleXor of TypedExpr * TypedExpr * Ty
    | TyShuffleUp of TypedExpr * TypedExpr * Ty
    | TyShuffleDown of TypedExpr * TypedExpr * Ty
    | TyShuffleSource of TypedExpr * TypedExpr * Ty
    | TyLog of TypedExpr * Ty
    | TyExp of TypedExpr * Ty
    | TyTanh of TypedExpr * Ty
    | TyNeg of TypedExpr * Ty
    // Mutable operations.
    | TyMSet of TypedExpr * TypedExpr
    | TyAtomicAdd of TypedExpr * TypedExpr * Ty
    | TyWhile of TypedExpr * TypedExpr * TypedExpr * Ty


let eval x = 
    match eval x with
    | Succ (typed_exp, imemo) ->
        Fail "placeholder"
