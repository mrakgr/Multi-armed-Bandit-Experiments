#load "Typechecker_v5b.fsx"
open Typechecker_v5b
open System.Collections.Generic
open System.Text

type CudaProgram =
| Statement of string
| Indent
| Dedent
| Statements of ResizeArray<CudaProgram>

let exp x = String.concat "" x
let state (program: ResizeArray<_>) x = exp x |> Statement |> program.Add
let enter (program: ResizeArray<_>) body v =
    Indent |> program.Add
    body v
    Dedent |> program.Add
let expand (program: ResizeArray<_>) x = Statements x |> program.Add

let process_statements (statements: ResizeArray<CudaProgram>) =
    let rec process_statement (code: StringBuilder,ind as state) statement =
        match statement with
        | Statement x -> [|String.replicate ind " "; x; "\n"|] |> exp |> code.Append, ind
        | Indent -> code, ind+4
        | Dedent -> code, ind-4
        | Statements x -> process_statements state x
    and process_statements state (statements: ResizeArray<CudaProgram>) =
        Seq.fold process_statement state statements
    process_statements (StringBuilder(),0) statements
    |> fun (code,ind) -> code.ToString()


let codegen (imemo: MethodImplDict) main_expr =
    let program = ResizeArray()

    let state x = program.Add <| Statement x
    let enter f = 
        program.Add Indent
        f() |> state
        program.Add Dedent

    let rec codegen = function
        | TyV(tag,_,ty) -> sprintf "var_%i" tag
        | TyIf(cond,tr,fl,_) -> 
            sprintf "if (%s) {" (codegen cond) |> state
            enter <| fun _ -> sprintf "return %s;" (codegen tr)
            "}" |> state
            "else {" |> state
            enter <| fun _ -> sprintf "return %s;" (codegen fl)
            "};" |> state
            ""
        | TyLet((tag,_,ty),b,e,_) ->
            sprintf "%s var_%i = %s;" (print_type ty) tag (codegen b) |> state
            codegen e
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
    ()

//let eval x = 
//    match eval x with
//    | Succ (typed_exp, imemo) ->
//        Fail "placeholder"
