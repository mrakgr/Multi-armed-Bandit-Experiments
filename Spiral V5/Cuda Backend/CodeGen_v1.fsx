#load "Typechecker_v6a.fsx"
open Typechecker_v6a
open System.Collections.Generic
open System.Text

type CudaProgram =
| Statement of string
| Indent
| Dedent
| Statements of ResizeArray<CudaProgram>

let exp x = String.concat "" x

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

let print_method_dictionary (imemo: MethodImplDict) =
    let program = ResizeArray()

    let state x = program.Add <| Statement x
    let enter' f = 
        program.Add Indent
        f()
        program.Add Dedent
    let enter f = 
        enter' (fun _ -> f() |> state)

    let tuple_definitions = Dictionary(HashIdentity.Structural)
    let tuple_def_proc t f = 
        match tuple_definitions.TryGetValue t with
        | true, v -> f v
        | false, _ ->
            let v = get_tag()
            tuple_definitions.Add(t,v)
            f v

    let rec print_type = function
        | UnitT -> "void"
        | UInt32T -> "unsigned int"
        | UInt64T -> "unsigned long long int"
        | Int32T -> "int"
        | Int64T -> "long long int"
        | Float32T -> "float"
        | Float64T -> "double"
        | BoolT -> "int"
        | VTT _ as t -> tuple_def_proc t (fun v -> sprintf "tuple_%i" v)
        | NominalT x -> x
        | ConstT x -> sprintf "const %s" (print_type x)
        | SharedT x -> sprintf "__shared__ %s" (print_type x)
        | ArrayT (_,t) -> sprintf "%s *" (print_type t)
        | ArrT _ -> failwith "This thing is just a placeholder for now."

    let print_tyv (tag,_,_) = sprintf "var_%i" tag
    let print_tyv_with_type (tag,_,ty) =
        sprintf "%s var_%i" (print_type ty) tag
    let print_method tag = sprintf "method_%i" tag

    let rec print_methodcall = function
        | MCTag _ | MCET _ -> []
        | MCVV x | MCVT x -> List.collect print_methodcall x
        | MCTypedExpr(_,x) -> [codegen x]

    and codegen = function
        | TyV x -> print_tyv x
        | TyIf(cond,tr,fl,_) -> // If statements will aways be hoisted into methods in this language.
            sprintf "if (%s) {" (codegen cond) |> state
            enter <| fun _ -> sprintf "return %s;" (codegen tr)
            "}" |> state
            "else {" |> state
            enter <| fun _ -> sprintf "return %s;" (codegen fl)
            "}" |> state
            ""
        | TyLet(tyv,TyCreateArray(ar_sizes,ar_ty),e,_) ->
            let dims =
                List.map (codegen >> sprintf "[%s]") ar_sizes
                |> String.concat ""
            sprintf "%s%s;" (print_tyv_with_type tyv) dims |> state
            codegen e
        | TyLet(tyv,b,e,_) ->
            sprintf "%s = %s;" (print_tyv_with_type tyv) (codegen b) |> state
            codegen e
        | TyUnit -> ""
        | TyLitInt x -> string x
        | TyLitFloat x -> string x
        | TyLitBool x -> if x then "1" else "0"
        | TyMethodCall((tag,_ as mkey),call,t) ->
            let (_,_,implicit_args) = imemo.[mkey]
            let implicit_args = Set.map print_tyv implicit_args |> Set.toList
            let explicit_args = print_methodcall call
            let args = implicit_args @ explicit_args |> String.concat ", "
            let method_name = print_method tag
            sprintf "%s(%s)" method_name args

        | TyMethod((tag,_ as mkey),call,t) -> 
            // This case is not for printing method signatures.
            // It is for passing global methods into Cub functions that do not use the usual
            // language mechanism for higher order functions.
            sprintf "(&%s)" (print_method tag)

        // Value tuple cases
        | TyIndexVT(v,i,_) -> sprintf "(%s.tup%s)" (codegen v) (codegen i)
        | TyVT(l,t) -> 
            tuple_def_proc t (fun _ -> ())
            List.mapi (fun i x -> sprintf ".tup%i = %s" i (codegen x)) l
            |> String.concat ", "
            |> sprintf "{ %s }"
            
        // Array cases
        | TyIndexArray(ar,i,_) ->
            let index = 
                let ar_sizes =
                    match get_type ar with
                    | ArrayT(sizes,_) -> sizes
                    | _ -> failwith "impossible"
                let rec loop = function
                    | None, s :: sx, i :: ix ->
                        loop (Some(sprintf "(%s) * %s" (codegen i) (print_tyv s)),sx,ix)
                    | None, [], [i] ->
                        string i
                    | Some p, s :: sx, i :: ix ->
                        loop (Some(sprintf "(%s + (%s)) * %s" p (codegen i) (print_tyv s)),sx,ix)
                    | Some p, [], [i] ->
                        sprintf "%s + (%s)" p (codegen i)
                    | _ -> failwith "invalid state"
                loop (None,ar_sizes,i)
            sprintf "(%s).[%s]" (codegen ar) index

        | TyCreateArray _ -> failwith "This expression should never appear in isolation."

        // Cuda kernel constants
        | TyThreadIdxX -> "threadIdx.x"
        | TyThreadIdxY -> "threadIdx.y"
        | TyThreadIdxZ -> "threadIdx.z"
        | TyBlockIdxX -> "blockIdx.x"
        | TyBlockIdxY -> "blockIdx.y"
        | TyBlockIdxZ -> "blockIdx.z"
        | TyBlockDimX -> "blockDim.x"
        | TyBlockDimY -> "blockDim.y"
        | TyBlockDimZ -> "blockDim.z"
        | TyGridDimX -> "gridDim.x"
        | TyGridDimY -> "gridDim.y"
        | TyGridDimZ -> "gridDim.z"
   
        // Primitive operations on expressions.
        | TyAdd (x,y,_) -> sprintf "(%s + %s)" (codegen x) (codegen y)
        | TySub (x,y,_) -> sprintf "(%s - %s)" (codegen x) (codegen y)
        | TyMult (x,y,_) -> sprintf "(%s * %s)" (codegen x) (codegen y)
        | TyDiv (x,y,_) -> sprintf "(%s / %s)" (codegen x) (codegen y)
        | TyMod (x,y,_) -> sprintf "(%s %% %s)" (codegen x) (codegen y)
        | TyLT (x,y) -> sprintf "(%s < %s)" (codegen x) (codegen y)
        | TyLTE (x,y) -> sprintf "(%s <= %s)" (codegen x) (codegen y)
        | TyEQ (x,y) -> sprintf "(%s == %s)" (codegen x) (codegen y)
        | TyGT (x,y) -> sprintf "(%s > %s)" (codegen x) (codegen y)
        | TyGTE (x,y) -> sprintf "(%s >= %s)" (codegen x) (codegen y)
        | TyLeftShift (x,y,_) -> sprintf "(%s << %s)" (codegen x) (codegen y)
        | TyRightShift (x,y,_) -> sprintf "(%s >> %s)" (codegen x) (codegen y)
        | TySyncthreads -> state "syncthreads();"; ""
        | TyShuffleXor (x,y,_) -> sprintf "cub::ShuffleXor(%s, %s)" (codegen x) (codegen y)
        | TyShuffleUp (x,y,_) -> sprintf "cub::ShuffleUp(%s, %s)" (codegen x) (codegen y)
        | TyShuffleDown (x,y,_) -> sprintf "cub::ShuffleDown(%s, %s)" (codegen x) (codegen y)
        | TyShuffleIndex (x,y,_) -> sprintf "cub::ShuffleIndex(%s, %s)" (codegen x) (codegen y)
        | TyLog (x,_) -> sprintf "log(%s)" (codegen x)
        | TyExp (x,_) -> sprintf "exp(%s)" (codegen x)
        | TyTanh (x,_) -> sprintf "tanh(%s)" (codegen x)
        | TyNeg (x,_) -> sprintf "(-%s)" (codegen x)
        // Mutable operations.
        | TyMSet (a,b,e,_) ->
            sprintf "%s = %s;" (print_tyv a) (codegen b) |> state
            codegen e
        | TyAtomicAdd (a,b,_) ->
            sprintf "atomicAdd(&(%s),%s)" (codegen a) (codegen b)
        // Loops
        | TyWhile (cond,body,e,_) -> 
            sprintf "while (%s) {" (codegen cond) |> state
            enter <| fun _ -> codegen body
            "}" |> state
            codegen e

    let print_method (tag,_) (explicit_args,body,implicit_args) = 
        let check_valid_arg tag args =
            if List.exists (fun (_,_,t) -> t = UnitT) args then
                failwithf "UnitT arguments are not allowed in method calls. Tag=%i, Args: %A" tag args
            else
                args

        let method_name = print_method tag
        let args = 
            Set.toList implicit_args @ explicit_args
            |> check_valid_arg tag
            |> List.map print_tyv_with_type
            |> String.concat ", "
        sprintf "%s %s(%s) {" (print_type (get_type body)) method_name args |> state

        enter' <| fun _ ->
            match codegen body with
            | "" -> ()
            | s -> sprintf "return %s;" s |> state

        "}" |> state

    try
        for x in imemo do print_method x.Key x.Value
        process_statements program |> Succ
    with e -> Fail e.Message


let eval x = 
    match typecheck0 x with
    | Succ imemo -> print_method_dictionary imemo
    | Fail er -> Fail er

printfn "%A" (eval term4)
