﻿#load "Typechecker_v6b.fsx"
open Typechecker_v6b
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
        enter' (fun _ -> 
            match f() with
            | "" -> ()
            | s -> state s)

    let tuple_definitions = Dictionary(HashIdentity.Structural)
    let tuple_def_proc t f = 
        match tuple_definitions.TryGetValue t with
        | true, v -> f v
        | false, _ ->
            let v = get_tag()
            tuple_definitions.Add(t,v)
            f v

    let print_tuple v = sprintf "tuple_%i" v

    let rec print_type = function
        | UnitT -> "void"
        | UInt32T -> "unsigned int"
        | UInt64T -> "unsigned long long int"
        | Int32T -> "int"
        | Int64T -> "long long int"
        | Float32T -> "float"
        | Float64T -> "double"
        | BoolT -> "int"
        | VTT t -> tuple_def_proc t print_tuple
        | NominalT x -> x
        | LocalArrayT (_,t) | GlobalArrayT (_,t) -> sprintf "%s *" (print_type t)
        | SharedArrayT (_,t) -> sprintf "__shared__ %s *" (print_type t)
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
        | TyLet(tyv,(TyCreateSharedArray(ar_sizes,_) | TyCreateLocalArray(ar_sizes,_)),e,_) ->
            let dims =
                List.map (codegen >> sprintf "[%s]") ar_sizes
                |> String.concat ""
            sprintf "%s%s;" (print_tyv_with_type tyv) dims |> state
            codegen e
        | TyLet((_,_,UnitT),_,e,_) -> codegen e
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

        // Value tuple cases
        | TyIndexVT(v,i,_) -> sprintf "(%s.tup%s)" (codegen v) (codegen i)
        | TyVT(l,(VTT t)) -> 
            tuple_def_proc t (fun _ -> ())
            List.mapi (fun i x -> sprintf ".tup%i = %s" i (codegen x)) l
            |> String.concat ", "
            |> sprintf "{ %s }"
        | TyVT(l,_) -> failwith "The type of TyVT should always be VTT."

        // Array cases
        | TyIndexArray(ar,i,_) as ar' ->
            let index = 
                let ar_sizes =
                    match get_type ar with
                    | LocalArrayT(sizes,_) | SharedArrayT(sizes,_) | GlobalArrayT(sizes,_) -> sizes
                    | _ -> failwith "impossible"
                let rec loop x =
                    match x with
                    | None, s :: sx, i :: ix ->
                        loop (Some(sprintf "(%s) * %s" (codegen i) (print_tyv s)),sx,ix)
                    | None, [], [i] ->
                        codegen i
                    | Some p, s :: sx, i :: ix ->
                        loop (Some(sprintf "(%s + (%s)) * %s" p (codegen i) (print_tyv s)),sx,ix)
                    | Some p, [], [i] ->
                        sprintf "%s + (%s)" p (codegen i)
                    | _ -> failwith "invalid state"
                loop (None,List.tail ar_sizes,i)
            sprintf "%s.[%s]" (codegen ar) index

        | TyCreateSharedArray _ | TyCreateLocalArray _ -> failwith "This expression should never appear in isolation."

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
            sprintf "%s = %s;" (codegen a) (codegen b) |> state
            codegen e
        | TyAtomicAdd (a,b,_) ->
            sprintf "atomicAdd(&(%s),%s)" (codegen a) (codegen b)
        // Loops
        | TyWhile (cond,body,e,_) -> 
            sprintf "while (%s) {" (codegen cond) |> state
            enter <| fun _ -> codegen body
            "}" |> state
            codegen e
        | TyCubBlockReduce(TyMethodCall((tag,_),_,_),num_valid,t) ->
            match num_valid with
            | Some num_valid -> sprintf "cub::BlockReduce<%s,blockDim.x>(%s,%s)" (print_type t) (print_method tag) (codegen num_valid)
            | None -> sprintf "cub::BlockReduce<%s,blockDim.x>(%s)" (print_type t) (print_method tag)
        | TyCubBlockReduce(_,_,_) -> failwith "impossible"

    let print_tuple_defintion tys tag =
        sprintf "struct %s {" (print_tuple tag) |> state
        enter <| fun _ -> List.iteri (fun i x -> sprintf "%s tup%i;" (print_type x) i |> state) tys; ""
        "}" |> state

    let print_method prefix (tag,_) (explicit_args,body,implicit_args) = 
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
        sprintf "%s %s %s(%s) {" prefix (print_type (get_type body)) method_name args |> state

        enter' <| fun _ ->
            match codegen body with
            | "" -> ()
            | s -> sprintf "return %s;" s |> state

        "}" |> state

    try
        let min = imemo |> Seq.fold (fun s kv -> min (fst kv.Key) s) System.Int64.MaxValue
        for x in imemo do 
            let prefix = if fst x.Key = min then "__global__" else "__device__"
            print_method prefix x.Key x.Value
        for x in tuple_definitions do print_tuple_defintion x.Key x.Value
        process_statements program |> Succ
    with e -> Fail e.Message

let eval env_adder body_conv arg_conv inputs body = 
    match typecheck env_adder body_conv arg_conv inputs body with
    | Succ imemo -> print_method_dictionary imemo
    | Fail er -> Fail er

let eval0 body = eval stan_env_adder stan_body_conv stan_arg_conv [] body

let while_ cond body rest = While(cond,body,rest)
let s l fin = List.foldBack (fun x rest -> x rest) l fin

let for_ init cond body rest =
    s [l (V "init") init
       while_ (cond (V "init")) 
            (MSet(V "init", body (V "init"),LitUnit))] rest

let map_env_adder (n,ins,outs) m =
    let f l m = stan_env_adder l m
    f [n] m |> f ins |> f outs
let map_body_conv (n,ins,outs) = 
    let f x = stan_body_conv x
    (f [n], f ins, f outs)
let map_arg_conv (n,ins,outs) =
    let f x = stan_arg_conv x
    MCVV [f [n]; f ins; f outs]

let map_module f (VV [n], ins, outs) =
    for_ (BlockIdxX * BlockDimX + ThreadIdxX) 
        (fun x -> x .< n)
        (f ins outs) LitUnit

printfn "%A" (eval0 term4)

let map_1_1 = 
    let n = get_tag(),"n",Int32T
    let in_ = get_tag(),"in",GlobalArrayT([n],Float32T)
    let out_ = get_tag(),"out",GlobalArrayT([n],Float32T)
    eval map_env_adder map_body_conv map_arg_conv 
        (n,[in_],[out_])
        (map_module (fun (VV [in_]) (VV [out_]) i -> MSet(VV [IndexArray(in_,[i])],VV [IndexArray(out_,[i])],i + GridDimX * BlockDimX)))
        
printfn "%A" map_1_1

