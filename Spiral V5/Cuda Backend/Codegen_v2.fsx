﻿#load "Typechecker_v6c.fsx"
open Typechecker_v6c
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

    let print_array_type = function
        | UnitT -> failwith "Void types should not be inside local arrays."
        | UInt32T -> "unsigned int"
        | UInt64T -> "unsigned long long int"
        | Int32T -> "int"
        | Int64T -> "long long int"
        | Float32T -> "float"
        | Float64T -> "double"
        | BoolT -> "int"
        | VVT t -> tuple_def_proc t print_tuple
        | LocalArrayT (_,t) | SharedArrayT (_,t) | GlobalArrayT (_,t) -> 
            // The only reason is really because C syntax is such a pain in the ass.
            failwith "Arrays should not be inside other arrays."
        | TagT _ -> failwith "Can't print tagged types."

    let rec print_simple_type = function
        | UnitT -> "void"
        | UInt32T -> "const unsigned int"
        | UInt64T -> "const unsigned long long int"
        | Int32T -> "const int"
        | Int64T -> "const long long int"
        | Float32T -> "const float"
        | Float64T -> "const double"
        | BoolT -> "const int"
        | VVT t -> sprintf "const %s" (tuple_def_proc t print_tuple)
        | GlobalArrayT (_,t) -> sprintf "%s *" (print_array_type t)
        | TagT _ -> failwith "Can't print tagged types."
        | x -> failwithf "The type %A can't be printed here." x

    let print_tyv (tag,_) = sprintf "var_%i" tag
    let print_tyv_with_type (_,ty as v) = sprintf "%s %s" (print_simple_type ty) (print_tyv v)
    let print_method tag = sprintf "method_%i" tag

    let rec print_array is_shared typ v ar_sizes =   
        let typ = print_array_type typ
        let nam = print_tyv v
        let dim =
            List.map (codegen >> sprintf "[%s]") ar_sizes
            |> String.concat ""
        
        match is_shared with
        | true -> sprintf "__shared__ %s %s%s;" typ nam dim |> state
        | false -> sprintf "%s %s%s;" typ nam dim |> state

    and print_methodcall x = filter_simple_vars_template (snd >> codegen) x
    and codegen = function
        | TySeq(seq,rest,_) ->
            List.map codegen seq
            |> List.forall ((=) "")
            |> fun r -> if r then () else failwith "In TySeq the sequences should all be statements."
            codegen rest
        | TyV x -> print_tyv x
        | TyIf(cond,tr,fl,_) -> // If statements will aways be hoisted into methods in this language.
            sprintf "if (%s) {" (codegen cond) |> state
            enter <| fun _ -> sprintf "return %s;" (codegen tr)
            "}" |> state
            "else {" |> state
            enter <| fun _ -> sprintf "return %s;" (codegen fl)
            "}" |> state
            ""
        | TyLet((_,LocalArrayT(_,typ) as v),TyCreateLocalArray(ar_sizes,_)) ->
            print_array false typ v ar_sizes
            ""
        | TyLet((_,SharedArrayT(_,typ) as v),TyCreateSharedArray(ar_sizes,_)) ->
            print_array true typ v ar_sizes
            ""
        | TyLet(_,(TyUnit | Inlineable' _ | Method' _)) -> ""
        | Inlineable' _ | Method' _ -> failwith "Inlineable' and Method' should never appear in isolation."
        | TyLet((_,t),b) when t = UnitT ->
            sprintf "%s;" (codegen b) |> state
            ""
        | TyLet(tyv,b) ->
            sprintf "%s = %s;" (print_tyv_with_type tyv) (codegen b) |> state
            ""
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
        | TyIndexVV(v,i,_) -> sprintf "(%s.tup%s)" (codegen v) (codegen i)
        | TyVV(l,(VVT t)) -> 
            tuple_def_proc t (fun _ -> ())
            List.mapi (fun i x -> sprintf ".tup%i = %s" i (codegen x)) l
            |> String.concat ", "
            |> sprintf "{ %s }"
        | TyVV(l,_) -> failwith "The type of TyVT should always be VTT."

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
        | TyMSet (a,b) ->
            sprintf "%s = %s;" (codegen a) (codegen b) |> state
            ""
        | TyAtomicAdd (a,b,_) ->
            sprintf "atomicAdd(&(%s),%s)" (codegen a) (codegen b)
        // Loops
        | TyWhile (cond,body) -> 
            sprintf "while (%s) {" (codegen cond) |> state
            enter <| fun _ -> codegen body
            "}" |> state
            ""
        | TyCubBlockReduce(ins, TyMethodCall((tag,_),_,_),num_valid,t) ->
            match num_valid with
            | Some num_valid -> 
                sprintf "cub::BlockReduce<%s,blockDim.x>(%s,%s,%s)" 
                    (print_simple_type t) (codegen ins) (print_method tag) (codegen num_valid)
            | None -> sprintf "cub::BlockReduce<%s,blockDim.x>(%s,%s)" (print_simple_type t) (codegen ins) (print_method tag)
        | TyCubBlockReduce(_,_,_,_) -> failwith "impossible"

    let print_tuple_defintion tys tag =
        sprintf "struct %s {" (print_tuple tag) |> state
        enter <| fun _ -> List.iteri (fun i x -> sprintf "%s tup%i;" (print_simple_type x) i |> state) tys; ""
        "}" |> state

    let print_method prefix (tag,_) (explicit_args,body,implicit_args) = 
        let check_valid_arg tag args =
            if List.exists (fun (_,t) -> t = UnitT) args then
                failwithf "UnitT arguments are not allowed in method calls. Tag=%i, Args: %A" tag args
            else
                args

        let method_name = print_method tag
        let args = 
            Set.toList implicit_args @ explicit_args
            |> check_valid_arg tag
            |> List.map print_tyv_with_type
            |> String.concat ", "
        sprintf "%s %s %s(%s) {" prefix (print_simple_type (get_type body)) method_name args |> state

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
    with e -> Fail (e.Message, e.StackTrace)

let eval body inputs macros = 
    match typecheck body inputs macros with
    | Succ imemo -> print_method_dictionary imemo
    | Fail er -> Fail er

let eval0 body = eval body [] []

let while_ cond body rest = While(cond,body,rest)
let s l fin = List.foldBack (fun x rest -> x rest) l fin

let dref x = IndexArray(x,[LitInt 0])
let cref x = l (V "ref") (CreateLocalArray([LitInt 1],x)) (MSet(dref (V "ref"),x,V "ref"))
    
let for' init cond body =
    l (V "init") (cref init)
        (while_ (ap cond (dref <| V "init"))
            (MSet(dref (V "init"), ap body (dref <| V "init"),LitUnit)) (dref <| V "init"))

let for_ init cond body =
    l (V "") (for' init cond body)

let mset out in_ rest = MSet(out,in_,rest)

let map_module =
    s [ l (VV [V "n"]) (V "n")
        l (V "stride") (GridDimX*BlockDimX)
        l (V "i") (BlockIdxX * BlockDimX + ThreadIdxX)
        for_ (V "i") 
            (inl (V "i") (V "i" .< V "n"))
            (inl (V "i") 
                (l (V "") (ap (V "map_op") (V "i"))
                    (V "i" + V "stride")))
        ] LitUnit

let map_redo_map_module =
    s [ l (V "stride") (GridDimX*BlockDimX)
        l (V "i") (BlockIdxX * BlockDimX + ThreadIdxX)
        l (VV [V "n"]) (V "n")
        l (VV [V ""; V "value"])
            (for' (VV [V "i";ap (V "map_load_op") (V "i")])
                (inl (VV [V "i"; V ""]) (V "i" .< V "n"))
                (inl (VV [V "i"; V "value"]) 
                    (VV [V "i" + V "stride"; ap (V "reduce_op") (VV [V "value";ap (V "map_load_op") (V "i")])])))
        l (V "result") (CubBlockReduce(V "value",V "reduce_op",None))
        l (V "") (If(ThreadIdxX .= LitInt 0, ap (V "map_store_op") (V "result"), LitUnit))
        ] LitUnit
    

let map_1_1 = 
    let n = get_tag(),Int32T
    let in_ = get_tag(),GlobalArrayT([n],Float32T)
    let out_ = get_tag(),GlobalArrayT([n],Float32T)

    let args = ["n",[n];"ins",[in_];"outs",[out_]]
    let map_op = 
        inl (V "i")
            (s [l (VV [V "in1"]) (V "ins")
                l (VV [V "out1"]) (V "outs")
                mset (VV [IndexArray(V "out1",[V "i"])]) (VV [IndexArray(V "in1",[V "i"])])
                ] LitUnit)

    eval map_module args ["map_op",map_op]

let map_redo_map_1_1 = 
    let n = get_tag(),Int32T
    let in_ = get_tag(),GlobalArrayT([n],Float32T)
    let out_ = get_tag(),GlobalArrayT([n],Float32T)

    let args = ["n",[n];"ins",[in_];"outs",[out_]]
    let map_load_op =
        inl (V "i")
            (l (VV [V "in1"]) (V "ins")
                (IndexArray(V "in1",[V "i"])))
    let reduce_op = 
        meth (VV [V "a"; V "b"])
            (V "a" + V "b")
    let map_store_op =
        inl (V "value")
            (s [l (VV [V "out1"]) (V "outs")
                l (V "") (AtomicAdd(dref (V "out1"),V "value"))
                ] LitUnit)

    eval map_redo_map_module args ["map_load_op",map_load_op;"reduce_op",reduce_op;"map_store_op",map_store_op]

printfn "%A" map_1_1
printfn "%A" map_redo_map_1_1

