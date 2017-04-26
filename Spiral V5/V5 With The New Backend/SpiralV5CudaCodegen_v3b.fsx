#load "SpiralV5CudaTypechecker_v7c'.fsx"
open SpiralV5CudaTypechecker_v7c'
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

type CodegenChannels =
| Main = 0
| Code = 1
| TupleDefinitions = 2

let print_method_dictionary (imemo: MethodImplDict) =
    let program = Array.init 3 <| fun _ -> ResizeArray()
    let mutable channel = CodegenChannels.Main

    let cur_program () = program.[int channel]
    let with_channel i f =
        let x = channel
        channel <- i
        let t = f()
        channel <- x
        t

    /// Clears the channel a afterwards.
    let add_channels_a_to_b a b = // Yeah, I know this is not best programming practice, but tuples do need to come first.
        let a = program.[a] 
        let b = program.[b]
        b.AddRange a
        a.Clear()

    let add_channels_a_to_main x = add_channels_a_to_b (int x) (int CodegenChannels.Main)

    let state x = program.[int channel].Add <| Statement x
    let enter' f = 
        program.[int channel].Add Indent
        f()
        program.[int channel].Add Dedent
    let enter f = 
        enter' <| fun _ -> 
            match f() with
            | "" -> ()
            | s -> state s

    let tuple_definitions = Dictionary(HashIdentity.Structural)
    let tuple_def_proc t = 
        match tuple_definitions.TryGetValue t with
        | true, v -> v
        | false, _ ->
            let v = get_tag()
            tuple_definitions.Add(t,v)
            v

    let print_tuple v = sprintf "tuple_%i" v

    let case_array_in_array _ = 
        // The only reason is really because C syntax is such a pain in the ass.
        // I do not want to deal with casting void pointers here.
        failwith "Arrays should not be inside other arrays."
    let rec print_type array_case = function
        | UnitT -> "void"
        | PrimT x -> 
            match x with
            | UInt8T -> "unsigned char"
            | UInt16T -> "unsigned short"
            | UInt32T -> "unsigned int"
            | UInt64T -> "unsigned long long int"
            | Int8T -> "char"
            | Int16T -> "short"
            | Int32T -> "int"
            | Int64T -> "long long int"
            | Float32T -> "float"
            | Float64T -> "double"
            | BoolT -> "int"
        | VVT t -> tuple_def_proc t |> print_tuple
        | LocalArrayT (_,t) | SharedArrayT (_,t) | GlobalArrayT (_,t) -> array_case t
        | InlineableT _ | MethodT _ -> failwith "Can't print tagged types."

    let print_simple_type = print_type (fun t -> sprintf "%s *" (print_type case_array_in_array t))
    let print_array_type = print_type case_array_in_array

    let print_tyv (tag,_) = sprintf "var_%i" tag
    let print_tyv_with_type (_,ty as v) = sprintf "%s %s" (print_simple_type ty) (print_tyv v)
    let print_method tag = sprintf "method_%i" tag

    let rec print_array_declaration is_shared typ v ar_sizes =   
        let typ = print_array_type typ
        let nam = print_tyv v
        let dim =
            if List.isEmpty ar_sizes then "1"
            else
                List.map (codegen >> sprintf "%s") ar_sizes
                |> String.concat " * "
        
        match is_shared with
        | true -> sprintf "__shared__ %s %s[%s];" typ nam dim |> state
        | false -> sprintf "%s %s[%s];" typ nam dim |> state

    and print_methodcall x = List.map codegen (filter_vars is_arg x)
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
        | TyLet((_,t) as v,TyArrayCreate _, rest,_) ->
            match t with
            | LocalArrayT(TyVV(ar_sizes,_),typ) -> print_array_declaration false typ v ar_sizes
            | SharedArrayT(TyVV(ar_sizes,_),typ) -> print_array_declaration true typ v ar_sizes
            | LocalArrayT(ar_sizes,typ) -> print_array_declaration false typ v [ar_sizes]
            | SharedArrayT(ar_sizes,typ) -> print_array_declaration true typ v [ar_sizes]
            | _ -> failwith "impossible"
            codegen rest
        | TyLet(_,(TyUnit | TyType _),rest,_) -> codegen rest
        | TyType _ -> failwith "Inlineable and Method (TyType) should never appear in isolation."
        | TyLet((_,t),b,rest,_) when t = UnitT ->
            sprintf "%s;" (codegen b) |> state
            codegen rest
        | TyLet(tyv,b,rest,_) ->
            sprintf "%s = %s;" (print_tyv_with_type tyv) (codegen b) |> state
            codegen rest
        | TyUnit -> ""
        | TyLitUInt32 x -> string x 
        | TyLitUInt64 x -> string x 
        | TyLitInt32 x -> string x
        | TyLitInt64 x -> string x
        | TyLitFloat32 x -> string x
        | TyLitFloat64 x -> string x
        | TyLitBool x -> if x then "1" else "0"
        | TyMethodCall(mkey,call,t) ->
            let _,_,tag,implicit_args = imemo.[mkey]
            let implicit_args = Set.toList implicit_args |> List.map print_tyv
            let explicit_args = print_methodcall call
            let args = implicit_args @ explicit_args |> String.concat ", "
            let method_name = print_method tag
            sprintf "%s(%s)" method_name args

        // Value tuple cases
        | TyVVIndex(v,i,_) -> sprintf "%s.tup%s" (codegen v) (codegen i)
        | TyVV(l,(VVT t)) -> 
            List.choose (fun x -> let x = codegen x in if x = "" then None else Some x) l
            |> String.concat ", "
            |> sprintf "make_tuple_%i(%s)" (tuple_def_proc t)
        | TyVV(l,_) -> failwith "The type of TyVT should always be VTT."

        // Array cases
        | TyArrayIndex(ar,i,_) as ar' ->
            let index = 
                let ar_sizes =
                    match get_type ar with
                    | LocalArrayT(TyVV(sizes,_),_) | SharedArrayT(TyVV(sizes,_),_) | GlobalArrayT(TyVV(sizes,_),_) -> sizes
                    | LocalArrayT(sizes,_) | SharedArrayT(sizes,_) | GlobalArrayT(sizes,_) -> [sizes]
                    | _ -> failwith "impossible"
                let rec loop x =
                    match x with
                    | None, s :: sx, i :: ix ->
                        loop (Some(sprintf "(%s) * %s" (codegen i) (codegen s)),sx,ix)
                    | None, [], [i] ->
                        codegen i
                    | Some p, s :: sx, i :: ix ->
                        loop (Some(sprintf "(%s + (%s)) * %s" p (codegen i) (codegen s)),sx,ix)
                    | Some p, [], [i] ->
                        sprintf "%s + (%s)" p (codegen i)
                    | _ -> failwith "invalid state"
                if i.IsEmpty = false then
                    loop (None,List.tail ar_sizes,i)
                else 
                    "0"
            sprintf "%s[%s]" (codegen ar) index

        | TyArrayCreate _ -> failwith "This expression should never appear in isolation."

        // Cuda kernel constants
        | TyThreadIdxX -> "threadIdx.x"
        | TyThreadIdxY -> "threadIdx.y"
        | TyThreadIdxZ -> "threadIdx.z"
        | TyBlockIdxX -> "blockIdx.x"
        | TyBlockIdxY -> "blockIdx.y"
        | TyBlockIdxZ -> "blockIdx.z"
   
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
        | TyMSet (a,b,rest,_) ->
            sprintf "%s = %s;" (codegen a) (codegen b) |> state
            codegen rest
        | TyAtomicAdd (a,b,_) ->
            sprintf "atomicAdd(&(%s),%s)" (codegen a) (codegen b)
        // Loops
        | TyWhile (cond,body,rest,_) -> 
            sprintf "while (%s) {" (codegen cond) |> state
            enter <| fun _ -> codegen body
            "}" |> state
            codegen rest
        | TyCubBlockReduce(dim, ins, TyMethodCall(mkey,_,_),num_valid,t) ->
            let _,_,tag,_ = imemo.[mkey]
            match num_valid with
            | Some num_valid -> 
                sprintf "cub::BlockReduce<%s,%s>().Reduce(%s,%s,%s)"  // TODO: Do not forget to change the template parameter.
                    (print_simple_type t) (codegen dim) (codegen ins) (print_method tag) (codegen num_valid)
            | None -> 
                sprintf "cub::BlockReduce<%s,%s>().Reduce(%s,%s)" 
                    (print_simple_type t) (codegen dim) (codegen ins) (print_method tag)
        | TyCubBlockReduce _ -> failwith "impossible"

    let print_tuple_defintion tys tag =
        let tuple_name = print_tuple tag
        sprintf "struct %s {" tuple_name |> state
        enter <| fun _ -> List.iteri (fun i ty -> 
            match ty with
            | UnitT -> ()
            | _ -> sprintf "%s tup%i;" (print_simple_type ty) i |> state) tys; ""
        "};" |> state
        sprintf "__device__ __forceinline__ %s make_tuple_%i(%s){" 
            tuple_name
            tag
            (List.mapi (fun i ty -> 
                match ty with
                | UnitT -> ""
                | _ -> sprintf "%s tup_arg%i" (print_simple_type ty) i) tys
             |> List.filter ((<>) "") 
             |> String.concat ", ")
        |> state
        enter' <| fun _ ->
            sprintf "%s tmp;" tuple_name |> state
            List.iteri (fun i _ -> sprintf "tmp.tup%i = tup_arg%i;" i i |> state) tys
            "return tmp;" |> state
        "}" |> state

    let print_method is_main (explicit_args,body,tag,implicit_args) = 
        let prefix = if is_main then "__global__" else "__device__"
        let method_name = if is_main then "MainKernel" else print_method tag
        let args = 
            Set.toList implicit_args @ explicit_args
            |> List.map print_tyv_with_type
            |> String.concat ", "
        sprintf "%s %s %s(%s) {" prefix (print_simple_type (get_type body)) method_name args |> state

        enter' <| fun _ ->
            match codegen body with
            | "" -> ()
            | s -> sprintf "return %s;" s |> state
        "}" |> state

    try
        """#include "cub/cub.cuh" """ |> state
        """extern "C" {""" |> state

        
        enter' <| fun _ ->
            with_channel CodegenChannels.Code <| fun _ ->
                let get_tag (_,_,tag,_) = tag
                let min = imemo |> Seq.fold (fun s kv -> 
                    max (get_tag kv.Value) s) System.Int64.MinValue
                for x in imemo do 
                    let is_main = get_tag x.Value = min
                    print_method is_main x.Value

            with_channel CodegenChannels.TupleDefinitions <| fun _ ->
                for x in tuple_definitions do print_tuple_defintion x.Key x.Value

            // I am just swapping the positions so the tuple definitions come first in the printed code.
            // Unfortunately, getting the definitions requires a pass through the AST first
            // so I can't just print them at the start.
            add_channels_a_to_main CodegenChannels.TupleDefinitions
            add_channels_a_to_main CodegenChannels.Code
        
        "}" |> state
        
        cur_program () |> process_statements |> Succ
    with e -> Fail (e.Message, e.StackTrace)

let eval body (inputs, dims) = 
    match typecheck dims body inputs with
    | Succ imemo -> print_method_dictionary imemo
    | Fail er -> Fail er

let eval0 body = eval body (VV [], default_dims)

let map_forward_setter = inl' [S' "array_index"; SS [S' "result"; S' "out"]] (MSet(ap (V "array_index") (V "out"), V "result"))
let map_backward_setter = inl' [S' "array_index"; SS [S' "result"; S' "out"]] (AtomicAdd(ap (V "array_index") (V "out"), V "result"))
let map_redo_map_backward_setter = inl' [S' "array_index"; SS [S' "result"; S' "out"]] (AtomicAdd(V "out", V "result"))
let cuda_module_map_template =
    inl (S "setter")
        (meth (SS [S "map_op"; S' "n"; S "ins"; S "outs"])
            (s [l (S "stride") (GridDimX*BlockDimX)
                l (S "i") (BlockIdxX * BlockDimX + ThreadIdxX)
                for_ (V "i") 
                    (inl (S "i") (V "i" .< V "n"))
                    (inl (S "i") 
                        (l E 
                           (s  [l (S "array_index") (inl (S "x") (ArrayIndex(V "x",V "i")))
                                l (S "f") (inl (S' "in") (ap (V "map_op") (VV [V "i"; ap (V "array_index") (V "in")])))
                                l (S "results") (ap cuda_map (VV [V "f"; V "ins"]))
                                l (S "results_outs") (VVZip(VV [V "results"; V "outs"]))
                                ] (ap cuda_map (VV [ap (V "setter") (V "array_index"); V "results_outs"])))
                            (V "i" + V "stride")))
                ] B))

let cuda_module_map_forward = ap cuda_module_map_template map_forward_setter
let cuda_module_map_backward = ap cuda_module_map_template map_backward_setter

let cuda_module_map_redo_map_template =
    meth (SS [S "map_load_op";S "reduce_op";S "map_store_op"; S' "n"; S "ins"; S "outs"])
        (s [l (S "stride") (GridDimX*BlockDimX)
            l (S "i") (BlockIdxX * BlockDimX + ThreadIdxX)
            l (S "map_load_op") 
                (inl (S' "i") 
                    (l (S "ins") (ap cuda_map (VV [inl (S' "in") (ArrayIndex(V "in",V "i")); V "ins"]))
                    (ap (V "map_load_op") (VV [V "i"; V "ins"]))))
            l (S "map_store_op")
                (inl (S "results")
                     (s [l (S "x") (VVZip (VV [ap (V "map_store_op") (V "results"); V "outs"]))
                         l (S "f") (inl (SS [S' "result"; S' "out"]) (MSet(ArrayIndex(V "out",BlockIdxX), V "result")))
                         l E (ap cuda_map (VV [V "f"; V "x"]))
                         ] B))
            l (S "inc") (inl (S "i") (V "i" + V "stride"))
            l (SS [E; S "value"])
                (for' (VV [ap (V "inc") (V "i"); ap (V "map_load_op") (V "i")])
                    (inl (SS [S "i"; E]) (V "i" .< V "n"))
                    (inl (SS [S "i"; S "value"]) 
                        (VV [ap (V "inc") (V "i"); ap (V "reduce_op") (VV [V "value";ap (V "map_load_op") (V "i")])])))
            l (S "results") (CubBlockReduce(V "value",meth (SS [S "a"; S "b"]) (ap (V "reduce_op") (VV [V "a"; V "b"])),None))
            l E (If(ThreadIdxX .= LitUInt64 0UL, ap (V "map_store_op") (V "results"), B))
            ] B)

let cuda_module_map_redo_map_forward = cuda_module_map_redo_map_template
let cuda_module_map_redo_map_backward = ap cuda_module_map_template map_redo_map_backward_setter

let cuda_module_map_redocol_map_template =
    meth (SS [S "map_load_op";S "reduce_op";S "map_store_op"; SS [S' "num_cols"; S' "num_rows"];S "ins";S "outs"])
        (s [l (S "map_load_op")
                (inl (SS [S' "col"; S' "row"]) 
                    (l (S "ins") (ap cuda_map (VV [inl (S' "in") (ArrayIndex(V "in",VV [V "col"; V "row"])); V "ins"]))
                    (ap (V "map_load_op") (VV [VV [V "col"; V "row"]; V "ins"]))))
            l (S "map_store_op")
                (inl (SS [S' "col"; S "results"])
                        (s [l (S "x") (VVZip (VV [ap (V "map_store_op") (V "results"); V "outs"]))
                            l (S "f") (inl (SS [S' "result"; S' "out"]) (MSet(ArrayIndex(V "out",V "col"), V "result")))
                            l E (ap cuda_map (VV [V "f"; V "x"]))
                            ] B))
            l (S "inc_col") (inl (S "col") (V "col" + GridDimX))
            l (S "inc_row") (inl (S "row") (V "row" + BlockDimX))
            for_ BlockIdxX
                (inl (S "col") (V "col" .< V "num_cols"))
                (inl (S "col")
                    (s [l (SS [E; S "value"]) 
                            (for' (VV [ap (V "inc_row") ThreadIdxX; ap (V "map_load_op") (VV [V "col"; ThreadIdxX])])
                                (inl (SS [S "row"; S ""]) (V "row" .< V "num_rows"))
                                (inl (SS [S "row"; S "value"])
                                    (VV [ap (V "inc_row") (V "row"); 
                                         ap (V "reduce_op") (VV [V "value"; ap (V "map_load_op") (VV [V "col"; V "row"])])])))
                        l (S "results") (CubBlockReduce(V "value",meth (SS [S "a"; S "b"]) (ap (V "reduce_op") (VV [V "a"; V "b"])),None))
                        l E (If(ThreadIdxX .= LitUInt64 0UL, ap (V "map_store_op") (VV [V "col"; V "results"]), B))
                        ] (ap (V "inc_col") (V "col"))))
            ] B)

let cuda_module_mapcol_template = // TODO: Revisit this after finishing the parser.
    inl (S "setter")
        (meth (SS [S "map_op"; SS [S' "num_cols"; S' "num_rows"]; S "outs_prim_adj"; S "ins_prim_adj"])
            (s [l (S "inc_col") (inl (S "col") (V "col" + GridDimX))
                l (S "inc_row") (inl (S "row") (V "row" + BlockDimX))
                l (S "array_index") (inl' [S' "i"; S' "x"] (ArrayIndex(V "x",V "i")))
                for_ BlockIdxX
                    (inl (S "col") (V "col" .< V "num_cols"))
                    (inl (S "col")
                        (s [l (S "outs_prim_adj") (ap cuda_map (VV [ap (V "array_index") (V "col"); V "outs_prim_adj"]))
                            for_ ThreadIdxX 
                                (inl (SS [S "row"]) (V "row" .< V "num_rows"))
                                (inl (SS [S "row"])
                                   (l E (s [l (SS [S "ins_prim"; S "ins_adj"]) (VVUnzip(V "ins_prim_adj"))
                                            l (S "ins_prim") (ap cuda_map (VV [ap (V "array_index") (VV [V "col"; V "row"]); V "ins_prim"]))
                                            l (S "f") (inl (SS [SS [S' "out_prim"; S' "out_adj"]; S' "in_prim"]) (ap (V "map_op") (VV [VV [V "col"; V "row"]; VV [V "out_prim"; V "out_adj"]; V "in_prim"])))
                                            l (S "results") (ap cuda_map (VV [V "f"; VVZip(VV [V "outs_prim_adj"; V "in_prim"])]))
                                            l (S "results_ins_adj") (VVZip(VV [V "results"; V "ins_adj"]))
                                            ] (ap cuda_map (VV [ap (V "setter") (ap (V "array_index") (VV [V "col"; V "row"])); V "results_ins_adj"])))
                                        (ap (V "inc_row") (V "row"))))
                            ] (ap (V "inc_col") (V "col"))))
                ] B))

let cuda_module_map_redocol_map_forward = cuda_module_map_redocol_map_template