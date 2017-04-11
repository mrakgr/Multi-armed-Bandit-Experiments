#load "Typechecker_v6e.fsx"
open Typechecker_v6e
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
        | UInt32T -> "unsigned int"
        | UInt64T -> "unsigned long long int"
        | Int32T -> "int"
        | Int64T -> "long long int"
        | Float32T -> "float"
        | Float64T -> "double"
        | BoolT -> "int"
        | VVT t -> tuple_def_proc t |> print_tuple
        | LocalArrayT (_,t) | SharedArrayT (_,t) | GlobalArrayT (_,t) -> array_case t
        | TagT _ -> failwith "Can't print tagged types."

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

    and print_methodcall x = List.map codegen (filter_simple_vars x)
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
        | TyLet((_,t) as v,TyCreateArray _, rest,_) ->
            match t with
            | LocalArrayT(ar_sizes,typ) -> print_array_declaration false typ v ar_sizes
            | SharedArrayT(ar_sizes,typ) -> print_array_declaration true typ v ar_sizes
            | _ -> failwith "impossible"
            codegen rest
        | TyLet(_,(TyUnit | Inlineable' _ | Method' _),rest,_) -> codegen rest
        | Inlineable' _ | Method' _ -> failwith "Inlineable' and Method' should never appear in isolation."
        | TyLet((_,t),b,rest,_) when t = UnitT ->
            sprintf "%s;" (codegen b) |> state
            codegen rest
        | TyLet(tyv,b,rest,_) ->
            sprintf "%s = %s;" (print_tyv_with_type tyv) (codegen b) |> state
            codegen rest
        | TyUnit -> ""
        | TyLitInt x -> string x
        | TyLitFloat x -> string x
        | TyLitBool x -> if x then "1" else "0"
        | TyMethodCall((tag,_ as mkey),call,t) ->
            let (_,_,implicit_args) = imemo.[mkey]
            let implicit_args = Set.toList implicit_args |> List.map print_tyv
            let explicit_args = print_methodcall call
            let args = implicit_args @ explicit_args |> String.concat ", "
            let method_name = print_method tag
            sprintf "%s(%s)" method_name args

        // Value tuple cases
        | TyIndexVV(v,i,_) -> sprintf "%s.tup%s" (codegen v) (codegen i)
        | TyVV(l,(VVT t)) -> 
            List.map (fun x -> codegen x) l
            |> String.concat ", "
            |> sprintf "make_tuple_%i(%s)" (tuple_def_proc t)
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
        | TyCubBlockReduce(dim, ins, TyMethodCall((tag,_),_,_),num_valid,t) ->
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
        enter <| fun _ -> List.iteri (fun i ty -> sprintf "%s tup%i;" (print_simple_type ty) i |> state) tys; ""
        "};" |> state
        sprintf "__device__ __forceinline__ %s make_tuple_%i(%s){" 
            tuple_name
            tag
            (List.mapi (fun i ty -> sprintf "%s tup_arg%i" (print_simple_type ty) i) tys
             |> String.concat ", ")
            |> state
        enter' <| fun _ ->
            sprintf "%s tmp;" tuple_name |> state
            List.iteri (fun i _ -> sprintf "tmp.tup%i = tup_arg%i;" i i |> state) tys
            "return tmp;" |> state
        "}" |> state

    let print_method is_main (tag,_) (explicit_args,body,implicit_args) = 
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
                let min = imemo |> Seq.fold (fun s kv -> min (fst kv.Key) s) System.Int64.MaxValue
                for x in imemo do 
                    let is_main = fst x.Key = min
                    print_method is_main x.Key x.Value

            with_channel CodegenChannels.TupleDefinitions <| fun _ ->
                for x in tuple_definitions do print_tuple_defintion x.Key x.Value

            // I am just swapping the positions so the tuple definitions come first.
            // Unfortunately, getting the definitions requires a pass through the AST first
            // so I can't just print them first.
            add_channels_a_to_main CodegenChannels.TupleDefinitions
            add_channels_a_to_main CodegenChannels.Code
        
        "}" |> state
        
        cur_program () |> process_statements |> Succ
    with e -> Fail (e.Message, e.StackTrace)

let eval dims body inputs = 
    match typecheck dims body inputs with
    | Succ imemo -> print_method_dictionary imemo
    | Fail er -> Fail er

let eval0 body = eval default_dims body (VV [])

let while_ cond body rest = While(cond,body,rest)
let s l fin = List.foldBack (fun x rest -> x rest) l fin

let dref x = IndexArray(x,[])
let cref x = l (V "ref") (CreateLocalArray([],x)) (MSet(dref (V "ref"),x,V "ref"))
    
let for' init cond body =
    l (V "init") (cref init)
        (while_ (ap cond (dref <| V "init"))
            (MSet(dref (V "init"), ap body (dref <| V "init"),B)) (dref <| V "init"))

let for_ init cond body =
    l B (for' init cond body)

let mset out in_ rest = MSet(out,in_,rest)

let map_module =
    meth (VV [V "map_op"; V "n";V "ins";V "outs"])
        (s [l (V "stride") (GridDimX*BlockDimX)
            l (V "i") (BlockIdxX * BlockDimX + ThreadIdxX)
            for_ (V "i") 
                (inl (V "i") (V "i" .< V "n"))
                (inl (V "i") 
                    (l B (ap (V "map_op") (VV [V "i";V "ins";V "outs"]))
                        (V "i" + V "stride")))
            ] B)

let map_redo_map_module =
    meth (VV [V "map_load_op";V "reduce_op";V "map_store_op"; V "n";V "ins";V "outs"])
        (s [l (V "stride") (GridDimX*BlockDimX)
            l (V "i") (BlockIdxX * BlockDimX + ThreadIdxX)
            l (VV [B; V "value"])
                (for' (VV [V "i";ap (V "map_load_op") (VV [V "i";V "ins"])])
                    (inl (VV [V "i"; V ""]) (V "i" .< V "n"))
                    (inl (VV [V "i"; V "value"]) 
                        (VV [V "i" + V "stride"; ap (V "reduce_op") (VV [V "value";ap (V "map_load_op") (VV [V "i";V "ins"])])])))
            l (V "result") (CubBlockReduce(V "value",V "reduce_op",None))
            l B (If(ThreadIdxX .= LitInt 0, ap (V "map_store_op") (VV [V "result"; V "outs"]), B))
            ] B)

let map_redocol_map_module =
    meth (VV [V "map_load_op";V "reduce_op";V "map_store_op"; VV [V "num_cols"; V "num_rows"];V "ins";V "outs"])
        (for_ BlockIdxX
            (inl (V "col") (V "col" .< V "num_cols"))
            (inl (V "col") 
                (s [l (VV [B; V "value"]) 
                        (for' (VV [ThreadIdxX; ap (V "map_load_op") (VV [VV [V "col"; ThreadIdxX]; V "ins"])])
                            (inl (VV [V "row";B]) (V "row" .< V "num_rows"))
                            (inl (VV [V "row";V "value"])
                                (VV [V "row" + BlockDimX; 
                                        ap (V "reduce_op") (VV [V "value"; ap (V "map_load_op") (VV [VV [V "col"; V "row"]; V "ins"])])])))
                    l (V "result") (CubBlockReduce(V "value",V "reduce_op",None))
                    l B (If(ThreadIdxX .= LitInt 0, ap (V "map_store_op") (VV [V "result"; V "col"; V "outs"]), B))
                    ] (V "col" + GridDimX)))
            B)
