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
        | TyLet(_,(TyVV([],_) | TyType _),rest,_) -> codegen rest
        | TyType _ -> failwith "Inlineable and Method (TyType) should never appear in isolation."
        | TyLet((_,t),b,rest,_) when t = VVT [] ->
            sprintf "%s;" (codegen b) |> state
            codegen rest
        | TyLet(tyv,b,rest,_) ->
            sprintf "%s = %s;" (print_tyv_with_type tyv) (codegen b) |> state
            codegen rest
        | TyVV([],_) -> ""
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
        | TyNEQ (x,y) -> sprintf "(%s != %s)" (codegen x) (codegen y)
        | TyGT (x,y) -> sprintf "(%s > %s)" (codegen x) (codegen y)
        | TyGTE (x,y) -> sprintf "(%s >= %s)" (codegen x) (codegen y)
        | TyAnd (x,y) -> sprintf "(%s && %s)" (codegen x) (codegen y)
        | TyOr (x,y) -> sprintf "(%s || %s)" (codegen x) (codegen y)
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

let map_forward_setter = "inl i (*result, *out) -> out.[i] <- result"
let map_backward_setter = "inl i (*result, *out) -> out.[i] <- out.[i] + result"

let cuda_modules =
    """
inl module_selector ->
    inl index_if_array i *in =
        typecase in with
        | Array(in,size) -> in.[i]
        | Array(in,()) -> in.[()]

    inl map_load_op ins map_op i = Tuple.Map (index_if_array i) ins |> map_op i

    fun map (setter, map_op, *n, ins, outs) =
        inl stride = Cuda.GridDimX * Cuda.BlockDimX
        inl map_load_op = map_load_op ins map_op
        
        fun rec loop i =
            if i < n then
                Tuple.ZipReg (outs,map_load_op ins) |> Tuple.Map (setter i)
                loop (i+stride)
        loop (Cuda.BlockIdxX * Cuda.BlockDimX + Cuda.ThreadIdxX)

    fun map_redo (map_op,(neutral_elem,reduce_op),*n,ins,outs) =
        inl stride = Cuda.GridDimX * Cuda.BlockDimX
        inl map_load_op = map_load_op ins map_op

        fun rec loop (i, value) =
            if i < n then loop (i + stride, reduce_op value (map_load_op i))
            else value
    
        inl results = 
            inl i = Cuda.BlockIdxX * Cuda.BlockDimX + Cuda.ThreadIdxX
            Cuda.CubBlockReduce(loop (i, neutral_elem), fun (a,b) -> reduce_op a b)

        if Cuda.ThreadIdxX = 0UL then 
            Tuple.ZipReg(outs,results)
            |> Tuple.Map (inl (*out,*result) -> out.[Cuda.BlockIdxX] <- result)
            ()

    fun map_redocol_map (map_op,(neutral_elem,reduce_op),map_store_op,(*num_cols,*num_rows),ins,outs) =
        inl map_load_op = map_load_op ins map_op

        fun rec loop_col col =
            let rec loop_row (row, value) = 
                if row < num_rows then loop_row (row + Cuda.BlockDimX, reduce_op value (map_load_op (col,row)))
                else value

            if col < num_cols then 
                inl results = Cuda.CubBlockReduce(loop_row (Cuda.ThreadIdxX, neutral_elem), fun (a,b) -> reduce_op a b)
                    
                if Cuda.ThreadIdxX = 0UL then 
                    Tuple.ZipReg(outs,map_store_op results)
                    |> Tuple.Map (inl (*out,*result) -> out.[col] <- result)
                    ()        
                loop_col (col + Cuda.GridDimX)
        loop_col Cuda.BlockIdxX

    fun mapcol (setter,map_op,(*num_cols,*num_rows),ins,outs) =
        inl map_load_op = map_load_op ins map_op

        fun rec loop_col col =
            inl ins = map_load_op col
            let rec loop_row row = 
                if row < num_rows then 
                    Tuple.ZipReg(outs, ins) |> Tuple.Map (setter (col,row))
                    loop_row (row + Cuda.BlockDimX)

            if col < num_cols then 
                loop_row Cuda.ThreadIdxX
                loop_col (col + Cuda.GridDimX)

        loop_col Cuda.BlockIdxX

    module_selector (map, map_redo, map_redocol_map, mapcol)
    """

let cuda_module_map_template = "fun map :: _ -> map"
let cuda_module_map_redo_template = "fun _ :: map_redo :: _ -> map_redo"
let cuda_module_map_redocol_map_template = "fun _ :: _ :: map_redocol_map :: _ -> map_redocol_map"
let cuda_module_mapcol_template = "fun _ :: _ :: _ :: mapcol :: _ -> mapcol"