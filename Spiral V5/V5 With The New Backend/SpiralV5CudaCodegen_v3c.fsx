﻿#load "SpiralV5Parser_v2a.fsx"

open SpiralV5Language_v8b
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
| Definitions = 2

let print_method_dictionary (imemo: MemoDict) =
    let program = Array.init 3 <| fun _ -> ResizeArray()
    let mutable channel = CodegenChannels.Main

    let get_tag =
        let mutable i = 0
        fun () -> i <- i+1; i

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

    let def_proc (d: Dictionary<_,_>) t = 
        match d.TryGetValue t with
        | true, v -> v
        | false, _ ->
            let v = get_tag()
            d.Add(t,v)
            v

    let tuple_definitions = d0()
    let tuple_tag x = def_proc tuple_definitions x
    let print_tuple' v = sprintf "tuple_%i" v
    let print_tuple t = tuple_tag t |> print_tuple'

    let closure_type_definitions = d0()
    let closure_type_tag x = def_proc closure_type_definitions x
    let print_closure_type' tag = sprintf "clo_type_%i" tag
    let print_closure_type typ = closure_type_tag typ |> print_closure_type'

    let case_array_in_array _ = 
        // The only reason is really because C syntax is such a pain in the ass.
        // I do not want to deal with casting void pointers here.
        failwith "Arrays should not be inside other arrays."
    let rec print_type array_case = function
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
            | StringT -> "char *"
        | ClosureT(a,r) -> print_closure_type (a,r)
        | VVT ([], _) -> "void"
        | VVT (t, _) -> print_tuple t
        | StructT(Array(_,_,t)) -> array_case t
        | StructT _ -> failwith "Can't struct types directly."
        | FunctionT _ -> failwith "Can't function types directly."

    let print_simple_type = print_type (fun t -> sprintf "%s *" (print_type case_array_in_array t))
    let print_array_type = print_type case_array_in_array

    let print_tyv (tag,_) = sprintf "var_%i" tag
    let print_tyv_with_type (tag,ty as v) = sprintf "%s %s" (print_simple_type ty) (print_tyv v)
    let print_method tag = sprintf "method_%i" tag

    let print_value = function
        | LitUInt32 x -> string x 
        | LitUInt64 x -> string x 
        | LitInt32 x -> string x
        | LitInt64 x -> string x
        | LitFloat32 x -> string x
        | LitFloat64 x -> string x
        | LitBool x -> if x then "1" else "0"
        | LitString x -> sprintf "\"%s\"" x

    let rec print_array a b =
        let ar_sizes =
            match a with
            | Array(_,sizes,_) -> sizes
            | _ -> failwith "impossible"

        let i = tuple_field b

        // Array cases
        let index = 
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
            if i.IsEmpty = false then loop (None,List.tail ar_sizes,i)
            else "0"
        sprintf "%s[%s]" (codegen a) index

    and print_array_declaration is_shared typ v ar_sizes =   
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

    and if_ cond tr fl t =
        let p, r =
            match t with
            | VVT([],_) -> 
                (fun x -> enter <| fun _ -> sprintf "%s;" (codegen x)), ""
            | _ -> 
                let r = get_tag() |> sprintf "if_var_%i"
                (fun x -> enter <| fun _ -> sprintf "%s = %s;" r (codegen x)), r
        sprintf "%s %s;" (print_simple_type t) r |> state
        sprintf "if (%s) {" (codegen cond) |> state
        p tr
        "} else {" |> state
        p fl
        "}" |> state
        r

    and codegen = function
        | TyVV([],_) | TyType _ -> ""
        | TyV v -> print_tyv v
        | TyOp(If,[cond;tr;fl],t) -> if_ cond tr fl t
        | TyLet(LetInvisible, _, _, rest, _) -> codegen rest
        | TyLet(_, v, TyOp(StructCreate, [Array(ar_typ,ar_sizes,typ)], _), rest, _) ->
            match ar_typ with
            | Local | Global -> print_array_declaration false typ v ar_sizes
            | Shared -> print_array_declaration true typ v ar_sizes
            codegen rest
        | TyLet(_,_,(TyVV([],_) | TyType _),rest,_) -> codegen rest
        | TyLet(_,(_,VVT([],_)),b,rest,_) -> sprintf "%s;" (codegen b) |> state; codegen rest
        | TyLet(_,_,TyOp(MSet,[a;b],_),rest,_) -> sprintf "%s = %s;" (codegen a) (codegen b) |> state; codegen rest
        | TyLet(_,v, TyMemoizedExpr(MemoClosure,_,_,tag,ClosureT(a,r)), rest, _) ->
            sprintf "%s = %s;" (print_tyv_with_type v) (print_method tag) |> state; 
            codegen rest
        | TyLet(_,tyv,b,rest,_) -> sprintf "%s = %s;" (print_tyv_with_type tyv) (codegen b) |> state; codegen rest
        | TyLit x -> print_value x
        | TyMemoizedExpr(MemoMethod,used_vars,_,tag,t) ->
            let args = Set.toList !used_vars |> List.map print_tyv |> String.concat ", "
            let method_name = print_method tag
            sprintf "%s(%s)" method_name args
        | TyVV(l,(VVT (t, _))) -> 
            List.choose (fun x -> let x = codegen x in if x = "" then None else Some x) l
            |> String.concat ", "
            |> sprintf "make_tuple_%i(%s)" (tuple_tag t)
        | TyVV(l,_) -> failwith "The type of TyVT should always be VTT."

        | TyOp(Apply,[a;b],t) -> // Apply during codegen is only used for applying closures.
            // There is one level of flattening in the outer arguments.
            let b = tuple_field b |> List.map codegen |> String.concat ", "
            sprintf "%s(%s)" (codegen a) b

        // Primitive operations on expressions.
        | TyOp(Add,[a;b],t) -> sprintf "(%s + %s)" (codegen a) (codegen b)
        | TyOp(Sub,[a;b],t) -> sprintf "(%s - %s)" (codegen a) (codegen b)
        | TyOp(Mult,[a;b],t) -> sprintf "(%s * %s)" (codegen a) (codegen b)
        | TyOp(Div,[a;b],t) -> sprintf "(%s / %s)" (codegen a) (codegen b)
        | TyOp(Mod,[a;b],t) -> sprintf "(%s %% %s)" (codegen a) (codegen b)
        | TyOp(LT,[a;b],t) -> sprintf "(%s < %s)" (codegen a) (codegen b)
        | TyOp(LTE,[a;b],t) -> sprintf "(%s <= %s)" (codegen a) (codegen b)
        | TyOp(EQ,[a;b],t) -> sprintf "(%s == %s)" (codegen a) (codegen b)
        | TyOp(NEQ,[a;b],t) -> sprintf "(%s != %s)" (codegen a) (codegen b)
        | TyOp(GT,[a;b],t) -> sprintf "(%s > %s)" (codegen a) (codegen b)
        | TyOp(GTE,[a;b],t) -> sprintf "(%s >= %s)" (codegen a) (codegen b)
        | TyOp(And,[a;b],t) -> sprintf "(%s && %s)" (codegen a) (codegen b)
        | TyOp(Or,[a;b],t) -> sprintf "(%s || %s)" (codegen a) (codegen b)

        | TyOp(ShiftLeft,[x;y],_) -> sprintf "(%s << %s)" (codegen x) (codegen y)
        | TyOp(ShiftRight,[x;y],_) -> sprintf "(%s >> %s)" (codegen x) (codegen y)

        | TyOp(ShuffleXor,[x;y],_) -> sprintf "cub::ShuffleXor(%s, %s)" (codegen x) (codegen y)
        | TyOp(ShuffleUp,[x;y],_) -> sprintf "cub::ShuffleUp(%s, %s)" (codegen x) (codegen y)
        | TyOp(ShuffleDown,[x;y],_) -> sprintf "cub::ShuffleDown(%s, %s)" (codegen x) (codegen y)
        | TyOp(ShuffleIndex,[x;y],_) -> sprintf "cub::ShuffleIndex(%s, %s)" (codegen x) (codegen y)

        | TyOp(Neg,[a],t) -> sprintf "(-%s)" (codegen a)
        | TyOp(VVIndex,[a;b],t) -> sprintf "%s.tup%s" (codegen a) (codegen b)
        | TyOp(ArrayIndex,[a;b],t) -> print_array a b
        | TyOp(Log,[x],_) -> sprintf "log(%s)" (codegen x)
        | TyOp(Exp,[x],_) -> sprintf "exp(%s)" (codegen x)
        | TyOp(Tanh,[x],_) -> sprintf "tanh(%s)" (codegen x)

        // Cuda kernel constants
        | TyOp(Syncthreads,[],_) -> state "syncthreads();"; ""
        | TyOp(ThreadIdxX,[],_) -> "threadIdx.x"
        | TyOp(ThreadIdxY,[],_) -> "threadIdx.y"
        | TyOp(ThreadIdxZ,[],_) -> "threadIdx.z"
        | TyOp(BlockIdxX,[],_) -> "blockIdx.x"
        | TyOp(BlockIdxY,[],_) -> "blockIdx.y"
        | TyOp(BlockIdxZ,[],_) -> "blockIdx.z"

        | TyOp _ as x -> failwithf "Missing TyOp case. %A" x

    let print_closure_a a = tuple_field_ty a |> List.map print_simple_type |> String.concat ", "

    let print_closure_type_definition (a,r) tag =
        sprintf "typedef %s(*%s)(%s);" (print_simple_type r) (print_closure_type' tag) (print_closure_a a) |> state

    let print_tuple_defintion tys tag =
        let tuple_name = print_tuple' tag
        sprintf "struct %s {" tuple_name |> state
        enter <| fun _ -> List.iteri (fun i ty -> 
            match ty with
            | VVT([],_) -> ()
            | _ -> sprintf "%s tup%i;" (print_simple_type ty) i |> state) tys; ""
        "};" |> state
        sprintf "__device__ __forceinline__ %s make_tuple_%i(%s){" 
            tuple_name
            tag
            (List.mapi (fun i ty -> 
                match ty with
                | VVT([],_) -> ""
                | _ -> sprintf "%s tup_arg%i" (print_simple_type ty) i) tys
                |> List.filter ((<>) "") 
                |> String.concat ", ")
        |> state
        enter' <| fun _ ->
            sprintf "%s tmp;" tuple_name |> state
            List.iteri (fun i _ -> sprintf "tmp.tup%i = tup_arg%i;" i i |> state) tys
            "return tmp;" |> state
        "}" |> state

    let print_method (body,tag,args) = 
        let is_main = tag = 0L
        let prefix = if is_main then "__global__" else "__device__"
        let method_name = if is_main then "MainKernel" else print_method tag
        let args = 
            Set.toList !args
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
                for x in imemo do print_method (memo_value x.Value)

            with_channel CodegenChannels.Definitions <| fun _ ->
                for x in tuple_definitions do print_tuple_defintion x.Key x.Value
                for x in closure_type_definitions do print_closure_type_definition x.Key x.Value

            // I am just swapping the positions so the tuple definitions come first in the printed code.
            // Unfortunately, getting the definitions requires a pass through the AST first
            // so I can't just print them at the start.
            add_channels_a_to_main CodegenChannels.Definitions
            add_channels_a_to_main CodegenChannels.Code
        
        "}" |> state
        
        cur_program () |> process_statements |> Succ
    with e -> Fail (e.Message, e.StackTrace)

open SpiralV5Parser_v2a
open FParsec

let spiral_codegen dims body = 
    match spiral_parse body with
    | Success(r,_,_) ->
        match spiral_typecheck dims r with
        | Succ(_,memo) -> print_method_dictionary memo
        | Fail er -> Fail er
    | Failure(er,_,_) -> 
        Fail (er,"")

let fib =
    """
fun rec fib x = if x <= 0 then 0 else fib (x-1) + fib (x-2)
fib 5
    """

let fib_y =
    """
fun rec y f x = f (y f) x
inl fib r x = if x <= 0 then 0 else r (x-1) + r (x-2)
y fib 5
    """

let fib_acc =
    """
inl fib n =
    fun rec fib n a b = if n >= 0 then fib (n-1) b (a+b) else a
    fib n 0 1
fib 2
    """

let fib_acc_y = // The Y Combinator needs all the arguments when dealing with methods.
    """
fun rec y f n a b = f (y f) n a b
inl fib n =
    inl fib r n a b = if n >= 0 then r (n-1) b (a+b) else a
    y fib n 0 1
fib 2
    """

let clo1 =
    """
inl add (x,y),_ = x+y
fun t() =
    inl f = add `((3*2,4/3),(5-1,5))
    f ((1,1),(2,2)), f((2,2),(2,2))
t()
    """

let zip =
    """
inl rec tuple_fold f s l =
    typecase l with
    | x :: xs -> tuple_fold f (f s x) xs
    | _ -> s

inl transpose l on_fail succ =
    
    """

let tuple_library =
    """
inl tuple =
    inl rec tuple_foldl f s l =
        typecase l with
        | x :: xs -> tuple_foldl f (f s x) xs
        | () -> s
    inl rec tuple_foldr f s l =
        typecase l with
        | x :: xs -> f x (tuple_foldr f s xs)
        | () -> s
    
    inl id x = x
    inl tuple_rev, tuple_map =
        inl tuple_map' f l = tuple_foldl (inl s x -> f x :: s) () l
        inl tuple_rev l = tuple_map' id l
        inl tuple_map f = tuple_map' f >> tuple_rev
        tuple_rev, tuple_map
    inl tuple_forall f l = 
        tuple_foldl (inl con x l ->
            typecase f x with
            | .True() -> con l
            | .False() -> .False()
            ) id l .True()
    inl tuple_exists f l =
        tuple_foldl (inl con x l ->
            typecase f x with
            | .False() -> con l
            | .True() -> .True()
            ) id l .False()
    inl tuple_filter f l =
        tuple_foldl (inl s x ->
            typecase f x with
            | .True() -> x :: s
            | .False() -> s
            ) () l
        |> tuple_rev
    module

fun top() =    
    (tuple.tuple_forall) (tuple.id) (.True(),.True(),.True())
top ()
    """

let r = spiral_codegen default_dims tuple_library

printfn "%A" r

