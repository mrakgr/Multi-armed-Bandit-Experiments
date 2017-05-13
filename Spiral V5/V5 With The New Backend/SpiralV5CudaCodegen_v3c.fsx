#load "SpiralV5Parser_v2a.fsx"

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
| TupleDefinitions = 2

let print_method_dictionary (imemo: MemoDict) =
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

    let get_tag =
        let mutable i = 0
        fun () -> i <- i+1; i

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
        | VVT ([], _) -> "void"
        | VVT (t, _) -> tuple_def_proc t |> print_tuple
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
        | LitString x -> x

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

    and codegen = function
        | TyV v -> print_tyv v
        | TyIf(cond,tr,fl,_) -> // If statements will aways be hoisted into methods in this language.
            sprintf "if (%s) {" (codegen cond) |> state
            enter <| fun _ -> sprintf "return %s;" (codegen tr)
            "}" |> state
            "else {" |> state
            enter <| fun _ -> sprintf "return %s;" (codegen fl)
            "}" |> state
            ""
        | TyLet(v, TyUnOp(StructCreate,Array(ar_typ,ar_sizes,typ),_), rest, _) ->
            match ar_typ with
            | Local | Global -> print_array_declaration false typ v ar_sizes
            | Shared -> print_array_declaration true typ v ar_sizes
            codegen rest
        | TyLet(_, TyUnOp(StructCreate, _, _), _, _) -> failwith "Unknown struct. For now each struct is intended to have their own case in the codegen."
        | TyVV([],_) | TyType _ -> ""
        | TyLet(_,(TyVV([],_) | TyType _),rest,_) -> codegen rest
        | TyLet((_,VVT([],_)),b,rest,_) -> sprintf "%s;" (codegen b) |> state; codegen rest
        | TyLet(_,TyBinOp(MSet,a,b,_),rest,_) -> sprintf "%s = %s;" (codegen a) (codegen b) |> state; codegen rest
        | TyLet(tyv,b,rest,_) -> sprintf "%s = %s;" (print_tyv_with_type tyv) (codegen b) |> state; codegen rest
        | TyLit x -> print_value x
        | TyMethodCall(used_vars,_,tag,t) ->
            let args = Set.toList !used_vars |> List.map print_tyv |> String.concat ", "
            let method_name = print_method tag
            sprintf "%s(%s)" method_name args
        | TyVV(l,(VVT (t, _))) -> 
            List.choose (fun x -> let x = codegen x in if x = "" then None else Some x) l
            |> String.concat ", "
            |> sprintf "make_tuple_%i(%s)" (tuple_def_proc t)
        | TyVV(l,_) -> failwith "The type of TyVT should always be VTT."

        | TyUnOp(op,a,t) ->
            match op with
            | Neg -> sprintf "(-%s)" (codegen a)
            | StructCreate -> failwith "It should have been taken care of in a let statement."
            | CallAsMethod | TypeError -> failwith "Does not appear in codegen."

        | TyBinOp(op,a,b,t) ->
            match op with
            | VVIndex -> sprintf "%s.tup%s" (codegen a) (codegen b)
            | ArrayIndex -> print_array a b

            // Primitive operations on expressions.
            | Add -> sprintf "(%s + %s)" (codegen a) (codegen b)
            | Sub -> sprintf "(%s - %s)" (codegen a) (codegen b)
            | Mult -> sprintf "(%s * %s)" (codegen a) (codegen b)
            | Div -> sprintf "(%s / %s)" (codegen a) (codegen b)
            | Mod -> sprintf "(%s %% %s)" (codegen a) (codegen b)
            | LT -> sprintf "(%s < %s)" (codegen a) (codegen b)
            | LTE -> sprintf "(%s <= %s)" (codegen a) (codegen b)
            | EQ -> sprintf "(%s == %s)" (codegen a) (codegen b)
            | NEQ -> sprintf "(%s != %s)" (codegen a) (codegen b)
            | GT -> sprintf "(%s > %s)" (codegen a) (codegen b)
            | GTE -> sprintf "(%s >= %s)" (codegen a) (codegen b)
            | And -> sprintf "(%s && %s)" (codegen a) (codegen b)
            | Or -> sprintf "(%s || %s)" (codegen a) (codegen b)
            
            // Inapplicable during codegen
            | MSet -> failwith "It should have been taken care of in a let statement."
            | VVCons | ArrayCreateShared | ArrayCreate | Apply -> failwith "Should never appear in the codegen phase."

    let print_tuple_defintion tys tag =
        let tuple_name = print_tuple tag
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

open SpiralV5Parser_v2a
open FParsec

let spiral_codegen body = 
    match spiral_parse body with
    | Success(r,_,_) ->
        match spiral_typecheck r with
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

let r = spiral_codegen fib_acc_y

printfn "%A" r