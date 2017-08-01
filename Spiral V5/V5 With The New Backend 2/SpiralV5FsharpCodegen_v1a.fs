﻿module Spiral.Codegen

open Spiral.Lang
open System.Collections.Generic
open System.Text

type ProgramNode =
| Statement of string
| Indent
| Dedent
| Statements of ResizeArray<ProgramNode>

let exp x = String.concat "" x

let process_statements (statements: ResizeArray<ProgramNode>) =
    let rec process_statement (code: StringBuilder,ind as state) statement =
        match statement with
        | Statement x -> [|String.replicate ind " "; x; "\n"|] |> exp |> code.Append, ind
        | Indent -> code, ind+4
        | Dedent -> code, ind-4
        | Statements x -> process_statements state x
    and process_statements state (statements: ResizeArray<ProgramNode>) =
        Seq.fold process_statement state statements
    process_statements (StringBuilder(),0) statements
    |> fun (code,ind) -> code.ToString()

type Buf = ResizeArray<ProgramNode>

let print_program ((main, globals): TypedExpr * LangGlobals) =
    let get_tag =
        let mutable i = 0L
        fun () -> i <- i+1L; i

    let def_proc (d: Dictionary<_,_>) t = 
        match d.TryGetValue t with
        | true, v -> v
        | false, _ ->
            let v = get_tag()
            d.Add(t,v)
            v

    let tuple_definitions = d0()
    let tuple_tag x = def_proc tuple_definitions x
    let print_tuple' v = sprintf "Tuple%i" v
    let print_tuple = function
        | [] -> "unit"
        | t -> tuple_tag t |> print_tuple'

    let env_ty_definitions = d0()
    let env_ty_tag x = def_proc env_ty_definitions x
    let print_env_ty' v = sprintf "EnvTy%i" v
    let print_env_ty t = env_ty_tag t |> print_env_ty'

    let union_ty_definitions = d0()
    let union_ty_tag x = def_proc union_ty_definitions x
    let print_union_ty' v = sprintf "UnionTy%i" v
    let print_union_ty set = union_ty_tag set |> print_union_ty'

    let print_rec_ty v = sprintf "RecTy%i" v

    let state (buffer: Buf) x = buffer.Add <| Statement x
    let enter' (buffer: Buf) f = buffer.Add Indent; f(); buffer.Add Dedent
    let enter (buffer: Buf) f = 
        enter' buffer <| fun _ -> 
            match f() with
            | "" -> ()
            | s -> state buffer s

    let rec is_unit = function
        | VVT [] | TypeConstructorT _ | LitT _ | ForCastT _ | DotNetAssemblyT _ | DotNetTypeRuntimeT _ -> true
        | UnionT _ | RecT _ | DotNetTypeInstanceT _ | ClosureT _ | PrimT _ -> false
        | ArrayT(_,t) -> is_unit t
        | TyEnvT env -> Map.forall (fun _ -> is_unit) env
        | VVT t -> List.forall is_unit t

    let (|Unit|_|) x = if is_unit x then Some () else None

    let rec print_type = function
        | Unit -> "unit"
        | TyEnvT env -> print_env_ty env
        | VVT t -> print_tuple t
        | UnionT t -> print_union_ty t
        | RecT t -> print_rec_ty t
        | ArrayT(DotNetReference,t) -> sprintf "%s ref" (print_type t)
        | ArrayT(DotNetHeap,t) -> sprintf "%s []" (print_type t)
        | ArrayT _ -> failwith "Not implemented."
        | DotNetTypeInstanceT t ->
            globals.memoized_dotnet_types |> snd |> fun x -> x.[t]
            |> print_dotnet_instance_type
        | ClosureT(a,b) -> 
            let a = tuple_field_ty a |> List.map print_type |> String.concat " * "
            sprintf "(%s) -> %s" a (print_type b)
        | PrimT x ->
            match x with
            | Int8T -> "int8"
            | Int16T -> "int16"
            | Int32T -> "int32"
            | Int64T -> "int64"
            | UInt8T -> "uint8"
            | UInt16T -> "uint16"
            | UInt32T -> "uint32"
            | UInt64T -> "uint64"
            | Float32T -> "float32"
            | Float64T -> "float64"
            | BoolT -> "bool"
            | StringT -> "string"
            | CharT -> "char"
        | TypeConstructorT _ | LitT _ | ForCastT _ | DotNetAssemblyT _ | DotNetTypeRuntimeT _ -> 
            failwith "Should be covered in Unit."
                

    and print_dotnet_instance_type (x: System.Type) =
        if x.GenericTypeArguments.Length > 0 then
            [|
            x.Namespace
            "." 
            x.Name.Split '`' |> Array.head
            "<"
            Array.map (dotnet_type_to_ty globals.memoized_dotnet_types >> print_type) x.GenericTypeArguments |> String.concat ","
            ">"
            |] |> String.concat null
        else
            [|x.Namespace; "."; x.Name|] |> String.concat null

    let print_tyv (tag,ty) = 
        match ty with
        | Unit -> failwith "Unit type variables should never be printed."
        | _ -> sprintf "var_%i" tag
    let print_tyv_with_type (tag,ty as v) = sprintf "(%s: %s)" (print_tyv v) (print_type ty)
    let print_method tag = sprintf "method_%i" tag

    let print_args args = 
        Set.toList args |> List.filter (snd >> is_unit >> not)
        |> List.map print_tyv_with_type |> String.concat ", "

    let print_union_case tag i = sprintf "Union%iCase%i" tag i
    let print_rec_tuple tag = sprintf "Rec%iTuple" tag
    let print_rec_case tag i = sprintf "Rec%iCase%i" tag i

    let rec codegen (buffer: Buf) expr =
        let state = state buffer
        let enter' = enter' buffer
        let enter = enter buffer
       
        let print_value = function
            | LitInt8 x -> sprintf "%iy" x
            | LitInt16 x -> sprintf "%is" x
            | LitInt32 x -> sprintf "%i" x
            | LitInt64 x -> sprintf "%iL" x
            | LitUInt8 x -> sprintf "%iuy" x
            | LitUInt16 x -> sprintf "%ius" x
            | LitUInt32 x -> sprintf "%iu" x
            | LitUInt64 x -> sprintf "%iUL" x
            | LitFloat32 x -> sprintf "%ff" x
            | LitFloat64 x -> sprintf "%f" x
            | LitString x -> sprintf "\"%s\"" x
            | LitChar x -> sprintf "'%c'" x
            | LitBool x -> if x then "true" else "false"

        let codegen x = codegen buffer x

        let print_if t f =
            match t with
            | Unit -> f (); ""
            | t ->
                let if_var = sprintf "(if_var_%i: %s)" (get_tag()) (print_type t)
                sprintf "let %s =" if_var |> state
                enter' <| fun _ -> f()
                if_var
        
        let rec if_ cond tr fl =
            print_if (get_type tr) <| fun _ ->
                sprintf "if %s then" (codegen cond) |> state
                enter <| fun _ -> codegen tr
                "else" |> state
                enter <| fun _ -> codegen fl

        let make_struct l on_empty on_rest =
            Seq.choose (fun x -> let x = codegen x in if x = "" then None else Some x) l
            |> String.concat ", "
            |> function
                | "" -> on_empty()
                | x -> on_rest x

        let if_not_unit ty f = if is_unit ty then "" else f()

        let print_case_tuple' t l name = make_struct l (fun _ -> name) (sprintf "%s(%s(%s))" name (print_tuple t))
        let print_case_tuple l name = print_case_tuple' (List.map get_type l) l name

        let union_process_tuple f l (tys: Set<_>) =
            let l_tys = List.map get_type l
            let l_vvt = VVT l_tys
            let i = Seq.findIndex ((=) l_vvt) tys
            print_case_tuple' l_tys l (f i)

        let print_case_var l name = make_struct l (fun _ -> name) (sprintf "%s(%s)" name)
        let union_process_var f (v, v_ty) (tys: Set<_>) =
            let i = Seq.findIndex ((=) v_ty) tys
            print_case_var [v] (f i)

        let (|DotNetTypeRuntime|_|) = function
            | DotNetTypeRuntimeT x -> map_rev_dotnet globals.memoized_dotnet_types x |> Some
            | _ -> None

        let (|DotNetTypeInstance|_|) = function
            | DotNetTypeInstanceT x -> map_rev_dotnet globals.memoized_dotnet_types x |> Some
            | _ -> None

        let (|DotNetPrintedArgs|) x = List.map codegen x |> List.filter ((<>) "") |> String.concat ", "

        let array_create (TyTuple size) = function
            | Unit -> ""
            | ArrayT(_,t) ->
                let x = List.map codegen size |> String.concat "*"
                sprintf "Array.zeroCreate<%s> (System.Convert.ToInt32(%s))" (print_type t) x
            | _ -> failwith "impossible"

        let reference_create = function
            | TyType Unit -> ""
            | x -> sprintf "(ref %s)" (codegen x)

        let array_index (TyTuple size) ar (TyTuple idx) =
            match ar with
            | TyType Unit -> ""
            | _ ->
                let size, idx = List.map codegen size, List.map codegen idx
                // Print assertions for multidimensional arrays.
                // For 1d arrays, the .NET runtime does the checking.
                match size with
                | _ :: _ :: _ ->
                    List.iter2 (fun s i -> 
                        let cond = sprintf "%s < %s && %s >= 0L" i s i
                        // I am worried about code size blowup due to the string in sprintf so I'll leave the message out.
                        //let message = """sprintf "Specified argument was out of the range of valid values in array indexing. index=%i size=%i" """
                        sprintf "if (%s) = false then raise <| System.ArgumentOutOfRangeException(\"%s\")" cond i |> state
                        ) size idx
                | _ -> ()

                let rec index_first = function
                    | _ :: s :: sx, i :: ix -> index_rest (sprintf "%s * %s" i s) (sx, ix)
                    | [_], [i] -> i
                    | _ -> "0"
                and index_rest prev = function
                    | s :: sx, i :: ix -> index_rest (sprintf "(%s + %s) * %s" prev i s) (sx, ix)
                    | [], [i] -> sprintf "%s + %s" prev i
                    | _ -> failwith "Invalid state."

                sprintf "%s.[int32 (%s)]" (codegen ar) (index_first (size, idx))

        let reference_index = function
            | TyType Unit -> ""
            | x -> sprintf "(!%s)" (codegen x)

        let array_set size ar idx r = 
            match ar with
            | TyType Unit -> ()
            | _ -> sprintf "%s <- %s" (array_index size ar idx) (codegen r) |> state
        let reference_set l r = 
            match l with
            | TyType Unit -> ()
            | _ -> sprintf "%s := %s" (codegen l) (codegen r) |> state

        match expr with
        | TyTag (_, Unit) | TyV (_, Unit) -> ""
        | TyTag v -> print_tyv v
        | TyOp(If,[cond;tr;fl],t) -> if_ cond tr fl
        | TyLet(LetInvisible, _, _, rest, _) -> codegen rest
        | TyLet(_,(_,Unit),b,rest,_) ->
            match b with
            | TyOp(ArraySet,[TyOp(ArrayIndex,[size;ar;idx],_);b],_) ->
                match get_type ar with
                | ArrayT(DotNetReference,_) -> reference_set ar b
                | ArrayT(DotNetHeap,_) -> array_set size ar idx b
                | _ -> failwith "impossible"
            | _ ->
                let b = codegen b
                if b <> "" then sprintf "%s" b |> state
            codegen rest
        | TyLet(_,tyv,b,rest,_) -> sprintf "let %s = %s" (print_tyv_with_type tyv) (codegen b) |> state; codegen rest
        | TyLit x -> print_value x
        | TyMemoizedExpr(MemoMethod,fv,_,tag,_) ->
            let method_name = print_method tag
            sprintf "%s(%s)" method_name (print_args !fv)
        | TyMemoizedExpr(MemoClosure args,fv,rev_renamer,tag,_) -> 
            let method_name = print_method tag
            let fv = !fv - (renamer_apply_pool rev_renamer args)
            if fv.IsEmpty then method_name
            else sprintf "%s(%s)" method_name (print_args fv)
        | TyV(x & TyType t, UnionT tys) -> union_process_var (print_union_case (union_ty_tag tys)) (x,t) tys
        | TyV(x & TyType t, RecT tag) ->
            match globals.memoized_types.[tag] with
            | UnionT tys -> union_process_var (print_rec_case tag) (x,t) tys
            | _ -> failwith "Only UnionT can be a recursive var type."
        | TyVV(l,VVT t) -> make_struct l (fun _ -> "") (fun args -> sprintf "%s(%s)" (print_tuple t) args)
        | TyVV(l,UnionT tys) -> union_process_tuple (print_union_case (union_ty_tag tys)) l tys
        | TyVV(l,RecT tag) -> 
            match globals.memoized_types.[tag] with
            | VVT _ -> print_case_tuple l (print_rec_tuple tag)
            | UnionT tys -> union_process_tuple (print_rec_case tag) l tys
            | _ -> failwith "Only VVT and UnionT are recursive tuple types."
        | TyVV(_,_) -> failwith "TyVV's type can only by VVT, UnionT and RecT."
        | TyEnv(env_term, TyEnvT env_ty) ->
            Map.toArray env_term
            |> Array.map snd
            |> fun x -> make_struct x (fun _ -> "") (fun args -> sprintf "%s(%s)" (print_env_ty env_ty) args)
        | TyEnv(env_term,_) -> failwith "Can't be any other type."
        | TyOp(op,args,t) ->
            match op, args with
            | Apply,[a;b] ->
                // Apply during codegen is only used for applying closures.
                // There is one level of flattening in the outer arguments.
                // The reason for this is the symmetry between the F# and the Cuda side.
                let b = tuple_field b |> List.map codegen |> String.concat ", "
                sprintf "%s(%s)" (codegen a) b
            | Case,v :: cases ->
                print_if t <| fun _ ->
                    let tag = 
                        match get_type v with
                        | RecT tag -> tag
                        | UnionT tys -> union_ty_tag tys
                        | _ -> failwith "impossible"

                    sprintf "match %s with" (codegen v) |> state
                    let print_case i = function
                        | case & TyType Unit -> sprintf "| %s ->" (print_union_case tag i) |> state
                        | case -> sprintf "| %s(%s) ->" (print_union_case tag i) (codegen case) |> state
                    let rec loop i = function
                        | case :: body :: rest -> 
                            print_case i case
                            enter <| fun _ -> codegen body
                            loop (i+1) rest
                        | [] -> ()
                        | _ -> failwith "The cases should always be in pairs."
                    loop 0 cases

            | ArrayCreate,[a] -> array_create a t
            | ReferenceCreate,[a] -> reference_create a
            | ArrayIndex,[a;b & TyType(ArrayT(DotNetHeap,_));c] -> array_index a b c
            | ArrayIndex,[a;b & TyType(ArrayT(DotNetReference,_));c] -> reference_index b

            // Primitive operations on expressions.
            | Add,[a;b] -> sprintf "(%s + %s)" (codegen a) (codegen b)
            | Sub,[a;b] -> sprintf "(%s - %s)" (codegen a) (codegen b)
            | Mult,[a;b] -> sprintf "(%s * %s)" (codegen a) (codegen b)
            | Div,[a;b] -> sprintf "(%s / %s)" (codegen a) (codegen b)
            | Mod,[a;b] -> sprintf "(%s %% %s)" (codegen a) (codegen b)
            | LT,[a;b] -> sprintf "(%s < %s)" (codegen a) (codegen b)
            | LTE,[a;b] -> sprintf "(%s <= %s)" (codegen a) (codegen b)
            | EQ,[a;b] -> sprintf "(%s == %s)" (codegen a) (codegen b)
            | NEQ,[a;b] -> sprintf "(%s != %s)" (codegen a) (codegen b)
            | GT,[a;b] -> sprintf "(%s > %s)" (codegen a) (codegen b)
            | GTE,[a;b] -> sprintf "(%s >= %s)" (codegen a) (codegen b)
            | And,[a;b] -> sprintf "(%s && %s)" (codegen a) (codegen b)
            | Or,[a;b] -> sprintf "(%s || %s)" (codegen a) (codegen b)

            | ShiftLeft,[x;y] -> sprintf "(%s << %s)" (codegen x) (codegen y)
            | ShiftRight,[x;y] -> sprintf "(%s >> %s)" (codegen x) (codegen y)

    //        | ShuffleXor,[x;y],_) -> sprintf "cub::ShuffleXor(%s, %s)" (codegen x) (codegen y)
    //        | ShuffleUp,[x;y],_) -> sprintf "cub::ShuffleUp(%s, %s)" (codegen x) (codegen y)
    //        | ShuffleDown,[x;y],_) -> sprintf "cub::ShuffleDown(%s, %s)" (codegen x) (codegen y)
    //        | ShuffleIndex,[x;y],_) -> sprintf "cub::ShuffleIndex(%s, %s)" (codegen x) (codegen y)

            | Neg,[a] -> sprintf "(-%s)" (codegen a)
            | VVIndex,[a;b] -> if_not_unit t <| fun _ -> sprintf "%s.mem_%s" (codegen a) (codegen b)
            | EnvUnseal,[r; TyLit (LitString k)] -> if_not_unit t <| fun _ -> sprintf "%s.mem_%s" (codegen r) k
            | Log,[x] -> sprintf "log(%s)" (codegen x)
            | Exp,[x] -> sprintf "exp(%s)" (codegen x)
            | Tanh,[x] -> sprintf "tanh(%s)" (codegen x)

            | DotNetTypeConstruct,[TyTuple (DotNetPrintedArgs args)] ->
                match t with 
                | DotNetTypeInstance instance_type -> sprintf "%s(%s)" (print_dotnet_instance_type instance_type) args
                | _ -> failwith "impossible"
            | DotNetTypeCallMethod,[v; TyTuple [TypeString method_name; TyTuple (DotNetPrintedArgs method_args)]] ->
                match v with
                | TyType (DotNetTypeRuntime t) -> sprintf "%s.%s(%s)" (print_dotnet_instance_type t) method_name method_args
                | _ -> sprintf "%s.%s(%s)" (codegen v) method_name method_args
            // Cuda kernel constants
    //        | Syncthreads,[],_) -> state "syncthreads();"; ""

            | x -> failwithf "Missing TyOp case. %A" x
        | x -> failwithf "The match cases were incomplete. Got: %A" x

    let print_rec_definition prefix (buffer: Buf) ty tag =
        let state = state buffer
        let enter' = enter' buffer
        let enter = enter buffer

        sprintf "%s %s =" prefix (print_rec_ty tag) |> state
        match ty with
        | VVT _ & Unit -> "| " + print_rec_tuple tag |> state
        | VVT _ -> sprintf "| %s of %s" (print_rec_tuple tag) (print_type ty) |> state
        | UnionT tys ->
            let tys = Set.toList tys
            enter' <| fun _ ->
                List.iteri (fun i -> function
                    | Unit -> "| " + print_rec_case tag i |> state
                    | x -> sprintf "| %s of %s" (print_rec_case tag i) (print_type x) |> state) tys
        | _ -> failwith "Only VVT and UnionT are recursive types."

    let print_union_definition prefix (buffer: Buf) tys tag =
        let state = state buffer
        let enter' = enter' buffer
        let enter = enter buffer

        let tys = Set.toList tys

        sprintf "%s %s =" prefix (print_union_ty' tag) |> state
        enter' <| fun _ ->
            List.iteri (fun i -> function
                | Unit -> "| " + print_union_case tag i |> state
                | x -> sprintf "| %s of %s" (print_union_case tag i) (print_type x) |> state) tys

    let print_struct_definition prefix (buffer: Buf) iter fold name tys tag =
        let state = state buffer
        let enter' = enter' buffer
        let enter = enter buffer

        let args sep f =
            fold (fun s k ty -> 
                match ty with
                | Unit -> s
                | _ -> f k :: s) [] tys
            |> List.rev
            |> String.concat sep

        let args_declaration = args ", " <| fun k -> sprintf "arg_mem_%s" k
        let args_mapping = args "; " <| fun k -> sprintf "mem_%s = arg_mem_%s" k k

        sprintf "%s %s =" prefix name |> state
        enter' <| fun _ -> 
            "struct" |> state
            iter (fun k ty -> 
                match ty with
                | Unit -> ()
                | _ -> sprintf "val mem_%s: %s" k (print_type ty) |> state) tys
            
            sprintf "new(%s) = {%s}" args_declaration args_mapping |> state
            "end" |> state

    let print_method_definition (buffer: Buf) is_first (memo_type,body,tag,fv) = 
        let state = state buffer
        let enter' = enter' buffer
        let enter = enter buffer

        let prefix = if is_first then "let rec" else "and"
        let method_name = print_method tag
        match memo_type with
        | MemoClosure args -> 
            let fv = !fv - args |> fun fv -> if fv.IsEmpty then "" else sprintf "(%s) " (print_args fv)
            sprintf "%s %s %s(%s): %s =" prefix method_name fv (print_args args) (print_type (get_type body))
            
        | MemoMethod -> sprintf "%s %s(%s): %s =" prefix method_name (print_args !fv) (print_type (get_type body))
        |> state

        enter' <| fun _ -> 
            match codegen buffer body with
            | "" -> ()
            | x -> state x
            match get_type body with
            | Unit -> "()" |> state
            | _ -> ()

    let method_buffer = ResizeArray()
    let definitions_buffer = ResizeArray()

    globals.memoized_methods |> Seq.fold (fun is_first x -> 
        match x.Value with
        | MemoMethodDone (memo_type, e, tag, args) -> print_method_definition method_buffer is_first (memo_type, e, tag, args); false
        | _ -> is_first) true |> ignore
    codegen method_buffer main |> state method_buffer // Can't forget the non-method

    let type_prefix =
        let mutable prefix = false
        fun () -> if prefix then "and" else prefix <- true; "type"

    for x in globals.memoized_types do
        let tag,ty = x.Key, x.Value
        print_rec_definition (type_prefix()) definitions_buffer ty tag

    for x in union_ty_definitions do
        let tys,tag = x.Key, x.Value
        print_union_definition (type_prefix()) definitions_buffer tys tag

    for x in env_ty_definitions do
        let tys, tag = x.Key, x.Value
        let tuple_name = print_env_ty' tag
        print_struct_definition (type_prefix()) definitions_buffer Map.iter Map.fold tuple_name tys tag

    for x in tuple_definitions do
        let tys, tag = x.Key, x.Value
        let tuple_name = print_tuple' tag
        let fold f s l = List.fold (fun (i,s) ty -> i+1, f s (string i) ty) (0,s) l |> snd
        let iter f l = List.iteri (fun i x -> f (string i) x) l
        print_struct_definition (type_prefix()) definitions_buffer iter fold tuple_name tys tag
    
    // Definitions need a pass through the AST in order to be memoized. Hence they are printed first in 
    // actual code while here in the codegen that is done last. Here I just swap the buffers.
    definitions_buffer.AddRange method_buffer
    process_statements definitions_buffer

open FParsec

let spiral_codegen aux_modules main_module = 
    let rec parse_modules xs on_fail ret =
        let p x on_fail ret =
            match Spiral.Parse.spiral_parse x with
            | Success(r,_,_) -> ret r
            | Failure(er,_,_) -> on_fail er
        match xs with
        | (name,code as x) :: xs -> 
            p x on_fail <| fun r -> 
                parse_modules xs on_fail <| fun rs ->
                    l name r rs |> ret
        | [] -> p main_module on_fail ret

    let code =
        let d = Dictionary()
        let f (name,code: string) = d.Add(name, code.Split [|'\n'|])
        Seq.iter f aux_modules
        f main_module
        d
     
    parse_modules aux_modules Fail (fun r -> 
        spiral_typecheck code r Fail (print_program >> Succ))

let test1 = // Does it run?
    "test1",
    """
inl a = 5
inl b = 10
a + b
    """

let test2 = // Does it run methods?
    "test2",
    """
met a () = 5
met b () = 10
a () + b ()
    """

let test3 = // Does this method case work?
    "test3",
    """
met a = 5
met b = 10
a + b
    """

let test4 = // Does the and pattern work correctly?
    "test4",
    """
met f (a, b) (c, d) = (a+c,b+d)
met q & (a, b) = 1,2
met w & (c, d) = 3,4
f q w
    """

let test5 = // Does basic pattern matching work?
    "test5",
    """
inl f = function
    || .Add x y -> x + y
    || .Sub x y -> x - y
    || .Mult x y -> x * y
inl a = f .Add 1 2
inl b = f .Sub 1 2
inl c = f .Mult 1 2
a, b, c
    """

let fib = // Does recursion work on the fibonacci example?
    "fib",
    """
met rec fib ^dyn x = 
    if x <= 0 then 0 else fib (x-1) + fib (x-2)
    : x
fib 1
    """

let test6 = // Does returning type level methods from methods work?
    "test6",
    """
met min n =
    met tes a =
        met b -> 
            met c ->
                met d -> a,b,c
    tes 1 2 (2.2,3,4.5)
min 10
    """
let test7 = // Do active patterns work?
    "test7",
    """
inl f op1 op2 op3 = function
    | ^op1 (.Some, x) -> x
    | ^op2 (.Some, x) -> x
    | ^op3 (.Some, x) -> x

inl add = function
    | .Add -> .Some, inl x y -> x + y
    | _ -> .None
inl sub = function
    | .Sub -> .Some, inl x y -> x - y
    | _ -> .None
inl mult = function
    | .Mult -> .Some, inl x y -> x * y
    | _ -> .None

inl f = f add sub mult

inl a = f .Add 1 2
inl b = f .Sub 1 2
inl c = f .Mult 1 2
a, b, c
    """

let test8 = // Does the basic union type work?
    "test8",
    """
met x =
    inl option_int = 
        type 
            .Some, 1 
            .None
    option_int .None //(.Some, 10)
match x with
| .Some, x -> x
| .None -> 0
    """

let test9 = // Does the partial evaluator optimize unused match cases?
    "test9",
    """
inl ab = 
    type .A
         .B
met x = (ab .A, ab .A, ab .A)
match x with
| .A, _, _ -> 1
| _, .A, _ -> 2
| _, _, .A -> 3
| _ -> 4
    """

let test10 = // The worst case for partially evaluated pattern matchers.
    "test10",
    """
inl ab = 
    type .A
         .B
met x = (ab .A, ab .A, ab .A, ab .A)
match x with
| .A, .A, _ -> 1
| _, _, .A, .A -> 2
| .A, .B, .A, .B -> 3
| _ -> 4
    """

let test11 = // Do the nested patterns work on dynamic data?
    "test1",
    """
inl a = type (1,2)
inl b = type (1,a,a)
met x = b (1, a (2,3), a (4,5))
match x with
| _, (x, _), (_, y) -> x + y
| _, _, _ -> 0
| _ :: () -> 0
    """

let test12 = // Does recursive pattern matching work on static data?
    "test12",
    """
inl rec p = function
    | .Some, x -> p x
    | .None -> 0
p (.Some, .None)
    """

let test13 = // A more complex interpreter example on static data.
    "test13",
    """
met rec expr x = 
    type 
        .V, x
        .Add, expr x, expr x
        .Mult, expr x, expr x
inl int_expr = expr int64
inl v x = int_expr (.V, x)
inl add a b = int_expr (.Add, a, b)
inl mult a b = int_expr (.Mult, a, b)
inl a = add (v 1) (v 2)
inl b = add (v 3) (v 4)
inl c = mult a b
inl rec interpreter_static = function
    | .V, x -> x
    | .Add, a, b -> interpreter_static a + interpreter_static b
    | .Mult, a, b -> interpreter_static a * interpreter_static b
interpreter_static c
    """

let test14 = // Does recursive pattern matching work on partially static data?
    "test14",
    """
met rec expr x = 
    type 
        .V, x
        .Add, expr x, expr x
        .Mult, expr x, expr x
inl int_expr = expr int64
inl v x = int_expr (.V, x)
inl add a b = int_expr (.Add, a, b)
inl mult a b = int_expr (.Mult, a, b)
met a = add (v 1) (v 2)
met b = add (v 3) (v 4)
inl c = mult a b
met rec inter x = 
    match x with
    | .V, x -> x
    | .Add, a, b -> inter a + inter b
    | .Mult, a, b -> inter a * inter b
    : 0
inter c
    """

let test15 = // Does basic .NET interop work?
    "test15",
    """
inl system = load_assembly .mscorlib
inl builder_type = ."System.Text.StringBuilder" |> system 
inl b = builder_type ("Qwe", 128i32)
inl a x =
    b .Append x |> ignore
    b .AppendLine () |> ignore
a 123
a 123i16
a "qwe"
inl str = b.ToString()
inl console = ."System.Console" |> system
console .Write str |> ignore

inl dictionary_type = ."System.Collections.Generic.Dictionary`2" |> system
inl dict = dictionary_type(int64, int64)(128i32)
dict.Add(1,2) |> ignore
dict.get_Item 1
    """

let hacker_rank_1 =
    "hacker_rank_1",
    """
// The very first warmup exercise : https://www.hackerrank.com/challenges/solve-me-first
inl console = lit_lift "System.Console" |> mscorlib
inl parse_int32 = 
    inl f = lit_lift "System.Int32" |> mscorlib
    inl str -> f .Parse str
inl read_line () = console.ReadLine()
inl write x = console.Write x
inl read_int () = read_line() |> parse_int32
inl a, b = read_int(), read_int()
write (a + b)
    """

let test16 = // Do var union types work?
    "test16",
    """
inl t = type (union (type int64) (type float32))
if dyn true then t 0
else t 0.0
    """

let test17 = // Do modules work?
    "test17",
    """
inl m =
    inl x = 2
    inl y = 3.4
    inl z = "123"
    module (x,(y),z)
m.x, m.y, m.z
    """

let test18 = // Do arrays and references work?
    "test18",
    """
inl a = ref 0
a := 5
a() |> ignore

inl a = ref ()
a := ()
a() |> ignore

inl a = array_create (10,15,5) int64
a (0,8,1) <- 2
a (0,8,1) |> ignore

inl a = array_create 3 ()
a (1) <- ()
a (1) |> ignore
    """

let test19 = // Does term level casting for functions work?
    "test19",
    """
inl add a b (c, (d, f), e) = a + b + c + d + e + f
inl f = add 1 (dyn 2) `(int64,(int64,int64),int64)
f (1,(2,5),3)
    """

let test20 = // Does pattern matching on union non-tuple types work? Do type annotation patterns work?
    "test20",
    """
inl t = union (type int64) (type float32)
inl x = t 3.5
match x with
| q : float32 -> x + x
| q : int64 -> x * x
    """

let test21 = // Does defining user operators work?
    "test21",
    """
inl (.+) a b = a + b
2 * 22 .+ 33 |> ignore

inl f op a b = op a b
f (*) 2 x
    """

let test22 = // Do unary operators work?
    "test22",
    """
inl lit_lift x =
    print_static "I am in lit_lift."
    lit_lift x
inl t1 x = -x
inl t2 x = `x
inl t3 x = .(x)
t1 2.2, t2 true, t3 "asd"
    """

let test23 = // Do when and as patterns work?
    "test23",
    """
inl f = function
    | a,b,c as q when (a < 10) -> q
    | _ -> 0,0,0
f (1,2,3)
    """

let parsers =
    "parsers",
    """
inl to_int32 = mscorlib ."System.Convert.ToInt32"
inl to_int64 = mscorlib ."System.Convert.ToInt64"

inl is_digit (^to_int32 (_: char) x) = x >= to_int32 '0' && x <= to_int32 '9'
inl is_whitespace x = x = ' '
inl is_newline x = x = '\n' || x = '\r'

inl StreamPosition = int64
inl stream stream =
    inl (pos: StreamPosition) = ref 0
    module (pos, stream)

inl ParserResult suc =
    type 
        .Succ, suc
        .Fail, (StreamPosition, string)
        .FatalFail, string

met rec List x =
    type
        .ListCons, (x, List x)
        .ListNil

inl pchar s ret = 
    match (s.stream) (s.pos()) with
    | .Succ, _ as c -> 
        s.pos := !s.pos + 1
        ret c
    | c -> ret c

inl pdigit s ret =
    inl c = pchar s
    
    if is_digit c then ret (.Succ, c)
    else ret (.Fail, (pos, "digit"))

inl pint64 s ret =
    met rec loop state i = 
        read_digit s <| function
            | .Succ, c -> 
                inl x = to_int64 c - to_int64 '0'
                i * 10 + x |> loop .Rest
            | .Fail, _ -> 
                match state with
                | .First -> ret (.Fail, (s.pos, "int64"))
                | .Rest -> ret (.Succ, i)
            | .FatalFail, _ as x -> ret x
    loop .First 0

inl many typ_p p s ret =
    inl state = s.pos
    let typ = List typ_p

    met rec many (^dyn r) =
        match p s with
        | .Succ, x when (state < s.pos) -> many <| typ (.ListCons (x, r))
        | .Succ, _ when (state = s.pos) -> ret (.FatalFail, "Many parser succeeded without changing the parser state. Unless the computation had been aborted, the parser would have gone into an infinite loop.")
        | .Fail, _ when (state = s.pos) -> ret (.Succ, r)
        | .Fail, _ -> ret (.Fail, (pos, "many"))
        | .FatalFail, _ as x -> ret x
        : ParserResult typ

    many <| typ .ListNil

met rec spaces s ret =
    inl c = pchar s
    
    if is_whitespace c || is_newline c then spaces s
    else ret (.Succ,())

inl tuple2 a b s ret =
    a s <| function
    | .Succ, a ->
        b s <| function
        | .Succ, b -> ret (.Succ, (a, b))
        | x -> ret x
    | x -> ret x

inl (>>=) a b s ret =
    a s <| function
    | .Succ, x -> b x s ret
    | x -> ret x

inl (|>>) a f = a >>= fun x s ret -> ret (.Succ, f x)

/// ---

inl parse_int = tuple2 pint64 spaces |>> fst
inl parse_ints = many int64 parse_int
    """


printfn "%A" (spiral_codegen [] test23)

//