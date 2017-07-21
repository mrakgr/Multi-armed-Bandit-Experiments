#if INTERACTIVE
#load "SpiralV5Parser_v3a.fsx"
#endif

module Spiral.Codegen

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
        | VVT [] | TypeConstructorT _ | LitT _ | ForCastT _ | DotNetAssemblyT _ | DotNetRuntimeTypeT _ -> true
        | UnionT _ | RecT _ | DotNetTypeInstanceT _ | DotNetTypeAndMethodInstanceT _ | ClosureT _ | PrimT _ -> false
        | TyEnvT env -> Map.forall (fun _ -> is_unit) env
        | VVT t -> List.forall is_unit t

    let (|Unit|_|) x = if is_unit x then Some () else None

    let rec print_type = function
        | Unit -> "unit"
        | TyEnvT env -> print_env_ty env
        | VVT t -> print_tuple t
        | UnionT t -> print_union_ty t
        | RecT t -> print_rec_ty t
        | DotNetTypeInstanceT t | DotNetTypeAndMethodInstanceT (t,_) ->
            globals.memoized_dotnet_types |> snd |> fun x -> x.[t]
            |> print_dotnet_instance_type
        | ClosureT(a,b) -> sprintf "%s -> %s" (print_type a) (print_type b)
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
            | StringT -> "string"
            | BoolT -> "bool"
        | TypeConstructorT _ | LitT _ | ForCastT _ | DotNetAssemblyT _ | DotNetRuntimeTypeT _ -> 
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
        Set.toList !args |> List.filter (snd >> is_unit >> not)
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

        let (|TyDotNetTypeInstance|) = function
            | DotNetTypeInstanceT x -> map_rev_dotnet globals.memoized_dotnet_types x
            | _ -> failwith "impossible"

        let (|DotNetPrintedArgs|) x = List.map codegen x |> List.filter ((<>) "") |> String.concat ", "

        match expr with
        | TyV (_, Unit) -> ""
        | TyV v -> print_tyv v
        | TyOp(If,[cond;tr;fl],t) -> if_ cond tr fl
        | TyLet(LetInvisible, _, _, rest, _) -> codegen rest
        | TyLet(_,(_,Unit),b,rest,_) ->
            let b = codegen b
            if b <> "" then sprintf "%s" b |> state
            codegen rest
//        | TyLet(_,_, TyOp(MSet,[a;b],_),rest,_) -> sprintf "%s = %s;" (codegen a) (codegen b) |> state; codegen rest
        | TyLet(_,tyv,b,rest,_) -> sprintf "let %s = %s" (print_tyv_with_type tyv) (codegen b) |> state; codegen rest
        | TyLit x -> print_value x
        | TyMemoizedExpr(MemoMethod,used_vars,renamer,tag,_) ->
            let method_name = print_method tag
            sprintf "%s(%s)" method_name (print_args used_vars)
//        | TyMemoizedExpr(MemoClosure,_,_,tag,_) -> sprintf "(&%s)" (print_method tag)
        | TyOp(TypeConstructorApply,[x & TyV(_,t)],UnionT tys) -> union_process_var (print_union_case (union_ty_tag tys)) (x,t) tys
        | TyOp(TypeConstructorApply,[x & TyV(_,t)],RecT tag) ->
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
        | TyOp(Apply,[a;b],t) -> 
            // Apply during codegen is only used for applying closures.
            // There is one level of flattening in the outer arguments.
            let b = tuple_field b |> List.map codegen |> String.concat ", "
            sprintf "%s(%s)" (codegen a) b
        | TyOp(Case,v :: cases,t) ->
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

//        | TyOp(ShuffleXor,[x;y],_) -> sprintf "cub::ShuffleXor(%s, %s)" (codegen x) (codegen y)
//        | TyOp(ShuffleUp,[x;y],_) -> sprintf "cub::ShuffleUp(%s, %s)" (codegen x) (codegen y)
//        | TyOp(ShuffleDown,[x;y],_) -> sprintf "cub::ShuffleDown(%s, %s)" (codegen x) (codegen y)
//        | TyOp(ShuffleIndex,[x;y],_) -> sprintf "cub::ShuffleIndex(%s, %s)" (codegen x) (codegen y)

        | TyOp(Neg,[a],t) -> sprintf "(-%s)" (codegen a)
        | TyOp(VVIndex,[a;b],t) -> if_not_unit t <| fun _ -> sprintf "%s.mem_%s" (codegen a) (codegen b)
        | TyOp(EnvUnseal,[r; TyLit (LitString k)], t) -> if_not_unit t <| fun _ -> sprintf "%s.mem_%s" (codegen r) k
//        | TyOp(ArrayIndex,[a;b],t) -> print_array true a b
//        | TyOp(ArrayUnsafeIndex,[a;b],t) -> print_array false a b
        | TyOp(Log,[x],_) -> sprintf "log(%s)" (codegen x)
        | TyOp(Exp,[x],_) -> sprintf "exp(%s)" (codegen x)
        | TyOp(Tanh,[x],_) -> sprintf "tanh(%s)" (codegen x)

        | TyOp(DotNetTypeConstruct,[TyTuple (DotNetPrintedArgs args)], TyDotNetTypeInstance instance_type) ->
            let ins = print_dotnet_instance_type instance_type
            sprintf "%s(%s)" ins args
        | TyOp(DotNetTypeCallMethod,[v; TyLit (LitString method_name); TyTuple(DotNetPrintedArgs method_args)],ret_type) ->
            sprintf "%s.%s(%s)" (codegen v) method_name method_args

        // Cuda kernel constants
//        | TyOp(Syncthreads,[],_) -> state "syncthreads();"; ""

        | TyOp _ as x -> failwithf "Missing TyOp case. %A" x

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

    let print_method (buffer: Buf) is_first (body,tag,args) = 
        let state = state buffer
        let enter' = enter' buffer
        let enter = enter buffer

        let prefix = if is_first then "let rec" else "and"
        let method_name = print_method tag
        sprintf "%s %s(%s): %s =" prefix method_name (print_args args) (print_type (get_type body)) |> state
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
        | MemoMethodDone (e, tag, args) -> print_method method_buffer is_first (e, tag, args); false
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
met f (a b) (c d) = (a+c,b+d)
met &(q (a b)) = 1,2
met &(w (c d)) = 3,4
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
    | ^op1 (.Some x) -> x
    | ^op2 (.Some x) -> x
    | ^op3 (.Some x) -> x

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
    inl option_int = type (.Some, 1) |> union (type .None)
    option_int (.Some, 10)
match x with
| (.Some x) -> x
| .None -> 0
    """

let test9 = // Does the partial evaluator optimize unused match cases?
    "test9",
    """
inl ab = type .A |> union (type .B)
met x = (ab .A, ab .A, ab .A)
match x with
| (.A _ _) -> 1
| (_ .A _) -> 2
| (_ _ .A) -> 3
| _ -> 4
    """

let test10 = // The worst case for partially evaluated pattern matchers.
    "test10",
    """
inl ab = type .A |> union (type .B)
met x = (ab .A, ab .A, ab .A, ab .A)
match x with
| (.A .A _) -> 1
| (_ _ .A .A) -> 2
| (.A .B .A .B) -> 3
| _ -> 4
    """

let test11 = // Do the nested patterns work on dynamic data?
    "test1",
    """
inl a = type (1,2)
inl b = type (1,a,a)
met x = b (1, a (2,3), a (4,5))
match x with
| (_ (x _) (_ y)) -> x + y
| (_ _ _) -> 0
| (_) -> 0
    """

let test12 = // Does recursive pattern matching work on static data?
    "test12",
    """
inl rec p = function
    | (.Some x) -> p x
    | .None -> 0
p (.Some, .None)
    """

let test13 = // A more complex interpreter example on static data.
    "test13",
    """
met rec expr x = type (type (.V, x) 
                       |> union (type (.Add, expr x, expr x))
                       |> union (type (.Mult, expr x, expr x)))
inl int_expr = expr int64
inl v x = int_expr (.V, x)
inl add a b = int_expr (.Add, a, b)
inl mult a b = int_expr (.Mult, a, b)
inl a = add (v 1) (v 2)
inl b = add (v 3) (v 4)
inl c = mult a b
inl rec interpreter_static = function
    | (.V x) -> x
    | (.Add a b) -> interpreter_static a + interpreter_static b
    | (.Mult a b) -> interpreter_static a * interpreter_static b
interpreter_static c
    """

let test14 = // Does recursive pattern matching work on partially static data?
    "test14",
    """
met rec expr x = type (type (.V, x) 
                       |> union (type (.Add, expr x, expr x))
                       |> union (type (.Mult, expr x, expr x)))
inl int_expr = expr int64
inl v x = int_expr (.V, x)
inl add a b = int_expr (.Add, a, b)
inl mult a b = int_expr (.Mult, a, b)
met a = add (v 1) (v 2)
met b = add (v 3) (v 4)
inl c = mult a b
met rec inter x = 
    match x with
    | (.V x) -> x
    | (.Add a b) -> inter a + inter b
    | (.Mult a b) -> inter a * inter b
    : 0
inter c
    """

let test15 = 
    "test15",
    """
inl system = load_assembly .mscorlib
inl builder_type = lit_lift "System.Text.StringBuilder" |> system 
inl b = builder_type ("Qwe", 128i32)
inl a = b .Append >> ignore
a 123
a 123i16
a "qwe"
inl str = b.ToString()
inl console = 
    inl c = lit_lift "System.Console" |> system
    function
    | method (args) -> c (method :: args)
    | method arg -> c (method, arg)
console .Write str
    """

let test16 =
    "test16",
    """
//inl f = function
//    | method (args) -> 1
//    | method arg -> 2

inl f =
    inl method ->
        function
        | (args) -> 1
        | arg -> 2

f .Hello (1,2,3)
    """

printfn "%A" (spiral_codegen [] test16)

System.Console.Write(123)