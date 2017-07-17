﻿#load "SpiralV5Parser_v3a.fsx"

open SpiralV5Language_v10c
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

//    let rec_ty_definitions = d0()
//    let rec_ty_tag x = def_proc rec_ty_definitions x
    let print_rec_ty v = sprintf "RecTy%i" v

    let state (buffer: Buf) x = buffer.Add <| Statement x
    let enter' (buffer: Buf) f =
        buffer.Add Indent
        f()
        buffer.Add Dedent
    let enter (buffer: Buf) f = 
        enter' buffer <| fun _ -> 
            match f() with
            | "" -> ()
            | s -> state buffer s

    let rec type_unit_is = function
        | VVT [] | TypeConstructorT _ | LitT _ | ForCastT _ | DotNetAssembly _ | DotNetRuntimeTypeT _ -> true
        | UnionT _ | RecT _ | DotNetTypeInstanceT _ | ClosureT _ | PrimT _ -> false
        | ModuleT env | FunctionT(env,_) -> Map.forall (fun _ -> type_unit_is) env
        | VVT t -> List.forall type_unit_is t

    let (|Unit|_|) x = if type_unit_is x then Some () else None

    let rec print_type = function
        | Unit -> "unit"
        | FunctionT(env,_) | ModuleT env -> print_env_ty env
        | VVT t -> print_tuple t
        | UnionT t -> print_union_ty t
        | RecT t -> print_rec_ty t
        | DotNetTypeInstanceT t ->
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
        | TypeConstructorT _ | LitT _ | ForCastT _ | DotNetAssembly _ | DotNetRuntimeTypeT _ -> 
            failwith "Should be covered in Unit."
                

    and print_dotnet_instance_type (x: System.Type) =
        if x.GenericTypeArguments.Length > 0 then
            [|
            x.Namespace 
            x.Name.Split '`' |> Array.head
            "<"
            Array.map (dotnet_type_to_ty globals.memoized_dotnet_types >> print_type) x.GenericTypeArguments |> String.concat ","
            ">"
            |] |> String.concat null
        else
            x.Namespace + x.Name

    let print_tyv (tag,ty) = 
        match ty with
        | Unit -> failwith "Unit type variables should never be printed."
        | _ -> sprintf "var_%i" tag
    let print_tyv_with_type (tag,ty as v) = sprintf "(%s: %s)" (print_tyv v) (print_type ty)
    let print_method tag = sprintf "method_%i" tag

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
        
        let rec if_ cond tr fl =
            let print_if () =
                sprintf "if %s then" (codegen cond) |> state
                enter <| fun _ -> codegen tr
                "else" |> state
                enter <| fun _ -> codegen fl

            match get_type tr with
            | Unit -> print_if (); ""
            | t ->
                let if_var = print_tyv_with_type (get_tag(), t)
                sprintf "let %s =" if_var |> state
                enter' <| fun _ -> print_if()
                if_var

        let make_struct l on_empty on_rest =
            Seq.choose (fun x -> let x = codegen x in if x = "" then None else Some x) l
            |> String.concat ", "
            |> function
                | "" -> on_empty()
                | x -> on_rest x

        let if_not_unit ty f = if type_unit_is ty then "" else f()

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
            let args = Set.toList !used_vars |> List.map print_tyv |> String.concat ", "
            let method_name = print_method tag
            sprintf "%s(%s)" method_name args
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
        | TyEnv(env_term,(FunctionT(env_ty,_) | ModuleT env_ty)) ->
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
            let tag = 
                match get_type v with
                | RecT tag -> tag
                | UnionT tys -> union_ty_tag tys
                | _ -> failwith "impossible"

            let match_var = sprintf "match_var_%i" (get_tag())
            sprintf "let %s: %s =" match_var (print_type t) |> state
            enter' <| fun _ ->
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
            match_var

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

        

        // Cuda kernel constants
//        | TyOp(Syncthreads,[],_) -> state "syncthreads();"; ""

        | TyOp _ as x -> failwithf "Missing TyOp case. %A" x

    let print_rec_definition (buffer: Buf) ty tag =
        let state = state buffer
        let enter' = enter' buffer
        let enter = enter buffer

        sprintf "and %s =" (print_rec_ty tag) |> state
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

    let print_union_definition (buffer: Buf) tys tag =
        let state = state buffer
        let enter' = enter' buffer
        let enter = enter buffer

        let tys = Set.toList tys

        sprintf "and %s =" (print_union_ty' tag) |> state
        enter' <| fun _ ->
            List.iteri (fun i -> function
                | Unit -> "| " + print_union_case tag i |> state
                | x -> sprintf "| %s of %s" (print_union_case tag i) (print_type x) |> state) tys

    let print_struct_definition (buffer: Buf) iter fold name tys tag =
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

        sprintf "and %s =" name |> state
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
        let args = 
            Set.toList !args
            |> List.map print_tyv_with_type
            |> String.concat ", "

        sprintf "%s %s(%s): %s =" prefix method_name args (print_type (get_type body)) |> state
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

    "type DefinitionsStarter() = class end" |> state definitions_buffer
    for x in globals.memoized_types do
        let tag,ty = x.Key, x.Value
        print_rec_definition definitions_buffer ty tag

    for x in union_ty_definitions do
        let tys,tag = x.Key, x.Value
        print_union_definition definitions_buffer tys tag

    for x in env_ty_definitions do
        let tys, tag = x.Key, x.Value
        let tuple_name = print_env_ty' tag
        print_struct_definition definitions_buffer Map.iter Map.fold tuple_name tys tag

    for x in tuple_definitions do
        let tys, tag = x.Key, x.Value
        let tuple_name = print_tuple' tag
        let fold f s l = List.fold (fun (i,s) ty -> i+1, f s (string i) ty) (0,s) l |> snd
        let iter f l = List.iteri (fun i x -> f (string i) x) l
        print_struct_definition definitions_buffer iter fold tuple_name tys tag
    
    // Definitions need a pass through the AST in order to be memoized. Hence they are printed first in 
    // actual code while here in the codegen that is done last. Here I just swap the buffers.
    definitions_buffer.AddRange method_buffer
    process_statements definitions_buffer

open FParsec

let spiral_codegen aux_modules main_module = 
    let rec parse_modules xs on_fail ret =
        let p x on_fail ret =
            match SpiralV5Parser_v3a.spiral_parse x with
            | Success(r,_,_) -> ret r
            | Failure(er,_,_) -> on_fail er
        match xs with
        | (name,code as x) :: xs -> 
            p x on_fail <| fun r -> 
                parse_modules xs on_fail <| fun rs ->
                    l name r None rs |> ret
        | [] -> p main_module on_fail ret

    let code =
        let d = Dictionary()
        let f (name,code: string) = d.Add(name, code.Split [|'\n'|])
        Seq.iter f aux_modules
        f main_module
        d
     
    parse_modules aux_modules Fail (fun r -> spiral_typecheck code r Fail (print_program >> Succ))

let test1 =
    "test1",
    """
inl a = 5
inl b = 10
a + b
    """

let test2 =
    "test2",
    """
met a () = 5
met b () = 10
a () + b ()
    """

let test3 =
    "test3",
    """
met a = 5
met b = 10
a + b
    """

let test4 =
    "test4",
    """
met f (a b) (c d) = (a+c,b+d)
met &(q (a b)) = 1,2
met &(w (c d)) = 3,4
f q w
    """

let test5 =
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

let fib =
    "fib",
    """
met rec fib x = 
    if x <= 0 then 0 else 
        // Without this it crashes now that I've added partial evaluation on arithmetic operations. 
        // Later, I will add a special op for making static stuff dynamic.

        // Maybe adding if_static as a keyword would not be a bad idea either.
        met x = x
        fib (x-1) + fib (x-2)
    : x
fib 1
    """

let test6 =
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
let test7 =
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

let test8 =
    "test8",
    """
met x =
    inl option_int = type (.Some, 1) |> union (type .None)
    option_int (.Some, 10)
match x with
| (.Some x) -> x
| .None -> 0
    """

let test9 =
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

let test10 = // This particular pattern is the worst case for partially evaluated pattern matchers.
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

let test11 = 
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

let test12 =
    "test12",
    """
met rec t x = type (union (type (.V, x)) (type (.Add, t x, t x)))
t 1 (.V, 5)
    """

let r = spiral_codegen [] test12
printfn "%A" r

