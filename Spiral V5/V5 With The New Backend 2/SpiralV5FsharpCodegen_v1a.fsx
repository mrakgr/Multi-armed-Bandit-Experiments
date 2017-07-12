#load "SpiralV5Parser_v3a.fsx"

open SpiralV5Language_v10b
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

let print_program (globals: LangGlobals) (main: TypedExpr) =
    let get_tag =
        let mutable i = 0
        fun () -> i <- i+1; i

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

    let print_main (buffer: Buf) =
        let state x = buffer.Add <| Statement x
        let enter' f = 
            buffer.Add Indent
            f()
            buffer.Add Dedent
        let enter f = 
            enter' <| fun _ -> 
                match f() with
                | "" -> ()
                | s -> state s

        let rec type_filter_unit = function
            | ModuleT env -> // I'd prefer to use choose here, but Map and Set do not have it.
                let env = Map.map (fun _ -> type_filter_unit) env |> Map.filter (fun _ -> type_unit_is)
                if env.IsEmpty then BVVT else ModuleT env
            | FunctionT(env,t) -> 
                let env = Map.map (fun _ -> type_filter_unit) env |> Map.filter (fun _ -> type_unit_is) 
                if env.IsEmpty then BVVT else FunctionT(env,t)
            | VVT t -> 
                let t = List.map type_filter_unit t |> List.filter type_unit_is
                if t.IsEmpty then BVVT else VVT t
            | UnionT t -> Set.map type_filter_unit t |> UnionT
            | RecT _ | DotNetTypeInstanceT _ | ClosureT _ | PrimT _ as x -> x
            | TypeConstructorT _ | LitT _ | ForCastT _ | DotNetAssembly _ | DotNetRuntimeTypeT _ -> BVVT

        and type_unit_is = function
            | VVT [] -> false
            | _ -> true

        let rec print_type = function
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
                failwith "Should have been filtered out. Only VVT [] should be the unit type."

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

        let print_type = type_filter_unit >> print_type

        failwith ""
    failwith ""

//        let rec print_type r = 
//            match r with
//            | PrimT x -> 
//                match x with
//                | UInt8T -> "unsigned char"
//                | UInt16T -> "unsigned short"
//                | UInt32T -> "unsigned int"
//                | UInt64T -> "unsigned long long int"
//                | Int8T -> "char"
//                | Int16T -> "short"
//                | Int32T -> "int"
//                | Int64T -> "long long int"
//                | Float32T -> "float"
//                | Float64T -> "double"
//                | BoolT -> "int"
//            | ClosureT(a,r) -> print_fun_pointer_type (a,r)
//            | ModuleT env | FunctionT(env,_) -> print_env_ty env
//            | VVT t -> print_tuple t
//
//        failwith ""
//
//    let inline if_not_unit ty f =
//        match ty with
//        | Unit -> ""
//        | _ -> f()
//
//    let print_tyv (tag,ty) = sprintf "var_%i" tag
//    let print_tyv_with_type (tag,ty as v) = sprintf "%s %s" (print_type ty) (print_tyv v)
//    let print_method tag = sprintf "method_%i" tag
//
//    let print_value = function
//        | LitUInt8 x -> string x 
//        | LitUInt16 x -> string x 
//        | LitUInt32 x -> string x 
//        | LitUInt64 x -> string x 
//        | LitInt8 x -> string x
//        | LitInt16 x -> string x
//        | LitInt32 x -> string x
//        | LitInt64 x -> string x
//        | LitFloat32 x -> string x
//        | LitFloat64 x -> string x
//        | LitBool x -> if x then "1" else "0"
//        | LitString x -> sprintf "\"%s\"" x
//        | ThreadIdxX -> "threadIdx.x"
//        | ThreadIdxY -> "threadIdx.y"
//        | ThreadIdxZ -> "threadIdx.z"
//        | BlockIdxX -> "blockIdx.x"
//        | BlockIdxY -> "blockIdx.y"
//        | BlockIdxZ -> "blockIdx.z"
//
//    let rec print_array is_print_assertion a b =
//        let ar_sizes =
//            match a with
//            | Array(_,sizes,_) -> List.map codegen sizes
//            | _ -> failwith "impossible"
//
//        let i = tuple_field b |> List.map codegen
//
//        if is_print_assertion then
//            List.map2 (fun size i -> sprintf "%s >= 0 && %s < %s" i i size) ar_sizes i
//            |> String.concat " + "
//            |> state
//
//        // Array cases
//        let index = 
//            let rec loop = function
//                | None, s :: sx, i :: ix ->
//                    loop (Some(sprintf "(%s) * %s" i s),sx,ix)
//                | None, [], [i] ->
//                    i
//                | Some p, s :: sx, i :: ix ->
//                    loop (Some(sprintf "(%s + (%s)) * %s" p i s),sx,ix)
//                | Some p, [], [i] ->
//                    sprintf "%s + (%s)" p i
//                | _ -> failwith "invalid state"
//            if i.IsEmpty = false then loop (None,List.tail ar_sizes,i)
//            else "0"
//        sprintf "%s[%s]" (codegen a) index
//
//    and print_array_declaration is_shared typ v ar_sizes =   
//        let typ = print_type typ
//        let nam = print_tyv v
//        let dim =
//            if List.isEmpty ar_sizes then "1"
//            else
//                List.map (codegen >> sprintf "%s") ar_sizes
//                |> String.concat " * "
//        
//        match is_shared with
//        | true -> sprintf "__shared__ %s %s[%s];" typ nam dim |> state
//        | false -> sprintf "%s %s[%s];" typ nam dim |> state
//
//    and if_ cond tr fl t =
//        let p, r =
//            match t with
//            | Unit -> 
//                (fun x -> enter <| fun _ -> sprintf "%s;" (codegen x)), ""
//            | _ -> 
//                let r = get_tag() |> sprintf "if_var_%i"
//                (fun x -> enter <| fun _ -> sprintf "%s = %s;" r (codegen x)), r
//        sprintf "%s %s;" (print_type t) r |> state
//        sprintf "if (%s) {" (codegen cond) |> state
//        p tr
//        "} else {" |> state
//        p fl
//        "}" |> state
//        r
//
//    and make_struct l =
//        Seq.choose (fun x -> let x = codegen x in if x = "" then None else Some x) l
//        |> String.concat ", "
//
//    and codegen x = 
//        match get_type x with
//        | Unit -> ""
//        | _ ->
//            match x with
//            | TyType _ -> failwith "Covered in Unit"
//            | TyV v -> print_tyv v
//            | TyOp(If,[cond;tr;fl],t) -> if_ cond tr fl t
//            | TyLet(LetInvisible, _, _, rest, _) -> codegen rest
//            | TyLet(_, v, TyOp(ArrayCreate, [Array(ar_typ,ar_sizes,typ)], _), rest, _) ->
//                match ar_typ with Local | Global -> false | Shared -> true
//                |> fun ar_typ -> print_array_declaration ar_typ typ v ar_sizes
//                codegen rest
//            | TyLet(_,(_,Unit),b,rest,_) -> 
//                let b = codegen b
//                if b <> "" then sprintf "%s;" b |> state
//                codegen rest
//            | TyLet(_,_, TyOp(MSet,[a;b],_),rest,_) -> sprintf "%s = %s;" (codegen a) (codegen b) |> state; codegen rest
//            | TyLet(_,tyv,b,rest,_) -> sprintf "%s = %s;" (print_tyv_with_type tyv) (codegen b) |> state; codegen rest
//            | TyLit x -> print_value x
//            | TyMemoizedExpr(MemoMethod,used_vars,renamer,tag,_) ->
//                let args = Set.toList !used_vars |> List.map print_tyv |> String.concat ", "
//                let method_name = print_method tag
//                sprintf "%s(%s)" method_name args
//            | TyMemoizedExpr(MemoClosure,_,_,tag,_) -> sprintf "(&%s)" (print_method tag)
//            | TyVV(l,(VVT (t, _))) -> make_struct l |> sprintf "make_struct_%i(%s)" (tuple_tag t)
//            | TyVV(l,_) -> failwith "The type of TyVT should always be VTT."
//
//            | TyEnv(env_term,(FunctionT(env_ty,_) | ModuleT env_ty)) ->
//                Map.toArray env_term
//                |> Array.map snd
//                |> make_struct |> sprintf "make_struct_%i(%s)" (env_ty_tag env_ty)
//            | TyEnv(env_term,_) -> failwith "Can't be any other type."
//
//            | TyOp(Apply,[a;b],t) -> 
//                // Apply during codegen is only used for applying closures.
//                // There is one level of flattening in the outer arguments.
//                let b = tuple_field b |> List.map codegen |> String.concat ", "
//                sprintf "%s(%s)" (codegen a) b
//
//            // Primitive operations on expressions.
//            | TyOp(Add,[a;b],t) -> sprintf "(%s + %s)" (codegen a) (codegen b)
//            | TyOp(Sub,[a;b],t) -> sprintf "(%s - %s)" (codegen a) (codegen b)
//            | TyOp(Mult,[a;b],t) -> sprintf "(%s * %s)" (codegen a) (codegen b)
//            | TyOp(Div,[a;b],t) -> sprintf "(%s / %s)" (codegen a) (codegen b)
//            | TyOp(Mod,[a;b],t) -> sprintf "(%s %% %s)" (codegen a) (codegen b)
//            | TyOp(LT,[a;b],t) -> sprintf "(%s < %s)" (codegen a) (codegen b)
//            | TyOp(LTE,[a;b],t) -> sprintf "(%s <= %s)" (codegen a) (codegen b)
//            | TyOp(EQ,[a;b],t) -> sprintf "(%s == %s)" (codegen a) (codegen b)
//            | TyOp(NEQ,[a;b],t) -> sprintf "(%s != %s)" (codegen a) (codegen b)
//            | TyOp(GT,[a;b],t) -> sprintf "(%s > %s)" (codegen a) (codegen b)
//            | TyOp(GTE,[a;b],t) -> sprintf "(%s >= %s)" (codegen a) (codegen b)
//            | TyOp(And,[a;b],t) -> sprintf "(%s && %s)" (codegen a) (codegen b)
//            | TyOp(Or,[a;b],t) -> sprintf "(%s || %s)" (codegen a) (codegen b)
//
//            | TyOp(ShiftLeft,[x;y],_) -> sprintf "(%s << %s)" (codegen x) (codegen y)
//            | TyOp(ShiftRight,[x;y],_) -> sprintf "(%s >> %s)" (codegen x) (codegen y)
//
//            | TyOp(ShuffleXor,[x;y],_) -> sprintf "cub::ShuffleXor(%s, %s)" (codegen x) (codegen y)
//            | TyOp(ShuffleUp,[x;y],_) -> sprintf "cub::ShuffleUp(%s, %s)" (codegen x) (codegen y)
//            | TyOp(ShuffleDown,[x;y],_) -> sprintf "cub::ShuffleDown(%s, %s)" (codegen x) (codegen y)
//            | TyOp(ShuffleIndex,[x;y],_) -> sprintf "cub::ShuffleIndex(%s, %s)" (codegen x) (codegen y)
//
//            | TyOp(Neg,[a],t) -> sprintf "(-%s)" (codegen a)
//            | TyOp(VVIndex,[a;b],t) -> if_not_unit t <| fun _ -> sprintf "%s.mem_%s" (codegen a) (codegen b)
//            | TyOp(ArrayIndex,[a;b],t) -> print_array true a b
//            | TyOp(ArrayUnsafeIndex,[a;b],t) -> print_array false a b
//            | TyOp(Log,[x],_) -> sprintf "log(%s)" (codegen x)
//            | TyOp(Exp,[x],_) -> sprintf "exp(%s)" (codegen x)
//            | TyOp(Tanh,[x],_) -> sprintf "tanh(%s)" (codegen x)
//
//            | TyOp(EnvUnseal,[r; TyLit (LitString k)], t) -> if_not_unit t <| fun _ -> sprintf "%s.mem_%s" (codegen r) k
//
//            // Cuda kernel constants
//            | TyOp(Syncthreads,[],_) -> state "syncthreads();"; ""
//
//            | TyOp _ as x -> failwithf "Missing TyOp case. %A" x
//
//    let print_closure_a a = tuple_field_ty a |> List.map print_type |> String.concat ", "
//
//    let print_closure_type_definition (a,r) tag =
//        sprintf "typedef %s(*%s)(%s);" (print_type r) (print_fun_pointer_type' tag) (print_closure_a a) |> state
//
//    let print_struct_definition iter fold name tys tag =
//        sprintf "struct %s {" name |> state
//        enter <| fun _ -> iter (fun k ty -> 
//            match ty with
//            | Unit -> ()
//            | _ -> sprintf "%s mem_%s;" (print_type ty) k |> state) tys; ""
//        "};" |> state
//
//        let print_args =
//            let args =
//                fold (fun s k ty -> 
//                    match ty with
//                    | Unit -> s
//                    | _ -> sprintf "%s mem_%s" (print_type ty) k :: s) [] tys
//                |> List.rev
//                |> String.concat ", "
//            sprintf "__device__ __forceinline__ %s make_struct_%i(%s){" name tag args |> state
//            
//        enter' <| fun _ ->
//            sprintf "%s tmp;" name |> state
//            iter (fun k -> function
//                | Unit -> ()
//                | _ -> sprintf "tmp.mem_%s = mem_%s;" k k |> state) tys
//            "return tmp;" |> state
//        "}" |> state
//
//    let print_method (body,tag,args) = 
//        let is_main = tag = 0L
//        let prefix = if is_main then "__global__" else "__device__"
//        let method_name = if is_main then "MainKernel" else print_method tag
//        let args = 
//            Set.toList !args
//            |> List.map print_tyv_with_type
//            |> String.concat ", "
//        sprintf "%s %s %s(%s) {" prefix (print_type (get_type body)) method_name args |> state
//
//        enter' <| fun _ ->
//            match codegen body with
//            | "" -> ()
//            | s -> sprintf "return %s;" s |> state
//        "}" |> state
//
//    """#include "cub/cub.cuh" """ |> state
//    """#include <assert.h> """ |> state
//    """extern "C" {""" |> state
//        
//    enter' <| fun _ ->
//        with_channel CodegenChannels.Code <| fun _ ->
//            for x in imemo do print_method (memo_value x.Value)
//
//        with_channel CodegenChannels.Definitions <| fun _ ->
//            for x in tuple_definitions do 
//                let tys, tag = x.Key, x.Value
//                let tuple_name = print_tuple' tag
//                let fold f s l = List.fold (fun (i,s) ty -> i+1, f s (string i) ty) (0,s) l |> snd
//                let iter f l = List.iteri (fun i x -> f (string i) x) l
//                print_struct_definition iter fold tuple_name tys tag
//
//            for x in env_ty_definitions do
//                let tys, tag = x.Key, x.Value
//                let tuple_name = print_env_ty' tag
//                print_struct_definition Map.iter Map.fold tuple_name tys tag
//
//            for x in closure_type_definitions do print_closure_type_definition x.Key x.Value
//
//        // I am just swapping the positions so the definitions come first in the printed code.
//        // Unfortunately, getting the definitions requires a pass through the AST first
//        // so I can't just print them at the start.
//        add_channels_a_to_main CodegenChannels.Definitions
//        add_channels_a_to_main CodegenChannels.Code
//        
//    "}" |> state
//        
//    cur_program () |> process_statements
//