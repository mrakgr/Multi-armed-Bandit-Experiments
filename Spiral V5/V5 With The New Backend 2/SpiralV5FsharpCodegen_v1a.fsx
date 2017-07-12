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
        | VVT [] | TypeConstructorT _ | LitT _ | ForCastT _ | DotNetAssembly _ | DotNetRuntimeTypeT _ -> false
        | RecT _ | DotNetTypeInstanceT _ | ClosureT _ | PrimT _ -> true
        | ModuleT env | FunctionT(env,_) -> Map.forall (fun _ -> type_unit_is) env
        | UnionT set -> Set.forall type_unit_is set
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
        
        let print_tyv (tag,ty) = 
            match ty with
            | Unit -> failwith "Unit type variables should never be printed."
            | _ -> sprintf "var_%i" tag
        let print_tyv_with_type (tag,ty as v) = sprintf "(%s: %s)" (print_tyv v) (print_type ty)
        let print_method tag = sprintf "method_%i" tag

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

        let make_struct l =
            Seq.choose (fun x -> let x = codegen x in if x = "" then None else Some x) l
            |> String.concat ", "

        let inline if_not_unit ty f =
            match ty with
            | Unit -> ""
            | _ -> f()

        match expr with
        | TyV (_, Unit) -> ""
        | TyV v -> print_tyv v
        | TyOp(If,[cond;tr;fl],t) -> if_ cond tr fl
        | TyLet(LetInvisible, _, _, rest, _) -> codegen rest
        | TyLet(_,(_,Unit),b,rest,_) -> 
            let b = codegen b
            if b <> "" then sprintf "%s;" b |> state
            codegen rest
//        | TyLet(_,_, TyOp(MSet,[a;b],_),rest,_) -> sprintf "%s = %s;" (codegen a) (codegen b) |> state; codegen rest
        | TyLet(_,tyv,b,rest,_) -> sprintf "let %s = %s" (print_tyv_with_type tyv) (codegen b) |> state; codegen rest
        | TyLit x -> print_value x
        | TyMemoizedExpr(MemoMethod,used_vars,renamer,tag,_) ->
            let args = Set.toList !used_vars |> List.map print_tyv |> String.concat ", "
            let method_name = print_method tag
            sprintf "%s(%s)" method_name args
//        | TyMemoizedExpr(MemoClosure,_,_,tag,_) -> sprintf "(&%s)" (print_method tag)
        | TyVV(l,(VVT t)) -> make_struct l |> sprintf "make_struct_%i(%s)" (tuple_tag t)
        | TyVV(l,_) -> failwith "The type of TyVT should always be VTT." // TODO: Make creating union types work.

        | TyEnv(env_term,(FunctionT(env_ty,_) | ModuleT env_ty)) ->
            Map.toArray env_term
            |> Array.map snd
            |> make_struct |> sprintf "make_struct_%i(%s)" (env_ty_tag env_ty)
        | TyEnv(env_term,_) -> failwith "Can't be any other type."

        | TyOp(Apply,[a;b],t) -> 
            // Apply during codegen is only used for applying closures.
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
        
    let print_struct_definition (buffer: Buf) iter fold name tys tag =
        let state = state buffer
        let enter' = enter' buffer
        let enter = enter buffer

        "[<Struct>]" |> state
        sprintf "type %s =" name |> state
        enter <| fun _ -> iter (fun k ty -> 
            match ty with
            | Unit -> ()
            | _ -> sprintf "%s mem_%s" (print_type ty) k |> state) tys; ""

        let args =
            fold (fun s k ty -> 
                match ty with
                | Unit -> s
                | _ -> sprintf "%s arg_mem_%s" (print_type ty) k :: s) [] tys
            |> List.rev
            |> String.concat ", "

        sprintf "new(%s) = {" args |> state
            //sprintf "__device__ __forceinline__ %s make_struct_%i(%s){" name tag args |> state
            
        enter' <| fun _ ->
            sprintf "%s tmp;" name |> state
            iter (fun k -> function
                | Unit -> ()
                | _ -> sprintf "tmp.mem_%s = mem_%s;" k k |> state) tys
            "return tmp;" |> state
        "}" |> state

    ()
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