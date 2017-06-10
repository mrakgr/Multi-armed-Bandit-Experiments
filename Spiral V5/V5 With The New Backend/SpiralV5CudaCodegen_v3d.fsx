#load "SpiralV5Parser_v2b.fsx"

open SpiralV5Language_v9b
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

    let sealed_env_definitions = d0()
    let sealed_env_tag x = def_proc sealed_env_definitions x
    let print_sealed_env' v = sprintf "sealed_env_%i" v
    let print_sealed_env t = sealed_env_tag t |> print_sealed_env'

    let closure_type_definitions = d0()
    let closure_type_tag x = def_proc closure_type_definitions x
    let print_fun_pointer_type' tag = sprintf "fun_pointer_type_%i" tag
    let print_fun_pointer_type typ = closure_type_tag typ |> print_fun_pointer_type'

    let rec print_type r = 
        match r with
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
        | ClosureT(a,r) -> print_fun_pointer_type (a,r)
        | SealedFunctionT(env,_) | SealedModuleT env -> print_sealed_env env
        | VVT ([], _) -> "void"
        | VVT (t, _) -> print_tuple t
        | LocalPointerT x | SharedPointerT x | GlobalPointerT x -> sprintf "%s *" (print_type x)
        | FunctionT _ -> failwith "Can't print function types directly."
        | ModuleT _ | ForModuleT _ | ForCastT _ -> failwithf "Can't print the type of %A." r

    let print_tyv (tag,_) = sprintf "var_%i" tag
    let print_tyv_with_type (tag,ty as v) = sprintf "%s %s" (print_type ty) (print_tyv v)
    let print_method tag = sprintf "method_%i" tag

    let print_value = function
        | LitUInt8 x -> string x 
        | LitUInt16 x -> string x 
        | LitUInt32 x -> string x 
        | LitUInt64 x -> string x 
        | LitInt8 x -> string x
        | LitInt16 x -> string x
        | LitInt32 x -> string x
        | LitInt64 x -> string x
        | LitFloat32 x -> string x
        | LitFloat64 x -> string x
        | LitBool x -> if x then "1" else "0"
        | LitString x -> sprintf "\"%s\"" x
        | ThreadIdxX -> "threadIdx.x"
        | ThreadIdxY -> "threadIdx.y"
        | ThreadIdxZ -> "threadIdx.z"
        | BlockIdxX -> "blockIdx.x"
        | BlockIdxY -> "blockIdx.y"
        | BlockIdxZ -> "blockIdx.z"

    let (|Unit|_|) l = 
        match l with
        | SealedFunctionT (env, _) | SealedModuleT env when env.IsEmpty -> Some ()
        | FunctionT(env,_) | ModuleT env when env.IsEmpty -> Some ()
        | VVT([],_) -> Some ()
        | _ -> None

    let rec print_array is_print_assertion a b =
        let ar_sizes =
            match a with
            | Array(_,sizes,_) -> List.map codegen sizes
            | _ -> failwith "impossible"

        let i = tuple_field b |> List.map codegen

        if is_print_assertion then
            List.map2 (fun size i -> sprintf "%s >= 0 && %s < %s" i i size) ar_sizes i
            |> String.concat " + "
            |> state

        // Array cases
        let index = 
            let rec loop = function
                | None, s :: sx, i :: ix ->
                    loop (Some(sprintf "(%s) * %s" i s),sx,ix)
                | None, [], [i] ->
                    i
                | Some p, s :: sx, i :: ix ->
                    loop (Some(sprintf "(%s + (%s)) * %s" p i s),sx,ix)
                | Some p, [], [i] ->
                    sprintf "%s + (%s)" p i
                | _ -> failwith "invalid state"
            if i.IsEmpty = false then loop (None,List.tail ar_sizes,i)
            else "0"
        sprintf "%s[%s]" (codegen a) index

    and print_array_declaration is_shared typ v ar_sizes =   
        let typ = print_type typ
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
            | Unit -> 
                (fun x -> enter <| fun _ -> sprintf "%s;" (codegen x)), ""
            | _ -> 
                let r = get_tag() |> sprintf "if_var_%i"
                (fun x -> enter <| fun _ -> sprintf "%s = %s;" r (codegen x)), r
        sprintf "%s %s;" (print_type t) r |> state
        sprintf "if (%s) {" (codegen cond) |> state
        p tr
        "} else {" |> state
        p fl
        "}" |> state
        r

    and make_struct l =
        Seq.choose (fun x -> let x = codegen x in if x = "" then None else Some x) l
        |> String.concat ", "

    and codegen = function
        | TyVV([],_) -> ""
        | TyType _ -> ""
        | TyV v -> print_tyv v
        | TyOp(If,[cond;tr;fl],t) -> if_ cond tr fl t
        | TyLet(LetInvisible, _, _, rest, _) -> codegen rest
        | TyLet(_, v, TyOp(ArrayCreate, [Array(ar_typ,ar_sizes,typ)], _), rest, _) ->
            match ar_typ with Local | Global -> false | Shared -> true
            |> fun ar_typ -> print_array_declaration ar_typ typ v ar_sizes
            codegen rest
        | TyLet(_,_,(TyVV([],_) | TyType _), rest, _) -> codegen rest
        | TyLet(_,(_,Unit),b,rest,_) -> 
            let b = codegen b
            if b <> "" then sprintf "%s;" b |> state
            codegen rest
        | TyLet(_,_, TyOp(MSet,[a;b],_),rest,_) -> sprintf "%s = %s;" (codegen a) (codegen b) |> state; codegen rest
        | TyLet(_,tyv,b,rest,_) -> sprintf "%s = %s;" (print_tyv_with_type tyv) (codegen b) |> state; codegen rest
        | TyLit x -> print_value x
        | TyMemoizedExpr(MemoMethod,used_vars,_,tag,_) ->
            let args = Set.toList !used_vars |> List.map print_tyv |> String.concat ", "
            let method_name = print_method tag
            sprintf "%s(%s)" method_name args
        | TyMemoizedExpr(MemoClosure,_,_,tag,_) -> sprintf "(&%s)" (print_method tag)
        | TyVV(l,(VVT (t, _))) -> make_struct l |> sprintf "make_struct_%i(%s)" (tuple_tag t)
        | TyVV(l,_) -> failwith "The type of TyVT should always be VTT."

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

        | TyOp(ShuffleXor,[x;y],_) -> sprintf "cub::ShuffleXor(%s, %s)" (codegen x) (codegen y)
        | TyOp(ShuffleUp,[x;y],_) -> sprintf "cub::ShuffleUp(%s, %s)" (codegen x) (codegen y)
        | TyOp(ShuffleDown,[x;y],_) -> sprintf "cub::ShuffleDown(%s, %s)" (codegen x) (codegen y)
        | TyOp(ShuffleIndex,[x;y],_) -> sprintf "cub::ShuffleIndex(%s, %s)" (codegen x) (codegen y)

        | TyOp(Neg,[a],t) -> sprintf "(-%s)" (codegen a)
        | TyOp(VVIndex,[a;b],t) -> sprintf "%s.mem_%s" (codegen a) (codegen b)
        | TyOp(ArrayIndex,[a;b],t) -> print_array true a b
        | TyOp(ArrayUnsafeIndex,[a;b],t) -> print_array false a b
        | TyOp(Log,[x],_) -> sprintf "log(%s)" (codegen x)
        | TyOp(Exp,[x],_) -> sprintf "exp(%s)" (codegen x)
        | TyOp(Tanh,[x],_) -> sprintf "tanh(%s)" (codegen x)

        | TyOp(TypeSeal,[r],(SealedFunctionT(t,_) | SealedModuleT t)) ->
            match get_type r with
            | FunctionT(env,_) | ModuleT env ->
                if env.IsEmpty = false then
                    Map.toArray env
                    |> Array.map snd
                    |> make_struct |> sprintf "make_struct_%i(%s)" (sealed_env_tag t)
                else ""
            | _ -> failwith "impossible"

        | TyOp(EnvUnseal,[r; TyLit (LitString k)], Unit) -> ""
        | TyOp(EnvUnseal,[r; TyLit (LitString k)], _) -> sprintf "%s.mem_%s" (codegen r) k

        // Cuda kernel constants
        | TyOp(Syncthreads,[],_) -> state "syncthreads();"; ""

        | TyOp _ as x -> failwithf "Missing TyOp case. %A" x

    let print_closure_a a = tuple_field_ty a |> List.map print_type |> String.concat ", "

    let print_closure_type_definition (a,r) tag =
        sprintf "typedef %s(*%s)(%s);" (print_type r) (print_fun_pointer_type' tag) (print_closure_a a) |> state

    let print_struct_definition iter fold name tys tag =
        sprintf "struct %s {" name |> state
        enter <| fun _ -> iter (fun k ty -> 
            match ty with
            | Unit -> ()
            | _ -> sprintf "%s mem_%s;" (print_type ty) k |> state) tys; ""
        "};" |> state

        let print_args =
            let args =
                fold (fun s k ty -> 
                    match ty with
                    | Unit -> s
                    | _ -> sprintf "%s mem_%s" (print_type ty) k :: s) [] tys
                |> List.rev
                |> String.concat ", "
            sprintf "__device__ __forceinline__ %s make_struct_%i(%s){" name tag args |> state
            
        enter' <| fun _ ->
            sprintf "%s tmp;" name |> state
            iter (fun k -> function
                | Unit -> ()
                | _ -> sprintf "tmp.mem_%s = mem_%s;" k k |> state) tys
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
        sprintf "%s %s %s(%s) {" prefix (print_type (get_type body)) method_name args |> state

        enter' <| fun _ ->
            match codegen body with
            | "" -> ()
            | s -> sprintf "return %s;" s |> state
        "}" |> state

    """#include "cub/cub.cuh" """ |> state
    """#include <assert.h> """ |> state
    """extern "C" {""" |> state
        
    enter' <| fun _ ->
        with_channel CodegenChannels.Code <| fun _ ->
            for x in imemo do print_method (memo_value x.Value)

        with_channel CodegenChannels.Definitions <| fun _ ->
            for x in tuple_definitions do 
                let tys, tag = x.Key, x.Value
                let tuple_name = print_tuple' tag
                let fold f s l = List.fold (fun (i,s) ty -> i+1, f s (string i) ty) (0,s) l |> snd
                let iter f l = List.iteri (fun i x -> f (string i) x) l
                print_struct_definition iter fold tuple_name tys tag

            for x in sealed_env_definitions do
                let tys, tag = x.Key, x.Value
                let tuple_name = print_sealed_env' tag
                print_struct_definition Map.iter Map.fold tuple_name tys tag

            for x in closure_type_definitions do print_closure_type_definition x.Key x.Value

        // I am just swapping the positions so the definitions come first in the printed code.
        // Unfortunately, getting the definitions requires a pass through the AST first
        // so I can't just print them at the start.
        add_channels_a_to_main CodegenChannels.Definitions
        add_channels_a_to_main CodegenChannels.Code
        
    "}" |> state
        
    cur_program () |> process_statements

open SpiralV5Parser_v2b
open FParsec

let spiral_codegen dims aux_modules main_module = 
    let rec parse_modules xs on_fail ret =
        let p x on_fail ret =
            match spiral_parse x with
            | Success(r,_,_) -> ret r
            | Failure(er,_,_) -> on_fail er
        match xs with
        | (name,code as x) :: xs -> 
            p x on_fail <| fun r -> 
                parse_modules xs on_fail <| fun rs ->
                    l (S name) r None rs |> ret
        | [] -> p main_module on_fail ret

    let code =
        let d = Dictionary()
        let f (name,code: string) = d.Add(name, code.Split [|'\n'|])
        Seq.iter f aux_modules
        f main_module
        d
     
    parse_modules aux_modules Fail
        (fun r ->
            spiral_typecheck code dims r Fail <| fun (_,memo) ->
                print_method_dictionary memo |> Succ)

let fib =
    "Fib",
    """
fun rec fib x = 
    if x <= 0 then 0 else fib (x-1) + fib (x-2)
    : x
fib 5
    """

let fib_y =
    "fib_y",
    """
fun rec y f x = f (y f) x
inl fib r x = 
    if x <= 0 then 0 else r (x-1) + r (x-2)
    : x
y fib 5
    """

let fib_acc =
    "fib_acc",
    """
inl fib n =
    fun rec fib n a b = 
        if n >= 0 then fib (n-1) b (a+b) else a
        : a
    fib n 0 1
fib 2
    """

let fib_acc_y = // The Y Combinator needs all the arguments when dealing with methods.
    "fib_acc_y",
    """
fun rec y f n a b = f (y f) n a b
inl fib n =
    inl fib r n a b = 
        if n >= 0 then r (n-1) b (a+b) else a
        : a // The return type is the type of `a`
    y fib n 0 1
fib 5
    """

let clo1 =
    "clo1",
    """
inl add (x,y),_ = x+y
fun t() =
    inl f = add `((3*2,4/3),(5-1,5))
    f ((1,1),(2,2)), f((2,2),(2,2))
t()
    """

let rec1 =
    """
fun rec meth () =
    if false then
        if false then
            if true then
                meth()
            else
                meth()
        else
            88
    else
        meth()
fun top() = meth()
top()
    """

let fib_acc_er =
    "fibonacci_acc_er",
    """
inl fib n =
    fun rec fib n a b = 
        if n >= 0 then fib (n-1) b (a+b) else a + 8.8
        : b
    fib n 0 1
fib 2
    """

let tuple_library =
    "tuple",
    """
inl ignore _ = ()
inl id x = x

inl rec tuple_foldl f s l =
    typecase l with
    | x :: xs -> tuple_foldl f (f s x) xs
    | () -> s
inl rec tuple_foldr f s l =
    typecase l with
    | x :: xs -> f x (tuple_foldr f s xs)
    | () -> s
    
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
inl tuple_is_empty = typeinl
    | () -> .True()
    | _ -> .False()
inl tuple_zip, tuple_zip_irreg, tuple_unzip, tuple_unzip_irreg =
    inl vv x = x :: ()
    inl transpose l on_fail on_succ =
        inl is_all_vv_empty = tuple_forall (typeinl () -> .True() | _ -> .False())
        inl rec loop acc_total acc_head acc_tail l = 
            typecase l with
            | () :: ys ->
                typecase acc_head with
                | () ->
                    typecase is_all_vv_empty ys with
                    | .True() -> 
                        typecase acc_total with
                        | () -> errortype "Empty inputs in the inner dimension to transpose are invalid."
                        | _ -> tuple_rev acc_total |> on_succ
                    | .False() -> on_fail()
                | _ -> on_fail()
            | (x :: xs) :: ys -> loop acc_total (x :: acc_head) (xs :: acc_tail) ys
            | _ :: _ -> on_fail ()
            | () -> 
                typecase acc_tail with
                | _ :: _ -> loop (tuple_rev acc_head :: acc_total) () () (tuple_rev acc_tail)
                | _ -> tuple_rev acc_total |> on_succ
        loop () () () l
    inl zip_template on_ireg l = 
        inl rec zip l = 
            typecase l with
            | _ :: _ -> transpose l (inl _ -> on_ireg l) (tuple_map (typeinl x :: () -> zip x | x -> x))
            | _ -> errortype "Empty input to zip is invalid."
        zip l

    inl zip_reg_guard l =
        typecase tuple_forall (typeinl _ :: _ -> .False() | _ -> .True()) l with
        | .True() -> l
        | .False() -> errortype "Irregular inputs in zip."
    inl zip_reg = zip_template zip_reg_guard
    inl zip_irreg = zip_template id

    inl rec unzip_template on_irreg l = 
        inl is_all_vv x = tuple_forall (typeinl _ :: _ -> .True() | _ -> .False()) x
        inl rec unzip l =
            typecase l with
            | _ :: _ ->
                typecase is_all_vv l with
                | .True() -> transpose (tuple_map unzip l) (inl _ -> on_irreg l) id
                | .False() -> l
            | x -> errortype "Unzip called on a non-tuple."
        unzip l

    inl unzip_reg = unzip_template zip_reg_guard
    inl unzip_irreg = unzip_template id

    zip_reg, zip_irreg, unzip_reg, unzip_irreg

module
    """

let map_forward_setter = "inl i (*result, *out) -> out i <- result"
let map_backward_setter = "inl i (*result, *out) -> out i <- out i + result"

let cuda_kernels =
    "cuda_kernels",
    """
import Tuple

inl uncurry2 f (a,b) = f a b

inl index_if_array i *in =
    typecase in with
    | .Array(in,size) -> in i
    | .Array(in,()) -> in ()

inl map_load_op ins map_op i = tuple_map (index_if_array i) ins |> map_op i

fun cuda_map (setter, map_op, *n, ins, outs) =
    inl stride = gridDimX * blockDimX
    inl map_load_op = map_load_op ins map_op
        
    fun rec loop i =
        if i < n then
            tuple_zip (outs,map_load_op i) |> tuple_map (setter i)
            loop (i+stride)
        : () // unit type
    loop (blockIdxX * blockDimX + threadIdxX)

fun cuda_map_redo (map_op,(neutral_elem,reduce_op),*n,ins,outs) =
    inl stride = gridDimX * blockDimX
    inl map_load_op = map_load_op ins map_op

    fun rec loop (i, value) =
        if i < n then loop (i + stride, reduce_op value (map_load_op i))
        else value
    
    inl results = 
        inl i,n = blockIdxX * blockDimX + threadIdxX, neutral_elem
        cubBlockReduce(loop (i, n), uncurry2 reduce_op `(n,n))

    if threadIdxX = 0 then 
        tuple_zip(outs,results)
        |> tuple_map (inl (*out,*result) -> out blockIdxX <- result)
        ()

fun cuda_map_redocol_map (map_op,(neutral_elem,reduce_op),map_store_op,(*num_cols,*num_rows),ins,outs) =
    inl map_load_op = map_load_op ins map_op

    fun rec loop_col col =
        let rec loop_row (row, value) = 
            if row < num_rows then loop_row (row + blockDimX, reduce_op value (map_load_op (col,row)))
            else value

        if col < num_cols then 
            inl n = neutral_elem
            inl results = cubBlockReduce(loop_row (threadIdxX, n), uncurry2 reduce_op `(n,n))
                    
            if threadIdxX = 0 then 
                tuple_zip(outs, map_store_op results)
                |> tuple_map (inl (*out,*result) -> out col <- result)
                ()        
            loop_col (col + gridDimX)
    loop_col blockIdxX

fun cuda_mapcol (setter,map_op,(*num_cols,*num_rows),ins,outs) =
    inl map_load_op = map_load_op ins map_op

    fun rec loop_col col =
        let rec loop_row ins row = 
            if row < num_rows then 
                tuple_zip (outs, ins) |> tuple_map (setter (col,row))
                loop_row (row + blockDimX)

        if col < num_cols then 
            loop_row (map_load_op col) threadIdxX
            loop_col (col + gridDimX)

    loop_col blockIdxX
module
    """

let tup1 =
    "tup1",
    """
fun tup() = 1,2,3
fun add x,y,z = x+y+z
fun top() =
    inl x = tup()
    add x
top()
    """

let tup2 =
    "tup2",
    """
inl tup () () = 1,2,3
fun add f = 
    inl x,y,z = f()
    x+y+z
fun top() =
    inl x = tup()
    add x
top()
    """

let tup3 =
    "tup3",
    """
fun tup x y z = 
    fun () -> x*2,y*3,z*5
fun add f = 
    inl x,y,z = f()
    x+y+z
fun top() =
    inl x = tup 1 2 3
    add x
top()
    """

let fib_acc_alt =
    "fib_acc_alt",
    """
fun fib n =
    fun rec fib n = 
        fun a b () ->
            if n >= 0 then fib (n-1) b (a+b) () else a
            : a
    inl x = fib n
    inl y = x 0
    inl z = y 1
    z
    //fib n 0 1
fib 10
    """

let r = spiral_codegen default_dims [] fib_acc_alt

printfn "%A" r

