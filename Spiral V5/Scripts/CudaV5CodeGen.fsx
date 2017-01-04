#load "SpiralV5.fsx"
open SpiralV5

let map_launcher_block_size = 256
let map_redocol_map_launcher_block_size = 128
let map_redo_map_launcher_block_size = 256

open ManagedCuda
open ManagedCuda.BasicTypes
open ManagedCuda.VectorTypes
open ManagedCuda.CudaBlas
open ManagedCuda.CudaRand
open ManagedCuda.NVRTC
open ManagedCuda.CudaDNN

open System
open System.Text
open System.Diagnostics
open System.IO
open System.Collections.Generic
open System.Runtime.InteropServices

let cuda_tag =
    let mutable i = 0
    fun () ->
        i <- i+1
        i

type CudaExpr =
| Statement of string
| Indent
| Dedent
| Statements of ResizeArray<CudaExpr>

let exp x = String.concat "" x
let state (program: ResizeArray<_>) x = exp x |> Statement |> program.Add
let enter (program: ResizeArray<_>) body v =
    Indent |> program.Add
    body v
    Dedent |> program.Add
let expand (program: ResizeArray<_>) x = Statements x |> program.Add

/// The outer level of the Cuda compiler language.
let cuda_kernel_module_template 
            kernel_name args method_body_macro // kernel_param
            (method_, externCBlock, include_, expand) = // kernel primitives
    include_ "thrust/tuple.h"
    include_ "cub/cub.cuh"
    externCBlock ( fun () ->
        method_ "__global__ void " kernel_name args (fun () -> expand method_body_macro)
        )

/// The inner level of the language. Standard map.
let cuda_map_template 
        n map_macro
        (class_method2, class1, typedef, ifvoid, set, is, times, plus, less_than, for_, while_, madd, lambda2, text, var, init, expand)
        = // kernel params
    for_ 
        (init "blockIdx.x * blockDim.x + threadIdx.x")
        (fun i -> less_than i n) 
        (fun i -> plus i "gridDim.x * blockDim.x")
        (fun i -> expand (map_macro i))

let map_redo_map_template 
        block_reduce_type map_load_op_macro reduce_op_macro map_store_macro n
        (class_method2, class1, typedef, ifvoid, set, eq, times, plus, less_than, for_, while_, madd, lambda2, text, var, init, expand)
        =
    let block_reduce_typedef = typedef ("cub::BlockReduce<"+block_reduce_type+", " + string map_redo_map_launcher_block_size + ">")
    let temp_storage = var "__shared__ BlockReduceT::TempStorage" ""
    let block_reduce = class1 block_reduce_typedef temp_storage

    let reduce_op = lambda2 reduce_op_macro
    let i = var "int" "blockIdx.x*blockDim.x + threadIdx.x"
    let value = var "auto" (map_load_op_macro i)
    let stride = var "const auto" "gridDim.x*blockDim.x"
    madd i stride
    while_ (less_than i n) <| fun () ->
        set value (reduce_op value (map_load_op_macro i))
        madd i stride

    let result = var "const auto" (class_method2 block_reduce "Reduce" value reduce_op)
    ifvoid (eq "threadIdx.x" "0")
           (fun () -> expand (map_store_macro result))
           (fun () -> ())

let process_statements (statements: ResizeArray<CudaExpr>) =
    let rec process_statement (code: StringBuilder,ind as state) statement =
        match statement with
        | Statement x -> [|String.replicate ind " "; x; "\n"|] |> exp |> code.Append, ind
        | Indent -> code, ind+4
        | Dedent -> code, ind-4
        | Statements x -> process_statements state x
    and process_statements state (statements: ResizeArray<CudaExpr>) =
        Seq.fold process_statement state statements
    process_statements (StringBuilder(),0) statements
    |> fun (code,ind) -> code.ToString()

let cuda_kernel_module_compile map_module =
    let program = ResizeArray()

    let state x = state program x
    let enter x = enter program x
    let expand x = expand program x

    let method_ rtyp kernel_name args body =
        [|rtyp;kernel_name;"(";args;") {"|] |> state
        enter body ()
        [|"}"|] |> state
    let externCBlock body =
        [|"extern \"C\" {"|] |> state
        enter body ()
        [|"}"|] |> state
    let include_ str =
        [|"#include "; quote str|] |> state

    map_module (method_, externCBlock, include_, expand)
    program

let cuda_inner_compile kernel_body =
    let program = ResizeArray()
    
    let state x = state program x
    let enter x = enter program x
    let expand x = expand program x
    let varn x = x + string (cuda_tag())

    let plus x y = [|"(";x;" + ";y;")"|] |> exp
    let times x y = [|"(";x;" * ";y;")"|] |> exp
    let less_than x y = [|"(";x;" < ";y;")"|] |> exp
    let init x = 
        let v = varn "var_"
        let decl = [|"auto ";v;" = ";x|] |> exp
        v, decl
    let for_ (v,decl) cond incr body =
        [|"for(";decl;"; ";cond v;"; ";incr v;") {"|] |> state
        enter body v
        [|"}"|] |> state
    let ifvoid cond true_ false_ =
        [|"if (";cond;") {"|] |> state
        enter true_ ()
        [|"} else {"|] |> state
        enter false_ ()
        [|"}"|] |> state
    let args2 arg_a arg_b = [|"(";arg_a;", ";arg_b;")"|] |> exp
    let class_method2 class_name method_name arg_a arg_b =
        [|class_name;".";method_name;args2 arg_a arg_b|] |> exp
    let class1 class_name arg =
        let v = varn "class_"
        [|"auto ";v;" = ";class_name;"(";arg;");"|] |> state
        v
    let typedef x =
        let v = varn "typedef_"
        [|"typedef ";x;" ";v;";"|] |> state
        v
    let set x v =
        [|x;" = ";v;";"|] |> state
    let eq x v =
        [|x;" == ";v|] |> exp
    let while_ cond body =
        [|"while (";cond;") {"|] |> state
        enter body ()
        [|"}"|] |> state
    let madd x v =
        [|x;" += ";v|] |> exp
    let lambda2 x =
        let v = varn "lambda_"
        [|"const auto ";v;" = ";x;";"|] |> state
        fun a b -> [|v;args2 a b|] |> exp
    let text x =
        [|x|] |> state
    let var typ init = 
        let v = varn "var_"
        [|typ;" ";v;" = ";init;";"|] |> state
        v
    kernel_body 
        (class_method2, class1, typedef, ifvoid, set, eq, times, plus, less_than, for_, while_, madd, lambda2, text, var, init, expand)
    program


let cudavar_template get_method_arg get_var name typ size =
    let method_arg = get_method_arg name typ
    let var = get_var name // TODO: Put in the signature here later.
    method_arg, var

let cudavar_var name typ =
    let get_method_arg name typ =
        [|typ;name|] |> String.concat ""
    let get_var name = name
    cudavar_template get_method_arg get_var name typ ()

let cudavar_ar1d name typ size =
    let get_method_arg name typ =
        [|typ;name|] |> String.concat ""
    let get_var name i = 
        [|name;"[";i;"]"|] |> String.concat ""
    cudavar_template get_method_arg get_var name typ size

let cudavar_ar2d name typ (size_col,size_row as size) =
    let get_method_arg name typ =
        [|typ;name|] |> String.concat ""
    let get_var name (c,r) =
        [|name;"[";c;" * ";size_row;" + ";r;"]"|] |> String.concat ""
    cudavar_template get_method_arg get_var name typ size

let map_module_template 
        process_ins process_outs process_args
        ins outs kernel_name map_macro =
    let n, ins_arg, ins_var = process_ins ins outs
    let outs_arg, outs_var = process_outs ins outs

    let args = process_args n ins_arg outs_arg

    let body = cuda_inner_compile <| cuda_map_template n (map_macro ins_var outs_var)
    let all = cuda_kernel_module_template args kernel_name body
    cuda_kernel_module_compile all |> process_statements
    
let cuda_group (num_in, names_in) =
    let f i n = n + string i
    List.collect (fun (i: int) ->
        List.map (f i) names_in) [1..num_in]

let mapcoef_module_forward_template 
        args_in args_coef args_out kernel_name map_macro 
        map_ins map_consts map_outs
        flatten_ins flatten_consts flatten_outs
        =
    let size_arg, size_var = cudavar_var "n" "const int "
    let process_ins (ins,consts) _ =
        let ins_arg, ins_var = 
            map_ins (fun n -> cudavar_ar1d n "const float *" size_arg) ins
        let consts_arg, consts_var =
            map_consts (fun n -> cudavar_var n "const float ") consts
        size_arg,(ins_arg,consts_arg),(size_var,ins_var,consts_var)

    let process_outs _ outs =
        let outs_arg, outs_var = 
            map_outs (fun n -> cudavar_ar1d n "float *" size_arg) outs
        outs_arg,outs_var

    let process_args size_arg (ins_arg, consts_arg) outs_arg =
        size_arg :: ([flatten_ins ins_arg;flatten_consts consts_arg;flatten_outs outs_arg] |> List.concat) |> String.concat ", "
    
    map_module_template process_ins process_outs process_args (args_in, args_coef) args_out kernel_name map_macro

/// Mapcoef modules with generic number of arguments.
let mapcoef_module_forward_list args_in args_coef args_out kernel_name map_macro =
    let map f x = List.map f (cuda_group x) |> List.unzip
    let flatten x = x
    mapcoef_module_forward_template 
        args_in args_coef args_out kernel_name map_macro
        map map map flatten flatten flatten

let mapcoef_module_backward_template 
        args_in args_coef args_out kernel_name map_macro 
        map_ins_prim map_consts map_outs map_ins_adj
        flatten_ins_prim flatten_consts flatten_outs flatten_ins_adj =
    let size_arg, size_var = cudavar_var "n" "const int "
    let process_ins (ins,consts) outs =
        let ins_prim_arg, ins_prim_var = 
            map_ins_prim (fun n -> cudavar_ar1d n "const float *" size_arg) ins
        let consts_arg, consts_var = 
            map_consts (fun n -> cudavar_var n "const float ") consts
        let outs_arg, outs_var =
            map_outs (fun n -> cudavar_ar1d n "const float *" size_arg) outs
        size_arg,(ins_prim_arg,consts_arg,outs_arg),(size_var,ins_prim_var,consts_var,outs_var)

    let process_outs (ins,_) _ =
        let ins_adj_arg, ins_adj_var = 
            map_ins_adj (fun n -> cudavar_ar1d n "float *" size_arg) ins
        ins_adj_arg,ins_adj_var

    let process_args size_arg (ins_prim_arg, consts_arg, outs_arg) ins_adj_arg =
        size_arg :: ([flatten_ins_prim ins_prim_arg;flatten_consts consts_arg;flatten_outs outs_arg
                      flatten_ins_adj ins_adj_arg] 
                      |> List.concat) 
        |> String.concat ", "
    
    map_module_template process_ins process_outs process_args (args_in, args_coef) args_out kernel_name map_macro

// The list version of the backward module for a kernel with a generic number of arguments.
let mapcoef_module_backward_list args_in args_coef args_out kernel_name map_macro =
    let names_into_prim_and_adj names = List.collect (fun name -> [name+"_primal";name+"_adjoint"]) names
    let names_into_primals names = List.map (fun name -> name+"_primal") names
    let names_into_adjoints names = List.map (fun name -> name+"_adjoint") names

    let map g f x = List.map f (cuda_group x |> g) |> List.unzip
    let flatten x = x

    mapcoef_module_backward_template
        args_in args_coef args_out kernel_name map_macro 
//        map_ins_prim map_consts map_outs map_ins_adj
        (map names_into_primals) (map id) (map names_into_prim_and_adj) (map names_into_adjoints)
//        flatten_ins_prim flatten_consts flatten_outs flatten_ins_adj
        flatten flatten flatten flatten

