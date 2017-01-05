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

let cuda_outer_compile map_module =
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
    let madd' x v =
        [|x;" += ";v;";"|] |> state
    let lambda2 x =
        let v = varn "lambda_"
        [|"const auto ";v;" = ";x;";"|] |> state
        v, fun a b -> [|v;args2 a b|] |> exp
    let text x =
        [|x|] |> state
    let var typ init = 
        let v = varn "var_"
        [|typ;" ";v;" = ";init;";"|] |> state
        v
    
    kernel_body 
        (class_method2, class1, typedef, ifvoid, set, eq, times, plus, less_than, for_, while_, madd, madd', lambda2, text, var, init, expand)
    program


/// The outer level of the Cuda compiler language.
let cuda_kernel_module_template kernel_name args method_body_macro = 
    cuda_outer_compile <| fun (method_, externCBlock, include_, expand) ->
        include_ "thrust/tuple.h"
        include_ "cub/cub.cuh"
        externCBlock ( fun () ->
            method_ "__global__ void " kernel_name args (fun () -> expand method_body_macro)
            )

/// The inner level of the language. Standard map.
let cuda_map_template n map_macro =
    cuda_inner_compile <| fun (class_method2, class1, typedef, ifvoid, set, eq, times, plus, less_than, for_, while_, madd, madd', lambda2, text, var, init, expand) ->
        for_ 
            (init "blockIdx.x * blockDim.x + threadIdx.x")
            (fun i -> less_than i n) 
            (fun i -> madd i "gridDim.x * blockDim.x")
            (fun i -> expand (map_macro i))

let cuda_map_redo_map_template block_reduce_type n map_load_op_macro reduce_op_macro map_store_macro =
    cuda_inner_compile <| fun (class_method2, class1, typedef, ifvoid, set, eq, times, plus, less_than, for_, while_, madd, madd', lambda2, text, var, init, expand) ->
        let block_reduce_typedef = typedef ("cub::BlockReduce<"+block_reduce_type+", " + string map_redo_map_launcher_block_size + ">")
        let temp_storage = var "__shared__ BlockReduceT::TempStorage" ""
        let block_reduce = class1 block_reduce_typedef temp_storage

        let reduce_op_name, reduce_op = lambda2 reduce_op_macro
        let i = var "int" "blockIdx.x*blockDim.x + threadIdx.x"
        let value = var "auto" (map_load_op_macro i)
        let stride = var "const auto" "gridDim.x*blockDim.x"
        madd' i stride
        while_ (less_than i n) <| fun () ->
            set value (reduce_op value (map_load_op_macro i))
            madd' i stride

        let result = var "const auto" (class_method2 block_reduce "Reduce" value reduce_op_name)
        ifvoid (eq "threadIdx.x" "0")
               (fun () -> expand (map_store_macro result))
               (fun () -> ())

let cudavar_template get_method_arg get_var typ size name =
    let method_arg = get_method_arg name typ
    let var = get_var name // TODO: Put in the signature here later.
    method_arg, var

let cudavar_var typ name =
    let get_method_arg name typ =
        [|typ;" ";name|] |> String.concat ""
    let get_var name = name
    cudavar_template get_method_arg get_var typ () name

let cudavar_ar1d typ size name =
    let get_method_arg name typ =
        [|typ;" ";name|] |> String.concat ""
    let get_var name i = 
        [|name;"[";i;"]"|] |> String.concat ""
    cudavar_template get_method_arg get_var typ size name

let cudavar_ar2d typ (size_col,size_row as size) name =
    let get_method_arg name typ =
        [|typ;" ";name|] |> String.concat ""
    let get_var name (c,r) =
        [|name;"[";c;" * ";size_row;" + ";r;"]"|] |> String.concat ""
    cudavar_template get_method_arg get_var typ size name

let forward_template size_arg map_ins_f map_consts_f map_outs_f kernel
        map_ins map_consts map_outs  
        flatten_ins flatten_consts flatten_outs
        =
    let process_ins (ins,consts) _ =
        let ins_arg, ins_var = 
            map_ins map_ins_f ins
        let consts_arg, consts_var =
            map_consts map_consts_f consts
        (ins_arg,consts_arg),(ins_var,consts_var)
    let process_outs _ outs =
        let outs_arg, outs_var = 
            map_outs map_outs_f outs
        outs_arg,outs_var
    let process_args (ins_arg, consts_arg) outs_arg =
        size_arg :: ([flatten_ins ins_arg;flatten_consts consts_arg;flatten_outs outs_arg] |> List.concat) |> String.concat ", "
    kernel process_ins process_outs process_args

let mapcoef_forward kernel =
    let size_arg, size_var = cudavar_var "const int" "n"
    
    let map_ins_f n = cudavar_ar1d "const float *" size_var n
    let map_consts_f n = cudavar_var "const float" n

    let map_outs_f n = cudavar_ar1d "float *" size_var n

    forward_template size_arg map_ins_f map_consts_f map_outs_f (kernel size_var)

let map_redo_map_forward kernel =
    let size_arg, size_var = cudavar_var "const int" "n"
    
    let map_ins_f n = cudavar_ar1d "const float *" size_var n
    let map_consts_f n = cudavar_var "const float" n

    let map_outs_f n = cudavar_ar1d "float *" "1" n

    forward_template size_arg map_ins_f map_consts_f map_outs_f (kernel size_var)

let cuda_group (num_in, names_in) =
    let f i n = n + string i
    List.collect (fun (i: int) ->
        List.map (f i) names_in) [1..num_in]

let map_forward_list_template map_module =
    let map f x = List.map f (cuda_group x) |> List.unzip
    let flatten x = x
    map_module map map map flatten flatten flatten

let mapcoef_forward_list kernel = map_forward_list_template (mapcoef_forward kernel)
let map_redo_map_forward_list kernel = map_forward_list_template (map_redo_map_forward kernel)

let map_forward_1_0_1_template map_module =
    let map f x = f x
    let map_const f () = (),()
    let flatten arg = [arg]
    let flatten_const () = []
    map_module map map_const map flatten flatten_const flatten

let mapcoef_forward_1_0_1 kernel = map_forward_1_0_1_template (mapcoef_forward kernel)
let map_redo_map_forward_1_0_1 kernel = map_forward_1_0_1_template (map_redo_map_forward kernel)

let backward_template 
        size_arg map_ins_prim_f map_consts_f map_outs_f map_ins_adj_f kernel
        map_ins_prim map_consts map_outs map_ins_adj 
        flatten_ins_prim flatten_consts flatten_outs flatten_ins_adj
        =
    let process_ins (ins,consts) outs =
        let ins_prim_arg, ins_prim_var = 
            map_ins_prim map_ins_prim_f ins
        let consts_arg, consts_var =
            map_consts map_consts_f consts
        let outs_arg, outs_var =
            map_outs map_outs_f outs
        (ins_prim_arg,consts_arg,outs_arg),(ins_prim_var,consts_var,outs_var)
    let process_outs (ins,_) _ =
        let ins_adj_arg, ins_adj_var = 
            map_ins_adj map_ins_adj_f ins
        ins_adj_arg, ins_adj_var
    let process_args (ins_prim_arg, consts_arg, outs_arg) ins_adj_arg =
        size_arg :: ([flatten_ins_prim ins_prim_arg;flatten_consts consts_arg;flatten_outs outs_arg
                      flatten_ins_adj ins_adj_arg] 
                      |> List.concat)
        |> String.concat ", "
    kernel process_ins process_outs process_args

let mapcoef_backward kernel =
    let size_arg, size_var = cudavar_var "const int" "n"
    
    let map_ins_prim_f n = cudavar_ar1d "const float *" size_var n
    let map_consts_f n = cudavar_var "const float" n
    let map_outs_f n = cudavar_ar1d "const float *" size_var n
    
    let map_ins_adj_f n = cudavar_ar1d "const float *" size_var n

    backward_template size_arg map_ins_prim_f map_consts_f map_outs_f map_ins_adj_f (kernel size_var)

let map_redo_map_backward kernel =
    let size_arg, size_var = cudavar_var "const int" "n"
    
    let map_ins_prim_f n = cudavar_ar1d "const float *" size_var n
    let map_consts_f n = cudavar_var "const float" n
    let map_outs_f n = cudavar_ar1d "const float" size_var n
    
    let map_ins_adj_f n = cudavar_ar1d "const float *" size_var n

    backward_template size_arg map_ins_prim_f map_consts_f map_outs_f map_ins_adj_f (kernel size_var)

let map_backward_list_template map_module =
    let names_into_prim_and_adj names = List.collect (fun name -> [name+"_primal";name+"_adjoint"]) names
    let names_into_primals names = List.map (fun name -> name+"_primal") names
    let names_into_adjoints names = List.map (fun name -> name+"_adjoint") names

    let map g f x = List.map f (cuda_group x |> g) |> List.unzip
    let flatten_list x = x
    map_module 
        (map names_into_primals) (map id) (map names_into_prim_and_adj) (map names_into_adjoints)
        flatten_list flatten_list flatten_list flatten_list

let mapcoef_backward_list kernel = map_backward_list_template (mapcoef_backward kernel)
let map_redo_map_backward_list kernel = map_backward_list_template (map_redo_map_backward kernel)

let mapcoef_compile_forward_1_0_1_template kernel_name macro =
    mapcoef_forward_1_0_1 <| fun size_var process_ins process_outs process_args ->
        let x = "x"
        let c = ()
        let o = "o"
        let ins_arg, ins_var = process_ins (x,c) o
        let outs_arg, outs_var = process_outs (x,c) o
        let args = process_args ins_arg outs_arg

        let body = cuda_map_template size_var (macro (fst ins_var) outs_var)
        cuda_kernel_module_template kernel_name args body
        |> process_statements

let square_macro x o i =
    cuda_inner_compile <| fun (class_method2, class1, typedef, ifvoid, set, eq, times, plus, less_than, for_, while_, madd, madd', lambda2, text, var, init, expand) ->
        set (o i) (times (x i) (x i))

mapcoef_compile_forward_1_0_1_template "Square" square_macro