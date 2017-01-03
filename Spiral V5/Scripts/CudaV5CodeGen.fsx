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

let cuda_map_all_template 
        times plus less_than // basic operators
        gridDim_x blockDim_x blockIdx_x threadIdx_x // kernel constants
        for_ var // kernel primitives
        n map_macro = // kernel params
    for_ 
        (var (plus (times blockIdx_x blockDim_x) threadIdx_x))
        (fun i -> less_than i n) 
        (fun i -> plus i (times gridDim_x blockDim_x))
        map_macro

let cuda_tag =
    let mutable i = 0
    fun () ->
        i <- i+1
        i

type CudaExpr =
| Statement of string
| Statements of ResizeArray<CudaExpr>

let cuda_map_all n map_macro =
    let program = ResizeArray()

    let exp x = String.concat "" x
    let state x = exp x |> Statement |> program.Add
    let states x = Statements x |> program.Add

    let plus x y = [|"(";x;" + ";y;")"|] |> exp
    let times x y = [|"(";x;" * ";y;")"|] |> exp
    let less_than x y = [|"(";x;" < ";y;")"|] |> exp
    let gridDim_x, blockDim_x = "gridDim.x","blockDim.x"
    let blockIdx_x, threadIdx_x = "blockIdx.x","threadIdx.x"
    let var x = 
        let v = "auto var_" + string (cuda_tag())
        let decl = [|v;" = ";x|] |> exp
        v, decl
    let for_ (v,decl) cond incr body =
        [|"for(";decl;"; ";cond v;"; ";incr v;") {"|] |> state
        body v |> states
        [|"}"|] |> state
        
    cuda_map_all_template 
        times plus less_than // basic operators
        gridDim_x blockDim_x blockIdx_x threadIdx_x // kernel constants
        for_ var // kernel primitives
        n map_macro

    program

let cuda_map_module_template 
            kernel_name args method_body // kernel_param
            method_ externCBlock include_ = // kernel primitives
        include_ "thrust/tuple.h"
        include_ "cub/cub.cuh"
        externCBlock ( fun () ->
            method_ "__global__ void " kernel_name args method_body
            )

let cuda_map_module_compile map_module =
    let program = ResizeArray()

    let exp x = String.concat "" x
    let state x = exp x |> Statement |> program.Add
    let states x = Statements x |> program.Add

    let method_ rtyp kernel_name args body =
        [|rtyp;kernel_name;"(";args;") {"|] |> state
        body |> states
        [|"}"|] |> state
    let externCBlock body =
        [|"extern \"C\" {"|] |> state
        body()
        [|"}"|] |> state
    let include_ str =
        [|"#include "; quote str|] |> state

    let rec process_statement ind statement =
        match statement with
        | Statement x -> [|String.replicate ind " "; x; "\n"|] |> exp
        | Statements x -> process_statements (ind+4) x

    and process_statements ind (statements: ResizeArray<CudaExpr>) =
        let code = StringBuilder()
        Seq.iter (fun x -> process_statement ind x |> code.Append |> ignore) statements
        code.ToString()
    
    map_module method_ externCBlock include_
    process_statements 0 program

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

    cuda_map_module_compile (cuda_map_module_template args kernel_name (cuda_map_all n (map_macro ins_var outs_var)))
    
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
