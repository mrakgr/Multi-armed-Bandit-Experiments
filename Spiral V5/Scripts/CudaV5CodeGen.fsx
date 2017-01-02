﻿#load "SpiralV5.fsx"
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
open System.Diagnostics
open System.IO
open System.Collections.Generic
open System.Runtime.InteropServices

let cuda_map_all_template 
        times plus less_than // basic operators
        gridDim_x blockDim_x blockIdx_x threadIdx_x // kernel constants
        var for_ // kernel primitives
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

let ind i = String.replicate i " "

let cuda_map_all n map_macro s =
    let program = Text.StringBuilder()
    let p (x: string) = program.Append x |> ignore

    let plus x y = [|"(";x;" + ";y;")"|] |> String.concat ""
    let times x y = [|"(";x;" * ";y;")"|] |> String.concat ""
    let less_than x y = [|"(";x;" < ";y;")"|] |> String.concat ""
    let gridDim_x, blockDim_x = "gridDim.x","blockDim.x"
    let blockIdx_x, threadIdx_x = "blockIdx.x","threadIdx.x"
    let var x = 
        let v = "auto var_" + string (cuda_tag())
        let decl = [|ind s;v;" = ";x|] |> String.concat ""
        v
    let for_ (v,decl) cond incr body =
        [|ind s;"for(";decl;", ";cond v;", ";incr v;") {\n"|] |> Array.iter p
        body v (s+4) |> p
        [|ind s;"}\n"|] |> Array.iter p
        
    cuda_map_all_template 
        times plus less_than // basic operators
        gridDim_x blockDim_x blockIdx_x threadIdx_x // kernel constants
        var for_ // kernel primitives
        n map_macro

let cuda_map_module_template 
        n args kernel_name map_macro // kernel params
        method_ externCBlock include_ = // kernel primitives
        
    include_ "thrust/tuple.h"
    include_ "cub/cub.cuh"
    externCBlock (
        method_ "__global__ void " kernel_name args (
            cuda_map_all n map_macro
            )
        )

let cuda_map_module_compile map_module =
    let program = Text.StringBuilder()
    let p (x: string) = program.Append x |> ignore

    let method_ rtyp kernel_name args body s =
        [|ind s;rtyp;kernel_name;"(";args;") {\n"|] |> Array.iter p
        body (s+4) |> p
        [|ind s;"}"|] |> Array.iter p
    let externCBlock body =
        [|"extern \"C\" {"|] |> Array.iter p
        body 4
        [|"}\n"|] |> Array.iter p
    let include_ str =
        p "#include "; p (quote str); p "\n"

    map_module method_ externCBlock include_
    program.ToString()

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

    let f = cuda_map_module_template n args kernel_name (map_macro ins_var outs_var)
       
    cuda_map_module_compile f
    
let cuda_group (num_in, names_in) =
    let f i n = n + string i
    List.collect (fun (i: int) ->
        List.map (f i) names_in) [1..num_in]

let mapcoef_module_forward args_in args_coef args_out kernel_name map_macro =
    let size_arg, size_var = cudavar_var "n" "const int "
    let process_ins (ins,consts) _ =
        let ins_arg, ins_var = 
            List.map (fun n -> cudavar_ar1d n "const float *" size_arg) (cuda_group ins)
            |> List.unzip
        let consts_arg, consts_var =
            List.map (fun n -> cudavar_var n "const float ") (cuda_group consts)
            |> List.unzip
        size_arg,(ins_arg,consts_arg),(size_var,ins_var,consts_var)

    let process_outs _ outs =
        let outs_arg, outs_var = 
            List.map (fun n -> cudavar_ar1d n "float *" size_arg) (cuda_group outs)
            |> List.unzip
        outs_arg,outs_var

    let process_args size_arg (ins_arg, consts_arg) outs_arg =
        size_arg :: ([ins_arg;consts_arg;outs_arg] |> List.concat) |> String.concat ", "
    
    map_module_template process_ins process_outs process_args (args_in, args_coef) args_out kernel_name map_macro

