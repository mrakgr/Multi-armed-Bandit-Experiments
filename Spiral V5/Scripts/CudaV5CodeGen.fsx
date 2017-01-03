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

type ReaderBuilder() =
    member inline t.Return a = fun r -> a
    member inline t.Bind(a,f) = fun r -> f (a r) r
    member inline t.ReturnFrom x = x
    member inline t.Zero() = t.Return ()

let reader = ReaderBuilder()

let cuda_map_all_template 
        times plus less_than // basic operators
        gridDim_x blockDim_x blockIdx_x threadIdx_x // kernel constants
        for_ // kernel primitives
        n map_macro = // kernel params
    for_ 
        (plus (times blockIdx_x blockDim_x) threadIdx_x) 
        (fun i -> less_than i n) 
        (fun i -> plus i (times gridDim_x blockDim_x))
        map_macro

let cuda_tag =
    let mutable i = 0
    fun () ->
        i <- i+1
        i

let p (x: string) (c': StringBuilder,s : int as c) = c'.Append x |> ignore
let ind (c': StringBuilder,i : int as c) = p (String.replicate i " ") c
let step_up (c': StringBuilder,i : int as c) = c', i+4

let cuda_map_all n map_macro =
    let plus x y c = p "(" c;x c;p " + " c;y c;p ")" c
    let times x y c = p "(" c;x c;p " + " c;y c;p ")" c
    let less_than x y c = p "(" c;x c;p " + " c;y c;p ")" c
    let gridDim_x c = p "gridDim.x" c
    let blockDim_x c = p "blockDim.x" c
    let blockIdx_x c = p "blockIdx.x" c
    let threadIdx_x c = p "threadIdx.x" c
    let for_ init cond incr body c =
        let v c = p ("auto var_" + string (cuda_tag())) c
        ind c;p "for(" c;v c;p " = " c;init c;p "; " c;cond v c;p "; " c;incr v c;p ") {\n" c
        body v (step_up c)
        ind c;p "}\n" c
        
    cuda_map_all_template 
        times plus less_than // basic operators
        gridDim_x blockDim_x blockIdx_x threadIdx_x // kernel constants
        for_ // kernel primitives
        n map_macro

let cuda_map_module_template 
        n args kernel_name map_macro // kernel params
        method_ externCBlock include_ c = // kernel primitives
    reader {
        let! _ = include_ "thrust/tuple.h"
        let! _ = include_ "cub/cub.cuh"
        return
            externCBlock (
                method_ "__global__ void " kernel_name args (
                    cuda_map_all n map_macro
                    )
                )
        }

let cuda_map_module_compile map_module =
    let method_ rtyp kernel_name args body c =
        ind c;p rtyp c;p kernel_name c;p "(" c;p args c;p ") {\n" c
        body (step_up c)
        ind c;p "}" c
    let externCBlock body c =
        p "extern \"C\" {" c
        body (step_up c)
        p "}\n" c
    let include_ str c =
        p "#include " c; p (quote str) c; p "\n" c

    map_module method_ externCBlock include_

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
    
//let cuda_group (num_in, names_in) =
//    let f i n = n + string i
//    List.collect (fun (i: int) ->
//        List.map (f i) names_in) [1..num_in]
//
//let mapcoef_module_forward args_in args_coef args_out kernel_name map_macro =
//    let size_arg, size_var = cudavar_var "n" "const int "
//    let process_ins (ins,consts) _ =
//        let ins_arg, ins_var = 
//            List.map (fun n -> cudavar_ar1d n "const float *" size_arg) (cuda_group ins)
//            |> List.unzip
//        let consts_arg, consts_var =
//            List.map (fun n -> cudavar_var n "const float ") (cuda_group consts)
//            |> List.unzip
//        size_arg,(ins_arg,consts_arg),(size_var,ins_var,consts_var)
//
//    let process_outs _ outs =
//        let outs_arg, outs_var = 
//            List.map (fun n -> cudavar_ar1d n "float *" size_arg) (cuda_group outs)
//            |> List.unzip
//        outs_arg,outs_var
//
//    let process_args size_arg (ins_arg, consts_arg) outs_arg =
//        size_arg :: ([ins_arg;consts_arg;outs_arg] |> List.concat) |> String.concat ", "
//    
//    map_module_template process_ins process_outs process_args (args_in, args_coef) args_out kernel_name map_macro
//
