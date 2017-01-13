#load "SpiralV5DM.fsx"

open SpiralV5
open SpiralV5DM

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

let varn x = x + string (cuda_tag())

type CudaExpr =
| Statement of string
| Indent
| Dedent
| Statements of ResizeArray<CudaExpr>

let expr x = String.concat "" x
let state (program: ResizeArray<_>) x = expr x |> Statement |> program.Add
let enter (program: ResizeArray<_>) body v =
    Indent |> program.Add
    body v
    Dedent |> program.Add

let process_statements (statements: ResizeArray<CudaExpr>) =
    let rec process_statement (code: StringBuilder,ind as state) statement =
        match statement with
        | Statement x -> [|String.replicate ind " "; x; "\n"|] |> expr |> code.Append, ind
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
    let expand x = Statements x |> program.Add

    map_module (method_, externCBlock, include_, expand)
    program

//// Expressions
let init x = 
    let v = varn "var_"
    let decl = [|"auto ";v;" = ";x|] |> expr
    v, decl
let args2 arg_a arg_b = [|"(";arg_a;", ";arg_b;")"|] |> expr
let class_method2 class_name method_name arg_a arg_b =
    [|class_name;".";method_name;args2 arg_a arg_b|] |> expr
let madd x v = [|x;" += ";v|] |> expr

let unary_fun_op op x = [|op;"(";x;")"|] |> expr
let exp x = unary_fun_op "exp" x
let log x = unary_fun_op "log" x
let tanh x = unary_fun_op "tanh" x
    
let unary_op op x = [|"(";op;x;")"|] |> expr
let neg x = unary_op "-" x

let binary_op op x y = [|"(";x;op;y;")"|] |> expr
let (.=) x y = binary_op " == " x y
let (.<) x y = binary_op " < " x y
let (.<=) x y = binary_op " <= " x y
let (.>) x y = binary_op " > " x y
let (.>=) x y = binary_op " >= " x y

let (.||) x y = binary_op " || " x y
let (.&&) x y = binary_op " && " x y

let (.+) x y = binary_op " + " x y
let (.*) x y = binary_op " * " x y
let (./) x y = binary_op " / " x y
let (.-) x y = binary_op " - " x y

let if_ cond true_ false_ = [|"((";cond;") ? (";true_;") : (";false_;"))"|] |> expr

let cuda_inner_compile kernel_body =
    let program = ResizeArray()
    
    let state x = state program x
    let enter x = enter program x
 
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
    let while_ cond body =
        [|"while (";cond;") {"|] |> state
        enter body ()
        [|"}"|] |> state
    let madd' x v = // TODO: Make madd' check for null first.
        [|x;" += ";v;";"|] |> state
    let lambda2 lam =
        let v,a,b = varn "lambda_", varn "lambda_arg_", varn "lambda_arg_"
        [|"const auto ";v;" = [](const auto ";a;", const auto ";b;") {"|] |> state
        enter (fun (a,b) -> lam a b) (a, b)
        [|"};"|] |> state
        v, fun a b -> [|v;args2 a b|] |> expr
    let text x =
        [|x|] |> state
    let var typ init = 
        let v = varn "var_"
        [|typ;" ";v;" = ";init;";"|] |> state
        v
    let return_ x = [|"return ";x;";"|] |> state

    kernel_body (return_,lambda2, class1, typedef, ifvoid, set, for_, while_, madd', text, var)
    program


/// The outer level of the Cuda compiler language.
let cuda_kernel_module kernel_name args method_body_macro = 
    cuda_outer_compile <| fun (method_, externCBlock, include_, expand) ->
        include_ "thrust/tuple.h"
        include_ "cub/cub.cuh"
        externCBlock ( fun () ->
            method_ "__global__ void " kernel_name args (fun () -> expand method_body_macro)
            )

/// The inner level of the language. Standard map.
let cuda_map n map_macro =
    cuda_inner_compile <| fun (return_,lambda2, class1, typedef, ifvoid, set, for_, while_, madd', text, var as funs) ->
        for_ 
            (init "blockIdx.x * blockDim.x + threadIdx.x")
            (fun i -> i .< n) 
            (fun i -> madd i "gridDim.x * blockDim.x")
            (fun i -> map_macro i funs)

let cuda_map_redo_map block_reduce_type n (map_load_op_macro, reduce_op_macro, map_store_macro) =
    cuda_inner_compile <| fun (return_,lambda2, class1, typedef, ifvoid, set, for_, while_, madd', text, var as funs) ->
        let block_reduce_typedef = typedef ("cub::BlockReduce<"+block_reduce_type+", " + string map_redo_map_launcher_block_size + ">")
        let temp_storage = var "__shared__ BlockReduceT::TempStorage" ""
        let block_reduce = class1 block_reduce_typedef temp_storage

        let reduce_op_name, reduce_op = lambda2 (reduce_op_macro funs)
        let i = var "int" "blockIdx.x*blockDim.x + threadIdx.x"
        let value = var "auto" (map_load_op_macro i)
        let stride = var "const auto" "gridDim.x*blockDim.x"
        madd' i stride
        while_ (i .< n) <| fun () ->
            set value (reduce_op value (map_load_op_macro i))
            madd' i stride

        let result = var "const auto" (class_method2 block_reduce "Reduce" value reduce_op_name)
        ifvoid ("threadIdx.x" .= "0")
               (fun () -> map_store_macro result funs)
               (fun () -> ())

let cudavar_var typ sig_ suffix =
    let name = [|varn "arg_"; suffix|] |> String.concat ""
    let arg = [|typ;" ";name|] |> String.concat ""
    arg, name, sig_

let cudavar_ar1d typ size sig_ suffix =
    let name = [|varn "arg_"; suffix|] |> String.concat ""
    let arg = [|typ;" ";name|] |> String.concat ""
    let var i = [|name;"[";i;"]"|] |> String.concat ""
    arg, var, sig_

let cudavar_ar2d typ (size_col,size_row as size) sig_ suffix =
    let name = [|varn "arg_"; suffix|] |> String.concat ""
    let arg = [|typ;" ";name|] |> String.concat ""
    let var (c,r) = [|name;"[";c;" * ";size_row;" + ";r;"]"|] |> String.concat ""
    arg, var, sig_

type CudaIntAr2D = CudaIntAr2D
type CudaFloatAr2D = CudaFloatAr2D
type CudaIntAr1D = CudaIntAr1D
type CudaFloatAr1D = CudaFloatAr1D
// These two are supposed to be expressions.
type CudaInt = CudaInt of string
type CudaFloat = CudaFloat of string

let mkCudaLambdaArgs<'ins, 'consts,'outs>(f: 'ins -> 'consts -> 'outs) : 'ins * 'consts * 'outs =
    Unchecked.defaultof<'ins>,Unchecked.defaultof<'consts>,Unchecked.defaultof<'outs>