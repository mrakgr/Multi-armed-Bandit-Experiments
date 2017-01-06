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
let cuda_kernel_module kernel_name args method_body_macro = 
    cuda_outer_compile <| fun (method_, externCBlock, include_, expand) ->
        include_ "thrust/tuple.h"
        include_ "cub/cub.cuh"
        externCBlock ( fun () ->
            method_ "__global__ void " kernel_name args (fun () -> expand method_body_macro)
            )

/// The inner level of the language. Standard map.
let cuda_map n map_macro =
    cuda_inner_compile <| fun (class_method2, class1, typedef, ifvoid, set, eq, times, plus, less_than, for_, while_, madd, madd', lambda2, text, var, init, expand) ->
        for_ 
            (init "blockIdx.x * blockDim.x + threadIdx.x")
            (fun i -> less_than i n) 
            (fun i -> madd i "gridDim.x * blockDim.x")
            (fun i -> expand (map_macro i))

let cuda_map_redo_map block_reduce_type n map_load_op_macro reduce_op_macro map_store_macro =
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

let cudavar_template get_method_arg get_var typ size name sig_ =
    let method_arg = get_method_arg name typ
    let var = get_var name
    method_arg, var, sig_

let cudavar_var typ name sig_ =
    let get_method_arg name typ =
        [|typ;" ";name|] |> String.concat ""
    let get_var name = name
    cudavar_template get_method_arg get_var typ () name sig_

let cudavar_ar1d typ size name sig_ =
    let get_method_arg name typ =
        [|typ;" ";name|] |> String.concat ""
    let get_var name i = 
        [|name;"[";i;"]"|] |> String.concat ""
    cudavar_template get_method_arg get_var typ size name sig_

let cudavar_ar2d typ (size_col,size_row as size) name sig_ =
    let get_method_arg name typ =
        [|typ;" ";name|] |> String.concat ""
    let get_var name (c,r) =
        [|name;"[";c;" * ";size_row;" + ";r;"]"|] |> String.concat ""
    cudavar_template get_method_arg get_var typ size name sig_

let forward_template size_arg map_ins_f map_consts_f map_outs_f kernel
        ((map_ins,flatten_ins), (map_consts,flatten_consts), (map_outs,flatten_outs))
        =
    let process_ins (ins,consts) _ =
        let ins_arg, ins_var, ins_sig = 
            map_ins map_ins_f ins
        let consts_arg, consts_var, consts_sig =
            map_consts map_consts_f consts
        (ins_arg,consts_arg),(ins_var,consts_var),(ins_sig,consts)
    let process_outs _ outs =
        let outs_arg, outs_var, outs_sig = 
            map_outs map_outs_f outs
        outs_arg,outs_var,outs_sig
    let process_args (ins_arg, consts_arg) outs_arg =
        size_arg :: ([flatten_ins ins_arg;flatten_consts consts_arg;flatten_outs outs_arg] |> List.concat) |> String.concat ", "
    kernel process_ins process_outs process_args

let mapcoef_forward num_args kernel =
    let size_arg, size_var, size_sig = 
        cudavar_var "const int" "n" (fun (size: int) -> box size)
    
    let map_ins_f n = 
        cudavar_ar1d "const float *" size_var n (fun (x: DM<int,float32>) -> box x.P.DevicePointer)
    let map_consts_f n = 
        cudavar_var "const float" n (fun (x: float32) -> box x)

    let map_outs_f n = 
        cudavar_ar1d "float *" size_var n (fun (x: DM<int,float32>) -> box x.P.DevicePointer)

    forward_template size_arg map_ins_f map_consts_f map_outs_f (kernel (cuda_map size_var) size_sig) num_args

let map_redo_map_forward num_args kernel =
    let size_arg, size_var, size_sig = 
        cudavar_var "const int" "n" (fun (size: int) -> [|box size|])
    
    let map_ins_f n = 
        cudavar_ar1d "const float *" size_var n (fun (x: DM<int,float32>) -> [|box x.P.DevicePointer|])
    let map_consts_f n = 
        cudavar_var "const float" n (fun (x: float32) -> [|box x|])

    let map_outs_f n = 
        cudavar_ar1d "float *" "1" n (fun (x: DM<int,float32>) -> [|box x.P.DevicePointer|])

    forward_template size_arg map_ins_f map_consts_f map_outs_f (kernel (cuda_map_redo_map size_var) size_sig) num_args

let cuda_group (num_in, names_in) =
    let f i n = n + string i
    List.collect (fun (i: int) ->
        List.map (f i) names_in) [1..num_in]

let f_zero =
    let map f () = (),(),()
    let flatten () = []
    map,flatten

let f_one =
    let map f x = f x
    let flatten x = [x]
    map,flatten

let f_two = 
    let map f (x1,x2) = 
        let (a1,v1,s1),(a2,v2,s2) = f x1, f x2
        (a1,a2),(v1,v2),(s1,s2)
    let flatten (x1,x2) = [x1;x2]
    map,flatten

let f_three =
    let map f (x1,x2,x3) = 
        let (a1,v1,s1),(a2,v2,s2),(a3,v3,s3) = f x1, f x2, f x3
        (a1,a2,a3),(v1,v2,v3),(s1,s2,s3)
    let flatten (x1,x2,x3) = [x1;x2;x3]
    map,flatten

let f_four =
    let map f (x1,x2,x3,x4) = 
        let (a1,v1,s1),(a2,v2,s2),(a3,v3,s3),(a4,v4,s4) = f x1, f x2, f x3, f x4
        (a1,a2,a3,a4),(v1,v2,v3,v4),(s1,s2,s3,s4)
    let flatten (x1,x2,x3,x4) = [x1;x2;x3;x4]
    map,flatten

let f_list =
    let map f x = List.map f (cuda_group x) |> List.unzip3
    let flatten x = x
    map, flatten

let backward_template 
        size_arg map_ins_prim_f map_consts_f map_outs_f map_ins_adj_f kernel
        ((map_ins_prim,flatten_ins_prim), (map_consts,flatten_consts), (map_outs,flatten_outs), (map_ins_adj,flatten_ins_adj))
        =
    let process_ins (ins,consts) outs =
        let ins_prim_arg, ins_prim_var, ins_prim_sig = 
            map_ins_prim map_ins_prim_f ins
        let consts_arg, consts_var, consts_sig =
            map_consts map_consts_f consts
        let outs_arg, outs_var, outs_sig =
            map_outs map_outs_f outs
        (ins_prim_arg,consts_arg,outs_arg),(ins_prim_var,consts_var,outs_var),(ins_prim_sig,consts_sig,outs_sig)
    let process_outs (ins,_) _ =
        let ins_adj_arg, ins_adj_var, ins_adj_sig = 
            map_ins_adj map_ins_adj_f ins
        ins_adj_arg, ins_adj_var, ins_adj_sig
    let process_args (ins_prim_arg, consts_arg, outs_arg) ins_adj_arg =
        size_arg :: ([flatten_ins_prim ins_prim_arg;flatten_consts consts_arg;flatten_outs outs_arg
                      flatten_ins_adj ins_adj_arg] 
                      |> List.concat)
        |> String.concat ", "
    kernel process_ins process_outs process_args

let mapcoef_backward num_args kernel =
    let size_arg, size_var,size_sig = 
        cudavar_var "const int" "n" (fun (size: int) -> [|box size|])
    
    let map_ins_prim_f n = 
        cudavar_ar1d "const float *" size_var n (fun (x: DM<int,float32>) -> [|box x.P.DevicePointer|])
    let map_consts_f n = 
        cudavar_var "const float" n (fun (x: float32) -> [|box x|])
    let map_outs_f n = 
        cudavar_ar1d "const float *" size_var n (fun (x: DM<int,float32>) -> [|box x.P.DevicePointer;box x.A.DevicePointer|])
    
    let map_ins_adj_f n = 
        cudavar_ar1d "const float *" size_var n (fun (x: DM<int,float32>) -> [|box x.A.DevicePointer|])

    backward_template size_arg map_ins_prim_f map_consts_f map_outs_f map_ins_adj_f (kernel (cuda_map size_var) size_sig) num_args

let map_redo_map_backward num_args kernel =
    let size_arg, size_var,size_sig = 
        cudavar_var "const int" "n" (fun (size: int) -> [|box size|])
            
    let map_ins_prim_f n = 
        cudavar_ar1d "const float *" size_var n (fun (x: DM<int,float32>) -> [|box x.P.DevicePointer|])
    let map_consts_f n = 
        cudavar_var "const float" n (fun (x: float32) -> [|box x|])
    let map_outs_f n = 
        cudavar_var "const float" n (fun (x: Df) -> [|box x.P.Value;box x.A.Value|])
    
    let map_ins_adj_f n = 
        cudavar_ar1d "const float *" size_var n (fun (x: DM<int,float32>) -> [|box x.A.DevicePointer|])

    // TODO: I think the backward of map_redo_map is just a plain map. Remove this comment 
    // if that turns out to be the case which it should.
    backward_template size_arg map_ins_prim_f map_consts_f map_outs_f map_ins_adj_f (kernel (cuda_map size_var) size_sig) num_args

let compile_template module_ num_args ins outs kernel_name macro =
    module_ num_args <| fun cuda_kernel size_sig process_ins process_outs process_args ->
        let ins_arg, ins_var, ins_sig = process_ins ins outs
        let outs_arg, outs_var, outs_sig = process_outs ins outs
        let args = process_args ins_arg outs_arg

        let body = cuda_kernel (macro ins_var outs_var)
        let cuda_code = cuda_kernel_module kernel_name args body |> process_statements
        cuda_code, fun str compiler_caller (grid_size: int) (block_size: int) signature_checker ->
            let kernel: CudaKernel = compiler_caller cuda_code
            kernel.GridDimensions <- dim3 grid_size
            kernel.BlockDimensions <- dim3 block_size
            kernel.RunAsync(str,signature_checker size_sig ins_sig outs_sig)

let mapcoef_forward_compile num_args ins outs kernel_name macro =
    compile_template mapcoef_forward num_args ins outs kernel_name macro

let name_no_change f name = f name
let name_into_primal f name = f (name+"_primal")
let name_into_adjoint f name = f (name+"_adjoint")
let name_into_prim_adj f name =
    let (pr_arg,pr_var,pr_sig),(adj_arg,adj_var,adj_sig) = f (name+"_primal"), f (name+"_adjoint")
    (pr_arg,adj_arg),(pr_var,adj_var),(pr_sig,adj_sig)

let name_into_x_flatten a = [a]
let name_into_prim_adj_flatten (a, b) = [a;b] // The flatteners for the rest are name_into_x_flatten

let b_zero (g_map,g_flatten) = f_zero
let b_one (g_map,g_flatten) = 
    let map f x = g_map f x
    let flatten x = [g_flatten x] |> List.concat
    map, flatten
let b_two (g_map,g_flatten) = 
    let map f (x1,x2) = 
        let (a1,v1,s1),(a2,v2,s2) = g_map f x1, g_map f x2
        (a1,a2),(v1,v2),(s1,s2)
    let flatten (x1,x2) = [g_flatten x1;g_flatten x2] |> List.concat
    map, flatten
let b_three (g_map,g_flatten) = 
    let map f (x1,x2,x3) = 
        let (a1,v1,s1),(a2,v2,s2),(a3,v3,s3) = g_map f x1, g_map f x2,g_map f x3
        (a1,a2,a3),(v1,v2,v3),(s1,s2,s3)
    let flatten (x1,x2,x3) = [g_flatten x1;g_flatten x2;g_flatten x3] |> List.concat
    map, flatten
let b_four (g_map,g_flatten) = 
    let map f (x1,x2,x3,x4) = 
        let (a1,v1,s1),(a2,v2,s2),(a3,v3,s3),(a4,v4,s4) = g_map f x1, g_map f x2, g_map f x3, g_map f x4
        (a1,a2,a3,a4),(v1,v2,v3,v4),(s1,s2,s3,s4)
    let flatten (x1,x2,x3,x4) = [g_flatten x1;g_flatten x2;g_flatten x3;g_flatten x4] |> List.concat
    map, flatten

let b_list (g_map,g_flatten) =
    let names_into_prim_and_adj names = List.collect (fun name -> [name+"_primal";name+"_adjoint"]) names
    let names_into_primals names = List.map (fun name -> name+"_primal") names
    let names_into_adjoints names = List.map (fun name -> name+"_adjoint") names

    let map f x = List.map (g_map f) (cuda_group x) |> List.unzip3
    let flatten_list x = List.collect g_flatten x
    ((map names_into_primals),flatten_list), ((map id),flatten_list), 
    ((map names_into_prim_and_adj),flatten_list),((map names_into_adjoints),flatten_list)

let b_args (num_ins, num_consts, num_outs) =
    (num_ins (name_into_primal,name_into_x_flatten)),
    (num_consts (name_no_change,name_into_x_flatten)),
    (num_outs (name_into_prim_adj,name_into_prim_adj_flatten)),
    (num_ins (name_into_adjoint,name_into_x_flatten))

let mapcoef_backward_compile num_args ins outs kernel_name macro =
    compile_template mapcoef_backward (b_args num_args) ins outs kernel_name macro

let a_zero = f_zero, b_zero
let a_one = f_one, b_one
let a_two = f_two, b_two
let a_three = f_three, b_three
let a_four = f_four, b_four

let mapcoef() = mapcoef_forward, mapcoef_backward
let map_redo_map() = map_redo_map_forward, map_redo_map_backward

let compile_fb_template module_ num_args ins outs kernel_name (macro_forward, macro_backward) =
    let a_args ((f_in,b_in),(f_con,b_con),(f_out,b_out)) = (f_in,f_con,f_out), b_args (b_in,b_con,b_out)
    let f_args,b_args = a_args num_args
    let module_forward,module_backward = module_()
    compile_template module_forward f_args ins outs kernel_name macro_forward,
    compile_template module_backward b_args ins outs (kernel_name+"Backward") macro_backward

// Kernels

let square_macro (x, ()) o i =
    cuda_inner_compile <| fun (class_method2, class1, typedef, ifvoid, set, eq, times, plus, less_than, for_, while_, madd, madd', lambda2, text, var, init, expand) ->
        set (o i) (times (x i) (x i))

let square_macro_backward (x_pr,(),(o_pr,o_adj)) x_adj i =
    cuda_inner_compile <| fun (class_method2, class1, typedef, ifvoid, set, eq, times, plus, less_than, for_, while_, madd, madd', lambda2, text, var, init, expand) ->
        madd' (x_adj i) (times (x_pr i) "2.0" |> times (o_adj i))

let square_code = lazy mapcoef_forward_compile (f_one, f_zero, f_one) ("x", ()) "o" "Square" square_macro
printfn "%A" square_code.Value

let square_backward_code = lazy mapcoef_backward_compile (b_one,b_zero,b_one) ("x",()) "o" "SquareBackward" square_macro_backward
printfn "%A" square_backward_code.Value

let square_fb = lazy compile_fb_template mapcoef (a_one, a_zero, a_one) ("x", ()) "o" "Square" (square_macro, square_macro_backward)
printfn "%A" square_fb.Value