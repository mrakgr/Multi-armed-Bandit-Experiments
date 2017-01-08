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
    let madd' x v =
        [|x;" += ";v;";"|] |> state
    let lambda2 lam =
        let v,a,b = varn "lambda_", varn "lambda_arg_", varn "lambda_arg_"
        [|"const auto ";v;" = [](const auto ";a;", const auto ";b;") {"|] |> state
        lam a b
        [|"}"|] |> state
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

let forward_template size_arg map_ins_f map_consts_f map_outs_f kernel
        ((map_ins,flatten_ins), (map_consts,flatten_consts), (map_outs,flatten_outs))
        =
    let ins =
        let ins_arg, ins_var, ins_sig = 
            map_ins map_ins_f
        let consts_arg, consts_var, consts_sig =
            map_consts map_consts_f
        (ins_arg,consts_arg),(ins_var,consts_var),(ins_sig,consts_sig)
    let outs =
        let outs_arg, outs_var, outs_sig = 
            map_outs map_outs_f
        outs_arg,outs_var,outs_sig
    let args =
        let (ins_arg, consts_arg),_,_ = ins
        let outs_arg,_,_ = outs
        size_arg :: ([flatten_ins ins_arg;flatten_consts consts_arg;flatten_outs outs_arg] |> List.concat) |> String.concat ", "
    kernel ins outs args

let mapcoef_forward num_args kernel =
    let size_arg, size_var, size_sig = 
        cudavar_var "const int" (fun (size: int) -> box size) ""
    
    let map_ins_f () = 
        cudavar_ar1d "const float *" size_var (fun (x: DM<int,float32>) -> box x.P.DevicePointer) ""
    let map_consts_f () = 
        cudavar_var "const float" (fun (x: float32) -> box x) ""

    let map_outs_f () = 
        cudavar_ar1d "float *" size_var (fun (x: DM<int,float32>) -> box x.P.DevicePointer) ""

    forward_template size_arg map_ins_f map_consts_f map_outs_f (kernel (cuda_map size_var) size_sig) num_args

let map_redo_map_forward num_args kernel =
    let size_arg, size_var, size_sig =
        cudavar_var "const int" (fun (size: int) -> [|box size|]) ""
    
    let map_ins_f () =
        cudavar_ar1d "const float *" size_var (fun (x: DM<int,float32>) -> [|box x.P.DevicePointer|]) ""
    let map_consts_f () =
        cudavar_var "const float" (fun (x: float32) -> [|box x|]) ""

    let map_outs_f () =
        cudavar_ar1d "float *" "1" (fun (x: DM<int,float32>) -> [|box x.P.DevicePointer|]) ""

    forward_template size_arg map_ins_f map_consts_f map_outs_f (kernel (cuda_map_redo_map size_var) size_sig) num_args

let f_zero =
    let map f = (),(),()
    let flatten () = []
    map,flatten

let f_one =
    let map f = f ()
    let flatten x = [x]
    map,flatten

let f_two = 
    let map f =
        let (a1,v1,s1),(a2,v2,s2) = f (), f ()
        (a1,a2),(v1,v2),(s1,s2)
    let flatten (x1,x2) = [x1;x2]
    map,flatten

let f_three =
    let map f = 
        let (a1,v1,s1),(a2,v2,s2),(a3,v3,s3) = f (), f (), f ()
        (a1,a2,a3),(v1,v2,v3),(s1,s2,s3)
    let flatten (x1,x2,x3) = [x1;x2;x3]
    map,flatten

let f_four =
    let map f =
        let (a1,v1,s1),(a2,v2,s2),(a3,v3,s3),(a4,v4,s4) = f (), f (), f (), f ()
        (a1,a2,a3,a4),(v1,v2,v3,v4),(s1,s2,s3,s4)
    let flatten (x1,x2,x3,x4) = [x1;x2;x3;x4]
    map,flatten

let backward_template 
        size_arg map_ins_prim_f map_consts_f map_outs_f map_ins_adj_f kernel
        ((map_ins_prim,flatten_ins_prim), (map_consts,flatten_consts), (map_outs,flatten_outs), (map_ins_adj,flatten_ins_adj))
        =
    let ins =
        let ins_prim_arg, ins_prim_var, ins_prim_sig = 
            map_ins_prim map_ins_prim_f
        let consts_arg, consts_var, consts_sig =
            map_consts map_consts_f
        let outs_arg, outs_var, outs_sig =
            map_outs map_outs_f
        (ins_prim_arg,consts_arg,outs_arg),(ins_prim_var,consts_var,outs_var),(ins_prim_sig,consts_sig,outs_sig)
    let outs =
        let ins_adj_arg, ins_adj_var, ins_adj_sig = 
            map_ins_adj map_ins_adj_f
        ins_adj_arg, ins_adj_var, ins_adj_sig
    let args =
        let (ins_prim_arg, consts_arg, outs_arg),_,_ = ins
        let ins_adj_arg,_,_ = outs
        size_arg :: ([flatten_ins_prim ins_prim_arg;flatten_consts consts_arg;flatten_outs outs_arg
                      flatten_ins_adj ins_adj_arg] 
                      |> List.concat)
        |> String.concat ", "
    kernel ins outs args

let mapcoef_backward num_args kernel =
    let size_arg, size_var,size_sig = 
        cudavar_var "const int" (fun (size: int) -> [|box size|]) ""
    
    let map_ins_prim_f suffix = 
        cudavar_ar1d "const float *" size_var (fun (x: DM<int,float32>) -> [|box x.P.DevicePointer|]) suffix
    let map_consts_f suffix = 
        cudavar_var "const float" (fun (x: float32) -> [|box x|]) suffix
    let map_outs_f suffix = 
        cudavar_ar1d "const float *" size_var (fun (x: DM<int,float32>) -> [|box x.P.DevicePointer;box x.A.DevicePointer|]) suffix
    
    let map_ins_adj_f suffix = 
        cudavar_ar1d "const float *" size_var (fun (x: DM<int,float32>) -> [|box x.A.DevicePointer|]) suffix

    backward_template size_arg map_ins_prim_f map_consts_f map_outs_f map_ins_adj_f (kernel (cuda_map size_var) size_sig) num_args

let map_redo_map_backward num_args kernel =
    let size_arg, size_var,size_sig = 
        cudavar_var "const int" (fun (size: int) -> [|box size|]) ""
            
    let map_ins_prim_f suffix = 
        cudavar_ar1d "const float *" size_var (fun (x: DM<int,float32>) -> [|box x.P.DevicePointer|]) suffix
    let map_consts_f suffix = 
        cudavar_var "const float" (fun (x: float32) -> [|box x|]) suffix
    let map_outs_f suffix = 
        cudavar_var "const float" (fun (x: Df) -> [|box x.P.Value;box x.A.Value|]) suffix
    
    let map_ins_adj_f suffix = 
        cudavar_ar1d "const float *" size_var (fun (x: DM<int,float32>) -> [|box x.A.DevicePointer|]) suffix

    backward_template size_arg map_ins_prim_f map_consts_f map_outs_f map_ins_adj_f (kernel (cuda_map size_var) size_sig) num_args

let compile_template module_ num_args kernel_name macro =
    module_ num_args <| fun cuda_kernel size_sig (_, ins_var, ins_sig) (_, outs_var, outs_sig) args ->
        let body = cuda_kernel (macro ins_var outs_var)
        let cuda_code = cuda_kernel_module kernel_name args body |> process_statements
        cuda_code, fun str compiler_caller (grid_size: int) (block_size: int) signature_checker ->
            let kernel: CudaKernel = compiler_caller cuda_code
            kernel.GridDimensions <- dim3 grid_size
            kernel.BlockDimensions <- dim3 block_size
            kernel.RunAsync(str,signature_checker size_sig ins_sig outs_sig)

let mapcoef_forward_compile num_args kernel_name macro =
    compile_template mapcoef_forward num_args kernel_name macro

let name_no_change f = f ""
let name_into_primal f = f "_primal"
let name_into_adjoint f = f "_adjoint"
let name_into_prim_adj f =
    let (pr_arg,pr_var,pr_sig),(adj_arg,adj_var,adj_sig) = f "_primal", f "_adjoint"
    (pr_arg,adj_arg),(pr_var,adj_var),(pr_sig,adj_sig)

let name_into_x_flatten a = [a]
let name_into_prim_adj_flatten (a, b) = [a;b] // The flatteners for the rest are name_into_x_flatten

let b_zero (g_map,g_flatten) = f_zero
let b_one (g_map,g_flatten) = 
    let map f = g_map f
    let flatten x = [g_flatten x] |> List.concat
    map, flatten
let b_two (g_map,g_flatten) = 
    let map f = 
        let (a1,v1,s1),(a2,v2,s2) = g_map f, g_map f
        (a1,a2),(v1,v2),(s1,s2)
    let flatten (x1,x2) = [g_flatten x1;g_flatten x2] |> List.concat
    map, flatten
let b_three (g_map,g_flatten) = 
    let map f = 
        let (a1,v1,s1),(a2,v2,s2),(a3,v3,s3) = g_map f, g_map f, g_map f
        (a1,a2,a3),(v1,v2,v3),(s1,s2,s3)
    let flatten (x1,x2,x3) = [g_flatten x1;g_flatten x2;g_flatten x3] |> List.concat
    map, flatten
let b_four (g_map,g_flatten) = 
    let map f = 
        let (a1,v1,s1),(a2,v2,s2),(a3,v3,s3),(a4,v4,s4) = g_map f, g_map f, g_map f, g_map f
        (a1,a2,a3,a4),(v1,v2,v3,v4),(s1,s2,s3,s4)
    let flatten (x1,x2,x3,x4) = [g_flatten x1;g_flatten x2;g_flatten x3;g_flatten x4] |> List.concat
    map, flatten

let b_args (num_ins, num_consts, num_outs) =
    (num_ins (name_into_primal,name_into_x_flatten)),
    (num_consts (name_no_change,name_into_x_flatten)),
    (num_outs (name_into_prim_adj,name_into_prim_adj_flatten)),
    (num_ins (name_into_adjoint,name_into_x_flatten))

let mapcoef_backward_compile num_args kernel_name macro =
    compile_template mapcoef_backward (b_args num_args) kernel_name macro

let a_zero = f_zero, b_zero
let a_one = f_one, b_one
let a_two = f_two, b_two
let a_three = f_three, b_three
let a_four = f_four, b_four

let mapcoef() = mapcoef_forward, mapcoef_backward
let map_redo_map() = map_redo_map_forward, map_redo_map_backward

let compile_fb_template module_ num_args kernel_name (macro_forward, macro_backward) =
    let a_args ((f_in,b_in),(f_con,b_con),(f_out,b_out)) = (f_in,f_con,f_out), b_args (b_in,b_con,b_out)
    let f_args,b_args = a_args num_args
    let module_forward,module_backward = module_()
    compile_template module_forward f_args kernel_name macro_forward,
    compile_template module_backward b_args (kernel_name+"Backward") macro_backward

// List arguments

let cuda_group (num_in, names_in) =
    let f i n = n + string i
    List.collect (fun (i: int) ->
        List.map (f i) names_in) [1..num_in]

let f_list (num_in, names_in as args) =
    let map f x = List.map f (cuda_group args) |> List.unzip3
    let flatten x = x
    map, flatten

let b_list (num_in, names_in as args) (g_map,g_flatten) =
    let names_into_prim_and_adj names = List.collect (fun name -> [name+"_primal";name+"_adjoint"]) names
    let names_into_primals names = List.map (fun name -> name+"_primal") names
    let names_into_adjoints names = List.map (fun name -> name+"_adjoint") names

    let map f x = List.map (g_map f) (cuda_group args) |> List.unzip3
    let flatten_list x = List.collect g_flatten x
    ((map names_into_primals),flatten_list), ((map id),flatten_list), 
    ((map names_into_prim_and_adj),flatten_list),((map names_into_adjoints),flatten_list)

// Kernels

let map_1_0_1 name forward_op backward_op =
    let forward_macro (x, ()) o i (return_,lambda2, class1, typedef, ifvoid, set, for_, while_, madd', text, var as funs) =
        let x,o = x i, o i // Note that this does not get evaluated, it will instead get expanded to the full thing on the Cuda side.
        set o (forward_op x)
    let backward_macro (x_pr,(),(o_pr,o_adj)) x_adj i (return_,lambda2, class1, typedef, ifvoid, set, for_, while_, madd', text, var as funs) =
        let x_pr, o_pr, o_adj, x_adj = x_pr i, o_pr i, o_adj i, x_adj i
        madd' x_adj (o_adj .* backward_op x_pr o_pr)
    lazy compile_fb_template mapcoef (a_one, a_zero, a_one) name (forward_macro, backward_macro)

let map_1_2_1 name forward_op backward_op =
    let forward_macro (x,consts) o i (return_,lambda2, class1, typedef, ifvoid, set, for_, while_, madd', text, var as funs) =
        let x,o = x i, o i // Note that this does not get evaluated, it will instead get expanded to the full thing on the Cuda side.
        set o (forward_op x consts)
    let backward_macro (x_pr,consts,(o_pr,o_adj)) x_adj i (return_,lambda2, class1, typedef, ifvoid, set, for_, while_, madd', text, var as funs) =
        let x_pr, o_pr, o_adj, x_adj = x_pr i, o_pr i, o_adj i, x_adj i
        madd' x_adj (o_adj .* backward_op x_pr consts o_pr)
    lazy compile_fb_template mapcoef (a_one, a_two, a_one) name (forward_macro, backward_macro)

let clip_fb_template name fw_op bw_op = 
    map_1_2_1 name
        (fun x (min,max) -> if_ (x .< min) min (if_ (x .> max) max (fw_op x)))
        (fun x_pr (min,max) o_pr -> if_ (x_pr .< min .|| x_pr .> max) "0" (bw_op x_pr o_pr))

let sig_fw x = "1" ./ ("1" .+ (exp (neg x)))
let sig_bw x_pr o_pr = o_pr .* ("1" .- o_pr)

let sigmoid_fb = map_1_0_1 "Sigmoid" sig_fw sig_bw
let tanh_fb = map_1_0_1 "Tanh" (fun x -> tanh x) (fun x_pr o_pr -> "1" .- o_pr .* o_pr)
let relu_fb = map_1_0_1 "Relu" (fun x -> if_ (x .> "0") x "0") (fun x_pr o_pr -> if_ (x_pr .> "0") "1" "0")
let log_fb = map_1_0_1 "Log" (fun x -> log x) (fun x_pr o_pr -> "1" ./ x_pr)
let square_fb = map_1_0_1 "Square" (fun x -> x .* x) (fun x_pr o_pr -> x_pr .* "2")

let clip_fb = clip_fb_template "Clip" id (fun _ _ -> "1")
let clipped_sigmoid_fb = clip_fb_template "ClippedSigmoid" sig_fw sig_bw

let map_redo_map_1_0_1 name (forward_map_load_op, forward_reduce_op, forward_store_op) backward_op =
    let forward_macro_load (x, ()) i =
        forward_map_load_op (x i)
    let forward_macro_reduce (return_,lambda2, class1, typedef, ifvoid, set, for_, while_, madd', text, var as funs) a b = 
        return_ (forward_reduce_op a b)
    let forward_macro_store o result (return_,lambda2, class1, typedef, ifvoid, set, for_, while_, madd', text, var as funs) =
        set (o "0") (forward_store_op result)
    let backward_macro (x_pr,(),(o_pr,o_adj)) x_adj i (return_,lambda2, class1, typedef, ifvoid, set, for_, while_, madd', text, var as funs) =
        let x_pr, o_pr, o_adj, x_adj = x_pr i, o_pr i, o_adj i, x_adj i
        madd' x_adj (o_adj .* backward_op x_pr o_pr)
    let forward_macro ins outs =
        forward_macro_load ins, forward_macro_reduce, forward_macro_store outs
    compile_fb_template mapcoef (a_one, a_zero, a_one) name (forward_macro, backward_macro)