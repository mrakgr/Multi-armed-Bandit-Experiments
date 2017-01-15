#load "SpiralV5CudaModules.fsx"

open ManagedCuda.VectorTypes
open SpiralV5
open SpiralV5DM
open SpiralV5CudaModules

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
        cudavar_var "const int" (fun (size: int) -> box size) ""
    
    let map_ins_f () =
        cudavar_ar1d "const float *" size_var (fun (x: DM<int,float32>) -> box x.P.DevicePointer) ""
    let map_consts_f () =
        cudavar_var "const float" (fun (x: float32) -> box x) ""

    let map_outs_f () =
        cudavar_ar1d "float *" "1" (fun (x: DM<Scalar,float32>) -> box x.P.DevicePointer) ""

    forward_template size_arg map_ins_f map_consts_f map_outs_f (kernel (cuda_map_redo_map "float" size_var) size_sig) num_args

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
        size_arg map_ins_prim_f map_consts_f map_outs_prim_f map_outs_adj_f map_ins_adj_f kernel
        ((map_ins_prim,flatten_ins_prim), (map_consts,flatten_consts), 
         (map_outs_prim,flatten_outs_prim), (map_outs_adj,flatten_outs_adj),
         (map_ins_adj,flatten_ins_adj))
        =
    let ins =
        let ins_prim_arg, ins_prim_var, ins_prim_sig = 
            map_ins_prim map_ins_prim_f
        let consts_arg, consts_var, consts_sig =
            map_consts map_consts_f
        let outs_prim_arg, outs_prim_var, outs_prim_sig =
            map_outs_prim map_outs_prim_f
        let outs_adj_arg, outs_adj_var, outs_adj_sig =
            map_outs_adj map_outs_adj_f
        (ins_prim_arg,consts_arg,outs_prim_arg,outs_adj_arg),(ins_prim_var,consts_var,outs_prim_var,outs_adj_var),
        (ins_prim_sig,consts_sig,outs_prim_sig,outs_adj_sig)
    let outs =
        let ins_adj_arg, ins_adj_var, ins_adj_sig = 
            map_ins_adj map_ins_adj_f
        ins_adj_arg, ins_adj_var, ins_adj_sig
    let args =
        let (ins_prim_arg, consts_arg, outs_prim_arg, outs_adj_arg),_,_ = ins
        let ins_adj_arg,_,_ = outs
        size_arg :: ([flatten_ins_prim ins_prim_arg;flatten_consts consts_arg;
                      flatten_outs_prim outs_prim_arg;flatten_outs_adj outs_adj_arg // ins

                      flatten_ins_adj ins_adj_arg] //outs
                      |> List.concat)
        |> String.concat ", "
    kernel ins outs args

let mapcoef_backward num_args kernel =
    let size_arg, size_var,size_sig = 
        cudavar_var "const int" (fun (size: int) -> box size) ""
    
    let map_ins_prim_f suffix =
        cudavar_ar1d "const float *" size_var (fun (x: DM<int,float32>) -> box x.P.DevicePointer) suffix
    let map_consts_f suffix =
        cudavar_var "const float" (fun (x: float32) -> box x) suffix
    let map_outs_prim_f suffix =
        cudavar_ar1d "const float *" size_var (fun (x: DM<int,float32>) -> box x.P.DevicePointer) suffix
    let map_outs_adj_f suffix =
        cudavar_ar1d "const float *" size_var (fun (x: DM<int,float32>) -> box x.A.DevicePointer) suffix
    
    let map_ins_adj_f suffix = 
        cudavar_ar1d "float *" size_var (fun (x: DM<int,float32>) -> box x.A.DevicePointer) suffix

    backward_template size_arg map_ins_prim_f map_consts_f map_outs_prim_f map_outs_adj_f map_ins_adj_f (kernel (cuda_map size_var) size_sig) num_args

let map_redo_map_backward num_args kernel =
    let size_arg, size_var,size_sig = 
        cudavar_var "const int" (fun (size: int) -> box size) ""
            
    let map_ins_prim_f suffix = 
        cudavar_ar1d "const float *" size_var (fun (x: DM<int,float32>) -> box x.P.DevicePointer) suffix
    let map_consts_f suffix = 
        cudavar_var "const float" (fun (x: float32) -> box x) suffix
    let map_outs_prim_f suffix = 
        cudavar_var "const float" (fun (x: Df) -> box x.P.Value) suffix
    let map_outs_adj_f suffix = 
        cudavar_var "const float" (fun (x: Df) -> box x.A.Value) suffix
    
    let map_ins_adj_f suffix = 
        cudavar_ar1d "float *" size_var (fun (x: DM<int,float32>) -> box x.A.DevicePointer) suffix

    backward_template size_arg map_ins_prim_f map_consts_f map_outs_prim_f map_outs_adj_f map_ins_adj_f (kernel (cuda_map size_var) size_sig) num_args

let compile_template module_ num_args kernel_name macro =
    module_ num_args <| fun cuda_kernel size_sig (_, ins_var, ins_sig) (_, outs_var, outs_sig) args ->
        let body = cuda_kernel (macro ins_var outs_var)
        let cuda_code = cuda_kernel_module kernel_name args body |> process_statements
        let kernel = compile_kernel_using_nvcc_bat_router kernel_name cuda_code
        cuda_code, fun str (grid_size: int, block_size: int) signature_checker ->
            kernel.GridDimensions <- dim3 grid_size
            kernel.BlockDimensions <- dim3 block_size
            kernel.RunAsync(str,signature_checker size_sig ins_sig outs_sig)

let name_no_change f = f ""
let name_into_primal f = f "_primal"
let name_into_adjoint f = f "_adjoint"

let b_zero g_map = f_zero
let b_one g_map = 
    let map f = g_map f
    let flatten x = [x]
    map, flatten
let b_two g_map = 
    let map f = 
        let (a1,v1,s1),(a2,v2,s2) = g_map f, g_map f
        (a1,a2),(v1,v2),(s1,s2)
    let flatten (x1,x2) = [x1;x2]
    map, flatten
let b_three g_map = 
    let map f = 
        let (a1,v1,s1),(a2,v2,s2),(a3,v3,s3) = g_map f, g_map f, g_map f
        (a1,a2,a3),(v1,v2,v3),(s1,s2,s3)
    let flatten (x1,x2,x3) = [x1;x2;x3]
    map, flatten
let b_four g_map = 
    let map f = 
        let (a1,v1,s1),(a2,v2,s2),(a3,v3,s3),(a4,v4,s4) = g_map f, g_map f, g_map f, g_map f
        (a1,a2,a3,a4),(v1,v2,v3,v4),(s1,s2,s3,s4)
    let flatten (x1,x2,x3,x4) = [x1;x2;x3;x4]
    map, flatten

let b_args (num_ins, num_consts, num_outs) =
    (num_ins name_into_primal),
    (num_consts name_no_change),
    (num_outs name_into_primal),
    (num_outs name_into_adjoint),
    (num_ins name_into_adjoint)

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
    let map f = List.map (fun _ -> f ()) (cuda_group args) |> List.unzip3
    let flatten x = x
    map, flatten

let b_list (num_in, names_in as args) g_map =
    let map f = List.map (fun _ -> g_map f) (cuda_group args) |> List.unzip3
    let flatten_list x = x
    map, flatten_list

let a_list num_in names_in = f_list (num_in, names_in), b_list (num_in, names_in)

// Kernels

let map_1_0_1 name forward_op backward_op =
    let forward_macro (x, ()) o i (return_,lambda2, class1, typedef, ifvoid, set, for_, while_, madd', text, var as funs) =
        let def = var "auto"
        let x = def (x i)
        set (o i) (forward_op x)
    let backward_macro (x_pr,(),o_pr,o_adj) x_adj i (return_,lambda2, class1, typedef, ifvoid, set, for_, while_, madd', text, var as funs) =
        let def = var "auto"
        let x_pr, o_pr, o_adj = def (x_pr i), def (o_pr i), def (o_adj i)
        madd' (x_adj i) (o_adj .* backward_op x_pr o_pr)
    lazy compile_fb_template mapcoef (a_one, a_zero, a_one) name (forward_macro, backward_macro)

let map_1_2_1 name forward_op backward_op =
    let forward_macro (x,consts) o i (return_,lambda2, class1, typedef, ifvoid, set, for_, while_, madd', text, var as funs) =
        let def = var "auto"
        let x = def (x i)
        set (o i) (forward_op x consts)
    let backward_macro (x_pr,consts,o_pr,o_adj) x_adj i (return_,lambda2, class1, typedef, ifvoid, set, for_, while_, madd', text, var as funs) =
        let def = var "auto"
        let x_pr, o_pr, o_adj = def (x_pr i), def (o_pr i), def (o_adj i)
        madd' (x_adj i) (o_adj .* backward_op x_pr consts o_pr)
    lazy compile_fb_template mapcoef (a_one, a_two, a_one) name (forward_macro, backward_macro)

let clip_fw fw_op x (min, max) = if_ (x .< min) min (if_ (x .> max) max (fw_op x))
let clip_bw bw_op x_pr (min, max) o_pr = if_ (x_pr .< min .|| x_pr .> max) "0" (bw_op x_pr o_pr)
let clip_fb_template name fw_op bw_op = map_1_2_1 name (clip_fw fw_op) (clip_bw bw_op)

let sig_fw x = "1" ./ ("1" .+ (exp (neg x)))
let sig_bw x_pr o_pr = o_pr .* ("1" .- o_pr)

let sigmoid_fb = map_1_0_1 "Sigmoid" sig_fw sig_bw
let tanh_fb = map_1_0_1 "Tanh" (fun x -> tanh x) (fun x_pr o_pr -> "1" .- o_pr .* o_pr)
let relu_fb = map_1_0_1 "Relu" (fun x -> if_ (x .> "0") x "0") (fun x_pr o_pr -> if_ (x_pr .> "0") "1" "0")
let log_fb = map_1_0_1 "Log" (fun x -> log x) (fun x_pr o_pr -> "1" ./ x_pr)
let square_fb = map_1_0_1 "Square" (fun x -> x .* x) (fun x_pr o_pr -> x_pr .* "2")

let clip_fb = clip_fb_template "Clip" id (fun _ _ -> "1")
let clipped_sigmoid_fb = clip_fb_template "ClippedSigmoid" sig_fw sig_bw
let scalar_matrix_add_fb = 
    map_1_2_1 "ScalarMatrixAdd" 
        (fun x (coef,scalar) -> coef .* x .+ scalar) 
        (fun x_pr (coef,scalar) o_pr -> coef)

let map_redo_map_1_0_1 name (forward_map_load_op, forward_reduce_op, forward_store_op) backward_op =
    let forward_macro_load (x, ()) i =
        forward_map_load_op (x i)
    let forward_macro_reduce (return_,lambda2, class1, typedef, ifvoid, set, for_, while_, madd', text, var as funs) a b = 
        return_ (forward_reduce_op a b)
    let forward_macro_store o result (return_,lambda2, class1, typedef, ifvoid, set, for_, while_, madd', text, var as funs) =
        set (o "0") (forward_store_op result)
    let forward_macro ins outs =
        forward_macro_load ins, forward_macro_reduce, forward_macro_store outs
    let backward_macro (x_pr,(),o_pr,o_adj) x_adj i (return_,lambda2, class1, typedef, ifvoid, set, for_, while_, madd', text, var as funs) =
        let def = var "auto"
        let x_pr = def (x_pr i)
        madd' (x_adj i) (o_adj .* backward_op x_pr o_pr)
    lazy compile_fb_template map_redo_map (a_one, a_zero, a_one) name (forward_macro, backward_macro)

let sum_fb = map_redo_map_1_0_1 "Sum" (id,(.+),id) (fun _ _ -> "1")

let mutable_mapcoef kernel_name num_args macro_forward =
    lazy compile_template mapcoef_forward num_args kernel_name macro_forward

let gradclip = 
    mutable_mapcoef "GradClip" (f_zero,f_one,f_one)
    <| fun ((),bound) x i (return_,lambda2, class1, typedef, ifvoid, set, for_, while_, madd', text, var as funs) ->
        let x = x i
        set x (if_ (x .< neg bound) (neg bound) (if_ (x .> bound) bound x))

let sgd = 
    mutable_mapcoef "Sgd" (f_zero,f_one,f_two)
    <| fun ((),learning_rate) (x_pr,x_adj) i (return_,lambda2, class1, typedef, ifvoid, set, for_, while_, madd', text, var as funs) ->
        let x_pr, x_adj = x_pr i, x_adj i
        madd' x_pr (x_adj .* learning_rate)
        set x_adj "0"

let clipped_sgd = 
    mutable_mapcoef "ClippedSgd" (f_zero,f_two,f_two)
    <| fun ((),(learning_rate,bound)) (x_pr,x_adj) i (return_,lambda2, class1, typedef, ifvoid, set, for_, while_, madd', text, var as funs) ->
        let x_pr, x_adj = x_pr i, x_adj i
        let x_adj_clipped = clip_fw id x_adj (neg bound,bound)
        madd' x_pr (x_adj_clipped .* learning_rate)
        set x_adj "0"

let random_nomalization = 
    mutable_mapcoef "RandomNormalization" (f_zero,f_two,f_one)
    <| fun ((),(scaling_factor,location)) x i (return_,lambda2, class1, typedef, ifvoid, set, for_, while_, madd', text, var as funs) ->
        let x = x i
        set x (scaling_factor .* (x .- "0.5") .+ location)

// The hadmult module generic in the number of input arguments.
let hadmult_generic =
    memoize <| fun num_input_pairs ->
        let rec forward_hadmult i = function
            | a :: b :: [] ->
                (a i) .* (b i)
            | a :: b :: t ->
                (a i) .* (b i) .+ forward_hadmult i t
            | x -> failwithf "Should never reach here. x = %A" x

        let name = "Hadmult"
        let forward (ins,()) o i (return_,lambda2, class1, typedef, ifvoid, set, for_, while_, madd', text, var as funs) =
            set (o i) (forward_hadmult i ins)
        let backward (ins_prim,(),o_pr,o_adj) ins_adj i (return_,lambda2, class1, typedef, ifvoid, set, for_, while_, madd', text, var as funs) =
            let chunk2 l =
                List.chunkBySize 2 l
                |> List.map (fun [a;b] -> (a,b))
            let adjl = chunk2 ins_adj
            let priml = chunk2 ins_prim // Organizes the primals and the adjoints into pairs of two.

            let o_adj = var "auto" (o_adj i)
            List.iter2 (fun (adj_a,adj_b) (prim_a,prim_b) ->
                madd' (adj_a i) (o_adj .* (prim_b i))
                madd' (adj_b i) (o_adj .* (prim_a i))
                ) adjl priml
                
        let args_in = a_list num_input_pairs ["a";"b"]
        lazy compile_fb_template mapcoef (args_in, a_zero, a_one) name (forward, backward)
  
let get_accuracy_forward num_args kernel =
    let size_arg, size_var, size_sig =
        cudavar_var "const int" (fun (size: int) -> box size) ""
    
    let map_ins_f () =
        cudavar_ar1d "const float *" size_var (fun (x: DM<int,float32>) -> box x.P.DevicePointer) ""
    let map_consts_f () =
        cudavar_var "const float" (fun (x: float32) -> box x) ""

    let map_outs_f () =
        cudavar_ar1d "float *" "1" (fun (x: DM<Scalar,float32>) -> box x.P.DevicePointer) "",
        cudavar_ar1d "int *" "1" (fun (x: DM<Scalar,int>) -> box x.P.DevicePointer) ""

    forward_template size_arg map_ins_f map_consts_f map_outs_f (kernel (cuda_map_redo_map "thrust::tuple<int,float>" size_var) size_sig) num_args

let f_accuracy = 
    let map f =
        let (a1,v1,s1),(a2,v2,s2) = f ()
        (a1,a2),(v1,v2),(s1,s2)
    let flatten (x1,x2) = [x1;x2]
    map,flatten

let get_accuracy =
    let forward_macro_load (x, ()) i =
        thrust_make_tuple [|"float";"int"|] [|x i; i|]
    let forward_macro_reduce (return_,lambda2, class1, typedef, ifvoid, set, for_, while_, madd', text, var as funs) a b = 
        return_ (if_ (a .> b) a b)
    let forward_macro_store (o1,o2) result (return_,lambda2, class1, typedef, ifvoid, set, for_, while_, madd', text, var as funs) =
        set (o1 "0") (thrust_get_tuple "0" result)
        set (o2 "0") (thrust_get_tuple "1" result) // TODO, fix this.
    let forward_macro ins outs =
        forward_macro_load ins, forward_macro_reduce, forward_macro_store outs

    lazy compile_template get_accuracy_forward (f_one, f_zero, f_accuracy) "GetAccuracy" forward_macro
