#load "SpiralV5DM.fsx"

open System
open System.Diagnostics
open SpiralV5
open SpiralV5DM

open ManagedCuda
open ManagedCuda.BasicTypes
open ManagedCuda.VectorTypes
open ManagedCuda.CudaBlas

module Primitives =
    type VarF32 = CudaDeviceVariable<float32>

    let guardSizes (x: seq<'a>) =
        let h = Seq.head x
        let t = Seq.tail x
        Seq.iter (fun e -> if h <> e then failwithf "%A <> %A" h e) t

    let T = Operation.Transpose
    let nT = Operation.NonTranspose

    // y <- alpha * x + y
    let saxpy 
            (str: CudaStream) 
            (alpha:float32) (sx: int, x: VarF32) (sy, y: VarF32) =
        guardSizes [|sx;sy|]
        cublas.Stream <- str.Stream

        // The previous version of the library had a bug here because the size was not passed in explicitly.
        let _status = CudaBlasNativeMethods.cublasSaxpy_v2(cublas.CublasHandle, sx, ref alpha, x.DevicePointer, 1, y.DevicePointer, 1)
        Debug.WriteLine(String.Format("{0:G}, {1}: {2}", DateTime.Now, "cublasSaxpy_v2", _status))
        if _status <> CublasStatus.Success then raise <| new CudaBlasException(_status)

    /// General matrix-matrix addition. Inplace version.
    let inline geam 
            (str: CudaStream) transa transb 
            (alpha: float32) ((cols_A: int,rows_A: int), A: VarF32) 
            (beta: float32)  ((cols_B: int,rows_B: int), B: VarF32) 
                             ((cols_C: int,rows_C: int), C: VarF32) =
        let a_row = if transa = nT then rows_A else cols_A
        let a_col = if transa = nT then cols_A else rows_A
        let b_row = if transb = nT then rows_B else cols_B
        let b_col = if transb = nT then cols_B else rows_B
        
        if a_row <> b_row then failwithf "a_row <> b_row in geam! %i <> %i" a_row b_row
        if a_col <> b_col then failwithf "a_col <> b_col in geam! %i <> %i" a_col b_col

        if a_row <> rows_C then failwithf "a_row <> C_num_rows in geam! %i <> %i" a_col rows_C
        if a_col <> cols_C then failwithf "a_col <> C_num_cols in geam! %i <> %i" a_col cols_C

        let lda = if transa = nT then a_row else a_col
        let ldb = if transa = nT then b_row else b_col
        let ldc = a_row

        cublas.Stream <- str.Stream
        cublas.Geam(transa, transb, a_row, a_col, alpha, A, lda, B, ldb, beta, C, ldc)

    /// General matrix-matrix multiply from cuBLAS. Inplace version
    let inline gemm 
            (str: CudaStream) transa transb 
            (alpha: float32) ((cols_A: int,rows_A: int as sA), A: VarF32)
                             ((cols_B: int,rows_B: int as sB), B: VarF32)
            (beta: float32)  ((cols_C: int,rows_C: int as sC), C: VarF32) =

        // -------

        // These two are meant to be called from inside gemm as they lack boundary checks.
        // I've added them to enhance gemm's vector handling capabilities for online learning
        // tasks.

        /// o <- alpha * op(A) * x + beta * o
        /// Matrix-vector multiplication. Inplace version.
        let inline gemv
                (str: CudaStream) transa transb
                (alpha:float32) ((cols_A: int,rows_A: int), A: VarF32)
                                ((cols_x: int,rows_x: int), x: VarF32)
                (beta:float32)  ((cols_o: int,rows_o: int), o: VarF32) =
            let m = rows_A
            let n = cols_A
            let lda = m
            cublas.Stream <- str.Stream
            cublas.Gemv(transa, m, n, alpha, A, lda, x, 1, beta, o, 1)

        // A <- alpha * x * yT + beta * A (outer product)
        let inline ger 
                (str: CudaStream)
                (alpha: float32) ((cols_x: int,rows_x: int), x: VarF32)
                                 ((cols_y: int,rows_y: int), y: VarF32)
                (beta: float32)  ((cols_a: int,rows_a: int as sa), a: VarF32) =
            let m = max rows_x cols_x
            let n = max rows_y cols_y
            if beta <> 1.0f then geam str nT nT beta (sa, a) 0.0f (sa, a) (sa, a) 
            cublas.Stream <- str.Stream
            let _status = CudaBlasNativeMethods.cublasSger_v2(cublas.CublasHandle, m, n, ref alpha, x.DevicePointer, 1, y.DevicePointer, 1, a.DevicePointer, m)
            if _status <> CublasStatus.Success then raise <| new CudaBlasException(_status)

        // -------

        let inline is_vector (cols_x,rows_x) = rows_x = 1 || cols_x = 1

        let a_col = if transa = nT then cols_A else rows_A
        let b_row = if transb = nT then rows_B else cols_B
        if a_col <> b_row then failwithf "a_col(%i) <> b_row(%i) in gemm!" a_col b_row
        let m = if transa = nT then rows_A else cols_A
        let n = if transb = nT then cols_B else rows_B
        let k = a_col
        let lda = if transa = nT then m else k
        let ldb = if transb = nT then k else n
        let ldc = m

        if m <> rows_C || n <> cols_C then failwithf "m(%i) <> rows C(%i) || n(%i) <> cols C(%i)" m rows_C n cols_C

        // If is outer product call ger
        if a_col = 1 && b_row = 1 then 
            ger str alpha (sA, A) (sB, B) beta (sC, C)
        // If the vector is on the right side or both are vectors call gemv normally.
        elif is_vector sB then 
            gemv str transa transb alpha (sA,A) (sB,B) beta (sC,C)
        // If the vector is on the left side call gemv with the arguments switched and transposed
        // It does not actually transpose them, just their views. The function should work regardless.
        elif is_vector sA then
            let opta = if transa = nT then T else nT
            let optb = if transb = nT then T else nT
            gemv str optb opta alpha (sB,B) (sA,A) beta (sC,C)
        // Just do the standard matrix multiply
        else
            cublas.Stream <- str.Stream
            cublas.Gemm(transa, transb, m, n, k, alpha, A, lda, B, ldb, beta, C, ldc)

    let copy (to_: VarF32) (from: VarF32) (num_elems: int) (env: SpiralEnv<_>) =
        to_.AsyncCopyToDevice(from.DevicePointer,SizeT 0,SizeT 0,SizeT (sizeof<float32> * num_elems),env.Str)

    /// Does not copy the values.
    let inline dm_like (a: DM<_,_>) (env: SpiralEnv<_>) =
        env.Mem.GetDM(a.Size,default_num_vars,env)

    /// Copies only the primals.
    let inline dm_like_with_primals (a: DM<_,_>)  (env: SpiralEnv<_>) =
        let c = dm_like a env
        copy c.P a.P (size_to_total_size c.Size) env
        c

    let inline generic_operation (env: SpiralEnv<_>) forward_op =
        //let c,backward_op = forward_op()
        //if env.IsInferenceOnly = false then env.PushTape backward_op
        //c, backward_op
        forward_op() // The runner is to take care of the backwards part.

    let add_forward (alpha: float32) s (a: VarF32) beta (b: VarF32) (c: VarF32) (env: SpiralEnv<_>) =
        geam env.Str nT nT alpha (s, a) beta (s, b) (s, c)

    let add_backward (alpha: float32) s (er: VarF32) (x_adj: VarF32) (env: SpiralEnv<_>) =
        saxpy env.Str alpha (s,er) (s,x_adj)

    let inline add (alpha: float32) (a: DM<'size,float32>) beta (b: DM<'size,float32>) (c: DM<'size,float32>) (env: SpiralEnv<_>) =
        if a.Size <> b.Size then failwithf "a.Size(%A) <> b.Size(%A)" a.Size b.Size
        let s = size_to_total_size a.Size
        add_forward alpha (s,1) a.P beta b.P c.P env

        c, fun _ ->
            if a.HasAdjoint then add_backward alpha s c.A a.A env
            if b.HasAdjoint then add_backward beta s c.A b.A env

    /// An umbrella function that does simple addition if all the dimensions sizes are the same and broadcast addition if they are 4d or 2d.
    /// Raises an error otherwise.
    let add_tensor_4d_forward (alpha: float32) (sa,a: VarF32) beta (sb,b: VarF32) (env: SpiralEnv<_>) =
        let aDesc = env.Mem.GetTensorDescriptor sa
        let bDesc = env.Mem.GetTensorDescriptor sb

        cudnn.SetStream(env.Str)
        cudnn.AddTensor(beta, bDesc, b, alpha, aDesc, a) // The output is a

    let add_tensor_backwards_4d_b alpha (serr,err: VarF32) beta (sb,b_adj: VarF32) (env: SpiralEnv<_>) =
        let errDesc = env.Mem.GetTensorDescriptor serr
        let inpDesc = env.Mem.GetTensorDescriptor sb

        cudnn.SetStream env.Str
        cudnn.ConvolutionBackwardBias(beta,errDesc,err,1.0f,inpDesc,b_adj)

    let add_tensor_backwards_4d_a alpha (sa,a_adj: VarF32) beta (serr,err: VarF32) (env: SpiralEnv<_>) =
        saxpy env.Str alpha (serr,err) (sa,a_adj)

    let inline add_tensor (s_to_4d, s_to_4d_backwards) (alpha: float32) (a: DM<'size,float32>) beta (b: DM<'size,float32>) (c: DM<'size,float32>) (env: SpiralEnv<_>) =
        let sa_total = size_to_total_size a.Size
        let sa = a.Size
        let sb = b.Size

        copy c.P a.P sa_total env
        add_tensor_4d_forward alpha (s_to_4d sa,c.P) beta (s_to_4d sb,b.P) env

        c, fun _ ->
            if b.HasAdjoint then add_tensor_backwards_4d_b alpha (s_to_4d_backwards sa,c.A) beta (s_to_4d_backwards sb,b.A) env
            if c.HasAdjoint then add_tensor_backwards_4d_a alpha (sa_total,a.A) beta (sa_total,c.A) env

    let inline routed_add (s_to_4d, s_to_4d_backwards) inplace_mode (alpha: float32) (a: DM<'size,float32>) beta (b: DM<'size,float32>) (env: SpiralEnv<_>) =
        generic_operation env <| fun _ ->
            let c = if inplace_mode then a else dm_like a  env
            if a.Size = b.Size then add  alpha a beta b c env
            else add_tensor  (s_to_4d, s_to_4d_backwards) alpha a beta b c env

    let inline standard_add (alpha: float32) (a: DM< ^size,float32>) beta (b: DM< ^size,float32>) (env: SpiralEnv<_>) =
        generic_operation env <| fun _ ->
            let c = dm_like a env
            add alpha a beta b c env

    let matmult_backwards (a: DM<int*int,_>) (b: DM<int*int,_>) (c: DM<int*int,_>) (env: SpiralEnv<_>) =
        if a.HasAdjoint then gemm env.Str nT T 1.0f (c.Size,c.A) (b.Size, b.P) 1.0f (a.Size, a.A)
        if b.HasAdjoint then gemm env.Str T nT 1.0f (a.Size, a.P) (c.Size, c.A) 1.0f (b.Size, b.A)

    let seqmatmult (l: (DM<int*int,float32> * DM<int*int,float32>) list) (env: SpiralEnv<_>) =
        generic_operation env <| fun _ ->
            let sc = l |> List.map (fun (a,b) -> 
                let (cols_b,_) = b.Size
                let (_,rows_a) = a.Size
                cols_b,rows_a)
            guardSizes sc
            let cols_c, rows_c as sc = List.head sc
            let c = env.Mem.GetDM(sc, default_num_vars, env)
            for a,b in l do gemm env.Str nT nT 1.0f (a.Size,a.P) (b.Size,b.P) 0.0f (c.Size,c.P)

            c, fun _ -> for a,b in l do matmult_backwards a b c env

    let matmult (a: DM<int*int,float32>) (b: DM<int*int,float32>) (env: SpiralEnv<_>) =
        seqmatmult [a,b] env

    let grid_size_and_block_size_for_map total_size =
        min (2*numSm*(1024/map_launcher_block_size)) (divup total_size map_launcher_block_size), map_launcher_block_size

    let o_map_zero () () = ()
    let o_map_one f1 x1 = f1 x1
    let o_map_two (f1,f2) (x1,x2) = f1 x1, f2 x2
    let o_map_three (f1,f2,f3) (x1,x2,x3) = f1 x1, f2 x2, f3 x3
    let o_map_four (f1,f2,f3,f4) (x1,x2,x3,x4) = f1 x1, f2 x2, f3 x3, f4 x4

    let o_toar_zero () = [||]
    let o_toar_one x1 = [|x1|]
    let o_toar_two (x1,x2) = [|x1;x2|]
    let o_toar_three (x1,x2,x3) = [|x1;x2;x3|]
    let o_toar_four (x1,x2,x3,x4) = [|x1;x2;x3;x4|]

    let o_zero f x = o_map_zero f x |> o_toar_zero
    let o_one f x = o_map_one f x |> o_toar_one
    let o_two f x = o_map_two f x |> o_toar_two
    let o_three f x = o_map_three f x |> o_toar_three
    let o_four f x = o_map_four f x |> o_toar_four
    // 2017/1/11: Taking a 'break' from this library. I can't get it right in F#.

    let o_map_one' f x1 = f x1
    let o_map_two' f (x1,x2) = f x1, f x2
    let o_map_three' f (x1,x2,x3) = f x1, f x2, f x3
    let o_map_four' f (x1,x2,x3,x4) = f x1, f x2, f x3, f x4

    let operation_template grid_and_block_sizes signature_checker_backward has_adjoint signature_checker_forward total_size outs (kernels: Lazy<_>) (env: SpiralEnv<_>) =
        let (_,f_launch),(_,b_launch) = kernels.Value
        let str = env.Str.Stream

        f_launch str grid_and_block_sizes signature_checker_forward
        outs, fun () -> 
            if env.IsInferenceOnly = false && has_adjoint then // TODO: Maybe this should be removed?
                b_launch str grid_and_block_sizes signature_checker_backward

    let map_operation_template signature_checker_backward has_adjoint signature_checker_forward total_size outs (kernels: Lazy<_>) (env: SpiralEnv<_>) =
        operation_template (grid_size_and_block_size_for_map total_size) signature_checker_backward has_adjoint 
                            signature_checker_forward total_size outs kernels env

    // Creates a flattened view on a DM.
    let inline flatten_dm (a: DM<_,_>) = new DM<_,_>(size_to_total_size a.Size,a.Data)

    let inline map_operation_1_0_1 (a: DM<_,_>) (kernels: Lazy<_>) (env: SpiralEnv<_>) =
        let c = dm_like a env
        let total_size = size_to_total_size a.Size
        let ins = flatten_dm a
        let outs = flatten_dm c
        let signature_checker_forward size_sig (ins_sig, cvars_sig) outs_sig =
            [|size_sig total_size;ins_sig ins; outs_sig outs|]
        let has_adjoint = ins.HasAdjoint
        let signature_checker_backward size_sig (ins_prim_sig, cvars_sig, outs_prim_sig, outs_adj_sig) ins_adj_sig =
            [|size_sig total_size;ins_prim_sig ins;outs_prim_sig outs;outs_adj_sig outs;ins_adj_sig ins|]
        map_operation_template
            signature_checker_backward has_adjoint signature_checker_forward total_size 
            c (kernels: Lazy<_>) (env: SpiralEnv<_>)

    let inline map_operation_1_2_1 (a: DM<_,_>) (cvar1, cvar2) (kernels: Lazy<_>) (env: SpiralEnv<_>) =
        let c = dm_like a env
        let total_size = size_to_total_size a.Size
        let ins = flatten_dm a
        let outs = flatten_dm c
        let signature_checker_forward size_sig (ins_sig, (cvars_sig1, cvars_sig2)) outs_sig =
            [|size_sig total_size;ins_sig ins;cvars_sig1 cvar1;cvars_sig2 cvar2;outs_sig outs|]
        let has_adjoint = ins.HasAdjoint
        let signature_checker_backward size_sig (ins_prim_sig, (cvars_sig1, cvars_sig2), outs_prim_sig, outs_adj_sig) ins_adj_sig =
            [|size_sig total_size;ins_prim_sig ins;outs_prim_sig outs;cvars_sig1 cvar1;cvars_sig2 cvar2;outs_adj_sig outs;ins_adj_sig ins|]
        map_operation_template
            signature_checker_backward has_adjoint signature_checker_forward total_size 
            c (kernels: Lazy<_>) (env: SpiralEnv<_>)

    let grid_size_and_block_size_for_map_redo_map total_size =
        min (2*numSm*(1024/map_launcher_block_size)) (divup total_size map_redo_map_launcher_block_size), map_redo_map_launcher_block_size

    let map_redo_map_operation_template signature_checker_backward has_adjoint signature_checker_forward total_size outs (kernels: Lazy<_>) (env: SpiralEnv<_>) =
        operation_template (grid_size_and_block_size_for_map_redo_map total_size) signature_checker_backward has_adjoint 
                            signature_checker_forward total_size outs kernels env

    let inline map_redo_map_operation_1_0_1 (a: DM<_,_>) (kernels: Lazy<_>) (env: SpiralEnv<_>) =
        let outs = env.Mem.GetDM(Scalar,a.NumVars,env)
        let total_size = size_to_total_size a.Size
        let ins = flatten_dm a
        let c = Df.create(lazy outs.P.Gather().[0])
        let signature_checker_forward size_sig (ins_sig, cvars_sig) outs_sig =
            [|size_sig total_size;ins_sig ins; outs_sig outs|]
        let has_adjoint = ins.HasAdjoint
        let signature_checker_backward size_sig (ins_prim_sig, cvars_sig, outs_prim_sig, outs_adj_sig) ins_adj_sig =
            [|size_sig total_size;ins_prim_sig ins;outs_prim_sig c;outs_adj_sig c;ins_adj_sig ins|]
        map_redo_map_operation_template
            signature_checker_backward has_adjoint signature_checker_forward total_size 
            c (kernels: Lazy<_>) (env: SpiralEnv<_>)
                              
//    let inline seqhadmult (ab: (DM<'s,float32> * DM<'s,float32>) list) env =
//        let l = ab.Length
//        let args = ab |> List.collect (fun (a,b) -> [a;b])
//        map_operation args [] (hadmult_generic l) env
//
//    let inline hadmult  (ab: DM<'s,float32> * DM<'s,float32>) env =
//        seqhadmult [ab] env

    /// alpha * a
    let scale (alpha: float32) (a:Df) (env: SpiralEnv<_>) =
        generic_operation env <| fun _ ->
            let c = Df.create (lazy (alpha * a.P.Value))
            c, fun _ ->  a.A := alpha * !c.A + !a.A

    let sum_scalars (a:Df seq) (env: SpiralEnv<_>) =
        generic_operation env <| fun _ ->
            let c = 
                Df.create 
                    (lazy 
                        a |> Seq.fold (fun s e -> s + e.P.Value) 0.0f)

            c, fun _ -> for l in a do l.A := !c.A + !l.A

    let reshape (a: DM<_,_>) conv (env: SpiralEnv<_>) =
        generic_operation {env with IsInferenceOnly = true} <| fun _ ->
            let c = new DM<_,_>(conv a.Size,a.Data)
            c, fun _ -> ()

    let inline relu x = 
        map_operation_1_0_1 x relu_fb
    let inline tanh x =
        map_operation_1_0_1 x tanh_fb
    let inline sigmoid x =
        map_operation_1_0_1 x sigmoid_fb
    let inline clipped_sigmoid x min max =
        map_operation_1_2_1 x (min,max) clipped_sigmoid_fb
    let inline clip x min max =
        map_operation_1_2_1 x (min,max) clip_fb
    let inline square x =
        map_operation_1_0_1 x square_fb
    let inline sum x =
        map_redo_map_operation_1_0_1 x sum_fb
    let inline log x =
        map_operation_1_0_1 x log_fb
    let inline scalar_matrix_add x coef scalar =
        map_operation_1_2_1 x (coef,scalar) scalar_matrix_add_fb
