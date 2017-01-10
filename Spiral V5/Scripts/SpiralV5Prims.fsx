#load "SpiralV5CudaCodeGen.fsx"
open System
open System.Diagnostics
open SpiralV5
open SpiralV5DM
open SpiralV5CudaCodeGen

open ManagedCuda
open ManagedCuda.BasicTypes
open ManagedCuda.VectorTypes
open ManagedCuda.CudaBlas

module Primitives =
    type CallerVar =
    | CInt of int
    | CF32 of float32
    | CArrayInt of CudaDeviceVariable<int>
    | CArrayF32 of CudaDeviceVariable<float32>

    let kernel_caller (grid_size: int) (block_size: int) (str: CudaStream) ex (ks: Lazy<_>) (args_in: CallerVar list) (args_out: CallerVar list) =
        let kernel, signs: CudaKernel * _ = ex ks.Value
        kernel.GridDimensions <- dim3 grid_size
        kernel.BlockDimensions <- dim3 block_size
        let signs_in, signs_out = List.splitAt args_in.Length signs

        List.iter2 (fun arg sign ->
            match arg, sign with
            | CInt _, CudaVar(_, CudaConst CudaInt) -> ()
            | CF32 _, CudaVar(_, CudaConst CudaFloat) -> ()
            | CArrayInt _, CudaArray(_, CudaConst CudaInt, _) -> ()
            | CArrayF32 _, CudaArray(_, CudaConst CudaFloat, _) -> ()
            | x,y -> failwithf "Typechecking failed for input arguments of kernel %s.\nThe non-matching types are %A,%A" kernel.KernelName x y
            ) args_in signs_in

        List.iter2 (fun arg sign ->
            match arg, sign with
            | CInt _, CudaVar(_, CudaInt) -> ()
            | CF32 _, CudaVar(_, CudaFloat) -> ()
            | CArrayInt _, CudaArray(_, CudaInt, _) -> ()
            | CArrayF32 _, CudaArray(_, CudaFloat, _) -> ()
            | x,y -> failwithf "Typechecking failed for input arguments of kernel %s.\nThe non-matching types are %A,%A" kernel.KernelName x y
            ) args_out signs_out

        let f x = x |> List.map (function
            | CInt x -> box x
            | CF32 x -> box x
            | CArrayInt x -> box x
            | CArrayF32 x -> box x
            )

        let a1 = f args_in
        let a2 = f args_out

        kernel.RunAsync(str.Stream, a1 @ a2 |> List.toArray)

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

    let inline dm_like (a: DM<_,_>) (env: SpiralEnv<_>) =
        env.Mem.GetDM(a.Size,default_num_vars,env)

    /// Copies only the primals.
    let inline dm_like_using_obj_pool (a: DM<_,_>)  (env: SpiralEnv<_>) =
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

    let guarded_map_to_caller_var (a: DM<_,_> list) f =
        a 
        |> List.map (fun x -> x.Size)
        |> guardSizes

        a |> List.map (fun x -> f x)

    let operation_prelude (a: DM<_,_> list) (c: DM<_,_>) cvars =
        let input_prims = guarded_map_to_caller_var a <| fun x -> CArrayF32 x.P
        let input_adjs = a |> List.map (fun x -> CArrayF32 x.A)
        let cvars = List.map (fun x -> CF32 x) cvars
        let out_prim = CArrayF32 c.P
        let out_adj = CArrayF32 c.A

        input_prims, input_adjs, cvars, out_prim, out_adj

    let grid_size_and_block_size_for_map total_size =
        min (2*numSm*(1024/map_launcher_block_size)) (divup total_size map_launcher_block_size), map_launcher_block_size

    let inline mutable_map_operation desired_args (a: DM<_,_>) cvars kernel env =
        let cvars = cvars |> List.map CF32
        let ins = get_args a desired_args |> List.map CArrayF32

        let a_total_size = size_to_total_size a.Size
        let grid_size, block_size = grid_size_and_block_size_for_map a_total_size
        let size = CInt a_total_size

        kernel_caller grid_size block_size
            env.Str id kernel cvars (size :: ins)

    let inline map_operation (a: _ list) cvars kernels env =
        generic_operation env <| fun () ->
            let h = a.Head
            let c = dm_like h env

            let input_prims, input_adjs, cvars, out_prim, out_adj = operation_prelude a c cvars

            let h_total_size = size_to_total_size h.Size
            let grid_size, block_size = grid_size_and_block_size_for_map h_total_size
            let size = CInt h_total_size

            kernel_caller grid_size block_size 
                env.Str fst kernels (size :: input_prims @ cvars) [out_prim]

            c, fun () ->
                let as_have_adjoint = a |> List.exists (fun x -> x.HasAdjoint)
                if as_have_adjoint then
                    kernel_caller grid_size block_size 
                        env.Str snd kernels (size :: out_prim :: out_adj :: input_prims @ cvars) input_adjs

    let grid_size_and_block_size_for_map_redo_map total_size =
        min (2*numSm*(1024/map_launcher_block_size)) (divup total_size map_redo_map_launcher_block_size), map_redo_map_launcher_block_size

    let inline map_redo_map_operation (a: _ list) cvars kernels env =
        generic_operation env <| fun () ->
            let h = a.Head
            let c = env.Mem.GetDM(Scalar,default_num_vars,env)

            let input_prims, input_adjs, cvars, out_prim, _ = operation_prelude a c cvars

            let h_total_size = size_to_total_size h.Size
            let grid_size, block_size = grid_size_and_block_size_for_map_redo_map h_total_size
            let size = CInt h_total_size

            kernel_caller grid_size block_size
                env.Str fst kernels (size :: input_prims @ cvars) [out_prim]

            let c' = Df.create(lazy c.P.Gather().[0])
            c', fun () ->
                let as_have_adjoint = a |> List.exists (fun x -> x.HasAdjoint)
                if as_have_adjoint then
                    let out_prim = c'.P.Value |> CF32
                    let out_adj = c'.A.Value |> CF32
                    kernel_caller grid_size block_size 
                        env.Str snd kernels (out_prim :: out_adj :: size :: input_prims @ cvars) input_adjs
    
    let inline seqhadmult (ab: (DM<'s,float32> * DM<'s,float32>) list) env =
        let l = ab.Length
        let args = ab |> List.collect (fun (a,b) -> [a;b])
        map_operation args [] (hadmult_generic l) env

    let inline hadmult  (ab: DM<'s,float32> * DM<'s,float32>) env =
        seqhadmult [ab] env

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
        map_operation  [x] [] relu
    let inline tanh x =
        map_operation  [x] [] tanh
    let inline sigmoid x =
        map_operation  [x] [] sigmoid
    let inline clipped_sigmoid x min max =
        map_operation  [x] [min;max] clipped_sigmoid
    let inline clip x min max =
        map_operation  [x] [min;max] clip
    let inline square x =
        map_operation  [x] [] square
    let inline sum x =
        map_redo_map_operation [x] [] sum
    let inline log x =
        map_operation [x] [] log_
    let inline scalar_matrix_add x coef scalar =
        map_operation [x] [coef;scalar] scalar_matrix_add
