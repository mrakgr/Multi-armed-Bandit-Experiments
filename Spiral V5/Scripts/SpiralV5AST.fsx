#load "SpiralV5DM.fsx"
open System
open System.Diagnostics
open SpiralV5
open SpiralV5DM
open SpiralV5CudaCodeGen

open ManagedCuda
open ManagedCuda.BasicTypes
open ManagedCuda.VectorTypes
open ManagedCuda.CudaBlas

// Unlike in the last iteration of the library, I need the ids to make sure that the expressions are evaluated only once.
type SpiralExp =
// Root nodes
| BaseNode of DM<float32>
// Basic operations
| Matmult of id: int * SpiralExp * SpiralExp
| SeqMatmult of id: int * (SpiralExp * SpiralExp) list
| Add of id: int * SpiralExp * SpiralExp
| BAdd of id: int * matrix: SpiralExp * vector: SpiralExp // Addition with broadcasting.
| Hadmult of id: int * SpiralExp * SpiralExp
| SeqHadmult of id: int * (SpiralExp * SpiralExp) list // TODO: Make optimized implementation.
// Activations
| Relu of id: int * SpiralExp
| Tanh of id: int * SpiralExp
| Sigmoid of id: int * SpiralExp
| ClippedSigmoid of id: int * min: float32 * max: float32 * SpiralExp // TODO: Make optimized implementation
| SoftmaxInstance of id: int * SpiralExp
| SoftmaxChannel of id: int * SpiralExp // TODO: I forgot what these two softmaxes are supposed to be doing.
| Clip of id: int * min: float32 * max: float32 * SpiralExp
// Normalization functions
| BatchNorm of id: int * SpiralExp
| LayerNorm of id: int * SpiralExp // TODO: Need to implement this one.
// 4d operations
| Convolve of id: int * SpiralExp * SpiralExp
| Pool of id: int * SpiralExp
// Cost function auxiliaries.
| Square of id: int * SpiralExp
| Sum of id: int * SpiralExp
| Scale of id: int * SpiralExp
| SumScalars of id: int * SpiralExp
| Log of id: int * SpiralExp
| ScalarMatrixAdd of id: int * SpiralExp
// Optimized cost functions
| SquaredError of id: int * SpiralExp // TODO: Need to implement this one.
| CrossEntropy of id: int * SpiralExp // TODO: Need to implement this one.

type VarF32 = CudaDeviceVariable<float32>

let guardSizes (x: 'a[]) = 
    for i=1 to x.Length-1 do
        if x.[i-1] <> x.[i] then failwithf "%A <> %A" x.[i-1] x.[i]

let T = Operation.Transpose
let nT = Operation.NonTranspose

// y <- alpha * x + y
let saxpy 
        (str: CudaStream) 
        (alpha:float32) ((c: int,r: int as sx), x: VarF32) (sy, y: VarF32) =
    guardSizes [|sx;sy|]
    cublas.Stream <- str.Stream

    // The previous version of the library had a bug here because the size was not passed in explicitly.
    let _status = CudaBlasNativeMethods.cublasSaxpy_v2(cublas.CublasHandle, c*r, ref alpha, x.DevicePointer, 1, y.DevicePointer, 1)
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

type CallerVar =
| CInt of int
| CF32 of float32
| CArrayInt of CudaDeviceVariable<int>
| CArrayF32 of CudaDeviceVariable<float32>

let kernel_caller (str: CudaStream) (kernel: CudaKernel) (signs: CudaVar list) (args_in: CallerVar list) (args_out: CallerVar list) =
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

let map_launcher (str: CudaStream) (ks: Lazy<CudaKernel * CudaVar list>) 
        (args_in: (int[] * VarF32) list) 
        (args_out: (int[] * VarF32) list) =
    // Checks whether all the sizes are the same.
    args_in @ args_out
    |> List.toArray
    |> Array.map fst
    |> guardSizes

    let f x = x |> List.map (fun (ex,x) -> CArrayF32 x)

    let total_size = args_in.Head |> fst |> total_size_of

    let args_in = f args_in
    let args_out = f args_out

    let kernel, sig_ = ks.Value
    let block_size = map_launcher_block_size
    let gridSize = min (2*numSm*(1024/block_size)) (divup total_size block_size)
    kernel.GridDimensions <- dim3(gridSize)
    kernel.BlockDimensions <- dim3(block_size)
    kernel_caller str kernel sig_ args_in args_out

let default_num_vars = 2

let rec eval (env: SpiralEnv) x =
    let eval' x = eval env x
    let if_not_evaluated id f =
        match env.Nodes.TryGetValue id with
        | true, v -> v
        | false, _ -> f()
            
    let eval2d x er =
        let x: DM<float32> = eval env x
        let sx: int * int =
            match x.Size with
            | [|cols_a;rows_a|] -> cols_a, rows_a
            | _ -> failwith er
        sx, x

    /// Relu, Tanh and Sigmoid have so much in common that it is worth factoring them out.
    /// The would be identical if not for the fact that the backwards steps for tanh and sigmoid need
    /// the outputs rather than input as one of the backwards call arguments.
    let activation_f id x forward backward ex =
        let x = eval' x
        let c = env.Mem.GetDM(x.Size,default_num_vars, env)
        map_launcher env.Str forward x.TotalSize [|x.TotalSize;x.P.DevicePointer;c.P.DevicePointer|]

        env.Nodes.Add(id,c)

        if env.IsInferenceOnly = false then
            if c.HasAdjoint then 
                let relu_backward () = 
                    map_launcher env.Str backward x.TotalSize [|x.TotalSize;c.A.DevicePointer;ex (c,x);x.A.DevicePointer|]
                env.PushTape relu_backward
        c

    match x with
    // Root nodes
    | BaseNode x -> x
    // Basic operations
    | Matmult(id,a,b) ->
        if_not_evaluated id <| fun _ ->
            let er = "Input to matmult must be 2D."
            let ((cols_a,rows_a as sa), a), ((cols_b,rows_b as sb), b) = eval2d a er, eval2d b er
            let cols_c,rows_c as sc = cols_b, rows_a
            let c = env.Mem.GetDM([|cols_c; rows_c|],default_num_vars, env)
            let aP, bP, cP = primal a, primal b, primal c

            gemm env.Str nT nT 1.0f (sa,aP) (sb,bP) 0.0f (sc,cP)

            env.Nodes.Add(id,c)

            if env.IsInferenceOnly = false then
                if a.HasAdjoint then 
                    let matmult_backward_left () = 
                        gemm env.Str nT T 1.0f (sc,c.A) (sb, b.P) 1.0f (sa, a.A)
                    env.PushTape matmult_backward_left

                if b.HasAdjoint then 
                    let matmult_backward_right () = 
                        gemm env.Str T nT 1.0f (sa, a.P) (sc, c.A) 1.0f (sb, b.A)
                    env.PushTape matmult_backward_right
            c

    | Relu(id, x) ->
        if_not_evaluated id <| fun _ ->
            activation_f id x relu relu_backward (fun (c,x) -> x.P.DevicePointer)
    | Tanh(id, x) ->
        if_not_evaluated id <| fun _ ->
            activation_f id x tanh tanh_backward (fun (c,x) -> c.P.DevicePointer)
    | Sigmoid(id, x) ->
        if_not_evaluated id <| fun _ ->
            activation_f id x tanh tanh_backward (fun (c,x) -> c.P.DevicePointer)