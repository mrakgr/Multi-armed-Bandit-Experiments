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

let default_num_vars = 2

type Scalar = Scalar

type SpiralExpDMF32<'size,'conv when 'size: equality and 'conv: equality> = SpiralExp<DM<'size,float32>,'size,'conv>
and SpiralExpF32<'conv when 'conv: equality> = SpiralExp<Df,Scalar,'conv>
// Unlike in the last iteration of the library, I need the ids to make sure that the expressions are evaluated only once.
and SpiralExp<'a,'size,'conv when 'size: equality and 'conv: equality> =
// Root nodes
| BaseNode of 'a //DM<float32>
// Basic operations
// Note: The `return_type: (DM<'size,float32> -> 'a)` things are just hooks for the typechecker to associate the evaluator return types with
// the generic parameter in the SpiralExp. It is a way of emulating GADTs with standard F# discriminated unions.
| Matmult of id: int * SpiralExpDMF32<int*int,'conv> * SpiralExpDMF32<int*int,'conv> * return_type: (DM<int*int,float32> -> 'a)
| SeqMatmult of id: int * (SpiralExpDMF32<int*int,'conv> * SpiralExpDMF32<int*int,'conv>) list * return_type: (DM<int*int,float32> -> 'a)
// Addition with broadcasting.
// TODO: Turn this into BroadcastOp later.
| Add of id: int * size_to_4d: ('size -> int*int*int*int) * size_to_4d_backwards: ('size -> int*int*int*int) * 
          alpha: float32 * matrix: SpiralExpDMF32<'size,'conv> * beta: float32 * 
          vector: SpiralExpDMF32<'size,'conv> * return_type: (DM<'size,float32> -> 'a)
| Hadmult of id: int * SpiralExpDMF32<'size,'conv> * SpiralExpDMF32<'size,'conv> * return_type: (DM<'size,float32> -> 'a)
| SeqHadmult of id: int * (SpiralExpDMF32<'size,'conv> * SpiralExpDMF32<'size,'conv>) list * return_type: (DM<'size,float32> -> 'a)
// Activations
| Relu of id: int * SpiralExpDMF32<'size,'conv> * return_type: (DM<'size,float32> -> 'a)
| Tanh of id: int * SpiralExpDMF32<'size,'conv> * return_type: (DM<'size,float32> -> 'a)
| Sigmoid of id: int * SpiralExpDMF32<'size,'conv> * return_type: (DM<'size,float32> -> 'a)
| ClippedSigmoid of id: int * min: float32 * max: float32 * SpiralExpDMF32<'size,'conv> * return_type: (DM<'size,float32> -> 'a)
//| SoftmaxInstance of id: int * SpiralExpDMF32
//| SoftmaxChannel of id: int * SpiralExpDMF32 // TODO: I forgot what these two softmaxes are supposed to be doing.
| Clip of id: int * min: float32 * max: float32 * SpiralExpDMF32<'size,'conv> * return_type: (DM<'size,float32> -> 'a)
// Normalization functions
//| BatchNorm of id: int * SpiralExp
//| LayerNorm of id: int * SpiralExp // TODO: Need to implement this one.
// 4d operations
//| Convolve of id: int * SpiralExp * SpiralExp
//| Pool of id: int * SpiralExp
// Cost function auxiliaries.
| Square of id: int * SpiralExpDMF32<'size,'conv> * return_type: (DM<'size,float32> -> 'a)
| Sum of id: int * SpiralExpDMF32<'size,'conv> * return_type: (Df -> 'a)
| Log of id: int * SpiralExpDMF32<'size,'conv> * return_type: (DM<'size,float32> -> 'a)
| ScalarMatrixAdd of id: int * SpiralExpDMF32<'size,'conv> * coef: float32 * scalar: float32 * return_type: (DM<'size,float32> -> 'a)
| Scale of id: int * SpiralExpF32<'conv> * return_type: (Df -> 'a)
| SumScalars of id: int * SpiralExpF32<'conv> [] * return_type: (Df -> 'a)
// Cost functions
| SquaredError of id: int * SpiralExpDMF32<'size,'conv> * return_type: (Df -> 'a) // TODO: Make an optimized implementation.
| CrossEntropy of id: int * SpiralExpDMF32<'size,'conv> * return_type: (Df -> 'a) // TODO: Make an optimized implementation.
// Converters - the 'conv generic parameter in all those other branches is just used in this one.
| ConvertTo of id: int * SpiralExpDMF32<'size,'conv> * conv: ('size -> 'conv) * return_type: (DM<'conv,float32> -> 'a)

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

let copy (to_: VarF32) (from: VarF32) (num_elems: int) (env: SpiralEnv) =
    to_.AsyncCopyToDevice(from.DevicePointer,SizeT 0,SizeT 0,SizeT (sizeof<float32> * num_elems),env.Str)

let like_dm (a: DM<_,_>) (env: SpiralEnv) =
    env.Mem.GetDM(a.Size,a.TotalSizeInElems,default_num_vars,env)

/// Copies only the primals.
let copy_dm_using_obj_pool (a: DM<_,_>) (env: SpiralEnv) =
    let c = like_dm a env
    copy c.P a.P a.TotalSizeInElems env
    c

let add_forward (alpha: float32) s (a: VarF32) beta (b: VarF32) (c: VarF32) (env: SpiralEnv) =
    geam env.Str nT nT alpha (s, a) beta (s, b) (s, c)
let add_backward (alpha: float32) s (er: VarF32) (x_adj: VarF32) (env: SpiralEnv) =
    let add_backward() = saxpy env.Str alpha (s,er) (s,x_adj)
    env.PushTape add_backward

let add (alpha: float32) (a: DM<'size,float32>) beta (b: DM<'size,float32>) (c: DM<'size,float32>) (env: SpiralEnv) =
    if a.Size <> b.Size then failwithf "a.Size(%A) <> b.Size(%A)" a.Size b.Size
    let s = a.TotalSizeInElems
    add_forward alpha (s,1) a.P beta b.P c.P env

    if env.IsInferenceOnly then
        add_backward alpha s c.A a.A env
        add_backward beta s c.A b.A env

/// An umbrella function that does simple addition if all the dimensions sizes are the same and broadcast addition if they are 4d or 2d.
/// Raises an error otherwise.
let add_tensor_4d_forward (alpha: float32) (sa,a: VarF32) beta (sb,b: VarF32) (env: SpiralEnv) =
    let aDesc = env.Mem.GetTensorDescriptor sa
    let bDesc = env.Mem.GetTensorDescriptor sb

    cudnn.SetStream(env.Str)
    cudnn.AddTensor(beta, bDesc, b, alpha, aDesc, a) // The output is a

let add_tensor_backwards_4d_b alpha (serr,err: VarF32) beta (sb,b_adj: VarF32) (env: SpiralEnv) =
    let tensor_add_right_backwards () =
        cudnn.SetStream env.Str
        let errDesc = env.Mem.GetTensorDescriptor serr
        let inpDesc = env.Mem.GetTensorDescriptor sb
        cudnn.ConvolutionBackwardBias(beta,errDesc,err,1.0f,inpDesc,b_adj)
    
    env.PushTape tensor_add_right_backwards

let add_tensor_backwards_4d_a alpha (sa,a_adj: VarF32) beta (serr,err: VarF32) (env: SpiralEnv) =
    let tensor_add_left_backwards () = saxpy env.Str alpha (serr,err) (sa,a_adj)
    env.PushTape(tensor_add_left_backwards)

let add_tensor s_to_4d s_to_4d_backwards (alpha: float32) (a: DM<'size,float32>) beta (b: DM<'size,float32>) (c: DM<'size,float32>) (env: SpiralEnv) =
    let sa_total = a.TotalSizeInElems
    let sa = a.Size
    let sb = b.Size

    copy c.P a.P sa_total env
    add_tensor_4d_forward alpha (s_to_4d sa,c.P) beta (s_to_4d sb,b.P) env

    if env.IsInferenceOnly = false then
        add_tensor_backwards_4d_b alpha (s_to_4d_backwards sa,c.A) beta (s_to_4d_backwards sb,b.A) env
        add_tensor_backwards_4d_a alpha (sa_total,a.A) beta (sa_total,c.A) env

//let s_to_4d (c,r) = (c,1,r,1)
//let s_to_4d_backwards (c,r) = (c,r,1,1) // A hack to make the backwards step 10x faster

let routed_add id' s_to_4d s_to_4d_backwards (alpha: float32) (a: DM<'size,float32>) beta (b: DM<'size,float32>) (env: SpiralEnv) =
    let c = like_dm a env

    if a.Size = b.Size then add alpha a beta b c env
    else add_tensor s_to_4d s_to_4d_backwards alpha a beta b c env

    env.Nodes.Add(id',c)
    c

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

// Trying to figure out how to do a generic launcher function with closures is making me depressed so I'll
// use a discriminated union type instead.
type GenericLauncher<'size when 'size: equality> =
| MapLauncher of args_in: ('size * int * VarF32) list * args_cvar: float32 list * args_out: ('size * int * VarF32) list
| MapRedoMapLauncher of args_in: ('size * int * VarF32) list * args_cvar: float32 list * args_out: (Scalar * int * VarF32) list
//| MapRedocolMapLauncher

let generic_lanucher (str: CudaStream) (ks: Lazy<CudaKernel * CudaVar list>) lan =
    // Checks whether all the sizes are the same.
    match lan with
    | MapLauncher(args_in,args_cvar,args_out) -> args_in @ args_out
    | MapRedoMapLauncher(args_in,args_cvar,args_out) -> args_in
    |> List.map (fun (a,b,c) -> a,b)
    |> guardSizes

    let proccess_array x = List.map (fun (_,_,x) -> CArrayF32 x) x
    let process_cvars x = List.map CF32 x
    let process_total_size (_,total_size_in_elems,_) = total_size_in_elems
    let process_all (args_in: _ list,args_cvar,args_out) =
        process_total_size args_in.Head, proccess_array args_in, process_cvars args_cvar, proccess_array args_out

    let (total_size, args_in, args_cvar, args_out), block_size = 
        match lan with
        | MapLauncher(args_in,args_cvar,args_out) -> 
            process_all (args_in,args_cvar,args_out), map_launcher_block_size
        | MapRedoMapLauncher(args_in,args_cvar,args_out) ->
            process_all (args_in,args_cvar,args_out), map_redo_map_launcher_block_size

    let kernel, sig_ = ks.Value

    let gridSize = min (2*numSm*(1024/block_size)) (divup total_size block_size)
    kernel.GridDimensions <- dim3(gridSize)
    kernel.BlockDimensions <- dim3(block_size)
    kernel_caller str kernel sig_ (CInt total_size :: args_in @ args_cvar) args_out

let map_launcher (str: CudaStream) (ks: Lazy<CudaKernel * CudaVar list>) 
        (args_in: ('size * int * VarF32) list) 
        (args_cvar: float32 list)
        (args_out: ('size * int * VarF32) list) =
    generic_lanucher str ks (MapLauncher(args_in,args_cvar,args_out))

let map_redo_map_launcher (str: CudaStream) (ks: Lazy<CudaKernel * CudaVar list>)
        (args_in: ('size * int * VarF32) list)
        (args_cvar: float32 list)
        (args_out: (Scalar * int * VarF32) list) =
    generic_lanucher str ks (MapRedoMapLauncher(args_in,args_cvar,args_out))

let matmult_backwards (a: DM<int*int,_>) (b: DM<int*int,_>) (c: DM<int*int,_>) (env: SpiralEnv) =
    if a.HasAdjoint then 
        let matmult_backward_left () = 
            gemm env.Str nT T 1.0f (c.Size,c.A) (b.Size, b.P) 1.0f (a.Size, a.A)
        env.PushTape matmult_backward_left

    if b.HasAdjoint then 
        let matmult_backward_right () = 
            gemm env.Str T nT 1.0f (a.Size, a.P) (c.Size, c.A) 1.0f (b.Size, b.A)
        env.PushTape matmult_backward_right

let seqmatmult id (l: (DM<int*int,float32> * DM<int*int,float32>) list) (env: SpiralEnv) =
    let sc = l |> List.map (fun (a,b) -> 
        let (cols_b,_) = b.Size
        let (_,rows_a) = a.Size
        cols_b,rows_a)
    guardSizes sc
    let cols_c, rows_c as sc = List.head sc
    let c = env.Mem.GetDM(sc,cols_c*rows_c,default_num_vars, env)
    for a,b in l do gemm env.Str nT nT 1.0f (a.Size,a.P) (b.Size,b.P) 0.0f (c.Size,c.P)

    if env.IsInferenceOnly = false then
        for a,b in l do matmult_backwards a b c env

    env.Nodes.Add(id,c)
    c

let matmult id (a: DM<int*int,float32>) (b: DM<int*int,float32>) (env: SpiralEnv) =
    seqmatmult id [a,b] env

// Look, just like for the mapcoef_module and mapcoef_redo_map modules, I spent like 3 hours in wain trying to abstract the two functions. 
// The annoying thing is that the slight differences set them apart enough that any abstraction that I can come up ends up being both
// more convoluted and longer than the original.

// The solution to this problem would be D's string mixins or Lisp's macros, but here I am just going to copy paste this crap.
// I can't deal with this.
let generic_activation_reduce id (x: DM<'size,float32> list) cvars forward backward (env: SpiralEnv) (c: DM<_,_>) launcher_for launcher_back =
    let input_prims = x |> List.map (fun x -> x.P')

    launcher_for env.Str forward input_prims cvars [c.P']
    let c = Df.create (lazy c.P.Gather().[0])

    env.Nodes.Add(id,c)

    if env.IsInferenceOnly = false then
        let input_adjs = x |> List.map (fun x -> x.A')
        let activation_backward () =
            let err_args = [c.P.Value;c.A.Value]
            launcher_back env.Str backward err_args input_prims cvars input_adjs
        env.PushTape activation_backward
    c

let generic_activation_map id (x: DM<'size,float32> list) cvars forward backward (env: SpiralEnv) (c: DM<_,_>) launcher =
    let input_prims = x |> List.map (fun x -> x.P')

    launcher env.Str forward input_prims cvars [c.P']

    env.Nodes.Add(id,c)

    if env.IsInferenceOnly = false then
        if c.HasAdjoint then
            let input_adjs = x |> List.map (fun x -> x.A')
            let activation_backward () =
                let err_args = [c.P';c.A']
                launcher env.Str backward (err_args @ input_prims) cvars input_adjs
            env.PushTape activation_backward
    c

let activations_map id' (x: DM<_,float32> list) cvars forward backward (env: SpiralEnv) =
    let h = x.Head
    let c = env.Mem.GetDM(h.Size,h.TotalSizeInElems,default_num_vars, env)
    generic_activation id' x cvars forward backward env c map_launcher 
        (fun c -> c)

let activations_map_redo_map id' (x: DM<_,float32> list) cvars forward backward (env: SpiralEnv) =
    let c = env.Mem.GetDM(Scalar,1,default_num_vars, env)
    generic_activation id' x cvars forward backward env c map_redo_map_launcher
        (fun c -> Df.create (lazy c.P.Gather().[0]))
    
let activation id (x: DM<_,float32>) cvars forward backward (env: SpiralEnv) =
    activations_map id [x] cvars forward backward env 

let seqhadmult id (ab: (DM<'s,float32> * DM<'s,float32>) list) env =
    let l = ab.Length
    let forward_kernel = hadmult_generic_memoized l
    let backward_kernel = hadmult_backward_generic_memoized l
    let args = ab |> List.collect (fun (a,b) -> [a;b])
    activations_map id args [] forward_kernel backward_kernel env

let hadmult id (ab: DM<'s,float32> * DM<'s,float32>) env =
    seqhadmult id [ab] env

let rec eval<'a,'size,'conv when 'size: equality and 'conv: equality> (env: SpiralEnv) (x: SpiralExp<'a,'size,'conv>): 'a =
    let eval' x = eval env x
    let if_not_evaluated r id f =
        match env.Nodes.TryGetValue id with
        | true, v -> v :?> _
        | false, _ -> f()
        |> r

    match x with
    // Root nodes
    | BaseNode x -> x
    // Basic operations
    | Matmult(id, a, b, r) ->
        if_not_evaluated r id <| fun _ ->
            matmult id (eval' a) (eval' b) env
    | SeqMatmult(id, l, r) -> 
        if_not_evaluated r id <| fun _ ->
            let l = l |> List.map (fun (x,y) -> eval' x, eval' y)
            seqmatmult id l env
    | Add(id, s_to_4d, s_to_4d_backwards, alpha, a, beta, b, r) ->
        if_not_evaluated r id <| fun _ ->
            routed_add id s_to_4d s_to_4d_backwards alpha (eval' a) beta (eval' b) env
    | Hadmult(id, a, b, r) -> 
        if_not_evaluated r id <| fun _ -> hadmult id (eval' a, eval' b) env
    | SeqHadmult(id , abs, r) ->
        if_not_evaluated r id <| fun _ -> 
            seqhadmult id (abs |> List.map (fun (a,b) -> eval' a, eval' b)) env
    | Relu(id, x, r) ->
        if_not_evaluated r id <| fun _ -> activation id (eval' x) [] relu relu_backward env
    | Tanh(id, x, r) ->
        if_not_evaluated r id <| fun _ -> activation id (eval' x) [] tanh tanh_backward env
    | Sigmoid(id, x, r) ->
        if_not_evaluated r id <| fun _ -> activation id (eval' x) [] tanh tanh_backward env
    | Clip(id, min, max, x, r) ->
        if_not_evaluated r id <| fun _ -> activation id (eval' x) [min;max] clip clip_backward env
    | ClippedSigmoid(id, min, max, x, r) ->
        if_not_evaluated r id <| fun _ -> activation id (eval' x) [min;max] clipped_sigmoid clipped_sigmoid_backward env
    | Square(id, x, r) ->
        if_not_evaluated r id <| fun _ -> activation id (eval' x) [] square square_backward env
    | Sum(id, x, r) ->
        if_not_evaluated r id <| fun _ -> 
////| Sum of id: int * SpiralExpDMF32 * return_type: (Df -> 'a)
////| Scale of id: int * SpiralExpDMF32 * return_type: (Df -> 'a)
////| SumScalars of id: int * SpiralExpF32 [] * return_type: (Df -> 'a)
////| Log of id: int * SpiralExpDMF32 * return_type: (DM<float32> -> 'a)
////| ScalarMatrixAdd of id: int * SpiralExpDMF32 * coef: float32 * scalar: float32 * return_type: (DM<float32> -> 'a)