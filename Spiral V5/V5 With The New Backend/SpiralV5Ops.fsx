#load "SpiralV5DevVar.fsx"
open System
open System.Diagnostics
open SpiralV5CudaInit
open SpiralV5DevVar

open ManagedCuda
open ManagedCuda.BasicTypes
open ManagedCuda.VectorTypes
open ManagedCuda.CudaBlas

let T = Operation.Transpose
let nT = Operation.NonTranspose

let inline guard_sizes x y =
    if x <> y then failwithf "guard_sizes failed.\n%A <> %A" x y
    y

let guard_cublas _status = if _status <> CublasStatus.Success then raise <| new CudaBlasException(_status)

// y <- alpha * x + y
let inline axpy_template axpy (str: CudaStream) alpha (sx: int, x) (sy, y) =
    if sx <> sy then failwithf "%A <> %A" sx sy
    cublas.Stream <- str.Stream

    // The previous version of the library had a bug here because the size was not passed in explicitly.
    axpy (cublas.CublasHandle, sx, ref alpha, x, 1, y, 1) |> guard_cublas

let axpy_f32 str alpha x y = axpy_template CudaBlasNativeMethods.cublasSaxpy_v2 str alpha x y
let axpy_f64 str alpha x y = axpy_template CudaBlasNativeMethods.cublasDaxpy_v2 str alpha x y

/// General matrix-matrix addition. Inplace version.
let inline geam_template geam
        (str: CudaStream) transa transb 
        alpha ((cols_A: int,rows_A: int), A) 
        beta  ((cols_B: int,rows_B: int), B) 
        ((cols_C: int,rows_C: int), C) =
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
    
    geam (cublas.CublasHandle, transa, transb, a_row, a_col, ref alpha, A, lda, ref beta, B, ldb, C, ldc) |> guard_cublas

let geam_f32 str transa transb alpha a beta b c = geam_template CudaBlasNativeMethods.cublasSgeam str transa transb alpha a beta b c
let geam_f64 str transa transb alpha a beta b c = geam_template CudaBlasNativeMethods.cublasDgeam str transa transb alpha a beta b c

/// General matrix-matrix multiply from cuBLAS. Inplace version
let inline gemm_template zero one geam ger gemv gemm
        (str: CudaStream) transa transb 
        alpha ((cols_A: int,rows_A: int as sA), A)
              ((cols_B: int,rows_B: int as sB), B)
        beta  ((cols_C: int,rows_C: int as sC), C) =

    // -------

    // These two are meant to be called from inside gemm as they lack boundary checks.
    // I've added them to enhance gemm's vector handling capabilities for online learning
    // tasks.

    /// o <- alpha * op(A) * x + beta * o
    /// Matrix-vector multiplication. Inplace version.
    let inline gemv
            (str: CudaStream) transa transb
            alpha ((cols_A: int,rows_A: int), A)
                  ((cols_x: int,rows_x: int), x)
            beta  ((cols_o: int,rows_o: int), o) =
        let m = rows_A
        let n = cols_A
        let lda = m
        cublas.Stream <- str.Stream
        gemv (cublas.CublasHandle, transa, m, n, ref alpha, A, lda, x, 1, ref beta, o, 1) |> guard_cublas

    // A <- alpha * x * yT + beta * A (outer product)
    let inline ger 
            (str: CudaStream)
            alpha ((cols_x: int,rows_x: int), x)
                  ((cols_y: int,rows_y: int), y)
            beta  ((cols_a: int,rows_a: int as sa), a) =
        let m = max rows_x cols_x
        let n = max rows_y cols_y
        if one <> beta then geam str nT nT beta (sa, a) zero (sa, a) (sa, a) 
        cublas.Stream <- str.Stream
        ger(cublas.CublasHandle, m, n, ref alpha, x, 1, y, 1, a, m) |> guard_cublas

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
        gemm(cublas.CublasHandle, transa, transb, m, n, k, ref alpha, A, lda, B, ldb, ref beta, C, ldc) |> guard_cublas

let gemm_f32 str transa transb alpha a b beta c =
    gemm_template 0.0f 1.0f geam_f32 CudaBlasNativeMethods.cublasSger_v2 CudaBlasNativeMethods.cublasSgemv_v2 CudaBlasNativeMethods.cublasSgemm_v2
        str transa transb alpha a b beta c

let gemm_f64 str transa transb alpha a b beta c =
    gemm_template 0.0 1.0 geam_f64 CudaBlasNativeMethods.cublasDger_v2 CudaBlasNativeMethods.cublasDgemv_v2 CudaBlasNativeMethods.cublasDgemm_v2
        str transa transb alpha a b beta c

type GenericPrim = GenericPrim with
    static member Gemm(_: GenericPrim,str,transa,transb,alpha,a,b,beta,c) = gemm_f32 str transa transb alpha a b beta c
    static member Gemm(_: GenericPrim,str,transa,transb,alpha,a,b,beta,c) = gemm_f64 str transa transb alpha a b beta c
    static member Geam(_: GenericPrim,str,transa,transb,alpha,a,beta,b,c) = geam_f32 str transa transb alpha a beta b c
    static member Geam(_: GenericPrim,str,transa,transb,alpha,a,beta,b,c) = geam_f64 str transa transb alpha a beta b c
    static member Axpy(_: GenericPrim,str,alpha,x,y) = axpy_f32 str alpha x y
    static member Axpy(_: GenericPrim,str,alpha,x,y) = axpy_f64 str alpha x y

let inline gemm str transa transb alpha a b beta c = 
    ((^a or ^f or ^v): (static member Gemm: ^a * CudaStream * Operation * Operation * ^f * ^v * ^v * ^f * ^v -> unit) 
        GenericPrim,str,transa,transb,alpha,a,b,beta,c)

let inline geam str transa transb alpha a beta b c = 
    ((^a or ^f or ^v): (static member Geam: ^a * CudaStream * Operation * Operation * ^f * ^v * ^f * ^v * ^v -> unit) 
        GenericPrim,str,transa,transb,alpha,a,beta,b,c)

let inline axpy str alpha x y = 
    ((^a or ^f or ^v): (static member Axpy: ^a * CudaStream * ^f * ^v * ^v -> unit) 
        GenericPrim,str,alpha,x,y)

let inline add proj_2d proj_1d alpha (a: DM) beta (b: DM) (c: DM) (env: SpiralEnv<_>) = // TODO: Figure out how to do projections.
    guard_sizes a.Size b.Size |> guard_sizes c.Size |> ignore
    geam env.Str nT nT alpha (a.P' proj_2d) beta (b.P' proj_2d) (c.P' proj_2d)

    c, fun _ ->
        if a.HasAdjoint then axpy env.Str alpha (c.A' proj_1d) (a.A' proj_1d)
        if b.HasAdjoint then axpy env.Str beta (c.A' proj_1d) (b.A' proj_1d)

open System.Collections.Generic
open SpiralV5CudaTypechecker_v6e
open SpiralV5CudaCodegen_v3a

let compile kernel inputs = 
    let get = function Succ x -> x | _ -> failwith "Error"
    match eval kernel inputs with
    | Succ k -> 
        printfn "%s" k
        let h = k |> hash |> string
        compile_kernel_using_nvcc_bat_router h k
    | Fail x -> failwithf "Kernel failed to compile.\n%A" x

let call_map = compile map_module |> memoize
let call_map_redo_map = compile map_redo_map_module |> memoize
let call_map_redocol_map = compile map_redocol_map_module |> memoize

let reserve_tags n =
    let tags = ResizeArray()
    for i=1 to n do
        tags.Add(get_tag())
    tags

let reserved_tags = reserve_tags 100

let map =
    let n = TyV (reserved_tags.[0], PrimT UInt64T)
    fun (str: CudaStream) map_op (inputs: DM list) (outputs: DM list) ->
        let args = inputs @ outputs
        let total_size = total_size args.Head
        let block_size = 256UL
        let grid_size = min (2UL*numSm*(1024UL/block_size)) (divup total_size block_size)

        let dims = dim3(int block_size), dim3(int grid_size)

        match args with
        | x :: xs -> List.fold (fun x y -> guard_sizes x (size y)) (size x) xs |> ignore
        | [] -> ()

        let to_typechecking_form =
            let mutable i = 0
            let inc() = i <- i+1; i
            let conv (x : DM) = V' (reserved_tags.[inc()],GlobalArrayT([n],PrimT x.Type))
            fun inputs ->
                match inputs with
                | [x] -> conv x
                | x :: xs -> VV (List.map conv inputs)
                | [] -> B
        let ins = to_typechecking_form inputs
        let outs = to_typechecking_form outputs

        let map_op = 
            inl (VV [V "i";V "ins";V "outs"])
                (s [l (V "indexer") (inl (V "x") (ArrayIndex(V "x",[V "i"])))
                    l (V "ins") (VVMap(V "indexer",V "ins"))
                    mset (VVMap(V "indexer",V "outs")) (ap map_op (V "ins"))
                    ] B)

        let kernel = call_map (VV [map_op; CudaExpr.T n; ins; outs], dims)

        let get_ptrs x = List.map (fun (x: DM) -> box x.P.GetDevicePtr.Value) x |> List.toArray
        let args = [|[|box total_size|];get_ptrs inputs; get_ptrs outputs|] |> Array.concat
        kernel.RunAsync(str.Stream,args)
