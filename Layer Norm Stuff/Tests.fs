open System
open SpiralV4
open SpiralV4.Flame
open ManagedCuda
open ManagedCuda.BasicTypes
open ManagedCuda.VectorTypes

cuda_context.Synchronize()
let ctx = Context<_>.create

let getd2M (x: d2M) =
    let t = x.GPV.Gather()
    Array2D.init x.Rows x.Columns (fun row col -> t.[col*x.Rows + row])

let d2MtoCudaArray (x: d2M): CudaGlobalArray<float32> =
    let p = nativeint x.GPP.Pointer
    CudaGlobalArray<_>.create(x.Size,p)

let d2MtoCuda2dArray (x: d2M): CudaGlobal2dArray<float32> =
    let p = nativeint x.GPP.Pointer
    CudaGlobal2dArray<_>.create(x.Columns,x.Rows,p)

let inline test_launcher(str: CudaStream, kernel: CudaKernel, x, o: Ar) =
    let inline f x = (^a: (member Conv: obj) x)
    kernel.GridDimensions <- dim3(24)
    kernel.BlockDimensions <- dim3(128)
    let args: obj [] = [|f x; f o|]
    kernel.RunAsync(str.Stream, args)

let map_test() =
    let simple_map_kernel =
        <@
        let kernel_main (x: Ar, o: Ar) =
            let mutable i = BlockIdx.x*BlockDim.x + ThreadIdx.x
            while i < x.length do
                o.[i] <- x.[i]*2.0f
                i <- i + BlockDim.x
        ()
        @>

    let kernel_main_name = "MapTest" // kernel_main will be renamed to this.
    let prog = compile kernel_main_name simple_map_kernel
    printfn "%s" prog

    let cuda_kernel = compile_kernel_nvrtc prog kernel_main_name

    let cols, rows = 10, 1
    let a = d2M.create((rows,cols)) 
            |> fun x -> fillRandomUniformMatrix ctx.Str x 1.0f 0.0f; x 
    let a' = d2MtoCudaArray a

    printfn "%A" (getd2M a)

    let o = d2M.create((rows,cols))
    let o' = d2MtoCudaArray o

    test_launcher(ctx.Str,cuda_kernel,a',o')
    cuda_context.Synchronize()

    printfn "%A" (getd2M o)

let mapredocolmap_test() =
    let mapredocolmap_kernel map_load neutral_element shuffle redo map_out =
        <@
        let map_load: 'a -> 'b = %map_load
        let shuffle: 'b * int -> 'b = %shuffle
        let redo: 'b * 'b -> 'b = %redo
        let map_out: 'b -> 'c = %map_out

        let warp_reduce(value: 'b) =
            let mutable v = value
            _unroll()
            for i=0 to 3 do
                v <- redo(shuffle(v, 1 <<< i), v)
            v

        let block_reduce(v: 'b) =
            let mutable temp = CudaSharedArray(32)
            if ThreadIdx.x < 32 then
                temp.[ThreadIdx.x] <- %neutral_element
            let out_partial = warp_reduce v
            _syncthreads()
            if ThreadIdx.x % 32 = 0 then
                temp.[ThreadIdx.x / 32] <- out_partial
            _syncthreads()
            warp_reduce temp.[ThreadIdx.x % 32]

        let kernel_main (x: CudaGlobal2dArray<'a>, o: CudaGlobalArray<'c>) =
            let mutable col = BlockIdx.x
            while col < x.num_cols do
                let mutable row = ThreadIdx.x
                let mutable v = %neutral_element
                while row < x.num_rows do
                    v <- redo(map_load x.[col,row], v)
                    row <- row + BlockDim.x
                let final_result = map_out(block_reduce v)
                if ThreadIdx.x = 0 then o.[col] <- final_result
                col <- col + GridDim.x
        ()
        @>

    let kernel_main_name = "MapRedocolTest" // kernel_main will be renamed to this.
    let k = mapredocolmap_kernel 
                <@ fun x -> x @> // map_load
                <@ 0.0f @> // neutral_elem
                <@ fun (v,i) -> Shuffle.Xor(v,i) @> // shuffle
                <@ fun (a,b) -> a+b @> // redo
                <@ fun x -> x @> // map_out

    let prog = compile kernel_main_name k
    printfn "%s" prog

    let cuda_kernel = compile_kernel_nvrtc prog kernel_main_name

    let cols, rows = 128, 128
    let a = d2M.create((rows,cols)) 
            |> fun x -> fillRandomUniformMatrix ctx.Str x 1.0f 0.0f; x 
    let a' = d2MtoCuda2dArray a

    //printfn "%A" (getd2M a)

    let o = d2M.create((1,cols))
    let o' = d2MtoCudaArray o

    test_launcher(ctx.Str,cuda_kernel,a',o')
    cuda_context.Synchronize()

    let watch = Diagnostics.Stopwatch.StartNew()
    for i=1 to 10000 do
        test_launcher(ctx.Str,cuda_kernel,a',o')
        cuda_context.Synchronize()
    printfn "Time elapsed: %A" watch.Elapsed

    //printfn "%A" (o.GPV.Gather())

//mapredocolmap_test()

/// o <- op_col(x)
/// Applies a reduce along the inner dimension of a matrix.
type DeviceColumnReduceModule(map_load_op, reduce_op, neutral_elem, map_store_op, block_size) = 
    let kernel_name = "DeviceColumnReduceKernel"
    let kernel_code = 
        [|
        """
        #include "thrust/tuple.h"
        #include "cub/cub.cuh"

        //Kernel code:
        extern "C" {
            // Device code
            __global__ void """;kernel_name;"""(const int num_cols, const int num_rows, const float* A, float* O)
            {
                typedef cub::BlockReduce<float, """;string block_size;"""> BlockReduceT;
                __shared__ typename BlockReduceT::TempStorage temp_storage;

                const auto reduce_op = """;reduce_op;"""
                const auto neutral_elem = """;neutral_elem;"""
                const auto map_store_op = """;map_store_op;"""
                for (int col = blockIdx.x; col < num_cols; col += gridDim.x)
                {
                    auto value = neutral_elem;
                    for (int row = threadIdx.x; row < num_rows; row += blockDim.x)
                    {
                        const auto map_load_op = """;map_load_op;"""
                        auto tmp = map_load_op(A[col*num_rows+row]);
                        value = reduce_op(tmp,value);
                    }

                    auto result = BlockReduceT(temp_storage).Reduce(value,reduce_op);
                    if (threadIdx.x == 0) O[col] = map_store_op(result);
                }
            }
        }
        """
        |] |> String.concat ""

    member val Kernel = compile_kernel_using_nvcc_bat_router kernel_code kernel_name
    member val block_size = block_size // I will get an implementation error unless I do this.
    member inline t.A
            (str: CudaStream,
                (ext_x: ^a -> CUdeviceptr, x: ^a),
                (o: CudaDeviceVariable<_>)) = 
        let r,c = rc x
        if int o.Size <> c then failwithf "if int o.Size(%i) <> c(%i)" (int o.Size) c

        max_column_launcher (Some t.block_size) (str, t.Kernel, r, c, [|c; r; ext_x x; o.DevicePointer|])


let cub_test (map_load_op, map_load_op_host) (reduce_op,reduce_op_host) (neutral_elem,neutral_elem_host) (map_store_op,map_store_op_host) block_size =
    let m = DeviceColumnReduceModule(map_load_op, reduce_op, neutral_elem, map_store_op, block_size)

    let state = Context.create

    let cols, rows = 128, 128
    use a = d2M.create((rows,cols)) 
            |> fun x -> fillRandomUniformMatrix ctx.Str x 1.0f 0.0f; x 

    //printfn "%A" (getd2M a)

    use o = d2M.create((1,cols))
    m.A(ctx.Str,P a,o.GPV) // Warm up the JIT
    cuda_context.Synchronize()

    for i=1 to 100000 do
        m.A(ctx.Str,P a,o.GPV)
        cuda_context.Synchronize()

    let watch = Diagnostics.Stopwatch.StartNew()
    for i=1 to 100000 do
        m.A(ctx.Str,P a,o.GPV)
        cuda_context.Synchronize()
    printfn "Time elapsed: %A" watch.Elapsed

//    let inline col_reduce map_load_op reduce_op neutral_elem map_store_op (x: 'a[,]) = // Amazingly I got this one right on the first try. Proof that I am able to juggle these dimensions properly now.
//        Array.init (Array2D.length2 x) <| fun col -> 
//            Seq.fold (fun s row -> 
//                reduce_op s (map_load_op col row x.[row,col])) neutral_elem {0..Array2D.length1 x - 1}
//            |> map_store_op
//
//    let inline compare_with_host_reduce (device_reduce: 'a[]) (host_reduce: 'a[]) =
//        printfn "%A" device_reduce
//        printfn "%A" host_reduce
//        let diff = Array.map2 (fun a b -> abs(a-b)) host_reduce device_reduce |> Array.max
//        printfn "%A" diff
//
//    let a' = a.ToFloat32Array2D
//    let device_reduce = o.GPV.Gather()
//    let host_reduce: float32[] = col_reduce map_load_op_host reduce_op_host neutral_elem_host map_store_op_host a'
//    compare_with_host_reduce device_reduce host_reduce

let _ =
    let id = "[](auto x){return x;};"
    let map_load_op, map_load_op_host = id, fun _ _ x -> x
    let reduce_op, reduce_op_host = "[](auto x, auto y){return x+y;};", (+)
    let neutral_elem, neutral_elem_host = "__int_as_float(0x00000000);", 0.0f
    let map_store_op, map_store_op_host = id, fun x -> x
    let block_size = 128

    cub_test (map_load_op, map_load_op_host) (reduce_op,reduce_op_host) (neutral_elem,neutral_elem_host) (map_store_op,map_store_op_host) block_size
                
