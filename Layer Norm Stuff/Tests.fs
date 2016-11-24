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

    let watch = Diagnostics.Stopwatch.StartNew()
    for i=1 to 10000 do
        test_launcher(ctx.Str,cuda_kernel,a',o')
        cuda_context.Synchronize()
    printfn "Time elapsed: %A" watch.Elapsed

    //printfn "%A" (o.GPV.Gather())

//mapredocolmap_test()

let code = 
    """
    #include "thrust/tuple.h"

    extern "C" {
	    __global__ void CPP11Test(int *c, const int *a, const int *b)
	    {
		    int i = threadIdx.x;
		    auto lamb = [](int x) {return x + 1; }; // Works.
		    auto t = thrust::make_tuple(1, 2, 3);
		    c[i] = a[i] + b[i];
	    }
    }
    """

let k = compile_kernel_using_nvcc_bat_router code "CPP11Test"


