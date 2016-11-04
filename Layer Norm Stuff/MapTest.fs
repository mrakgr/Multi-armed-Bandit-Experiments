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

type Ar = CudaGlobalArray<float32>

let simple_copy_kernel =
    <@
    let kernel_main (x: Ar, o: Ar) =
        let mutable i = BlockIdx.x*BlockDim.x + ThreadIdx.x
        while i < x.length do
            o.[i] <- x.[i]*2.0f
            i <- i + BlockDim.x
    ()
    @>

let prog = codegen (parse_exprs simple_copy_kernel)
//let prog =
//    """
//extern "C" {
//    typedef struct {
//        int length;
//        float *pointer;
//    } global_array_float;
//    __global__ void kernel_main(global_array_float x){
//        int i = ((blockIdx.x * blockDim.x) + threadIdx.x);
//        if (threadIdx.x == 0) printf("x=%lld",x.pointer);
//}
//;}
//"""

printfn "%s" prog
let cuda_kernel = compile_kernel prog "kernel_main"

let test_launcher(str: CudaStream, kernel: CudaKernel, a: CudaGlobalArray<float32>, o: CudaGlobalArray<float32>) =
    let block_size = 128 

    kernel.GridDimensions <- dim3(1)
    kernel.BlockDimensions <- dim3(block_size)
    let args: obj [] = [|a.length;a.pointer;o.length;o.pointer|]
    kernel.RunAsync(str.Stream, args)

let cols, rows = 10, 1
let a = d2M.create((rows,cols)) 
        |> fun x -> fillRandomUniformMatrix ctx.Str x 1.0f 0.0f; x 
let a' = d2MtoCudaArray a

printfn "%A" (getd2M a)

let o = d2M.create((cols,rows)) 
let o' = d2MtoCudaArray o

test_launcher(ctx.Str,cuda_kernel,a',o')
cuda_context.Synchronize()

printfn "%A" (getd2M a)