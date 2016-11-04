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

//let prog = codegen (parse_exprs simple_copy_kernel)
let prog =
    """//Kernel code:
extern "C" {
    #pragma pack(1)
    typedef struct {
        int length;
        float *pointer;
    } global_array_float;
    __global__ void kernel_main(global_array_float x){
        printf("(on device) x.length=%d\n",x.length); // prints: (on device) x.length=10
        printf("(on device) x.pointer=%lld\n",x.pointer); // prints: (on device) x.pointer=0
        printf("sizeof(global_array_float)=%d", sizeof(global_array_float)); // 12 bytes just as expected
    }
;}"""

printfn "%s" prog
let cuda_kernel = compile_kernel prog "kernel_main"

let test_launcher(str: CudaStream, kernel: CudaKernel, x: CudaGlobalArray<float32>, o: CudaGlobalArray<float32>) =
    let block_size = 1 

    kernel.GridDimensions <- dim3(1)
    kernel.BlockDimensions <- dim3(block_size)
    printfn "(on host) x.length=%i"  x.length // prints: (on host) x.length=10
    printfn "(on host) x.pointer=%i" x.pointer // prints: (on host) x.pointer=21535919104
    let args: obj [] = [|x.length;x.pointer|]
    kernel.RunAsync(str.Stream, args)

let cols, rows = 10, 1
let a = d2M.create((rows,cols)) 
        |> fun x -> fillRandomUniformMatrix ctx.Str x 1.0f 0.0f; x 
let a' = d2MtoCudaArray a

//printfn "%A" (getd2M a)

let o = d2M.create((rows,cols)) // o does nothing here as this is a minimalist example.
let o' = d2MtoCudaArray o

test_launcher(ctx.Str,cuda_kernel,a',o')
cuda_context.Synchronize()

//printfn "%A" (getd2M o)