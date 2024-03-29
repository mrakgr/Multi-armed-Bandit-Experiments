﻿// There is a bug in the tuple codegen and I am leaning towards this being a bug in the NVRTC compiler right now.

// Looking at it broadly, I can try compiling the same with NVCC and I can try fidling with the PTX by hand just to see if
// everything is in order.

// Edit: Solved it: http://stackoverflow.com/questions/40423722/how-to-properly-pass-arguments-as-structs-to-nvrtc/40427350#40427350

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

let ptx_code =
    """
//
// Generated by NVIDIA NVVM Compiler
//
// Compiler Build ID: CL-21112126
// Cuda compilation tools, release 8.0, V8.0.43
// Based on LLVM 3.4svn
//

.version 5.0
.target sm_30
.address_size 64

	// .globl	kernel_main
.extern .func  (.param .b32 func_retval0) vprintf
(
	.param .b64 vprintf_param_0,
	.param .b64 vprintf_param_1
)
;
.global .align 1 .b8 $str[28] = {40, 111, 110, 32, 100, 101, 118, 105, 99, 101, 41, 32, 120, 46, 112, 111, 105, 110, 116, 101, 114, 61, 37, 108, 108, 100, 10, 0};

.visible .entry kernel_main(
	.param .align 1 .b8 kernel_main_param_0[12]
)
{
	.local .align 8 .b8 	__local_depot0[8];
	.reg .b64 	%SP;
	.reg .b64 	%SPL;
	.reg .b32 	%r<2>;
	.reg .b64 	%rd<28>;


	mov.u64 	%rd27, __local_depot0;
	cvta.local.u64 	%SP, %rd27;
	ld.param.u64 	%rd22, [kernel_main_param_0+4];
	add.u64 	%rd23, %SP, 0;
	cvta.to.local.u64 	%rd24, %rd23;
	st.local.u64 	[%rd24], %rd22;
	mov.u64 	%rd25, $str;
	cvta.global.u64 	%rd26, %rd25;
	// Callseq Start 0
	{
	.reg .b32 temp_param_reg;
	// <end>}
	.param .b64 param0;
	st.param.b64	[param0+0], %rd26;
	.param .b64 param1;
	st.param.b64	[param1+0], %rd23;
	.param .b32 retval0;
	call.uni (retval0), 
	vprintf, 
	(
	param0, 
	param1
	);
	ld.param.b32	%r1, [retval0+0];
	
	//{
	}// Callseq End 0
	ret;
}
    """.ToCharArray() |> Array.map byte
    
let cuda_kernel = cuda_context.LoadKernelPTX(ptx_code,"kernel_main")

let test_launcher(str: CudaStream, kernel: CudaKernel, x: CudaGlobalArray<float32>, o: CudaGlobalArray<float32>) =
    let block_size = 1 

    kernel.GridDimensions <- dim3(1)
    kernel.BlockDimensions <- dim3(block_size)
    printfn "(on host) x.length=%i"  x.length // prints: (on host) x.length=10
    printfn "(on host) x.pointer=%i" x.pointer // prints: (on host) x.pointer=21535919104
    let args: obj [] = [|CudaLocalArray(x.length,x.pointer)|]
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