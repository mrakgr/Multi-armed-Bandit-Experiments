// For the past week I went on a little adventure of exploration in Haskell studying as much as I could
// on compilers and how to make code more generic. I figured out GADTs and the finally tagless style of
// writting highly generic interpreters.

// I feel that I gained a lot from that, and I even looked at the Accelerate library.

// To be honest, that GPU library is so feature packed that for a few days I flirted with the idea of
// doing a port of Spiral to Haskell just to try out my new skills and see how it goes, but I got stomped
// on a bug in the Accelerate library. It turns out that currently there is a dump code generation bug,
// due to a missing feature in cl (the Microsoft Visual C++ compiler) which nvcc (Nvidia's Cuda C++ compiler)
// uses under the hood.

// It is a bit frustrating as it could be fixed just by adding a bunch of brackets and return statements.

// So that brings me back here. Forget Haskell. I'll deal with making the library generic in due time.

// Instead I will just write these Cuda kernels by hand like a good worker ant that I am.

// In the past I had a lot of trouble visualizing multidimensional arrays, but as the
// group_data_by_seq_length_and_load_to_gpu shows, I do not really have that problem much anymore.

// So, I'll do layer norm for real this time. Forget maximizing genericity. First off, I'll start with
// the mean function which I currently lack, and to do that, I need the columnwise reduce.

open System
open SpiralV4
open ManagedCuda
open ManagedCuda.BasicTypes

type ReduceOperation =
| MaxReduce
| MinReduce
| AddReduce
| MulReduce

let private get_hex_init_of_reduce_op = function
    | MaxReduce -> "0xff800000" // Negative infinity in float32
    | MinReduce -> "0x7f800000" // Positive infinity in float32
    | AddReduce -> "0x00000000" // Zero in float32
    | MulReduce -> "0x3f800000" // One in float32

let private get_string_of_reduce_op = function
    | MaxReduce -> "a1 > a2 ? a1 : a2"
    | MinReduce -> "a1 < a2 ? a1 : a2"
    | AddReduce -> "a1 + a2"
    | MulReduce -> "a1 * a2"

/// o <- op_col(x)
/// Applies a reduce along the inner dimension of a matrix.
type DeviceColumnReduceModule(reduce_op: ReduceOperation) = 
    let init = get_hex_init_of_reduce_op reduce_op
    let reduce_op = get_string_of_reduce_op reduce_op
    let kernel_name = "DeviceColumnReduceKernel"
    let kernel_code = 
        [|"
        //Kernel code:
        extern \"C\" {
            typedef float floatType;
            #define INIT __int_as_float(";init;") // The constant init for the reduce operations. This is float negative infinity.

            __device__ inline floatType reduce_op(floatType a1, floatType v1)
            {
                return ";reduce_op;"
            }

            // The max reduce version.
            __device__ inline floatType warpReduce(floatType v1){
                #pragma unroll
	            for (int i=1; i<32; i*=2) {
                    floatType a1 = __shfl_xor(v1, i);
                    v1 = reduce_op(a1,v1);
                    }
	            return v1;
            }
              
            __device__ inline floatType blockReduce(floatType value){
	            __shared__ floatType temp[32];
                if (threadIdx.x < 32) temp[threadIdx.x] = INIT; // Just in case there are less than 32 warps.
                floatType out_partial = warpReduce(value);
                __syncthreads();
	            if (threadIdx.x % 32 == 0) temp[threadIdx.x / 32] = out_partial;
                __syncthreads();
	            if (threadIdx.x < 32) return warpReduce(temp[threadIdx.x]);
            }

            // Device code
            __global__ void ";kernel_name;"(const int num_cols, const int num_rows, const floatType* A, floatType* O)
            {
                int col = blockIdx.x;
                while(col < num_cols)
                {
                    int row = threadIdx.x;
                    floatType value = INIT; // Init of the reduce operation
                    while (row < num_rows)
                    {
                        value = reduce_op(A[col*num_rows+row],value);
                        row += blockDim.x;
                    }
                
                    O[col] = blockReduce(value);
                    col++;
                }
            }
        }

        "|] |> String.concat ""

    member val Kernel = load_kernel kernel_code kernel_name
    member inline t.A
            (str: CudaStream,
                (ext_x: ^a -> CUdeviceptr, x: ^a),
                (o: CudaDeviceVariable<_>)) = 
        let r,c = rc x
        max_column_launcher(str, t.Kernel, r, c, [|c; r; ext_x x; o.DevicePointer|])
