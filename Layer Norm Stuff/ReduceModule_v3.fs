module MapReduceColumnModule

//type CudaArguments =
//| CudaInput of typ: string * is_pointer
//
///// o <- op_col(x)
///// Applies a reduce along the inner dimension of a matrix.
//type DeviceMapColumnReduceMapModule(map_load_op, reduce_op, neutral_elem, map_store_op, block_size, kernel_name, inputs, outputs) = 
//    //let kernel_name = "DeviceColumnReduceKernel"
//    let kernel_code = 
//        [|
//        """
//#include "thrust/tuple.h"
//#include "thrust/pair.h"
//#include "cub/cub.cuh"
//
//#define NEGATIVE_FLOAT_INF __int_as_float(0xff800000)
//
////Kernel code:
//extern "C" {
//    // Device code
//    __global__ void """;kernel_name;"""(const int num_cols, const int num_rows, const float* A, """;output_type;"""* O)
//    {
//        typedef cub::BlockReduce<float, """;string block_size;"""> BlockReduceT;
//        __shared__ typename BlockReduceT::TempStorage temp_storage;
//
//        const auto reduce_op = """;reduce_op;"""
//        const auto neutral_elem = """;neutral_elem;"""
//        const auto map_store_op = """;map_store_op;"""
//        for (int col = blockIdx.x; col < num_cols; col += gridDim.x)
//        {
//            auto value = neutral_elem;
//            for (int row = threadIdx.x; row < num_rows; row += blockDim.x)
//            {
//                const auto map_load_op = """;map_load_op;"""
//                auto tmp = map_load_op(A[col*num_rows+row]);
//                value = reduce_op(tmp,value);
//            }
//
//            auto result = BlockReduceT(temp_storage).Reduce(value,reduce_op);
//            if (threadIdx.x == 0) O[col] = map_store_op(result);
//        }
//    }
//}
//        """
//        |] |> String.concat ""
//
//    member val Kernel = compile_kernel_using_nvcc_bat_router kernel_code kernel_name
//    member val block_size = block_size // I will get an implementation error unless I do this.
//    member inline t.A
//            (str: CudaStream,
//                (ext_x: ^a -> CUdeviceptr, x: ^a),
//                (o: CudaDeviceVariable<_>)) = 
//        let r,c = rc x
//        if int o.Size <> c then failwithf "if int o.Size(%i) <> c(%i)" (int o.Size) c
//
//        max_column_launcher (Some t.block_size) (str, t.Kernel, r, c, [|c; r; ext_x x; o.DevicePointer|])
//
//let DeviceMaxColumnActivationModule = 
//    let map_load_op = """[row](float x){return thrust::make_pair(x,row);};"""
//    let reduce_op = """[](auto x, auto y){return thrust::maximum(x,y);};"""
//    let neutral_elem = """thrust::make_pair(NEGATIVE_FLOAT_INF,-1);"""
//    let map_store_op = """[](auto x){return x.second;};"""
//    let kernel_name = "DeviceMaxColumnActivationModule"
//    let output_type = "int"
//    DeviceMapColumnReduceMapModule(map_load_op,reduce_op,neutral_elem,map_store_op,128,kernel_name,output_type)