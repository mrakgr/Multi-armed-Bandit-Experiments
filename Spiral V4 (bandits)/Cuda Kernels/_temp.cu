
        #include "thrust/tuple.h"
        #include "cub/cub.cuh"

        //Kernel code:
        extern "C" {
            // Device code
            __global__ void DeviceColumnReduceKernel(const int num_cols, const int num_rows, const float* A, float* O)
            {
                typedef cub::BlockReduce<float, 128> BlockReduceT;
                __shared__ typename BlockReduceT::TempStorage temp_storage;

                const auto reduce_op = [](auto x, auto y){return x+y;};
                const auto neutral_elem = __int_as_float(0x00000000);
                const auto map_store_op = [](auto x){return x;};
                for (int col = blockIdx.x; col < num_cols; col += gridDim.x)
                {
                    auto value = neutral_elem;
                    for (int row = threadIdx.x; row < num_rows; row += blockDim.x)
                    {
                        const auto map_load_op = [](auto x){return x;};
                        auto tmp = map_load_op(A[col*num_rows+row]);
                        value = reduce_op(tmp,value);
                    }

                    auto result = BlockReduceT(temp_storage).Reduce(value,reduce_op);
                    if (threadIdx.x == 0) O[col] = map_store_op(result);
                }
            }
        }
        