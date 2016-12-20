#include "thrust/tuple.h"
#include "cub/cub.cuh"
extern "C" {
    __global__ void Sum(const int n, const float *x1, float *o1) {
        typedef cub::BlockReduce<float, 256> BlockReduceT;
        __shared__ BlockReduceT::TempStorage temp_storage;
        const auto reduce_op = [](auto x1, auto x2){
            return (x1 + x2);
        };
        int i = blockIdx.x*blockDim.x + threadIdx.x;
        auto value = x1[i];
        const auto stride = gridDim.x*blockDim.x;
        i += stride;
        while ((i < n)){
            value = reduce_op(value, x1[i]);
            i += stride;
        }
        const auto result = BlockReduceT(temp_storage).Reduce(value, reduce_op);
        if ((threadIdx.x == 0)){
            atomicAdd((&o1[0]), result);
        }
    }
}
