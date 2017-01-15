#include "thrust/tuple.h"
#include "thrust/functional.h"
#include "cub/cub.cuh"
extern "C" {
    __global__ void GetAccuracy(const int arg_1, const float * arg_2, float * arg_3, int * arg_4) {
        typedef cub::BlockReduce<thrust::tuple<int,float>, 256> typedef_5;
        __shared__ typedef_5::TempStorage var_6;
        auto class_7 = typedef_5(var_6);
        const auto lambda_8 = [](const auto lambda_arg_9, const auto lambda_arg_10) {
            return thrust::maximum(lambda_arg_9, lambda_arg_10);
        };
        int var_11 = blockIdx.x*blockDim.x + threadIdx.x;
        auto var_12 = thrust::make_tuple<float, int>(arg_2[var_11], var_11);
        const auto var_13 = gridDim.x*blockDim.x;
        var_11 += var_13;
        while ((var_11 < arg_1)) {
            var_12 = lambda_8(var_12, thrust::make_tuple<float, int>(arg_2[var_11], var_11));
            var_11 += var_13;
        }
        const auto var_14 = class_7.Reduce(var_12, lambda_8);
        if ((threadIdx.x == 0)) {
            arg_3[0] = thrust::get<0>(var_14);
            arg_4[0] = thrust::get<1>(var_14);
        } else {
        }
    }
}
