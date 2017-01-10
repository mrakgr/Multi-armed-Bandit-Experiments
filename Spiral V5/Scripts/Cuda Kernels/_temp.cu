#include "thrust/tuple.h"
#include "cub/cub.cuh"
extern "C" {
    __global__ void ReluBackward(const int arg_20, const float * arg_21_primal, const float * arg_22_primal, const float * arg_23_adjoint, float * arg_24_adjoint) {
        for(auto var_25 = blockIdx.x * blockDim.x + threadIdx.x; (var_25 < arg_20); var_25 += gridDim.x * blockDim.x) {
            auto var_26 = arg_21_primal[var_25];
            auto var_27 = arg_22_primal[var_25];
            auto var_28 = arg_23_adjoint[var_25];
            arg_24_adjoint[var_25] += (var_28 * (((var_26 > 0)) ? (1) : (0)));
        }
    }
}
