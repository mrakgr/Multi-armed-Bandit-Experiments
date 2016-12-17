#include "thrust/tuple.h"
#include "cub/cub.cuh"
extern "C" {
    __global__ void HadMultBackward1(const int n, const float *o_primal_1, const float *o_adjoint_1, const float *a_primal_1, const float *b_primal_1, float *a_adjoint_1, float *b_adjoint_1) {
        for (int i = blockIdx.x*blockDim.x + threadIdx.x; (i < n); i += gridDim.x*blockDim.x) {
            const auto err = o_adjoint_1[i];
            if ((a_adjoint_1 != NULL)){
                a_adjoint_1[i] += (err * b_primal_1[i]);
            }
            if ((b_adjoint_1 != NULL)){
                b_adjoint_1[i] += (err * a_primal_1[i]);
            }
        }
    }
}
