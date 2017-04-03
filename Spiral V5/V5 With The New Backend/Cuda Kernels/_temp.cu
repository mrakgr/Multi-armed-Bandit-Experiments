#include "cub/cub.cuh" 
extern "C" {
    struct tuple_169 {
        int tup0;
        float tup1;
    };
    __device__ int method_135(int var_122, int * var_133) {
        int var_137 = var_133[0];
        return (var_137 < var_122);
    }
    __device__ int method_148(int var_123, tuple_169 * var_144) {
        int var_150 = var_144[0].tup0;
        float var_151 = var_144[0].tup1;
        return (var_150 < var_123);
    }
    __device__ float method_128(float var_154, float var_155) {
        return (var_154 + var_155);
    }
    __device__ void method_163(float * var_125, int var_139, float var_161) {
        if ((threadIdx.x == 0)) {
            float var_165 = atomicAdd(&(var_125[var_139]),var_161);
            return ;
        }
        else {
            return ;
        }
    }
    __global__ void method_126(int var_122, int var_123, float * var_124, float * var_125) {
        int var_133[1];
        int var_134 = blockIdx.x;
        var_133[0] = var_134;
        while (method_135(var_122, var_133)) {
            int var_139 = var_133[0];
            int var_143 = threadIdx.x;
            tuple_169 var_144[1];
            int var_145 = threadIdx.x;
            int var_146 = threadIdx.x;
            float var_147 = var_124[(var_139) * var_123 + (var_145)];
            var_144[0] = { .tup0 = var_146, .tup1 = var_147 };
            while (method_148(var_123, var_144)) {
                int var_153 = var_144[0].tup0;
                float var_154 = var_144[0].tup1;
                float var_155 = var_124[(var_139) * var_123 + (var_153)];
                int var_156 = (var_153 + blockDim.x);
                float var_157 = method_128(var_154, var_155);
                var_144[0] = { .tup0 = var_156, .tup1 = var_157 };
            }
            int var_158 = var_144[0].tup0;
            float var_159 = var_144[0].tup1;
            float var_161 = cub::BlockReduce<float,128>().Reduce(var_159,method_128);
            method_163(var_125, var_139, var_161);
            int var_167 = (var_139 + gridDim.x);
            var_133[0] = var_167;
        }
        int var_168 = var_133[0];
    }
}
