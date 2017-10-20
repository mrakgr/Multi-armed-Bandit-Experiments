// __host__ __device__ long long int method_0(long long int var_0) {
    // if ((var_0 > 1)) {
        // long long int var_1 = (var_0 - 1);
        // long long int var_2 = method_0(var_1);
        // long long int var_3 = (var_0 + 2);
        // long long int var_4 = method_0(var_3);
        // return (var_2 + var_4);
    // } else {
        // return 1;
    // }
// }

let (var_0: int64) = 5L
// Cuda method call
method_0((var_0: int64))
