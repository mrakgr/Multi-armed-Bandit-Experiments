type Tuple0 =
    struct
    val mem_0: (int64 [])
    val mem_1: (float [])
    new(arg_mem_0, arg_mem_1) = {mem_0 = arg_mem_0; mem_1 = arg_mem_1}
    end
let (var_0: (int64 [])) = Array.zeroCreate<int64> (System.Convert.ToInt32(0L))
let (var_1: (float [])) = Array.zeroCreate<float> (System.Convert.ToInt32(1L))
var_1.[int32 0L] <- 2.200000
Tuple0(var_0, var_1)
