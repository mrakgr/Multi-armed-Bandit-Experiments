type CudaModuleArgument =
| InputArg of c_type1: string * c_type2: string * name: string
| OutputArg of c_type1: string * c_type2: string * name: string

type CudaLambdaExpr =
| Var of x: string
| Add of x: CudaLambdaExpr * y: CudaLambdaExpr
| Mult of x: CudaLambdaExpr * y: CudaLambdaExpr
| Max of x: CudaLambdaExpr * y: CudaLambdaExpr
| Min of x: CudaLambdaExpr * y: CudaLambdaExpr
| First of x: CudaLambdaExpr // For tuples.
| Second of y: CudaLambdaExpr
| Item of x: CudaLambdaExpr * index: int
| MakeTuple of x: CudaLambdaExpr list
| Return of x: CudaLambdaExpr

type CudaLambdaArgument = // They do not need types as I can just set them to auto.
| Argument of x: string

type CudaLambda =
| Lambda of arguments: CudaLambdaArgument list * expr: CudaLambdaExpr

type CudaModule =
| MapModule of name: string * arguments: CudaModuleArgument list * map_lambda: CudaLambda

let float_p x = "float", "*", x
let float_v x = "float", "", x
let int_p x = "int", "*", x
let int_v x = "int", "", x
let args (inputs: (string * string * string) list) (outputs: (string * string * string) list): CudaModuleArgument list =
    [List.map InputArg inputs; List.map InputArg outputs] |> List.concat
    
let inline map_launcher(str: CudaStream, kernel: CudaKernel, total_size: int, [<ParamArray>] args: obj[]) =
    let block_size = 256
    let gridSize = min (2*numSm*(1024/block_size)) (divup total_size block_size)
    kernel.GridDimensions <- dim3(gridSize)
    kernel.BlockDimensions <- dim3(block_size)
    kernel.RunAsync(str.Stream, args)

let make_map_module (name: string) (arguments: CudaModuleArgument list) (map_lambda: CudaLambda) =
    let eval_map_arguments (arguments: CudaModuleArgument list) = 
        match arguments with
        | h :: t -> 
            match h with
            | InputArg(tpy,pointer,name) -> sprintf

    let eval_map_lambda map_lambda = ""

    let kernel_name = "MapKernel"+name
    let kernel_code = 
        [|"""
#include "thrust/tuple.h"
#include "cub/cub.cuh"

#define NEGATIVE_FLOAT_INF __int_as_float(0xff800000)

//Kernel code:
extern \"C\" {
    // Device code
    __global__ void """;kernel_name;"""(""";eval_map_arguments arguments;""")
    {
        const auto = """;eval_map_lambda map_lambda;"""
        int i = blockDim.x * blockIdx.x + threadIdx.x;
        const int stride = blockDim.x * gridDim.x;
        while (i < N)
        {
            O[i] = op(A[i]);
            i += stride;
        }
    }
}
        """|] |> String.concat ""

    kernel_code

let m = make_map_module "Id" (args [float_p "x"] [float_p "o"]) (Lambda([Argument "x"],Return(Var "x")))

printfn "%s" m