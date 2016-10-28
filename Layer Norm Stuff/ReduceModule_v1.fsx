// V1 Note: Screw LLVM. I just need to get this done. Someway or another as it always does,
// the mental block I was having with turning F# quotations to C resolved itself and I think
// I can proceed forward.

open System
open SpiralV4
open ManagedCuda
open ManagedCuda.BasicTypes

type ReduceOperation =
    | MaxReduceOp
    | MinReduceOp
    | AddReduceOp
    | MulReduceOp
    | MaxIndexReduceOp
    | MinIndexReduceOp

    member t.ValueType = 
        match t with
        | MaxReduceOp | MinReduceOp | AddReduceOp | MulReduceOp -> "typedef float valueType;"
        | MaxIndexReduceOp | MinIndexReduceOp -> "typedef struct {float value; int index;} valueType;"

    member t.Init name =
        match t with
        | MaxReduceOp -> sprintf "%s = __int_as_float(0xff800000);" name // Negative infinity in float32
        | MinReduceOp -> sprintf "%s = __int_as_float(0x7f800000);" name // Positive infinity in float32
        | AddReduceOp -> sprintf "%s = __int_as_float(0x00000000);" name // Zero in float32
        | MulReduceOp -> sprintf "%s = __int_as_float(0x3f800000);" name // One in float32
        | MaxIndexReduceOp -> sprintf "%s.value = __int_as_float(0xff800000);" name // Negative infinity + index needs not be initialized
        | MinIndexReduceOp -> sprintf "%s.value = __int_as_float(0x7f800000);" name // Positive infinity + index needs not be initialized

    member t.ReduceOp =
        match t with
        | MaxReduceOp -> "a > b ? a : b;"
        | MinReduceOp -> "a < b ? a : b;"
        | AddReduceOp -> "a + b;"
        | MulReduceOp -> "a * b;"
        | MaxIndexReduceOp -> "a.value > b.value ? a : b;"
        | MinIndexReduceOp -> "a.value < b.value ? a : b;"

    member t.Shuffle a b =
        match t with
        | MaxReduceOp | MinReduceOp | AddReduceOp | MulReduceOp -> 
            sprintf "valueType %s = __shfl_xor(%s, i);" a b
        | MaxIndexReduceOp | MinIndexReduceOp -> 
            [|
            sprintf "valueType %s;" a
            sprintf "%s.value = __shfl_xor(%s.value, i);" a b
            sprintf "%s.index = __shfl_xor(%s.index, i);" a b
            |] |> String.concat " "

    member t.MapToValueType name var row =
        match t with
        | MaxReduceOp | MinReduceOp | AddReduceOp | MulReduceOp -> 
            sprintf "valueType %s = %s;" name var
        | MaxIndexReduceOp | MinIndexReduceOp -> 
            sprintf "valueType %s; %s.value = %s; %s.index = %s;" name name var name row

type FinalOperation =
    | FirstFinalOp
    | SecondFinalOp
    | SquareFinalOp
    | MeanFinalOp
    | NoFinalOp

    member t.FinalType reduce_op =
        match reduce_op with
        | MaxReduceOp | MinReduceOp | AddReduceOp | MulReduceOp -> 
            match t with
            | FirstFinalOp | SecondFinalOp -> failwith "Invalid operation"
            | MeanFinalOp | SquareFinalOp | NoFinalOp -> "typedef floatType finalType;"
        | MaxIndexReduceOp | MinIndexReduceOp -> 
            match t with
            | FirstFinalOp -> "typedef floatType finalType;"
            | SecondFinalOp -> "typedef int finalType;"
            | MeanFinalOp | SquareFinalOp | NoFinalOp -> "typedef valueType finalType;"

    member t.FinalOp reduce_op result final_result num_rows = 
        match reduce_op with
        | MaxReduceOp | MinReduceOp | AddReduceOp | MulReduceOp -> 
            match t with
            | FirstFinalOp | SecondFinalOp -> failwith "Invalid operation"
            | MeanFinalOp -> sprintf "%s = %s/%s;" final_result result num_rows
            | SquareFinalOp -> sprintf "%s = %s*%s;" final_result result result
            | NoFinalOp -> sprintf "%s = %s;" final_result result
        | MaxIndexReduceOp | MinIndexReduceOp -> 
            match t with
            | FirstFinalOp -> sprintf "%s = %s.value;" final_result result
            | SecondFinalOp -> sprintf "%s = %s.index;" final_result result
            | SquareFinalOp -> sprintf "%s.value = %s.value * %s.value; %s.index = %s.index;" final_result result result final_result result
            | MeanFinalOp -> sprintf "%s.value = %s.value / %s; %s.index = %s.index;" final_result result num_rows final_result result
            | NoFinalOp -> sprintf "%s = %s;" final_result result

/// o <- op_col(x)
/// Applies a reduce along the inner dimension of a matrix.
type DeviceColumnReduceModule(reduce_op: ReduceOperation, final_op: FinalOperation) = 
    let kernel_name = "DeviceColumnReduceKernel"
    let kernel_code = 
        [|"
        //Kernel code:
        extern \"C\" {
            typedef float floatType;
            ";reduce_op.ValueType;"
            ";final_op.FinalType reduce_op;"

            __device__ inline valueType reduce_op(valueType a, valueType b)
            {
                return ";reduce_op.ReduceOp;"
            }

            __device__ inline valueType warpReduce(valueType b){
                #pragma unroll
	            for (int i=1; i<32; i*=2) {
                    ";reduce_op.Shuffle "a" "b";"
                    b = reduce_op(a,b);
                    }
	            return b;
            }
              
            __device__ inline valueType blockReduce(valueType value){
	            __shared__ valueType temp[32];
                if (threadIdx.x < 32) 
                    {
                    // Just in case there are less than 32 warps init all to the neutral element.
                    ";reduce_op.Init "temp[threadIdx.x]";" 
                    }
                valueType out_partial = warpReduce(value);
                __syncthreads();
	            if (threadIdx.x % 32 == 0) temp[threadIdx.x / 32] = out_partial;
                __syncthreads();
	            return warpReduce(temp[threadIdx.x % 32]);
            }

            // Device code
            __global__ void ";kernel_name;"(const int num_cols, const int num_rows, const floatType* A, finalType* O)
            {
                for (int col = blockIdx.x; col < num_cols; col += gridDim.x)
                {
                    valueType value;
                    ";reduce_op.Init "value";" // Init of the reduce operation
                    for (int row = threadIdx.x; row < num_rows; row += blockDim.x)
                    {
                        ";reduce_op.MapToValueType "tmp" "A[col*num_rows+row]" "row";"
                        value = reduce_op(tmp,value);
                    }
                    
                    valueType result = blockReduce(value);
                    finalType final_result;
                    ";final_op.FinalOp reduce_op "result" "final_result" "num_rows";"
                    if (threadIdx.x == 0) O[col] = final_result;
                }
            }
        }

        "|] |> String.concat ""
    do printfn "%s" kernel_code

    member val Kernel = compile_kernel kernel_code kernel_name
    member inline t.A
            (str: CudaStream,
                (ext_x: ^a -> CUdeviceptr, x: ^a),
                (o: CudaDeviceVariable<_>)) = 
        let r,c = rc x
        if int o.Size <> c then failwithf "if int o.Size(%i) <> c(%i)" (int o.Size) c
        max_column_launcher(str, t.Kernel, r, c, [|c; r; ext_x x; o.DevicePointer|])

cuda_context.Synchronize()

let op = MaxIndexReduceOp
let final_op = MeanFinalOp

let k = DeviceColumnReduceModule(op,final_op)
let ctx = Context<_>.create

let cols, rows = 545, 432
let a = d2M.create((rows,cols)) |> fun x -> fillRandomUniformMatrix ctx.Str x 1.0f 0.0f; x

let getd2M (x: d2M) =
    let t = x.GPV.Gather()
    Array2D.init x.Rows x.Columns (fun row col -> t.[col*x.Rows + row])

type Float32Indx =
    struct
    val value : float32
    val index : int
    new (a1,a2) = {value=a1;index=a2}
    end

    override t.ToString() = sprintf "(%f,%i)" t.value t.index

    static member (-) (a: Float32Indx,b: Float32Indx) = a.value - b.value

let inline col_reduce map_op reduce_op neutral_elem (x: 'a[,]) = // Amazingly I got this one right on the first try. Proof that I am able to juggle these dimensions properly now.
    Array.init (Array2D.length2 x) (fun col -> Seq.fold (fun s row -> reduce_op s (map_op col row x.[row,col])) neutral_elem {0..Array2D.length1 x - 1})

let a' = getd2M a
printfn "%A" a'

let inline compare_with_host_reduce (o: CudaDeviceVariable<'a>) (host_reduce: 'a[]) =
    let o' = o.Gather()
    printfn "%A" o'
    printfn "%A" host_reduce
    let diff = Array.map2 (fun a b -> abs(a-b)) o' host_reduce |> Array.max
    printfn "%A" diff

let inline complete_col_reduce f o neutral_elem comp_func map_func =
    k.A(ctx.Str,P a,o)
    col_reduce 
        f
        comp_func 
        neutral_elem 
        a'
    |> Array.map map_func
    |> compare_with_host_reduce o

match op with
| MaxReduceOp | MinReduceOp | AddReduceOp | MulReduceOp -> 
    let only_x _ _ x = x
    use o = new CudaDeviceVariable<float32>(SizeT cols)
    let map_func = 
        match final_op with
        | FirstFinalOp | SecondFinalOp -> failwith "Invalid"
        | NoFinalOp -> fun x -> x
        | SquareFinalOp -> fun x -> x*x
        | MeanFinalOp -> fun x -> x / float32 a.Rows
    match op with
    | MaxIndexReduceOp | MinIndexReduceOp -> failwith "Invalid"
    | MaxReduceOp -> complete_col_reduce only_x o Single.NegativeInfinity max map_func
    | MinReduceOp -> complete_col_reduce only_x o Single.PositiveInfinity min map_func
    | AddReduceOp -> complete_col_reduce only_x o 0.0f (+) map_func
    | MulReduceOp -> complete_col_reduce only_x o 1.0f (*) map_func
| MaxIndexReduceOp | MinIndexReduceOp -> 
    let comp_func =
        match op with
        | MaxReduceOp | MinReduceOp | AddReduceOp | MulReduceOp -> failwith "Invalid"
        | MaxIndexReduceOp ->
            fun (a: Float32Indx) (b: Float32Indx) ->
                if a.value > b.value then a else b
        | MinIndexReduceOp ->
            fun (a: Float32Indx) (b: Float32Indx) ->
                if a.value < b.value then a else b

    let f col row x = Float32Indx(x,row)
    let neutral_elem = 
        match op with
        | MaxReduceOp | MinReduceOp | AddReduceOp | MulReduceOp -> failwith "Invalid"
        | MaxIndexReduceOp ->
            Float32Indx(Single.NegativeInfinity,0)
        | MinIndexReduceOp ->
            Float32Indx(Single.PositiveInfinity,0)
    match final_op with
    | NoFinalOp ->
        use o = new CudaDeviceVariable<Float32Indx>(SizeT cols)
        complete_col_reduce f o neutral_elem comp_func (fun x -> x)
    | SquareFinalOp ->
        use o = new CudaDeviceVariable<Float32Indx>(SizeT cols)
        complete_col_reduce f o neutral_elem comp_func (fun x -> Float32Indx(x.value*x.value,x.index))
    | FirstFinalOp ->
        use o = new CudaDeviceVariable<float32>(SizeT cols)
        complete_col_reduce f o neutral_elem comp_func (fun x -> x.value)
    | SecondFinalOp ->
        use o = new CudaDeviceVariable<int>(SizeT cols)
        complete_col_reduce f o neutral_elem comp_func (fun x -> x.index)
    | MeanFinalOp ->
        use o = new CudaDeviceVariable<Float32Indx>(SizeT cols)
        complete_col_reduce f o neutral_elem comp_func (fun x -> Float32Indx(x.value / float32 a.Rows, x.index))
