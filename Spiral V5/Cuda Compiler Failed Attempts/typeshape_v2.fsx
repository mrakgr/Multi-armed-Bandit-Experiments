type CudaIntAr1D = CudaIntAr1D
type CudaFloatAr1D = CudaFloatAr1D
type CudaInt = CudaInt of string
type CudaFloat = CudaFloat of string

let mkCudaLambdaArgs<'ins, 'consts,'outs>(f: 'ins -> 'consts -> 'outs) : 'ins * 'consts * 'outs =
    Unchecked.defaultof<'ins>,Unchecked.defaultof<'consts>,Unchecked.defaultof<'outs>

let f (ins: CudaInt * (CudaFloat -> CudaInt)) (const_: CudaFloat) = fst ins

let ins,consts,outs = mkCudaLambdaArgs f