type CudaInt = CudaInt
type CudaFloat = CudaFloat

let mkCudaType<'T>() : 'T = // It is possible to do it like this in order to remove the dependency on TypeShape.
    if typeof<'T> = typeof<CudaInt> then unbox<'T> CudaInt
    elif typeof<'T> = typeof<CudaFloat> then unbox<'T> CudaFloat
    //elif // That having said, I'd rather not write all the matchers for the tuples.
    else failwith "Unsupported type."

let x: CudaFloat = mkCudaType()

let y: CudaInt = Unchecked.defaultof<_> // Returns the correct type, but as a null.
