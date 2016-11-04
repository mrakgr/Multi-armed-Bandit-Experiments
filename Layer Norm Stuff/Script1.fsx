#load "CudaCodegen.fsx"
open SpiralV4.Flame

let a =
    <@
    let main(a: CudaGlobalArray<float32>, o: CudaGlobalArray<float32>) =
        a.[0] <- o.[0]
    ()
    @>
