/// Generic function for training and inference for 1D RNNs.
let inline private runRNN 
        (network: ^target -> ^input -> ^state -> LayerWrapper< (Lazy<int> * Df),^state>) 
        (optimizer: Optimizer)
        (set : (^input * ^target)[][]) 
        (state: ^state) 
        test_accuracy =
    let mutable accumulated_cost = 0.0f
    let mutable accuracy = 0
    let mutable max_accuracy = 0

    let ar = ResizeArray()
    Array.fold 
        (mem state).Reset()
        ar.Clear()
        Array.fold <| fun (accuracy,max_accuracy,accumulated_cost,_) (input,target) ->
            setTimestep state iter
            let fin = network target input state
            let (hits,r),_ = fin.RunLayer

            if test_accuracy then 
                accuracy <- accuracy + hits.Value
                max_accuracy <- max_accuracy + (size target)

            accumulated_cost <- accumulated_cost + r.P.Value.Value

            ar.Add(r)
            if iter = sequence.Length-1 then 
                let r = sum_scalars ar state |> fst

                if is_inference_only state = true && (tape state).Count > 0 then
                    failwith "Forgot to use the is_inference_only flag in a library function somewhere"
                else
                    r.A := 1.0f
                    let tape = tape state
                    while tape.Count > 0 do
                        tape.Pop()
                        |> fun (name,func) -> func()
                    update (optimizer, state) fin
                //(accuracy, max_accuracy), accumulated_cost / float32 set.Length, fin.WrappedNodes
    <| (0,0,0.0f,None) <| set
