let a = [|1.0f;2.0f|]

Array.fold (fun (s,iter) x -> x/iter+s,iter+1.0f) (0.0f,1.0f) a