open System.Collections.Generic

let (>>=) a b ret =
    a <| fun a ->
        b a ret

let add a b ret =
    ret (a+b)

let succ a ret = ret a

let q = 
    add 1 2 >>= fun r ->
    add 3 4 >>= fun r' ->
    succ (r+r')

q id