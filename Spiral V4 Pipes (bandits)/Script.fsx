type A(x : int) = 
    member val x = x with get,set

let a = A(2)
let b = A(2)

type C = B of A

let c = B a
let c' = B a
c = c'