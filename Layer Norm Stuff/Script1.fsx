let inline f x =
    printfn "Hello"
    fun y z -> x + y + z

let a = f 2
let b = f 4.5