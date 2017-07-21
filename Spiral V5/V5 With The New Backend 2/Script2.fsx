// Incorrect let generalization bug
let tag = 
    let mutable i = 0
    fun _ -> i <- i+1; i

let a1 = tag() // 1
let a2 = tag() // 1
let a3 = tag() // 1

let tag2 =
    let mutable i = 0
    fun () -> i <- i+1; i

let b1 = tag2() // 1
let b2 = tag2() // 2
let b3 = tag2() // 3

let tag3 =
    let i = ref 0
    fun _ -> i := !i+1; !i

let c1 = tag3() // 1
let c2 = tag3() // 2
let c3 = tag3() // 3