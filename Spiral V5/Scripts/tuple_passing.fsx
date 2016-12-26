let vale (x,_,_) = x
let adde (_,x,_) = x
let multe (_,_,x) = x

let val_ x d =
    let f = vale d
    f x

let add x y d =
    let f = adde d
    f (x d) (y d)

let mult x y d =
    let f = multe d
    f (x d) (y d)

let in_1 =
    let val_ (x: int) = x
    let add x y = x+y
    let mult x y = x*y
    val_,add,mult

let in_2 =
    let val_ (x: int) = string x
    let add x y = sprintf "(%s + %s)" x y
    let mult x y = sprintf "(%s * %s)" x y
    val_,add,mult

let r2 d = add (val_ 1) (val_ 3) d

//let test x = x in_1, x in_2 // Type error.

let a2 = r2 in_1
let b2 = r2 in_2