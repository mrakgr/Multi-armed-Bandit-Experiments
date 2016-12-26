// Nope I thought the duplication idea might work, but I was wrong.

let vale (x,_,_) = x
let adde (_,x,_) = x
let multe (_,_,x) = x

let apply1 x1 x2 s1 s2 =
    (fun d -> let f = s1 d in f x1),
    (fun d -> let f = s2 d in f x2)

let apply2 x1 x2 y1 y2 s1 s2 =
    (fun d -> let f = s1 d in f x1 y2),
    (fun d -> let f = s2 d in f x1 y2)

let val_ (x: int) = apply1 x x vale vale
let add x y = apply2 x y adde adde
let mult x y = apply2 x y multe multe

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

let r2 (d1,d2) = 
    let int_,str_ = add (val_ 1) (val_ 3)
    int_ d1, str_ d2

//let test x = x (in_1, in_2) fst, x (in_1, in_2) snd // Type error.

let a2 = r2 (in_1,in_2)
