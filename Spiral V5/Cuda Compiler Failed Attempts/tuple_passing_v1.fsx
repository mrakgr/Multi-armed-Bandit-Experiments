// Another try. This time with a selector function. As expected, I get a type error.

// I pretty much wasted the entire day yesterday, but as I am so succeptible to type obsession, finally coming to grips with
// the limit of static languages might be worth it. Inlined functions, interfaces, discriminated unions, I've been dreaming up ways of
// using them, but I am an idiot. If I had bothered to start here and focused on just functions, I would have realized that if 
// standard F# functions cannot do it, then there no way those other contraptions can do it either.

// This is the limit of the language and by extension my limit as well.

// I can either accept it and realize that I do not really need more abstraction than provided by F#'s type system, or I can
// turn to dynamic languages.

// This is it. This is the hard line in the ground that I cannot cross.

// And the proof is right here in this file.

let vale (x,_,_) = x
let adde (_,x,_) = x
let multe (_,_,x) = x

let val_ x d s =
    let f = vale (s d)
    f x

let add x y d s =
    let f = adde (s d)
    f (x d s) (y d s)

let mult x y d s =
    let f = multe (s d)
    f (x d s) (y d s)

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

//let test x = x (in_1, in_2) fst, x (in_1, in_2) snd // Type error.

let a2 = r2 in_1 id
let b2 = r2 in_2 id

test r2