type A =
    {
    mutable x: int list
    v : string
    }

let a = {x=[1;2;3]; v="Hello."}
let b = {a with v="Bye."}
a.x <- [3;4;5]
a
b