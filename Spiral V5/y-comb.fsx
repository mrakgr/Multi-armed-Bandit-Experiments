// Y combinator. Since I am moving to more advanced functional stuff in my own programming, I am going to try to figure out
// this concept that I used and knew, but did not understand.

// Last night I tried thinking of how I would do recursion and I managed to get to `let rec y f = f (y f)` which is pretty close
// to the actual y combinator in F# `let rec y f x = f (y f) x`. And I did think this would diverge and it did. And it did occur
// to me to put a stopper in the form of an extra argument.

// Edit: Ok, I see that x is used to pass the arguments to f. Makes sense. This is is quite brilliant.
// Unlike before when I only got 20%, I think I get 80% now. Let me get to 100%. Just why can't I recurse it.

let f q () = 
    let t = 1 + q ()
    printfn "%i" t
    t

let rec y f x = f (y f) x

let q = y f
q ()

// Edit2: I am 90% and I've only been at this for like 20m + plus all the thinking I did last night. The sure proof of that is, 
// is that I will be able to reproduce the Y Combinator

// To understand the combinator is easy. Suppose you have an expression like (1 + x). Obviously to recurse on it, you'd expand it like so:
// (1 + (1 + x)) and then (1 + (1 + (1 + x))) and so on...

// So you'd have a function like f x = (1 + x). And to move from here you would feed it into itself like f (f (f ...))
// In other words something like y f = f (y f). Note that this recursion expands like f (f (y f)) and then f (f (f (y f))) and so on.

// As said above though, y f = f (y f) overflows. But if you add an extra argument it lines up: y f x = f (y f) x.
// The first application on the outside and the arguments to (y f) get applied inside the function. Quite ingenius.

// The thing to understand is that f (y f) x is not an infinite expansion, but a transformation. One should not look for
// recursion - from the outside y is only called once and that is to transform the f - the first argument to it into f (y f).

// It matches up.

let func q i = if i < 10 then printfn "%i..." i; q (i+1)
let func' = y func

// What is the result of apply y func?

// fun x -> func (fun x -> func x) x. The type inside the argument passed to func and func itself are the same. If that is not obvious,
// at least note the the number of arguments to func are certainly the same in both cases.