/// http://okmij.org/ftp/tagless-final/course/lecture.pdf

// I am really curious about the above.
// I feel like following along in F#.

// This might be a substitute for pattern matching.
// I was leaning in this direction already, but there are some things,
// I cannot quite figure out how to represent without using GADTs.

// Maybe this will resolve some of the things that have been on my mind.

//type Repr = int
//
//let lit (n: Repr): Repr = n
//let neg (n: Repr): Repr = -n
//let add (a: Repr) (b: Repr): Repr = a+b
//
//let tf1 = add (lit 8) (neg (add (lit 1) (lit 2)))
//


type ExprSym<'repr> =
    abstract Lit: int -> 'repr
    abstract Neg: 'repr -> 'repr
    abstract Add: 'repr -> 'repr -> 'repr

let lit (cl: ExprSym<'repr>) n = cl.Lit n
let neg (cl: ExprSym<'repr>) n = cl.Neg n
let add (cl: ExprSym<'repr>) a b = cl.Add a b

// Nope, the examples cannot be emulated by anything in F#.

// ...Haskell is doing a particularly good job of making me want
// new language features lately.
    
// Edit: Actually, this could be done using object algebras.
// https://oleksandrmanzyuk.wordpress.com/2014/06/18/from-object-algebras-to-finally-tagless-interpreters-2/

// So something like...

let expr1 (f: ExprSym<'repr>) =
    f.Lit 2 |> f.Add (f.Lit 3) |> f.Neg

let r = 
    expr1 
        {new ExprSym<int> with
            member t.Lit x = x
            member t.Add a b = a+b
            member t.Neg x = -x
            }

let p =
    expr1 
        {new ExprSym<string> with
            member t.Lit x = string x
            member t.Add a b = sprintf "(%s + %s)" a b
            member t.Neg x = sprintf "-(%s)" x
            }

