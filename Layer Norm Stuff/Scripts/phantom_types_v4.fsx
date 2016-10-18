// For the SO question: http://stackoverflow.com/questions/40062180/how-can-this-phantom-type-example-possibly-be-valid/40062961#40062961

type Expr<'a> =
    | C of 'a
    | Add of ((int -> 'a) * Expr<int> * Expr<int>)
    | Eq of ((bool -> 'a) * Expr<int> * Expr<int>)

let inline id x = x
let inline add x y = Add(id,x,y)
let inline eq x y = Eq(id,x,y)

let f<'T> (x : 'T) y = x

let rec eval<'a> (x: Expr<'a>) : 'a = 
    match x with
    | C x -> x
    | Add(f,x,y) -> f (eval x + eval y)
    | Eq(f,x,y) -> f (eval x = eval y)

let expr = add (C 5) (C 1) |> eq (C 6)

let r = eval expr
