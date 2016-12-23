type Exp<'a,'r> =
    | Base of 'r
    | Op of 'a * ('a -> 'r)

let eval<'a,'r> (exp:Exp<'a,'r>): 'r =
    match exp with
    | Base r -> r
    | Op(x, r) -> r x
    
