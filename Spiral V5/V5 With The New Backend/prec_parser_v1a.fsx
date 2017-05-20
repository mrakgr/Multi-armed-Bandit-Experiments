type Token =
| Num of int
| Add
| Sub
| Mult
| Div
| Stop

type Expr =
| ENum of int
| EAdd of Expr * Expr
| ESub of Expr * Expr
| EMult of Expr * Expr
| EDiv of Expr * Expr
| EStop

let bp = function
    | Stop -> 0
    | Num x -> 1
    | Add -> 50
    | Sub -> 50
    | Mult -> 60
    | Div -> 60

let test = [|Num 2; Mult; Num 3; Add; Num 1; Stop|]

let tokens = test
let mutable token_idx = 0
let token() = tokens.[token_idx]
let advance() = token_idx <- token_idx + 1

let rec led token left =
    let bp = bp token
    match token with
    | Add -> EAdd(left, expression bp)
    | Sub -> ESub(left, expression bp)
    | Mult -> EMult(left, expression bp)
    | Div -> EDiv(left, expression bp)

and nud token = 
    let bp = bp token
    match token with
    | Stop -> EStop
    | Num x -> ENum x

and expression rbp =
    let t = token()
    advance()
    let mutable left = nud t 
    while rbp < bp (token()) do
        let t = token()
        advance()
        left <- led t left
    left

let t = expression 0
