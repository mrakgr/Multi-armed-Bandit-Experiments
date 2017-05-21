type Token =
| Num of int
| Add
| Sub
| Mult
| Div
| Pow
| Stop

type Expr =
| ENum of int
| EAdd of Expr * Expr
| ESub of Expr * Expr
| EMult of Expr * Expr
| EDiv of Expr * Expr
| EPow of Expr * Expr
| ENegate of Expr
| EStop

let bp x s = 
    match x with
    | Stop -> 0
    | Num x -> 1
    | Add -> 50
    | Sub -> 50
    | Mult -> 60
    | Div -> 60
    | Pow -> 70

let test1 = [|Num 2; Mult; Num 3; Add; Num 1; Stop|]
let test2 = [|Num 5; Div; Num 2; Pow; Num 3; Pow; Num 4; Stop|]
let test3 = [|Sub; Sub; Num 5; Div; Num 2; Pow; Num 3; Pow; Num 4; Stop|]

type ParserState =
    {
    mutable token_idx: int
    tokens: Token[]
    }

let s0 x = {token_idx=0; tokens=x}
let peek (s: ParserState) = s.tokens.[s.token_idx]
let token (s: ParserState) = 
    let t = peek s
    s.token_idx <- s.token_idx + 1
    t

let rec led left token s =
    let bp = bp token s
    match token with
    | Add -> EAdd(left, expression bp s)
    | Sub -> ESub(left, expression bp s)
    | Mult -> EMult(left, expression bp s)
    | Div -> EDiv(left, expression bp s)
    | Pow -> EPow(left, expression (bp-1) s)

and nud token s = 
    match token with
    | Sub -> ENegate (expression 100 s)
    | Stop -> EStop
    | Num x -> ENum x

and expression rbp s =
    let inline comb parser token s = parser (token s) s
    let rec loop (left: Expr) = if rbp < comb bp peek s then comb (led left) token s |> loop else left
    comb nud token s |> loop
    
let t = expression 0 (s0 test3)
