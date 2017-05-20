#r "../../packages/FParsec.1.0.2/lib/net40-client/FParsecCS.dll"
#r "../../packages/FParsec.1.0.2/lib/net40-client/FParsec.dll"

open FParsec
 
type Token =
| Num of int
| Add
| Sub
| Mult
| Div


let prec = function
    | Num x -> 1
    | Add -> 50
    | Sub -> 50
    | Mult -> 60
    | Div -> 60

let is_nub = function
    | Num _ -> true
    | _ -> false

let test = [|Num 1; Add; Num 2; Mult; Num 3|]

