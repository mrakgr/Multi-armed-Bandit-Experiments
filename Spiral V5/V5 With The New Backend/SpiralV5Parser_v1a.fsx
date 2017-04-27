#load "SpiralV5CudaTypechecker_v7c'.fsx"
#r "../../packages/FParsec.1.0.2/lib/net40-client/FParsecCS.dll"
#r "../../packages/FParsec.1.0.2/lib/net40-client/FParsec.dll"
//#r "../../packages/FParsec-Pipes.0.3.1.0/lib/net45/FParsec-Pipes.dll"

open SpiralV5CudaTypechecker_v7c'
open FParsec

let skipSpacesChar x = spaces >>. skipChar x
let comma = skipSpacesChar ','
let semicolon = skipSpacesChar ';'
let eq = skipSpacesChar '='
let lam x = 
    spaces >>. skipString "->"
    <| x
let inl x = 
    spaces >>. skipString "inl"
    <| x
let fun_ x = 
    spaces >>. skipString "fun"
    <| x

let identifier_template x = spaces >>. many1Satisfy isAsciiLetter |>> x 
let pattern_identifier = identifier_template S

let between_brackets l p r = between (skipSpacesChar l) (skipSpacesChar r) p
let between_rounds p = between_brackets '(' p ')'
let between_curlies p = between_brackets '{' p '}'
let between_squares p = between_brackets '[' p ']'

let rec pattern x = (spaces >>. (pattern_identifier <|> (sepBy (spaces >>. pattern) comma |> between_rounds |>> SS))) x

let pattern_list = sepEndBy1 pattern spaces

let pbool = (skipString "false" |>> fun _ -> LitBool false) <|> (skipString "true" |>> fun _ -> LitBool true)
let puint32 = puint32 .>> skipChar 'u' |>> LitUInt32
let pfloat32 = pfloat .>> skipChar 'f' |>> (float32 >> LitFloat32)
let puint64 = puint64 .>> skipChar 'U' |>> LitUInt64
let pint64 = pint64 .>> skipChar 'L' |>> LitInt64
let pint32 = pint32 .>> notFollowedByString "."  |>> LitInt32
let pfloat64 = pfloat |>> LitFloat64

let plit = spaces >>. ([pbool;puint32;pfloat32;puint64;pint64;pint32;pfloat64] |> List.map attempt |> choice)
let variable = identifier_template V
let application = failwith ""

let expr_body = spaces >>. ([plit; variable; application] |> List.map attempt |> choice)

let expr_single = spaces >>. expr_body
let expr_block = between_curlies (sepBy1 expr_single semicolon)
let expr = spaces >>. (expr_block <|> expr_single)

// There will be 7 basic patterns in the language.

//1) pat = expr
//2) name [pat] = expr
//3) inl name [pat] = expr
//4) inl [pat] → expr
//5) fun name [pat] = expr
//6) fun [pat] → expr
//7) expr

// #2 and #3 are equivalent to each other, but there rest are not. #4 and #6 are expressions
// themselves and the `=` patterns are statements.

let name = identifier_template id

let case_pat = pipe2 pattern (eq >>. expr) (fun a b -> a)
let case_name_pat_list = pipe3 name pattern (eq >>. expr) (fun a b c -> (a,b))
let case_inl_name_pat = pipe3 (inl >>. name) pattern (eq >>. expr) (fun a b c -> (a,b))
let case_inl_pat = pipe3 (inl >>. name) pattern (lam >>. expr) (fun a b c -> (a,b))
let case_fun_name_pat = pipe3 (fun_ >>. name) pattern (eq >>. expr) (fun a b c -> (a,b))
let case_fun_pat = pipe3 (fun_ >>. name) pattern (lam >>. expr) (fun a b c -> (a,b))
let case_expr = expr

run case_pat "(a,b) = a + b;"

