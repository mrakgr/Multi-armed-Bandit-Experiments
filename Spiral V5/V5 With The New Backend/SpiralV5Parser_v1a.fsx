#load "SpiralV5CudaTypechecker_v7c'.fsx"
#r "../../packages/FParsec.1.0.2/lib/net40-client/FParsecCS.dll"
#r "../../packages/FParsec.1.0.2/lib/net40-client/FParsec.dll"
//#r "../../packages/FParsec-Pipes.0.3.1.0/lib/net45/FParsec-Pipes.dll"

open SpiralV5CudaTypechecker_v7c'
open FParsec

type ParserExpr =
    | ParserStatement of (CudaExpr -> CudaExpr)
    | ParserExpr of CudaExpr

let skipSpacesChar x = spaces >>. skipChar x
let comma = skipSpacesChar ','
let semicolon = skipSpacesChar ';'
let eq = skipSpacesChar '='
let lam = spaces >>. skipString "->"
let inl_ = spaces >>. skipString "inl"
let fun_ = spaces >>. skipString "fun"

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

let term = spaces >>. choice [plit; variable]

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

let case_pat expr = pipe2 pattern (eq >>. expr) (fun pattern body -> ParserStatement <| l pattern body)
let case_name_pat_list expr = pipe3 name pattern_list (eq >>. expr) (fun name pattern body -> ParserStatement <| l (S name) (inlr' "" pattern body))
let case_inl_name_pat_list expr = pipe3 (inl_ >>. name) pattern_list (eq >>. expr) (fun name pattern body -> ParserStatement <| l (S name) (inlr' name pattern body))
let case_inl_pat_list expr = pipe2 (inl_ >>. pattern_list) (lam >>. expr) (fun pattern body -> ParserExpr <| inl' pattern body)
let case_fun_name_pat_list expr = pipe3 (fun_ >>. name) pattern_list (eq >>. expr) (fun name pattern body -> ParserStatement <| l (S name) (methr' name pattern body))
let case_fun_pat_list expr = pipe2 (fun_ >>. pattern_list) (lam >>. expr) (fun pattern body -> ParserExpr <| meth' pattern body)
let case_expr expr = expr |>> ParserExpr

let cases_all expr =
    let cases =
        [case_pat; case_name_pat_list; case_inl_name_pat_list; case_inl_pat_list; case_fun_name_pat_list; case_fun_pat_list; case_expr]
        |> List.map (fun x -> x expr |> attempt)
    spaces >>. choice cases

let expr_block expr =
    let process_parser_expr a b =
        match a,b with
        | ParserStatement a, ParserExpr b -> a b |> ParserExpr
        | ParserExpr a, ParserExpr b -> l E a b |> ParserExpr
        | _, ParserStatement _ -> failwith "Parser statements not allowed in the last position of a block. Expected an expression."
    between_curlies (sepBy1 (cases_all expr) semicolon)
    |>> List.reduceBack process_parser_expr
    |>> function ParserExpr a -> a | _ -> failwith "impossible"

let application expr =
    spaces >>. many1 expr
    |>> function x :: xs -> ap' x xs | _ -> failwith "impossible"

let rec expr x = 
    let expr_body x = (spaces >>. (term <|> expr_block expr)) x
    application expr_body x

run (expr) ""
run (expr) "{f g {q w e}}"

