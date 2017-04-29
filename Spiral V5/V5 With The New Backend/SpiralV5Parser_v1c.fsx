#load "SpiralV5CudaTypechecker_v7c'.fsx"
#r "../../packages/FParsec.1.0.2/lib/net40-client/FParsecCS.dll"
#r "../../packages/FParsec.1.0.2/lib/net40-client/FParsec.dll"
//#r "../../packages/FParsec-Pipes.0.3.1.0/lib/net45/FParsec-Pipes.dll"

open SpiralV5CudaTypechecker_v7c'
open FParsec

type ParserExpr =
    | ParserStatement of (CudaExpr -> CudaExpr)
    | ParserExpr of CudaExpr

let comma = skipChar ',' .>> spaces
let semicolon = skipChar ';' .>> spaces
let eq = skipChar '=' .>> spaces
let lam = skipString "->" .>> spaces
let inl_ = skipString "inl" .>> spaces
let fun_ = skipString "fun" .>> spaces

let identifier_template = many1Satisfy2L isAsciiLetter (fun x -> isAsciiLetter x || isDigit x || x = ''') "identifier" .>> spaces
let between_brackets l p r = between (skipChar l .>> spaces) (skipChar r .>> spaces) p
let between_rounds p = between_brackets '(' p ')'
let between_curlies p = between_brackets '{' p '}'
let between_squares p = between_brackets '[' p ']'

let pattern_identifier = identifier_template |>> S 
let rec pattern_inner_template f = f pattern comma |>> SS
and pattern_inner x = pattern_inner_template sepBy x 
and pattern_inner1 x = pattern_inner_template sepBy1 x
and pattern x = (pattern_identifier <|> between_rounds pattern_inner) x
let pattern_list = sepEndBy1 pattern_inner1 spaces

let pbool = (skipString "false" |>> fun _ -> LitBool false) <|> (skipString "true" |>> fun _ -> LitBool true)
let puint32 = puint32 .>> skipChar 'u' |>> LitUInt32
let pfloat32 = pfloat .>> skipChar 'f' |>> (float32 >> LitFloat32)
let puint64 = puint64 .>> skipChar 'U' |>> LitUInt64
let pint64 = pint64 .>> skipChar 'L' |>> LitInt64
let pint32 = pint32 .>> notFollowedByString "."  |>> LitInt32
let pfloat64 = pfloat |>> LitFloat64

let plit = ([pbool;puint32;pfloat32;puint64;pint64;pint32;pfloat64] |> List.map attempt |> choice) .>> notFollowedBy asciiLetter .>> spaces
let variable_or_if_then_else expr =
    identifier_template 
    >>=? function
        | "if" -> 
            pipe3
                expr
                (skipString "then" >>. spaces1 >>. expr)
                (skipString "else" >>. spaces1 >>. expr)
                (fun cond tr fl -> If(cond,tr,fl))
        | "then" | "else" | "inl" | "fun" as x -> 
            fun _ -> Reply(Error, messageError <| sprintf "variable cannot be named one of: then, else, inl, fun.\nGot: %s" x)
        | x -> fun _ -> Reply(V x)
            
let term expr = plit <|> variable_or_if_then_else expr

// There will be 8 basic patterns in the language.

// 1) pat = expr
// 2) name [pat] = expr // Is not recursive.
// 3) inl name [pat] = expr // Is recursive
// 4) inl [pat] → expr
// 5) fun name [pat] = expr // Is recursive
// 6) fun [pat] → expr
// 7) expr
// 8) [pat] -> expr

let name = identifier_template

let case_pat expr = pipe2 pattern_inner1 (eq >>. expr) (fun pattern body -> ParserStatement <| l pattern body)
let case_name_pat_list expr = pipe3 name pattern_list (eq >>. expr) (fun name pattern body -> ParserStatement <| l (S name) (inlr' "" pattern body))
let case_inl_name_pat_list expr = pipe3 (inl_ >>. name) pattern_list (eq >>. expr) (fun name pattern body -> ParserStatement <| l (S name) (inlr' name pattern body))
let case_inl_pat_list expr = pipe2 (inl_ >>. pattern_list) (lam >>. expr) (fun pattern body -> ParserExpr <| inl' pattern body)
let case_fun_name_pat_list expr = pipe3 (fun_ >>. name) pattern_list (eq >>. expr) (fun name pattern body -> ParserStatement <| l (S name) (methr' name pattern body))
let case_fun_pat_list expr = pipe2 (fun_ >>. pattern_list) (lam >>. expr) (fun pattern body -> ParserExpr <| meth' pattern body)
let case_expr expr = expr |>> ParserExpr
let case_pat_list expr = pipe2 pattern_list (lam >>. expr) (fun pattern body -> ParserExpr <| inl' pattern body)

let cases_all expr =
    [case_inl_name_pat_list; case_inl_pat_list; case_fun_name_pat_list; case_fun_pat_list
     case_pat; case_name_pat_list; case_pat_list; case_expr]
    |> List.map (fun x -> x expr |> attempt)
    |> choice 

let expr_indent expr (s: CharStream<_>) =
    let i = s.Column
    let mutable idx = s.Index
    (many1 (expr >>=? fun x -> 
        if i = s.Column then 
            if idx <> s.Index then 
                idx <- s.Index
                preturn x 
            else pzero
        else pzero)) s

let expr_semicolon expr = sepBy expr semicolon

let expr_block expr =
    let process_parser_expr a b =
        match a,b with
        | ParserStatement a, ParserExpr b -> a b |> ParserExpr
        | ParserExpr a, ParserExpr b -> l E a b |> ParserExpr
        | _, ParserStatement _ -> failwith "Parser statements not allowed in the last position of a block. Expected an expression."

    let process_parser_exprs l =
        List.reduceBack process_parser_expr l
        |> function
            | ParserExpr a -> a 
            | ParserStatement a ->
                failwithf "Last member of a block must be an expression or the block must be empty. Got a statement instead.\n%A" (a (V "debug"))
    
    let process_fin = function
        | _ :: _ as l -> process_parser_exprs l
        | _ -> VV []

    let cases x = cases_all expr x

    between_rounds (expr_indent (expr_semicolon cases) |>> (fun x -> printfn "Done with expr_indent.x=%A" x; x)) 
    |>> (List.concat >> process_fin)
    //<|> (expr_indent cases |>> process_fin)

    //between_rounds (expr_semicolon cases) |>> process_fin
    

let application expr =
    many1 expr
    |>> function x :: xs -> ap' x xs | _ -> failwith "impossible in application"

let tuple expr =
    sepBy1 expr comma
    |>> function
        | _ :: _ :: _ as l -> VV l
        | x :: _ -> x
        | _ -> failwith "Empty tuples are handled in expr_block."

let expr =
    let opp = new OperatorPrecedenceParser<_,_,_>()

    opp.AddOperator(InfixOperator("+", spaces, 6, Associativity.Left, fun x y -> Add(x,y)))
    opp.AddOperator(InfixOperator("-", spaces, 6, Associativity.Left, fun x y -> Sub(x,y)))
    opp.AddOperator(PrefixOperator("-", spaces, 10, true, Neg))
    opp.AddOperator(InfixOperator("*", spaces, 7, Associativity.Left, fun x y -> Mult(x,y)))
    opp.AddOperator(InfixOperator("/", spaces, 7, Associativity.Left, fun x y -> Div(x,y)))

    opp.AddOperator(InfixOperator("<=", spaces, 4, Associativity.None, fun x y -> LTE(x,y)))
    opp.AddOperator(InfixOperator("<", spaces, 4, Associativity.None, fun x y -> LT(x,y)))
    opp.AddOperator(InfixOperator("==", spaces, 4, Associativity.None, fun x y -> EQ(x,y)))
    opp.AddOperator(InfixOperator(">", spaces, 4, Associativity.None, fun x y -> GT(x,y)))
    opp.AddOperator(InfixOperator(">=", spaces, 4, Associativity.None, fun x y -> GTE(x,y)))

    opp.AddOperator(InfixOperator("||", spaces, 2, Associativity.Right, fun x y -> And(x,y)))
    opp.AddOperator(InfixOperator("&&", spaces, 3, Associativity.Right, fun x y -> Or(x,y)))

    let expr = tuple opp.ExpressionParser
    //opp.TermParser <- application (expr_block expr <|> term expr)
    opp.TermParser <- expr_block expr <|> term expr
    //opp.TermParser <- expr_block (term expr)
    expr

let test = 
    """(2)"""

//run (expr) ""
run (spaces >>. expr) test
  
