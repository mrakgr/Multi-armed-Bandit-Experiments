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
let inl_rec = skipString "inl" .>> spaces .>> skipString "rec" .>> spaces
let fun_rec = skipString "fun" .>> spaces .>> skipString "rec" .>> spaces

let identifier_template = 
    many1Satisfy2L isAsciiLetter (fun x -> isAsciiLetter x || isDigit x || x = ''') "identifier" .>> spaces
    >>=? function
        | "rec" | "if" | "then" | "else" | "inl" | "fun" as x -> fun _ -> 
            Reply(Error,messageError <| sprintf "%s not allowed as an identifier." x)
        | x -> preturn x
        
let between_brackets l p r = between (skipChar l .>> spaces) (skipChar r .>> spaces) p
let rounds p = between_brackets '(' p ')'
let curlies p = between_brackets '{' p '}'
let quares p = between_brackets '[' p ']'

let pattern_identifier = identifier_template |>> S 
let rec pattern_inner_template f = f pattern comma |>> SS
and pattern_inner x = pattern_inner_template sepBy x 
and pattern_inner1 x = pattern_inner_template sepBy1 x
and pattern x = (pattern_identifier <|> rounds pattern_inner) x
let pattern_list = sepEndBy1 pattern_inner1 spaces

let pbool = (skipString "false" |>> fun _ -> LitBool false) <|> (skipString "true" |>> fun _ -> LitBool true)
let puint32 = puint32 .>> skipChar 'u' |>> LitUInt32
let pfloat32 = pfloat .>> skipChar 'f' |>> (float32 >> LitFloat32)
let puint64 = puint64 .>> skipChar 'U' |>> LitUInt64
let pint64 = pint64 .>> skipChar 'L' |>> LitInt64
let pint32 = pint32 .>> notFollowedByString "."  |>> LitInt32
let pfloat64 = pfloat |>> LitFloat64

let lit = ([pbool;puint32;pfloat32;puint64;pint64;pint32;pfloat64] |> List.map attempt |> choice) .>> notFollowedBy asciiLetter .>> spaces
let var = identifier_template |>> V
let if_then_else expr =
    pipe3
        (skipString "if" >>. spaces1 >>. expr)
        (skipString "then" >>. spaces1 >>. expr)
        (skipString "else" >>. spaces1 >>. expr)
        (fun cond tr fl -> If(cond,tr,fl))

let name = identifier_template

let case_inl_pat_statement expr = pipe2 (inl_ >>. pattern_inner1) (eq >>. expr) (fun pattern body -> l pattern body)
let case_inl_name_pat_list_statement expr = pipe3 (inl_ >>. name) pattern_list (eq >>. expr) (fun name pattern body -> l (S name) (inlr' "" pattern body))
let case_inl_rec_name_pat_list_statement expr = pipe3 (inl_rec >>. name) pattern_list (eq >>. expr) (fun name pattern body -> l (S name) (inlr' name pattern body))
let case_fun_name_pat_list_statement expr = pipe3 (fun_ >>. name) pattern_list (eq >>. expr) (fun name pattern body -> l (S name) (methr' "" pattern body))
let case_fun_rec_name_pat_list_statement expr = pipe3 (fun_rec >>. name) pattern_list (eq >>. expr) (fun name pattern body -> l (S name) (methr' name pattern body))

let statements expr = 
    [case_inl_pat_statement; case_inl_name_pat_list_statement; case_inl_rec_name_pat_list_statement
     case_fun_name_pat_list_statement; case_fun_rec_name_pat_list_statement]
    |> List.map (fun x -> x expr |> attempt)
    |> choice

let case_inl_pat_expr expr = pipe2 (inl_ >>. pattern_inner1) (lam >>. expr) (fun pattern body -> inl pattern body)
let case_inl_rec_name_pat_list_expr expr = pipe3 (inl_rec >>. name) pattern_list (lam >>. expr) (fun name pattern body -> inlr' name pattern body)
let case_fun_rec_name_pat_list_expr expr = pipe3 (fun_rec >>. name) pattern_list (lam >>. expr) (fun name pattern body -> methr' name pattern body)

let case_plit expr = lit
let case_if_then_else expr = if_then_else expr 
let case_rounds expr = rounds expr 
let case_var expr = var

let expressions expr =
    [case_inl_pat_expr; case_inl_rec_name_pat_list_expr; case_fun_rec_name_pat_list_expr
     case_plit; case_if_then_else; case_rounds; case_var]
    |> List.map (fun x -> x expr |> attempt)
    |> choice
   
let process_parser_exprs exprs = 
    let error_statement_in_last_pos _ = Reply(Error,messageError "Statements not allowed in the last position of a block.")
    let process_parser_expr a b on_fail on_succ =
        match a,b with
        | ParserStatement a, ParserExpr b -> a b |> ParserExpr |> on_succ
        | ParserExpr a, ParserExpr b -> l E a b |> ParserExpr |> on_succ
        | _, ParserStatement _ -> on_fail error_statement_in_last_pos

    let process_parser_exprs l =
        let rec loop = function
            | b :: a :: xs ->
                process_parser_expr a b 
                    id
                    (fun r -> loop (r :: xs))
            | x :: xs ->
                match x with
                | ParserExpr a -> preturn a 
                | ParserStatement a -> error_statement_in_last_pos
            | [] -> failwith "impossible"
        loop (List.rev l)

    match exprs with
    | _ :: _ -> process_parser_exprs exprs
    | _ -> preturn (VV [])

let indentations statements expressions =
    let expr_indent expr (s: CharStream<_>) =
        let i = s.Column
        let expr (s: CharStream<_>) = 
            if i = s.Column then (optional semicolon >>. expr) s
            elif i < s.Column then (semicolon >>. expr) s
            else pzero s
        many1 expr s

    expr_indent ((statements |>> ParserStatement) <|> (expressions |>> ParserExpr)) >>= process_parser_exprs

let application expr (s: CharStream<_>) =
    let i = s.Column
    let expr_up (s: CharStream<_>) = 
        if i < s.Column then expr s
        else pzero s
    pipe2 expr (many expr_up) (fun x xs -> ap' x xs) s

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

    let operators expr =
        let p = opp.ExpressionParser
        opp.TermParser <- expr
        p

    let rec expr s = 
        tuple (indentations (statements expr) (operators (application (expressions expr)))) s

    expr
    
let test = "a,(b + f e, 2, 3),c"

let test2 = 
    """
    inl w = 4;2
    2
    """

let result = run (spaces >>. expr) test2

