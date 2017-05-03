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

let pattern_identifier = 
    choice
        [
        skipChar '_' |>> fun _ -> S ""
        skipChar '*' >>. identifier_template |>> S'
        identifier_template |>> S
        ]
let rec pattern_inner_template f = f pattern comma |>> SS
and pattern_inner x = pattern_inner_template sepBy x 
and pattern_inner1 x = pattern_inner_template sepBy1 x
and pattern x = (pattern_identifier <|> rounds pattern_inner) x
let pattern_list = sepEndBy1 (rounds pattern_inner1 <|> pattern_inner1) spaces


let pbool = (skipString "false" |>> fun _ -> LitBool false) <|> (skipString "true" |>> fun _ -> LitBool true)

// FParsec has inbuilt parsers for ints and floats, but they will scan + and - which will wreak havoc with other parts
// of the library hence the custom number parsers.
let pnumber = 
    let pnumber = many1Satisfy isDigit
    pnumber >>= fun a ->
        choice [
            skipChar 'u' |>> (fun _ -> uint32 a |> LitUInt32)
            skipString "UL" |>> (fun _ -> uint64 a |> LitUInt64)
            skipString "L" |>> (fun _ -> int64 a |> LitInt64)
            skipChar '.' >>. pnumber >>= fun b ->
                let cfloat f = sprintf "%s.%s" a b |> f
                skipChar 'f' |>> fun _ -> cfloat float32 |> LitFloat32
                <|> fun _ -> Reply(cfloat float |> LitFloat64)
            fun _ -> Reply(int a |> LitInt32)
            ]

let lit = (pbool <|> pnumber) .>> notFollowedBy asciiLetter .>> spaces
let var = identifier_template |>> V
let if_then_else expr (s: CharStream<_>) =
    let i = s.Column
    let expr_indent expr (s: CharStream<_>) = if i <= s.Column then expr s else pzero s
    pipe3
        (skipString "if" >>. spaces1 >>. expr)
        (expr_indent (skipString "then" >>. spaces1 >>. expr))
        (opt (expr_indent (skipString "else" >>. spaces1 >>. expr)))
        (fun cond tr fl -> 
            let fl = match fl with Some x -> x | None -> VV []
            If(cond,tr,fl))
        s

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

let case_inl_pat_list_expr expr = pipe2 (inl_ >>. pattern_list) (lam >>. expr) (fun pattern body -> inl' pattern body)
let case_fun_pat_list_expr expr = pipe2 (fun_ >>. pattern_list) (lam >>. expr) (fun pattern body -> meth' pattern body)
let case_inl_rec_name_pat_list_expr expr = pipe3 (inl_rec >>. name) pattern_list (lam >>. expr) (fun name pattern body -> inlr' name pattern body)
let case_fun_rec_name_pat_list_expr expr = pipe3 (fun_rec >>. name) pattern_list (lam >>. expr) (fun name pattern body -> methr' name pattern body)

let case_plit expr = lit
let case_if_then_else expr = if_then_else expr 
let case_rounds expr = rounds expr 
let case_var expr = var

let expressions expr =
    [case_inl_pat_list_expr; case_fun_pat_list_expr; case_inl_rec_name_pat_list_expr; case_fun_rec_name_pat_list_expr
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

let indentations statements expressions (s: CharStream<_>) =
    let i = s.Column
    let inline if_col op tr fl (s: CharStream<_>) = if op i s.Column then tr s else fl s
    let expr_indent expr (s: CharStream<_>) =
        let expr = 
            if_col (=) 
                (optional semicolon >>. expr)
                (if_col (<)
                    (semicolon >>. (if_col (<=) expr pzero))
                    pzero)
        many1 expr s

    expr_indent ((statements |>> ParserStatement) <|> (expressions i |>> ParserExpr)) >>= process_parser_exprs
    <| s

let application expr (s: CharStream<_>) =
    let i = s.Column
    let expr_up (s: CharStream<_>) = 
        if i < s.Column then expr s
        else pzero s
    pipe2 expr (many expr_up) ap' s

let tuple expr i (s: CharStream<_>) =
    let expr_indent expr (s: CharStream<_>) = if i <= s.Column then expr s else pzero s
    sepBy1 (expr_indent (expr i)) (expr_indent comma)
    |>> function
        | _ :: _ :: _ as l -> VV l
        | x :: _ -> x
        | _ -> failwith "Empty tuples are handled in expr_block."
    <| s

let expr =
    let opp = new OperatorPrecedenceParser<_,_,_>()

    opp.AddOperator(InfixOperator("+", spaces, 6, Associativity.Left, fun x y -> Add(x,y)))
    opp.AddOperator(InfixOperator("-", spaces, 6, Associativity.Left, fun x y -> Sub(x,y)))
    opp.AddOperator(PrefixOperator("-", spaces, 10, true, Neg))
    opp.AddOperator(InfixOperator("*", spaces, 7, Associativity.Left, fun x y -> Mult(x,y)))
    opp.AddOperator(InfixOperator("/", spaces, 7, Associativity.Left, fun x y -> Div(x,y)))

    opp.AddOperator(InfixOperator("<=", spaces, 4, Associativity.None, fun x y -> LTE(x,y)))
    opp.AddOperator(InfixOperator("<", spaces, 4, Associativity.None, fun x y -> LT(x,y)))
    opp.AddOperator(InfixOperator("=", spaces, 4, Associativity.None, fun x y -> EQ(x,y)))
    opp.AddOperator(InfixOperator("<>", spaces, 4, Associativity.None, fun x y -> NEQ(x,y)))
    opp.AddOperator(InfixOperator(">", spaces, 4, Associativity.None, fun x y -> GT(x,y)))
    opp.AddOperator(InfixOperator(">=", spaces, 4, Associativity.None, fun x y -> GTE(x,y)))

    opp.AddOperator(InfixOperator("||", spaces, 2, Associativity.Right, fun x y -> And(x,y)))
    opp.AddOperator(InfixOperator("&&", spaces, 3, Associativity.Right, fun x y -> Or(x,y)))

    opp.AddOperator(InfixOperator("|>", spaces, 1, Associativity.Left, fun a f -> Apply(f,a)))
    opp.AddOperator(InfixOperator(">>", spaces, 1, Associativity.Left, fun a b -> inl (S "x") (Apply(b,Apply(a,V "x")))))
    opp.AddOperator(InfixOperator("<|", spaces, 1, Associativity.Left, fun f a -> Apply(f,a)))
    opp.AddOperator(InfixOperator("<<", spaces, 1, Associativity.Left, fun b a -> inl (S "x") (Apply(b,Apply(a,V "x")))))

    let operators expr i =
        let f expr (s: CharStream<_>) = if i <= s.Column then expr s else pzero s
        opp.TermParser <- f expr
        opp.ExpressionParser

    let rec expr s = indentations (statements expr) (tuple (operators (application (expressions expr)))) s

    expr
    
let test = "a,(b + f e, 2, 3),c"

let test2 = 
    """
    2 
     + 
    2
    """

let result = run (spaces >>. expr) test2

