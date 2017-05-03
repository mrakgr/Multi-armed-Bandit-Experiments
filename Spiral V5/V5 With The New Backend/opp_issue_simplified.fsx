#r "../../packages/FParsec.1.0.2/lib/net40-client/FParsecCS.dll"
#r "../../packages/FParsec.1.0.2/lib/net40-client/FParsec.dll"

open FParsec

type Expr = 
    | V of string
    | Add of Expr * Expr

let identifier = many1Satisfy2L isAsciiLetter (fun x -> isAsciiLetter x || isDigit x || x = ''') "identifier" .>> spaces |>> V

let indentations expressions (s: CharStream<_>) =
    let i = s.Column
    let expr_indent expr (s: CharStream<_>) =
        let expr (s: CharStream<_>) = if i <= s.Column then expr s else pzero s
        many1 expr s

    expr_indent (expressions i) s

let expr =
    let opp = new OperatorPrecedenceParser<_,_,_>()
    opp.AddOperator(InfixOperator("+", spaces, 6, Associativity.Left, fun x y -> Add(x,y)))

    let operators expr i =
        let f expr (s: CharStream<_>) = if i <= s.Column then expr s else pzero s
        opp.TermParser <- f expr
        f opp.ExpressionParser

    let rec expr s = indentations (operators identifier) s

    expr

let test2 = // I'd like this to be an error.
    """
    a 
   + 
    b 
    """

let result = run (spaces >>. expr) test2