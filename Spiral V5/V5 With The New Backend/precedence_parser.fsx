#r "../../packages/FParsec.1.0.2/lib/net40-client/FParsecCS.dll"
#r "../../packages/FParsec.1.0.2/lib/net40-client/FParsec.dll"

open FParsec

let runCalculatorParser =
    let ws = spaces
    let str_ws s = pstring s >>. ws

    let opp = new OperatorPrecedenceParser<float,unit,unit>()
    let expr = opp.ExpressionParser
    let rec term x = ((pfloat .>> ws) <|> between (str_ws "(") (str_ws ")") term) x
    opp.TermParser <- term

    opp.AddOperator(InfixOperator("+", ws, 6, Associativity.Left, fun x y -> x + y))
    opp.AddOperator(InfixOperator("-", ws, 6, Associativity.Left, fun x y -> x - y))
    opp.AddOperator(InfixOperator("*", ws, 7, Associativity.Left, fun x y -> x * y))
    opp.AddOperator(InfixOperator("/", ws, 7, Associativity.Left, fun x y -> x / y))

    fun str -> run (ws >>. expr) str

runCalculatorParser "2 + (5 + 2)"