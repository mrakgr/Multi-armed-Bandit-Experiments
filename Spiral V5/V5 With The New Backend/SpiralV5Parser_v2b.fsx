#load "SpiralV5Language_v8b.fsx"
#r "../../packages/FParsec.1.0.2/lib/net40-client/FParsecCS.dll"
#r "../../packages/FParsec.1.0.2/lib/net40-client/FParsec.dll"

open SpiralV5Language_v8b

type TokenValue = 
| TokUInt8 of uint8
| TokUInt16 of uint16
| TokUInt32 of uint32
| TokUInt64 of uint64
| TokInt8 of int8
| TokInt16 of int16
| TokInt32 of int32
| TokInt64 of int64
| TokFloat32 of float32
| TokFloat64 of float
| TokBool of bool
| TokString of string

| TokDefaultInt of string
| TokDefaultFloat of string

type TokenCore =
| TValue of TokenValue
| TVar of string
| TType of string
| TBuiltinType of string
| TOp of string
| TOpenPar
| TClosePar
| TStop

type Token = int64 * int64 * TokenCore

type ParserState =
    {
    default_int : string -> Value
    default_float : string -> Value
    }

module Tokenizer =
    open FParsec

    let isAsciiIdStart c =
        isAsciiLetter c || c = '_'

    let tokenize p (s: CharStream<_>) =
        let c,r = s.Column, s.Line
        (p |>> fun x -> r, c, x) s

    let isAsciiIdContinue c =
        isAsciiLetter c || isDigit c || c = '_' || c = '\''

    let identifier =
        let identifier' : Parser<_,unit> =
            identifier (IdentifierOptions(isAsciiIdStart    = isAsciiIdStart,
                                          isAsciiIdContinue = isAsciiIdContinue))
            |>> fun x ->
                if x.Length = 1 && x.[0] = '_' then TVar ""
                elif x.[0] = '_' && isAsciiUpper x.[1] then TBuiltinType x.[1..]
                elif isAsciiUpper x.[0] then TType x
                else TVar x
        identifier' .>> spaces |> tokenize

    let number : Parser<Token, unit> =
        let numberFormat =     NumberLiteralOptions.AllowMinusSign
                           ||| NumberLiteralOptions.AllowFraction
                           ||| NumberLiteralOptions.AllowExponent
                           ||| NumberLiteralOptions.AllowHexadecimal
                           ||| NumberLiteralOptions.AllowBinary
                           ||| NumberLiteralOptions.AllowInfinity
                           ||| NumberLiteralOptions.AllowNaN

        let parser = numberLiteral numberFormat "number"

        let followedBySuffix x default_ =
            let f str f = followedBy (skipString str) |>> fun _ -> f x |> TValue
            choice
                [
                f "i8" (int8 >> TokInt8)
                f "i16" (int16 >> TokInt16)
                f "i32" (int32 >> TokInt32)
                f "i64" (int64 >> TokInt64)

                f "u8" (uint8 >> TokUInt8)
                f "u16" (uint16 >> TokUInt16)
                f "u32" (uint32 >> TokUInt32)
                f "u64" (uint64 >> TokUInt64)

                f "f32" (float32 >> TokFloat32)
                f "f64" (float >> TokFloat64)
                default_
                ]

        fun s ->
            let reply = parser s
            if reply.Status = Ok then
                let nl = reply.Result // the parsed NumberLiteral
                try 
                    let default_ (s: CharStream<_>) =
                        if nl.IsInteger then TokDefaultInt nl.String
                        else TokDefaultFloat nl.String
                        |> TValue |> Reply

                    tokenize 
                        (followedBySuffix nl.String default_
                        .>> notFollowedBy (satisfy (fun c -> isDigit c || isAsciiIdStart c))
                        .>> spaces) s
                    
                with
                | :? System.OverflowException as e ->
                    s.Skip(-nl.String.Length)
                    Reply(FatalError, messageError e.Message)
            else // reconstruct error reply
                Reply(reply.Status, reply.Error)

    let operator: Parser<_,unit> =
        (many1Satisfy (fun c -> (isAsciiIdContinue c || isAnyOf [|' ';'\t';'\n';'\"';'(';')'|] c) = false) |>> TOp)
        .>> spaces
        |> tokenize

    let parenth: Parser<_,unit> =
        satisfy (fun c -> c = '(' || c = ')') |>> function
            | '(' -> TOpenPar
            | _ -> TClosePar
        .>> spaces
        |> tokenize

    let row (row, _, _) = row
    let col (_, col, _) = col

    let arrayMany x =
        Inline.Many(
            (fun x ->
                let t = ResizeArray()
                t.Add x
                t),
            (fun s x ->
                s.Add x
                s),
            (fun s ->
                s.Add (0L,0L,TStop)
                s.ToArray()),
            x)

    let lex = spaces >>. arrayMany (choice [identifier; number; operator; parenth]) .>> eof
    let run_lex x = run lex x

let fib_acc_y = // The Y Combinator needs all the arguments when dealing with methods.
    """
fun rec y f n a b = f (y f) n a b
inl fib n =
    inl fib r n a b = if n >= 0 then r (n-1) b (a+b) else a
    y fib n 0 1
fib 2
    """

Tokenizer.run_lex fib_acc_y

let parser (default_int,default_float) =
    ()