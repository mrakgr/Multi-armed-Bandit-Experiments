#r "../../../packages/FParsec.1.0.2/lib/net40-client/FParsecCS.dll"
#r "../../../packages/FParsec.1.0.2/lib/net40-client/FParsec.dll"

type TokenValue = 
| LitUInt8 of uint8
| LitUInt16 of uint16
| LitUInt32 of uint32
| LitUInt64 of uint64
| LitInt8 of int8
| LitInt16 of int16
| LitInt32 of int32
| LitInt64 of int64
| LitFloat32 of float32
| LitFloat64 of float
| LitBool of bool
| LitString of string

| DefaultInt of string
| DefaultFloat of string

type TokenCore =
| TValue of TokenValue
| TVar of string
| TType of string
| TBuiltinType of string
| TOp of string
| TOpenPar
| TClosePar

type Token = int64 * int64 * TokenCore

type ParserState =
    {
    default_int : string -> Value
    default_float : string -> Talue
    }

module Tokenizer =
    open FParsec

    let isAsciiIdStart c =
        isAsciiLetter c || c = '_'

    let tokenize p (s: CharStream<_>) =
        let c,r = s.Column, s.Line
        (p |>> fun x -> r, c, x) s

    let identifier : Parser<Token,unit> =
        let isAsciiIdContinue c =
            isAsciiLetter c || isDigit c || c = '_' || c = '\''

        identifier (IdentifierOptions(isAsciiIdStart    = isAsciiIdStart,
                                      isAsciiIdContinue = isAsciiIdContinue))
        .>> spaces
        |>> fun x ->
            if x.Length = 1 && x.[0] = '_' then TVar ""
            elif x.[0] = '_' && isAsciiUpper x.[1] then TBuiltinType x.[1..]
            elif isAsciiUpper x.[0] then TType x
            else TVar x
        |> tokenize


    let pnumber : Parser<Token, unit> =
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
                f "i8" (int8 >> LitInt8)
                f "i16" (int16 >> LitInt16)
                f "i32" (int32 >> LitInt32)
                f "i64" (int64 >> LitInt64)

                f "u8" (uint8 >> LitUInt8)
                f "u16" (uint16 >> LitUInt16)
                f "u32" (uint32 >> LitUInt32)
                f "u64" (uint64 >> LitUInt64)

                f "f32" (float32 >> LitFloat32)
                f "f64" (float >> LitFloat64)
                default_
                ]
            .>> notFollowedBy (satisfy (fun c -> isDigit c || isAsciiIdStart c))
            .>> spaces
            |> tokenize

        fun s ->
            let reply = parser s
            if reply.Status = Ok then
                let nl = reply.Result // the parsed NumberLiteral
                try 
                    let default_ (s: CharStream<_>) =
                        if nl.IsInteger then DefaultInt nl.String
                        else DefaultFloat nl.String
                        |> TValue |> Reply

                    followedBySuffix nl.String default_ s
                with
                | :? System.OverflowException as e ->
                    s.Skip(-nl.String.Length)
                    Reply(FatalError, messageError e.Message)
            else // reconstruct error reply
                Reply(reply.Status, reply.Error)

