#r "../../../packages/FParsec.1.0.2/lib/net40-client/FParsecCS.dll"
#r "../../../packages/FParsec.1.0.2/lib/net40-client/FParsec.dll"

type Value = 
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

type Token =
| TValue of Value
| TVar of string
| TType of string
| TBuiltinType of string
| TBlank
| TOp of string
| TOpPar
| TCloPar

module Tokenizer =
    open FParsec

    type ParserState =
        {
        default_int : string -> Value
        default_float : string -> Value
        }

    let identifier : Parser<_,ParserState> =
        let isAsciiIdStart c =
            isAsciiLetter c || c = '_'

        let isAsciiIdContinue c =
            isAsciiLetter c || isDigit c || c = '_' || c = '\''

        identifier (IdentifierOptions(isAsciiIdStart    = isAsciiIdStart,
                                      isAsciiIdContinue = isAsciiIdContinue))
        |>> fun x ->
            if x.Length = 1 && x.[0] = '_' then TBlank
            elif x.[0] = '_' && isAsciiUpper x.[1] then TBuiltinType x.[1..]
            elif isAsciiUpper x.[0] then TType x
            else TVar x

    let pnumber : Parser<_, ParserState> =
        let numberFormat =     NumberLiteralOptions.AllowMinusSign
                           ||| NumberLiteralOptions.AllowFraction
                           ||| NumberLiteralOptions.AllowExponent
                           ||| NumberLiteralOptions.AllowHexadecimal
                           ||| NumberLiteralOptions.AllowSuffix
                           ||| NumberLiteralOptions.AllowBinary
                           ||| NumberLiteralOptions.AllowInfinity
                           ||| NumberLiteralOptions.AllowNaN

        let parser = numberLiteral numberFormat "number"

        let suffix_error (x: int) (s: CharStream<_>) = s.Skip(x); Reply(Error, messageError "invalid number suffix")
        let c2 x a b s =
            match a,b with
            | 'i','8' -> LitInt8 (int8 x) |> TValue |> Reply
            | 'u','8' -> LitUInt8 (uint8 x) |> TValue |> Reply
            | _ -> suffix_error -2 s


        let c3 x a b c (s: CharStream<_>) =
            match a,b,c with
            | 'u','1','6' -> LitUInt16 (uint16 x) |> TValue |> Reply
            | 'u','3','2' -> LitUInt32 (uint32 x) |> TValue |> Reply
            | 'u','6','4' -> LitUInt64 (uint64 x) |> TValue |> Reply
            | 'i','1','6' -> LitInt16 (int16 x) |> TValue |> Reply
            | 'i','3','2' -> LitInt32 (int32 x) |> TValue |> Reply
            | 'i','6','4' -> LitInt64 (int64 x) |> TValue |> Reply
            | 'f','3','2' -> LitFloat32 (float32 x) |> TValue |> Reply
            | 'f','6','4' -> LitFloat64 (float x) |> TValue |> Reply
            | _ -> suffix_error -3 s

        fun s ->
            let reply = parser s
            if reply.Status = Ok then
                let nl = reply.Result // the parsed NumberLiteral
                try 
                    if nl.SuffixLength = 2 then c2 nl.String nl.SuffixChar1 nl.SuffixChar2 s
                    elif nl.SuffixLength = 3 then c3 nl.String nl.SuffixChar1 nl.SuffixChar2 nl.SuffixChar3 s
                    elif nl.IsInteger then s.UserState.default_int nl.String |> TValue |> Reply
                    else s.UserState.default_float nl.String |> TValue |> Reply
                with
                | :? System.OverflowException as e ->
                    s.Skip(-nl.String.Length)
                    Reply(FatalError, messageError e.Message)
            else // reconstruct error reply
                Reply(reply.Status, reply.Error)

