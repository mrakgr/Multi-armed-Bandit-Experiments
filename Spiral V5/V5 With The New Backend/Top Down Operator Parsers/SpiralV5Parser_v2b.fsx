﻿// 5/23/2017:

// I intended for this to replace the 2a version of the parser, but it would be too much work
// to make the error messages as good as what Fparsec already has. What I will do is pull out
// the number parser into 2a and make a top down Pratt operator parser in 2a to replace the
// current broken one that comes with FParsec.

// At some point it might be good to write my own completely from scratch, maybe even in Spiral
// itself so I will leave this here.

// Right now I want to get back to work on the typechecker. I want to rewrite in CPS so I can
// get much better active patterns, and I want to put in some partial evaluation as well.

// When I am done with the typechecker's core functionality, eventually I am going to have to deal 
// with the horrible state of error messages in it and it will be time to focus more on the user 
// experience. That is when I will pick up from where I left off here.

// I am going to have to study how Fparsec library deals with propagating errors specifically.



#load "../SpiralV5Language_v8b.fsx"
#r "../../../packages/FParsec.1.0.2/lib/net40-client/FParsecCS.dll"
#r "../../../packages/FParsec.1.0.2/lib/net40-client/FParsec.dll"

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
    mutable token_idx: int64
    tokens: Token[]
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

module Parser =
    type ParserError = int64 * int64 * string
    let no_er = 0L,0L,""
    let s0 x = 
        {
        default_int = fun x -> LitInt64 (int64 x)
        default_float = fun x -> LitFloat32 (float32 x)
        token_idx=0L
        tokens=x
        }

    let array_idx (i: int64) (x : 'a []) = x.GetValue(i) :?> 'a
    let idx (s: ParserState) = s.token_idx
    let set x (s: ParserState) = s.token_idx <- x
    let modify f (s: ParserState) = s.token_idx <- f s.token_idx
    
    let peek (s: ParserState) = array_idx (idx s) s.tokens
    let advance (s: ParserState) = s.token_idx <- s.token_idx + 1L
        
    /// Returns the token and advances the index.
    let token (s: ParserState) = 
        let t = peek s
        advance s
        t

    let merge_error (_,_,y_er as y) x = 
        if y_er <> "" then y :: x else x
    let attempt f (s: ParserState) on_fail ret = 
        let t = idx s
        f s (fun er -> 
            set t s
            on_fail er)
            ret

    let choice l s on_fail ret =
        let rec loop l on_fail =
            match l with
            | x :: xs -> x s (fun er -> loop xs (merge_error er)) ret
            | [] -> on_fail []
        loop l on_fail

    let many p s on_fail ret =
        let t = idx s
        let rec loop acc =
            p s (fun _ -> List.rev acc |> ret)
                (fun r -> 
                    let t' = idx s
                    if t = t' then failwith "A parser succeeds without changing state in many.\n(It would have gone into an infinite loop had an exception not been raised.)"
                    else loop (r :: acc))
        loop []

    let one_of p s on_fail ret =
        p s on_fail (fun r ->
            if List.isEmpty r then on_fail "one of _"
            else ret r)

    let many1 p s on_fail ret = one_of (many p) s on_fail ret

    let (.>>) a b s on_fail ret =
        a s on_fail (fun r -> b s on_fail (fun _ -> ret r))

    let (>>.) a b s on_fail ret =
        a s on_fail (fun _ -> b s on_fail ret)

    let sepBy p sep s on_fail ret =
        let t = idx s
        let on_fail acc _ = List.rev acc |> ret
        p s (on_fail [])
            (fun r ->
            let rec loop acc =
                (sep >>. p) s (on_fail acc)
                    (fun r -> 
                        let t' = idx s
                        if t = t' then failwith "A parser succeeds without changing state in sepBy.\n(It would have gone into an infinite loop had an exception not been raised.)"
                        else loop (r :: acc))
            loop [r]
            )

    let sepBy1 p sep s on_fail ret =
        one_of (sepBy p sep) s on_fail ret

    let parser (builtin_ops: string -> Op) s on_fail ret =
        ()