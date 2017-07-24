type ParserState =
    {
    pos : int64
    stream : string
    }

type ParserResult<'a> =
    | Succ of 'a
    | Fail of int64 * string
    | FatalFail of string

let rec many_cps p s ret =
    let state = s.pos
    p s <| function
        | Succ x when state < s.pos ->
            many_cps p s <| function
                | Succ xs -> ret (Succ (x :: xs))
                | x -> ret x
        | Succ _ when state = s.pos -> ret (FatalFail "Many parser succeeded without changing the parser state. Unless the computation had been aborted, the parser would have gone into an infinite loop.")
        | Fail _ when state = s.pos -> ret (Succ [])
        | Fail _ -> ret (Fail (s.pos, "many"))
        | FatalFail x -> ret (FatalFail x)