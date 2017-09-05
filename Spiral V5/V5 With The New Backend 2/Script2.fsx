
inl sprintf_parser append =
    inl rec sprintf_parser sprintf_state =
        inl parse_variable = 
            inm c = try_with stream_char (inl x -> append '%'; fail "done" x)
            match c with
            | 's' -> function
                | x : string -> x
                | _ -> error_type "Expected a string in sprintf."
            | 'c' -> function
                | x : char -> x
                | _ -> error_type "Expected a char in sprintf."
            | 'b' -> function
                | x : bool -> x
                | _ -> error_type "Expected a bool in sprintf."
            | 'i' -> function
                | x : int32 | x : int64 | x : uint32 | x : uint64 -> x
                | _ -> error_type "Expected an integer in sprintf."
            | 'f' -> function
                | x : float32 | x : float64 -> x
                | _ -> error_type "Expected a float in sprintf."
            | 'A' -> id
            | _ -> error_type "Unexpected literal in sprintf."
            |> inl guard_type -> 
                inm state, d = state_d
                succ (inl x -> append (guard_type x); sprintf_parser .None state d)

        inl append_state state {d with stream {ret with on_succ on_fail}} =
            match sprintf_state with
            | .None -> on_succ state ()
            | ab -> stream {
                idx = ab
                on_succ = inl r -> append r; on_succ state ()
                on_fail = inl msg -> on_fail state msg
                }

        inm c = try_with stream_char_pos (append_state >>. fail "done")
        match c with
        | '%', _ -> append_state >>. parse_variable
        | _, pos ->
            match sprintf_state with
            | .None -> (pos, pos)
            | (start,_) -> (start, pos)
            |> sprintf_parser
    sprintf_parser .None

