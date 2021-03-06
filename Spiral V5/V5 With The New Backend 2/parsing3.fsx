﻿let parsing3 =
    (
    "Parsing",[tuple],"Parser combinators.",
    """
// Primitives
inl tuple_template flag_term_cast l state {d with {ret with on_succ}} =
    inl rec loop state l ret = 
        match l with
        | x :: xs -> 
            inl x, k = 
                match flag_term_cast,x with
                | true,(x,t) -> 
                    inl k = (inl state, x -> loop state xs <| inl state xs -> ret state (x :: xs)) `(state,t)
                    x, inl state x -> k (state,x)
                | _, x ->
                    x, inl state x -> loop state xs <| inl state xs -> ret state (x :: xs)
            x state {d.ret with on_succ = k}
        | () -> ret state ()
        | _ -> error_type "Incorrect input to tuple."
    loop state l on_succ

inl tuple = tuple_template false
inl tuple_chain = tuple_template true

inl (>>=) a b state d = a state {d.ret with on_succ = inl state x -> b x state d}
inl (|>>) a f = a >>= inl x state {d with {ret with on_succ}} -> on_succ state (f x)
inl (.>>) a b = tuple (a,b) |>> fst
inl (>>.) a b = tuple (a,b) |>> snd

// TODO: Instead of just passing the old state on failure to the next parser, the parser should
// compare states and fail if the state changed. Right now that cannot be done because Spiral is missing
// polymorphic structural equality on all but primitive types. I want to be able to structurally compare anything.
inl (<|>) a b state {d.ret with on_succ on_fail} = a state { d.ret with on_fail = inl _ _ -> b state d }

inl attempt a state d = a state { d.ret with on_fail = inl _ -> self state}

// CharParsers
inl convert = mscorlib ."System.Convert"
inl to_int64 = convert .ToInt64

inl is_digit x = x >= '0' && x <= '9'
inl is_whitespace x = x = ' '
inl is_newline x = x = '\n' || x = '\r'

inl pchar_pos {state with pos} {d with stream {ret with on_succ on_fail}} = 
    stream {
        idx = pos
        on_succ = inl c -> on_succ {state with pos=pos+1} (c, pos)
        on_fail = inl msg -> on_fail state msg
        }

inl pchar {state with pos} {d with stream {ret with on_succ on_fail}} = 
    stream {
        idx = pos
        on_succ = inl c -> on_succ {state with pos=pos+1} c
        on_fail = inl msg -> on_fail state msg
        }

inl pdigit state {d.ret with on_succ on_fail} =
    pchar state { d.ret with
        on_succ = inl state c -> 
            if is_digit c then on_succ state c
            else on_fail state "digit"
        }

inl skipString str =
    met rec loop (!dyn i) state {d.ret with on_succ} =
        if i < string_length str then
            (pchar >>= inl c state {d.ret with on_fail} ->
                if c = str i then loop (i+1) state d
                else on_fail state str)
                state d
        else
            on_succ state ()
    loop 0

inl skipChar c =
    pchar >>= inl c' state {d.ret with on_succ on_fail} ->
        if c = c' then on_succ state ()
        else on_fail state "skipChar"

inl pint64 state {d with {ret with on_succ on_fail on_type}} =
    met rec loop on_fail state i = 
        pdigit state { d with
            ret = { self with
                on_succ = inl state c ->
                    inl x = to_int64 c - to_int64 '0'
                    inl i = i * 10 + x 
                    loop (inl state _ -> on_succ state i) state i
                on_fail = on_fail
                }
            }
        : on_type
            
    loop (inl state _ -> on_fail state "pint64") state 0

//inl pint64 state {d with {ret with on_succ on_fail on_type}} =
//    met rec loop (!dyn fail_state) state (!dyn i) = 
//        pdigit state { d with
//            ret = { self with
//                on_succ = inl state c ->
//                    inl x = to_int64 c - to_int64 '0'
//                    inl i = i * 10 + x 
//                    loop false state i
//                on_fail = inl state _ ->
//                    match fail_state with
//                    | true -> on_fail state "pint64"
//                    | _ -> on_succ state i
//                }
//            }
//        : on_type
//            
//    loop true state 0

met rec spaces state {d with {ret with on_succ on_fail on_type}} =
    pchar state { d.ret with
        on_succ = inl state' c ->
            if is_whitespace c || is_newline c then spaces state' d
            else on_succ state ()
        on_fail = inl state _ -> on_succ state ()
        }
    : on_type

inl string_stream str {idx on_succ on_fail} =
    inl f idx = idx >= 0 && idx < string_length str
    match idx with
    | a, b when f a && f b | idx when f idx -> on_succ (str idx)
    | _ -> on_fail "string index out of bounds"

inl run data parser ret = 
    match data with
    | _ : string -> 
        parser { pos = if is_static data then 0 else dyn 0 }
            { stream = string_stream data; ret = ret }
    | _ -> error_type "Only strings supported for now."

inl parse_int = ((skipChar '-' >>. pint64 |>> negate) <|> pint64) .>> spaces

inl parse_n_array p n state {d.ret with on_fatal_fail} =
    if n > 0 then
        (p >>= inl x ->
            inl ar = array_create n x
            ar 0 <- x
            met rec loop (!dyn i) state {d.ret with on_succ on_type} =
                if i < n then 
                    (p >>= inl x ->
                        ar i <- x
                        loop (i+1)
                        ) state d
                else on_succ state ar
                : on_type
            loop 1) state d
    else
        on_fatal_fail state "n in parse array must be > 0."

inl parse_n_ints = function
    | .no_clo, n | n when n <= 5 -> Tuple.repeat n parse_int |> tuple
    | n when n > 5 -> Tuple.repeat n (parse_int,int64) |> tuple_chain
    | n : int64 -> type_error "The input to this function must be static."
    
inl preturn x state {{ret with on_succ}} = on_succ state x

inl with_unit_ret = {
    on_type = ()
    on_succ = inl state x -> ()
    on_fail = inl state -> failwith
    on_fatal_fail = inl state -> failwith
    }

inl run_with_unit_ret data parser = run data parser with_unit_ret

inl sprintf_parser =
    inl rec sprintf_parser sprintf_state append state {d with {ret with on_succ on_fail}} =
        inl parse_variable state d = 
            pchar state { d.ret with
                on_succ = inl state c -> 
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
                    |> inl guard_type -> self state (inl x -> append (guard_type x); sprintf_parser .None append state d)
               
                on_fail = inl state x ->
                    append '%'
                    self state x
                }
        inl append_state state {d with stream {ret with on_succ on_fail}} =
            match sprintf_state with
            | .None -> on_succ state ()
            | ab -> stream {
                idx = ab
                on_succ = inl r -> append r; on_succ state ()
                on_fail = inl msg -> on_fail state msg
                }

        pchar_pos state { d.ret with
            on_succ = inl state -> function
                | '%', _ -> (append_state >>= inl _ -> parse_variable) state d
                | _, pos ->
                    inl sprintf_state = 
                        match sprintf_state with
                        | .None -> (pos, pos)
                        | (start,_) -> (start, pos)
                    sprintf_parser sprintf_state append state d
            on_fail = inl state mes -> (append_state >>= inl _ state _ -> on_fail state mes) state d
            }
    sprintf_parser .None

inl sprintf_template append ret format =
    run format (sprintf_parser append) ret

inl sprintf format = 
    inl strb = mscorlib."System.Text.StringBuilder"(64i32)
    inl append x = strb.Append x |> ignore
    sprintf_template append {
        on_succ = inl state x -> x
        on_fail = inl state msg -> strb.ToString()
        } format

module 
    (run,spaces,tuple,(>>=),(|>>),pchar,pdigit,pint64,preturn,parse_int,parse_n_ints,run_with_unit_ret,sprintf,sprintf_template,
     parse_n_array,(<|>),attempt,(>>.),(.>>))
    """) |> module_
