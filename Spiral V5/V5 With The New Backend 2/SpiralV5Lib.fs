module Spiral.Lib
open Main

let tuple =
    (
    "Tuple",[],"Operations on tuples.",
    """
inl rec foldl f s = function
    | x :: xs -> foldl f (f s x) xs
    | () -> s
inl rec foldr f l s = 
    match l with
    | x :: xs -> f x (foldr f xs s)
    | () -> s

inl upon = foldl (inl module_ (k,v) -> upon module_ k v)
inl upon' = foldl (inl module_ (k,v) -> upon' module_ k v)

inl rev, map =
    inl map' f l = foldl (inl s x -> f x :: s) () l
    inl rev l = map' id l
    inl map f = map' f >> rev
    rev, map

inl rec forall f = function
    | x :: xs -> f x && forall f xs
    | () -> true

inl rec exists f = function
    | x :: xs -> f x || exists f xs
    | () -> false

inl filter f l ret =
    inl rec loop acc = function
        | x :: xs when f x -> loop (x :: acc) xs
        | x :: xs -> loop acc xs
        | () -> ret <| rev acc
    loop ()

inl is_empty = function
    | _ :: _ -> false
    | () -> true
    | _ -> error_type "Not a tuple."

inl is_tuple = function
    | _ :: _ -> true
    | _ -> false

inl transpose l on_fail on_succ =
    inl rec loop acc_total acc_head acc_tail l = 
        match l with
        | () :: ys ->
            match acc_head with
            | () when forall is_empty ys ->
                match acc_total with
                | _ :: _ -> rev acc_total |> on_succ
                | () -> error_type "Empty inputs in the inner dimension to transpose are invalid."
            | _ -> on_fail()
        | (x :: xs) :: ys -> loop acc_total (x :: acc_head) (xs :: acc_tail) ys
        | _ :: _ -> on_fail ()
        | () -> 
            match acc_tail with
            | _ :: _ -> loop (rev acc_head :: acc_total) () () (rev acc_tail)
            | () -> rev acc_total |> on_succ
    loop () () () l

inl zip_template on_ireg l = 
    inl rec zip = function // when forall is_tuple l 
        | _ :: _ as l -> transpose l (inl _ -> on_ireg l) (map (function | x :: () -> zip x | x -> x))
        | () -> error_type "Zip called on an empty tuple."
        | _ -> error_type "Zip called on a non-tuple."
    zip l

inl regularity_guard l =
    if forall is_empty l then l
    else error_type "Irregular inputs in unzip/zip."
inl zip = zip_template regularity_guard
inl zip' = zip_template id

inl rec unzip_template on_irreg l = 
    inl rec unzip = function
        | _ :: _ as l when forall is_tuple l -> transpose (map unzip l) (inl _ -> on_irreg l) id 
        | _ :: _ -> l
        | () -> error_type "Unzip called on an empty tuple."
        | _ -> error_type "Unzip called on a non-tuple."
    unzip l

inl unzip = unzip_template regularity_guard
inl unzip' = unzip_template id
inl index = tuple_index

inl init_template k n f =
    inl rec loop n = 
        match n with 
        | n when n > 0 -> 
            inl n = n - 1
            f n :: loop n
        | 0 -> ()
        | _ -> error_type "The input to this function cannot be static or less than 0 or not an int."
    loop n |> k

inl init = init_template rev
inl repeat n x = init_template id n (inl _ -> x)

module (foldl,foldr,rev,map,forall,exists,filter,is_empty,is_tuple,zip,unzip,index,upon,upon',init,repeat)
    """) |> module_

let parsing2 =
    (
    "Parsing",[tuple],"Parser combinators.",
    """
inl convert = mscorlib ."System.Convert"
inl to_int64 = convert .ToInt64

inl is_digit x = x >= '0' && x <= '9'
inl is_whitespace x = x = ' '
inl is_newline x = x = '\n' || x = '\r'

met rec List x =
    type
        .Cons, (x, List x)
        .Nil

inl pchar stream pos ret = 
    stream pos (upon' ret .on_succ <| inl _, c ->
        ret .on_succ (pos+1, c))

inl pchar_pos stream pos ret = 
    stream pos (upon' ret .on_succ <| inl _, c ->
        ret .on_succ (pos+1, (c, pos)))

inl pdigit stream pos ret =
    pchar stream pos (upon' ret .on_succ <| inl pos, (c: char) ->
        if is_digit c then ret .on_succ (pos, c)
        else ret .on_fail (pos, "digit"))

inl pint64 stream pos ret =
    met rec loop on_fail pos (!dyn i) = 
        pdigit stream pos <| Tuple.upon' ret (
            (.on_succ, inl pos, c ->
                inl x = to_int64 c - to_int64 '0'
                i * 10 + x |> loop (inl i (pos, _) -> ret .on_succ (pos, i)) pos
                ),
            (.on_fail, on_fail i)
            )
        : ret .on_type
            
    loop (inl _ (pos, _) -> ret .on_fail (pos, "int64")) pos 0

met rec spaces stream pos ret =
    pchar stream pos <| Tuple.upon' ret (
        (.on_succ, inl pos, c ->    
            if is_whitespace c || is_newline c then spaces stream pos ret
            else ret .on_succ (pos-1, ())
            ),
        (.on_fail, inl pos, _ -> ret .on_succ (pos, ()))
        )
    : ret .on_type

inl list_rev typ l = 
    met rec loop (!typ (!dyn acc)) l = 
        match l with
        | .Nil -> acc
        | .Cons, (x, xs) -> loop (.Cons, (x, acc)) xs
        : l
    loop .Nil l

inl many typ_p p stream pos ret =
    inl typ = List typ_p
    inl state = pos

    met rec many pos (!typ (!dyn r)) =
        p stream pos <| Tuple.upon' ret (
            (.on_succ, function
                | pos,_ when state = pos -> ret .on_fatal_fail (pos, "Many parser succeeded without changing the parser state. Unless the computation had been aborted, the parser would have gone into an infinite loop.")
                | pos,x -> many pos <| (.Cons, (x, r))
                )
            (.on_fail, function
                | pos,_ when state = pos -> ret .on_succ (pos, list_rev typ r)
                | pos,_ -> ret .on_fail (pos, "many")
                )
            )
        : ret .on_type

    many pos .Nil

inl tuple_template k l stream pos ret =
    inl rec loop pos acc = function
        | x :: xs -> 
            inl k = k (inl pos, x -> loop pos (x :: acc) xs)
            x stream pos <| upon' ret .on_succ k
        | () -> ret .on_succ (pos, Tuple.rev acc)
        | _ -> error_type "Incorrect input to tuple."
    loop pos () l

inl tuple = tuple_template id
inl tuple_chain typ_chain = tuple_template <| inl x -> x `(int64, typ_chain)

inl (>>=) a b stream pos ret = a stream pos <| upon' ret .on_succ (inl pos, x -> b x stream pos ret)
inl (|>>) a f = a >>= inl x stream pos ret -> ret .on_succ (pos, f x)

inl string_stream str pos ret =
    inl f pos = pos >= 0 && pos < string_length str
    match pos with
    | a, b when f a && f b | pos when f pos -> ret .on_succ (pos, str pos)
    | _ -> ret .on_fail (pos, "string index out of bounds")

inl run data parser ret = 
    match data with
    | _ : string -> parser (string_stream data) (if is_static data then 0 else dyn 0) ret
    | _ -> error_type "Only strings supported for now."

inl parse_int = tuple (pint64, spaces) |>> fst

inl parse_n_ints_template k n = Tuple.repeat n parse_int |> k
inl parse_n_ints = function
    | .no_clo, n | n when n <= 5 -> parse_n_ints_template tuple n
    | n when n > 5 -> parse_n_ints_template (tuple_chain int64) n
    | n : int64 -> type_error "The input to this function must be static."
    
inl parse_ints = many int64 parse_int
inl preturn x stream pos ret = ret .on_succ (pos, x)

inl with_unit_ret f = 
    inl on_succ pos, x = f x
    inl on_fail pos, x = ()
    inl on_fatal_fail pos, x = ()
    inl on_type = ()
    module (on_succ,on_fail,on_fatal_fail,on_type)

inl run_with_unit_ret data parser f = run data parser (with_unit_ret f)

inl sprintf_parser =
    inl rec sprintf_parser state append stream pos ret =
        inl parse_variable stream pos ret = 
            pchar stream pos <| Tuple.upon' ret (
                (.on_succ, inl pos, c -> 
                    match c with
                    | 's' -> function
                        | x : string -> x
                        | _ -> error_type "Expected a bool in sprintf."
                    | 'c' -> function
                        | x : char -> x
                        | _ -> error_type "Expected a bool in sprintf."
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
                    |> inl guard_type -> ret .on_succ (pos, inl x -> append (guard_type x); sprintf_parser .None append stream pos ret)
                    ),
                (.on_fail, inl pos, x ->
                    append '%'
                    ret .on_fail (pos, x)
                    )
                )
        inl append_state stream pos ret =
            match state with
            | .None -> ret .on_succ (pos, ())
            | ab -> stream ab (upon' ret .on_succ (inl _, r -> append r; ret .on_succ (pos, ())))

        pchar_pos stream pos <| Tuple.upon' ret (
            (.on_succ, function
                | pos, ('%', _) -> (append_state >>= inl _ -> parse_variable) stream pos ret
                | pos, char_pos -> 
                    inl state = 
                        match state with
                        | .None -> (char_pos, char_pos)
                        | (start,_) -> (start, char_pos)
                    sprintf_parser state append stream pos ret
                ),
            (.on_fail, inl pos, mes -> (append_state |>> inl _ -> ret .on_fail (pos, mes)) stream pos ret)
            )
    sprintf_parser .None

inl sprintf_template append on_succ on_fail format =
    run format (sprintf_parser append) (module(on_succ,on_fail))

inl sprintf format = 
    inl strb = mscorlib."System.Text.StringBuilder"(64i32)
    inl append x = strb.Append x |> ignore
    inl on_succ pos, x = x
    inl on_fail pos, x = strb.ToString()
    sprintf_template append on_succ on_fail format

module 
    (List,run,spaces,tuple,many,(>>=),(|>>),pint64,preturn,parse_int,parse_n_ints,parse_ints,run_with_unit_ret,sprintf,sprintf_template,
     tuple_chain)
    """) |> module_

let parsing3 =
    (
    "Parsing",[tuple],"Parser combinators.",
    """
inl convert = mscorlib ."System.Convert"
inl to_int64 = convert .ToInt64

inl is_digit x = x >= '0' && x <= '9'
inl is_whitespace x = x = ' '
inl is_newline x = x = '\n' || x = '\r'

inl pchar {d with stream {state with pos} ret} = 
    stream { 
        idx = pos
        on_succ = inl c -> on_succ {state with pos=pos+1} c
        on_fail = inl msg -> on_fail state msg
        }

inl pchar_pos {d with stream {state with pos} ret} = 
    stream { 
        idx = pos
        on_succ = inl c -> on_succ {state with pos=pos+1} (c, pos)
        on_fail = inl msg -> on_fail state msg
        }

inl pdigit {d with {ret with on_succ on_fail} =
    pchar { d.ret with
        on_succ = inl state c -> 
            if is_digit c then on_succ state c
            else on_fail state "digit"
        }

inl pint64 {d with state {ret with on_succ on_fail on_type} =
    met rec loop on_fail state (!dyn i) = 
        pdigit { d with
            state = state
            ret = { self with
                on_succ = inl state c ->
                    inl x = to_int64 c - to_int64 '0'
                    inl i = i * 10 + x 
                    loop (inl state _ -> on_succ state i) state i
                on_fail = on_fail
                }
        : on_type
            
    loop (inl state _ -> on_fail state "pint64") state 0

met rec spaces {d with state {ret with on_succ on_fail on_type} =
    pchar { d.ret with
        on_succ = inl state' c ->
            if is_whitespace c || is_newline c then spaces { d with state = state' }
            else on_succ state ()
        on_fail = inl state _ -> on_succ state ()
        }
    : on_type

inl many typ_p p {d with state {ret with on_succ on_fail on_fatal_fail on_type} =
    inl typ = List.list typ_p

    met rec many {state with pos} (!typ (!dyn r)) =
        p { d with
            state = state
            ret = {self with
                on_succ = inl {state with pos=pos'} -> function
                    | _ when pos = pos' -> on_fatal_fail state "Many parser succeeded without changing the parser state. Unless the computation had been aborted, the parser would have gone into an infinite loop."
                    | x -> many state <| (.Cons, (x, r))
                on_fail = inl {state with pos=pos'} -> function
                    | _ when pos = pos' -> on_succ state (List.rev typ r)
                    | _ -> on_fail state "many"
                }
            }
        : on_type

    many state .Nil

inl tuple l {d with state {ret with on_succ on_fail}} =
    inl rec loop state = function
        | x :: xs -> 
            x {d with 
                state = state
                ret = {self with on_succ = inl state x -> on_succ state (x :: loop state xs)}}
        | () -> on_succ state ()
        | _ -> error_type "Incorrect input to tuple."
    loop state () l

inl tuple_closure l {d with state {ret with on_succ on_fail}} =
    inl rec loop state = function
        | (x,typ) :: xs -> 
            x {d with 
                state = state
                ret = {self with on_succ = 
                    inl k = (inl state, x -> on_succ state (x :: loop state xs)) `(state,typ)
                    inl state x -> k (state,x)
                    }
                }
        | () -> on_succ state ()
        | _ -> error_type "Incorrect input to tuple_closure."
    loop state () l

inl (>>=) a b d = a {d with on_succ = inl state x -> b x d}
inl (|>>) a f = a >>= inl x {d with state {ret with on_succ}} -> on_succ state (f x)

inl string_stream str {idx on_succ on_fail} =
    inl f idx = idx >= 0 && idx < string_length str
    match idx with
    | a, b when f a && f b | idx when f idx -> on_succ (str idx)
    | _ -> on_fail "string index out of bounds"

inl run data parser ret = 
    match data with
    | _ : string -> parser {
        stream = string_stream data
        { state with pos = if is_static data then 0 else dyn 0 }
        ret = ret
        }
    | _ -> error_type "Only strings supported for now."

inl parse_int = tuple (pint64, spaces) |>> fst

inl parse_n_ints = function
    | .no_clo, n | n when n <= 5 -> Tuple.repeat n parse_int |> tuple
    | n when n > 5 -> Tuple.repeat (n,int64) parse_int |> tuple_chain
    | n : int64 -> type_error "The input to this function must be static."
    
inl parse_ints = many int64 parse_int
inl preturn x {state {ret with on_succ}} = on_succ state x

inl with_unit_ret f = {
    on_succ = inl state x -> f x
    on_fail = inl state x -> ()
    on_fatal_fail = inl state x -> ()
    on_type = ()
    }

inl run_with_unit_ret data parser f = run data parser (with_unit_ret f)

inl sprintf_parser =
    inl rec sprintf_parser sprintf_state append {d with {ret with on_succ on_fail}} =
        inl parse_variable d = 
            pchar { d.ret with
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
                    |> inl guard_type -> self state (inl x -> append (guard_type x); sprintf_parser .None append {d with state = state})
                    ),
                on_fail = inl state x ->
                    append '%'
                    self state x
                    )
                )
        inl append_state {d with stream state {ret with on_succ on_fail}} =
            match state with
            | .None -> on_succ state ()
            | ab -> stream {
                idx = ab
                on_succ = inl r -> append r; on_succ state ()
                on_fail = inl msg -> on_fail state msg
                }

        pchar_pos { d.ret with
            on_succ = inl state -> function
                | '%', _ -> (append_state >>= inl _ -> parse_variable) {d with state = state}
                | _, pos ->
                    inl sprintf_state = 
                        match sprintf_state with
                        | .None -> (pos, pos)
                        | (start,_) -> (start, pos)
                    sprintf_parser sprintf_state append {d with state = state}
                ),
            on_fail = inl state mes -> (append_state |>> inl _ -> on_fail state mes) {d with state = state}
            )
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
    (List,run,spaces,tuple,many,(>>=),(|>>),pint64,preturn,parse_int,parse_n_ints,parse_ints,run_with_unit_ret,sprintf,sprintf_template,
     tuple_chain)
    """) |> module_


let console =
    (
    "Console",[parsing2],"IO printing functions.",
    """
inl console = mscorlib."System.Console"
inl readline () = console.ReadLine()
inl write = console.Write
inl writeline = console.WriteLine

inl printf_template cont = 
    Parsing.sprintf_template write {
        on_succ = inl state x -> x
        on_fail = inl state msg -> cont()
        }

inl printf = printf_template id
inl printfn = printf_template writeline
module (console,readline,write,writeline,printf,printfn)
    """) |> module_