﻿module Spiral.Lib
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

let parsing3 =
    (
    "Parsing",[tuple],"Parser combinators.",
    """
inl convert = mscorlib ."System.Convert"
inl to_int64 = convert .ToInt64

inl is_digit x = x >= '0' && x <= '9'
inl is_whitespace x = x = ' '
inl is_newline x = x = '\n' || x = '\r'

inl pchar_pos {d with stream {state with pos} {ret with on_succ on_fail}} = 
    stream {
        idx = pos
        on_succ = inl c -> on_succ {state with pos=pos+1} (c, pos)
        on_fail = inl msg -> on_fail state msg
        }

inl pchar {d with stream {state with pos} {ret with on_succ on_fail}} = 
    stream {
        idx = pos
        on_succ = inl c -> on_succ {state with pos=pos+1} c
        on_fail = inl msg -> on_fail state msg
        }

//inl pdigit {d.ret with on_succ on_fail} =
//    pchar { d.ret with
//        on_succ = inl state c -> 
//            if is_digit c then on_succ state c
//            else on_fail state "digit"
//        }

inl pdigit {d with stream {state with pos} {ret with on_succ on_fail}} = 
    stream {
        idx = pos
        on_succ = inl c ->
            inl state = {state with pos=pos+1}
            met f c = 
                if is_digit c then on_succ state c
                else on_fail state "digit"
            print_static f
            f c
        on_fail = inl msg -> on_fail state msg
        }

//inl pint64 {d with state {ret with on_succ on_fail on_type}} =
//    met rec loop (!dyn fail_state) state (!dyn i) = 
//        pdigit { d with
//            state = state
//            ret = { self with
//                on_succ = inl state c ->
//                    print_static "I am in on_succ."
//                    inl x = to_int64 c - to_int64 '0'
//                    inl i = i * 10 + x 
//                    loop false state i
//                on_fail = inl state _ ->
//                    if fail_state then on_fail state "pint64"
//                    else on_succ state i
//                }
//            }
//        : on_type
//            
//    loop true state 0

inl pint64 {d with state {ret with on_succ on_fail on_type}} =
    met rec loop on_fail {state with pos} i = 
        pdigit { d with
            state = state
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

met rec spaces {d with state {ret with on_succ on_fail on_type}} =
    pchar { d.ret with
        on_succ = inl state' c ->
            if is_whitespace c || is_newline c then spaces { d with state = state' }
            else on_succ state ()
        on_fail = inl state _ -> on_succ state ()
        }
    : on_type

inl many typ_p p {d with state {ret with on_succ on_fail on_fatal_fail on_type}} =
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
    inl rec loop state l ret = 
        match l with
        | x :: xs -> 
            x {d with 
                state = state
                ret = {self with on_succ = inl state x -> 
                    loop state xs <| inl state xs -> 
                        ret state (x :: xs) }}
        | () -> ret state ()
        | _ -> error_type "Incorrect input to tuple."
    loop state l on_succ

inl tuple_chain l {d with state {ret with on_succ on_fail}} =
    inl rec loop state l ret = 
        match l with
        | (x,t) :: xs -> 
            inl k = (inl state, x -> loop state xs <| inl state xs -> ret state (x :: xs)) `(state,t)
            x {d with 
                state = state
                ret = {self with on_succ = inl state x -> k (state,x)}
                }
        | () -> ret state ()
        | _ -> error_type "Incorrect input to tuple."
    loop state l on_succ

inl (>>=) a b {d with stream state ret} = // The bug was in bind. TODO: Add remove to the language.
    a {d.ret with on_succ = inl state x -> b x {stream=stream; state=state; ret=ret}}
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
        state = { pos = if is_static data then 0 else dyn 0 }
        ret = ret
        }
    | _ -> error_type "Only strings supported for now."

inl parse_int = tuple (pint64, spaces) |>> fst

inl parse_n_ints = function
    | .no_clo, n | n when n <= 5 -> Tuple.repeat n parse_int |> tuple
    | n when n > 5 -> Tuple.repeat n (parse_int,int64) |> tuple_chain
    | n : int64 -> type_error "The input to this function must be static."
    
inl parse_ints = many int64 parse_int
inl preturn x {state {ret with on_succ}} = on_succ state x

inl with_unit_ret f = {
    on_type = ()
    on_succ = inl state x -> f x
    on_fail = inl state x -> ()
    on_fatal_fail = inl state x -> ()
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
               
                on_fail = inl state x ->
                    append '%'
                    self state x
                }
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
            on_fail = inl state mes -> (append_state |>> inl _ -> on_fail state mes) {d with state = state}
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
    (run,spaces,tuple,many,(>>=),(|>>),pchar,pdigit,pint64,preturn,parse_int,parse_n_ints,parse_ints,run_with_unit_ret,sprintf,sprintf_template
     )
    """) |> module_


let console =
    (
    "Console",[parsing3],"IO printing functions.",
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