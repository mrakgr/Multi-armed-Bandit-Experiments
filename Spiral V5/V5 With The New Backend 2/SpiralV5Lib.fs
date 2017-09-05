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

inl singleton x = x :: ()
inl append = foldr (::)

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

module (foldl,foldr,rev,map,forall,exists,filter,is_empty,is_tuple,zip,unzip,index,upon,upon',init,repeat,append,singleton)
    """) |> module_

let array =
    (
    "Array",[],"The array module",
    """
inl foldl f s ar =
    met rec loop (!dyn i) s =
        if i < array_length ar then loop (i+1) (f s (ar i))
        else s
        : s
    loop 0 s

inl init n f =
    assert (n > 0) "n > 0"
    inl ar = array_create n (type (f 0))
    met rec loop (!dyn i) =
        if i < n then ar i <- f i
        else ()
    loop 0
    ar

module (foldl,init)
    """) |> module_

let queue =
    (
    "Queue",[tuple],"The queue module.",
    """
inl queue = mscorlib ."System.Collections.Generic.Queue"

inl rec create = function
    | _ :: _ as x -> Tuple.map create x
    | x -> 
        inl q = queue x // Type application
        q.Enqueue x // Adds the first element
        q

inl enqueue q x =
    inl rec loop = function
        | x :: xs, x' :: xs' -> loop (x,x'); loop (xs,xs')
        | x,x' -> x.Enqueue x'
    loop (Tuple.zip (q,x))

inl rec dequeue = function
    | _ :: _ as x -> Tuple.map dequeue x
    | x -> x.Dequeue()
    """) |> module_

let parsing4 =
    (
    "Parsing",[tuple],"Parser combinators.",
    """
// Primitives
inl goto point x state _ = point state x
inl succ x state {{ret with on_succ}} = on_succ state x
inl fail x state {{ret with on_fail}} = on_fail state x
inl fatal_fail x state {{ret with on_fatal_fail}} = on_fatal_fail state x
inl type_ state {d.ret with on_succ on_type} = on_succ state on_type
inl state state {{ret with on_succ}} = on_succ state state
inl set_state state _ {{ret with on_succ}} = on_succ state ()
inl (>>=) a b state d = a state {d.ret with on_succ = inl state x -> b x state d}
inl try_with handle handler state d = handle state {d.ret with on_fail = inl state _ -> handler state d}
inl guard cond handler state d = if cond then d.ret.on_succ state () else handler state d
inl ifm cond tr fl state d = if cond then tr () state d else fl () state d
inl attempt a state d = a state { d.ret with on_fail = inl _ -> self state}

inl rec tuple = function
    | () -> succ ()
    | x :: xs ->
        inm x = x
        inm xs = tuple xs
        succ (x :: xs)
    

inl (|>>) a f = a >>= inl x -> succ (f x)
inl (.>>.) a b = tuple (a,b)
inl (.>>) a b = tuple (a,b) |>> fst
inl (>>.) a b = a >>= inl _ -> b // The way bind is used here in on purpose. `spaces` diverges otherwise.
inl (>>%) a b = a |>> inl _ -> b

// TODO: Instead of just passing the old state on failure to the next parser, the parser should
// compare states and fail if the state changed. Right now that cannot be done because Spiral is missing
// polymorphic structural equality on all but primitive types. I want to be able to structurally compare anything.
inl (<|>) a b = try_with (attempt a) b

// CharParsers
inl convert = mscorlib ."System.Convert"
inl to_int64 = convert .ToInt64

inl is_digit x = x >= '0' && x <= '9'
inl is_whitespace x = x = ' '
inl is_newline x = x = '\n' || x = '\r'

inl string_stream str {idx on_succ on_fail} =
    inl f idx = idx >= 0 && idx < string_length str
    match idx with
    | a, b when f a && f b | idx when f idx -> on_succ (str idx)
    | _ -> on_fail "string index out of bounds"

inl stream_char {state with pos} {d with stream {ret with on_succ on_fail}} = 
    stream {
        idx = pos
        on_succ = inl c -> on_succ {state with pos=pos+1} c
        on_fail = inl msg -> on_fail state msg
        }

inl stream_char_pos =
    inm {pos} = state
    stream_char |>> inl x -> x,pos

inl satisfyL f m =
    inm s = state
    inm c = stream_char
    inm _ = guard (f c) (set_state s >>. fail m)
    succ c

inl (<?>) a m = try_with a (fail m)
inl pdigit = satisfyL is_digit "digit"
inl pchar c = satisfyL ((=) c) "char"

inl pstring (!dyn str) x =
    met rec loop (!dyn i) state d =
        inl f =
            ifm (i < string_length str)
            <| inl _ -> pchar (str i) >>. loop (i+1)
            <| inl _ -> succ str
        f state d
        : d.ret.on_type
    loop 0 x

inl pint64 =
    met rec loop handler i state {d.ret with on_succ on_type} =
        inl f =
            inm c = try_with pdigit handler
            inl x = to_int64 c - to_int64 '0'
            inl max = 922337203685477580 // max int64 divided by 10
            inm _ = guard (i = max && x <= 7 || i < max) (fail "integer overflow")
            inl i = i * 10 + x
            loop (goto on_succ i) i
        f state d : on_type
    loop (fail "pint64") 0

inl spaces x =
    met rec loop (!dyn i) state {d.ret with on_succ on_type} =
        inl f = try_with (satisfyL (inl c -> is_whitespace c || is_newline c) "space") (goto on_succ i) >>. loop (i+1)
        f state d : on_type
    loop 0 x

inl run data parser ret = 
    match data with
    | _ : string -> 
        parser { pos = if is_static data then 0 else dyn 0 }
            { stream = string_stream data; ret = ret }
    | _ -> error_type "Only strings supported for now."

inl parse_int =
    inm !dyn m = try_with (pchar '-' >>. succ false) (succ true)
    (pint64 |>> inl x -> if m then x else -x) .>> spaces

inl parse_n_array p n x =
    inl f =
        inm _ = guard (n > 0) (fatal_fail "n in parse array must be > 0")
        inl ar = array_create n x
        ar 0 <- x
        met rec loop (!dyn i) state d =
            ifm (i < n)
            <| inl _ ->
                inm x = p
                ar i <- x
                loop (i+1)
            <| inl _ -> 
                succ ar
            <| state <| d
            : d.ret.on_type
        loop 1
    f x
        
inl with_unit_ret = {
    on_type = ()
    on_succ = inl state x -> ()
    on_fail = inl state -> failwith
    on_fatal_fail = inl state -> failwith
    }

inl run_with_unit_ret data parser = run data parser with_unit_ret

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
                inl state d -> d.ret.on_succ state (inl x -> append x; sprintf_parser .None state d)

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
    (run,spaces,tuple,(>>=),(|>>),pchar,pdigit,pint64,pstring,succ,fail,fatal_fail,type_,state,parse_int,
     run_with_unit_ret,sprintf,sprintf_template,parse_n_array,(<|>),attempt,(>>.),(.>>),try_with,guard,(>>%))
    """) |> module_


let console =
    (
    "Console",[parsing4],"IO printing functions.",
    """
inl console = mscorlib."System.Console"
inl readall () = console.OpenStandardInput() |> mscorlib ."System.IO.StreamReader" |> inl x -> x.ReadToEnd()
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
module (console,readall,readline,write,writeline,printf,printfn)
    """) |> module_