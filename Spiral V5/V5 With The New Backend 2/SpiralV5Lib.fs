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
inl empty t = array_create 0 t
inl singleton x =
    inl ar = array_create 1 x
    ar 0 <- x
    ar

inl foldl f s ar =
    met rec loop (!dyn i) s =
        if i < array_length ar then loop (i+1) (f s (ar i))
        else s
        : s
    loop 0 s

inl init n f =
    assert (n >= 0) "The input to init needs to be greater or equal than 0."
    type typ = f 0
    inl ar = array_create n typ
    met rec loop (!dyn i) =
        if i < n then (ar i <- f i); loop (i+1)
        : ()
    loop 0 |> ignore
    ar

inl map f ar = init (array_length ar) (ar >> f)
inl filter f ar =
    inl count = foldl (inl s x -> if f x then s+1 else s) 0 ar
    inl filtered = array_create (array_length ar) (array_type ar)
    foldl (inl s x ->
        if f x then (filtered s <- x); s + 1
        else s
        ) 0 ar |> ignore
    filtered

inl concat ar =
    inl count = foldl (inl s ar -> s + array_length ar) 0 ar
    inl ar' = array_create (t.elem_type.elem_type)
    foldl (foldl <| inl i x -> (ar' i <- x); i+1) 0 ar |> ignore
    ar'

module (empty,singleton,foldl,init,map,filter,concat)
    """) |> module_

let typec =
    (
    "TypeC",[],"The type constructor module",
    """
inl chain a f on_fail on_succ =
    inl x = typec_map f a
    match eq_type x typec_error with
    | true -> on_fail()
    | _ -> on_succ x

module(chain)
    """) |> module_

let list =
    (
    "List",[tuple;typec],"The queue module.",
    """
type list x =
    ()
    x, list x

inl is x = 
    TypeC.chain (typec_split x) <| function
        | (),(a,b) | (a,b),() when eq_type (list a) x -> x
        | _ -> typec_error

inl elem_type x =
    TypeC.chain (typec_split x) <| function
        | @is ((),(a,b) | (a,b),()) -> a
        | _ -> typec_error

inl lw x on_fail on_succ = 
    inl rec loop tup_type n x on_fail on_succ =
        if n > 0 then
            match x with
            | () -> on_fail()
            | a, b -> loop (n-1) b on_fail <| inl b -> on_succ (a :: b)
        else
            match tup_type with
            | .tup ->
                match x with
                | () -> on_succ()
                | _ -> on_fail()
            | .cons -> on_succ x
        loop n x on_succ

    match x with
    | .var, x & @is _ -> on_succ x
    | (.tup | .cons) & typ, n, x & @is _ -> loop typ n x on_fail on_succ

inl empty x = list x ()
inl singleton x = list x (x, empty x)
inl cons a b = list a (a, list a b)

inl init n f =
    inl t = typec (f 0)
    met rec loop !dyn i =
        if i < n then cons (f i) (loop (i+1))
        else empty t
        : list t
    loop 0

met rec map f = function
    | #lw (x :: xs) -> cons (f x) (map f xs) : list x
    | @elem_type t -> empty t : list t

met rec foldl f s l = 
    match l with
    | #lw (x :: xs) -> foldl (f s x) xs
    | #lw () -> s
    : s

met rec foldr f l s = 
    match l with
    | #lw (x :: xs) -> f x (foldr f xs s)
    | #lw () -> s
    : s

inl append a b = 
    match a,b with 
    | _,#lw () -> a 
    | #lw (),_ -> b
    | _ -> foldlr cons a b
inl concat l & @elem_type t = foldr append l (empty t)

module (list,lw,init,map,foldl,foldr,empty,cons,singleton,append,concat)
    """) |> module_


let queue =
    (
    "Queue",[tuple],"The queue module.",
    """
// I started this because I realized I cannot pass Spiral's inbuilt tuples as generic types to the .NET side.
// This queue uses a tuple of queues representation and serves as an example of how this might be done with 
// arrays, in order to go from array of structs to struct of array representantion.

// Unfortuantely, the queue can't take Spiral's inbuilt union types either, so it is not particularly useful.
// I will have to either build all the essential datatypes directly into the language, or preferably implement them in it
// so that I can apply the full power of partial evaluation to them.

// This module is a of yet, untested.
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

let parsing =
    (
    "Parsing",[tuple],"Parser combinators.",
    """
// Primitives
inl m x parser = { elem = parser; elem_type = typec x}
inl goto point x = m () <| inl _ state -> point state x
inl succ x = m x <| inl {on_succ} state -> on_succ state x
inl fail () = m x <| inl {on_fail} state -> on_fail state x
inl fatal_fail () = m x <| inl {on_fatal_fail} state -> on_fatal_fail state x
inl type_ = m on_type <| inl {on_succ on_type} state -> on_succ state on_type
inl state = m state <| inl {on_succ} state -> on_succ state state
inl set_state state = m () <| inl {ret with on_succ} _ -> on_succ state ()
inl (>>=) a b = {
    elem = inl d state -> a .elem {d with on_succ = inl state x -> b x .elem d state} state
    elem_type = typec_map (inl a -> b a .elem_type) (a.elem_type)
    }
inl try_with handle handler = m (handle.elem_type) <| inl d state -> handle .elem {d with on_fail = inl state _ -> handler .elem d state} state
inl guard cond handler = m () <| inl d state -> if cond then d .on_succ state () else handler .elem d state
inl ifm cond tr fl = m (union (tr.elem_type) (fl.elem_type)) <| inl d state -> if cond then tr () .elem d state else fl () .elem d state
inl attempt a = m (a.elem_type) <| inl d state -> a { d with on_fail = inl _ -> self state} state

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

// Though to be fair, in all the times I've used `choice`, I don't think there has been a single time it was without `attempt`.
// Unlike with Fparsec, backing up the state in Spiral is essentially a no-op due to inlining.
inl (<|>) a b = try_with (attempt a) b
inl choice = function
    | x :: xs -> Tuple.foldl (<|>) x xs
    | () -> error_type "choice require at lease one parser as input"

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

inl stream_char = m char <| inl {d with stream on_succ on_fail} {state with pos} ->
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
    met rec loop (!dyn i) = m string <| inl d state ->
        inl f =
            ifm (i < string_length str)
            <| inl _ -> pchar (str i) >>. loop (i+1)
            <| inl _ -> succ str
        f d state
        : d.on_type
    loop 0 x

inl pint64 =
    met rec loop handler i = m int64 <| {d with on_succ on_type} state ->
        inl f =
            inm c = try_with pdigit handler
            inl x = to_int64 c - to_int64 '0'
            inl max = 922337203685477580 // max int64 divided by 10
            inm _ = guard (i = max && x <= 7 || i < max) (fail "integer overflow")
            inl i = i * 10 + x
            loop (goto on_succ i) i
        f d state : on_type
    loop (fail "pint64") 0

inl spaces x =
    met rec loop (!dyn i) = m () <| inl {d with on_succ on_type} state ->
        inl f = try_with (satisfyL (inl c -> is_whitespace c || is_newline c) "space") (goto on_succ i) >>. loop (i+1)
        f d state : on_type
    loop 0 x

inl run data parser ret = 
    match data with
    | _ : string -> 
        parser .elem { ret with stream = string_stream data}
            { pos = if is_static data then 0 else dyn 0 }
    | _ -> error_type "Only strings supported for now."

inl parse_int =
    inm !dyn m = try_with (pchar '-' >>. succ false) (succ true)
    (pint64 |>> inl x -> if m then x else -x) .>> spaces

inl parse_n_array p n x =
    inl f =
        inm _ = guard (n > 0) (fatal_fail "n in parse array must be > 0")
        inl ar = array_create n (p.elem_type)
        met rec loop (!dyn i) = m ar <| inl d state ->
            ifm (i < n)
            <| inl _ ->
                inm x = p
                ar i <- x
                loop (i+1)
            <| inl _ -> 
                succ ar
            <| state <| d
            : d.on_type
        loop 0
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
                m () <| inl d state -> d.on_succ state (inl x -> append x; sprintf_parser .None d state)

        inl append_state {d with stream on_succ on_fail} state =
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
     run_with_unit_ret,sprintf,sprintf_template,parse_n_array,(<|>),choice,attempt,(>>.),(.>>),(.>>.),try_with,guard,(>>%))
    """) |> module_


let console =
    (
    "Console",[parsing],"IO printing functions.",
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