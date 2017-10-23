﻿module Spiral.Lib
open Main

let option =
    (
    "Option",[],"The Option module.",
    """
inl Option x = union (type ([Some: x])) (type ([None]))

inl some x = box (Option x) [Some: x]
inl none x = box (Option x) [None]

{Option some none}
    """) |> module_

let tuple =
    (
    "Tuple",[],"Operations on tuples.",
    """
inl singleton x = x :: ()
inl head = function
    | x :: xs -> x
inl tail = function
    | x :: xs -> xs

inl rec foldl f s = function
    | x :: xs -> foldl f (f s x) xs
    | () -> s

inl rec foldr f l s = 
    match l with
    | x :: xs -> f x (foldr f xs s)
    | () -> s

inl rec scanl f s = function
    | x :: xs -> s :: scanl f (f s x) xs
    | () -> s :: ()

inl rec scanr f l s = 
    match l with
    | x :: xs -> 
        inl r = scanr f xs s
        f x (head r) :: r
    | () -> s :: ()

inl append = foldr (::)

inl rev, map =
    inl map' f l = foldl (inl s x -> f x :: s) () l
    inl rev l = map' id l
    inl map f = map' f >> rev
    rev, map

inl iter f = foldl (const f) ()
inl iteri f = foldl f 0

inl rec forall f = function
    | x :: xs -> f x && forall f xs
    | () -> true

inl rec exists f = function
    | x :: xs -> f x || exists f xs
    | () -> false

inl rec filter f = function
    | x :: xs -> if f x then x :: filter f xs else filter f xs
    | () -> ()

inl is_empty = function
    | _ :: _ -> false
    | () -> true
    | _ -> error_type "Not a tuple."

inl is_non_empty_tuple = function
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
    inl rec zip = function // when forall is_non_empty_tuple l 
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
        | _ :: _ as l when forall is_non_empty_tuple l -> transpose (map unzip l) (inl _ -> on_irreg l) id 
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
inl range (min,max) = 
    inl l = max-min+1
    if l > 0 then init l ((+) min)
    else error_type "The inputs to range must be both static and the length of the resulting tuple must be greater than 0."

inl rec tryFind f = function
    | x :: xs -> if f x then [Some: x] else tryFind f xs
    | () -> [None]

inl rec contains t x = 
    match tryFind ((=) x) t with
    | [Some: x] -> true
    | [None] -> false

{head tail foldl foldr scanl scanr rev map iter iteri forall exists filter zip unzip index init repeat append singleton range tryFind contains}
    """) |> module_

let core =
    (
    "Core",[tuple],"The Core module.",
    """
inl fsharp_core = assembly_load."FSharp.Core"
inl system = assembly_load ."system, Version=4.0.0.0, Culture=neutral, PublicKeyToken=b77a5c561934e089"
inl prim_eq = (=)
// Structural polymorphic equality for every type in the language (apart from functions).
inl (=) a b =
    inl rec (=) a b =
        inl body = function
            | .(a), .(b) -> a = b
            | a :: as', b :: bs -> a = b && as' = bs
            | {} & a, {} & b -> module_values a = module_values b
            | (), () -> true
            | a, b when eq_type a b -> prim_eq a b // This repeat eq_type check is because unboxed union types might lead to variables of different types to be compared.
            | _ -> false
        if boxed_variable_is a && boxed_variable_is b then (met _ -> body (a, b) : bool)()
        else body (a, b)
    if eq_type a b then a = b
    else error_type ("Trying to compare variables of two different types. Got:",a,b)

/// Generalizes term casting for curried functions. Used for F# interop involving delegates.
inl term_cast_curry f tys =
    inl rec loop vars tys =
        match tys with
        | x :: xs -> term_cast (inl x -> loop (x :: vars) xs) x
        | () -> Tuple.foldr (inl var f -> f var) vars f
    loop () tys

/// The sprintf in parsing is very slow to compile so this is the reasonable alternative to it.
/// It is a decent bit more flexible that it too.
inl rec string_concat sep l =
    inl StringBuilder = mscorlib."System.Text.StringBuilder"
    inl ap s = function
        | x : string when lit_is x && x = "" -> s
        | _ :: _ as l -> string_concat sep l
        | x -> s.Append x
    inl ap_sep s x = ap (ap s sep) x

    inl len =
        inl rec len = 
            inl f (static_len, dyn_len, num_sep as s) = function
                | x : string ->
                    if lit_is x then (static_len + string_length x, dyn_len, num_sep+1)
                    else (static_len, dyn_len + string_length x, num_sep+1)
                | _ :: _ as l -> len s l
                | x -> (static_len+6, dyn_len, num_sep+1)
            Tuple.foldl f
        inl static_len, dyn_len, num_sep = len (0,0,-1) l
        static_len + num_sep * string_length sep + dyn_len
        |> unsafe_convert int32

    Tuple.foldl ap_sep (ap (StringBuilder len) (Tuple.head l)) (Tuple.tail l)
    <| .ToString <| ()

{(=) term_cast_curry fsharp_core system string_concat}
    """) |> module_

let loops =
    (
    "Loops",[tuple],"Various imperative loop constructors module.",
    """
inl rec while {cond body state} as d =
    inl loop_body {state cond body} as d =
        if cond state then while {d with state=body state}
        else state
    match d with
    | {static} -> loop_body d
    | _ -> (met _ -> loop_body d : state) ()

inl for_template kind =
    inl rec loop {from (near_to ^ to)=to by} as d =
        inl loop_body {check from by state body finally} as d =
            if check from then 
                match kind with
                | .Navigable ->
                    inl d = {d without state}
                    inl next state = loop {d with state from=from+by}
                    body {next state i=from}
                | .Standard ->
                    loop {d with state=body {state i=from}; from=from+by}
            else finally state
            : finally state

        match d with
        | {static} when Tuple.forall lit_is (from,to,by) -> loop_body d
        | _ -> (met d -> loop_body d) {d with from=dyn from}

    inl er_msg = "The by field should not be zero in loop as the program would diverge."

    function | {static_from} as d -> {d with static=()} | d -> d
    >> function | {from ^ static_from=from} as d -> {d with from without static_from} | d -> error_type "The from field to loop is missing."
    >> function | {to ^ near_to} as d -> d | d -> "For loop needs exlusively to or near_to fields."
    >> function | {body} as d -> d | d -> error_type "The loop body is missing."
    >> function | {state} as d -> d | d -> {d with state=()}
    >> function | {by} as d -> d | d -> {d with by=1}
    >> function | {finally} as d -> d | d -> {d with finally=id}
    >> function 
        | {by} when lit_is by && by = 0 -> error_type er_msg
        // The `check` field is a binding time improvement so the loop gets specialized to negative steps.
        // That way it will get specialized even if `by` is dynamic.
        | {by} as d -> 
            if by < 0 then loop {d with check=match d with | {to} -> inl from -> from >= to | {near_to} -> inl from -> from > near_to}
            else loop {d with check=match d with | {to} -> inl from -> from <= to | {near_to} -> inl from -> from < near_to}

inl for = for_template .Standard
inl for' = for_template .Navigable

{for for' while}
    """) |> module_


let array =
    (
    "Array",[tuple;loops],"The array module",
    """
open Loops

inl empty t = array_create 0 t
inl singleton x =
    inl ar = array_create 1 x
    ar 0 <- x
    ar

inl foldl f state ar = for {from=0; near_to=array_length ar; state; body=inl {state i} -> f state (ar i)}
inl foldr f ar state = for {to=0; from=array_length ar-1; by= -1; state; body=inl {state i} -> f (ar i) state}

inl init = 
    inl body is_static n f =
        assert (n >= 0) "The input to init needs to be greater or equal than 0."
        inl typ = type (f 0)
        inl ar = array_create n typ
        inl d = 
            inl d = {from=0; near_to=n; body=inl {i} -> ar i <- f i}
            if is_static then {d with static = ()} else d
        for d
        ar
    function
    | .static n f -> body true n f
    | n f -> body false n f


inl map f ar = init (array_length ar) (ar >> f)
inl filter f ar =
    inl ar' = array_create (array_length ar) (ar.elem_type)
    inl count = foldl (inl s x -> if f x then ar' s <- x; s+1 else s) (dyn 0) ar
    init count ar'

inl append l =
    inl ar' = array_create (Tuple.foldl (inl s l -> s + array_length l) 0 l) ((fst l).elem_type)
    inl ap s ar = foldl (inl i x -> ar' i <- x; i+1) s ar
    Tuple.foldl ap (dyn 0) l |> ignore
    ar'

inl concat ar =
    inl count = foldl (inl s ar -> s + array_length ar) (dyn 0) ar
    inl ar' = array_create count (ar.elem_type.elem_type)
    (foldl << foldl) (inl i x -> ar' i <- x; i+1) (dyn 0) ar |> ignore
    ar'

inl forall f ar = for' {from=0; near_to=array_length ar; state=true; body = inl {next state i} -> f (ar i) && next state}
inl exists f ar = for' {from=0; near_to=array_length ar; state=false; body = inl {next state i} -> f (ar i) || next state}

{empty singleton foldl foldr init map filter append concat forall exists}
    """) |> module_

let list =
    (
    "List",[loops;option;tuple],"The List module.",
    """
open Loops
open Option

type List x =
    ()
    x, List x

inl lw x = 
    inl rec loop tup_type n x on_fail on_succ =
        if n > 0 then
            match x with
            | () -> on_fail()
            | a, b -> loop tup_type (n-1) b on_fail <| inl b -> on_succ (a :: b)
        else
            match tup_type with
            | .tup ->
                match x with
                | () -> on_succ()
                | _ -> on_fail()
            | .cons -> on_succ (x :: ())

    // Testing for whether the type is a List is not possible since the types are stripped away so ruthlesly in case.
    match x with
    | [var: x] _ on_succ -> on_succ x
    | (.tup | .cons) & typ, (n, x) -> loop typ n x

inl empty x = box (List x) ()
inl singleton x = box (List x) (x, empty x)
inl cons a b = 
    inl t = List a
    box t (a, box t b)

inl init = 
    inl body n f =
        inl t = type (f 0)
        {from=0; near_to=n; state=empty t; body=inl {next i state} -> cons (f i) (next state)}

    function
    | .static n f -> body n f |> inl d -> {d with static=()} |> for'
    | n f -> body n f |> for'
    
    
inl elem_type l =
    match split l with
    | (), (a,b) when eq_type (List a) l -> a
    | _ -> error_type "Expected a List in elem_type."

inl is_static x = box_is x || lit_is x

inl rec map f l = 
    inl t = type (f (elem_type l))
    inl loop map =
        match l with
        | #lw (x :: xs) -> cons (f x) (map f xs)
        | #lw () -> empty t
        : List t
    if is_static l then loop map
    else (met _ -> loop map) ()

inl fold_template loop f s l = 
    if is_static l then loop f s l
    else (met () -> loop f s l) ()

inl rec foldl x =
    fold_template (inl f s l ->
        match l with
        | #lw (x :: xs) -> foldl f (f s x) xs
        | #lw () -> s
        : s) x

inl rec foldr f l s = 
    fold_template (inl f s l ->
        match l with
        | #lw (x :: xs) -> f x (foldr f xs s)
        | #lw () -> s
        : s) f s l

inl head_tail_template f l = 
    inl t = elem_type l
    match l with
    | #lw (x :: xs) -> f (x, xs) |> some
    | #lw () -> none t

inl head = head_tail_template fst
inl tail = head_tail_template snd

met rec last l = 
    inl t = elem_type l
    foldl (inl _ x -> some x) (none t) l

inl append a b = foldr cons a b
inl concat l & !elem_type !elem_type t = foldr append l (empty t)

{List lw init map foldl foldr empty cons singleton append concat head tail last}
    """) |> module_

let parsing =
    (
    "Parsing",[tuple],"Parser combinators.",
    """
// Primitives
inl m x = { 
    elem =
        match x with
        || {parser_rec} {d with on_type} state -> parser_rec d .elem d state : on_type
        | {parser} -> parser
        | {parser_mon} -> parser_mon .elem
    }
inl term_cast p typ = m {
    parser = inl d state ->
        p .elem {d with 
            on_succ = 
                inl k = term_cast (inl x,state -> self x state) (typ,state)
                inl x state -> k (x,state)
            } state
    }
inl goto point x = m {
    parser = inl _ -> point x
    }
inl succ x = m {
    parser = inl {on_succ} -> on_succ x
    }
inl fail x = m {
    parser = inl {on_fail} -> on_fail x
    }
inl fatal_fail x = m {
    parser = inl {on_fatal_fail} -> on_fatal_fail x
    }
inl type_ = m {
    parser = inl {on_type on_succ} -> on_succ on_type
    }
inl state = m {
    parser = inl {on_succ} state -> on_succ state state
    }
inl set_state state = m {
    parser = inl {on_succ} _ -> on_succ () state
    }
inl (>>=) a b = m {
    parser = inl d -> a .elem {d with on_succ = inl x -> b x .elem d}
    }
inl try_with handle handler = m {
    parser = inl d -> handle .elem {d with on_fail = inl _ -> handler .elem d}
    }
inl guard cond handler = m {
    parser = inl {d with on_succ} state -> 
        if cond then on_succ () state 
        else handler .elem d state
    }
inl ifm cond tr fl = m {
    parser = inl d state -> if cond then tr () .elem d state else fl () .elem d state
    }
inl attempt a = m {
    parser = inl d state -> a .elem { d with on_fail = inl x _ -> self x state } state
    }

inl rec tuple = function
    | () -> succ ()
    | x :: xs ->
        inm x = x
        inm xs = tuple xs
        succ (x :: xs)

inl (|>>) a f = a >>= inl x -> succ (f x)
inl (.>>.) a b = tuple (a,b)
inl (.>>) a b = tuple (a,b) |>> fst
inl (>>.) a b = a >>= inl _ -> b // The way bind is used here in on purpose. `spaces` diverges otherwise due to loop not being evaled in tail position.
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

inl stream_char = m {
    parser = inl {d with stream on_succ on_fail} {state with pos} ->
        stream {
            idx = pos
            on_succ = inl c -> on_succ c {state with pos=pos+1}
            on_fail = inl msg -> on_fail msg state
            }
    }

inl run data parser ret = 
    match data with
    | _ : string -> parser .elem { ret with stream = string_stream data} { pos = if lit_is data then 0 else dyn 0 }
    | _ -> error_type "Only strings supported for now."

inl with_unit_ret = {
    on_type = ()
    on_succ = inl _ _ -> ()
    on_fail = inl x _ -> failwith x
    on_fatal_fail = inl x _ -> failwith x
    }

inl run_with_unit_ret data parser = run data parser with_unit_ret

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
    inl rec loop (!dyn i) = m {
        parser_rec = inl {d with on_succ} ->
            ifm (i < string_length str)
            <| inl _ -> pchar (str i) >>. loop (i+1)
            <| inl _ -> succ str
        }
    loop 0 x

inl pint64 =
    inl rec loop handler i = m {
        parser_rec = inl {on_succ} ->
            inm c = try_with pdigit handler
            inl x = to_int64 c - to_int64 '0'
            inl max = 922337203685477580 // max int64 divided by 10
            inm _ = guard (i = max && x <= 7 || i < max) (fail "integer overflow")
            inl i = i * 10 + x
            loop (goto on_succ i) i
        }
    loop (fail "pint64") 0

/// Note: Unlike the Fparsec version, this spaces returns the number of spaces skipped.
inl spaces x =
    inl rec loop (!dyn i) = m {
        parser_rec = inl {on_succ} -> try_with (satisfyL (inl c -> is_whitespace c || is_newline c) "space") (goto on_succ i) >>. loop (i+1)
        }
    loop 0 x

inl parse_int =
    inm !dyn m = try_with (pchar '-' >>. succ false) (succ true)
    (pint64 |>> inl x -> if m then x else -x) .>> spaces

inl repeat n parser =
    inl rec loop (!dyn i) = m {
        parser_rec = inl _ ->
            ifm (i < n)
            <| inl _ -> parser i >>. loop (i+1)
            <| inl _ -> succ ()
        }
    loop 0

inl parse_array {parser typ n} = m {
    parser_mon =
        inm _ = guard (n >= 0) (fatal_fail "n in parse array must be >= 0")
        inl ar = array_create n typ
        repeat n (inl i -> parser |>> inl x -> ar i <- x) >>. succ ar
    }

inl sprintf_parser append =
    inl rec sprintf_parser sprintf_state =
        inl parse_variable = m {
            parser_mon =
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
                    m { parser = inl d state -> d.on_succ (inl x -> append x; sprintf_parser .None .elem d state) state }
            }

        inl append_state = m {
            parser = inl {d with stream on_succ on_fail} state ->
                match sprintf_state with
                | .None -> on_succ () state
                | ab -> stream {
                    idx = ab
                    on_succ = inl r -> append r; on_succ () state 
                    on_fail = inl msg -> on_fail msg state
                    }
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
        on_succ = inl x _ -> x
        on_fail = inl msg _ -> strb.ToString()
        } format

{run run_with_unit_ret succ fail fatal_fail state type_ tuple (>>=) (|>>) (.>>.) (.>>) (>>.) (>>%) (<|>) choice stream_char 
 ifm (<?>) pdigit pchar pstring pint64 spaces parse_int repeat parse_array sprintf sprintf_template term_cast}
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
        on_succ = inl x _ -> x
        on_fail = inl msg _ -> cont()
        }

inl printf = printf_template id
inl printfn = printf_template writeline

{console readall readline write writeline printf printfn}
    """) |> module_

let queue =
    (
    "Queue",[tuple;loops;console],"The queue module.",
    """
open Loops
open Console
// The design of this is not ideal, it should be a single object with 3 mutable fields instead of just one tuple field,
// but it should do nicely in a pinch for those dynamic programming kind of problems.
inl add_one len x =
    inl x = x + 1
    if x = len then 0 else x

inl resize {len from to ar} =
    inl ar' = array_create (len*3/2+3) (ar.elem_type)
    for {from near_to=len; body=inl {i} -> ar' (i - from) <- ar i}
    for {from=0; near_to=from; body=inl {i} -> ar' (len - from + i) <- ar i}
    {from=0; to=len; ar=ar'}

met enqueue {state} (!dyn v) =
    inl {from to ar} = state()
    ar to <- v
    inl len = array_length ar
    inl to = add_one len to
    state := if from = to then resize {len from to ar} else {from to ar}

met dequeue {state} () =
    inl {from to ar} = state()
    assert (from <> to) "Cannot dequeue past the end of the queue."
    state := {from=add_one (array_length ar) from; to ar}
    ar from

inl create n typ =
    inl n = match n with | () -> 16 | n -> max 1 n
    inl queue = {state=ref {from=0; to=0; ar=array_create n typ}}
    {internal = queue; enqueue = enqueue queue; dequeue = dequeue queue}

{create}
    """) |> module_

let arrayn =
    (
    "ArrayN",[tuple;loops;console],"The array module",
    """
open Loops
open Console

inl dim_size {from to} = to - from + 1 |> max 0
inl offset_at_index array i =
    inl rec loop x state = 
        match x with
        | {dim_ranges=({from to} & dim_range) :: dim_ranges ar}, i :: is ->
            inl offset, dim_offset = loop ({dim_ranges ar}, is) state
            inl dim_offset = dim_offset() 
            assert (i >= from && i <= to) "Argument out of bounds."
            
            // The function wrapper here is to simulate lazy eval in order to 
            // avoid the multiply on the first index at the end of the loop.
            (offset+dim_offset*(i-from)), inl _ -> dim_offset * dim_size dim_range 
        | {dim_ranges=() ar}, () ->
            state
    loop (array,i) (0,inl _ -> 1) |> fst

inl index {ar} as x i = ar (offset_at_index x i)
inl set {ar} as x i v = ar (offset_at_index x i) <- v
        
inl map_dims = 
    Tuple.map (function
        | {from to} as d -> d
        | x -> {from=0; to=x-1})

inl init !map_dims dim_ranges f =
    match tuple_length dim_ranges with
    | 0 -> error_type "The number of dimensions to init must exceed 0. Use `ref` instead."
    | num_dims ->
        inl len :: dim_offsets = Tuple.scanr (inl (!dim_size dim) s -> dim * s) dim_ranges 1
        inl ar = array_create len (type (f (Tuple.repeat num_dims 0)))
        inl rec loop offset index = function
            | {from to} :: dim_ranges, dim_offset :: dim_offsets ->
                for {from to state=offset; body=inl {state=offset i} ->
                    loop offset (i :: index) (dim_ranges,dim_offsets)
                    offset+dim_offset
                    } |> ignore
            | (),() -> ar offset <- f (Tuple.rev index)
        loop (dyn 0) () (dim_ranges,dim_offsets)
        inl array_data = heap {dim_ranges ar}
        {array_data index=(inl i -> index array_data i); set=(inl i v -> set array_data i v)}
                
{init}
    """) |> module_

let cuda =
    (
    "Cuda",[core;console],"The Cuda module.",
    """
open Core
open Console

inl ops = fsharp_core."Microsoft.FSharp.Core.Operators"
inl Environment = mscorlib."System.Environment"

inl cuda_toolkit_path = 
    inl x = Environment.GetEnvironmentVariable("CUDA_PATH_V8_0")
    if ops.IsNull x then failwith "CUDA_PATH_V8_0 environment variable not found. Make sure Cuda 8.0 SDK is installed."
    x

inl visual_studio_path =
    inl x = Environment.GetEnvironmentVariable("VS140COMNTOOLS")
    if ops.IsNull x then failwith "VS140COMNTOOLS environment variable not found. Make sure VS2015 is installed."
    mscorlib."System.IO.Directory".GetParent(x).get_Parent().get_Parent().get_FullName()

inl cub_path = // The path for the Cuda Unbound library.
    inl x = Environment.GetEnvironmentVariable("CUB_PATH")
    if ops.IsNull x then 
        failwith 
            @"If you are getting this exception then that means that CUB_PATH environment variable is not defined.

Go to: https://nvlabs.github.io/cub/index.html#sec6
...and download the latest version of the library, extract it somewhere like, 
eg. : C:\cub-1.6.3
and add that directory to the global enviroment by creating the CUB_PATH variable with a pointer to it."
    x

inl ManagedCuda = assembly_load ."ManagedCuda, Version=7.5.7.0, Culture=neutral, PublicKeyToken=242d898828717aa0"
inl context = ManagedCuda ."ManagedCuda.CudaContext" false

inl compile_kernel_using_nvcc_bat_router (kernels_dir: string) =
    
    inl Path = mscorlib ."System.IO.Path"
    inl File = mscorlib ."System.IO.File"
    inl StreamWriter = mscorlib ."System.IO.StreamWriter"
    inl ProcessStartInfo = system ."System.Diagnostics.ProcessStartInfo"

    inl nvcc_router_path = Path.Combine(kernels_dir,"nvcc_router.bat")
    inl procStartInfo = ProcessStartInfo()
    procStartInfo.set_RedirectStandardOutput true
    procStartInfo.set_RedirectStandardError true
    procStartInfo.set_UseShellExecute false
    procStartInfo.set_FileName nvcc_router_path
    inl process = system ."System.Diagnostics.Process"()
    process.set_StartInfo procStartInfo
    inl print_to_standard_output = term_cast_curry (inl _ args -> args.get_Data() |> writeline)
    inl add_handler event =
        system ."System.Diagnostics.DataReceivedEventHandler" print_to_standard_output
        |> event_add_handler process event

    add_handler .ErrorDataReceived
//    add_handler .OutputDataReceived
    
    inl concat = string_concat ""
    inl (+) a b = concat (a, b)

    /// Puts quotes around the string.
    inl quote x = concat ('"',x,"'")
    inl call x = concat ("call ", x)
    inl quoted_vs_path_to_vcvars = Path.Combine(visual_studio_path, @"VC\bin\x86_amd64\vcvarsx86_amd64.bat") |> quote
    inl quoted_vs_path_to_cl = Path.Combine(visual_studio_path, @"VC\bin\x86_amd64") |> quote
    inl quoted_cuda_toolkit_path_to_include = Path.Combine(cuda_toolkit_path,"include") |> quote
    inl quoted_cub_path_to_include = cub_path |> quote
    inl quoted_kernels_dir = kernels_dir |> quote
    inl target_path = Path.Combine(kernels_dir,"cuda_kernels.ptx")
    inl quoted_target_path = target_path |> quote
    inl input_path = Path.Combine(kernels_dir,"cuda_kernels.cu")
    inl quoted_input_path = input_path |> quote

    if File.Exists input_path then File.Delete input_path
    File.WriteAllText(input_path,cuda_kernels)
    
    inl _ = 
        if File.Exists nvcc_router_path then File.Delete nvcc_router_path
        inl nvcc_router_file = File.OpenWrite(nvcc_router_path)
        inl nvcc_router_stream = StreamWriter(nvcc_router_file)

        nvcc_router_stream.WriteLine(call quoted_vs_path_to_vcvars)
        concat (
            "nvcc -gencode=arch=compute_30,code=\"sm_30,compute_30\" --use-local-env --cl-version 2015 -ccbin ",quoted_vs_path_to_cl,
            "  -I",quoted_cuda_toolkit_path_to_include," -I",quoted_cub_path_to_include," --keep-dir ",quoted_kernels_dir,
            " -maxrregcount=0  --machine 64 -ptx -cudart static  -o ",quoted_target_path,' ',quoted_input_path
            ) |> nvcc_router_stream.WriteLine
        nvcc_router_file.Dispose()
        nvcc_router_stream.Dispose()

    if process.Start() = false then failwith "NVCC failed to run."
    process.BeginOutputReadLine()
    process.BeginErrorReadLine()
    process.WaitForExit()

    inl exit_code = process.get_ExitCode()
    if exit_code <> 0i32 then failwith <| concat ("NVCC failed compilation with code ", exit_code)

    // Free memory
    process.Dispose()
    
    context.LoadModulePTX target_path

inl current_directory = Environment.get_CurrentDirectory()
inl modules = compile_kernel_using_nvcc_bat_router current_directory
writeline (Core.string_concat "" ("Compiled the kernels into the following directory: ", current_directory))

inl dim3 = ManagedCuda ."ManagedCuda.VectorTypes.dim3"

{ManagedCuda context modules dim3}
    """) |> module_