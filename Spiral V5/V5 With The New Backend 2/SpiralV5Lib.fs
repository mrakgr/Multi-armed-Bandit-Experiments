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

module (foldl,foldr,rev,map,forall,exists,filter,is_empty,is_tuple,zip,unzip,index,upon,upon')
    """) |> module_

let parsing =
    (
    "Parsing",[tuple],"Parser combinators. (depreciated)",
    """
inl convert = mscorlib ."System.Convert"
inl to_int64 = convert .ToInt64

met is_digit x = x >= '0' && x <= '9'
met is_whitespace x = x = ' '
met is_newline x = x = '\n' || x = '\r'

inl stream_create stream = 
    inl pos = ref 0
    module (pos, stream)

inl ParserResult suc =
    type 
        .Succ, suc
        .Fail, (StreamPosition, string)
        .FatalFail, string

met rec List x =
    type
        .ListCons, (x, List x)
        .ListNil

inl stream_advance i s = s.pos := s.pos () + i

inl pchar s ret = 
    (s.stream) (s.pos()) <| function
    | .Succ, _ as c -> 
        stream_advance 1 s
        ret c
    | c -> ret c

inl pdigit s ret =
    pchar s <| function
    | .Succ, (c: char) ->
        if is_digit c then ret (.Succ, c)
        else ret (.Fail, (s.pos, "digit"))
    | x -> ret x

inl pint64 s ret =
    met rec loop state (^ dyn i) = 
        pdigit s <| function
            | .Succ, c -> 
                inl x = to_int64 c - to_int64 '0'
                i * 10 + x |> loop .Rest
            | .Fail, _ -> 
                match state with
                | .First -> ret (.Fail, (s.pos, "int64"))
                | .Rest -> ret (.Succ, i)
            | x -> ret x
        : ret .FetchType
            
    loop .First 0

inl list_rev typ l = 
    met rec loop (^typ (^dyn acc)) l = 
        match l with
        | .ListNil -> acc
        | .ListCons, (x, xs) -> loop (.ListCons, (x, acc)) xs
        : l
    loop .ListNil l

inl many typ_p p s ret =
    inl typ = List typ_p

    met rec many (^typ (^dyn r)) =
        inl state = s.pos ()
        p s <| function
            | .Succ, x when state < s.pos() -> many <| (.ListCons, (x, r))
            | .Succ, _ when state = s.pos() -> ret (.FatalFail, "Many parser succeeded without changing the parser state. Unless the computation had been aborted, the parser would have gone into an infinite loop.")
            | .Fail, _ when state = s.pos() -> ret (.Succ, list_rev typ r)
            | .Fail, _ -> ret (.Fail, (s.pos, "many"))
            | x -> ret x
        : ret .FetchType

    many .ListNil

met rec spaces s ret =
    pchar s <| function
    | .Succ, c ->    
        if is_whitespace c || is_newline c then spaces s ret
        else stream_advance (-1) s; ret (.Succ,())
    | .Fail, _ -> ret (.Succ,())
    | x -> ret x
    : ret .FetchType

inl tuple l s ret =
    inl rec loop acc = function
        | x :: xs ->
            x s <| function
            | .Succ, x -> loop (x :: acc) xs
            | x -> ret x
        | () -> ret (.Succ, Tuple.rev acc)
        | _ -> error_type "Incorrect input to tuple."
    loop () l

inl (>>=) a b s ret =
    a s <| function
    | .Succ, x -> b x s ret
    | x -> ret x

inl (|>>) a f = a >>= inl x s ret -> ret (.Succ, f x)

inl string_stream str = 
    stream_create <| inl idx ret ->
        met cond = idx >= 0 && idx < string_length str
        if cond then ret (.Succ, (str idx)) 
        else ret (.Fail, (idx, "string index out of bounds"))

inl run (^dyn data) parser ret = 
    match data with
    | _ : string -> parser (string_stream data) ret
    | _ -> error_type "Only strings supported for now."

inl parse_int = tuple (pint64, spaces) |>> fst

inl parse_n_ints n = 
    inl rec loop n = 
        match n with 
        | n when n > 0 -> parse_int :: loop (n-1)
        | 0 -> ()
        | _ -> error_type "The input to this function cannot be static or less than 0 or not an int."
    loop n |> tuple
    
inl parse_ints = many int64 parse_int

inl preturn x s ret = ret (.Succ, x)

module (ParserResult,List,run,spaces,tuple,many,(>>=),(|>>),pint64,preturn,parse_int,parse_n_ints,parse_ints)
    """) |> module_

let parsing2 =
    (
    "Parsing",[tuple],"Parser combinators.",
    """
inl convert = mscorlib ."System.Convert"
inl to_int64 = convert .ToInt64

met is_digit x = x >= '0' && x <= '9'
met is_whitespace x = x = ' '
met is_newline x = x = '\n' || x = '\r'

inl stream_create stream = 
    inl pos = ref 0
    module (pos, stream)

inl ParserResult suc =
    type 
        .Succ, suc
        .Fail, (StreamPosition, string)
        .FatalFail, string

met rec List x =
    type
        .Cons, (x, List x)
        .Nil

inl stream_advance i s = s.pos := s.pos () + i

inl pchar s ret = 
    (s.stream) (s.pos()) (upon' ret .on_succ <| inl c ->
        stream_advance 1 s
        ret .on_succ c)
    
inl pdigit s ret =
    pchar s (upon' ret .on_succ <| inl (c: char) ->
        if is_digit c then ret .on_succ c
        else ret .on_fail (s.pos, "digit"))

inl pint64 s ret =
    met rec loop state (^ dyn i) = 
        pdigit s <| Tuple.upon' ret (
            (.on_succ, inl c ->
                inl x = to_int64 c - to_int64 '0'
                i * 10 + x |> loop .Rest
                ),
            (.on_fail, inl _ ->
                match state with
                | .First -> ret .on_fail (s.pos, "int64")
                | .Rest -> ret .on_succ i
                )
            )
        : ret .on_type
            
    loop .First 0

inl list_rev typ l = 
    met rec loop (^typ (^dyn acc)) l = 
        match l with
        | .Nil -> acc
        | .Cons, (x, xs) -> loop (.Cons, (x, acc)) xs
        : l
    loop .Nil l

inl many typ_p p s ret =
    inl typ = List typ_p

    met rec many (^typ (^dyn r)) =
        inl state = s.pos ()
        p s <| Tuple.upon' ret (
            (.on_succ, function
                | x when state < s.pos() -> many <| (.Cons, (x, r))
                | _ when state = s.pos() -> ret .on_fatal_fail "Many parser succeeded without changing the parser state. Unless the computation had been aborted, the parser would have gone into an infinite loop."
                )
            (.on_fail, function
                | _ when state = s.pos() -> ret .on_succ (list_rev typ r)
                | _ -> ret .on_fail (s.pos, "many")
                )
            )
        : ret .on_type

    many .Nil

met rec spaces s ret =
    pchar s <| Tuple.upon' ret (
        (.on_succ, inl c ->    
            if is_whitespace c || is_newline c then spaces s ret
            else stream_advance (-1) s; ret .on_succ ()
            ),
        (.on_fail, inl _ -> ret .on_succ ())
        )
    : ret .on_type

inl tuple l s ret =
    inl rec loop acc = function
        | x :: xs -> x s <| upon' ret .on_succ (inl x -> loop (x :: acc) xs)
        | () -> ret .on_succ (Tuple.rev acc)
        | _ -> error_type "Incorrect input to tuple."
    loop () l

inl (>>=) a b s ret = a s <| upon' ret .on_succ (inl x -> b x s ret)
inl (|>>) a f = a >>= inl x s ret -> ret .on_succ (f x)

inl string_stream str = 
    stream_create <| inl idx ret ->
        if idx >= 0 && idx < string_length str then ret .on_succ (str idx)
        else ret .on_fail (idx, "string index out of bounds")

inl run data parser ret = 
    match data with
    | _ : string -> parser (string_stream data) ret
    | _ -> error_type "Only strings supported for now."

inl parse_int = tuple (pint64, spaces) |>> fst

inl parse_n_ints n = 
    inl rec loop n = 
        match n with 
        | n when n > 0 -> parse_int :: loop (n-1)
        | 0 -> ()
        | _ -> error_type "The input to this function cannot be static or less than 0 or not an int."
    loop n |> tuple
    
inl parse_ints = many int64 parse_int
inl preturn x s ret = ret .on_succ x

inl with_unit_ret f = 
    inl on_succ x = unit (f x)
    inl on_fail x = ()
    inl on_fatal_fail x = ()
    inl on_type = ()
    module (on_succ,on_fail,on_fatal_fail,on_type)

inl run_with_unit_ret data parser f = run data parser (with_unit_ret f)

inl rec sprintf_parser append =
    inl f x = append x; sprintf_parser append
    inl parse_value s ret = 
        pchar s <| Tuple.upon' ret (
            (.on_succ, function
                | 'i' -> function
                    | x : int32 | x : int64 | x : uint32 | x : uint64 -> 
                        print_static 'i'
                        f x
                    | _ -> error_type "Expected an integer in sprintf."
                | 'f' -> function
                    | x : float32 | x : float64 -> 
                        print_static 'f'
                        f x
                    | _ -> error_type "Expected a float in sprintf."
                | _ -> error_type "Unexpected literal in sprintf."),
            (.on_fail, inl x ->
                append '%'
                ret .on_fail x)
            )
    pchar >>= function
        | '%' -> 
            print_static "%"
            parse_value
        | c -> 
            print_static c
            f c

inl sprintf format =
    inl strb = mscorlib."System.Text.StringBuilder"(64i32)
    inl append x = strb.Append x |> ignore
    inl on_fail = strb.ToString()
    run format (sprintf_parser append) (module(on_fail))

module (ParserResult,List,run,spaces,tuple,many,(>>=),(|>>),pint64,preturn,parse_int,parse_n_ints,parse_ints,run_with_unit_ret,sprintf)
    """) |> module_

let console =
    (
    "Console",[parsing2],"IO printing functions.",
    """
inl console = mscorlib."System.Console"
inl readline () = console.ReadLine()
inl write x = console.Write x
inl writeline x = write x; console.WriteLine()
    """) |> module_