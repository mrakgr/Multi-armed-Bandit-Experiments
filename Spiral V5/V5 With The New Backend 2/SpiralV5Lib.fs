module Spiral.Lib

let tuple =
    "Tuple",
    """
inl rec foldl f s = function
    | x :: xs -> foldl f (f s x) xs
    | () -> s
inl rec foldr f l s = 
    match l with
    | x :: xs -> f x (foldr f xs s)
    | () -> s
    
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

module (foldl,foldr,rev,map,forall,exists,filter,is_empty,is_tuple,zip,unzip,index)
    """

let parsing =
    "Parsing",
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
    """

