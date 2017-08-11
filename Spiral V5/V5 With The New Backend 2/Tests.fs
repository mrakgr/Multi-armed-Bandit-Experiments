module Spiral.Tests
open Main

let test1 = // Does it run?
    "test1",
    """
inl a = 5
inl b = 10
a + b
    """

let test2 = // Does it run methods?
    "test2",
    """
met a () = 5
met b () = 10
a () + b ()
    """

let test3 = // Does this method case work?
    "test3",
    """
met a = 5
met b = 10
a + b
    """

let test4 = // Does the and pattern work correctly?
    "test4",
    """
met f (a, b) (c, d) = (a+c,b+d)
met q & (a, b) = 1,2
met w & (c, d) = 3,4
f q w
    """

let test5 = // Does basic pattern matching work?
    "test5",
    """
inl f = function
    || .Add x y -> x + y
    || .Sub x y -> x - y
    || .Mult x y -> x * y
inl a = f .Add 1 2
inl b = f .Sub 1 2
inl c = f .Mult 1 2
a, b, c
    """

let fib = // Does recursion work on the fibonacci example?
    "fib",
    """
met rec fib ^dyn x = 
    if x <= 0 then 0 else fib (x-1) + fib (x-2)
    : x
fib 1
    """

let test6 = // Does returning type level methods from methods work?
    "test6",
    """
met min n =
    met tes a =
        met b -> 
            met c ->
                met d -> a,b,c
    tes 1 2 (2.2,3,4.5)
min 10
    """
let test7 = // Do active patterns work?
    "test7",
    """
inl f op1 op2 op3 = function
    | ^op1 (.Some, x) -> x
    | ^op2 (.Some, x) -> x
    | ^op3 (.Some, x) -> x

inl add = function
    | .Add -> .Some, inl x y -> x + y
    | _ -> .None
inl sub = function
    | .Sub -> .Some, inl x y -> x - y
    | _ -> .None
inl mult = function
    | .Mult -> .Some, inl x y -> x * y
    | _ -> .None

inl f = f add sub mult

inl a = f .Add 1 2
inl b = f .Sub 1 2
inl c = f .Mult 1 2
a, b, c
    """

let test8 = // Does the basic union type work?
    "test8",
    """
met x =
    inl option_int = 
        type 
            .Some, 1 
            .None
    option_int .None //(.Some, 10)
match x with
| .Some, x -> x
| .None -> 0
    """

let test9 = // Does the partial evaluator optimize unused match cases?
    "test9",
    """
inl ab = 
    type .A
         .B
met x = (ab .A, ab .A, ab .A)
match x with
| .A, _, _ -> 1
| _, .A, _ -> 2
| _, _, .A -> 3
| _ -> 4
    """

let test10 = // The worst case for partially evaluated pattern matchers.
    "test10",
    """
inl ab = 
    type .A
         .B
met x = (ab .A, ab .A, ab .A, ab .A)
match x with
| .A, .A, _ -> 1
| _, _, .A, .A -> 2
| .A, .B, .A, .B -> 3
| _ -> 4
    """

let test11 = // Do the nested patterns work on dynamic data?
    "test1",
    """
inl a = type (1,2)
inl b = type (1,a,a)
met x = b (1, a (2,3), a (4,5))
match x with
| _, (x, _), (_, y) -> x + y
| _, _, _ -> 0
| _ :: () -> 0
    """

let test12 = // Does recursive pattern matching work on static data?
    "test12",
    """
inl rec p = function
    | .Some, x -> p x
    | .None -> 0
p (.Some, .None)
    """

let test13 = // A more complex interpreter example on static data.
    "test13",
    """
met rec expr x = 
    type 
        .V, x
        .Add, expr x, expr x
        .Mult, expr x, expr x
inl int_expr = expr int64
inl v x = int_expr (.V, x)
inl add a b = int_expr (.Add, a, b)
inl mult a b = int_expr (.Mult, a, b)
inl a = add (v 1) (v 2)
inl b = add (v 3) (v 4)
inl c = mult a b
inl rec interpreter_static = function
    | .V, x -> x
    | .Add, a, b -> interpreter_static a + interpreter_static b
    | .Mult, a, b -> interpreter_static a * interpreter_static b
interpreter_static c
    """

let test14 = // Does recursive pattern matching work on partially static data?
    "test14",
    """
met rec expr x = 
    type 
        .V, x
        .Add, expr x, expr x
        .Mult, expr x, expr x
inl int_expr = expr int64
inl v x = int_expr (.V, x)
inl add a b = int_expr (.Add, a, b)
inl mult a b = int_expr (.Mult, a, b)
met a = add (v 1) (v 2)
met b = add (v 3) (v 4)
inl c = mult a b
met rec inter x = 
    match x with
    | .V, x -> x
    | .Add, a, b -> inter a + inter b
    | .Mult, a, b -> inter a * inter b
    : 0
inter c
    """

let test15 = // Does basic .NET interop work?
    "test15",
    """
inl system = load_assembly .mscorlib
inl builder_type = ."System.Text.StringBuilder" |> system 
inl b = builder_type ("Qwe", 128i32)
inl a x =
    b .Append x |> ignore
    b .AppendLine () |> ignore
a 123
a 123i16
a "qwe"
inl str = b.ToString()
inl console = ."System.Console" |> system
console .Write str |> ignore

inl dictionary_type = ."System.Collections.Generic.Dictionary`2" |> system
inl dict = dictionary_type(int64, int64)(128i32)
dict.Add(1,2) |> ignore
dict.get_Item 1
    """

let hacker_rank_1 =
    "hacker_rank_1",
    """
// The very first warmup exercise : https://www.hackerrank.com/challenges/solve-me-first
inl console = ."System.Console" |> mscorlib
inl parse_int32 = 
    inl f = ."System.Int32" |> mscorlib
    inl str -> f .Parse str
inl read_line () = console.ReadLine()
inl write x = console.Write x
inl read_int () = read_line() |> parse_int32
inl a, b = read_int(), read_int()
write (a + b)
    """

let test16 = // Do var union types work?
    "test16",
    """
inl t = type (union (type int64) (type float32))
if dyn true then t 0
else t 0.0
    """

let test17 = // Do modules work?
    "test17",
    """
inl m =
    inl x = 2
    inl y = 3.4
    inl z = "123"
    module (x,(y),z)
m.x, m.y, m.z
    """

let test18 = // Do arrays and references work?
    "test18",
    """
inl a = ref 0
a := 5
a() |> ignore

inl a = ref ()
a := ()
a() |> ignore

inl a = array_create (10,15,5) int64
a (0,8,1) <- 2
a (0,8,1) |> ignore

inl a = array_create 3 ()
a (1) <- ()
a (1) |> ignore
    """

let test19 = // Does term level casting for functions work?
    "test19",
    """
inl add a b (c, (d, f), e) = a + b + c + d + e + f
inl f = add 1 (dyn 2) `(int64,(int64,int64),int64)
f (1,(2,5),3)
    """

let test20 = // Does pattern matching on union non-tuple types work? Do type annotation patterns work?
    "test20",
    """
inl t = union (type int64) (type float32)
inl x = t 3.5
match x with
| q : float32 -> x + x
| q : int64 -> x * x
    """

let test21 = // Does defining user operators work?
    "test21",
    """
inl (.+) a b = a + b
2 * 22 .+ 33 |> ignore

inl f op a b = op a b
f (*) 2 x
    """

let test22 = // Do unary operators work?
    "test22",
    """
inl lit_lift x =
    print_static "I am in lit_lift."
    lit_lift x
inl t1 x = -x
inl t2 x = `x
inl t3 x = .(x)
t1 2.2, t2 true, t3 "asd"
    """

let test23 = // Do when and as patterns work?
    "test23",
    """
inl f = function
    | a,b,c as q when a < 10 -> q
    | _ -> 0,0,0
f (1,2,3)
    """

let test24 = // Do literal pattern matchers work? Does partial evaluation of equality work?
    "test24",
    """
inl f x = 
    match x with
    | 0 -> "0", x
    | 1 -> "1", x
    | false -> "false", x
    | true -> "true", x
    | "asd" -> "asd", x
    | 1i8 -> "1i8", x
    | 5.5 -> "5.5", x
    | .5.5 -> ".5.5", x
    | .23u32 -> ".23u32",x
    | _ -> "unknown", x

f 0, f 1, f false, f true, f "asd", f 1i8,
f 5.5, f 5f64, f .5.5, f .23u32
    """

let test25 = // Does the tuple cons pattern work?
    "test25",
    """
inl f = function | x1 :: x2 :: x3 :: xs -> 3 | x1 :: x2 :: xs -> 2 | x1 :: xs -> 1 | () -> 0

f (), f (1 :: ()), f (1,2)
    """

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

let test26 = // Does tuple map work? This also tests rev and foldl.
    "test26",
    """
Tuple.map (inl x -> x * 2) (1,2,3)
    """

let test27 = // Do tuple zip and unzip work?
    "test27",
    """
inl j = 2,3.3
inl k = 4.4,55
inl l = 66,77
inl m = 88,99
inl n = 123,456
Tuple.zip ((j,k),(l,m),n) |> Tuple.unzip
    """

let test28 = // Does string indexing work?
    "test28",
    """
inl console = mscorlib ."System.Console"
inl a = "qwe"
inl b = console.ReadLine()
a(0),b(0)
    """

let test29 = // Does a simple int parser work?
    "test29",
    """
inl t =
    type
        int64
        int64, int64
        string
        Parsing.List int64

Parsing.run "12 34 " (Parsing.parse_ints) <| function
    | .Succ, x -> t x
    | .FatalFail, er | .Fail, (_, er) -> t er
    | .FetchType -> t ""
    | x -> error_type "Got a strange input."
    """

let test30 = // Do recursive algebraic datatypes work?
    "test30",
    """
met rec List x =
    type
        .ListCons, (x, List x)
        .ListNil

inl t = List int64
inl nil = t .ListNil
inl cons x xs = t (.ListCons, (x, xs))

met rec sum (^int64 (^dyn s)) l = 
    match l with
    | .ListCons, (x, xs) -> sum (s + x) xs
    | .ListNil -> s
    : int64

nil |> cons 3 |> cons 2 |> cons 1 |> dyn |> sum 0
        """

let test31 = // Does passing types into types work?
    "test31",
    """
inl a = 
    type 
        .A, (int64, int64)
        .B, string

inl b = 
    type 
        a
        .Hello

a (.A, (2,3)) |> dyn |> b
    """

let test32 = // Do the .NET methods work inside methods?
    "test32",
    """
inl to_int64 = mscorlib ."System.Convert" .ToInt64
met f = to_int64 (dyn 'a')
f
    """

let hacker_rank_2 =
    "hacker_rank_2",
    """
// https://www.hackerrank.com/challenges/compare-the-triplets

inl console = mscorlib."System.Console"
inl (|>>) = Parsing."|>>"
inl parse_3 f = Parsing.run (console.ReadLine()) (Parsing.parse_n_ints 3 |>> f) (inl _ -> ())
inl alice = ref 0
inl bob = ref 0
inl comp = function
    | a,b when a > b -> alice := alice () + 1
    | a,b when a < b -> bob := bob () + 1
    | a,b -> ()

parse_3 <| inl a1,a2,a3 ->
    parse_3 <| inl x1,x2,x3 ->
        comp (a1,x1); comp (a2,x2); comp (a3,x3)

alice() |> console.Write
console.Write ' '
bob() |> console.Write
    """

let test35 = // How long does it take to produce Hello 2000x times? 0.27s. More than that and it overflows.
    "test35",
    """
inl console = mscorlib."System.Console"
inl rec loop = function
    | i when i > 0 -> 
        console.WriteLine "Hello."
        loop (i-1)
    | 0 -> ()
loop 2000
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
//inl parse_int = pint64
//inl parse_int = spaces

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

let test34 = // Does parse_n_ints blow up the code size? Does it scale linearly.
    "test34",
    """
inl console = mscorlib."System.Console"
inl (|>>) = Parsing."|>>"
inl parse_3 f = Parsing.run (console.ReadLine()) (Parsing.parse_n_ints 8 |>> f) (inl _ -> ())

parse_3 <| inl _ -> ()
    """

let x = spiral_peval [] test13
printfn "%A" x

