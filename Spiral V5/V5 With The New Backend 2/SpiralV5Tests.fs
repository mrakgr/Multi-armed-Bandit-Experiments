module Spiral.Tests
open Lib
open Main

let test1 = 
    "test1",[],"Does it run?",
    """
inl a = 5
inl b = 10
a + b
    """

let test2 = 
    "test2",[],"Does it run methods?",
    """
met a () = 5
met b () = 10
a () + b ()
    """

let test3 = // 
    "test3",[],"Does this method case work?",
    """
met a = 5
met b = 10
a + b
    """

let test4 = // 
    "test4",[],"Does the and pattern work correctly?",
    """
met f (a, b) (c, d) = (a+c,b+d)
met q & (a, b) = 1,2
met w & (c, d) = 3,4
f q w
    """

let test5 = // 
    "test5",[],"Does basic pattern matching work?",
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

let fib = // 
    "fib",[],"Does recursion work on the fibonacci example?",
    """
met rec fib !dyn x = 
    if x <= 0 then 0 else fib (x-1) + fib (x-2)
    : x
fib 1
    """

let test6 = // 
    "test6",[],"Does returning type level methods from methods work?",
    """
met min n =
    met tes a =
        met b -> 
            met c ->
                met d -> a,b,c
    tes 1 2 (2.2,3,4.5)
min 10
    """
let test7 = // 
    "test7",[],"Do active patterns work?",
    """
inl f op1 op2 op3 = function
    | !op1 (.Some, x) -> x
    | !op2 (.Some, x) -> x
    | !op3 (.Some, x) -> x

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

let test8 = // 
    "test8",[],"Does the basic union type work?",
    """
type option_int = 
    .Some, 1 
    .None

met x = option_int .None //(.Some, 10)
match x with
| .Some, x -> x
| .None -> 0
    """

let test9 = // 
    "test9",[],"Does the partial evaluator optimize unused match cases?",
    """
type ab = 
    .A
    .B
met x = (ab .A, ab .A, ab .A)
match x with
| .A, _, _ -> 1
| _, .A, _ -> 2
| _, _, .A -> 3
| _ -> 4
    """

let test10 = // 
    "test10",[],"The worst case for partially evaluated pattern matchers.",
    """
type ab = 
    .A
    .B
met x = (ab .A, ab .A, ab .A, ab .A)
match x with
| .A, .A, _, _ -> 1
| _, _, .A, .A -> 2
| .A, .B, .A, .B -> 3
| _ -> 4
    """

let test11 = // 
    "test1",[],"Do the nested patterns work on dynamic data?",
    """
type a = (1,2)
type b = (1,a,a)
met x = b (1, a (2,3), a (4,5))
match x with
| _, (x, _), (_, y) -> x + y
| _, _, _ -> 0
| _ :: () -> 0
    """

let test12 = // 
    "test12",[],"Does recursive pattern matching work on static data?",
    """
inl rec p = function
    | .Some, x -> p x
    | .None -> 0
p (.Some, .None)
    """

let test13 = 
    "test13",[],"A more complex interpreter example on static data.",
    """
type expr x = 
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
inl rec interpreter_static x = 
    match x with
    | .V, x -> x
    | .Add, a, b -> interpreter_static a + interpreter_static b
    | .Mult, a, b -> interpreter_static a * interpreter_static b
interpreter_static c
    """

let test14 =
    "test14",[],"Does recursive pattern matching work on partially static data?",
    """
type expr x = 
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

let test15 = // 
    "test15",[],"Does basic .NET interop work?",
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
    "hacker_rank_1",[],"The very first warmup exercise : https://www.hackerrank.com/challenges/solve-me-first",
    """
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

let test16 = // 
    "test16",[],"Do var union types work?",
    """
type t = 
    int64
    float32
if dyn true then t 0
else t 0.0
    """

let test17 = // 
    "test17",[],"Do modules work?",
    """
inl m =
    inl x = 2
    inl y = 3.4
    inl z = "123"
    module (x,(y),z)
m.x, m.y, m.z
    """

let test18 = // 
    "test18",[],"Do arrays and references work?",
    """
inl a = ref 0
a := 5
a() |> ignore

inl a = ref () // Is not supposed to be printed due to being unit.
a := ()
a()

inl a = ref <| (inl a, b -> a + b) `(int64,int64)
a := (inl a, b -> a * b) `(int64,int64)
a() |> ignore

inl a = array_create 10 int64
a 3 <- 2
a 3 |> ignore

inl a = array_create 3 id // Is supposed to be unit and not printed.
a 1 <- id
a 1 |> ignore
    """

let test19 = // 
    "test19",[],"Does term level casting for functions work?",
    """
inl add a b (c, (d, f), e) = a + b + c + d + e + f
inl f = add 1 (dyn 2) `(int64,(int64,int64),int64)
f (1,(2,5),3)
    """

let test20 = // 
    "test20",[],"Does pattern matching on union non-tuple types work? Do type annotation patterns work?",
    """
type t = 
    int64
    float32
inl x = t 3.5
match x with
| q : float32 -> x + x
| q : int64 -> x * x
    """

let test21 = // 
    "test21",[],"Does defining user operators work?",
    """
inl (.+) a b = a + b
inl x = 2 * 22 .+ 33

inl f op a b = op a b
f (*) 2 x
    """

let test22 = // 
    "test22",[],"Do unary operators work?",
    """
inl t1 x = -x
inl t2 x = `x
inl t3 x = .(x)
t2 true, t3 "asd", t1 2.2
    """

let test23 = // 
    "test23",[],"Do when and as patterns work?",
    """
inl f = function
    | a,b,c as q when a < 10 -> q
    | _ -> 0,0,0
f (1,2,3)
    """

let test24 = // 
    "test24",[],"Do literal pattern matchers work? Does partial evaluation of equality work?",
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

let test25 = // 
    "test25",[],"Does the tuple cons pattern work?",
    """
inl f = function | x1 :: x2 :: x3 :: xs -> 3 | x1 :: x2 :: xs -> 2 | x1 :: xs -> 1 | () -> 0

f (), f (1 :: ()), f (1,2)
    """

let test26 = // 
    "test26",[tuple],"Does tuple map work? This also tests rev and foldl.",
    """
Tuple.map (inl x -> x * 2) (1,2,3)
    """

let test27 = // 
    "test27",[tuple],"Do tuple zip and unzip work?",
    """
inl j = 2,3.3
inl k = 4.4,55
inl l = 66,77
inl m = 88,99
inl n = 123,456
Tuple.zip ((j,k),(l,m),n) |> Tuple.unzip
    """

let test28 = // 
    "test28",[],"Does string indexing work?",
    """
inl console = mscorlib ."System.Console"
inl a = "qwe"
inl b = console.ReadLine()
a(0),b(0)
    """

let test29 =
    "test29",[],"Does pattern matching work redux?",
    """
type t = 
    int64, int64
    int64

inl x = (1,1) |> t |> dyn
match x with
| a,b -> 0
| c -> c
    """

let test30 = // 
    "test30",[],"Do recursive algebraic datatypes work?",
    """
type List x =
    .ListCons, (x, List x)
    .ListNil

inl t = List int64
inl nil = t .ListNil
inl cons x xs = t (.ListCons, (x, xs))

met rec sum (!int64 (!dyn s)) l = 
    match l with
    | .ListCons, (x, xs) -> sum (s + x) xs
    | .ListNil -> s
    : int64

nil |> cons 3 |> cons 2 |> cons 1 |> dyn |> sum 0
        """

let test31 = // 
    "test31",[],"Does passing types into types work?",
    """
type a = 
    .A, (int64, int64)
    .B, string

type b = 
    a
    .Hello
(.A, (2,3)) |> a |> dyn |> b
    """

let test32 = // 
    "test32",[],"Do the .NET methods work inside methods?",
    """
inl to_int64 = mscorlib ."System.Convert" .ToInt64
met f = to_int64 (dyn 'a')
f
    """

let test33 = //
    "test33",[],"Does a simple loop have superlinear scaling?",
    """
inl rec loop = function
    | i when i > 0 -> loop (i-1)
    | 0 -> ()
loop 50000
    """

let test35 = // 
    "test35",[],"Does case on union types with recursive types work properly?",
    """
type List x = 
    .Nil
    .Cons, (int64, List x)

type Res =
    int64
    int64, int64
    List int64

match Res 1 |> dyn with
| x : int64 -> 1
| (a, b) as x -> 2
| _ -> 3
    """

let test38 =
    "test38",[],"Is type constructor of an int64 an int64?",
    """
inl t = int64 (dyn 1)
print_static t
    """

let test40 =
    "test40",[],"Does this compile into just one method? Are the arguments reversed in the method call?",
    """
met rec f a b =
    if dyn true then f b a
    else a + b
    : 0
f (dyn 1) (dyn 2)
    """

let test42 =
    "test42",[],"Do partial active patterns work?",
    """
inl f x on_fail on_succ =
    match x with
    | x : int64 -> on_succ (x,"is_int64")
    | x : int32 -> on_succ (x,"is_int32",x*x)
    | x -> on_fail()

inl m m1 = function
    | @m1 (q,w,e) -> q,w,e
    | @m1 (q,w) -> q,w
    | @m1 q -> q
    | x -> error_type "The call to m1 failed."

m f 2
    """

let test43 =
    "test43",[tuple],"Do extension active patterns work?",
    """
// This test is a bit strange since I expect the extension patterns to be used to implement views
// instead of doing general tranformations.
inl f x on_fail on_succ =
    match x with
    | .tup, n, (_ :: _ as x) when n = tuple_length x -> Tuple.repeat n x |> on_succ
    | .cons, n, x -> "rest" :: Tuple.repeat n x |> Tuple.rev |> on_succ 
    | .var, x -> on_succ ("is_var",x)
    | _ -> on_fail()

inl m m1 = function
    | #m1 (q,w,e) -> q, w, e
    | #m1 (q,w) -> q, w
    | #m1 (a :: b) -> a :: b
    | #m1 q -> q
    | _ -> error_type "The call to m1 failed."

m f true, // Tuple1(true, true, "rest")
m f (2.2,3.3), // Tuple3(Tuple2(2.200000f, 3.300000f), Tuple2(2.200000f, 3.300000f))
m f ("a","b","c"), // Tuple5(Tuple4("a", "b", "c"), Tuple4("a", "b", "c"), Tuple4("a", "b", "c"))
m f (1,2,3,4) // Tuple7(Tuple6(1L, 2L, 3L, 4L), Tuple6(1L, 2L, 3L, 4L), "rest")
    """

let test44 =
    "test44",[],"Do or extension active patterns work?",
    """
inl f x on_fail on_succ =
    match x with
    | .tup, 4, x -> on_succ x // This one does not get triggered due to not being in m
    | .tup, 3, x -> on_succ x
    | .tup, 2, x -> on_succ x
    | .var, x -> on_succ ("is_var",x)
    | _ -> on_fail()

inl m m1 = function
    | #m1 (x,_,_ | x,_ | x) -> x
    | _ -> error_type "The call to m1 failed."

// Tuple4(Tuple1("is_var", true), 2.200000f, "a", Tuple3("is_var", Tuple2(1L, 2L, 3L, 4L)))
m f true, m f (2.2,3.3), m f ("a","b","c"), m f (1,2,3,4)
    """

let test45 =
    "test45",[],"Do `type` and `when` patterns work with combination extension patterns?",
    """
inl f x on_fail on_succ =
    match x with
    | _, _, x -> on_succ x
    | _, x -> on_succ x

inl m m1 = function
    | #m1 ((a,b,c): (int64,int64,int64)) -> "is_int64_int64_int64"
    | #m1 ((a,b): (int64,int64)) -> "is_int64_int64"
    | #m1 (x : bool) -> "is_bool"
    | #m1 x -> "is_x"

inl m' n m1 = function
    | #m1 ((a,b,c): (int64,int64,int64) when n > 5) -> "is_int64_int64_int64, n > 5"
    | #m1 ((a,b): (int64,int64) when n > 5) -> "is_int64_int64, n > 5"
    | #m1 ((a,b): (int64,int64) when n <= 5) -> "is_int64_int64, n <= 5"
    | _ -> "???"

// Tuple5("is_int64_int64", "is_bool", "is_int64_int64, n > 5", "is_int64_int64, n <= 5", "???")
m f (1,1), m f true, m' 6 f (2,2), m' 5 f (2,2), m' 1 f 123.456
    """

let test46 =
    "test46",[],"Does the module pattern work?",
    """
inl f {a b c} = a + b + c
inl x =
    {
    a=1
    b=2
    c=3
    }

f {x with a = 4}
    """

let test47 =
    "test47",[],"Does the nested module pattern work?",
    """
inl f {name {p with x y}} = name,(x,y)
inl x = { name = "Coord" }

f {x with 
    p = { x = 1
          y = 2 }}
    """

let test48 =
    "test48",[],"Does the nested module pattern with rebinding work?",
    """
inl f {name {p with y=y' x=x'}} = name,(x',y')
inl x = { name = "Coord" }
f {x with 
    p = { x = 1
          y = 2 }}
    """

let test49 =
    "test49",[],"Does the lens pattern work? Does self work? Does the semicolon get parsed properly?",
    """
inl x = { a = { b = { c = 3 } } }

inl f {x.a.b with c q} = c,q
f {x.a.b with q = 4; c = self + 3; d = {q = 12; w = 23}}
    """

let test54 =
    "test54",[tuple],"Does the monadic bind `inm` work?",
    """
inl on_succ a = (a,())
inl on_log x = ((),Tuple.singleton x)
inl (>>=) (a,w) f = // The writer monad.
    inl a',w' = f a
    (a',Tuple.append w w')

inl add x y = x + y |> on_succ

inm x = add 1 1
inm _ = on_log x
inm y = add 3 4
inm _ = on_log y
inm z = add 5 6
inm _ = on_log z
on_succ (x+y+z) // Tuple2(20L, Tuple1(2L, 7L, 11L))
    """

//let test55 =
//    "test55",[array;console;parsing4],"Does the v4 of the (monadic) parsing library work? Birthday Cake Candles problem.",
//    """
////https://www.hackerrank.com/challenges/birthday-cake-candles
//open Console
//open Parsing
//
//inl solution x = 
//    inl f = 
//        inm n = parse_int
//        inm ar = parse_n_array parse_int n 
//        Array.foldl (inl (min,score as s) x ->
//            if x > score then (1,x)
//            elif x = score then (min+1,score)
//            else s
//            ) (dyn (0,mscorlib ."System.Int64" .MinValue)) ar
//        |> fst
//        |> writeline
//        |> succ
//    f x
//
//run_with_unit_ret (readall()) solution
//    """
//
//let test56 =
//    "test56",[array;console;parsing4],"Does the v4 of the (monadic) parsing library work? Diagonal Sum Difference problem.",
//    """
////https://www.hackerrank.com/challenges/diagonal-difference
//open Console
//open Parsing
//
//inl abs x = if x >= 0 then x else -x
//
//inl f =
//    inm n = parse_int
//    inm ar = parse_n_array parse_int (n*n)
//    inl load row col = 
//        inl f x = x >= 0 || x < n
//        assert (f row && f col) "Out of bounds."
//        ar (n * row + col)
//    met rec loop (!dyn i) (d1,d2 as s) =
//        if i < n then loop (i+1) (d1 + load i i, d2 + load i (n-i-1))
//        else s
//        : s
//    inl a,b = loop 0 (0,0)
//    abs (a-b) 
//    |> writeline
//    |> succ
//
//run_with_unit_ret (readall()) f
//        """
//
//let test57 =
//    "test57",[array;console;parsing4],"Speed test for the v4 of the parsing library.",
//    """
////https://www.hackerrank.com/challenges/diagonal-difference
//open Console
//open Parsing
//
//inl abs x = if x >= 0 then x else -x
//
//inl f = 
//    Tuple.repeat 240 parse_int |> tuple
//    |>> (Tuple.foldl (+) 0 >> writeline >> succ)
//    
//run_with_unit_ret (readall()) f
//        """

let test58 =
    "test58",[array;console],"Does the fold function get duplicated?",
    """
open Console

inl ar = array_create 128 (int64,int64)
Array.foldl (inl a,b c,d -> a+c,b+d) (dyn (1,2)) ar
|> inl a,b -> a*b
|> writeline
    """

//let test59 =
//    "test59",[parsing4;console],"Does sprintf work for v4?",
//    """
//inl a = dyn 1
//inl b = dyn 2
//Parsing.sprintf "%i + %i = %i" a b (a+b) |> ignore
//Console.printfn "(%i,%f,%b,%s)" 1 2.0 true "4"
//    """
//
//let test60 =
//    "test60",[array;queue;parsing4;console],"https://www.hackerrank.com/challenges/saveprincess",
//    """
//// A simple dynamic programming problem. It wouldn't be hard to do in F#, but Spiral
//// gives some novel challenges regarding it.
//open Parsing
//
//inl Cell =
//    type
//        .Empty
//        .Princess
//        .Mario
//
//inl empty = pchar 'e' >>% Cell .Empty
//inl princess = pchar 'p' >>% Cell .Princess
//inl mario = pchar '-' >>% Cell .Mario
//
//inl cell = empty <|> princess <|> mario
//
//inl parse_cols n = parse_n_array n cell .>> spaces
//inl parse_rows n = parse_n_array n (parse_cols n)
//
//inl solve n field =
//    inl cur_pos on_succ =
//        met rec loop1 (!dyn row) =
//            met rec loop2 (!dyn col) =
//                if col < n then
//                    match field row col with
//                    | .Mario -> on_succ row col
//                    | _ -> loop2 (col+1)
//                else loop1 (row+1)
//            if row < n then loop2 0
//            else failwith "Mario not found."
//        loop1 0
//    cur_pos <| inl row col ->
//    // init the arrays
//    inl cells_visited = Array.init n (inl _ -> Array.init n false)
//    cells_visited row col <- true
//
//    inl ar = Array.singleton ((row,col), List.empty string)
//
//    met rec loop on_fail on_succ =
//        inl row,col = Queue.dequeue p
//        match ar row col with
//        | .Princess -> on_succ()
//        | _ ->
//            inl up = (row-1,col),"UP"
//            inl down = (row+1,col),"DOWN"
//            inl left = (row,col-1),"LEFT"
//            inl right = (row,col+1),"RIGHT"
//            inl is_valid x = x >= 0 && x < n
//            inl is_in_range (row,col) = is_valid row && is_valid col
//            inl select (p,move) rest =
//                if is_in_range p then 
//
//inl f =
//    inm n = parse_int
//    inm ar = parse_rows n
//
//    """

let test61 =
    "test61",[],"Does dyn act like id on already dyned variables? It should not.",
    """
inl x = dyn false
dyn x || dyn x || dyn x

// The following is in fact the correct behavior. What happens is that x gets assumed to be true or false
// in one of the branches and then rewritten before being passed into dyn again and turned into a new variable.

//let (var_16: bool) = false
//if var_16 then
//    true
//else
//    let (var_17: bool) = false
//    if var_17 then
//        true
//    else
//        false
    """

let test62 =
    "test62",[],"Do && and || work correctly?",
    """
inl a,b,c,d,e = dyn (true, false, true, false, true)
met f x = x
f a && f b || f c && f d || f e

//let (var_21: bool) = method_14((var_16: bool))
//let (var_23: bool) = (var_21 && method_14((var_17: bool)))
//if var_23 then
//    true
//else
//    let (var_24: bool) = method_14((var_18: bool))
//    let (var_26: bool) = (var_24 && method_14((var_19: bool)))
//    (var_26 || method_14((var_20: bool)))
    """

let test63 =
    "test63",[list],"Does the List.empty work?",
    """
open List
cons 1 (cons 2 (cons 3 (empty int64)))
    """

let test64 =
    "test64",[list],"Does the list pattern work?",
    """
open List
inl l = empty int64 |> dyn
match l with
| #list (a,b,c) -> a+b+c
| #list (a,b) -> a*b
| _ -> 0
    """

let tests =
    [|
    test1;test2;test3;test4;test5;test6;test7;test8;test9
    test10;test11;test12;test13;test14;test15;test16;test17;test18;test19
    test20;test21;test22;test23;test24;test25;test26;test27;test28;test29
    test30;test31;test32;test33;test35;test38
    test40;test42;test43;test44;test45;test46;test47;test48;test49
    test54;test58;test61;test62;test63;test64
    hacker_rank_1
    |] |> Array.map module_

let run_test' is_big_test (name,aux,desc,body as m) =
    let main_module = module_ m
    printfn "%s - %s" name desc
    if is_big_test then
        let x = spiral_peval main_module (System.IO.Path.Combine(__SOURCE_DIRECTORY__,"output.txt"))
        printfn "Time spent in renaming: %A" total_time
        //printfn "%A" x
    else
        let x = spiral_peval main_module (System.IO.Path.Combine(__SOURCE_DIRECTORY__,"output.fsx"))
        printfn "Time spent in renaming: %A" total_time
        printfn "%A" x

let run_test is_big_test name =
    let (Module(N x)) = Array.find (fun (Module(N(name',aux,desc,body))) -> name = name') tests
    run_test' is_big_test x

    //System.Threading.Thread(System.Threading.ThreadStart f, 1024*1024*16).Start()

run_test' false test13

