﻿module Spiral.Tests
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

let test8 =
    "test8",[],"Does the basic union type work?",
    """
type option_int = 
    .Some, 1 
    .None

met x = box option_int .None
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
inl ab = box ab
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
inl ab = box ab
met x = (ab .A, ab .A, ab .A, ab .A)
match x with
| .A, .A, _, _ -> 1
| _, _, .A, .A -> 2
| .A, .B, .A, .B -> 3
| _ -> 4
    """

let test11 = // 
    "test11",[],"Do the nested patterns work on dynamic data?",
    """
type a = (1,2)
type b = (1,a,a)
inl a,b = box a, box b
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
inl int_expr = box (expr int64)
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
inl int_expr = box (expr int64)
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
    : int64
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
    float64
if dyn true then box t 0
else box t 0.0
    """

let test17 = // 
    "test17",[],"Do modules work?",
    """
inl m =
    inl x = 2
    inl y = 3.4
    inl z = "123"
    {x y z}
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

inl a = ref <| term_cast (inl a, b -> a + b) (int64,int64)
a := term_cast (inl a, b -> a * b) (int64,int64)
a() |> ignore

inl a = array_create 10 int64
a 3 <- 2
a 3 |> ignore

inl a = array_create 3 id // Is supposed to be unit and not printed.
a 1 <- id
a 1 |> ignore
    """

let test19 =
    "test19",[],"Does term casting for functions work?",
    """
inl add a b (c, (d, e), f) = a + b + c + d + e + f
inl f = term_cast (add 8 (dyn 7)) (int64,(int64,int64),int64)
f (1,(2,5),3)
    """

let test20 = // 
    "test20",[],"Does pattern matching on union non-tuple types work? Do type annotation patterns work?",
    """
type t = 
    int64
    float64
inl x = box t 3.5
match x with
| q : int64 -> x * x
| q : float64 -> x + x
    """

let test21 = // 
    "test21",[],"Does defining user operators work?",
    """
inl (.+) a b = a + b
inl x = 2 * 22 .+ 33

inl f op a b = op a b
f (*) 2 x
    """

let test22 = 
    "test22",[],"Do unary operators work?",
    """
inl t1 x = dyn <| -x
inl t3 x = .(x)
t1 2.2, t3 "asd"
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

inl x = (1,1) |> box t |> dyn
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

inl t = box (List int64)
inl nil = t .ListNil
inl cons x xs = t (.ListCons, (x, xs))

met rec sum (!dyn s) l = 
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
(.A, (2,3)) |> box a |> dyn |> box b
    """

let test32 = // 
    "test32",[],"Do the .NET methods work inside methods?",
    """
inl to_int64 = mscorlib ."System.Convert" .ToInt64
met f = to_int64 (dyn 'a')
f
    """

let test33 = // 0.42s
    "test33",[],"Does a simple loop have superlinear scaling?",
    """
inl rec loop = function
    | i when i > 0 -> loop (i-1)
    | 0 -> ()
loop 50000
    """

let test34 =
    "test34",[],"Does a simple stackified function work?",
    """
inl a = dyn 1
inl b = dyn 2
inl add c d = a + b + c + d
met f g c d = g c d
f (stack add) (dyn 3) (dyn 4)
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

match box Res 1 |> dyn with
| x : int64 -> 1
| (a, b) as x -> 2
| _ -> 3
    """

let test36 =
    "test36",[],"Does a simple heapified function work?",
    """
inl a = dyn 1
inl b = dyn 2
inl add c d = a + b + c + d
met f g c d = g c d
f (heap add) (dyn 3) (dyn 4)
    """

let test37 =
    "test37",[],"Does a simple heapified module work?",
    """
inl m = heap { a=dyn 1; b=dyn 2 }
inl add c d = 
    inl {a b} = m
    a + b + c + d
met f g c d = g c d
f (heap add) (dyn 3) (dyn 4)
    """

let test38 =
    "test38",[],"Is type constructor of an int64 an int64?",
    """
box int64 (dyn 1)
    """

let test39 =
    "test39",[],"Does a nested heapified module work?",
    """
inl m = heap {a=dyn 1; b=dyn 2; c' = {q=dyn 3; w=dyn 4}}
inl m' = {m.c' with q=dyn 6}
inl add c d = 
    inl {a b {c' with q w}} = m
    a + b + c + d + q + w
met f g c d = g c d
f (heap add) (dyn 3) (dyn 4)
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

let test41 =
    "test41",[],"Does a nested heapified module work?",
    """
inl m = heap {a=dyn 1; b=dyn 2; c' = stack {q=dyn 3; w=dyn 4}}
inl m' = {m.c' with q=dyn 9}
inl add c d = 
    inl {a b {c' with q w}} = m'
    a + b + c + d + q + w
met f g c d = g c d
f (heap add) (dyn 3) (dyn 4) // 23
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
    "test43",[array],"Do the Array constructors work?",
    """
open Array

empty int64, singleton 2.2
    """

let test44 =
    "test44",[],"Do or extension active patterns work?",
    """
inl f x on_fail on_succ =
    match x with
    | [tup: 4, x] -> on_succ x // This one does not get triggered due to not being in m
    | [tup: 3, x] -> on_succ x
    | [tup: 2, x] -> on_succ x
    | [var: x] -> on_succ ("is_var",x)
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
    | _, (_, x) -> on_succ x
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

let test50 =
    "test50",[array],"Do the Array init and fold work?",
    """
open Array

inl ar = init 6 (inl x -> x+1)
foldl (+) (dyn 0) ar, foldr (*) ar (dyn 1)
    """

let test51 =
    "test51",[array],"Do the Array map and filter work?",
    """
open Array

inl ar = init 16 id
map ((*) 2) ar
|> filter ((<) 15)
    """

let test52 =
    "test52",[array],"Does the Array concat work?",
    """
open Array

inl ar = init 4 (inl _ -> init 8 id)
concat ar
    """

let test53 =
    "test53",[array],"Does the Array append work?",
    """
open Array

inl ar = inl _ -> init 4 id
append (ar (), ar (), ar())
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

let test55 =
    "test55",[],"Does the type literal rebind pattern work?",
    """
inl f = .QWE,.66,.2.3
match f with
| .(a), .(b), .(c) -> a,b,c
    """

let test56 =
    "test56",[],"Does term casting with an unit return get printed properly?",
    """
inl add a, b = ()
inl k = term_cast add (int64,int64)
k (1, 2)
    """

let test57 =
    "test57",[],"Does the new module creation syntax work?",
    """
inl a = 1
inl b = 2
inl d = 4
{a b c = 3; d; e = 5}
    """

let test58 =
    "test58",[array],"Does the fold function get duplicated?",
    """
inl ar = array_create 128 (int64,int64)
Array.foldl (inl a,b c,d -> a+c,b+d) (dyn (1,2)) ar
|> inl a,b -> a*b
    """

let test59 =
    "test59",[],"Does the new named tuple syntax work?",
    """
inl f [a:q b:w c:e] = q,w,e
f [ a : 1; b : 2; c : 3 ]
    """

let test60 =
    "test60",[],"Is the trace being correctly propagated for TyTs?",
    """
inl a = dyn 1
inl b = dyn 2
inl c = dyn 3
4 + int64
    """

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
    """

let test63 =
    "test63",[list],"Do the list constructors work?",
    """
open List
cons 1 (cons 2 (singleton 3))
    """

let test64 =
    "test64",[tuple;list],"Does the list pattern work?",
    """
open List

match dyn (empty int64) with
| #lw (a,b) -> a + b + 10
| #lw (x :: x2 :: xs) -> x + x2
| #lw (x :: xs) -> 55
| #lw () -> 0
| _ -> 1 // Does not get triggered.
    """

let test65 =
    "test65",[tuple;list],"Do the list module folds work?",
    """
open List

foldl (+) 0.0 (dyn (empty float64)),
foldr (+) (dyn (empty float64)) 0.0f64
    """

let test66 =
    "test66",[tuple;list],"Does the list module concat (and by extension append) work?",
    """
open List

inl a = cons 3 () |> cons 2 |> cons 1 |> dyn
inl b = cons 6 () |> cons 5 |> cons 4 |> dyn
inl c = dyn (cons a (singleton b))
concat c
    """

let test67 =
    "test67",[tuple;list],"Does the list module map work?",
    """
open List

inl a = cons 3 () |> cons 2 |> cons 1 |> dyn

map ((*) 2) a
    """

let test68 =
    "test68",[tuple;list],"Is it possible to make a list of lists?",
    """
open List

inl a = empty int64 |> dyn
empty a
    """

let test69 =
    "test69",[tuple;list],"Does the list module init work?",
    """
open List

init 10 (inl x -> 2.2)
    """

let test70 =
    "test70",[],"Does the argument get printed on a type error?",
    """
inl a: float64 = 5
()
    """

let test71 =
    "test71",[],"Does the recent change to error printing work?",
    """
55 + id
    """

let test72 =
    "test72",[],"Does any kind of literal work in the named tuple syntax?",
    """
inl f = function
    | [1 : x] -> x
    | [true: x] -> x
    | [add: a,b] -> a+b
    | [3.3] -> 6.6

f [1: 1], f [true: 2], f [add: 1,2], f [3.3]
    """

let test73 =
    "test73",[],"Do the or module patterns work?",
    """
inl x = {a=1; c=3}
inl f = function
    | {(a | b)=t} -> t
inl g = function
    | {(a=t) | (b=t)} -> t
f x, g x
    """

let test74 =
    "test74",[],"Do the xor module patterns work?",
    """
inl x = {b=2; c=3}
inl f = function
    | {(((a | a) ^ (b | b)))=t} -> t
inl g = function
    | {(a=t) ^ (b=t)} -> t
f x, g x
    """

let test75 =
    "test75",[],"Does the not module pattern work?",
    """
inl x = {b=2; c=3}
inl f = function
    | {!a b} -> b
f x
    """

let test76 =
    "test76",[],"Do the xor module patterns work?",
    """
inl x = {b=2; c=3}
inl f = function
    | {b ^ c} -> c
f x
    """

let test77 =
    "test77",[],"Do the xor module patterns work?",
    """
inl x = {b=2; c=3}
inl f = function
    | {(!a) ^ c} -> c
f x
    """

let test78 =
    "test78",[tuple],"Do the tuple scan functions work?",
    """
inl x = 1,2,3,4
Tuple.scanl (+) 0 x, Tuple.scanr (+) x 0
    """

let test79 =
    "test79",[arrayn],"Does the ArrayN init work? Do set and index for the new array module work?",
    """
inl ar = ArrayN.init (10,10) (inl (a,b) -> a*b)
inl x = ar.index (2,2)
ar.set (2,2) (x+100)
ar.index (2,2)
    """

let test80 =
    "test80",[queue;console],"Does the Queue module work?",
    """
open Console
inl queue = Queue.create 1 int64
inl rec dequeue n =
    if n > 0 then queue .dequeue () |> writeline; dequeue (n-1)
    else ()
Tuple.iter (queue .enqueue) (Tuple.range (1,4))
dequeue 2
Tuple.iter (queue .enqueue) (Tuple.range (1,4))
Tuple.iter (queue .enqueue) (Tuple.range (1,4))
dequeue 2
dequeue 4
dequeue 4
    """

let parsing1 = 
    "parsing1",[parsing;console],"Does the Parsing module work?",
    """
open Parsing
open Console

inl p = 
    succ 1
    |>> writeline

run_with_unit_ret (readall()) p
    """

let parsing2 = 
    "parsing2",[parsing;console],"Does the Parsing module work?",
    """
open Parsing
open Console

inl p = 
    pdigit
    |>> writeline

run_with_unit_ret (dyn "2") p
    """

let parsing3 = 
    "parsing3",[parsing;console],"Does the Parsing module work?",
    """
open Parsing
open Console

inl p = 
    pstring "qwe"
    |>> writeline

run_with_unit_ret (dyn "qwerty") p
    """

let parsing4 = 
    "parsing4",[parsing;console],"Does the Parsing module work?",
    """
open Parsing
open Console

inl p = 
    parse_int
    |>> writeline

run_with_unit_ret (dyn "1 2 3") p
    """

let parsing5 =
    "parsing5",[parsing;console],"Does the Parsing module work?",
    """
open Parsing
open Console

inl p = 
    parse_array {parser=parse_int; typ=int64; n=16}
    |>> writeline

run_with_unit_ret (readall()) p
    """

let parsing6 =
    "parsing6",[parsing;console],"Do the printf's work?",
    """
open Parsing
open Console

inl a,b,c = dyn (1,2,3)
sprintf "%i + %i = %i" a b c |> ignore
printfn "(%i,%i,%i)" a b c
    """

let parsing7 =
    "parsing7",[array;console;parsing],"Does the parsing library work? Birthday Cake Candles problem.",
    """
//https://www.hackerrank.com/challenges/birthday-cake-candles
open Console
open Parsing

inl p = 
    inm n = parse_int
    inm ar = parse_array {parser=parse_int; typ=int64; n} 
    Array.foldl (inl (min,score as s) x ->
        if x > score then (1,x)
        elif x = score then (min+1,score)
        else s
        ) (dyn (0,mscorlib ."System.Int64" .MinValue)) ar
    |> fst
    |> writeline
    |> succ
        
run_with_unit_ret (readall()) p
    """

let parsing8 =
    "parsing8",[array;console;parsing],"Does the parsing library work? Diagonal Sum Difference problem.",
    """
//https://www.hackerrank.com/challenges/diagonal-difference
open Console
open Parsing

inl abs x = if x >= 0 then x else -x

inl f =
    inm n = parse_int
    inm ar = parse_array {parser=parse_int; typ=int64; n=n*n}
    inl load row col = 
        inl f x = x >= 0 || x < n
        assert (f row && f col) "Out of bounds."
        ar (n * row + col)
    met rec loop (!dyn i) (d1,d2 as s) =
        if i < n then loop (i+1) (d1 + load i i, d2 + load i (n-i-1))
        else s
        : s
    inl a,b = loop 0 (0,0)
    abs (a-b) 
    |> writeline
    |> succ

run_with_unit_ret (readall()) f
        """

let loop1 =
    "loop1",[loops;console],"Does the Loop module work?",
    """
open Console
open Loops
//If we list all the natural numbers below 10 that are multiples of 3 or 5, we get 3, 5, 6 and 9. The sum of these multiples is 23.
//Find the sum of all the multiples of 3 or 5 below 1000.

for {from=dyn 999; to=dyn 3; by=dyn <| -1; state=dyn 0; body = inl {state i} ->
    if i % 3 = 0 || i % 5 = 0 then state+i
    else state
    }
|> writeline
    """

let loop2 =
    "loop2",[loops;console],"Does the Loop module work?",
    """
open Console
open Loops

for {from=dyn 3; to=dyn 999; state=dyn 0; body = inl {state i} ->
    if i % 3 = 0 || i % 5 = 0 then state+i
    else state
    }
|> writeline
    """

let loop3 =
    "loop3",[loops;console],"Does the Loop module work?",
    """
open Console
open Loops

for {from=6; to=3; by= -1; state=0; body = inl {state i} ->
    if i % 3 = 0 || i % 5 = 0 then state+i
    else state
    }
|> writeline
    """

let loop4 =
    "loop4",[loops;console],"Does the Loop module work? This particular test should give an error.",
    """
open Console
open Loops

for {from=6; to=3; by=0; state=0; body = inl {state i} ->
    if i % 3 = 0 || i % 5 = 0 then state+i
    else state
    }
|> writeline
    """

let loop5 = 
    "loop5",[loops],"Does the Loop module work?",
    """
open Loops

for {from=2; to=2; body = inl {i} -> ()}
    """

let euler2 = 
    "euler2",[loops;console],"Even Fibonacci Numbers.",
    """
open Loops
open Console

while {
    state={sum=dyn 0; a=dyn 1; b=dyn 2}
    cond=inl {b} -> if b <= 4*1000*1000 then true else false
    body=inl {sum a b} -> {sum=if b % 2 = 0 then sum+b else sum; a=b; b=a+b}
    }
|> inl {sum} -> writeline sum
    """

let euler3 = 
    "euler3",[array;loops;console;option],"Largest prime factor",
    """
open Loops
open Console
open Array
open Option

// The prime factors of 13195 are 5, 7, 13 and 29.
// What is the largest prime factor of the number 600851475143 ?

inl math = mscorlib ."System.Math"
inl conv a b = unsafe_convert b a

inl target = dyn 600851475143

inl sieve_length = 
    math.Sqrt(conv float64 target)
    |> conv int64

inl sieve = Array.init (sieve_length+1) (inl _ -> true)
for {from=2; to=sieve_length; body = inl {i} ->
    if sieve i = true then
        for {from=i+i; to=sieve_length; by=i; body = inl {i} -> 
            sieve i <- false
            }
    }

for' {from=sieve_length; to=2; by= -1; state=none int64; body = inl {next state i} ->
    if sieve i = true && target % i = 0 then some i
    else next state
    }
|>  function
    | [Some: result] -> writeline result // 6857
    | [None] -> failwith "No prime factor found!"
    """

let euler4 = 
    "euler4",[array;loops;console],"Largest palindrome product",
    """
//A palindromic number reads the same both ways. The largest palindrome made from the product of two 2-digit numbers is 9009 = 91 × 99.
//Find the largest palindrome made from the product of two 3-digit numbers.

open Loops
open Console

inl reverse_number x =
    while {
        cond=inl {x} -> x > 0 
        state={x x' = dyn 0}
        body=inl {x x'} -> {x=x/10; x'= x'*10+x%10}
        }
    |> inl {x'} -> x'
inl is_palindrome x = x = reverse_number x
for {from=dyn 100; to=dyn 999; state={highest_palindrome=dyn 0}; body=inl {state i} ->
    for {from=i; to=dyn 999; state; body=inl {{state with highest_palindrome} i=j} ->
        inl x = i*j
        if is_palindrome x && highest_palindrome < x then {highest_palindrome=x} else state
        }
    } 
|> inl {highest_palindrome} -> writeline highest_palindrome
    """

let euler5 =
    "euler5",[tuple;loops;console],"Smallest multiple",
    """
//2520 is the smallest number that can be divided by each of the numbers from 1 to 10 without any remainder.
//What is the smallest positive number that is evenly divisible by all of the numbers from 1 to 20?

open Loops
open Console

inl primes = 2,3,5,11,13,17,19
inl non_primes = Tuple.range (2,20) |> Tuple.filter (Tuple.contains primes >> not)
inl step = Tuple.foldl (*) 1 primes
for' {from=step; to=mscorlib."System.Int64".MaxValue; by=step; state= -1; body=inl {next state i} ->
    if Tuple.forall (inl x -> i % x = 0) non_primes then i
    else next state
    }
|> writeline
    """

let hacker_rank_2 =
    "hacker_rank_2",[tuple;array;arrayn;loops;list;option;parsing;console],"Save The Princess",
    """
// https://www.hackerrank.com/challenges/saveprincess
// A simple dynamic programming problem. It wouldn't be hard to do in F#, but Spiral
// gives some novel challenges regarding it.
open Parsing
open Console
open Option
open Array
open Loops

type Cell =
    .Empty
    .Princess
    .Mario

inl empty = pchar '-' >>% box Cell .Empty
inl princess = pchar 'p' >>% box Cell .Princess
inl mario = pchar 'm' >>% box Cell .Mario

inl cell = empty <|> princess <|> mario

inl parse_cols n = parse_array {parser=cell; typ=Cell; n} .>> spaces
inl parse_field n = parse_array {parser=parse_cols n; typ=type (array_create 0 Cell); n}
inl parser = 
    inm n = parse_int 
    inm field = parse_field n
    inl no_pos = none (int64,int64)
    for' {from = 0; near_to=n; state={princess=no_pos; mario=no_pos}; body = inl {next=row state i=r} ->
        for' {from = 0; near_to=n; state; 
            body = inl {next=col state i=c} ->
                match field r c with
                | .Mario -> 
                    inl state = {state with mario=some (r,c)}
                    match state with
                    | {princess=[Some: _]} -> state
                    | _ -> col state
                | .Princess -> 
                    inl state = {state with princess=some (r,c)}
                    match state with
                    | {mario=[Some: _]} -> state
                    | _ -> col state
                | _ -> col state
            finally = row
            }
        }
    |> function
        | {mario=[Some: mario_row, mario_col as mario_pos] princess=[Some: princess_row, princess_col as princess_pos]} ->
            inl cells_visited = ArrayN.init (n,n) (const false)
            cells_visited.set mario_pos true

            inl up_string = dyn "UP"
            inl down_string = dyn "DOWN"
            inl left_string = dyn "LEFT"
            inl right_string = dyn "RIGHT"

            inl up (row,col), prev_moves = (row-1,col), List.cons up_string prev_moves
            inl down (row,col), prev_moves = (row+1,col), List.cons down_string prev_moves
            inl left (row,col), prev_moves = (row,col-1), List.cons left_string prev_moves
            inl right (row,col), prev_moves = (row,col+1), List.cons right_string prev_moves

            inl next_moves = up,down,left,right

            inl is_valid x = x >= 0 && x < n
            inl is_in_range (row,col), _ = is_valid row && is_valid col
            inl is_princess_in_state (row,col), _ = row = princess_row && col = princess_col

            inl start_queue = Array.singleton (mario_pos, List.empty string)
            inl state_type = start_queue.elem_type

            inl solution = ref (none state_type)
            met rec loop queue =
                inl queue =
                    Array.map (inl mario_pos, prev_moves as state ->
                        inl potential_new_states = 
                            Tuple.map (inl move -> 
                                inl new_pos,_ as new_state = move state
                                inl is_valid =
                                    if is_in_range new_state && cells_visited.index new_pos = false then 
                                        if is_princess_in_state new_state then solution := some new_state
                                        cells_visited.set new_pos true
                                        true
                                    else false
                                new_state, is_valid
                                ) next_moves
                        inl bool_to_int x = if x then 1 else 0
                        inl number_of_valid_states = Tuple.foldl (inl s (_,!bool_to_int x) -> s + x) 0 potential_new_states
                        inl new_states = array_create number_of_valid_states state_type
                        Tuple.foldl (inl i (state,is_valid) -> 
                            if is_valid then new_states i <- state; i+1
                            else i
                            ) 0 potential_new_states |> ignore
                        new_states
                        ) queue
                    |> Array.concat
                match solution() with
                | [None] -> loop queue
                | [Some: _,path] -> List.foldr (inl x _ -> Console.writeline x) path ()
                : ()
            loop start_queue
        | _ -> failwith "Current position not found."
    |> succ

//inl str = dyn "3
//---
//-m-
//p--
//    "
run_with_unit_ret (readall()) parser
    """

let hacker_rank_3 =
    "hacker_rank_3",[tuple;array;arrayn;loops;list;parsing;console;queue],"Save The Princess 2",
    """
// https://www.hackerrank.com/challenges/saveprincess2
// A version of this similar to the previous one made in order to test the new queue.
open Parsing
open Console
open Array
open Loops

type Cell =
    .Empty
    .Princess
    .Mario

inl empty = pchar '-' >>% box Cell .Empty
inl princess = pchar 'p' >>% box Cell .Princess
inl mario = pchar 'm' >>% box Cell .Mario

inl cell = empty <|> princess <|> mario

inl parse_cols n = parse_array {parser=cell; typ=Cell; n} .>> spaces
inl parse_field n = parse_array {parser=parse_cols n; typ=type (array_create 0 Cell); n}
inl parser ret = 
    inm n = parse_int .>> parse_int .>> parse_int
    inm field = parse_field n
    for' {from = 0; near_to=n; state={n field}; 
        body = inl {next=row state i=r} ->
            for' {from = 0; near_to=n; state; 
                body = inl {next=col state i=c} ->
                    match field r c with
                    | .Mario -> 
                        inl state = {state with mario=r,c}
                        match state with
                        | {princess} -> ret .some state
                        | _ -> col state
                    | .Princess -> 
                        inl state = {state with princess=r,c}
                        match state with
                        | {mario} -> ret .some state
                        | _ -> col state
                    | _ -> col state
                finally = row
                }
        finally = inl _ -> ret .none ()
        }
    |> succ

met main = {
    some = inl {n field mario=(mario_row, mario_col as mario_pos) princess=(princess_row, princess_col as princess_pos)} ->
        inl cells_visited = ArrayN.init (n,n) (const false)
        cells_visited.set mario_pos true

        inl up_string = dyn "UP"
        inl down_string = dyn "DOWN"
        inl left_string = dyn "LEFT"
        inl right_string = dyn "RIGHT"

        inl up (row,col), prev_moves = (row-1,col), List.cons up_string prev_moves
        inl down (row,col), prev_moves = (row+1,col), List.cons down_string prev_moves
        inl left (row,col), prev_moves = (row,col-1), List.cons left_string prev_moves
        inl right (row,col), prev_moves = (row,col+1), List.cons right_string prev_moves

        inl is_valid x = x >= 0 && x < n
        inl is_in_range (row,col), _ = is_valid row && is_valid col
        inl is_princess_in_state (row,col), _ = row = princess_row && col = princess_col

        inl init_state = (mario_pos, List.empty string)
//        inl state_type = type init_state

        ()

//        inl queue = Queue.create () state_type
//        queue.enqueue init_state

//
//        inl print_solution _,path = //List.foldr (inl x _ -> Console.writeline x) path ()
//            List.last path {
//                some = Console.writeline
//                none = inl _ -> failwith "Error: No moves taken."
//                }
//
//        met evaluate_move state move on_fail =
//            inl new_pos,_ as new_state = move state
//            if is_in_range new_state && cells_visited.index new_pos = false then 
//                if is_princess_in_state new_state then print_solution new_state
//                else
//                    cells_visited.set new_pos true
//                    queue.enqueue new_state
//                    on_fail ()
//            else on_fail ()
//            
//        met rec loop () =
//            inl next_moves = up, down, left, right
//            inl state = queue.dequeue()
//            Tuple.foldr (inl move next () -> evaluate_move state move next) next_moves loop ()
//            : ()
//
//        loop ()
    none = inl _ -> failwith "Current position not found."
    }
    

inl str = dyn "3
1 1
---
-m-
p--
    "

run_with_unit_ret (str) (parser main)
    """


let tests =
    [|
    test1;test2;test3;test4;test5;test6;test7;test8;test9
    test10;test11;test12;test13;test14;test15;test16;test17;test18;test19
    test20;test21;test22;test23;test24;test25;test26;test27;test28;test29
    test30;test31;test32;test33;test34;test35;test36;test37;test38;test39
    test40;test41;test42;test43;test44;test45;test46;test47;test48;test49
    test50;test51;test52;test53;test54;test55;test56;test57;test58;test59
    test60;test61;test62;test63;test64;test65;test66;test67;test68;test69
    test70;test71;test72;test73;test74;test75;test76;test77;test78;test79
    test80
    hacker_rank_1;hacker_rank_2
    parsing1;parsing2;parsing3;parsing4;parsing5;parsing6;parsing7;parsing8
    loop1;loop2;loop3;loop4;loop5
    euler2;euler3;euler4;euler5
    |]

open System.IO
open System.Text

let run_test_and_store_it_to_stream stream (name,aux,desc,body as m) =
    let main_module = module_ m
    sprintf "%s - %s:\n%s\n\n" name desc (body.Trim()) |> stream
    match spiral_peval main_module with
    | Succ x | Fail x -> stream x

let output_test_to_string test = 
    match spiral_peval (module_ test) with
    | Succ x | Fail x -> x

let output_test_to_temp test = 
    match spiral_peval (module_ test) with
    | Succ x | Fail x -> 
        let file = if x.Length > 1024*128 then "output.txt" else "output.fsx"
        File.WriteAllText(Path.Combine(__SOURCE_DIRECTORY__,file),x)
        x

let output_tests_to_file file =
    let s = System.Text.StringBuilder()
    Array.iter (run_test_and_store_it_to_stream (s.AppendLine >> ignore)) tests
    File.WriteAllText(Path.Combine(__SOURCE_DIRECTORY__,file),s.ToString())

let make_test_path_from_name name =
    let dir = Path.Combine(__SOURCE_DIRECTORY__,"TestCache")
    Directory.CreateDirectory dir |> ignore
    Path.Combine(dir,name+".txt")

let cache_test (name,aux,desc,body as m) = File.WriteAllText(make_test_path_from_name name, output_test_to_string m)
let rewrite_test_cache () = Array.iter cache_test tests

let get_diff_using_testcache (stream: StringBuilder) (name,aux,desc,body as m) =
    let append x = stream.AppendLine x |> ignore
    let path = make_test_path_from_name name
    if File.Exists path then 
        let original = File.ReadAllText path
        let current = output_test_to_string m
        if original <> current then 
            append <| sprintf "Test %s differs from the one in the cache." name
            append <| sprintf "Original:\n%s" original
            append <| sprintf "Current:\n%s" current
    else cache_test m
    stream

let get_all_diffs () = 
    Array.fold get_diff_using_testcache (StringBuilder()) tests
    |> fun x -> x.ToString()

let speed1 =
    "speed1",[parsing;console],"Does the Parsing module work?",
    """
open Parsing
open Console

inl p = 
    tuple (Tuple.repeat 240 <| term_cast (pint64 .>> spaces) int64)
    |>> (Tuple.foldl (+) 0 >> writeline)

run_with_unit_ret (readall()) p
    """

//rewrite_test_cache()

output_test_to_temp hacker_rank_3
|> printfn "%s"
|> ignore


