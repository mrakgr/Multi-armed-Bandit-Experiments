﻿module Spiral.Tests
open Lib
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
| .A, .A, _, _ -> 1
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
inl x = 2 * 22 .+ 33

inl f op a b = op a b
f (*) 2 x
    """

let test22 = // Do unary operators work?
    "test22",
    """
inl t1 x = -x
inl t2 x = `x
inl t3 x = .(x)
t2 true, t3 "asd", t1 2.2
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
(.A, (2,3)) |> a |> dyn |> b
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

let test33 = // Does a simple loop have superlinear scaling?
    "test33",
    """
inl rec loop = function
    | i when i > 0 -> loop (i-1)
    | 0 -> ()
loop 100000
    """

let test34 = // Does parse_n_ints blow up the code size? Does it scale linearly.
    "test34",
    """
inl console = mscorlib."System.Console"
inl (|>>) = Parsing."|>>"
inl end x = ()
inl parse f = Parsing.run (console.ReadLine()) (Parsing.parse_n_ints 40 |>> f) end

parse <| inl _ -> ()
    """

let test29 = // Does a simple int parser work?
    "test29",
    """
inl console = mscorlib ."System.Console"
inl t =
    type
        int64
        int64, int64
        string
        Parsing.List int64

inl result =
    Parsing.run "12 34 " (Parsing.parse_ints) <| function
        | .Succ, x -> t x
        | .FatalFail, er | .Fail, (_, er) -> t er
        | .FetchType -> t ""
        | x -> error_type "Got a strange input."

match result with
| .ListCons,(a,(.ListCons,(b,.ListNil))) as x ->
    console.Write a
    console.WriteLine()
    console.Write b
    console.WriteLine()
    ()
| _ -> ()
    """

open System.Threading
let run f = Thread(ThreadStart f,1024*1024*8).Start() // It stack overflows without being spun on a separate thread.
    
run <| fun _ ->
    let x = spiral_peval [tuple;parsing] test34
    //printfn "%A" x
    ()

//Function Name	Exclusive Samples %	Inclusive Samples	Exclusive Samples	Inclusive Samples %
//Microsoft.FSharp.Collections.FSharpMap`2[System.__Canon,System.__Canon].Equals	43.26	3,614	3,138	49.82
//Microsoft.FSharp.Collections.FSharpMap`2[System.__Canon,System.__Canon].ComputeHashCode	30.58	2,670	2,218	36.81
//[clr.dll]	7.28	3,500	528	48.25
//Microsoft.FSharp.Collections.FSharpMap`2[System.__Canon,System.__Canon].Map	2.89	462	210	6.37
//Microsoft.FSharp.Core.LanguagePrimitives+HashCompare.GenericHashParamObj	2.73	198	198	2.73
//Microsoft.FSharp.Core.LanguagePrimitives+HashCompare.GenericEqualityObj	1.87	136	136	1.87
//[mscorlib.ni.dll]	1.39	6,297	101	86.81
//Microsoft.FSharp.Collections.MapTreeModule.foldOpt	0.94	204	68	2.81
//Spiral.Main+TypedExpr.Equals	0.91	3,614	66	49.82
//Spiral.Main.Equals$cont@177-2	0.81	141	59	1.94
//