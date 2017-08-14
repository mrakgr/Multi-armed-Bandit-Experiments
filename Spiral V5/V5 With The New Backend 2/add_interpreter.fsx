// I am testing superlinear performance behavior in the main partial evaluator.
// It checks out. Most likely, the problems I am having are due to excessive stack memory usage.

type Expr =
    | Lit of int
    | Add of Expr * Expr

let rec intr = function
    | Lit _ as x -> x
    | Add(Lit a,Lit b) -> Lit <| a + b
    | Add(a,b) -> intr <| Add(intr a, intr b)

let rec intr_cps x ret =
    match x with
    | Lit _ as x -> ret x
    | Add(Lit a,Lit b) -> Lit (a + b) |> ret
    | Add(a,b) -> 
        intr_cps a <| fun a ->
            intr_cps b <| fun b ->
                intr_cps (Add(a, b)) ret

let rec add n =
    if n > 1 then Add(Lit 1, add (n-1))
    else Lit 1

open System.Threading

let mem = 1024*1024*512 // ~536mb
// It stack overflows without being spun on a separate thread.
// By default, the program only has a few mb of stack memory at its disposal.
let run f = Thread(ThreadStart f,mem).Start() 
    
run <| fun _ ->
    let f n =
        let x = add n
        let stopwatch = System.Diagnostics.Stopwatch.StartNew()
        printfn "%A" (intr x)
        printfn "n_%i_std = %A" n stopwatch.Elapsed
    
        stopwatch.Restart()
        printfn "%A" (intr_cps x id)
        printfn "n_%i_cps = %A" n stopwatch.Elapsed
    f <| 1000*1000/2
    f <| 1000*1000
    f <| 1000*1000*2

//Lit 500000
//n_500000_std = 00:00:00.7764730
//Lit 500000
//n_500000_cps = 00:00:00.0800371
//Lit 1000000
//n_1000000_std = 00:00:02.9531043
//Lit 1000000
//n_1000000_cps = 00:00:00.1941828
//Lit 2000000
//n_2000000_std = 00:00:13.7823780
//Lit 2000000
//n_2000000_cps = 00:00:00.2767752