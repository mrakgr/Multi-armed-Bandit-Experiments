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
    if n > 0 then Add(Lit 1, add (n-1))
    else Lit 1

open System.Threading
let run f = Thread(ThreadStart f,134217728*4).Start() // It stack overflows without being spun on a separate thread.
    
run <| fun _ ->
    let stopwatch = System.Diagnostics.Stopwatch.StartNew()
    let n = 1000*1000
    printfn "%A" (intr (add n))
    printfn "time elapsed = %A" stopwatch.Elapsed
    
    stopwatch.Restart()
    printfn "%A" (intr_cps (add n) id)
    printfn "time elapsed for cps = %A" stopwatch.Elapsed