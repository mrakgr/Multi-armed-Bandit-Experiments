// The weak tables are no good for hash consing as they don't use structural equality.

open System
open System.Runtime.CompilerServices

type ManagedClass = Var of int

let mc1 = Var 1
let mc2 = Var 2
let mc3 = Var 3

let f () =
    let cwt = ConditionalWeakTable()
    cwt.Add(mc1, ref mc1)
    cwt.Add(mc2, ref mc2)
    cwt.Add(mc3, ref mc3)

    cwt

let cwt = f ()
//GC.Collect()
match cwt.TryGetValue (Var 1) with
| true, v -> printfn "%A" v
| false, _ -> printfn "no key found"
