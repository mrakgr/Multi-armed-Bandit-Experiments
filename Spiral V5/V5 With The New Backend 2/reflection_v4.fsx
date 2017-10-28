open System
open System.Reflection

//let m = Assembly.Load "ManagedCuda, Version=7.5.7.0, Culture=neutral, PublicKeyToken=242d898828717aa0"

type Qwe = 
    | QWE
    | RTY
    | UIO of string

let t = typeof<Qwe>

open Microsoft.FSharp.Reflection
let cases = FSharpType.GetUnionCases(t)
let ins = FSharpValue.MakeUnion(cases.[0],[||]) :?> Qwe
