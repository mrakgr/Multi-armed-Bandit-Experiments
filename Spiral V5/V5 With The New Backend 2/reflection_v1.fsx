open System
open System.Reflection

let fs = Assembly.Load("FSharp.Core")

let array_tys =
    fs.GetTypes()
    |> Array.filter (fun x -> x.FullName.Contains "Array")

let a = fs.GetType("Microsoft.FSharp.Collections.ArrayModule").GetMethod("SortWith").MakeGenericMethod([|typeof<int64>|])
let b = fs.GetType("Microsoft.FSharp.Collections.ArrayModule").GetMethod("SortWith").MakeGenericMethod([|typeof<int64>|])

a = b // true
Object.ReferenceEquals(a,b) // true



