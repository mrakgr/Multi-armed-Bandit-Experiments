open System
open System.Reflection

let fs = Assembly.Load("FSharp.Core")

let array_tys =
    fs.GetTypes()
    |> Array.filter (fun x -> x.FullName.Contains "Array")

fs.GetType("Microsoft.FSharp.Collections.ArrayModule").GetMethod("SortWith").MakeGenericMethod([|typeof<int64>;typeof<int64>|])

