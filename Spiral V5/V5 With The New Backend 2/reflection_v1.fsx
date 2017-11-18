open System
open System.Reflection

let fs = Assembly.Load("FSharp.Core")

let array_tys =
    fs.GetTypes()
    |> Array.filter (fun x -> x.FullName.Contains "Array")

let a = fs.GetType("Microsoft.FSharp.Collections.ArrayModule").GetMethod("SortWith").MakeGenericMethod([|typeof<int64>|])

let operators =
    fs.GetTypes()
    |> Seq.find (fun x ->
        x.Name = "Operators"
        )

let is_null = operators.GetMethod("IsNull")

let atr = is_null.CustomAttributes |> Seq.head
atr.ConstructorArguments |> Seq.head |> fun x -> x.Value :?> string
let compilation_source_name_attr = typeof<Microsoft.FSharp.Core.CompilationSourceNameAttribute>
atr.AttributeType = compilation_source_name_attr