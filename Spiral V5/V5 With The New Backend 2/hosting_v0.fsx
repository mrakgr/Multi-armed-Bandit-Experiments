#load "../Scripts/load-project-release.fsx"

open System
open Microsoft.CodeAnalysis.CSharp.Scripting

let await x = x |> Async.AwaitTask |> Async.RunSynchronously
let result = await <| CSharpScript.EvaluateAsync("int x = 5*5; int y = 5; x") //Note the last x is not contained in a proper statement
Console.WriteLine(result)

match result with
| :? int as x -> x

