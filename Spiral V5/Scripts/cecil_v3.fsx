#load "load-references-release.fsx"

open Mono.Cecil
open Mono.Cecil.Rocks

let mscorlib_path = @"C:\Windows\Microsoft.NET\Framework64\v4.0.30319\mscorlib.dll"
let mscorlib = AssemblyDefinition.ReadAssembly(mscorlib_path).MainModule

let task = mscorlib.Types |> Seq.find (fun x -> x.Name = "Task`1")

let task_cons = task.GetConstructors()

let con =
    task_cons
    |> Seq.toArray
    |> fun x -> x.[8]

con.Parameters.[0].ParameterType

