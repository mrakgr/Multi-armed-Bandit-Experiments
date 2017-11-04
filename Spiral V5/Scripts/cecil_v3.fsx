#load "load-references-release.fsx"

open System
open Mono.Cecil
open Mono.Cecil.Rocks

let mscorlib_path = @"C:\Windows\Microsoft.NET\Framework64\v4.0.30319\mscorlib.dll"
let mscorlib = AssemblyDefinition.ReadAssembly(mscorlib_path)

let dictionary_type =
    mscorlib.Modules.[0].Types
    |> Seq.find (fun x -> x.Name = "Dictionary`2")

let add_method =
    dictionary_type.Methods
    |> Seq.find (fun x -> x.Name = "Add")

let mets =
    mscorlib.MainModule.Types
    |> Seq.collect (fun x ->
        x.Methods
        )
    |> Seq.filter (fun x ->
        x.Parameters
        |> Seq.exists (fun x -> x.ParameterType.ContainsGenericParameter)
        )
    |> Seq.toArray

let met = mets |> Array.find (fun x -> x.Name.Contains "AsReadOnly")

let readonly_col = met.ReturnType.Resolve()
let readonly_col' = 
    mscorlib.MainModule.Types
    |> Seq.find (fun x -> x.Name.Contains "ReadOnlyCollection`1")

readonly_col = readonly_col' // true
Object.ReferenceEquals(readonly_col,readonly_col') // true