#load "load-references-release.fsx"

open Mono.Cecil

let resolver = new DefaultAssemblyResolver()
let assembly_load fullname = resolver.Resolve(AssemblyNameReference.Parse(fullname)).MainModule
let mscorlib = assembly_load "mscorlib"
let fsharp_core = assembly_load "FSharp.Core, Version=4.4.0.0, Culture=neutral, PublicKeyToken=b03f5f7f11d50a3a"

let bool = mscorlib.GetType("System.Boolean")
let ops = fsharp_core.GetType("Microsoft.FSharp.Core.Operators")
let isNull =
    ops.Methods
    |> Seq.find (fun x -> x.Name = "IsNull")

isNull.ReturnType.Resolve() = bool // false
// Is there any way to make this resolve to the same type?