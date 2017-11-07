#load "load-references-release.fsx"

open Mono.Cecil
open Mono.Cecil.Rocks

let resolver = new DefaultAssemblyResolver()
let assembly_load fullname = resolver.Resolve(AssemblyNameReference.Parse(fullname)).MainModule
let mscorlib = assembly_load "mscorlib"
let fsharp_core = assembly_load "FSharp.Core, Version=4.4.0.0, Culture=neutral, PublicKeyToken=b03f5f7f11d50a3a"


let task = mscorlib.Types |> Seq.find (fun x -> x.Name = "Task`1")

let task_cons = task.GetConstructors()

let con =
    task_cons
    |> Seq.toArray
    |> fun x -> x.[8]

let r = con.Parameters.[0].ParameterType

r.IsGenericInstance
let ins = r :?> GenericInstanceType
let args = ins.GenericArguments

mscorlib.GetType("System.Collections.Generic.Dictionary`2").HasGenericParameters