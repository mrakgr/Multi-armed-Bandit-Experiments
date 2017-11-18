#load "load-references-release.fsx"

open Mono.Cecil
open Mono.Cecil.Rocks

let resolver = new DefaultAssemblyResolver()
let mscorlib_path = AssemblyNameReference.Parse("mscorlib")
let mscorlib = resolver.Resolve(mscorlib_path).MainModule

let task = mscorlib.Types |> Seq.find (fun x -> x.Name = "Task`1")

let task_cons = task.GetConstructors()

let con =
    task_cons
    |> Seq.toArray
    |> fun x -> x.[8]

let r = con.Parameters.[0].ParameterType

let ins = r :?> GenericInstanceType
let args = ins.GenericArguments
args
