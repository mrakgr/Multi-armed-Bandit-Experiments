#load "load-references-release.fsx"

open System
open System.Runtime.CompilerServices
open Mono.Cecil
open Mono.Cecil.Rocks

type SpiralType =
    | IntT
    | StringT
    | TupleT of SpiralType list

let module_ = ModuleDefinition.CreateModule("TypeTokenFactory",ModuleKind.Console)
let r1 = module_.ImportReference(typeof<obj>)
let r2 = module_.ImportReference(typeof<obj>)

let table = ConditionalWeakTable()
table.Add(r1,IntT)
table.Add(r2,StringT)

let mscorlib_path = @"C:\Windows\Microsoft.NET\Framework64\v4.0.30319\mscorlib.dll"
let mscorlib = AssemblyDefinition.ReadAssembly(mscorlib_path)

let dictionary_type =
    mscorlib.Modules.[0].Types
    |> Seq.find (fun x -> x.Name = "Dictionary`2")

let dict_ins = dictionary_type.MakeGenericInstanceType([|r1;r2|])
// Lacks the Methods field...