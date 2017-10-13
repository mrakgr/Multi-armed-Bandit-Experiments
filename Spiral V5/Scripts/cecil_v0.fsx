#load "load-references-release.fsx"

open Mono.Cecil
open Mono.Cecil.Cil
open Mono.Cecil.Rocks

let mscorlib_path = @"C:\Windows\Microsoft.NET\Framework64\v4.0.30319\mscorlib.dll"

let mscorlib = AssemblyDefinition.ReadAssembly(mscorlib_path)

let m = mscorlib.Modules.[0]
m.Types.[0].Methods.[0].

//let dictionary_type =
//    mscorlib.Modules.[0].Types
//    |> Seq.find (fun x -> x.Name = "Dictionary`2")
//
//let par1 = dictionary_type.GenericParameters.[0]
//let par2 = 
//    dictionary_type.Methods
//    |> Seq.find (fun x -> x.Name = "Add")
//
//dictionary_type.MakeGenericInstanceType([||])
//
//let m = ModuleDefinition.CreateModule("SpiralTokens",ModuleParameters())
//
//let r = TypeReference("","SpiralType",m,)
//
//
////
////let m = Mono.Cecil.ModuleDefinition.CreateModule("test",ModuleKind.NetModule)
////
////let t = TypeDefinition("test","foo",TypeAttributes.AutoClass)
////t.IsValueType <- true
////t.IsValueType // false
//
