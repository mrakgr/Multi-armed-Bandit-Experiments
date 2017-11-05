open System
open System.Reflection

let assemblyName =  //"FSharp.Core, Version=4.4.0.0, Culture=neutral, PublicKeyToken=b03f5f7f11d50a3a")
AssemblyName("mscorlib").EscapedCodeBase
let path = Uri(assemblyName.EscapedCodeBase).LocalPath


let a = typeof<unit>.Assembly
a.FullName


