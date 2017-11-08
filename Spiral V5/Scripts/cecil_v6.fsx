#load "load-references-release.fsx"

open Mono.Cecil

let resolver = new DefaultAssemblyResolver()
let reader = ReaderParameters(ReadingMode.Immediate, AssemblyResolver=resolver)
let mscorlib_path = @"C:\Windows\Microsoft.NET\Framework64\v4.0.30319\mscorlib.dll"
let mscorlib = AssemblyDefinition.ReadAssembly(mscorlib_path,reader)