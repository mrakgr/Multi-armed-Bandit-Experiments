#load "load-references-release.fsx"

open System
open Mono.Cecil
open Mono.Cecil.Rocks

let mscorlib_path = @"C:\Windows\Microsoft.NET\Framework64\v4.0.30319\mscorlib.dll"
let mscorlib = AssemblyDefinition.ReadAssembly(mscorlib_path).MainModule

let fsharp_core_path = @"C:\WINDOWS\Microsoft.Net\assembly\GAC_MSIL\FSharp.Core\v4.0_4.4.0.0__b03f5f7f11d50a3a\FSharp.Core.dll"
let fsharp_core = AssemblyDefinition.ReadAssembly(fsharp_core_path).MainModule

let typeof_bool = mscorlib.GetType("System.Boolean")
let typeof_int8 = mscorlib.GetType("System.SByte")
let typeof_int16 = mscorlib.GetType("System.Int16")
let typeof_int32 = mscorlib.GetType("System.Int32")
let typeof_int64 = mscorlib.GetType("System.Int64")

let typeof_uint8 = mscorlib.GetType("System.Byte")
let typeof_uint16 = mscorlib.GetType("System.UInt16")
let typeof_uint32 = mscorlib.GetType("System.UInt32")
let typeof_uint64 = mscorlib.GetType("System.UInt64")

let typeof_float32 = mscorlib.GetType("System.Single")
let typeof_float64 = mscorlib.GetType("System.Double")

let typeof_char = mscorlib.GetType("System.Char")
let typeof_void = mscorlib.GetType("System.Void")

let fsharp_func = fsharp_core.GetType("Microsoft.FSharp.Core.FSharpFunc`2")
let fsharp_unit = fsharp_core.GetType("Microsoft.FSharp.Core.Unit")

let CompilationSourceNameAttribute = fsharp_core.GetType("Microsoft.FSharp.Core.CompilationSourceNameAttribute")

