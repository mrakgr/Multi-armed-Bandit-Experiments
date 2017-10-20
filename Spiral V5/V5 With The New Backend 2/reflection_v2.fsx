open System
open System.Reflection

let mscorlib = Reflection.Assembly.Load("mscorlib")
let fscore = Reflection.Assembly.Load("FSharp.Core")
let system = Reflection.Assembly.Load("system, Version=4.0.0.0, Culture=neutral, PublicKeyToken=b77a5c561934e089")

let proc = system.GetType("System.Diagnostics.Process")

proc.GetEvent("ErrorDataReceived")

let proc' = System.Diagnostics.Process()
proc'.ErrorDataReceived.AddHandler()