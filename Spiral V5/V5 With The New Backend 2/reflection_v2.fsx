open System
open System.Reflection

let system = Reflection.Assembly.Load("system, Version=4.0.0.0, Culture=neutral, PublicKeyToken=b77a5c561934e089")

//let del = system.GetType("System.Diagnostics.DataReceivedEventHandler")
//
//let inv = del.GetMethod("Invoke")
//inv.GetParameters().[1].ParameterType

let proc = system.GetType("System.Diagnostics.Process")

proc.GetEvent("ErrorDataReceived")