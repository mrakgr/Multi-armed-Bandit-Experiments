open System
open System.Reflection

let x = Reflection.Assembly.Load("mscorlib")
let array_type = x.GetType("System.Array")
let methods = array_type.GetMethods()
let method_types (x: MethodInfo) = x.GetParameters(), x.ReturnParameter
let param = methods |> Array.map (fun x -> x.Name, method_types x)
let members = array_type.GetMembers()
let o = array_type.InvokeMember("CreateInstance",BindingFlags.Default,null,null,[|typeof<int>,[|10|]|])

let ar = 10
let cr = array_type.GetMethod("CreateInstance",[|typeof<int>;ar.GetType()|])

