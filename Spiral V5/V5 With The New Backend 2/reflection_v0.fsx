open System
open System.Reflection

let x = Reflection.Assembly.Load("mscorlib")
let array_type = x.GetType("System.Array")

let methods = array_type.GetMethods()
let method_types (x: MethodInfo) = x.GetParameters(), x.ReturnParameter
let param = methods |> Array.map (fun x -> x.Name, method_types x)
let members = array_type.GetMembers()
let o_args: obj[] = [|typeof<int>;[|10|]|]
let o = array_type.InvokeMember("CreateInstance",BindingFlags.InvokeMethod,null,null,o_args)


let ar = 10
let ct_tys = [|typeof<int>.GetType();ar.GetType()|]
let cr = array_type.GetMethod("CreateInstance",ct_tys)
cr

let strb_ty = typeof<System.Text.StringBuilder>
let args: obj[] = [|"Qwe"; 128|]
let strb = Activator.CreateInstance(strb_ty,args)
strb

open System.Collections.Generic
let dic = typeof<Dictionary<int,int>>
let dic' = x.GetType("System.Collections.Generic.Dictionary`2")
let dic_ins_typ = dic'.MakeGenericType [|typeof<int>;typeof<int>|]
let dic_ins = Activator.CreateInstance(dic_ins_typ)
dic_ins_typ.InvokeMember("Add",BindingFlags.InvokeMethod,null,dic_ins,[|0;100|])
