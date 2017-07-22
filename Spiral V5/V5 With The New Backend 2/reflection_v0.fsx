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
dic'.GetGenericArguments()
let dic_ins_typ = dic'.MakeGenericType [|typeof<int>;typeof<int>|]
let dic_ins' = dic_ins_typ.GetConstructor([|typeof<int>|])
dic_ins'.Invoke([|10|])
let dic_ins = Activator.CreateInstance(dic_ins_typ)
dic_ins_typ.InvokeMember("Add",BindingFlags.InvokeMethod,null,dic_ins,[|0;100|])
dic_ins_typ.GetMethod("Add",[|typeof<int>;typeof<int>|])
dic'.ContainsGenericParameters // true
dic_ins_typ.ContainsGenericParameters // false

dic_ins_typ.GetMethod("get_Item",[|typeof<int>|])

let print_dotnet_instance_type type_printer (x: Type) =
    if x.GenericTypeArguments.Length > 0 then
        [|
        x.Namespace 
        x.Name.Split '`' |> Array.head
        "<"
        Array.map type_printer x.GenericTypeArguments |> String.concat ","
        ">"
        |] |> String.concat null
    else
        x.Namespace + x.Name
        

let def_proc (d: Dictionary<_,_>) f t = 
    match d.TryGetValue t with
    | true, v -> v
    | false, _ -> f d t

let rev_type_dict' = ResizeArray()
let map_type: Type -> int = 
    def_proc (Dictionary()) (fun d t ->
        let v = d.Count
        d.Add(t,v)
        rev_type_dict'.Add t
        v)
let rev_map_type i = rev_type_dict'.[i]

open System
type Ty =
    | DotNetMappedT of int // Since Type does not support the Comparable interface, I map it to int.
    | SetT of Set<Ty>

typeof<System.Console>.GetMethod("Write",[|typeof<int>|])

