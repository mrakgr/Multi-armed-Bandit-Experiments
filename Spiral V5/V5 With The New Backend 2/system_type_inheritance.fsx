open System

type SpiralDU =
    | IntT
    | StringT
    | TupleT of SpiralDU list

type SpiralTypeToken(metadata: SpiralDU) =
    inherit Type()

    member val Metadata = metadata

    override __.IsDefined(a,b) = base.IsDefined(a,b)
    override __.GetCustomAttributes(a,b) = base.GetCustomAttributes(a,b)
    override __.GetCustomAttributes(a) = base.GetCustomAttributes(a)
    override __.GUID with get() = base.GUID
    override __.InvokeMember(a,b,c,d,e,f,g,h) = base.InvokeMember(a,b,c,d,e,f,g,h)
    override __.Module with get() = base.Module
    override __.Assembly with get() = base.Assembly
    override __.Name with get() = base.Name
    override __.FullName with get() = base.FullName
    override __.Namespace with get() = base.Namespace
    override __.AssemblyQualifiedName with get() = base.AssemblyQualifiedName
    override __.BaseType with get() = base.BaseType
    override __.GetConstructorImpl(a,b,c,d,e) = base.GetConstructorImpl(a,b,c,d,e)
    override __.GetConstructors(a) = base.GetConstructors(a)
    override __.GetMethodImpl(a,b,c,d,e,f) = base.GetMethodImpl(a,b,c,d,e,f)
    override __.GetMethods(a) = base.GetMethods(a)
    override __.GetField(a,b) = base.GetField(a,b)
    override __.GetFields(a) = base.GetFields(a)
    override __.GetInterface(a,b) = base.GetInterface(a,b)
    override __.GetInterfaces() = base.GetInterfaces()
    override __.GetEvent(a,b) = base.GetEvent(a,b)
    override __.GetEvents(a) = base.GetEvents(a)
    override __.GetPropertyImpl(a,b,c,d,e,f) = base.GetPropertyImpl(a,b,c,d,e,f)
    override __.GetProperties(a) = base.GetProperties(a)
    override __.GetNestedType(a,b) = base.GetNestedType(a,b)
    override __.GetNestedTypes(a) = base.GetNestedTypes(a)
    override __.GetMembers(a) = base.GetMembers(a)
    override __.GetAttributeFlagsImpl() = base.GetAttributeFlagsImpl()
    override __.IsArrayImpl() = base.IsArrayImpl()
    override __.IsByRefImpl() = base.IsByRefImpl()
    override __.IsPointerImpl() = base.IsPointerImpl()
    override __.IsPrimitiveImpl() = base.IsPrimitiveImpl()
    override __.IsCOMObjectImpl() = base.IsCOMObjectImpl()
    override __.GetElementType() = base.GetElementType()
    override __.HasElementTypeImpl() = base.HasElementTypeImpl()
    override __.UnderlyingSystemType with get() = base.UnderlyingSystemType

open System.Collections.Generic
open System.Reflection
let x = Reflection.Assembly.Load("mscorlib")
let dic' = x.GetType("System.Collections.Generic.Dictionary`2")

let key = TupleT [IntT; IntT]
let st = SpiralTypeToken(key) :> Type
let dic_ins_typ = dic'.MakeGenericType [|typeof<int>;typeof<int>|]
let dic_ins_typ' = dic'.MakeGenericType [|st;typeof<int>|]

dic_ins_typ.GetMethod("get_Item",[|typeof<int>|]) // Works
dic_ins_typ'.GetMethod("get_Item",[|st|]) // System.NotSupportedException: Specified method is not supported.