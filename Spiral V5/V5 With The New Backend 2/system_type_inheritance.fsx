open System

type SpiralType() =
    inherit Type()

    override __.IsDefined(_,_) = false
    override __.GetCustomAttributes(_,_) = null
    override __.GetCustomAttributes(_) = null
    override __.GUID with get() = failwith "Don't call this."
    override __.InvokeMember(_,_,_,_,_,_,_,_) = null
    override __.Module with get() = failwith "Don't call this.": Reflection.Module
    override __.Assembly with get() = null
    override __.Name with get() = null
    override __.FullName with get() = null
    override __.Namespace with get() = null
    override __.AssemblyQualifiedName with get() = null
    override __.BaseType with get() = null
    override __.GetConstructorImpl(_,_,_,_,_) = null
    override __.GetConstructors(_) = null
    override __.GetMethodImpl(_,_,_,_,_,_) = null
    override __.GetMethods(_) = null
    override __.GetField(_,_) = null
    override __.GetFields(_) = null
    override __.GetInterface(_,_) = null
    override __.GetInterfaces() = null
    override __.GetEvent(_,_) = null
    override __.GetEvents(_) = null
    override __.GetPropertyImpl(_,_,_,_,_,_) = null
    override __.GetProperties(_) = null
    override __.GetNestedType(_,_) = null
    override __.GetNestedTypes(_) = null
    override __.GetMembers(_) = null
    override __.GetAttributeFlagsImpl() = failwith "Don't call this."
    override __.IsArrayImpl() = false
    override __.IsByRefImpl() = false
    override __.IsPointerImpl() = false
    override __.IsPrimitiveImpl() = false
    override __.IsCOMObjectImpl() = false
    override __.GetElementType() = null
    override __.HasElementTypeImpl() = false
    override __.UnderlyingSystemType with get() = null