open System
open System.Reflection

let o = typeof<Object>
o.GUID

//type SpiralType() =
//    inherit Type()
//
//    override __.GUID with get() = o.GUID
//
//    override __.InvokeMember(name:string,invokeAttr: Reflection.BindingFlags,binder: Reflection.Binder,target: obj,args: obj[],
//                             modifiers: Reflection.ParameterModifier[],culture: Globalization.CultureInfo,namedParameters: string[]) = null
//
//    override __.Module with get() = o.Module
//    override __.Assembly with get() = o.Assembly
//    override __.FullName with get() = "SpiralType"
//    override __.Namespace with get() = "Spiral"
//    override __.AssemblyQualifiedName with get() = o.AssemblyQualifiedName
//    override __.BaseType with get() = o.BaseType
//
//    override __.GetConstructorImpl(bindingAttr: Reflection.BindingFlags, binder: Reflection.Binder, callConvention: Reflection.CallingConventions,
//                                   types: Type[], modifiers: Reflection.ParameterModifier[]) = null
//
//    override __.GetConstructors(bindingAttr: Reflection.BindingFlags) = null
//
//    override __.GetMethodImpl(name:string, bindingAttr:Reflection.BindingFlags, binder:Reflection.Binder, 
//                              callConvention:Reflection.CallingConventions, types:Type[], modifiers:Reflection.ParameterModifier[]) = null

