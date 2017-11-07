module Spiral.DotNetInterop

open System
open System.Collections.Generic
open System.Reflection
open Types
open Mono.Cecil
open Mono.Cecil.Rocks

let listt x = ListT x
let litt x = LitT x
let funt (x, core) = MapT (x, core)
let uniont x = UnionT x
let closuret a b = ClosureT (a,b)
let arrayt x = ArrayT x

let BListT = listt []

/// Wraps the argument in a list if not a tuple type.
let tuple_field_ty = function 
    | ListT x -> x
    | x -> [x]

//let ss_cache_assembly: Dictionary<Assembly,TypedExpr> = d0()
let ss_cache_type = d0()
let ss_cache_method = d0()
let ss_cache_field = d0()
let ss_cache_constructor = d0()

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
let typeof_string = mscorlib.GetType("System.String")
let typeof_void = mscorlib.GetType("System.Void")

let typeof_func = fsharp_core.GetType("Microsoft.FSharp.Core.FSharpFunc`2")
let typeof_unit = fsharp_core.GetType("Microsoft.FSharp.Core.Unit")

let rec prim_type_to_ty (d: SSEnvTerm) (x: TypeDefinition) on_fail =
    if x = typeof_bool then PrimT BoolT
    elif x = typeof_int8 then PrimT Int8T
    elif x = typeof_int16 then PrimT Int16T
    elif x = typeof_int32 then PrimT Int32T
    elif x = typeof_int64 then PrimT Int64T

    elif x = typeof_uint8 then PrimT UInt8T
    elif x = typeof_uint16 then PrimT UInt16T
    elif x = typeof_uint32 then PrimT UInt32T
    elif x = typeof_uint64 then PrimT UInt64T

    elif x = typeof_float32 then PrimT Float32T
    elif x = typeof_float64 then PrimT Float64T
    elif x = typeof_string then PrimT StringT
    elif x = typeof_char then PrimT CharT
    elif x = typeof_unit || x = typeof_void then BListT
    elif x.IsArray then arrayt(DotNetHeap, ss_type_apply d (x.GetElementType()))
    // Note: The F# compiler doing implicit conversions on refs really screws with me here. I won't bother trying to make this sound.
    elif x.IsByReference then arrayt(DotNetReference, ss_type_apply d (x.GetElementType())) // Incorrect, but useful
//    elif FSharp.Reflection.FSharpType.IsFunction x then 
//        let a,b = FSharp.Reflection.FSharpType.GetFunctionElements x
//        closuret (prim_type_to_ty a on_fail) (prim_type_to_ty b on_fail)
    else on_fail x
//        memoize ss_cache_type x <| fun () -> on_fail x

let nodify_ssty = nodify <| d0()
let dotnet_typet x = nodify_ssty x |> DotNetTypeT

let rec ss_type_definition (x: TypeDefinition) =
    let gen_pars = x.GenericParameters |> Seq.map (fun x -> x.Name)
    if Seq.isEmpty gen_pars = false then SSTyLam (Map.empty, Seq.toArray gen_pars, SSCompileTypeDefinition x)
    else ss_compile_type_definition Map.empty x |> SSTyType

and ss_compile_type_definition (d: SSEnvTerm) (x: TypeDefinition): Ty =
     memoize ss_cache_type x <| fun () ->
        let gen_args = x.GenericParameters |> Seq.toArray
                
        let inline is_static x = (^a : (member IsStatic: bool) x)
        let inline is_public x = (^a : (member IsPublic: bool) x)
        let inline name x = (^a : (member Name: String) x)

        let inline partition g x =
            let inline f x = g x |> Map
            
            Seq.toArray x
            |> Array.filter is_public
            |> Array.partition is_static
            |> fun (a,b) -> f a, f b

        let static_methods, methods = 
            let name' (meth: MethodDefinition) = // Special case because the F# has special compilation names for static methods.
                meth.CustomAttributes
                |> Seq.tryFind (fun x -> x.AttributeType.Name = "CompilationSourceNameAttribute")
                |> Option.map (fun atr -> 
                    atr.ConstructorArguments |> Seq.head 
                    |> fun x -> x.Value :?> string)
                |> Option.defaultValue meth.Name

            x.Methods
            |> partition (Array.groupBy name' >> Array.map (fun (k,v) -> 
                let v =
                    v |> Array.map (fun x ->
                        let gen = x.GenericParameters |> Seq.toArray |> Array.map name
                        SSTyLam(d, gen, SSCompileMethod x)
                        )
                k, v))

        let static_fields, fields = 
            x.Fields
            |> partition (Array.map (fun x ->
                x.Name, SSTyLam(d, [||], SSCompileField x)
                ))

        let constructors =
            x.GetConstructors()
            |> Seq.toArray
            |> Array.map (fun x ->
                SSTyLam(d, [||], SSCompileConstructor x)
                )

        SSTyClass {
            full_name = let name = x.Name.Split '`' |> Array.head in String.concat "." [|x.Namespace;name|]
            generic_type_args = Array.map (fun x -> d.[name x]) gen_args
            methods = methods
            static_methods = static_methods
            fields = fields
            static_fields = static_fields
            constructors = constructors
            }
        |> dotnet_typet

and ss_type_apply (d: SSEnvTerm) (x: TypeReference): Ty =
    let gen_args = x.GenericParameters |> Seq.toArray

    if gen_args.Length = 0 then
        if x.IsGenericParameter then d.[x.Name]
        else ss_compile_type_definition Map.empty (x.Resolve())
    else
        let ty = x.Resolve()
        let gen =
            gen_args
            |> Array.map (function
                )
        
//    let ty = 
//
//        if x.IsGenericType then x.GetGenericTypeDefinition() else x
//        |> ss_type_definition 
//    let gen = 
//        gen_args
//        |> Array.map (function
//            | x when x.IsGenericParameter -> d.[x.Name]
//            | x -> ss_type_apply d x
//            )
//        |> Array.toList
//
//    ss_apply ty gen

and ss_compile_method (d: SSEnvTerm) (x: MethodInfo): Ty = 
    memoize ss_cache_method x <| fun () ->
        let pars =
            x.GetParameters()
            |> Array.map (fun x ->
                let t = x.ParameterType
                if t.IsGenericParameter then d.[t.Name]
                else ss_type_apply d t
                )
            |> Array.toList
            |> listt

        let ret = ss_type_apply d x.ReturnType

        closuret pars ret

and ss_compile_field (d: SSEnvTerm) (x: FieldInfo): Ty = 
    memoize ss_cache_field x <| fun () ->
        ss_type_apply  d x.FieldType

and ss_compile_constructor (d: SSEnvTerm) (x: ConstructorInfo): Ty = 
    memoize ss_cache_constructor x <| fun () ->
        x.GetParameters()
        |> Array.map (fun x -> ss_type_apply  d x.ParameterType)
        |> Array.toList
        |> listt

and ss_eval (d: SSEnvTerm) (x: SSExpr): Ty =
    let inline ss_eval d x = ss_eval  d x

    match x with
    | SSType a -> a
    | SSVar a -> d.[a]
    | SSArray a -> Array.map (ss_eval d) a |> Array.toList |> listt
    | SSLam (a,b) -> SSTyLam(d,a,b) |> dotnet_typet
    | SSCompileTypeDefinition a -> ss_compile_type_definition d a
    | SSCompileMethod a -> ss_compile_method d a 
    | SSCompileField a -> ss_compile_field d a
    | SSCompileConstructor a -> ss_compile_constructor d a

and ss_apply a args = 
    match a,args with
    | SSTyLam(env,bnds,body),_ ->
        let d = Seq.foldBack2 Map.add bnds args env
        ss_eval d body
    | SSTyType x,[] -> x // No need to apply a primitive type.
    | x -> failwithf "Expected a lambda as the first argument to apply.\nGot: %A" x
