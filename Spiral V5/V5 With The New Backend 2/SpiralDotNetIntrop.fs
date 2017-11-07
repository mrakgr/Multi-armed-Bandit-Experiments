module Spiral.DotNetInterop

open System
open System.Collections.Generic
open Types

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

let ss_cache_assembly: Dictionary<Mono.Cecil.AssemblyDefinition,TypedExpr> = d0()
let ss_cache_type = d0()
let ss_cache_method = d0()
let ss_cache_field = d0()
let ss_cache_constructor = d0()

let nodify_ssty = nodify <| d0()
let dotnet_typet x = nodify_ssty x |> DotNetTypeT

open Mono.Cecil
open Mono.Cecil.Rocks

let assembly_load = 
    let resolver = new DefaultAssemblyResolver()
    fun fullname -> resolver.Resolve(AssemblyNameReference.Parse(fullname))
let mscorlib = (assembly_load "mscorlib").MainModule
let fsharp_core = (assembly_load "FSharp.Core, Version=4.4.0.0, Culture=neutral, PublicKeyToken=b03f5f7f11d50a3a").MainModule

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

let typeof_fsharp_func = fsharp_core.GetType("Microsoft.FSharp.Core.FSharpFunc`2")
let typeof_fsharp_unit = fsharp_core.GetType("Microsoft.FSharp.Core.Unit")
let typeof_fsharp_compilation_source_name = fsharp_core.GetType("Microsoft.FSharp.Core.CompilationSourceNameAttribute")

let rec ss_type_definition (x: TypeDefinition) =
    let gen_pars = x.GenericParameters |> Seq.toArray |> Array.map (fun x -> x.Name)
    if gen_pars.Length > 0 then SSTyLam (Map.empty, gen_pars, SSCompileTypeDefinition x)
    else
        ss_compile_type_definition Map.empty x
        |> SSTyType

and ss_compile_type_definition (d: SSEnvTerm) =
    memoize ss_cache_type <| fun (x: TypeDefinition) ->
               
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
                |> Seq.tryFind (fun x -> x.AttributeType.Resolve() = typeof_fsharp_compilation_source_name)
                |> Option.map (fun atr -> 
                    atr.ConstructorArguments |> Seq.head 
                    |> fun x -> x.Value :?> string)
                |> Option.defaultValue meth.Name

            x.GetMethods() 
            |> partition (Array.groupBy name' >> Array.map (fun (k,v) -> 
                let v =
                    v |> Array.map (fun x ->
                        let gen = Array.map name (x.GenericParameters |> Seq.toArray)
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
            generic_type_args = Array.map (fun x -> d.[name x]) (x.GenericParameters |> Seq.toArray)
            methods = methods
            static_methods = static_methods
            fields = fields
            static_fields = static_fields
            constructors = constructors
            }
        |> dotnet_typet

and ss_type_apply (d: SSEnvTerm) (x: TypeReference): Ty =
    if x.IsGenericParameter then d.[x.Name]
    elif x.IsGenericInstance then
        let x' = x.Resolve()
        let args = (x :?> GenericInstanceType).GenericArguments |> Seq.toList
        if x' = typeof_fsharp_func then 
            match args with
            | [a;b] -> closuret (ss_type_apply d a) (ss_type_apply d b)
            | _ -> failwith "impossible"
        else
            let ty = ss_type_definition x'
            ss_apply ty (List.map (ss_type_apply d) args)
    elif x.IsArray then arrayt(DotNetHeap, ss_type_apply d (x.GetElementType()))
    // Note: The F# compiler doing implicit conversions on refs really screws with me here. I won't bother trying to make this sound.
    elif x.IsByReference then arrayt(DotNetReference, ss_type_apply d (x.GetElementType())) // Incorrect, but useful
    else
        let x = x.Resolve()
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
        elif x = typeof_fsharp_unit || x = typeof_void then BListT
        else ss_compile_type_definition Map.empty x

and ss_compile_method (d: SSEnvTerm) = 
    memoize ss_cache_method <| fun (x: MethodDefinition) ->
        let pars =
            x.Parameters
            |> Seq.toList
            |> List.map (fun x -> ss_type_apply d x.ParameterType)
            |> listt

        let ret = ss_type_apply d x.ReturnType

        closuret pars ret

and ss_compile_field (d: SSEnvTerm) = 
    memoize ss_cache_field <| fun (x: FieldDefinition) ->
        ss_type_apply d x.FieldType

and ss_compile_constructor (d: SSEnvTerm) = 
    memoize ss_cache_constructor <| fun (x: MethodDefinition) ->
        x.Parameters
        |> Seq.toList
        |> List.map (fun x -> ss_type_apply d x.ParameterType)
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
