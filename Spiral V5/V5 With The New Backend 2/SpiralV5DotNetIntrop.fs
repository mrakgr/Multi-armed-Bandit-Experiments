module Spiral.DotNetInterop

open System
open System.Collections.Generic
open System.Reflection
open Types

let vvt x = ListT x
let litt x = LitT x
let funt (x, core) = MapT (x, core)
let uniont x = UnionT x
let closuret a b = ClosureT (a,b)
let arrayt x = ArrayT x

let ss_cache_assembly: Dictionary<Assembly,TypedExpr> = d0()
let ss_cache_type: Dictionary<Type,Ty> = d0()
//let ss_cache_method: Dictionary<MethodInfo = d0()
//let ss_cache_field = d0()

let dotnet_typet (x: Type) = ss_cache_type.[x]

//// #Conversion
//let rec dotnet_type_to_ty (x: System.Type) =
//    if x = typeof<bool> then PrimT BoolT
//    elif x = typeof<int8> then PrimT Int8T
//    elif x = typeof<int16> then PrimT Int16T
//    elif x = typeof<int32> then PrimT Int32T
//    elif x = typeof<int64> then PrimT Int64T
//
//    elif x = typeof<uint8> then PrimT UInt8T
//    elif x = typeof<uint16> then PrimT UInt16T
//    elif x = typeof<uint32> then PrimT UInt32T
//    elif x = typeof<uint64> then PrimT UInt64T
//
//    elif x = typeof<float32> then PrimT Float32T
//    elif x = typeof<float> then PrimT Float64T
//    elif x = typeof<string> then PrimT StringT
//    elif x = typeof<char> then PrimT CharT
//    elif x = typeof<unit> || x = typeof<System.Void> then ListT []
//    elif x.IsArray then arrayt(DotNetHeap,dotnet_type_to_ty (x.GetElementType()))
//    // Note: The F# compiler doing implicit conversions on refs really screws with me here. I won't bother trying to make this sound.
//    elif x.IsByRef then arrayt(DotNetReference, dotnet_type_to_ty (x.GetElementType())) // Incorrect, but useful
//    elif FSharp.Reflection.FSharpType.IsFunction x then 
//        let a,b = FSharp.Reflection.FSharpType.GetFunctionElements x
//        closuret (dotnet_type_to_ty a) (dotnet_type_to_ty b)
//    else dotnet_typet x

let ss_compile_type_wrap (x: Type) =
    let gen_pars = x.GetGenericArguments() |> Array.map (fun x -> x.Name)
    SSTyLam (Map.empty, gen_pars,SSCompileType x)

let ss_compile_method_wrap (x: MethodInfo) =
    let gen = x.GetGenericArguments()
    let pars = x.GetParameters()

let ss_compile_type (d: SSEnvTerm) (x: Type) =
    let full_name = x.FullName
    let assembly_name = x.Assembly.FullName

    let inline is_static x = (^a : (member IsStatic: bool) x)
    let inline is_public x = (^a : (member IsPublic: bool) x)
    let inline name x = (^a : (member Name: String) x)

    let inline partition g x =
        let inline f x = 
            Array.groupBy name (g x)
            |> Map
            
        Array.filter is_public x
        |> Array.partition is_static
        |> fun (a,b) -> f a, f b

    let static_methods, methods = 
        x.GetMethods() 
        |> partition (Array.sortBy (fun x -> x.GetParameters().Length))
    
    let static_fields, fields = x.GetFields() |> partition id

    failwith ""
