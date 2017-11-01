module Spiral.DotNetInterop

open System
open System.Collections.Generic
open System.Reflection
open Types

let listt x = ListT x
let litt x = LitT x
let funt (x, core) = MapT (x, core)
let uniont x = UnionT x
let closuret a b = ClosureT (a,b)
let arrayt x = ArrayT x

let ss_cache_assembly: Dictionary<Assembly,TypedExpr> = d0()
let ss_cache_type: Dictionary<Type,Ty> = d0()
let ss_cache_method = d0()
let ss_cache_field = d0()

ss_cache_type.Add(typeof<bool>,PrimT BoolT)
ss_cache_type.Add(typeof<int8>,PrimT Int8T)
ss_cache_type.Add(typeof<int16>,PrimT Int16T)
ss_cache_type.Add(typeof<int32>,PrimT Int32T)
ss_cache_type.Add(typeof<int64>,PrimT Int64T)

ss_cache_type.Add(typeof<uint8>,PrimT UInt8T)
ss_cache_type.Add(typeof<uint16>,PrimT UInt16T)
ss_cache_type.Add(typeof<uint32>,PrimT UInt32T)
ss_cache_type.Add(typeof<uint64>,PrimT UInt64T)

ss_cache_type.Add(typeof<float32>,PrimT Float32T)
ss_cache_type.Add(typeof<float>,PrimT Float64T)
ss_cache_type.Add(typeof<string>,PrimT StringT)
ss_cache_type.Add(typeof<char>,PrimT CharT)
ss_cache_type.Add(typeof<unit>,ListT [])
ss_cache_type.Add(typeof<Void>,ListT [])

//let dotnet_typet (x: Type) = 
//    match ss_cache_type.TryGetValue x with
//    | true, v -> v
//    | false, _ -> failwith ""
//        if x.IsArray then arrayt(DotNetHeap,dotnet_type_to_ty (x.GetElementType()))
//        // Note: The F# compiler doing implicit conversions on refs really screws with me here. I won't bother trying to make this sound.
//        elif x.IsByRef then arrayt(DotNetReference, dotnet_type_to_ty (x.GetElementType())) // Incorrect, but useful
//        elif FSharp.Reflection.FSharpType.IsFunction x then 
//            let a,b = FSharp.Reflection.FSharpType.GetFunctionElements x
//            closuret (dotnet_type_to_ty a) (dotnet_type_to_ty b)
//        else dotnet_typet x

let nodify_ssty = nodify <| d0()

let ss_compile_type_definition' (x: Type) =
    let gen_pars = x.GetGenericArguments() |> Array.map (fun x -> x.Name)
    SSTyLam (Map.empty, gen_pars, SSCompileTypeDefinition x)

let ss_compile_type_definition (d: SSEnvTerm) (x: Type) =
    let run () =
        let full_name = String.concat "." [|x.Namespace;x.Name|]
        let assembly_name = x.Assembly.FullName

        let inline is_static x = (^a : (member IsStatic: bool) x)
        let inline is_public x = (^a : (member IsPublic: bool) x)
        let inline name x = (^a : (member Name: String) x)

        let inline partition g x =
            let inline f x = Array.groupBy name x |> g |> Map
            
            Array.filter is_public x
            |> Array.partition is_static
            |> fun (a,b) -> f a, f b

        let static_methods, methods = 
            x.GetMethods() 
            |> partition (Array.map (fun (k,v) -> 
                let v =
                    v |> Array.sortInPlaceBy (fun x -> x.GetParameters().Length)
                    v |> Array.map (fun x ->
                        let gen = x.GetGenericArguments() |> Array.map name
                        SSTyLam(d, gen, SSCompileMethod x)
                        )
                k, v))
    
        let static_fields, fields = 
            x.GetFields() 
            |> partition (Array.map (fun (k,v) ->
                k, Array.map (fun x -> SSTyLam(d, [||], SSCompileField x)) v
                ))

        SSTyClass {
            full_name = full_name
            assembly_name = assembly_name
            methods = methods
            static_methods = static_methods
            fields = fields
            static_fields = static_fields
            }
        |> nodify_ssty
        |> DotNetTypeT

    match ss_cache_type.TryGetValue(x) with
    | false, _ ->
        let v = run()
        ss_cache_type.[x] <- v
        v
    | true, v -> v

let ss_compile_apply a b =
    match a with
    | SSTyLam(env,bnds,body) ->
        match b with
        | SSTyArray args -> 
            let d = Array.foldBack2 Map.add bnds args env
            ss_eval d body
        | _ -> failwith "Invalid input."
    | _ -> failwith "Invalid input."

let rec ss_compile_type (d: SSEnvTerm) (x: Type) =
    let ty = 
        x.GetGenericTypeDefinition()
        |> ss_compile_type_definition'

    let gen = 
        x.GetGenericArguments()
        |> Array.map (function
            | x when x.IsGenericParameter -> d.[x.Name]
            | x -> ss_compile_type d x
            )
        |> SSTyArray

    ss_compile_apply ty gen

let ss_get_type = function
    | SSTyType x -> x
    | _ -> failwith "Not a type."

let ss_compile_method (d: SSEnvTerm) (x: MethodInfo) = 
    let run () =
        let pars =
            x.GetParameters()
            |> Array.map (fun x ->
                let t = x.ParameterType
                if t.IsGenericParameter then d.[t.Name] |> ss_get_type
                else ss_compile_type d t |> ss_get_type
                )
            |> Array.toList
            |> listt

        let ret =
            x.ReturnType
            |> ss_compile_type d
            |> ss_get_type

        closuret pars ret
    match ss_cache_method.TryGetValue x with
    | true, v -> v
    | false, _ ->
        let v = run()
        ss_cache_method.[x] <- v
        v