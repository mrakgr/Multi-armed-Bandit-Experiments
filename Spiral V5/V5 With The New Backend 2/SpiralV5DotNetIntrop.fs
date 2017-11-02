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
let ss_cache_type = d0()
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

let nodify_ssty = nodify <| d0()
let dotnet_typet x = nodify_ssty x |> DotNetTypeT

let ss_compile_type (x: Type) =
    let gen_pars = x.GetGenericArguments() |> Array.map (fun x -> x.Name)
    SSTyLam (Map.empty, gen_pars, SSCompileTypeDefinition x)

let ss_get_type = function
    | SSTyType x -> x
    | _ -> failwith "Not a type."

let rec ss_eval (d: SSEnvTerm) (x: SSExpr): SSTypedExpr =
    let rec type_to_ty (d: SSEnvTerm) (x: Type): Ty =
        memoize ss_cache_type x <| fun () ->
            if x.IsArray then arrayt(DotNetHeap,type_to_ty d (x.GetElementType()))
            // Note: The F# compiler doing implicit conversions on refs really screws with me here. I won't bother trying to make this sound.
            elif x.IsByRef then arrayt(DotNetReference, type_to_ty d (x.GetElementType())) // Incorrect, but useful
            elif FSharp.Reflection.FSharpType.IsFunction x then 
                let a,b = FSharp.Reflection.FSharpType.GetFunctionElements x
                closuret (type_to_ty d a) (type_to_ty d b)
            else
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
                |> dotnet_typet


    let ss_type_definition d x = type_to_ty d x |> SSTyType

    let rec ss_type (d: SSEnvTerm) (x: Type) =
        let ty = 
            x.GetGenericTypeDefinition()
            |> ss_compile_type

        let gen = 
            x.GetGenericArguments()
            |> Array.map (function
                | x when x.IsGenericParameter -> d.[x.Name]
                | x -> ss_type d x
                )
            |> SSTyArray

        ss_apply' ty gen

    and ss_method (d: SSEnvTerm) (x: MethodInfo) = 
        memoize ss_cache_method x <| fun () ->
            let pars =
                x.GetParameters()
                |> Array.map (fun x ->
                    let t = x.ParameterType
                    if t.IsGenericParameter then d.[t.Name] |> ss_get_type
                    else ss_type d t |> ss_get_type
                    )
                |> Array.toList
                |> listt

            let ret =
                x.ReturnType
                |> ss_type d
                |> ss_get_type

            closuret pars ret
            |> SSTyType

    and ss_field (d: SSEnvTerm) (x: FieldInfo) = 
        memoize ss_cache_field x <| fun () ->
            ss_type d x.FieldType
            
    and ss_apply' a b = 
        match a with
        | SSTyLam(env,bnds,body) ->
            match b with
            | SSTyArray args -> 
                let d = Array.foldBack2 Map.add bnds args env
                ss_eval d body
            | _ -> failwith "Expected an argument array as the second argument to apply"
        | _ -> failwith "Expected a lambda as the first argument to apply."

    let ss_apply d a b = ss_apply' (ss_eval d a) (ss_eval d b)

    match x with
    | SSAp(a,b) -> ss_apply d a b
    | SSType a -> SSTyType a
    | SSVar a -> d.[a]
    | SSArray a -> Array.map (ss_eval d) a |> SSTyArray
    | SSLam (a,b) -> SSTyLam(d,a,b)
    | SSCompileTypeDefinition a -> ss_type_definition d a
    | SSCompileMethod a -> ss_method d a 
    | SSCompileField a -> ss_field d a

let ss_compile_if_empty = function
    | SSTyLam(e,[||],b) -> ss_eval e b
    | x -> x

let ss_apply a b = 
    match a,b with
    | SSTyLam(e,arg,body), SSTyArray b -> 
        let e = Array.foldBack2 Map.add arg b e
        ss_eval e body
    | x -> failwith "Not applicable."

