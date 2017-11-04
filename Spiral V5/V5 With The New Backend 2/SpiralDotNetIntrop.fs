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

let BListT = listt []

/// Wraps the argument in a list if not a tuple type.
let tuple_field_ty = function 
    | ListT x -> x
    | x -> [x]

let ss_cache_assembly: Dictionary<Assembly,TypedExpr> = d0()
let ss_cache_type = d0()
let ss_cache_method = d0()
let ss_cache_field = d0()
let ss_cache_constructor = d0()
let ss_cache_event = d0()

let rec prim_type_to_ty (x: System.Type) on_fail =
    if x = typeof<bool> then PrimT BoolT
    elif x = typeof<int8> then PrimT Int8T
    elif x = typeof<int16> then PrimT Int16T
    elif x = typeof<int32> then PrimT Int32T
    elif x = typeof<int64> then PrimT Int64T

    elif x = typeof<uint8> then PrimT UInt8T
    elif x = typeof<uint16> then PrimT UInt16T
    elif x = typeof<uint32> then PrimT UInt32T
    elif x = typeof<uint64> then PrimT UInt64T

    elif x = typeof<float32> then PrimT Float32T
    elif x = typeof<float> then PrimT Float64T
    elif x = typeof<string> then PrimT StringT
    elif x = typeof<char> then PrimT CharT
    elif x = typeof<unit> || x = typeof<System.Void> then BListT
    elif x.IsArray then arrayt(DotNetHeap, prim_type_to_ty (x.GetElementType()) on_fail)
    // Note: The F# compiler doing implicit conversions on refs really screws with me here. I won't bother trying to make this sound.
    elif x.IsByRef then arrayt(DotNetReference, prim_type_to_ty (x.GetElementType()) on_fail) // Incorrect, but useful
    elif FSharp.Reflection.FSharpType.IsFunction x then 
        let a,b = FSharp.Reflection.FSharpType.GetFunctionElements x
        closuret (prim_type_to_ty a on_fail) (prim_type_to_ty b on_fail)
    else 
        memoize ss_cache_type x <| fun () -> on_fail x

let nodify_ssty = nodify <| d0()
let dotnet_typet x = nodify_ssty x |> DotNetTypeT

let rec ss_type_definition (x: Type) =
    let gen_pars = x.GetGenericArguments() |> Array.map (fun x -> x.Name)
    if gen_pars.Length > 0 then SSTyLam (Map.empty, gen_pars, SSCompileTypeDefinition x)
    else
        ss_compile_type_definition Map.empty x
        |> SSTyType

and ss_compile_type_definition (d: SSEnvTerm) (x: Type): Ty =
     prim_type_to_ty x <| fun (x: Type) ->
        let gen_args = x.GetGenericArguments()
                
        let inline is_static x = (^a : (member IsStatic: bool) x)
        let inline is_public x = (^a : (member IsPublic: bool) x)
        let inline name x = (^a : (member Name: String) x)

        let inline partition g x =
            let inline f x = g x |> Map
            
            Array.filter is_public x
            |> Array.partition is_static
            |> fun (a,b) -> f a, f b

        let static_methods, methods = 
            let name' (meth: MethodInfo) = // Special case because the F# has special compilation names for static methods.
                meth.CustomAttributes
                |> Seq.tryFind (fun x -> x.AttributeType = typeof<Microsoft.FSharp.Core.CompilationSourceNameAttribute>)
                |> Option.map (fun atr -> 
                    atr.ConstructorArguments |> Seq.head 
                    |> fun x -> x.Value :?> string)
                |> Option.defaultValue meth.Name

            x.GetMethods() 
            |> partition (Array.groupBy name' >> Array.map (fun (k,v) -> 
                let v =
                    v |> Array.sortInPlaceBy (fun x -> x.GetParameters().Length)
                    v |> Array.map (fun x ->
                        let gen = Array.map name (x.GetGenericArguments())
                        SSTyLam(d, gen, SSCompileMethod x)
                        )
                k, v))

        let static_fields, fields = 
            x.GetFields() 
            |> partition (Array.map (fun x ->
                x.Name, SSTyLam(d, [||], SSCompileField x)
                ))

        let constructors =
            if x.BaseType = typeof<System.MulticastDelegate> then // special case for delegate construction
                let handler_types =
                    let meth = x.GetMethod("Invoke")
                    let return_type = 
                        meth.ReturnType
                        |> ss_type_apply d
                    let pars =
                        meth.GetParameters()
                        |> Array.toList
                        |> List.map (fun x -> x.ParameterType |> ss_type_apply d)
                    pars @ [return_type]
                    |> List.reduceBack closuret
                [|SSTyLam(d,[||],SSType handler_types)|]
            else
                x.GetConstructors()
                |> Array.map (fun x ->
                    SSTyLam(d, [||], SSCompileConstructor x)
                    )

        let events = 
            x.GetEvents()
            |> Array.map (fun x ->
                x.Name, SSTyLam(d, [||], SSCompileEvent x)
                )
            |> Map

        SSTyClass {
            full_name = let name = x.Name.Split '`' |> Array.head in String.concat "." [|x.Namespace;name|]
            assembly_name = x.Assembly.FullName
            generic_type_args = Array.map (fun x -> d.[name x]) gen_args
            methods = methods
            static_methods = static_methods
            fields = fields
            static_fields = static_fields
            constructors = constructors
            events = events
            }
        |> dotnet_typet

and ss_type_apply (d: SSEnvTerm) (x: Type): Ty =
    let gen_args = x.GetGenericArguments()
        
    let ty = 
        if x.IsGenericType then x.GetGenericTypeDefinition() else x
        |> ss_type_definition 
    let gen = 
        gen_args
        |> Array.map (function
            | x when x.IsGenericParameter -> d.[x.Name]
            | x -> ss_type_apply d x
            )
        |> Array.toList

    ss_apply ty gen

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

and ss_compile_event (d: SSEnvTerm) (x: EventInfo): Ty =
    memoize ss_cache_event x <| fun () ->
        match ss_type_apply d x.EventHandlerType with
        | DotNetTypeT(N(SSTyClass x)) as t ->
            {x with 
                methods = // Special cases because either .NET or F# compiler has extension methods for events.
                    Map.add "AddHandler" [|SSTyLam (d,[||], SSType (closuret t BListT))|] x.methods
                    |> Map.add "RemoveHandler" [|SSTyLam (d,[||], SSType (closuret t BListT))|]
                }
            |> SSTyClass
            |> dotnet_typet
        | _ -> failwith "Applying a type here should always yield a class."

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
    | SSCompileEvent a -> ss_compile_event d a

and ss_apply a args = 
    match a,args with
    | SSTyLam(env,bnds,body),_ ->
        let d = Seq.foldBack2 Map.add bnds args env
        ss_eval d body
    | SSTyType x,[] -> x // No need to apply a primitive type.
    | x -> failwithf "Expected a lambda as the first argument to apply.\nGot: %A" x
