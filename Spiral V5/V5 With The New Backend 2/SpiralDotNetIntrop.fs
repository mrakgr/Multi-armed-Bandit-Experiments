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

let ss_type_definition (x: Type) =
    let gen_pars = x.GetGenericArguments() |> Array.map (fun x -> x.Name)
    SSTyLam (Map.empty, gen_pars, SSCompileTypeDefinition x)

let rec ss_eval (d: SSEnvTerm) (x: SSExpr): Ty =
    let rec ss_compile_type_definition (d: SSEnvTerm) (x: Type): Ty =
        if x.IsArray then arrayt(DotNetHeap,ss_compile_type_definition d (x.GetElementType()))
        // Note: The F# compiler doing implicit conversions on refs really screws with me here. I won't bother trying to make this sound.
        elif x.IsByRef then arrayt(DotNetReference, ss_compile_type_definition d (x.GetElementType())) // Incorrect, but useful
        elif FSharp.Reflection.FSharpType.IsFunction x then 
            let a,b = FSharp.Reflection.FSharpType.GetFunctionElements x
            closuret (ss_compile_type_definition d a) (ss_compile_type_definition d b)
        else
            memoize ss_cache_type x <| fun () ->
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
                    x.GetMethods() 
                    |> partition (Array.groupBy name >> Array.map (fun (k,v) -> 
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
            ss_type_apply d x.FieldType

    and ss_compile_constructor (d: SSEnvTerm) (x: ConstructorInfo): Ty = 
        memoize ss_cache_constructor x <| fun () ->
            x.GetParameters()
            |> Array.map (fun x -> ss_type_apply d x.ParameterType)
            |> Array.toList
            |> listt

    let ss_compile_event (d: SSEnvTerm) (x: EventInfo): Ty =
        memoize ss_cache_event x <| fun () ->
            match ss_type_apply d x.EventHandlerType with
            | DotNetTypeT(N(SSTyClass x)) as t ->
                let ob = typeof<obj> |> ss_compile_type_definition Map.empty
                {x with 
                    methods =
                        Map.add "Add" [|SSTyType (closuret t BListT)|] x.methods
                        |> Map.add "AddHandler" [|SSTyType (closuret ob (closuret t BListT))|]
                        |> Map.add "RemoveHandler" [|SSTyType (closuret t BListT)|]
                    }
                |> SSTyClass
                |> dotnet_typet
            | _ -> failwith "Applying a type here should always yield a class."

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
    match a with
    | SSTyLam(env,bnds,body) ->
        let d = Seq.foldBack2 Map.add bnds args env
        ss_eval d body
    | _ -> failwith "Expected a lambda as the first argument to apply."

