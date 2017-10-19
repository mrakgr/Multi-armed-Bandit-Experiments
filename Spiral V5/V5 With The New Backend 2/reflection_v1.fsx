open System
open System.Reflection

let fs = Assembly.Load("FSharp.Core")

let array_tys =
    fs.GetTypes()
    |> Array.filter (fun x -> x.FullName.Contains "Array")

let a = fs.GetType("Microsoft.FSharp.Collections.ArrayModule").GetMethod("SortWith").MakeGenericMethod([|typeof<int64>|])

let operators =
    fs.GetTypes()
    |> Seq.find (fun x ->
        x.Name = "Operators"
        )

let is_null = operators.GetMethod("IsNull")
let is_null' = is_null.MakeGenericMethod([|typeof<System.Object>|])

type M() =
    member __.Id<'a>(x: 'a) = x
    member __.Id<'a>(x: 'a,y: int) = x

let tys = [|typeof<int32>;typeof<int32>|]
open System
open System.Collections.Generic
let method_exists (ty: Type) method_name (args: Type[]) = 
    ty.GetMethods()
    |> Array.exists (fun method_ ->
        if method_.Name = method_name then
            let pars = method_.GetParameters()
            if pars.Length = args.Length then
                let s = Dictionary()
                (pars, args) ||> Array.forall2 (fun par arg ->
                    let par = par.ParameterType
                    if par.IsGenericParameter then
                        match s.TryGetValue par with
                        | true, par -> par = arg
                        | false, _ -> s.Add(par,arg); true
                    else par = arg
                    ) 
            else false
        else false
        )
    

//    |> Array.find (fun x -> 
//        if x.Name = "Id" then
//            let params = x.GetParameters()
//            if params.
//            )

//mets.[1].GetParameters().[0].ParameterType

//|> Array.find (fun x ->
//    x.
//    )