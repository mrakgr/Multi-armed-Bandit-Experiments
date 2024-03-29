﻿open System
open System.Text
open Microsoft.FSharp.Quotations
open Microsoft.FSharp.Quotations.Patterns
open Microsoft.FSharp.Quotations.DerivedPatterns

let println expr =
    let rec print_type_c (x: Type)= 
        if x = typeof<Int32> then "int"
        elif x = typeof<Single> then "float"
        elif x.IsGenericType && (x.GetGenericTypeDefinition() = typeof<System.Tuple<_,_>>.GetGenericTypeDefinition()) then 
            sprintf "%s_%s" (x.GetGenericArguments().[0] |> print_type_c) (x.GetGenericArguments().[1] |> print_type_c)
        else failwithf "Not supported(%A)" x

    let rec print expr: string =
        match expr with
        | Application(expr1, expr2) ->
            // Function application.
            [|
            print expr1
            sprintf "("
            print expr2
            sprintf ")"
            |] |> String.concat ""
        | SpecificCall <@@ (+) @@> (_, _, exprList) ->
            // Matches a call to (+). Must appear before Call pattern.
            [|
            print exprList.Head
            sprintf " + "
            print exprList.Tail.Head
            |] |> String.concat ""
        | SpecificCall <@@ fun x -> x |> ignore @@> (_, _, exprList) ->
            print exprList.Head
        | Call(exprOpt, methodInfo, exprList) ->
            // Method or module function call.
            [|
            yield
                match exprOpt with
                | Some expr -> print expr
                | None -> sprintf "%s" methodInfo.DeclaringType.Name
            yield sprintf ".%s(" methodInfo.Name
            if (exprList.IsEmpty) then 
                yield sprintf ")" 
            else
                yield print exprList.Head
                for expr in exprList.Tail do
                    yield sprintf ","
                    yield print expr
                yield sprintf ");\n"
            |] |> String.concat ""
        | Int32(n) ->
            sprintf "%d" n
        | Lambda(param, body) ->
            // Lambda expression.
            [|
            sprintf "fun (%s:%s) -> " param.Name (param.Type.ToString())
            print body
            |] |> String.concat ""
        | Let(var, expr1, expr2) ->
            // Let binding.
            [|
            if (var.IsMutable) then
                yield sprintf "%s %s = " (print_type_c var.Type) var.Name
            else
                yield sprintf "%s const %s = " (print_type_c var.Type) var.Name
            yield print expr1
            yield sprintf ";\n"
            yield print expr2
            |] |> String.concat ""
        | PropertyGet(_, propOrValInfo, _) ->
            sprintf "%s" propOrValInfo.Name
        | String(str) ->
            sprintf "%s" str
        | Value(value, typ) ->
            sprintf "%s" (value.ToString())
        | Var(var) ->
            sprintf "%s" var.Name
        | Sequential(expr1, expr2) ->
            [|
            yield print expr1
            yield sprintf ";\n"
            yield print expr2
            yield sprintf ";\n"
            |] |> String.concat ""
        | NewTuple(exprs) ->
            let sb = StringBuilder()
            sb.Append(sprintf "make_tuple_%s(" (print_type_c expr.Type)) |> ignore
            let ap (x: string) = sb.Append(x) |> ignore
            let rec loop (l: Expr list) = 
                match l with
                | x::y::ys -> 
                    sprintf "%s," (print x) |> ap
                    loop (y::ys)
                | x::xs -> 
                    sprintf "%s" (print x) |> ap
                    loop xs
                | [] -> sprintf ")" |> ap
            loop exprs
            sb.ToString()
        | _ -> sprintf "%s" (expr.ToString())
    printfn "%s" (print expr)
    printfn ""

let a =
    <@
        let x = 5,4
        x
    @>

println a
