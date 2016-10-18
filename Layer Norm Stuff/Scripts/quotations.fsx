open System
open Microsoft.FSharp.Quotations
open Microsoft.FSharp.Quotations.Patterns
open Microsoft.FSharp.Quotations.DerivedPatterns

let println expr =
    let rec print expr =
        match expr with
        | Application(expr1, expr2) ->
            // Function application.
            print expr1
            printf "("
            print expr2
            printf ")"
        | SpecificCall <@@ (+) @@> (_, _, exprList) ->
            // Matches a call to (+). Must appear before Call pattern.
            print exprList.Head
            printf " + "
            print exprList.Tail.Head
        | SpecificCall <@@ fun x -> x |> ignore @@> (_, _, exprList) ->
            print exprList.Head
        | Call(exprOpt, methodInfo, exprList) ->
            // Method or module function call.
            match exprOpt with
            | Some expr -> print expr
            | None -> printf "%s" methodInfo.DeclaringType.Name
            printf ".%s(" methodInfo.Name
            if (exprList.IsEmpty) then printf ")" else
            print exprList.Head
            for expr in exprList.Tail do
                printf ","
                print expr
            printfn ");"
        | Int32(n) ->
            printf "%d" n
        | Lambda(param, body) ->
            // Lambda expression.
            printf "fun (%s:%s) -> " param.Name (param.Type.ToString())
            print body
        | Let(var, expr1, expr2) ->
            let print_type_c (x: Type)= 
                if x = typeof<Int32> then "int"
                elif x = typeof<Single> then "float"
                else failwith "Not supported"
            
            // Let binding.
            if (var.IsMutable) then
                printf "%s %s = " (print_type_c var.Type) var.Name
            else
                printf "%s const %s = " (print_type_c var.Type) var.Name
            print expr1
            printfn ";"
            print expr2
        | PropertyGet(_, propOrValInfo, _) ->
            printf "%s" propOrValInfo.Name
        | String(str) ->
            printf "%s" str
        | Value(value, typ) ->
            printf "%s" (value.ToString())
        | Var(var) ->
            printf "%s" var.Name
        | Sequential(expr1, expr2) ->
            print expr1
            printfn ";"
            print expr2
            printfn ";"
        | _ -> printf "%s" (expr.ToString())
    print expr
    printfn ""

let a =
    <@
        let a = 5
        a + 7 |> ignore
        a + 5
    @>

println a