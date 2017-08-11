open System

type Node<'a>(expr:'a, symbol:int) = 
    member x.Expression = expr
    member x.Symbol = symbol
    override x.GetHashCode() = symbol
    override x.Equals(y) = 
        match y with 
        | :? Node<'a> as y -> symbol = y.Symbol
        | _ -> failwith "Invalid equality for Node."

    interface IComparable with
        member x.CompareTo(y) = 
            match y with
            | :? Node<'a> as y -> compare symbol y.Symbol
            | _ -> failwith "Invalid comparison for Node."


type EnvTy = Node<Map<string, Ty>>
and FunctionCore = Node<unit>

and Ty =
    | FunctionT of Node<EnvTy * FunctionCore> // Type level function. Can also be though of as a procedural macro.
    | RecFunctionT of Node<EnvTy * FunctionCore * string>
    | ModuleT of EnvTy

let n (x: Node<_>) = x.Expression
let (|N|) x = n x

let (|TyEnvT|_|) = function // So not a bug, but it seems like it.
    | ModuleT (x & N env) -> Some (x, env)
    | RecFunctionT (x & (N (N (env),_,_))) -> Some (x, env)
    | FunctionT(x & (N(N(env),_))) -> Some (x, env)
    | _ -> None