// I suppose this is how one would start a real compiler, but I was so in a hurry to do it properly in F# that
// a very simple idea got lost in the mix. I forgot something really very basic.

// C++ Cuda does in fact support lambdas and tuples in the latest iteration.

// As I was thinking of how to extend C with tuples and lambdas, it occured to me that if I was not going to
// go the quotations route and there are reason for that, I might as well build a parser for an extend subset
// for C. And then supposedly I could compile it to standard C...but in that case, I could just use C++ directly.

// I'll leave this as a stump here.

type T =
| TInt
| TBool
| TFloat32

type Expr =
| FuncDecl of string * (string * T) list * T

type Env =
    {
    function_definitions : Map<string, (string * T) list * T>
    variables_in_scope : Map<string, T>
    }

let env = {function_definitions=Map.empty; variables_in_scope=Map.empty}

let def (name: string) (args : (string * T) list) (return_type: T) (env: Env) = 
    let _ =
        let is_name_in_env = env.function_definitions |> Map.exists (fun k _ -> k = name)
        if is_name_in_env then failwith "Duplicate function names not allowed."

    FuncDecl(name,args,return_type), {env with function_definitions=env.function_definitions |> Map.add name (args,return_type)}

let rec prog (def_list: (Env -> Expr * Env) list) (env: Env) =
    match def_list with
    | h :: t -> 
        let f, env = h env 
        match f with
        | FuncDecl _ -> ()
        | _ -> failwith "Only function declarations allowed on the top scope."
        prog t env
    | [] -> env

let t = def "Main" ["a",TInt] TInt env