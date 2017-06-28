#load "SpiralV5Language_v10a.fsx"
#r "../../packages/FParsec.1.0.2/lib/net40-client/FParsecCS.dll"
#r "../../packages/FParsec.1.0.2/lib/net40-client/FParsec.dll"

open SpiralV5Language_v10a
open FParsec

type ParserExpr =
| ParserStatement of (Expr -> Expr)
| ParserExpr of Expr

type Pattern =
| PatVar of string
| PatTuple of Pattern list
| PatCons of Pattern list
| PatType of Pattern * Pattern
| PatActive of string * Pattern
| PatOr of Pattern list
| PatAnd of Pattern list
| PatClauses of Pattern * Expr

let spaces s =
    let rec spaces' s: Reply<unit> = (spaces >>. optional (followedByString "//" >>. skipRestOfLine true >>. spaces')) s
    spaces' s
    
let isAsciiIdStart c = isAsciiLetter c || c = '_'
let isAsciiIdContinue c = isAsciiLetter c || isDigit c || c = '_' || c = '''

let var_name =
    many1Satisfy2L isAsciiIdStart isAsciiIdContinue "identifier" .>> spaces
    >>=? function
        | "match" | "function" | "with" | "open" | "module" 
        | "rec" | "if" | "then" | "else" | "inl" | "met" | "type" as x -> 
            fun _ -> Reply(Error,messageError <| sprintf "%s not allowed as an identifier." x)
        | x -> preturn x

let between_brackets l p r = between (skipChar l .>> spaces) (skipChar r .>> spaces) p
let rounds p = between_brackets '(' p ')'
let curlies p = between_brackets '{' p '}'
let quares p = between_brackets '[' p ']'

let patid c = skipChar c >>. spaces
let patid_cons = patid '''
let patid_type = patid '?'
let patid_active = patid '^'
let patid_or = patid '|'
let patid_and = patid '&'
let pat_tuple' pattern = rounds (many pattern)

let pat_var pattern = var_name |>> PatVar
let pat_tuple pattern = pat_tuple' pattern |>> PatTuple
let pat_cons pattern = patid_cons >>. pat_tuple' pattern |>> PatCons
let pat_type pattern = patid_type >>. tuple2 pattern pattern |>> PatType
let pat_active pattern = patid_active >>. tuple2 var_name pattern |>> PatActive
let pat_or pattern = patid_or >>. pat_tuple' pattern |>> PatOr
let pat_and pattern = patid_and >>. pat_tuple' pattern |>> PatAnd

let rec patterns (s: CharStream<_>) =
    [|
    pat_var; pat_tuple; pat_cons; pat_type
    pat_active; pat_or; pat_and
    |] 
    |> Array.map (fun x -> x patterns)
    |> choice
    |> fun p -> p s

let pattern_list = patterns


