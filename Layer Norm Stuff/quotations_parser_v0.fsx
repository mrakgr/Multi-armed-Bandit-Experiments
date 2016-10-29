// In the end I've decided to simply write a parser for F# quotations.
// With this, I'll have to do parsing, which is something I know well by now, but not typechecking.

// It took me like over 3 weeks just to decide to do this.

// I learned a lot during this time, like the finally tagless style which might come in handy in the future
// once F# gets typeclasses, LLVM and some dependently typed programming but in general, I'd consider this a 
// horrifying waste of time as I failed to find any way to simplify making those modules.

// From here on out, I will go straight towards my goal. No more Haskell, no more Idris, no more wandering
// around looking for a better deal. I am now absolutely sure that there is not any.

open System
open System.Collections.Generic

open Microsoft.FSharp.Quotations
open Microsoft.FSharp.Quotations.Patterns
open Microsoft.FSharp.Quotations.DerivedPatterns

type Ty =
| TyInt
| TyFloat32
| TyBool
| TyTuple of Ty[]

let rec flatten_tytuple =
    function
    | TyTuple(x) -> x
    | x -> [|x|]

type ParsedFunc =
| PFAdd of ParsedExpr * ParsedExpr

and ParsedExpr =
| PVar of string * Ty
| PLet of (string * Ty) * ParsedExpr * ParsedExpr
| PTupleGet of ParsedExpr * int
| PCall of ParsedFunc

type MapRedocolMap =
| MapLoad of arguments: (string * Ty) * body: ParsedExpr

type Context =
    {
    tuple_definitions: HashSet<Ty[]> // Accumulates witnesssed tuples for later convertion to C structs.
    }

let add_tuple_definition_to_context (ctx: Context) (def: Ty[]) =
    ctx.tuple_definitions.Add def |> ignore
    def

let tuple_types = // All the possible tuple types. Tuple nesting is no problem for the parser as they will get flattened either way.
    [|
    typeof<System.Tuple<_,_>>.GetGenericTypeDefinition()
    typeof<System.Tuple<_,_,_>>.GetGenericTypeDefinition()
    typeof<System.Tuple<_,_,_,_>>.GetGenericTypeDefinition()
    typeof<System.Tuple<_,_,_,_,_>>.GetGenericTypeDefinition()
    typeof<System.Tuple<_,_,_,_,_,_>>.GetGenericTypeDefinition()
    typeof<System.Tuple<_,_,_,_,_,_,_>>.GetGenericTypeDefinition()
    typeof<System.Tuple<_,_,_,_,_,_,_,_>>.GetGenericTypeDefinition()
    |]

let rec parse_type (x: Type) (ctx: Context) = 
    if x = typeof<int> then TyInt
    elif x = typeof<float32> then TyFloat32
    elif x = typeof<bool> then TyBool
    elif x.IsGenericType then 
        let gen_type_def = x.GetGenericTypeDefinition() 

        if Array.exists ((=) gen_type_def) tuple_types then // Only checks for tuples. Nested lambdas are not permitted as I am compiling to C.
            Array.map (fun x -> parse_type x ctx) (x.GetGenericArguments())
            |> Array.collect flatten_tytuple
            |> add_tuple_definition_to_context ctx
            |> TyTuple

        else failwithf "Not supported(%A)" x
    else failwithf "Not supported(%A)" x

type Result<'a> =
| Ok of 'a
| ExpectedNotFoundError of string

let name_type_of_param (param: Var) (ctx: Context) =
    param.Name,parse_type param.Type ctx

let rec parse_exprs_without_lambda (exp: Expr) (ctx: Context) =
    let inline p e = parse_exprs_without_lambda e ctx
    match exp with
    | Let(param, init, body) -> 
        PLet(name_type_of_param param ctx, p init, p body)
    | TupleGet(expr,x) -> PTupleGet(p expr,x)
    | Call(exprOpt, methodInfo, exprList) ->
        match methodInfo.Name with
        | "op_Addition" -> let [x;y] = exprList in PCall(PFAdd(p x,p y))
    | Var(x) -> PVar <| name_type_of_param x ctx
        

let parse_lambda (exp: Expr) (ctx: Context) =
    match exp with
    | Lambda(param, body) -> MapLoad(name_type_of_param param ctx, parse_exprs_without_lambda body ctx) |> Ok
    | _ -> ExpectedNotFoundError "Lambda"

let add = <@ fun (x, y) -> x+y @>

let ctx = {tuple_definitions=HashSet(HashIdentity.Structural)}
parse_lambda add ctx

ctx