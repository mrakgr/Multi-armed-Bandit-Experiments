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
| TyUnit
| TyInt
| TyFloat32
| TyBool
| TyTuple of Ty[]
| TyFunc of call_type: Ty * return_type: Ty
| TyGlobalArray of Ty
| TyLocalArray of length: int * Ty
| TySharedArray of length: int * Ty

type ParsedFunc =
| PFAdd of ParsedExpr * ParsedExpr
| PFMult of ParsedExpr * ParsedExpr
| PFDiv of ParsedExpr * ParsedExpr
| PFMod of ParsedExpr * ParsedExpr
| PFLessThan of ParsedExpr * ParsedExpr
| PFLessThanOrEqual of ParsedExpr * ParsedExpr
| PFEquality of ParsedExpr * ParsedExpr
| PFGreaterThan of ParsedExpr * ParsedExpr
| PFGreaterThanOrEqual of ParsedExpr * ParsedExpr
| PFUnroll
| PFSyncthreads
| PFShuffleXor of ParsedExpr * ParsedExpr
| PFShuffleUp of ParsedExpr * ParsedExpr
| PFShuffleDown of ParsedExpr * ParsedExpr
| PFShuffleSource of ParsedExpr * ParsedExpr

and ParsedExpr =
| PVar of string * Ty
| PLet of (string * Ty) * ParsedExpr * ParsedExpr
| PTupleGet of ParsedExpr * int
| PCall of ParsedFunc
| PFunction of arguments: (string * Ty) * body: ParsedExpr
| PValue of string
| PIfThenElse of ParsedExpr * ParsedExpr * ParsedExpr
| PWhileLoop of ParsedExpr * ParsedExpr
| PVarSet of string * ParsedExpr
| PSequential of ParsedExpr * ParsedExpr
| PForIntegerRangeLoop of string * ParsedExpr * ParsedExpr * ParsedExpr
| PDeclareArray of string * Ty * ParsedExpr
| PGetArray of string * ParsedExpr
| PSetArray of string * ParsedExpr * ParsedExpr
| PApplication of string * ParsedExpr list
| PReturn of ParsedExpr

let rec add_return_to_max =
    function
    | PLet(a,b,c) -> PLet(a,b,add_return_to_max c)
    | PSequential(a,b) -> PSequential(a,add_return_to_max b)
    | PDeclareArray(a,b,c) -> PDeclareArray(a,b,add_return_to_max c)
    | x -> PReturn x

type ParserState =
| ParseLambdaOrStatements
| ParseStatementsOnly

type Context =
    {
    definitions: HashSet<Ty> // Accumulates witnesssed definitions for later convertion to C structs.
    state : ParserState
    }

let add_definition_to_context (ctx: Context) (def: Ty) =
    ctx.definitions.Add def |> ignore
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


open System.Runtime.InteropServices

#nowarn "9"
[<StructLayout(LayoutKind.Sequential)>]
type CudaGlobalArray<'a> =
    struct
    val Pointer: nativeint
    new (a: int) = {Pointer = nativeint 0}
    end

    member t.Length: int = failwith "Not implemented in native code."

    member this.Item
        with get(a: int): 'a = failwith "Not implemented in native code"
        and set(a: int) (value:'a): unit = failwith "Not implemented in native code"

[<StructLayout(LayoutKind.Sequential)>]
type CudaLocalArray<'a> =
    struct
    val Length: int
    val Pointer: nativeint
    new (a: int) = {Length=a; Pointer = nativeint 0}
    end

    member this.Item
        with get(a: int): 'a = failwith "Not implemented in native code"
        and set(a: int) (value:'a): unit = failwith "Not implemented in native code"

[<StructLayout(LayoutKind.Sequential)>]
type CudaSharedArray<'a> =
    struct
    val Length: int
    val Pointer: nativeint
    new (a: int) = {Length=a; Pointer = nativeint 0}
    end

    member this.Item
        with get(a: int): 'a = failwith "Not implemented in native code"
        and set(a: int) (value:'a): unit = failwith "Not implemented in native code"


let rec parse_type (x: Type) (ctx: Context) = 
    if x = typeof<unit> then TyUnit
    elif x = typeof<float32> then TyFloat32
    elif x = typeof<int> then TyInt
    elif x = typeof<bool> then TyBool
    elif x.IsGenericType then 
        let gen_type_def = x.GetGenericTypeDefinition() 

        if gen_type_def = typeof<FSharpFunc<_,_>>.GetGenericTypeDefinition() then
            let args = x.GetGenericArguments()
            TyFunc(parse_type args.[0] ctx, parse_type args.[1] ctx)
        elif Array.exists ((=) gen_type_def) tuple_types then // Only checks for tuples. Nested lambdas are not permitted as I am compiling to C.
            Array.map (fun x -> parse_type x ctx) (x.GetGenericArguments())
            |> TyTuple
            |> add_definition_to_context ctx
        elif gen_type_def = typeof<CudaGlobalArray<_>>.GetGenericTypeDefinition() then
            let arg_typ = x.GetGenericArguments().[0]
            TyGlobalArray(parse_type arg_typ ctx)
            |> add_definition_to_context ctx
        else failwithf "Not supported(%A)" x
    else failwithf "Not supported(%A)" x

let name_type_of_param (param: Var) (ctx: Context) =
    param.Name, parse_type param.Type ctx

let (|PropertyInfoName|) (x: Reflection.PropertyInfo) = x.Name

let parse_exprs (exp: Expr) =
    let ctx = 
        {definitions=HashSet(HashIdentity.Structural)
         state = ParseLambdaOrStatements}
    let rec loop (exp: Expr) (ctx: Context) =
        let inline p e = loop e ctx
        match exp with
        | Lambda(param, body) ->
            match ctx.state with
            | ParseLambdaOrStatements -> 
                let x = loop body {ctx with state = ParseStatementsOnly} |> add_return_to_max
                PFunction(name_type_of_param param ctx, x)
            | ParseStatementsOnly _ -> failwith "Nested lambdas disallowed."
        | Let(param, NewObject(_,[Int32 v]), body) ->
            let gen_type = param.Type.GetGenericTypeDefinition()
            if gen_type = typeof<CudaLocalArray<_>>.GetGenericTypeDefinition() then
                let arg_typ = param.Type.GetGenericArguments().[0]
                PDeclareArray(param.Name,TyLocalArray(v, parse_type arg_typ ctx), p body)
            elif gen_type = typeof<CudaSharedArray<_>>.GetGenericTypeDefinition() then
                let arg_typ = param.Type.GetGenericArguments().[0]
                PDeclareArray(param.Name,TySharedArray(v, parse_type arg_typ ctx), p body)
            else failwithf "Object creation not supported(%A)." param.Name
        | Let(param, init, body) ->
            PLet(name_type_of_param param ctx, p init, p body)
        | TupleGet(expr,x) -> PTupleGet(p expr,x)
        | Call(exprOpt, methodInfo, exprList) ->
            match methodInfo.DeclaringType.Name, methodInfo.Name with
            | _, "op_Addition" -> let [x;y] = exprList in PCall(PFAdd(p x,p y))
            | _, "op_Multiply" -> let [x;y] = exprList in PCall(PFMult(p x,p y))
            | _, "op_Division" -> let [x;y] = exprList in PCall(PFDiv(p x,p y))
            | _, "op_Modulus" -> let [x;y] = exprList in PCall(PFMod(p x,p y))
            | _, "op_LessThan" -> let [x;y] = exprList in PCall(PFLessThan(p x,p y))
            | _, "op_LessThanOrEqual" -> let [x;y] = exprList in PCall(PFLessThanOrEqual(p x,p y))
            | _, "op_Equality" -> let [x;y] = exprList in PCall(PFEquality(p x,p y))
            | _, "op_GreaterThan" -> let [x;y] = exprList in PCall(PFGreaterThan(p x,p y))
            | _, "op_GreaterThanOrEqual" -> let [x;y] = exprList in PCall(PFGreaterThanOrEqual(p x,p y))
            | _, "_unroll" -> PCall(PFUnroll)
            | _, "_syncthreads" -> PCall(PFSyncthreads)
            | "Shuffle", "Source" -> let [a;b] = exprList in PCall(PFShuffleSource(p a, p b))
            | "Shuffle", "Up" -> let [a;b] = exprList in PCall(PFShuffleUp(p a, p b))
            | "Shuffle", "Down" -> let [a;b] = exprList in PCall(PFShuffleDown(p a, p b))
            | "Shuffle", "Xor" -> let [a;b] = exprList in PCall(PFShuffleXor(p a, p b))
            | _,_ -> failwith "Call not supported."
        | Var(x) -> PVar <| name_type_of_param x ctx
        | Value(ob,ty) -> 
            match ob with
            | :? float32 as x when x = Single.MaxValue -> PValue("__int_as_float(0x7f800000)")
            | :? float32 as x when x = Single.MinValue -> PValue("__int_as_float(0xff800000)")
            | :? float as x when x = Double.MaxValue -> PValue("__int_as_float(0x7ff0000000000000)")
            | :? float as x when x = Double.MinValue -> PValue("__int_as_float(0xfff0000000000000)")
            | :? unit -> PValue ";"
            | _ -> PValue(string ob)
        | PropertyGet(Some ar,PropertyInfoName "Item",[value]) ->
            PGetArray(string ar,p value)
        | PropertyGet(_,x,_) ->
            match x.DeclaringType.Name with
            | "ThreadIdx" -> PVar("threadIdx"+"."+x.Name,TyInt)
            | "BlockIdx" -> PVar("blockIdx"+"."+x.Name,TyInt)
            | "BlockDim" -> PVar("blockDim"+"."+x.Name,TyInt)
            | "GridDim" -> PVar("gridDim"+"."+x.Name,TyInt)
            | _ -> failwithf "Property get not supported(%A)." x.Name
        | IfThenElse(a,b,c) -> PIfThenElse(p a, p b, p c)
        | WhileLoop(a,b) -> PWhileLoop(p a, p b)
        | VarSet(a,b) -> PVarSet(a.Name, p b)
        | Sequential(a,b) -> PSequential(p a, p b)
        | ForIntegerRangeLoop(a,b,c,d) -> PForIntegerRangeLoop(a.Name,p b, p c, p d)
        | PropertySet(Some ar,PropertyInfoName "Item",[index],value) ->
            PSetArray(string ar, p index, p value)
        | Application(a,NewTuple args) -> // Function call
            PApplication(string a, List.map p args)
        | x -> failwithf "%A" x
    loop exp ctx, ctx
// Global ids
module ThreadIdx =
    let x = 0
    let y = 0
    let z = 0

module BlockIdx =
    let x = 0
    let y = 0
    let z = 0

// Block and grid sizes
module BlockDim =
    let x = 0
    let y = 0
    let z = 0

module GridDim =
    let x = 0
    let y = 0
    let z = 0

type Shuffle =
    static member Source(var: int, srcLane: int) = 0
    static member Source(var: float32, srcLane: int) = 0.0f
    static member Xor(var: int, laneMask: int) = 0
    static member Xor(var: float32, laneMask: int) = 0.0f
    static member Up(var: int, delta: uint32) = 0
    static member Up(var: float32, delta: uint32) = 0.0f
    static member Down(var: int, delta: uint32) = 0
    static member Down(var: float32, delta: uint32) = 0.0f
    
let _unroll() = ()
let _syncthreads() = ()



