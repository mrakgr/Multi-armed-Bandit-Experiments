// In the end I've decided to simply write a parser for F# quotations.
// With this, I'll have to do parsing, which is something I know well by now, but not typechecking.

// It took me like over 3 weeks just to decide to do this.

// I learned a lot during this time, like the finally tagless style which might come in handy in the future
// once F# gets typeclasses, LLVM and some dependently typed programming but in general, I'd consider this a 
// horrifying waste of time as I failed to find any way to simplify making those modules.

// From here on out, I will go straight towards my goal. No more Haskell, no more Idris, no more wandering
// around looking for a better deal. I am now absolutely sure that there is not any.

[<AutoOpen>]
module SpiralV4.Flame.QuotationsParser

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
| TyDeforestedFunc of call_types: Ty[] * return_type: Ty
| TyGlobalArray of Ty
| TyLocalArray of length: int * Ty
| TySharedArray of length: int * Ty
| TyGlobal2dArray of Ty
| TyLocal2dArray of num_cols: int * num_rows: int * Ty
| TyShared2dArray of num_cols: int * num_rows: int * Ty

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
| PNewTuple of (ParsedExpr * Ty) list
| PPropertyGet of ParsedExpr * string * ParsedExpr list
| PDeforestedFunction of arguments: (string * Ty) list * body: ParsedExpr

let rec add_return_to_max x =
    match x with
    | PLet(a,b,c) -> PLet(a,b,add_return_to_max c)
    | PSequential(a,b) -> PSequential(a,add_return_to_max b)
    | PDeclareArray(a,b,c) -> PDeclareArray(a,b,add_return_to_max c)
    | PPropertyGet _ | PNewTuple _ | PApplication _ | PGetArray _
    | PIfThenElse _ | PValue _ | PCall _ | PTupleGet _ | PVar _ -> PReturn x
    | x -> x

type ParserState =
| ParseLambdaOrStatements
| ParseStatements
| ParseExpressionsOnly

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
type CudaGlobalArray<'a> =
    {
    length: int
    pointer: nativeint
    }

    static member create(a: int) = {length = a; pointer = nativeint 0}
    static member create(a: int, p: nativeint) = {length = a; pointer = p}

    member this.Item
        with get(a: int): 'a = failwith "Not implemented in native code"
        and set(a: int) (value:'a): unit = failwith "Not implemented in native code"

[<StructLayout(LayoutKind.Sequential,Pack=1)>]
type CudaLocalArray<'a> =
    struct
    val length: int
    val pointer: nativeint
    new (a: int) = {length=a; pointer = nativeint 0}
    new (a: int, p: nativeint) = {length = a; pointer = p}
    end

    member this.Item
        with get(a: int): 'a = failwith "Not implemented in native code"
        and set(a: int) (value:'a): unit = failwith "Not implemented in native code"

[<StructLayout(LayoutKind.Sequential,Pack=1)>]
type CudaSharedArray<'a> =
    struct
    val length: int
    val pointer: nativeint
    new (a: int) = {length=a; pointer = nativeint 0}
    end

    member this.Item
        with get(a: int): 'a = failwith "Not implemented in native code"
        and set(a: int) (value:'a): unit = failwith "Not implemented in native code"

type CudaGlobal2dArray<'a> =
    {
    num_cols: int
    num_rows: int
    pointer: nativeint
    }

    static member create(a,b) = {num_cols=a; num_rows = b; pointer = nativeint 0}
    static member create(a,b,c) = {num_cols=a; num_rows = b; pointer = c}

    member this.Item
        with get(a: int, b: int): 'a = failwith "Not implemented in native code"
        and set(a: int, b: int) (value:'a): unit = failwith "Not implemented in native code"

[<StructLayout(LayoutKind.Sequential,Pack=1)>]
type CudaLocal2dArray<'a> =
    struct
    val num_cols: int
    val num_rows: int
    val pointer: nativeint
    new (a,b) = {num_cols=a; num_rows = b; pointer = nativeint 0}
    end

    member this.Item
        with get(a: int, b: int): 'a = failwith "Not implemented in native code"
        and set(a: int, b: int) (value:'a): unit = failwith "Not implemented in native code"

[<StructLayout(LayoutKind.Sequential,Pack=1)>]
type CudaShared2dArray<'a> =
    struct
    val num_cols: int
    val num_rows: int
    val pointer: nativeint
    new (a,b) = {num_cols=a; num_rows = b; pointer = nativeint 0}
    end

    member this.Item
        with get(a: int, b: int): 'a = failwith "Not implemented in native code"
        and set(a: int, b: int) (value:'a): unit = failwith "Not implemented in native code"

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
        elif gen_type_def = typeof<CudaGlobal2dArray<_>>.GetGenericTypeDefinition() then
            let arg_typ = x.GetGenericArguments().[0]
            TyGlobal2dArray(parse_type arg_typ ctx)
            |> add_definition_to_context ctx
        else failwithf "Not supported(%A)" x
    else failwithf "Not supported(%A)" x

let name_type_of_param (param: Var) (ctx: Context) =
    param.Name, parse_type param.Type ctx

/// Untuples the main function and renames it.
let rec deforest_kernel_main_and_rename_it kernel_main_name (x: ParsedExpr) = 
    match x with
    | PLet(("kernel_main",TyFunc(TyTuple tup_typ, ret_typ)),PFunction(("tupledArg",_),body),rest) ->
        let l = tup_typ.Length
        let rec remove_unpacker_let_statemets_and_return_them_in_a_list i (x: ParsedExpr) (args_name_ty: (string * Ty) list) =
            if i < l then
                match x with
                | PLet((arg_name,arg_ty),_,rest) -> 
                    remove_unpacker_let_statemets_and_return_them_in_a_list (i+1) rest ((arg_name,arg_ty)::args_name_ty)
                | _ -> failwith "Not supposed to get here."
            else
                List.rev args_name_ty, x
        let x, body = remove_unpacker_let_statemets_and_return_them_in_a_list 0 body []
        PLet((kernel_main_name,TyDeforestedFunc(tup_typ, ret_typ)),PDeforestedFunction(x,body),rest)

    | PLet(a,b,c) -> PLet(a,b,deforest_kernel_main_and_rename_it kernel_main_name c)
    | PSequential(a,b) -> PSequential(a,deforest_kernel_main_and_rename_it kernel_main_name b)
    | PDeclareArray(a,b,c) -> PDeclareArray(a,b,deforest_kernel_main_and_rename_it kernel_main_name c)
    | x -> x

let (|PropertyInfoName|) (x: Reflection.PropertyInfo) = x.Name

let parse_exprs kernel_main_name (exp: Expr) =
    let ctx = 
        {definitions=HashSet(HashIdentity.Structural)
         state = ParseLambdaOrStatements}
    let rec loop (exp: Expr) (ctx: Context) =
        let inline p e = loop e ctx
        let inline ty e = parse_type e ctx
        match exp with
        | Lambda(param, body) ->
            match ctx.state with
            | ParseLambdaOrStatements -> 
                let x = loop body {ctx with state = ParseStatements} |> add_return_to_max
                PFunction(name_type_of_param param ctx, x)
            | ParseStatements | ParseExpressionsOnly -> failwith "Nested lambdas disallowed."
        | Let(_,_,_) when ctx.state = ParseExpressionsOnly ->
            failwith "Let statements not allowed inside if statements."
        | Let(param, NewObject(_,[Int32 v]), body) -> // For 1d arrays
            let gen_type = param.Type.GetGenericTypeDefinition()
            if gen_type = typeof<CudaLocalArray<_>>.GetGenericTypeDefinition() then
                let arg_typ = param.Type.GetGenericArguments().[0]
                PDeclareArray(param.Name,TyLocalArray(v, ty arg_typ), p body)
            elif gen_type = typeof<CudaSharedArray<_>>.GetGenericTypeDefinition() then
                let arg_typ = param.Type.GetGenericArguments().[0]
                PDeclareArray(param.Name,TySharedArray(v, ty arg_typ), p body)
            else failwithf "Object creation not supported(%A)." param.Name
        | Let(param, NewObject(_,[Int32 num_cols; Int32 num_rows]), body) -> // For 2d arrays
            let gen_type = param.Type.GetGenericTypeDefinition()
            if gen_type = typeof<CudaLocal2dArray<_>>.GetGenericTypeDefinition() then
                let arg_typ = param.Type.GetGenericArguments().[0]
                PDeclareArray(param.Name,TyLocal2dArray(num_cols, num_rows, ty arg_typ), p body)
            elif gen_type = typeof<CudaShared2dArray<_>>.GetGenericTypeDefinition() then
                let arg_typ = param.Type.GetGenericArguments().[0]
                PDeclareArray(param.Name,TyShared2dArray(num_cols, num_rows, ty arg_typ), p body)
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
        | PropertyGet(Some v,PropertyInfoName prop_name,exprs) ->
            PPropertyGet(p v, prop_name, List.map p exprs)
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
        | Sequential(_,_) when ctx.state = ParseExpressionsOnly ->
            failwith "Sequential statements not allowed inside if statements."
        | Sequential(a,b) -> PSequential(p a, p b)
        | ForIntegerRangeLoop(a,b,c,d) -> PForIntegerRangeLoop(a.Name,p b, p c, p d)
        | PropertySet(Some ar,PropertyInfoName "Item",[index],value) ->
            PSetArray(string ar, p index, p value)
        | Application(a,NewTuple args) -> // Function call
            PApplication(string a, List.map p args)
        | NewTuple args ->
            List.map (fun x -> p x, ty x.Type) args
            |> PNewTuple
        | x -> failwithf "%A" x
    let result = loop exp ctx
    deforest_kernel_main_and_rename_it kernel_main_name result, ctx // Has another deforestation pass at the end.
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
   

