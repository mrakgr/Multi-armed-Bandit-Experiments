#load "SpiralV5.fsx"
open SpiralV5

open ManagedCuda
open ManagedCuda.BasicTypes
open ManagedCuda.VectorTypes
open ManagedCuda.CudaBlas
open ManagedCuda.CudaRand
open ManagedCuda.NVRTC
open ManagedCuda.CudaDNN

open System
open System.Diagnostics
open System.IO
open System.Collections.Generic
open System.Runtime.InteropServices

type CudaType =
| CudaConst of subtype: CudaType
| CudaShared of subtype: CudaType
| CudaVoid
| CudaFloat
| CudaInt
| CudaAuto
| CudaThrustTuple of subtype: CudaType list

type CudaVar =
| CudaVar of name: string * typ: CudaType
| CudaArray1d of name: string * subtype: CudaType * bound_size: string
| CudaArray2d of name: string * subtype: CudaType * bound_size1: string * bound_size2: string
| CudaArrayGroup of num: int * subtype: CudaVar 

type CudaMethodAnnotation =
| CudaGlobal
| CudaDevice

type CudaExpr =
    // Main AST definitions.
    | Seq of CudaExpr list
    | Include of string
    | Define of string
    | ExternCBlock of CudaExpr
    | Method of CudaMethodAnnotation * return_type: CudaType * name: string * args: CudaVar list * body: CudaExpr
    | Var of string
    | Value of string
    | Let of var: CudaVar * initializer: CudaExpr * in_: CudaExpr
    | VarAr1d of name: string * accessor: CudaExpr
    | VarAr2d of name: string * col: CudaExpr * row: CudaExpr // The environment will track the size of the array and multiply accessor1 by size2.
    | For of initializer: (CudaVar * CudaExpr) list * cond: CudaExpr * incrementor: CudaExpr list * body: CudaExpr
    | While of cond: CudaExpr * body: CudaExpr
    | Return of CudaExpr
    | Call of name: string * CudaExpr list
    | IfVoid of cond: CudaExpr * true_: CudaExpr * false_: CudaExpr
    | NoExpr // Does nothing. Can be inserted into the else part of IfVoid so the else does not get printed.
    | If of cond: CudaExpr * true_: CudaExpr * false_: CudaExpr // For ?: C style conditionals.
    | Lambda of args: CudaVar list * body: CudaExpr

    // Primitive operations on expressions.
    | Add of CudaExpr * CudaExpr
    | Sub of CudaExpr * CudaExpr
    | Mult of CudaExpr * CudaExpr
    | Div of CudaExpr * CudaExpr
    | Mod of CudaExpr * CudaExpr
    | LT of CudaExpr * CudaExpr
    | LTE of CudaExpr * CudaExpr
    | EQ of CudaExpr * CudaExpr
    | GT of CudaExpr * CudaExpr
    | GTE of CudaExpr * CudaExpr
    | LeftShift of CudaExpr * CudaExpr
    | RightShift of CudaExpr * CudaExpr
    | Unroll
    | Syncthreads
    | ShuffleXor of CudaExpr * CudaExpr
    | ShuffleUp of CudaExpr * CudaExpr
    | ShuffleDown of CudaExpr * CudaExpr
    | ShuffleSource of CudaExpr * CudaExpr
    | Log of CudaExpr
    | Exp of CudaExpr
    | Tanh of CudaExpr
    | Neg of CudaExpr

    // Mutable operations.
    | MSet of var: CudaExpr * body: CudaExpr
    | MAdd of var: CudaExpr * body: CudaExpr

    static member (+)(x,y) = Add(x,y)
    static member (-)(x,y) = Sub(x,y)
    static member (~-)(x) = Neg(x)
    static member (*)(x,y) = Mult(x,y)
    static member (/)(x,y) = Div(x,y)
    static member (%)(x,y) = Mod(x,y)
    static member (.<)(x,y) = LT(x,y)
    static member (.<=)(x,y) = LTE(x,y)
    static member (.=)(x,y) = EQ(x,y)
    static member (.>)(x,y) = GT(x,y)
    static member (.>=)(x,y) = GTE(x,y)
    static member (<<<)(x,y) = LeftShift(x,y)
    static member (>>>)(x,y) = RightShift(x,y)

type CudaEnvironment =
    {
    indentation: int
    variables: Map<string, CudaVar>
    mutable_separator: string
    }

    /// Immutably increments the indentation by 4.
    member t.PlusIndent = {t with indentation = t.indentation+4}
    member t.AddVar(k,v) = 
        if t.variables.ContainsKey k 
        then failwith "Variable already exists in the environment. Duplicates are not allowed. Only arrays can have their sizes rebound."
        else {t with variables = t.variables.Add(k,v)}

    /// The separator for mutable expressions.
    member t.WithSeparator x = {t with mutable_separator=x}


let cuda_codegen (exp: CudaExpr) =
    let env = {indentation=0; variables=Map.empty; mutable_separator=";\n"}
    let program = Text.StringBuilder()
    let pp (x: string) = 
        program.Append x |> ignore
    let ppln (x: string) = 
        program.AppendLine x |> ignore
    let rec gen' (exp: CudaExpr) env =
        let ind() = program.Append (String.replicate env.indentation " ") |> ignore
        match exp with Seq _ -> () | _ -> ind()
        gen exp env
    and gen (exp: CudaExpr) env =
        let ind() = program.Append (String.replicate env.indentation " ") |> ignore
        let ind'() = program.Append (String.replicate (env.indentation+4) " ") |> ignore

        let rec print_type typ =
            match typ with
            | CudaConst(subtype: CudaType) ->
                pp "const "; print_type subtype
            | CudaShared subtype ->
                pp "__shared__ "; print_type subtype
            | CudaVoid -> pp "void "
            | CudaFloat -> pp "float "
            | CudaInt -> pp "int "
            | CudaAuto -> pp "auto "       
            | CudaThrustTuple(subtypes) ->
                pp "thrust::tuple<"; List.fold (fun prefix x -> pp prefix; print_type x; ", ") "" subtypes |> ignore; pp "> "

        let rec print_arguments (args: CudaVar list) (env: CudaEnvironment) prefix: CudaEnvironment =
            match args with
            | h :: t ->
                pp prefix
                match h with
                | CudaVar(name,typ) -> 
                    print_type typ; pp name
                    print_arguments t (env.AddVar(name,h)) ", "
                | CudaArray1d(name, subtype, bound_size: string) ->
                    match env.variables.TryFind bound_size with
                    | Some(CudaVar(_, CudaConst CudaInt)) ->
                        print_type subtype; pp "*"; pp name; print_arguments t (env.AddVar(name,h)) ", "
                    | Some x -> failwithf "Type checking for CudaArray1d failed. The variable its size is bound to should aways be a Var(_, CudaConst CudaInt), not %A" x
                    | None ->
                        let env = print_arguments [CudaVar(bound_size, CudaConst CudaInt)] env ""
                        pp ", "; print_type subtype; pp "*"; pp name; print_arguments t (env.AddVar(name,h)) ", "
                | CudaArray2d(name, subtype, bound_size1, bound_size2) ->
                    let vars_to_print =
                        let f bound_size =
                            match env.variables.TryFind bound_size with
                            | Some(CudaVar(_, CudaConst CudaInt)) -> []
                            | Some x -> failwithf "Type checking for CudaArray2d failed. The variable its size is bound to should aways be a Var(_, CudaConst CudaInt), not %A.\nName of the size bound variable is %s" x bound_size
                            | None -> [CudaVar(bound_size, CudaConst CudaInt)]
                        [f bound_size1; f bound_size2] |> List.concat
                    let env = print_arguments vars_to_print env ""
                    if vars_to_print.IsEmpty = false then pp ", "
                    print_type subtype; pp "*"; pp name; print_arguments t (env.AddVar(name,h)) ", "
                | CudaArrayGroup(num, subvar) ->
                    Seq.fold (fun l i ->
                        match subvar with
                        | CudaVar(name,typ) -> 
                            CudaVar (name + string i, typ) :: l
                        | CudaArray1d(name,typ,bound_size) ->
                            CudaArray1d (name + string i, typ, bound_size) :: l
                        | CudaArray2d(name,typ,bound_size1,bound_size2) ->
                            CudaArray2d (name + string i, typ, bound_size1, bound_size2) :: l
                        | x -> failwithf "%A not supported as a subtype of CudaArrayGroup."  x
                        ) [] {1..num}
                    |> fun args -> print_arguments (List.rev args) env ""
                    |> fun env -> print_arguments t env ", "
            | [] -> env

        let rec print_initializer prefix (env: CudaEnvironment) l: CudaEnvironment = 
            match l with
            | (CudaVar(name,typ) as h, ex) :: t -> 
                pp prefix
                let env = env.AddVar(name, h)
                print_type typ; pp name; pp " = "; gen ex env
                print_initializer ", " env t
            | [] -> env
            | _ -> failwith "Only CudaVar allowed in the For initializer."

        let print_var env = function 
            | Var _ | VarAr1d _ | VarAr2d _ as x -> gen x env
            | _ -> failwith "This expression must a Var kind."

        match exp with
        // Main expressions
        | Seq(h::t) ->
            ind()
            gen h env
            gen (Seq t) env
        | Seq [] ->
            ()
        | Include x ->
            pp "#include "
            ppln x
        | Define x ->
            pp "#define "
            ppln x
        | ExternCBlock x ->
            ppln """extern "C" {"""
            gen x env.PlusIndent
            ind(); ppln "}"
        | Method(annotation,return_type, name, args, body) ->
            match annotation with
            | CudaGlobal -> pp "__global__ "
            | CudaDevice -> pp "__device__ "
            pp name; pp "("
            let env' = print_arguments args env ""
            ppln ") {"
            gen' body env'.PlusIndent
            ind(); ppln "}"
        | Lambda(args, body) ->
            pp "[=]"
            pp "("
            let env' = print_arguments args env ""
            ppln ") {"
            gen' body env'.PlusIndent
            ind(); ppln "}"
        | Var x -> pp x
        | Value x -> pp x
        | Let(CudaVar(name,typ) as var, initializer, in_) ->
            print_type typ; pp name; 
            match initializer with
            | NoExpr -> ppln ";"
            | _ -> pp " = "; gen initializer env; ppln ";"
            gen' in_ (env.AddVar(name,var))
        | Let(CudaArray1d(name,typ,size) as var, initializer, in_) ->
            print_type typ; pp name; pp "["; pp size; ppln "]"
            match initializer with
            | NoExpr -> ppln ";"
            | _ -> failwith "Initializers not allowed for arrays."
            gen' in_ (env.AddVar(name,var))
        | Let(CudaArray2d(name,typ,size1,size2) as var, initializer, in_) ->
            print_type typ; pp name; pp "["; pp size1; pp " * "; pp size2; ppln "]"
            match initializer with
            | NoExpr -> ppln ";"
            | _ -> failwith "Initializers not allowed for arrays."
            gen' in_ (env.AddVar(name,var))
        | Let(CudaArrayGroup _,_,_) ->
            failwith "Array groups are only allowed in method declarations."
        | VarAr1d(name: string, accessor: CudaExpr) ->
            pp name; pp "["; gen accessor  env; pp "]"
        | VarAr2d(name: string, col: CudaExpr, row: CudaExpr) ->
            let size2 =
                match env.variables.TryFind(name) with
                | Some(CudaArray2d(name,typ,size1,size2))  -> size2
                | _ -> failwithf "CudaArray2d (%A) variable not found in the environment." name
            pp name; pp "["; gen col  env; pp " * "; pp size2; pp " + "; gen row  env; pp "]"
        | For(initializer: (CudaVar * CudaExpr) list, cond: CudaExpr, incrementor: CudaExpr list, body: CudaExpr) ->
            pp "for ("; 
            let env = (print_initializer "" env initializer).WithSeparator ""
            pp "; "; gen cond env; pp "; "
            Seq.fold (fun prefix x -> pp prefix; gen x env; ", ") "" incrementor |> ignore
            ppln "){"
            gen' body (env.PlusIndent.WithSeparator ";\n")
            ind(); ppln "}"
        | While(cond: CudaExpr, body: CudaExpr) ->
            pp "while ("; gen cond env; ppln "){"
            gen body env.PlusIndent
            ind(); ppln "}"
        | Return x ->
            gen x env; ppln ";"
        | Call(name, l) ->
            pp name; pp "("
            let rec print_call_args l =
                match l with
                | h :: [] -> gen h env
                | h :: t -> gen h env; pp ", "; print_call_args t
                | [] -> ()
            print_call_args l
            pp ")"
        | IfVoid(c, t, NoExpr) ->
            pp "if ("; gen c env; ppln "){"
            ind(); gen t env.PlusIndent
            ind(); ppln "}"
        | IfVoid(c, t, f) ->
            pp "if ("; gen c env; ppln "){"
            ind(); gen t env.PlusIndent
            ind(); ppln "} else {"
            ind(); gen f env.PlusIndent; ppln "}"
        | NoExpr -> () // Does nothing. Can be inserted into the else part of IfVoid so the else does not get printed.
        | If(c, t, f) -> pp "("; gen c env; pp ") ? "; gen t env; pp " : "; gen f env; pp ")"
        // Primitive operations on expressions.
        | Add(a,b) -> pp "("; gen a env; pp " + "; gen b env; pp ")"
        | Sub(a,b) -> pp "("; gen a env; pp " - "; gen b env; pp ")"
        | Mult(a,b) -> pp "("; gen a env; pp " * "; gen b env; pp ")"
        | Div(a,b) -> pp "("; gen a env; pp " / "; gen b env; pp ")"
        | Mod(a,b) -> pp "("; gen a env; pp " % "; gen b env; pp ")"
        | LT(a,b) -> pp "("; gen a env; pp " < "; gen b env; pp ")"
        | LTE(a,b) -> pp "("; gen a env; pp " <= "; gen b env; pp ")"
        | EQ(a,b) -> pp "("; gen a env; pp " == "; gen b env; pp ")"
        | GT(a,b) -> pp "("; gen a env; pp " > "; gen b env; pp ")"
        | GTE(a,b) -> pp "("; gen a env; pp " >= "; gen b env; pp ")"
        | LeftShift(a,b) -> pp "("; gen a env; pp " << "; gen b env; pp ")"
        | RightShift(a,b) -> pp "("; gen a env; pp " >> "; gen b env; pp ")"
        | Unroll -> ppln "#pragma unroll"
        | Syncthreads -> ppln "__syncthreads();"
        | ShuffleXor(a,b) -> pp "cub::ShflIndex("; gen a env; pp ", (threadIdx.x % 32) ^^ "; gen b env; pp ")"
        | ShuffleUp(a,b) -> pp "cub::ShflUp("; gen a env; pp ", "; gen b env; pp ")"
        | ShuffleDown(a,b) -> pp "cub::ShflDown("; gen a env; pp ", "; gen b env; pp ")"
        | ShuffleSource(a,b) -> pp "cub::ShflIndex("; gen a env; pp ", "; gen b env; pp ")"
        | Log(a) -> pp "log("; gen a env; pp ")"
        | Exp(a) -> pp "exp("; gen a env; pp ")"
        | Tanh(a) -> pp "tanh("; gen a env; pp ")"
        | Neg(a) -> pp "(-"; gen a env; pp ")"
        // Mutable operations.
        | MSet(var,ex) -> print_var env var; pp " = "; gen ex env; pp env.mutable_separator
        | MAdd(var,ex) -> print_var env var; pp " += "; gen ex env; pp env.mutable_separator
            
    gen exp env
    program.ToString()

let group1dar_to_varar group accessor =
    match group with
    | CudaArrayGroup(n,ar) ->
        List.map (fun i ->
            match ar with
            | CudaArray1d(v,_,_) -> VarAr1d(v + string i,accessor)
            | _ -> failwith "Works only on 1d arrays."
            ) [1..n]
    | _ -> failwith "Works only on array groups"

let map_module num_in num_out name f =
    let in_group = CudaArrayGroup(num_in,CudaArray1d("x",CudaFloat,"n"))
    let out_group = CudaArrayGroup(num_out,CudaArray1d("o",CudaFloat,"n"))
    cuda_codegen <|
        Seq [
            Include <| quote "thrust/tuple.h"
            Include <| quote "cub/cub.cuh"
            ExternCBlock <| Seq [
                Method(CudaGlobal,CudaVoid,name,[in_group; out_group],
                    For([CudaVar("i",CudaInt),Value "blockIdx.x*blockDim.x + threadIdx.x"], LT(Var "i",Var "n"),[MAdd(Var "i",Value "gridDim.x*blockDim.x")],
                        f (group1dar_to_varar out_group (Var "i")) (group1dar_to_varar in_group (Var "i"))
                        )
                    )
                ]
            ]

let unary_map_module name f =
    map_module 1 1 name (fun [o] [x] -> MSet(o, f x))
let binary_map_module name f =
    map_module 2 1 name (fun [o] [x1;x2] -> MSet(o, f x1 x2))
let ternary_map_module name f =
    map_module 3 1 name (fun [o] [x1;x2;x3] -> MSet(o, f x1 x2 x3))

let zero = Value "0"
let one = Value "1"
let neg_inf = Value "__int_as_float(0xff800000)"
let pos_inf = Value "__int_as_float(0x7f800000)"

let square = unary_map_module "Square" <| fun x -> x * x
let sigmoid = unary_map_module "Sigmoid" <| fun x -> one / (one + Exp(Neg x))
let tanh = unary_map_module "Tanh" <| fun x -> Tanh(x)
let relu = unary_map_module "Relu" <| fun x -> If(x .> zero, x, zero)
let hadmult = binary_map_module "HadMult" <| fun x1 x2 -> x1 * x2
