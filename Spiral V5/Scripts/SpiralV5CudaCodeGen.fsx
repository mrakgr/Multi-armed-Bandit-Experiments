﻿#load "SpiralV5.fsx"
open SpiralV5

let map_launcher_block_size = 256
let map_redocol_map_launcher_block_size = 128
let map_redo_map_launcher_block_size = 256

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
| CudaArray of name: string * subtype: CudaType * bound_size: string list
| CudaGroup of num: int * subtype: CudaVar list

type CudaMethodAnnotation =
| CudaGlobal
| CudaDevice

type CudaExpr =
    // Main AST definitions.
    | Include of string
    | Define of string
    | ExternCBlock of CudaExpr list
    | Method of CudaMethodAnnotation * return_type: CudaType * name: string * args: CudaVar list * body: CudaExpr list
    | Var of string
    | Value of string
    | Let of var: CudaVar * initializer: CudaExpr * in_: CudaExpr list
    | VarAr of name: CudaExpr * accessors: CudaExpr list
    | For of initializer: (CudaVar * CudaExpr) list * cond: CudaExpr * incrementor: CudaExpr list * body: CudaExpr list
    | While of cond: CudaExpr * body: CudaExpr list
    | Return of CudaExpr
    | Call of name: string * CudaExpr list
    | IfVoid of cond: CudaExpr * true_: CudaExpr list * false_: CudaExpr list
    | NoExpr // Does nothing. Can be inserted into the else part of IfVoid so the else does not get printed.
    | If of cond: CudaExpr * true_: CudaExpr * false_: CudaExpr // For ?: C style conditionals.
    | Lambda of args: CudaVar list * body: CudaExpr list

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
    // Cub operations
    | BlockReduce of temp_storage: CudaExpr * value: CudaExpr * op: CudaExpr
    // Mutable operations.
    | MSet of var: CudaExpr * body: CudaExpr
    | MAdd of var: CudaExpr * body: CudaExpr
    | AtomicAdd of out: CudaExpr * in_: CudaExpr
    // Null check
    | IsNotNull of var: CudaExpr

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
    /// The mutable assignment operator.
    static member (==)(var, body) = MSet(var,body)
    /// The mutable addition operator.
    static member (+=)(var, body) = MAdd(var,body)

type CudaEnvironment =
    {
    indentation: int
    variables: Map<string, CudaVar>
    mutable_separator: string
    }

    /// Immutably increments the indentation by 4.
    member t.PlusIndent = {t with indentation = t.indentation+4}
    member t.AddVar(k,v) = 
//        if t.variables.ContainsKey k 
//        then failwith "Variable already exists in the environment. Duplicates are not allowed. Only arrays can have their sizes rebound."
//        else 
        {t with variables = t.variables.Add(k,v)}

    /// The separator for mutable expressions.
    member t.WithSeparator x = {t with mutable_separator=x}


let cuda_codegen (exp: CudaExpr list) =
    let env = {indentation=0; variables=Map.empty; mutable_separator=";\n"}
    let program = Text.StringBuilder()
    let pp (x: string) = 
        program.Append x |> ignore
    let ppln (x: string) = 
        program.AppendLine x |> ignore
    let rec gen (exp: CudaExpr) env =
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
                | CudaArray(name, subtype, bound_size) ->
                    let vars_to_print =
                        let f bound_size =
                            match env.variables.TryFind bound_size with
                            | Some(CudaVar(_, CudaConst CudaInt)) -> None
                            | Some x -> failwithf "Type checking for CudaArray failed. The variable its size is bound to should aways be a Var(_, CudaConst CudaInt), not %A.\nName of the size bound variable is %s" x bound_size
                            | None -> Some <| CudaVar(bound_size, CudaConst CudaInt)
                        List.choose (fun bound_size -> f bound_size) bound_size
                    let env = print_arguments vars_to_print env ""
                    if vars_to_print.IsEmpty = false then pp ", "
                    print_type subtype; pp "*"; pp name; print_arguments t (env.AddVar(name,h)) ", "
                | CudaGroup(num, subvars) ->
                    Seq.fold (fun (l: CudaVar list) i ->
                        l @ List.map (fun subvar ->
                            match subvar with
                            | CudaVar(name,typ) -> 
                                CudaVar (name + string i, typ)
                            | CudaArray(name,typ,bound_size) ->
                                CudaArray (name + string i, typ, bound_size)
                            | x -> failwithf "%A not supported as a subtype of CudaArrayGroup."  x
                            ) subvars
                        ) [] {1..num}
                    |> fun args -> print_arguments args env ""
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
            | Var _ | VarAr _ as x -> gen x env
            | _ -> failwith "This expression must a Var kind."

        let print_seq (body: CudaExpr list) (env: CudaEnvironment) =
            let ind() = program.Append (String.replicate env.indentation " ") |> ignore
            for x in body do
                ind()
                gen x env

        match exp with
        // Main expressions
        | Include x ->
            pp "#include "
            ppln x
        | Define x ->
            pp "#define "
            ppln x
        | ExternCBlock body ->
            ppln """extern "C" {"""
            print_seq body env.PlusIndent
            ind(); ppln "}"
        | Method(annotation,return_type, name, args, body) ->
            match annotation with
            | CudaGlobal -> pp "__global__ "
            | CudaDevice -> pp "__device__ "
            pp name; pp "("
            let env = print_arguments args env ""
            ppln ") {"
            print_seq body env.PlusIndent
            ind(); ppln "}"
        | Lambda(args, body) ->
            pp "[&]"
            pp "("
            let env = print_arguments args env ""
            ppln "){"
            print_seq body env.PlusIndent
            ind(); pp "}"
        | Var x -> pp x
        | Value x -> pp x
        | Let(CudaVar(name,typ) as var, initializer, in_) ->
            print_type typ; pp name; 
            match initializer with
            | NoExpr -> ppln ";"
            | _ -> pp " = "; gen initializer env; ppln ";"
            print_seq in_ (env.AddVar(name,var))
        | Let(CudaArray(name,typ,size) as var, initializer, in_) ->
            print_type typ; pp name; pp "["
            List.fold (fun separator size -> pp separator; pp size; " * ") "" size |> ignore
            ppln "]"
            match initializer with
            | NoExpr -> ppln ";"
            | _ -> failwith "Initializers not allowed for arrays."
            print_seq in_ (env.AddVar(name,var))
        | Let(CudaGroup _,_,_) ->
            failwith "Array groups are only allowed in method declarations."
        | VarAr(Var name, accessors: CudaExpr list) ->
            pp name; pp "["; 
            let sizes =
                match env.variables.TryFind(name) with
                | Some(CudaArray(name,typ,sizes)) -> 
                    if accessors.Length <> sizes.Length then failwithf "accessors.Length(%A) <> sizes.Length(%A)" accessors.Length sizes.Length
                    else sizes
                | _ ->
                    printfn "%A" env.variables
                    failwithf "CudaArray2d (%A) variable not found in the environment." name
                |> List.map (fun x -> " * " + x)
                |> List.tail // This is because the first accessor needs the second size and so on.
                |> fun x -> x @ [""]
            List.fold2 (fun separator size accessor -> 
                pp separator; gen accessor env; pp size; " + ") "" sizes accessors |> ignore
            pp "]"
        | VarAr _ ->
            failwith "VarAr requires Var as a input."
        | For(initializer: (CudaVar * CudaExpr) list, cond: CudaExpr, incrementor: CudaExpr list, body: CudaExpr list) ->
            pp "for ("; 
            let env = (print_initializer "" env initializer).WithSeparator ""
            pp "; "; gen cond env; pp "; "
            Seq.fold (fun prefix x -> pp prefix; gen x env; ", ") "" incrementor |> ignore
            ppln ") {"
            print_seq body (env.PlusIndent.WithSeparator ";\n")
            ind(); ppln "}"
        | While(cond: CudaExpr, body: CudaExpr list) ->
            pp "while ("; gen cond env; ppln "){"
            print_seq body env.PlusIndent
            ind(); ppln "}"
        | Return x ->
            pp "return "; gen x env; ppln ";"
        | Call(name, l) ->
            pp name; pp "("
            let rec print_call_args l =
                match l with
                | h :: [] -> gen h env
                | h :: t -> gen h env; pp ", "; print_call_args t
                | [] -> ()
            print_call_args l
            pp ")"
        | IfVoid(c, t, []) ->
            pp "if ("; gen c env; ppln "){"
            print_seq t env.PlusIndent
            ind(); ppln "}"
        | IfVoid(c, t, f) ->
            pp "if ("; gen c env; ppln "){"
            ind(); print_seq t env.PlusIndent
            ind(); ppln "} else {"
            ind(); print_seq f env.PlusIndent; ppln "}"
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
        // Cub operations
        | BlockReduce(temp_storage,value,reduce_op) -> 
            pp "BlockReduceT("; gen temp_storage env; pp ").Reduce("; gen value env; pp ", "; gen reduce_op env; pp ")"
        // Mutable operations.
        | MSet(var,ex) -> print_var env var; pp " = "; gen ex env; pp env.mutable_separator
        | MAdd(var,ex) -> print_var env var; pp " += "; gen ex env; pp env.mutable_separator
        | AtomicAdd(out,in_) -> // Unlike the other functions, the atomic add returns the old value. This might be something to keep in mind.
            pp "atomicAdd("; print_var env out; pp ", "; print_var env in_; pp ")"; pp env.mutable_separator
        | IsNotNull(VarAr(Var name,_)) -> pp "("; pp name; pp " != NULL)"
        | IsNotNull x -> failwithf "Expected a VarAr(Var _,_). Got %A instead in IsNull." x
    
    for x in exp do        
        gen x env
    program.ToString()

let group_to_explist group (ar_accessor: CudaExpr list) =
    match group with
    | CudaGroup(num,subvars) ->
        Seq.fold (fun l i ->
            l @ List.map (fun subvar ->
                match subvar with
                | CudaVar(name,typ) -> Var(name + string i)
                | CudaArray(name,typ,bound_size) -> 
                    VarAr(Var(name + string i), ar_accessor)
                | x -> failwithf "%A not supported as a subtype of CudaGroup."  x
                ) subvars
            ) [] {1..num}
    | _ -> failwith "Must input a CudaGroup."

let zero = Value "0"
let one = Value "1"
let neg_inf = Value "__int_as_float(0xff800000)"
let pos_inf = Value "__int_as_float(0x7f800000)"

let include_ x = Include <| quote x
let define x = Define <| quote x
let externCBlock x = ExternCBlock x
let method_ ann ret name args body =
    Method(ann, ret, name, args, body)
let var x = Var x
let value x = Value x
let let_ var init in_ =
    Let(var,init,in_)
let varAr name accessor =
    VarAr(name,accessor)
let for_ initializer cond incrementor body =
    For(initializer,cond,incrementor,body)
let while_ cond body =
    While(cond,body)
let return_ x = Return x
let call name args =
    Call(name,args)
let ifVoid cond true_ false_ =
    IfVoid(cond,true_,false_)
let noExpr = NoExpr
/// For ?: C style conditionals.
let if_ cond true_ false_ =
    If(cond,true_,false_)
let lambda args body =
    Lambda(args, body)

let shuffleXor x y = ShuffleXor(x,y)
let shuffleUp x y = ShuffleUp(x,y)
let shuffleDown x y = ShuffleDown(x,y)
let shuffleSource x y = ShuffleSource(x,y)
let blockReduce ts v op =
    BlockReduce(ts,v,op)

let map_module num_in args_in num_out args_out name f =
    let in_group = CudaGroup(num_in,args_in)
    let out_group = CudaGroup(num_out,args_out)
    cuda_codegen <|
        [
        include_ "thrust/tuple.h"
        include_ "cub/cub.cuh"
        externCBlock [
            method_ CudaGlobal CudaVoid name [in_group; out_group] [
                for_ [CudaVar("i",CudaInt),Value "blockIdx.x*blockDim.x + threadIdx.x"] (Var "i" .< Var "n") [Var "i" += Value "gridDim.x*blockDim.x"]
                    (f (group_to_explist out_group [Var "i"]) (group_to_explist in_group [Var "i"]))
                ]
            ]
        ]

let map_redocol_map_module num_in args_in num_out args_out name map_load_op reduce_op map_store_op block_size =
//    let in_group = CudaGroup(num_in,[CudaArray("x",CudaConst CudaFloat,["num_cols";"num_rows"])])
//    let out_group = CudaGroup(num_out,[CudaArray("o",CudaFloat,["num_cols"])])
    let in_group = CudaGroup(num_in,args_in)
    let out_group = CudaGroup(num_out,args_out)
    cuda_codegen [
        include_ "thrust/tuple.h"
        include_ "cub/cub.cuh"
        externCBlock [
            method_ CudaGlobal CudaVoid name [in_group; out_group] [
                value <| "typedef cub::BlockReduce<float, "+block_size+"> BlockReduceT;\n"
                value "__shared__ BlockReduceT::TempStorage temp_storage;\n"
                let_ (CudaVar("reduce_op",CudaConst CudaAuto)) reduce_op [
                for_ ([CudaVar("col",CudaInt),Value "blockIdx.x"]) (Var "col" .< Var "num_cols") [Var "col" += Value "gridDim.x"] [
                    let_ (CudaVar("row",CudaInt)) (Value "threadIdx.x") [
                    let_ (CudaVar("map_load_op",CudaConst CudaAuto)) map_load_op [
                    let_ (CudaVar("value", CudaAuto)) (call "map_load_op" (group_to_explist in_group [Var "col"; Var "row"])) [
                    Var "row" += Value "blockDim.x"
                    while_ (Var "row" .< Var "num_rows") [
                        Var "value" == call "reduce_op" [Var "value"; call "map_load_op" (group_to_explist in_group [Var "col"; Var "row"])]
                        Var "row" += Value "blockDim.x"
                        ]
                    let_ (CudaVar("result",CudaConst CudaAuto)) (blockReduce (Value "temp_storage") (Var "value") (Var "reduce_op")) [
                    ifVoid 
                        (Value "threadIdx.x" .= zero)
                        (map_store_op (group_to_explist out_group [Var "col"]) (Var "result"))
                        []
                    ]]]]]
                ]]
            ]
        ]

let map_redo_map_module num_in args_in num_out args_out name map_load_op reduce_op map_store_op block_size =
//    let in_group = CudaGroup(num_in,[CudaArray("x",CudaConst CudaFloat,["n"])])
//    let out_group = CudaGroup(num_out,[CudaVar("o",CudaFloat)])
    let in_group = CudaGroup(num_in,args_in)
    let out_group = CudaGroup(num_out,args_out)
    cuda_codegen [
        include_ "thrust/tuple.h"
        include_ "cub/cub.cuh"
        externCBlock [
            method_ CudaGlobal CudaVoid name [in_group; out_group] [
                value <| "typedef cub::BlockReduce<float, "+block_size+"> BlockReduceT;\n"
                value "__shared__ BlockReduceT::TempStorage temp_storage;\n"
                let_ (CudaVar("reduce_op",CudaConst CudaAuto)) reduce_op [
                let_ (CudaVar("map_load_op",CudaConst CudaAuto)) map_load_op [
                let_ (CudaVar("i",CudaInt)) (Value "blockIdx.x*blockDim.x + threadIdx.x") [
                let_ (CudaVar("value", CudaAuto)) (call "map_load_op" (group_to_explist in_group [Var "i"])) [
                let_ (CudaVar("stride", CudaConst CudaAuto)) (Value "gridDim.x*blockDim.x") [
                Var "i" += Var "stride"
                while_ (Var "i" .< Var "n") [
                    Var "value" == call "reduce_op" [Var "value"; call "map_load_op" (group_to_explist in_group [Var "i"])]
                    Var "i" += Var "stride"
                    ]
                let_ (CudaVar("result",CudaConst CudaAuto)) (blockReduce (Value "temp_storage") (Var "value") (Var "reduce_op")) [
                    ifVoid 
                        (Value "threadIdx.x" .= zero)
                        (map_store_op (group_to_explist out_group []) (Var "result"))
                        []
                ]]]]]]]
            ]
        ]


let map_module_1_1 name f =
    map_module 1 [CudaArray("x",CudaConst CudaFloat,["n"])] 1 [CudaArray("o",CudaFloat,["n"])] name (fun [o] [x] -> [MSet(o, f x)])
let map_module_2_1 name f =
    map_module 2 [CudaArray("x",CudaConst CudaFloat,["n"])] 1 [CudaArray("o",CudaFloat,["n"])] name (fun [o] [x1;x2] -> [MSet(o, f x1 x2)])
let map_module_3_1 name f =
    map_module 3 [CudaArray("x",CudaConst CudaFloat,["n"])] 1 [CudaArray("o",CudaFloat,["n"])] name (fun [o] [x1;x2;x3] -> [MSet(o, f x1 x2 x3)])

let add_if_not_null o f =
    [IfVoid(IsNotNull o,[MAdd(o, f)],[])]

let map_backwards_module_2_1 name f =
    map_module 2 [CudaArray("x",CudaConst CudaFloat,["n"])] 1 [CudaArray("o",CudaFloat,["n"])] name 
        (fun [o] [x1;x2] -> add_if_not_null o (f x1 x2))
let map_backwards_module_3_1 name f =
    map_module 3 [CudaArray("x",CudaConst CudaFloat,["n"])] 1 [CudaArray("o",CudaFloat,["n"])] name 
        (fun [o] [x1;x2;x3] -> add_if_not_null o (f x1 x2 x3))

let map_backwards_module_3_2 name f1 f2 =
    map_module 3 [CudaArray("x",CudaConst CudaFloat,["n"])] 2 [CudaArray("o",CudaFloat,["n"])] name 
        (fun [o1;o2] [x1;x2;x3] -> 
            [add_if_not_null o1 (f1 x1 x2 x3)
             add_if_not_null o2 (f2 x1 x2 x3)]
            |> List.concat)

let mapcoef_module_1_1 name f =
    map_module 1 [CudaArray("x",CudaConst CudaFloat,["n"]); CudaVar("coef_x",CudaConst CudaFloat)] 1 [CudaArray("o",CudaFloat,["n"])] name (fun [o] [x;coef_x] -> [MSet(o, f x coef_x)])
let mapcoef_module_2_1 name f =
    map_module 2 [CudaArray("x",CudaConst CudaFloat,["n"]); CudaVar("coef_x",CudaConst CudaFloat)] 1 [CudaArray("o",CudaFloat,["n"])] name (fun [o] [x1;coef_x1;x2;coef_x2] -> [MSet(o, f x1 coef_x1 x2 coef_x2)])
let mapcoef_module_3_1 name f =
    map_module 3 [CudaArray("x",CudaConst CudaFloat,["n"]); CudaVar("coef_x",CudaConst CudaFloat)] 1 [CudaArray("o",CudaFloat,["n"])] name (fun [o] [x1;coef_x1;x2;coef_x2;x3;coef_x3] -> [MSet(o, f x1 coef_x1 x2 coef_x2 x3 coef_x3)])

let unary_op op = Lambda([CudaVar("x",CudaAuto)],op (Var "x"))
let binary_op op = Lambda([CudaVar("x1",CudaAuto);CudaVar("x2",CudaAuto)], op (Var "x1") (Var "x2"))
let nary_op num op =
    let args = List.map (fun i -> CudaVar("x"+string i,CudaAuto)) [1..num]
    let to_var x = List.map (fun (CudaVar(name,_)) -> Var name) x
    Lambda(args, op <| to_var args)

let map_redocol_map_module_1_1 name map_load_op reduce_op map_store_op =
    map_redocol_map_module 
        1 [CudaArray("x",CudaConst CudaFloat,["num_cols";"num_rows"])] 
        1 [CudaArray("o",CudaFloat,["num_cols"])] 
        name 
        (unary_op <| fun x -> [Return <| map_load_op x]) 
        (binary_op <| fun x y -> [Return <| reduce_op x y]) 
        (fun [o1] value -> [MSet(o1, map_store_op value)]) 
        (string map_redocol_map_launcher_block_size)

let map_redo_map_module_1_1 name map_load_op reduce_op map_store_op =
    map_redo_map_module 
        1 [CudaArray("x",CudaConst CudaFloat,["n"])] 
        1 [CudaVar("o",CudaFloat)] name
        (unary_op <| fun x -> [Return <| map_load_op x]) 
        (binary_op <| fun x y -> [Return <| reduce_op x y]) 
        (fun [o1] value -> [AtomicAdd(o1, map_store_op value)])
        (string map_redo_map_launcher_block_size)

let square = 
    let name = "Square"
    map_module_1_1 name <| fun x -> x * x
let square_backward =
    let name = "SquareBackward"
    map_backwards_module_2_1 name <| fun er inp -> er * Value "2" * inp

// Note: make sure to pass in the correct arguments into sigmoid_backward and
// tanh_backward. Their derivatives need the output primals and not the inputs.

let sigmoid = 
    let name = "Sigmoid"
    map_module_1_1 name <| fun x -> one / (one + Exp(-x))
let sigmoid_backward =
    let name = "SigmoidBackward"
    map_backwards_module_2_1 name <| fun er out -> er * out * (one - out)
let tanh = 
    let name = "Tanh"
    map_module_1_1 name <| fun x -> Tanh(x)
let tanh_backward =
    let name = "TanhBackward"
    map_backwards_module_2_1 name <| fun er out -> er * (one - out * out)
let relu = 
    let name = "Relu"
    map_module_1_1 name <| fun x -> if_ (x .> zero) x zero
let relu_backward =
    let name = "ReluBackward"
    map_backwards_module_2_1 name <| fun er inp -> if_ (inp .> zero) er zero

let hadmult = 
    let name = "HadMult"
    map_module_2_1 name <| fun x1 x2 -> x1 * x2
let hadmult_backward =
    let name = "HadMultBackward"
    map_backwards_module_3_2 name
        (fun er x1 x2 -> er*x2) // Adjoints for the left input (x1)
        (fun er x1 x2 -> er*x1) // Adjoinst for the right input (x2)

let sum = map_redo_map_module_1_1 "Sum" id (+) (id)
printfn "%s" sum

let colsum = map_redocol_map_module_1_1 "Colsum" id (+) id
let gradclip = mapcoef_module_1_1 "GradClip" <| fun x coef_x -> if_ (x .< -coef_x) -coef_x (if_ (x .> coef_x) coef_x x)