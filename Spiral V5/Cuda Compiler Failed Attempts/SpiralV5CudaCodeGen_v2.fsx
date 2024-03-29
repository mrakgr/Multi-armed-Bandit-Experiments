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

type CudaMethodAnnotation =
| CudaGlobal
| CudaDevice

type CudaExpr =
    // Main AST definitions.
    | Include of string
    | Define of string
    | ExternCBlock of CudaExpr list
    | Method of CudaMethodAnnotation * return_type: CudaType * name: KernelName * args: CudaVar list * body: CudaExpr list
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
    | Address of CudaExpr
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
    /// The mutable addition with null check operator.
    static member (+?=)(var, body) = IfVoid(IsNotNull var,[MAdd(var, body)],[])

type CudaEnvironment =
    {
    indentation: int
    variables: Map<string, CudaVar>
    mutable_separator: string
    }

    /// Immutably increments the indentation by 4.
    member t.PlusIndent = {t with indentation = t.indentation+4}
    member t.AddVar(k,v) = 
        {t with variables = t.variables.Add(k,v)}
    member t.AddVars(ks,v) =
        let m = List.fold (fun m k -> Map.add k v m) t.variables ks
        {t with variables = m}

    /// The separator for mutable expressions.
    member t.WithSeparator x = {t with mutable_separator=x}

    static member create() = {indentation=0; variables=Map.empty; mutable_separator=";\n"}

/// Unfolds the method arguments and returns them in a list along with the new environment.
let get_method_arguments (args: CudaVar list) (env: CudaEnvironment) =
    let rec loop (args: CudaVar list) (env: CudaEnvironment) acc =
        match args with
        | [] -> acc, env
        | h :: t ->
            match h with
            | CudaVar(name,typ) -> 
                loop t (env.AddVar(name,h)) (h :: acc)
            | CudaArray(name, subtype, bound_sizes) ->
                let vars_to_print =
                    let f bound_size =
                        match env.variables.TryFind bound_size with
                        | Some(CudaVar(_, CudaConst CudaInt)) -> None
                        | Some x -> failwithf "Type checking for CudaArray failed. The variable its size is bound to should aways be a Var(_, CudaConst CudaInt), not %A.\nName of the size bound variable is %s" x bound_size
                        | None -> Some <| CudaVar(bound_size, CudaConst CudaInt)
                    List.choose (fun bound_size -> f bound_size) bound_sizes
                let acc, env = loop vars_to_print env acc
                loop t (env.AddVar(name,h)) (h :: acc)
    let acc, env = loop args env []
    List.rev acc, env

let cuda_codegen (exp: CudaExpr list) =
    let env = CudaEnvironment.create()
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

        let print_arguments (args: CudaVar list) (env: CudaEnvironment) prefix: CudaEnvironment =
            let acc, env = get_method_arguments (args: CudaVar list) (env: CudaEnvironment)
            List.fold (fun prefix h ->
                match h with
                | CudaVar(name,typ) -> pp prefix; print_type typ; pp name; ", "
                | CudaArray(name, subtype, bound_size) ->
                    pp prefix; print_type subtype; pp "*"; pp name; ", "
                ) "" acc
            |> ignore
            env

        let rec print_initializer prefix (env: CudaEnvironment) l: CudaEnvironment = 
            match l with
            | (CudaVar(name,typ) as h, ex) :: t -> 
                pp prefix
                let env = env.AddVar(name, h)
                print_type typ; pp name; pp " = "; gen ex env
                print_initializer ", " env t
            | [] -> env
            | _ -> failwith "Only CudaVar allowed in the For initializer."

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
        | Method(annotation,return_type, KernelName name, args, body) ->
            match annotation with
            | CudaGlobal -> pp "__global__ "
            | CudaDevice -> pp "__device__ "
            print_type return_type
            pp name; pp "("
            let env = print_arguments args env ""
            ppln ") {"
            print_seq body env.PlusIndent
            ind(); ppln "}"
        | Lambda(args, body) ->
            pp "[]"
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
        | VarAr(Var name, []) -> // The VarArs without accessors are treated to be singular.
            pp name; pp "[0]"
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
        | Address(a) -> pp "(&"; gen a env; pp ")"
        // Cub operations
        | BlockReduce(temp_storage,value,reduce_op) -> 
            pp "BlockReduceT("; gen temp_storage env; pp ").Reduce("; gen value env; pp ", "; gen reduce_op env; pp ")"
        // Mutable operations.
        | MSet(var,ex) -> gen var env; pp " = "; gen ex env; pp env.mutable_separator
        | MAdd(var,ex) -> gen var env; pp " += "; gen ex env; pp env.mutable_separator
        | AtomicAdd(out,in_) -> // Unlike the other functions, the atomic add returns the old value. This might be something to keep in mind.
            pp "atomicAdd("; gen out env; pp ", "; gen in_ env; pp ")"; pp env.mutable_separator
        | IsNotNull(VarAr(Var name,_)) -> pp "("; pp name; pp " != NULL)"
        | IsNotNull x -> failwithf "Expected a VarAr(Var _,_). Got %A instead in IsNull." x
    
    for x in exp do        
        gen x env
    program.ToString() |> KernelCode

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
/// Declared the name as a const auto variable and passes it as a Var to the in_ function.
let letcavar name init in_ =
    Let(CudaVar(name,CudaConst CudaAuto),init,in_ (Var name))
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
let blockReduce ts v op = BlockReduce(ts,v,op)

let get_unfolded_signature (args: CudaVar list): CudaVar list =
    let env = CudaEnvironment.create()
    get_method_arguments args env |> fst

let cuda_group num_in ins =
    let f i = function
        | CudaVar(name,typ) -> CudaVar(name + string i,typ)
        | CudaArray(name,typ,size)  -> CudaArray(name + string i,typ,size) 
    List.collect (fun i ->
        List.map (f i) ins) [1..num_in]

let cuda_map_all_template 
        times plus less_than // basic operators
        gridDim_x blockDim_x blockIdx_x threadIdx_x // kernel constants
        var for_ // kernel primitives
        n in_exp out_exp map_macro = // kernel params
    for_ 
        (var (plus (times blockIdx_x blockDim_x) threadIdx_x)) 
        (fun i -> less_than i n) 
        (fun i -> plus i (times gridDim_x blockDim_x))
        (fun i -> map_macro (in_exp i) (out_exp i))

let cuda_map_module_template 
        times plus less_than // basic operators
        gridDim_x blockDim_x blockIdx_x threadIdx_x // kernel constants
        var for_ method_ externCBlock include_ // kernel primitives
        n in_exp out_exp args kernel_name map_macro = // kernel params
    include_ "thrust/tuple.h"
    include_ "cub/cub.cuh"
    externCBlock (
        method_ "__global__ void " kernel_name args (
            for_ 
                (var (plus (times blockIdx_x blockDim_x) threadIdx_x)) 
                (fun i -> less_than i n) 
                (fun i -> plus i (times gridDim_x blockDim_x))
                (fun i -> map_macro (in_exp i) (out_exp i))
            )
        )



let map_module_template 
        process_ins process_outs process_args
        args_in args_out kernel_name map_macro =
    let ins, in_exp = process_ins args_in args_out
    let outs, out_exp = process_outs args_in args_out

    let args = process_args ins outs
    cuda_map_module_template in_exp out_exp args kernel_name map_macro

let cudaar_to_exp l ar_accessor =
    List.map (function
    | CudaArray(name,typ,size) -> VarAr(Var name,[ar_accessor])) l
let cudavar_to_exp l =
    List.map (function
        | CudaVar(name,typ) -> Var name) l

let map_module_forward args_in args_out kernel_name map_macro =
    let process_ins (num_in, names_in) _ =
        let ins = List.map (fun n -> CudaArray(n,CudaConst CudaFloat,["n"])) names_in |> cuda_group num_in
        ins, cudaar_to_exp ins
    let process_outs _ (num_out, names_out) =
        let outs = List.map (fun n -> CudaArray(n,CudaFloat,["n"])) names_out |> cuda_group num_out
        outs, cudaar_to_exp outs
    let process_args ins outs =
        [ins;outs] |> List.concat
    
    map_module_template process_ins process_outs process_args args_in args_out kernel_name map_macro

let mapcoef_module_forward args_in args_coef args_out kernel_name map_macro =
    let process_ins ((num_in, names_in),(num_const,names_const)) _ =
        let ins = List.map (fun n -> CudaArray(n,CudaConst CudaFloat,["n"])) names_in |> cuda_group num_in
        let consts = List.map (fun x -> CudaVar(x,CudaConst CudaFloat)) names_const |> cuda_group num_const
        (ins, consts), (fun ac -> cudaar_to_exp ins ac, cudavar_to_exp consts)
    let process_outs _ (num_out, names_out) =
        let outs = List.map (fun n -> CudaArray(n,CudaFloat,["n"])) names_out |> cuda_group num_out
        outs, cudaar_to_exp outs
    let process_args (ins, consts) outs =
        [ins;consts;outs] |> List.concat
    
    map_module_template process_ins process_outs process_args (args_in, args_coef) args_out kernel_name map_macro

let mapcoef_module_backward args_in args_coef args_out kernel_name map_macro =
    let names_into_prim_and_adj names = List.collect (fun name -> [name+"_primal_";name+"_adjoint_"]) names
    let names_into_primals names = List.map (fun name -> name+"_primal_") names
    let names_into_adjoints names = List.map (fun name -> name+"_adjoint_") names

    let process_ins ((num_in, names_in),(num_const,names_const)) (num_out, names_out) =
        let ins_prim = 
            List.map (fun n -> CudaArray(n,CudaConst CudaFloat,["n"])) 
                     (names_into_primals names_in)
            |> cuda_group num_in
        let consts = 
            List.map (fun x -> CudaVar(x,CudaConst CudaFloat)) 
                     names_const 
            |> cuda_group num_const
        let outs = 
            List.map (fun n -> CudaArray(n,CudaConst CudaFloat,["n"])) 
                     (names_into_prim_and_adj names_out)
            |> cuda_group num_out
        (ins_prim, consts, outs), (fun ac -> cudaar_to_exp ins_prim ac, cudavar_to_exp consts, cudaar_to_exp outs ac)
    let process_outs ((num_in, names_in),_) _ =
        let ins_adj = 
            List.map (fun n -> CudaArray(n,CudaFloat,["n"])) 
                     (names_into_adjoints names_in)
            |> cuda_group num_in
        ins_adj, cudaar_to_exp ins_adj
    let process_args (ins_prim, consts, outs) ins_adj =
        [ins_prim;consts;outs;ins_adj] |> List.concat
    
    map_module_template process_ins process_outs process_args (args_in, args_coef) args_out kernel_name map_macro