#load "SpiralV5.fsx"
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
//        if t.variables.ContainsKey k 
//        then failwith "Variable already exists in the environment. Duplicates are not allowed. Only arrays can have their sizes rebound."
//        else 
        {t with variables = t.variables.Add(k,v)}
    member t.AddVars(ks,v) =
        let m = List.fold (fun m k -> Map.add k v m) t.variables ks
        {t with variables = m}

    /// The separator for mutable expressions.
    member t.WithSeparator x = {t with mutable_separator=x}

    static member create() = {indentation=0; variables=Map.empty; mutable_separator=";\n"}

// Here is the functional version, it is more convoluted than the one above, but probably faster.
let inline generic_flatten_cudagroup num subvars (var_flattener: (_ * _) -> 'a) ar_flattener: 'a list =
    Seq.fold (fun l i ->
        l @ List.map (fun subvar ->
            match subvar with
            | CudaVar(name,typ) -> var_flattener (name + string i, typ)
            | CudaArray(name,typ,bound_size) -> ar_flattener (name + string i, typ, bound_size)
            | x -> failwithf "%A not supported as a subtype of CudaArrayGroup."  x
            ) subvars
        ) [] {1..num}

let flatten_cudagroup_to_cudavar_list num subvars =
    generic_flatten_cudagroup num subvars
        (fun (name,typ) -> CudaVar (name, typ))
        (fun (name,typ,bound_size) -> CudaArray (name, typ, bound_size))

let flatten_cudagroup_to_cudaexp_list num subvars (ar_accessor: CudaExpr list) =
    generic_flatten_cudagroup num subvars
        (fun (name,typ) -> Var name)
        (fun (name,typ,bound_size) -> VarAr(Var name, ar_accessor))

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
            | CudaGroup(num, subvars) ->
                flatten_cudagroup_to_cudavar_list num subvars
                |> fun args -> loop (args @ t) env acc
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
                | CudaGroup(num, subvars) ->
                    failwith "This case should have been unfolded inside the get_method_arguments call." // Should never hit.
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
        | Let(CudaGroup _,_,_) ->
            failwith "Array groups are only allowed in method declarations."
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

let cudavars_to_cudaexps vars (ar_accessor: CudaExpr list) =
    List.collect (fun var ->
        match var with
        | CudaVar(name,typ) -> [Var(name)]
        | CudaArray(name,typ,bound_size) -> [VarAr(Var(name), ar_accessor)]
        | CudaGroup(num,subvars) -> flatten_cudagroup_to_cudaexp_list num subvars ar_accessor
        ) vars

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

type InputArgs = InputArgs of CudaVar list
type OutputArgs = OutputArgs of CudaVar list

type InputFArgs = InputFArgs of CudaExpr list
type OutputFArgs = OutputFArgs of CudaExpr list

let map_module_template ins_to_exps outs_to_exps to_args in_group out_group name map_macro =
    let args = to_args in_group out_group
    cuda_codegen <|
        [
        include_ "thrust/tuple.h"
        include_ "cub/cub.cuh"
        externCBlock [
            method_ CudaGlobal CudaVoid name args [
                for_ [CudaVar("i",CudaInt),Value "blockIdx.x*blockDim.x + threadIdx.x"] (Var "i" .< Var "n") [Var "i" += Value "gridDim.x*blockDim.x"]
                    (map_macro (ins_to_exps in_group [Var "i"]) (outs_to_exps out_group [Var "i"]))
                ]
            ]
        ]
    |> fun code -> code, get_unfolded_signature args

let map_redocol_map_module' (InputArgs in_group) (OutputArgs out_group) name map_load_op reduce_op map_store_op block_size =
    let args = [in_group; out_group] |> List.concat
    let map_load_op_macro = map_load_op (cudavars_to_cudaexps in_group [Var "col"; Var "row"])
    cuda_codegen [
        include_ "thrust/tuple.h"
        include_ "cub/cub.cuh"
        externCBlock [
            method_ CudaGlobal CudaVoid name args [
                value <| "typedef cub::BlockReduce<float, "+block_size+"> BlockReduceT;\n"
                value "__shared__ BlockReduceT::TempStorage temp_storage;\n"
                let_ (CudaVar("reduce_op",CudaConst CudaAuto)) reduce_op [
                for_ ([CudaVar("col",CudaInt),Value "blockIdx.x"]) (Var "col" .< Var "num_cols") [Var "col" += Value "gridDim.x"] [
                    let_ (CudaVar("row",CudaInt)) (Value "threadIdx.x") [
                    let_ (CudaVar("value", CudaAuto)) map_load_op_macro [
                    Var "row" += Value "blockDim.x"
                    while_ (Var "row" .< Var "num_rows") [
                        Var "value" == call "reduce_op" [Var "value"; map_load_op_macro]
                        Var "row" += Value "blockDim.x"
                        ]
                    let_ (CudaVar("result",CudaConst CudaAuto)) (blockReduce (Value "temp_storage") (Var "value") (Var "reduce_op")) [
                    ifVoid 
                        (Value "threadIdx.x" .= zero)
                        (map_store_op (InputFArgs [Var "result"]) (OutputFArgs (cudavars_to_cudaexps out_group [Var "col"])))
                        []
                    ]]]]
                ]]
            ]
        ]
    |> fun code -> code, get_unfolded_signature args

let map_redocol_map_module num_in args_in num_out args_out name map_load_op reduce_op map_store_op block_size =
    let in_group = InputArgs [CudaGroup(num_in,args_in)]
    let out_group = OutputArgs [CudaGroup(num_out,args_out)]
    map_redocol_map_module' in_group out_group name map_load_op reduce_op map_store_op block_size

let map_redo_map_module' (InputArgs in_group) (OutputArgs out_group) name map_load_op reduce_op map_store_op =
    let args = [in_group; out_group] |> List.concat
    let map_load_op_macro = map_load_op (cudavars_to_cudaexps in_group [Var "i"])
    cuda_codegen [
        include_ "thrust/tuple.h"
        include_ "cub/cub.cuh"
        externCBlock [
            method_ CudaGlobal CudaVoid name args [
                value <| "typedef cub::BlockReduce<float, " + string map_redo_map_launcher_block_size + "> BlockReduceT;\n"
                value "__shared__ BlockReduceT::TempStorage temp_storage;\n"
                let_ (CudaVar("reduce_op",CudaConst CudaAuto)) reduce_op [
                let_ (CudaVar("i",CudaInt)) (Value "blockIdx.x*blockDim.x + threadIdx.x") [
                let_ (CudaVar("value", CudaAuto)) map_load_op_macro [
                let_ (CudaVar("stride", CudaConst CudaAuto)) (Value "gridDim.x*blockDim.x") [
                Var "i" += Var "stride"
                while_ (Var "i" .< Var "n") [
                    Var "value" == call "reduce_op" [Var "value"; map_load_op_macro]
                    Var "i" += Var "stride"
                    ]
                let_ (CudaVar("result",CudaConst CudaAuto)) (blockReduce (Value "temp_storage") (Var "value") (Var "reduce_op")) [
                ifVoid 
                    (Value "threadIdx.x" .= zero)
                    (map_store_op (InputFArgs [Var "result"]) (OutputFArgs (cudavars_to_cudaexps out_group [])))
                    []
                ]]]]]]
            ]
        ]
    |> fun code -> code, get_unfolded_signature args

let mapcoef_module_template 
        process_ins process_outs process_args
        num_in names_in num_const names_const num_out names_out kernel_name map_macro =
    let ins, ins_to_exps = process_ins num_in names_in num_const names_const
    let outs, outs_to_exps = process_outs num_out names_out

    let in_group, out_group, to_args = process_args ins outs
    map_module_template ins_to_exps outs_to_exps to_args in_group out_group kernel_name map_macro

let mapcoef_module_forward_template num_in ins num_const cvars num_out outs kernel_name =
    let len_in = num_in * List.length ins
    let in_group = InputArgs [CudaGroup(num_in,ins); CudaGroup(num_const,cvars)]
    let out_group = OutputArgs [CudaGroup(num_out,outs)]
    len_in, fun f -> f in_group out_group kernel_name

let mapcoef_module_forward num_in names_in num_const names_const num_out names_out kernel_name map_func =
    let ins = List.map (fun x -> CudaArray(x,CudaConst CudaFloat,["n"])) names_in
    let cvars = List.map (fun x -> CudaVar(x,CudaConst CudaFloat)) names_const
    let outs = List.map (fun x -> CudaArray(x,CudaFloat,["n"])) names_out

    let len_in, f = mapcoef_module_forward_template num_in ins num_const cvars num_out outs kernel_name
    f map_module' <| fun (InputFArgs inps) (OutputFArgs outs) -> 
        let input_ars, cvars = List.splitAt len_in inps
        map_func (InputFArgs input_ars) (InputFArgs cvars) (OutputFArgs outs)

let mapcoef_redo_map_module_forward num_in names_in num_const names_const num_out names_out kernel_name (map_load_op, reduce_op, map_store_op) =
    let ins = List.map (fun x -> CudaArray(x,CudaConst CudaFloat,["n"])) names_in
    let cvars = List.map (fun x -> CudaVar(x,CudaConst CudaFloat)) names_const
    let outs = List.map (fun x -> CudaArray(x,CudaFloat,[])) names_out

    let len_in, f = mapcoef_module_forward_template num_in ins num_const cvars num_out outs kernel_name
    f map_redo_map_module' 
        (fun ins_and_cvars -> 
            let ins, cvars = List.splitAt len_in ins_and_cvars
            map_load_op ins cvars)
        reduce_op
        map_store_op

let mapcoef_module_backwards_template num_in ins_prim ins_adj num_const cvars num_out outs =
    let separate_names_into_prim_and_adj names = List.collect (fun name -> [name+"_primal_";name+"_adjoint_"]) names
    let names_into_primals names = List.map (fun name -> name+"_primal_") names
    let names_into_adjoints names = List.map (fun name -> name+"_adjoint_") names

    let outs = outs separate_names_into_prim_and_adj
    let ins_prim = ins_prim names_into_primals
    let cvars = cvars

    let ins_adj = ins_adj names_into_adjoints

    let len_out = num_out * List.length outs
    let len_in = num_in * List.length ins_prim

    let in_group = InputArgs [CudaGroup(num_out,outs); CudaGroup(num_in,ins_prim); CudaGroup(num_const,cvars)] 
    let out_group = OutputArgs [CudaGroup(num_in,ins_adj)]
    fun kernel_name f -> 
        map_module' in_group out_group kernel_name
            (fun (InputFArgs output_prim_adj_and_input_prims_and_cvars) (OutputFArgs input_adjoints) -> 
                let output_prim_adj, input_prims_and_cvars = List.splitAt len_out output_prim_adj_and_input_prims_and_cvars
                let input_prims, cvars = List.splitAt len_in input_prims_and_cvars
                f (InputFArgs output_prim_adj) (InputFArgs input_prims) (InputFArgs cvars) (OutputFArgs input_adjoints))

let mapcoef_module_backwards num_in names_in num_const names_const num_out names_out =
    let outs f = List.map (fun x -> CudaArray(x, CudaConst CudaFloat, ["n"])) (f names_out)
    let ins g f = List.map (fun x -> CudaArray(x, g, ["n"])) (f names_in)
    let cvars = List.map (fun x -> CudaVar(x, CudaConst CudaFloat)) names_const

    mapcoef_module_backwards_template num_in (ins <| CudaConst CudaFloat) (ins CudaFloat) num_const cvars num_out outs

let map_fst f (a, b) = f a, b
let load_kernel name = load_kernel_nvcc name // I've pulled this out just in case I need to edit this. I do not think I'll be going back to NVRTC though.

let combine_and_compile_modules forward_module backward_module num_in names_in num_const names_const num_out names_out kernel_name forward_fun backward_fun =
    let split_kernel_name_into_forward_and_backward (KernelName n as name) =
        name, KernelName <| n+"Backward"
    
    let forward_name, backward_name = split_kernel_name_into_forward_and_backward kernel_name
    let f = forward_module num_in names_in num_const names_const num_out names_out forward_name forward_fun
    let b = backward_module num_in names_in num_const names_const num_out names_out backward_name backward_fun
    map_fst (load_kernel forward_name) f, map_fst (load_kernel backward_name) b

let map_module_1_1 kernel_name map_forward map_backward =
    combine_and_compile_modules mapcoef_module_forward mapcoef_module_backwards 1 ["x"] 0 [] 1 ["o"] kernel_name
        (fun (InputFArgs [x]) _ (OutputFArgs [o]) -> 
            [o == map_forward x])
        (fun (InputFArgs [o_pr;o_adj]) (InputFArgs [x_pr]) _ (OutputFArgs [x_adj]) -> 
            [x_adj +?= map_backward (o_pr, o_adj) x_pr])

let map_module_2_1 kernel_name map_forward map_backward1 map_backward2 =
    combine_and_compile_modules mapcoef_module_forward mapcoef_module_backwards 2 ["x"] 0 [] 1 ["o"] kernel_name
        (fun (InputFArgs [x1;x2]) _ (OutputFArgs [o]) -> 
            [o == map_forward x1 x2])
        (fun (InputFArgs [o_pr;o_adj]) (InputFArgs [x_pr1;x_pr2]) _ (OutputFArgs [x_adj1;x_adj2]) -> 
            [x_adj1 +?= map_backward1 (o_pr, o_adj) x_pr1 x_pr2
             x_adj2 +?= map_backward2 (o_pr, o_adj) x_pr1 x_pr2])

let map_module_3_1 kernel_name map_forward map_backward1 map_backward2 map_backward3 =
    combine_and_compile_modules mapcoef_module_forward mapcoef_module_backwards 3 ["x"] 0 [] 1 ["o"] kernel_name
        (fun (InputFArgs [x1;x2;x3]) _ (OutputFArgs [o]) -> 
            [o == map_forward x1 x2 x3])
        (fun (InputFArgs [o_pr;o_adj]) (InputFArgs [x_pr1;x_pr2;x_pr3]) _ (OutputFArgs [x_adj1;x_adj2;x_adj3]) -> 
            [x_adj1 +?= map_backward1 (o_pr, o_adj) x_pr1 x_pr2 x_pr3
             x_adj2 +?= map_backward2 (o_pr, o_adj) x_pr1 x_pr2 x_pr3
             x_adj3 +?= map_backward3 (o_pr, o_adj) x_pr1 x_pr2 x_pr3])

let mapcoef_module_1_1_1 kernel_name map_forward map_backward =
    combine_and_compile_modules mapcoef_module_forward mapcoef_module_backwards 1 ["x"] 1 ["cvar"] 1 ["o"] kernel_name
        (fun (InputFArgs [in_]) (InputFArgs [cvar]) (OutputFArgs [out]) -> 
            [out == map_forward in_ cvar])
        (fun (InputFArgs [out_prim;out_adj]) (InputFArgs [inp_prim]) (InputFArgs [cvar]) (OutputFArgs [inp_adj]) -> 
            [inp_adj +?= map_backward (out_prim, out_adj) inp_prim cvar])

let mapcoef_module_1_2_1 kernel_name map_forward map_backward =
    combine_and_compile_modules mapcoef_module_forward mapcoef_module_backwards 1 ["x"] 2 ["cvar"] 1 ["o"] kernel_name
        (fun (InputFArgs [in_]) (InputFArgs [cvar1;cvar2]) (OutputFArgs [out]) -> 
            [out == map_forward in_ cvar1 cvar2])
        (fun (InputFArgs [out_prim;out_adj]) (InputFArgs [inp_prim]) (InputFArgs [cvar1;cvar2]) (OutputFArgs [inp_adj]) -> 
            [inp_adj +?= map_backward (out_prim, out_adj) inp_prim cvar1 cvar2])

let unary_op op = Lambda([CudaVar("x",CudaAuto)],op (Var "x"))
let binary_op op = Lambda([CudaVar("x1",CudaAuto);CudaVar("x2",CudaAuto)], op (Var "x1") (Var "x2"))
let nary_op num op =
    let args = List.map (fun i -> CudaVar("x"+string i,CudaAuto)) [1..num]
    let to_var x = List.map (fun (CudaVar(name,_)) -> Var name) x
    Lambda(args, op <| to_var args)

let map_redocol_map_module_1_1 name map_load_op reduce_op map_store_op =
    map_redocol_map_module
        1 [CudaArray("x",CudaConst CudaFloat,["num_cols";"num_rows"])] 
        1 [CudaArray("o",CudaFloat,["num_cols"])] name 
        (fun [x] -> map_load_op x)
        (binary_op <| fun x y -> [Return <| reduce_op x y]) 
        (fun (InputFArgs [value]) (OutputFArgs [o1]) -> [o1 == map_store_op value]) 
        (string map_redocol_map_launcher_block_size)

// Note: The backwards of a sum, min, max or top k operations does not require a reduction, instead a map will suffice.
let mapcoef_redo_map_module_backwards num_in names_in num_const names_const num_out names_out kernel_name =
    let ins g f = List.map (fun x -> CudaArray(x, g, ["n"])) (f names_in)
    let cvars = List.map (fun x -> CudaVar(x, CudaConst CudaFloat)) names_const
    let outs f = List.map (fun x -> CudaVar(x, CudaConst CudaFloat)) (f names_out) // The inputs are expected to be passed in from host here.

    mapcoef_module_backwards_template num_in (ins <| CudaConst CudaFloat) (ins CudaFloat) num_const cvars num_out outs kernel_name

let map_redo_map_module_1_1 kernel_name (map_load_op, reduce_op, map_store_op) backward_map_op =
    combine_and_compile_modules 
        mapcoef_redo_map_module_forward 
        mapcoef_redo_map_module_backwards
        1 ["x"] 0 [] 1 ["o"] kernel_name
        ((fun [x] [] -> map_load_op x), // TODO: Seperate the regular inputs and cvars here.
         (binary_op <| fun x y -> [Return <| reduce_op x y]),
         (fun (InputFArgs [result]) (OutputFArgs [o]) -> [AtomicAdd(Address o, map_store_op result)]))
        (fun (InputFArgs [er_pr;er_adj]) (InputFArgs [inp_pr]) (InputFArgs []) (OutputFArgs [inp_adj]) ->
            [inp_adj +?= backward_map_op (er_pr,er_adj) inp_pr inp_adj])


let sum = 
    lazy
        let name = KernelName "Sum"
        map_redo_map_module_1_1 name 
            (id, (+), id) // Forward
            (fun (er_pr,er_adj) inp_pr inp_adj -> er_adj) // Backward

let square = 
    lazy
        let name = KernelName "Square"
        map_module_1_1 name 
            (fun x -> x * x)
            (fun (er_pr,er_adj) inp_pr -> er_adj * Value "2" * inp_pr)

let sigmoid = 
    lazy
        let name = KernelName "Sigmoid"
        map_module_1_1 name 
            (fun x -> one / (one + Exp(-x)))
            (fun (er_pr,er_adj) inp_pr -> er_adj * er_pr * (one - er_pr))
let tanh = 
    lazy
        let name = KernelName "Tanh"
        map_module_1_1 name 
            (fun x -> Tanh(x))
            (fun (er_pr,er_adj) inp_pr -> er_adj * er_pr * (one - er_pr))
let relu = 
    lazy
        let name = KernelName "Relu"
        map_module_1_1 name 
            (fun x -> if_ (x .> zero) x zero)
            (fun (er_pr,er_adj) inp_pr -> if_ (inp_pr .> zero) er_adj zero)

// The hadmult module generic in the number of input arguments.
let hadmult_generic =
    memoize (fun num_input_pairs ->
        lazy
            let rec forward_hadmult = function
                | a :: b :: [] ->
                    a * b
                | a :: b :: t ->
                    a * b + forward_hadmult t
                | x -> failwithf "Should never reach here. x = %A" x

            let name = KernelName "Hadmult"
            combine_and_compile_modules mapcoef_module_forward mapcoef_module_backwards num_input_pairs ["a";"b"] 0 [] 1 ["o"] name
                (fun (InputFArgs l) _ (OutputFArgs [o]) -> 
                    [o == forward_hadmult l])
                (fun (InputFArgs [err_pr;err_adj]) (InputFArgs inp_prs) _ (OutputFArgs inp_adjs) -> 
                    let chunk2 l =
                        List.chunkBySize 2 l
                        |> List.map (fun [a;b] -> (a,b))
                    let adjl = chunk2 inp_adjs
                    let priml = chunk2 inp_prs // Organizes the primals and the adjoints into pairs of two.

                    [letcavar "err" err_adj <| fun err ->
                        List.map2 (fun (adj_a,adj_b) (prim_a,prim_b) ->
                            [adj_a +?= err*prim_b
                             adj_b +?= err*prim_a]
                            ) adjl priml
                        |> List.concat]))

// TODO: When needed, adjust map_redocol_map_module_1_1 to conform to the new interface.
//let colsum = map_redocol_map_module_1_1 (KernelName "Colsum") id (+) id

let clip =
    lazy
        let name = KernelName "Clip"
        mapcoef_module_1_2_1 name 
            (fun x min max -> if_ (x .< min) min (if_ (x .> max) max x))
            (fun (o_pr, o_adj) x min max -> if_ (x .< min) zero (if_ (x .> max) zero o_adj))

let clipped_sigmoid = 
    lazy
        let name = KernelName "Sigmoid"
        mapcoef_module_1_2_1 name 
            (fun x min max -> 
                letcavar "post_act" (one / (one + Exp(-x))) <| fun x -> 
                    [if_ (x .< min) min (if_ (x .> max) max x)])
            (fun (o_pr, o_adj) x min max -> // I hope the o_pr .<= min optimization will not backfire on me.
                if_ (o_pr .<= min) zero (if_ (o_pr .>= max) zero (o_adj * o_pr * (one - o_pr))))
let log_ =
    lazy
        let name = KernelName "Log"
        map_module_1_1 name 
            (fun x -> Log x)
            (fun (er_pr,er_adj) inp_pr -> er_adj / inp_pr)

let scalar_matrix_add =
    lazy
        let name = KernelName "ScalarMatrixAdd"
        mapcoef_module_1_2_1 name 
            (fun x coef scalar -> coef * x + scalar)
            (fun (er_pr,er_adj) inp_pr coef scalar -> coef * er_adj) 
                // In the previous iteration of the library the backwards pass was implemented using Saxpy, but
                // this optimization is not important enough to bother with here.

/// Unlike combine_and_compile_layers this one does not have a backwards step.
let mutable_mapcoef num_in names_in num_const names_const num_out names_out kernel_name forward_fun =
     mapcoef_module_forward num_in names_in num_const names_const num_out names_out kernel_name forward_fun
     |> map_fst (load_kernel kernel_name)

let gradclip = // TODO: This should be using the mutable_map stuff.
    lazy 
        let name = KernelName "GradClip"
        mutable_mapcoef 1 ["x"] 1 ["bound"] 1 ["o"] name 
            (fun (InputFArgs [x]) (InputFArgs [bound]) (OutputFArgs [o]) -> 
                [o == if_ (x .< -bound) -bound (if_ (x .> bound) bound x)])
         
let sgd = 
    lazy
        let name = KernelName "Sgd"
        mutable_mapcoef 0 [] 1 ["learning_rate"] 1 ["in_pr";"in_adj"] name 
            (fun (InputFArgs []) (InputFArgs [learning_rate]) (OutputFArgs [in_pr;in_adj]) -> 
                [in_pr += in_adj * learning_rate
                 in_adj == zero])

let clipped_sgd =
    lazy
        let name = KernelName "ClippedSgd"
        mutable_mapcoef 0 [] 1 ["learning_rate";"clipping_threshold"] 1 ["in_pr";"in_adj"] name 
            (fun (InputFArgs []) (InputFArgs [learning_rate;clipping_threshold]) (OutputFArgs [in_pr;in_adj]) -> 
                [in_pr += if_ (in_adj .< -clipping_threshold) zero (if_ (in_adj .> clipping_threshold) zero (in_adj * learning_rate))
                 in_adj == zero])

// For normalizing after random initialization of the weight matrices.
let random_normalization =
    lazy
        let name = KernelName "RandomNormalization"
        mutable_mapcoef 0 [] 1 ["scaling_factor";"location"] 1 ["x_pr";"x_adj"] name 
            (fun (InputFArgs []) (InputFArgs [scaling_factor;location]) (OutputFArgs [x_pr;x_adj]) -> 
                [ x_pr == scaling_factor * (x_pr - value "0.5") + location])
