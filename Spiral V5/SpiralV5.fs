// V4 made me reach my limit in terms of coding style. I am going to focus on maximizing expressive power and embracing dynamism
// in this iteration. I will not make an effort to make sure all the type checking passes muster in this version.
// Unlike last time, this time I will start directly from the Cuda modules. The theme of SpiralV5 is interfacing with the outside world.

[<AutoOpen>]
module SpiralV5.Main

// Open up the namespaces.
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

// Initialize the context. Analogous to a CPU process. Cuda tries to offload as much as possible during context creation so there aren't
// any unexpected delays later.
let cuda_context = new CudaContext(false)
let numSm = cuda_context.GetDeviceInfo().MultiProcessorCount // The number of streaming multiprocessors on the device.

// Set the Cuda libraries handles to the above stream.
let cublas = CudaBlas(PointerMode.Host,AtomicsMode.Allowed) // Better performance for some solver functions with atomics allowed. The Spiral library does not use them though.
let cudnn = new CudaDNNContext()
let cudaRandom = new CudaRand.CudaRandDevice(GeneratorType.PseudoDefault)


let inline divup a b = (a-1)/b+1 // Integer division with rounding up. (a+b-1)/b is another variant on this.

let visual_studio_path = //"C:/Program Files (x86)/Microsoft Visual Studio 14.0" usually.
    let x = Environment.GetEnvironmentVariable("VS140COMNTOOLS")
    if x <> null then IO.Directory.GetParent(x).Parent.Parent.FullName
    else failwith "VS140COMNTOOLS environment variable not found. Make sure VS2015 is installed."

let cuda_toolkit_path = 
    let x = System.Environment.GetEnvironmentVariable("CUDA_PATH_V8_0")
    if x <> null then x
    else failwith "CUDA_PATH_V8_0 environment variable not found. Make sure Cuda 8.0 SDK is installed."

let cub_path = // The path for the Cuda Unbound library.
    let x = System.Environment.GetEnvironmentVariable("CUB_PATH")
    if x <> null then x
    else 
        failwith 
            """If you are getting this exception then that means that CUB_PATH environment variable is not defined.

Go to: https://nvlabs.github.io/cub/index.html#sec6
...and download the latest version of the library, extract it somewhere like, 
eg. : C:\cub-1.6.3
and add that directory to the global enviroment by creating the CUB_PATH variable with a pointer to it."""

let kernels_dir = IO.Path.Combine(__SOURCE_DIRECTORY__,"Cuda Kernels")
IO.Directory.CreateDirectory(kernels_dir) |> ignore // Creates the Cuda Kernels directory if it does not exist. WriteAllBytes would otherwise throw an exception.

let compile_kernel_nvrtc (kernel_code: string) (kernel_name: string) = 
    let kernel_path = IO.Path.Combine(kernels_dir,kernel_name)
    let k = new ManagedCuda.NVRTC.CudaRuntimeCompiler(kernel_code,kernel_name)
    try k.Compile(
            [|
            "-arch=compute_30"
            "--std=c++11" // Surprisingly, NVRTC does support C++11 which means lambdas. Unfortunately, it does not support Thrust, so no tuples.
                          // ...So it is still pretty useless all things considered compared to NVCC.
            |])
    with 
    | :? NVRTCException as x -> 
        printfn "%s" (k.GetLogAsString())
        reraise()
    let ptx = k.GetPTX()
    IO.File.WriteAllBytes(kernel_path,ptx)
    cuda_context.LoadKernelPTX(ptx,kernel_name)

/// Puts quotes around the string.
let quote x = sprintf "\"%s\"" x

let inline compile_kernel_using_nvcc_bat_router (kernel_code: string) (kernel_name: string) =
    let nvcc_router_path = Path.Combine(kernels_dir,"nvcc_router.bat")
    use p = 
        let procStartInfo = 
            ProcessStartInfo(
                RedirectStandardOutput = true,
                RedirectStandardError = true,
                UseShellExecute = false,
                FileName = nvcc_router_path)

        let outputHandler f (_sender:obj) (args:DataReceivedEventArgs) = f args.Data
        let p = new Process(StartInfo = procStartInfo)
        let print_to_standard_output = outputHandler <| fun x -> printfn "%s" x
        //p.OutputDataReceived.AddHandler(DataReceivedEventHandler (print_to_standard_output))
        p.ErrorDataReceived.AddHandler(DataReceivedEventHandler (print_to_standard_output))
        p

    let call x = sprintf "call %s" x
    let quoted_vs_path_to_vcvars = Path.Combine(visual_studio_path, @"VC\bin\x86_amd64\vcvarsx86_amd64.bat") |> quote
    let quoted_vs_path_to_cl = Path.Combine(visual_studio_path, @"VC\bin\x86_amd64") |> quote
    let quoted_cuda_toolkit_path_to_include = Path.Combine(cuda_toolkit_path,"include") |> quote
    let quoted_cub_path_to_include = cub_path |> quote
    let quoted_kernels_dir = kernels_dir |> quote
    let target_path = Path.Combine(kernels_dir,kernel_name+".ptx")
    let quoted_target_path = target_path |> quote
    let input_path = Path.Combine(kernels_dir,"_temp.cu")
    let quoted_input_path = input_path |> quote
    
    if File.Exists input_path then File.Delete input_path
    File.WriteAllText(input_path,kernel_code)

    let _ = 
        if File.Exists nvcc_router_path then File.Delete nvcc_router_path
        use nvcc_router_file = File.OpenWrite(nvcc_router_path)
        use nvcc_router_stream = new StreamWriter(nvcc_router_file)

        nvcc_router_stream.WriteLine(call quoted_vs_path_to_vcvars)
        nvcc_router_stream.WriteLine(
            sprintf 
                """nvcc -gencode=arch=compute_30,code=\"sm_30,compute_30\" --use-local-env --cl-version 2015 -ccbin %s  -I%s -I%s --keep-dir %s -maxrregcount=0  --machine 64 -ptx -cudart static  -o %s %s"""
                quoted_vs_path_to_cl quoted_cuda_toolkit_path_to_include quoted_cub_path_to_include quoted_kernels_dir quoted_target_path quoted_input_path)

    if p.Start() = false then failwith "NVCC failed to run."
    p.BeginOutputReadLine()
    p.BeginErrorReadLine()
    p.WaitForExit()

    if p.ExitCode <> 0 then failwithf "NVCC failed compilation with code %i" p.ExitCode

    cuda_context.LoadKernelPTX(target_path,kernel_name)

let load_kernel compile_kernel (kernel_code: string) (kernel_name: string) = 
    let kernel_path = IO.Path.Combine(kernels_dir,kernel_name)
        
    if IO.File.Exists(kernel_path) 
    then cuda_context.LoadKernelPTX(kernel_path,kernel_name) // For all the modules, it takes roughly 0.35s to compile them. Loading them from drive takes less than a millisecond.
    else compile_kernel kernel_code kernel_name

let inline load_kernel_nvrtc kernel_code kernel_name = load_kernel compile_kernel_nvrtc kernel_code kernel_name
let inline load_kernel_nvcc kernel_code kernel_name = load_kernel compile_kernel_using_nvcc_bat_router kernel_code kernel_name

[<Flags>]
type CudaTypeFlags =
| CudaConstantType = 1
| CudaSharedType = 2
| CudaRestrictedType = 4 // methods arguments
| CudaGlobalType = 8 // methods only
| CudaDeviceType = 16 // methods only
| CudaPointerType = 32
| CudaPointerPointerType = 64

type CudaType =
    | CudaVoid of flags: CudaTypeFlags
    | CudaFloat of flags: CudaTypeFlags
    | CudaInt of flags: CudaTypeFlags
    | CudaAuto of flags: CudaTypeFlags

    member t.Flag = match t with CudaVoid x | CudaFloat x | CudaInt x | CudaAuto x -> x
    member t.IsPointer = t.Flag &&& CudaTypeFlags.CudaPointerType <> enum<_>(0)

type CudaConstraint = 
// This one is a bit special. It contrains the outer dimensions of an argument to some variable for easy later access
// using VarAr2d and such. When using DeclAr2d, the constraints are added implicitly to the environment.
| Constraint of var: CudaExpr

and CudaModuleArgument =
| Arg of typ: CudaType * name: string * CudaConstraint list

and CudaExpr =
// Main AST definitions.
| Seq of CudaExpr list
| Include of string
| Define of string
| ExternCBlock of CudaExpr
| Method of return_type: CudaType * name: string * args: CudaModuleArgument list * body: CudaExpr
| Var of string
| Value of string
| DeclVar of typ: CudaType * name: string * initializer: CudaExpr
| DeclAr1d of typ: CudaType * name: string * size: CudaExpr // For declaring local 1D arrays
| DeclAr2d of typ: CudaType * name: string * size1: CudaExpr * size2: CudaExpr
| VarAr1d of name: string * accessor: CudaExpr
| VarAr2d of name: string * accessor1: CudaExpr * accessor2: CudaExpr // The environment will track the size of the array and multiply accessor1 by size2.
| For of initializer: CudaExpr * cond: CudaExpr * incrementor: CudaExpr * body: CudaExpr
| While of cond: CudaExpr * body: CudaExpr
| Return of CudaExpr
| Call of name: string * CudaExpr list
| IfVoid of cond: CudaExpr * true_: CudaExpr * false_: CudaExpr
| NoExpr // Does nothing. Can be inserted into the else part of IfVoid so the else does not get printed.
| If of cond: CudaExpr * true_: CudaExpr * false_: CudaExpr // For ?: C style conditionals.

// Primitive operations on expressions.
| Add of CudaExpr * CudaExpr
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

type CudaEnvironment =
    {
    indentation: int
    variables: Map<string, CudaType * CudaConstraint list>
    }

    /// Immutably increments the indentation by 4.
    member t.PlusIndent = {t with indentation = t.indentation+4}

let codegen (exp: CudaExpr) =
    let env = {indentation=0;variables=Map.empty}
    let program = Text.StringBuilder()
    let pp (x: string) = 
        program.Append x |> ignore
    let ppln (x: string) = 
        program.AppendLine x |> ignore
    let rec gen (exp: CudaExpr) env =
        let ind() =
            program.Append (String.replicate env.indentation " ") |> ignore
        let ind'() =
            program.Append (String.replicate (env.indentation+4) " ") |> ignore

        let match_flags_prefix_method_return x = 
            if x &&& CudaTypeFlags.CudaConstantType <> enum<_>(0) then pp "const "
            if x &&& CudaTypeFlags.CudaSharedType <> enum<_>(0) then failwith "Method's return type can't be __shared__."
            if x &&& CudaTypeFlags.CudaRestrictedType <> enum<_>(0) then failwith "Method's return type can't be restricted."
            if x &&& CudaTypeFlags.CudaGlobalType <> enum<_>(0) then pp "__global__ "
            if x &&& CudaTypeFlags.CudaDeviceType <> enum<_>(0) then pp "__device__ "
        let match_flags_prefix_method_argument x = 
            if x &&& CudaTypeFlags.CudaConstantType <> enum<_>(0) then pp "const "
            if x &&& CudaTypeFlags.CudaSharedType <> enum<_>(0) then pp "__shared__ "
            if x &&& CudaTypeFlags.CudaRestrictedType <> enum<_>(0) then pp "__restricted__ "
            if x &&& CudaTypeFlags.CudaGlobalType <> enum<_>(0) then failwith "Method's argument type can't be __global__."
            if x &&& CudaTypeFlags.CudaDeviceType <> enum<_>(0) then failwith "Method's argument type can't be __device__."
        let match_flags_prefix_local x = 
            if x &&& CudaTypeFlags.CudaConstantType <> enum<_>(0) then pp "const "
            if x &&& CudaTypeFlags.CudaSharedType <> enum<_>(0) then pp "__shared__ "
            if x &&& CudaTypeFlags.CudaRestrictedType <> enum<_>(0) then pp "Local variable's type can't be __restricted__ "
            if x &&& CudaTypeFlags.CudaGlobalType <> enum<_>(0) then pp "Local variable's type can't be __global__ "
            if x &&& CudaTypeFlags.CudaDeviceType <> enum<_>(0) then pp "Local variable's type can't be __device__ "
        let match_flags_postfix x = 
            if x &&& CudaTypeFlags.CudaPointerType <> enum<_>(0) then pp "* "
            if x &&& CudaTypeFlags.CudaPointerPointerType <> enum<_>(0) then pp "** "

        let print_type match_flags_prefix match_flags_postfix x =
            let x, flags = 
                match x with
                | CudaVoid flags -> "void ", flags
                | CudaFloat flags -> "float ", flags
                | CudaInt flags -> "int ", flags
                | CudaAuto flags -> "auto ", flags
            match_flags_prefix flags
            pp x
            match_flags_postfix flags
        
        match exp with
        | Seq(h::t) ->
            ind()
            gen h env.PlusIndent
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
            match x with
            | Seq _ -> ()
            | _ -> ind'()
            gen x env.PlusIndent
            ind(); ppln "}"
        | Method(return_type, name, args, body) ->
            let rec print_arguments = function
                | (Arg(typ,name,constraints)) :: t -> // TODO: Deal with contraints tomorrow.
                    print_type match_flags_prefix_method_argument match_flags_postfix typ
                    pp name
                    if t.IsEmpty = false then pp ", "
                | [] -> ()
            let rec print_contraints = function
                | (Arg(typ,name,constraints)) :: t -> // TODO: Deal with contraints tomorrow.
                    if typ.IsPointer then
                        let rec subprint = function
                            | h :: t -> 
                                ind(); pp "__"; pp name
                                subprint t
                            | [] -> ()
                        
                    
                | [] -> ()

            print_type match_flags_prefix_method_return match_flags_postfix return_type
            pp name; pp "("; print_arguments args; ppln ") {"

            gen body env.PlusIndent
            ind(); ppln "}"
        | Var x -> pp x
        | Value x -> pp x
        | DeclVar(typ, name, initializer) ->
            print_type match_flags_prefix_local match_flags_postfix typ
            pp name; pp " = "; gen initializer env; ppln ";"
        | DeclAr1d(typ, name, size) -> //TODO: Contraints. Decide to whether to keep track of them in env.
            print_type match_flags_prefix_local match_flags_postfix typ
            pp "const auto __"; pp name; pp "_size = "; gen size env; ppln ";"
            ind(); pp name; pp "["; gen size env; ppln "];"
        | DeclAr2d(typ, name, size1, size2) -> //TODO: Contraints. Decide to whether to keep track of them in env.
            print_type match_flags_prefix_local match_flags_postfix typ
            pp "const auto __"; pp name; pp "_size1 = "; gen size1 env; ppln ";"
            ind(); pp "const auto __"; pp name; pp "_size2 = "; gen size2 env; ppln ";"
            ind(); pp name; pp "["; gen size1 env; pp " * "; gen size2 env; ppln "];"
        | VarAr1d(name: string, accessor: CudaExpr) ->
            pp name; pp "["; gen accessor  env; pp "]" // TODO: Optionally I might want to provide default values for out of bound accesses or throw error. Deal with that.
        | VarAr2d(name: string, accessor1: CudaExpr, accessor2: CudaExpr) -> //TODO: Contraints. Decide to whether to keep track of them in env.
            pp name; pp "["; gen accessor1  env; pp " * __"; pp name; pp "_size2 + "; gen accessor2  env; pp "]"
        | For(initializer: CudaExpr, cond: CudaExpr, incrementor: CudaExpr, body: CudaExpr) ->
            pp "for ("; gen initializer env; pp ", "; gen cond env; pp ", "; gen incrementor env; ppln "){"
            gen body env.PlusIndent
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
        | If(c, t, f) -> pp "("; gen c env; pp ") ? "; gen t env; pp " : "; gen f env
        // Primitive operations on expressions.
        | Add(a,b) -> pp "("; gen a env; pp " + "; gen b env; pp ")"
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

            
    gen exp env
    program.ToString()