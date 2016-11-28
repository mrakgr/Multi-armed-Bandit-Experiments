// V4 made me reach my limit in terms of that particular coding style. I am going to focus on maximizing expressive power and embracing dynamism
// in this iteration. I will not make an effort to make sure all the type checking passes muster in this version.
// Unlike last time, this time I will start directly from the Cuda modules. The theme of SpiralV5 is interfacing with the outside world.

//[<AutoOpen>]
//module SpiralV5.Main

#load "load-project-release.fsx"

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
and add that directory to the global enviroment by creating the CUB_PATH variable with a pointer to it.

Unlike with VS and Cuda SDK, this will have to be done manually."""

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

type unit_to_unit_delegate = delegate of unit -> unit
let add_callback_to_stream (str : CudaStream) (callback : unit -> unit) =
    let callb (str : CUstream) (res : CUResult) (p : nativeint) =
        let t : unit_to_unit_delegate = Runtime.InteropServices.Marshal.GetDelegateForFunctionPointer(p)
        t.Invoke()

    let aux = new unit_to_unit_delegate (callback)
    let ptr_to_aux = Marshal.GetFunctionPointerForDelegate aux

    let cuda_callback = CUstreamCallback(callb)
    str.AddCallback(cuda_callback,ptr_to_aux,CUStreamAddCallbackFlags.None)

//     Some formal rules for memory allocation
//    - On first allocation the memory is initialized to zero
//    - On the forward pass the primals of the objects in the ObjectPool are set to zero inside the functions during usage
//    - On the forward pass the adjoints of the objects in the ObjectPool are set to zero inside the ObjectPool
//    - During the optimization phase, the optimizers are responsible for setting the adjoints of the weight nodes to zero.

// Helper functions
let inline dispose (v: #IDisposable) = v.Dispose()

/// Copies a host array to device.
let inline to_dev (host_ar: 't []) =
    let d_a = new CudaDeviceVariable<'t>(SizeT host_ar.Length)    
    d_a.CopyToDevice(host_ar)
    d_a

/// Copies a device array to host.
let inline to_host (dev_ar: CudaDeviceVariable<'t>) =
    let h_a = Array.zeroCreate<'t> (int dev_ar.Size)
    dev_ar.CopyToHost(h_a)
    cuda_context.Synchronize()
    h_a

/// Copies the device array to host. Extends the CudaDeviceVariable class.
type CudaDeviceVariable<'t when 't: struct and 't: (new: unit -> 't) and 't:> System.ValueType> with
    member inline this.Gather() =
        to_host this

let defaultLayout = cudnnTensorFormat.NCHW
let defaultType = cudnnDataType.Float
let defaultMaxPoolingNanOption = cudnnNanPropagation.PropagateNan
let defaultReluNanOption = cudnnNanPropagation.PropagateNan

type TensorDescriptor with
    /// Extended method that works according to the bound defaultLayout and defaultType variables.
    member inline t.SetTensor4dDescriptor(n,c,h,w) = t.SetTensor4dDescriptor(defaultLayout,defaultType,n,c,h,w)

type FilterDescriptor with
    /// Extended method that works according to the bound defaultType variable.
    member inline t.SetFilter4dDescriptor(n,c,h,w) = t.SetFilter4dDescriptor(defaultType,defaultLayout,n,c,h,w)

type ConvolutionParameters = {
    pad_h : int
    pad_w : int
    stride_h : int
    stride_w : int
    upscale_h : int
    upscale_w : int
    mode : cudnnConvolutionMode
    }

type PoolingParameters =
    {
    mode : cudnnPoolingMode
    windowHeight : int
    windowWidth : int
    verticalPadding : int
    horizontalPadding : int
    verticalStride : int
    horizontalStride : int
    }

type PoolingDescriptor with
    member inline t.SetPooling2dDescriptor (p : PoolingParameters) =
        t.SetPooling2dDescriptor(p.mode,defaultMaxPoolingNanOption,p.windowHeight,p.windowWidth,p.verticalPadding,p.horizontalPadding,p.verticalStride,p.horizontalStride)

    member inline t.GetPooling2dForwardOutputDim s =
        let mutable n,c,h,w = 0,0,0,0
        t.GetPooling2dForwardOutputDim(s,&n,&c,&h,&w)
        n,c,h,w

let defaultConvPar = 
    {
    pad_h = 0
    pad_w = 0
    stride_h = 1
    stride_w = 1
    upscale_h = 1
    upscale_w = 1
    mode = cudnnConvolutionMode.Convolution
    }

type ConvolutionDescriptor with
    member inline t.SetConvolution2dDescriptor (p : ConvolutionParameters) =
        t.SetConvolution2dDescriptor(p.pad_h,p.pad_w,p.stride_h,p.stride_w,p.upscale_h,p.upscale_w,p.mode, defaultType)
    member inline t.GetConvolution2dForwardOutputDim (s,f) =
        let mutable n,c,h,w = 0,0,0,0
        t.GetConvolution2dForwardOutputDim(s,f,&n,&c,&h,&w)
        n,c,h,w

type DeadFlagType =
| Undefined
| Dead
| Alive

type AutoDiffType =
    | PrimalOnly of CudaDeviceVariable<float32> // Does no AD
    | PrimalAndAdjoint of CudaDeviceVariable<float32> * CudaDeviceVariable<float32> // Does first order AD

    static member CreatePrimalOnly(size: int) =
        new CudaDeviceVariable<float32>(SizeT size)
        |> fun x -> x.Memset(0u); x // Zeroes the variable (saves me from having to zero out the adjoint on the first run)
        |> PrimalOnly

    static member CreatePA(size: int) =
        // Zeroes the variables (saves me from having to zero out the adjoint on the first run)
        let a = new CudaDeviceVariable<float32>(SizeT size) |> fun x -> x.Memset(0u); x
        let b = new CudaDeviceVariable<float32>(SizeT size) |> fun x -> x.Memset(0u); x
        PrimalAndAdjoint(a,b)

    static member private resize (size: int) (v: CudaDeviceVariable<float32>) = 
        if size <= int v.Size then v
        else
            v.Dispose()
            new CudaDeviceVariable<float32>(SizeT size)

    member t.ResizePrimalOnly(size: int) =
        match t with
        | PrimalOnly v -> PrimalOnly(AutoDiffType.resize size v)
        | PrimalAndAdjoint(a,b) -> PrimalAndAdjoint(AutoDiffType.resize size a, AutoDiffType.resize size b) // Does not resize downwards.

    member t.ResizePrimalAndAdjoint(size: int) =
        match t with
        | PrimalOnly v -> PrimalAndAdjoint(AutoDiffType.resize size v, new CudaDeviceVariable<float32>(SizeT size))
        | PrimalAndAdjoint(a,b) -> PrimalAndAdjoint(AutoDiffType.resize size a, AutoDiffType.resize size b)

    member t.Resize(size: int) =
        match t with
        | PrimalOnly v -> PrimalOnly(AutoDiffType.resize size v)
        | PrimalAndAdjoint(a,b) -> PrimalAndAdjoint(AutoDiffType.resize size a, AutoDiffType.resize size b)
        
    member t.P =
        match t with
        | PrimalOnly v -> v
        | PrimalAndAdjoint(a,b) -> a

    member t.A =
        match t with
        | PrimalOnly v -> failwith "No adjoint!"
        | PrimalAndAdjoint(a,b) -> b

    member t.PA = t.P, t.A

    member t.CopyToPrimal(host_ar: float32[]) =
        let x = t.P
        if int x.Size <> host_ar.Length then failwithf "int x.Size(%i) <> host_ar.Length(%i)" (int x.Size) (host_ar.Length)
        x.CopyToDevice(host_ar)

    member t.HasAdjoint =
        match t with
        | PrimalOnly v -> false
        | _ -> true

    interface IDisposable with
        member t.Dispose() =
            match t with
            | PrimalOnly v -> v.Dispose()
            | PrimalAndAdjoint(a,b) -> a.Dispose(); b.Dispose()

let size_cr (c,r) = c*r
let size_ncr (n,c,r) = n*c*r
let size_nchw (n,c,h,w) = n*c*h*w

type DM =
    | Df of P: Lazy<float32> ref * A: float32 ref
    | D1M of n: int ref * diff: AutoDiffType ref * is_dead : DeadFlagType ref // flag to skip backprop
    | D2M of cr: (int * int) ref * diff: AutoDiffType ref * is_dead : DeadFlagType ref
    | D3M of ncr: (int * int * int) ref * diff: AutoDiffType ref * is_dead : DeadFlagType ref
    | D4M of nchw: (int * int * int * int) ref * diff: AutoDiffType ref * is_dead : DeadFlagType ref

    member t.diff =
        match t with
        | D1M(_,diff,_) | D2M(_,diff,_) | D3M(_,diff,_) | D4M(_,diff,_) -> diff
        | Df _ -> failwith "Invalid call. Df does not have a diff field."

    member t.is_dead =
        match t with
        | D1M(_,_,is_dead) | D2M(_,_,is_dead) | D3M(_,_,is_dead) | D4M(_,_,is_dead) -> is_dead
        | Df _ -> failwith "Invalid call. Df does not have a is_dead field."

    static member private create_dm size_f constructor_f size is_constant =
        let diff = 
            let s = size_f size
            match is_constant with
            | true -> AutoDiffType.CreatePrimalOnly s
            | false -> AutoDiffType.CreatePA s
        constructor_f(ref size, ref diff, ref Undefined)

    /// Creates a Df type.
    static member create(primal) =
        Df(ref primal, ref 0.0f)

    /// Creates a D1M type.
    static member create(size : int, is_constant) =
        DM.create_dm id D1M size is_constant
    /// Creates a D2M type.
    static member create(size : (int * int), is_constant) =
        DM.create_dm size_cr D2M size is_constant
    /// Creates a D3M type.
    static member create(size : (int * int * int), is_constant) =
        DM.create_dm size_ncr D3M size is_constant
    /// Creates a D4M type.
    static member create(size : (int * int * int * int), is_constant) =
        DM.create_dm size_nchw D4M size is_constant

    static member private create_dm_with_host_data size create_f (host_data : float32[]) is_constant =
        let t: DM = create_f(size, is_constant)
        t.diff.Value.CopyToPrimal(host_data)
        t

    /// Creates a D1M type with host's data.
    static member create(size : int, host_data : float32[], is_constant) =
        DM.create_dm_with_host_data size DM.create host_data is_constant
    /// Creates a D2M type with host's data.
    static member create(size : (int * int), host_data : float32[], is_constant) =
        DM.create_dm_with_host_data size DM.create host_data is_constant
    /// Creates a D3M type with host's data.
    static member create(size : (int * int * int), host_data : float32[], is_constant) =
        DM.create_dm_with_host_data size DM.create host_data is_constant
    /// Creates a D4M type with host's data.
    static member create(size : (int * int * int * int), host_data : float32[], is_constant) =
        DM.create_dm_with_host_data size DM.create host_data is_constant

    static member private replace_if assign_new_size_f size_f (t: DM) size is_constant =
        assign_new_size_f t size
        let new_size = size_f size
        match is_constant with
        | true -> t.diff := t.diff.Value.ResizePrimalOnly new_size
        | false -> t.diff := t.diff.Value.ResizePrimalAndAdjoint new_size

    /// Resizes the object. Does not free memory when resizing downwards.
    static member ReplaceIf(t, size : int, is_constant) =
        DM.replace_if (fun t size -> t |> function D1M(s,_,_) -> s := size) id t size is_constant
    static member ReplaceIf(t, size : int * int, is_constant) =
        DM.replace_if (fun t size -> t |> function D2M(s,_,_) -> s := size) size_cr t size is_constant
    static member ReplaceIf(t, size : int * int * int, is_constant) =
        DM.replace_if (fun t size -> t |> function D3M(s,_,_) -> s := size) size_ncr t size is_constant
    static member ReplaceIf(t, size : int * int * int * int, is_constant) =
        DM.replace_if (fun t size -> t |> function D4M(s,_,_) -> s := size) size_nchw t size is_constant

    /// Sets the adjoint to a value.
    member inline t.SetAdjoint(x: float32, str: CudaStream) = 
        let v = BitConverter.ToUInt32(BitConverter.GetBytes(x),0)
        t.diff.Value.A.MemsetAsync(v,str.Stream)

    /// Set the matrix to a value.
    member inline t.SetPrimal (x: float32, str: CudaStream) = 
        let v = BitConverter.ToUInt32(BitConverter.GetBytes(x),0)
        t.diff.Value.P.MemsetAsync(v,str.Stream)

    /// Returns whether the function has an adjoint
    member t.HasAdjoint = t.diff.Value.HasAdjoint

    interface IDisposable with
        member t.Dispose() = t.diff.Value |> dispose

type Workspace() = 
    let mutable workspace: CudaDeviceVariable<byte> = CudaDeviceVariable.Null

    /// Resizes the workspace if it is less than size and returns it. The size is in 'a.
    member t.ResizeIf<'a when 'a: (new: unit -> 'a) and 'a: struct and 'a :> ValueType>(size: int) =
        let toGeneric(workspace: CudaDeviceVariable<byte>) = new CudaDeviceVariable<'a>(workspace.DevicePointer,false,SizeT (size * sizeof<'a>))
        if size < int workspace.Size then toGeneric workspace
        else
            workspace.Dispose()
            workspace <- new CudaDeviceVariable<byte>(SizeT (size * sizeof<'a>))
            toGeneric workspace

    /// Resizes the workspace if it is less than size and returns it as a d2M.
    member t.ResizeIfD2M((r,c as rc): int*int) =
        let x = t.ResizeIf(r*c)
        D2M(ref rc, ref <| PrimalOnly x, ref Undefined)

    interface IDisposable with
        member t.Dispose() = workspace.Dispose()

/// The new object pool. Zeroes out the adjoints concurrently on the forward phase.
type ObjectPool() =
    let dMPool = ResizeArray()
    let dMp = ref 0

    let tensorDescriptorPool = Dictionary(HashIdentity.Structural)
    let filterDescriptorPool = Dictionary(HashIdentity.Structural)
    let convolutionDescriptorPool = Dictionary(HashIdentity.Structural)
    let poolingDescriptorPool = Dictionary(HashIdentity.Structural)
    let activationDescriptorPool = Dictionary(HashIdentity.Structural)
    let BNDescriptorPool = Dictionary(HashIdentity.Structural)

    static member inline private GetFromPool (pool : ResizeArray<_>) (pointer_to_pool : int ref) (creation_function) =
        if pool.Count > !pointer_to_pool then
            let t = pool.[!pointer_to_pool]
            pointer_to_pool := !pointer_to_pool+1
            t
        else
            let t = creation_function()
            pool.Add(t)
            pointer_to_pool := !pointer_to_pool+1
            t

    static member inline private GetFromDict (pool : Dictionary<_,_>) k creation_function set_function =
        match pool.TryGetValue k with
        | true, v -> v
        | false, _ ->
            let t = creation_function()
            set_function t k
            pool.Add(k, t)
            t

    static member get_dM pool pointer is_constant size creation_f replace_if_f is_inference_only (str: CudaStream): DM =
        let t': DM = ObjectPool.GetFromPool pool pointer (fun _ -> creation_f(size,is_constant))
        replace_if_f(t',size,is_constant)
        t'.is_dead := Undefined
        if is_inference_only = false && t'.HasAdjoint then t'.diff.Value.A.MemsetAsync(0u,str.Stream)
        t'

    member t.GetdM (is_constant, (size : int), is_inference_only, str: CudaStream): DM =
        ObjectPool.get_dM dMPool dMp is_constant size DM.create DM.ReplaceIf is_inference_only str
    member t.GetdM (is_constant, (size : (int*int)), is_inference_only, str: CudaStream): DM =
        ObjectPool.get_dM dMPool dMp is_constant size DM.create DM.ReplaceIf is_inference_only str
    member t.GetdM (is_constant, (size : (int*int*int)), is_inference_only, str: CudaStream): DM =
        ObjectPool.get_dM dMPool dMp is_constant size DM.create DM.ReplaceIf is_inference_only str
    member t.GetdM (is_constant, (size : (int*int*int*int)), is_inference_only, str: CudaStream): DM =
        ObjectPool.get_dM dMPool dMp is_constant size DM.create DM.ReplaceIf is_inference_only str

    member t.GetTensorDescriptor (nchw : int*int*int*int) = 
        ObjectPool.GetFromDict tensorDescriptorPool nchw (fun _ -> new TensorDescriptor()) (fun (t: TensorDescriptor) x -> x |> t.SetTensor4dDescriptor)
    member t.GetFilterDescriptor (nchw : int*int*int*int) = 
        ObjectPool.GetFromDict filterDescriptorPool nchw (fun _ -> new FilterDescriptor()) (fun (t: FilterDescriptor) x -> x |> t.SetFilter4dDescriptor)
    member t.GetConvolutionDescriptor (convPars : ConvolutionParameters) = 
        ObjectPool.GetFromDict convolutionDescriptorPool convPars (fun _ -> new ConvolutionDescriptor()) (fun (t: ConvolutionDescriptor) x -> x |> t.SetConvolution2dDescriptor)
    member t.GetPoolingDescriptor (p : PoolingParameters) = 
        ObjectPool.GetFromDict poolingDescriptorPool p (fun _ -> new PoolingDescriptor()) (fun (t: PoolingDescriptor) x -> x |> t.SetPooling2dDescriptor)
    member t.GetActivationDescriptor (mode : cudnnActivationMode, nanopt : cudnnNanPropagation, reluCeiling as p) = 
        ObjectPool.GetFromDict activationDescriptorPool p (fun _ -> new ActivationDescriptor()) (fun (t: ActivationDescriptor) x -> x |> t.SetActivationDescriptor)
    member t.GetBNDescriptor ((nchw : int*int*int*int, mode : cudnnBatchNormMode, srcDesc : TensorDescriptor) as p) = 
        ObjectPool.GetFromDict BNDescriptorPool p 
            (fun _ -> new TensorDescriptor()) 
            (fun (t: TensorDescriptor) (nchw, mode, srcDesc) -> cudnn.DeriveBNTensorDescriptor(t,srcDesc,mode))

    // Resets the pointer in the object pool
    member t.Reset() =
        dMp := 0

    interface IDisposable with
        member __.Dispose() =
            let inline dis' ex pool = 
                // The disposer helper. Uses an extractor for the dictionary values.
                // This succintly demonstrates how to get around the lack of higher kinded types in F#.
                let pool = ex pool
                for x in pool do dispose x
            let inline dis x = dis' id x
            dis dMPool

            let inline dis x = 
                dis' (fun v -> (^a: (member Values: ^b) v)) x
            dis tensorDescriptorPool // ...It would have been faster to just copy paste .Value everywhere.
            dis filterDescriptorPool
            dis convolutionDescriptorPool
            dis poolingDescriptorPool
            dis activationDescriptorPool
            dis BNDescriptorPool

let T = Operation.Transpose
let nT = Operation.NonTranspose

