#load "SpiralV5DM.fsx"

open SpiralV5
open SpiralV5DM

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
open System.Text
open System.Diagnostics
open System.IO
open System.Collections.Generic
open System.Runtime.InteropServices

let cuda_tag =
    let mutable i = 0
    fun () ->
        i <- i+1
        i

let inline varn x = x + string (cuda_tag())

type CudaExpr<'t> = CudaExpr of expr: string with
    member t.Expr = t |> fun (CudaExpr expr) -> expr
let inline cuda_expr<'t> expr = CudaExpr expr : CudaExpr<'t>

type CudaScalar<'t> = CudaScalar of name: string with
    member t.Name = t |> fun (CudaScalar name) -> name
let inline cuda_scalar<'t> name = CudaScalar name : CudaScalar<'t>

type CudaAr1D<'t> = CudaAr1D of CudaScalar<int> * name: string with
    member t.Name = t |> fun (CudaAr1D (_, name)) -> name
    member t.Access(x: CudaScalar<int>) =
        let name = t |> fun (CudaAr1D(size,name)) -> name
        let scalar_name = x.Name
        let expr = sprintf "%s[%s]" name scalar_name
        cuda_expr<'t> expr
let inline cuda_ar1d<'t> size name = CudaAr1D(size, name) : CudaAr1D<'t>

type TypePrinter<'t>() = class end

let inline print_type x =
    ((^T or ^in_) : (static member PrintType: TypePrinter< ^in_> -> string) x)

type TypePrinter with
    static member inline PrintType(_: TypePrinter<float32>) = "float32"
type TypePrinter with
    static member inline PrintType(_: TypePrinter<int>) = "int"

type ArgsPrinter = ArgsPrinter
let inline print_arg x =
    let inline call (tok : ^T) = ((^T or ^in_) : (static member PrintArg: ArgsPrinter * ^in_ -> string) tok, x)
    call ArgsPrinter       

type ArgsPrinter with
    static member inline PrintArg(_: ArgsPrinter, t: CudaScalar< ^t>) = 
        [|print_type (TypePrinter< ^t>()); t.Name|] |> String.concat " "
type ArgsPrinter with
    static member inline PrintArg(_: ArgsPrinter, t: CudaAr1D< ^t>) = 
        [|print_type (TypePrinter< ^t>()); "*"; t.Name|] |> String.concat " "
type ArgsPrinter with
    static member inline PrintArg(_: ArgsPrinter, (x1, x2)) = 
        [|print_arg x1;print_arg x2|] |> String.concat ", "
type ArgsPrinter with
    static member inline PrintArg(_: ArgsPrinter, (x1, x2, x3)) = 
        [|print_arg x1;print_arg x2;print_arg x3|] |> String.concat ", "
type ArgsPrinter with
    static member inline PrintArg(_: ArgsPrinter, (x1, x2, x3, x4)) = 
        [|print_arg x1;print_arg x2;print_arg x3;print_arg x4|] |> String.concat ", "
type ArgsPrinter with
    static member inline PrintArg(_: ArgsPrinter, (x1, x2, x3, x4, x5)) = 
        [|print_arg x1;print_arg x2;print_arg x3;print_arg x4;print_arg x5|] |> String.concat ", "

type CudaProgram =
| Statement of string
| Indent
| Dedent
| Statements of ResizeArray<CudaProgram>

let state x (program: ResizeArray<_>) = x |> String.concat "" |> Statement |> program.Add

// Inner language.

let inline var_scalar (init: CudaExpr<'t>) program = 
    let var_name = varn "var_"
    let var = cuda_scalar var_name : CudaScalar<'t>
    let typ = print_type (TypePrinter<'t>())
    state [|typ;" ";var_name;" = ";init.Expr;";"|] program; var

let inline var_1d (size: CudaExpr<int>) (init: CudaExpr<'t>) program = 
    let var_name = varn "var_"
    let typ = print_type (TypePrinter<'t>())
    let size: CudaScalar<int> = var_scalar size program // Type error
    let var = cuda_ar1d size var_name : CudaAr1D<'t>
    state [|typ;" ";var_name;"[";size.Name;"]";" = ";init.Expr;";"|] program; var

