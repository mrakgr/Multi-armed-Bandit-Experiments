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

type CudaAr2D<'t> = CudaAr2D of CudaScalar<int> * CudaScalar<int> * name: string with
    member t.Name = t |> fun (CudaAr2D (_, _, name)) -> name
    member t.Access((x: CudaScalar<int>, y: CudaScalar<int>)) =
        let name,size2 = t |> fun (CudaAr2D(size1,size2,name)) -> name, size2.Name
        let scalar_x_name = x.Name
        let scalar_y_name = y.Name
        let expr = sprintf "%s[%s * %s + %s]" name scalar_x_name size2 scalar_y_name
        cuda_expr<'t> expr

let inline cuda_ar2d<'t> (s1,s2) name = CudaAr2D(s1,s2,name) : CudaAr2D<'t>

let inline access x accessor = (^a: (member Access: ^ac -> CudaExpr< ^ex>) x, accessor)
let inline expr x = String.concat "" x |> cuda_expr

let inline unary_fun_op op (x: CudaExpr<'a>) = [|op;"(";x.Expr;")"|] |> expr : CudaExpr<'a>
let inline unary_op op x = [|"(";op;x;")"|] |> expr
let inline binary_op op (x: CudaExpr<'e>) (y: CudaExpr<'e>) = [|"(";x.Expr;op;y.Expr;")"|] |> expr 

let inline exp x = unary_fun_op "exp" x
let inline log x = unary_fun_op "log" x
let inline tanh x = unary_fun_op "tanh" x

type CudaExpr<'t> with
    static member inline (~-)(x) = unary_op "-" x

    static member inline (<)(x,y) = binary_op " < " x y : CudaExpr<bool>
    static member inline (<=)(x,y) = binary_op " <= " x y : CudaExpr<bool>
    static member inline (=)(x,y) = binary_op " == " x y : CudaExpr<bool>
    static member inline (>)(x,y) = binary_op " > " x y : CudaExpr<bool>
    static member inline (>=)(x,y) = binary_op " >= " x y : CudaExpr<bool>

    static member inline (||)(x,y) = binary_op " || " x y : CudaExpr<bool>
    static member inline (&&)(x,y) = binary_op " && " x y : CudaExpr<bool>

    static member inline (+) ((x: CudaExpr<'a>), y) = binary_op " + " x y : CudaExpr<'a>
    static member inline (*) ((x: CudaExpr<'a>), y) = binary_op " * " x y : CudaExpr<'a>
    static member inline (/) ((x: CudaExpr<'a>), y) = binary_op " / " x y : CudaExpr<'a>
    static member inline (-) ((x: CudaExpr<'a>), y) = binary_op " - " x y : CudaExpr<'a>

let inline if_ (cond: CudaExpr<bool>) (true_: CudaExpr<'a>) (false_: CudaExpr<'a>) = 
    [|"((";cond.Expr;") ? (";true_.Expr;") : (";false_.Expr;"))"|] |> expr : CudaExpr<'a>

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
    static member inline PrintArg(_: ArgsPrinter, t: CudaAr2D< ^t>) = 
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

let process_statements (statements: ResizeArray<CudaProgram>) =
    let rec process_statement (code: StringBuilder,ind as state) statement =
        match statement with
        | Statement x -> [|String.replicate ind " "; x; "\n"|] |> expr |> code.Append, ind
        | Indent -> code, ind+4
        | Dedent -> code, ind-4
        | Statements x -> process_statements state x
    and process_statements state (statements: ResizeArray<CudaProgram>) =
        Seq.fold process_statement state statements
    process_statements (StringBuilder(),0) statements
    |> fun (code,ind) -> code.ToString()

let state x (program: ResizeArray<_>) = x |> String.concat "" |> Statement |> program.Add
let enter body v (program: ResizeArray<_>) =
    Indent |> program.Add
    body v
    Dedent |> program.Add

let (|?>) a b program = a program; b program; ()

// Outer language.

let inline method_ rtype kernel_name ins consts outs body =
    let args = print_arg (ins, consts, outs)
    state [|rtype;kernel_name;"(";args;") {"|]
    |?> enter body ()
    |?> state [|"}"|]

let externCBlock body =
    state [|"extern \"C\" {"|]
    |?> enter body ()
    |?> state [|"}"|]

let include_ str =
    state [|"#include "; quote str|]

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
