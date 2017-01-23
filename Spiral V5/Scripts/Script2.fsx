type CudaInnerExpr<'t> = CudaInnerExpr of expr: string with
    member t.Expr = t |> fun (CudaInnerExpr expr) -> expr

type CudaScalar<'t> = CudaScalar of name: string with
    member t.Name = t |> fun (CudaScalar name) -> name

type CudaAr1D<'t> = CudaAr1D of CudaScalar<int> * name: string with
    member t.Name = t |> fun (CudaAr1D (_, name)) -> name

type CudaAr2D<'t> = CudaAr2D of CudaScalar<int> * CudaScalar<int> * name: string with
    member t.Name = t |> fun (CudaAr2D (_, _, name)) -> name

type ArgsPrinter = ArgsPrinter
let inline print_arg x =
    let inline call (tok : ^T) = ((^T or ^in_) : (static member PrintArg: ArgsPrinter * ^in_ -> string) tok, x)
    call ArgsPrinter       

type ArgsPrinter with
    static member inline PrintArg(_: ArgsPrinter, t: CudaScalar<float32>) = sprintf "float %s" t.Name
    static member inline PrintArg(_: ArgsPrinter, t: CudaScalar<int>) = sprintf "int %s" t.Name
    static member inline PrintArg(_: ArgsPrinter, t: CudaAr1D<float32>) = sprintf "float *%s" t.Name
    static member inline PrintArg(_: ArgsPrinter, t: CudaAr1D<int>) = sprintf "int *%s" t.Name
    static member inline PrintArg(_: ArgsPrinter, t: CudaAr2D<float32>) = sprintf "float *%s" t.Name
    static member inline PrintArg(_: ArgsPrinter, t: CudaAr2D<int>) = sprintf "int *%s" t.Name

    static member inline PrintArg(_: ArgsPrinter, (x1, x2)) = [|print_arg x1;print_arg x2|] |> String.concat ", "
    static member inline PrintArg(_: ArgsPrinter, (x1, x2, x3)) = [|print_arg x1;print_arg x2;print_arg x3|] |> String.concat ", "

let size = CudaScalar "size" : CudaScalar<int>
let x = CudaAr2D(size, size, "x") : CudaAr2D<float32>

let l = print_arg ((x,x),(x,x))