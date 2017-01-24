type CudaScalar<'t> = CudaScalar of name: string with
    member t.Name = t |> fun (CudaScalar name) -> name
type CudaAr1D<'t> = CudaAr1D of CudaScalar<int> * name: string with
    member t.Name = t |> fun (CudaAr1D (_, name)) -> name

type TypePrinter<'t>() = class end

let inline print_type x =
    ((^T or ^in_) : (static member PrintType: TypePrinter< ^in_> -> string) x)

type TypePrinter with
    static member inline PrintType(_: TypePrinter<float32>) = "float32"
    static member inline PrintType(_: TypePrinter<int>) = "int"

type ArgsPrinter = ArgsPrinter with
    static member inline PrintArg(_: ArgsPrinter, t: CudaScalar< ^t>) = 
        [|print_type (TypePrinter< ^t>()); t.Name|] |> String.concat " "
    static member inline PrintArg(_: ArgsPrinter, t: CudaAr1D< ^t>) = 
        [|print_type (TypePrinter< ^t>()); "*"; t.Name|] |> String.concat " "

let inline print_arg x =
    let inline call (tok : ^T) = ((^T or ^in_) : (static member PrintArg: ArgsPrinter * ^in_ -> string) tok, x)
    call ArgsPrinter       

type ArgsPrinter with
    static member inline PrintArg(_: ArgsPrinter, (x1, x2)) = 
        [|print_arg x1;print_arg x2|] |> String.concat ", "
type ArgsPrinter with
    static member inline PrintArg(_: ArgsPrinter, (x1, x2, x3)) = 
        [|print_arg x1;print_arg x2;print_arg x3|] |> String.concat ", "