type CudaScalar<'t> = CudaScalar of name: string with
    member t.Name = t |> fun (CudaScalar name) -> name

type TypePrinter = TypePrinter

let inline print_type x =
    let inline call (tok: ^T) = ((^T or ^in_) : (static member PrintType: TypePrinter * ^in_ -> string) tok, x)
    call TypePrinter

type TypePrinter with
    static member inline PrintType(_: TypePrinter,_: float32) = "float32"
    static member inline PrintType(_: TypePrinter,_ : int) = "int"

type ArgsPrinter = ArgsPrinter
let inline print_arg x =
    let inline call (tok : ^T) = ((^T or ^in_) : (static member PrintArg: ArgsPrinter * ^in_ -> string) tok, x)
    call ArgsPrinter       

type ArgsPrinter with
    static member inline PrintArg(_: ArgsPrinter, t: CudaScalar< ^t>) = 
        [|print_type Unchecked.defaultof< ^t>; t.Name|] |> String.concat " "

//let x: CudaScalar<float32> = CudaScalar "x"
//print_arg x