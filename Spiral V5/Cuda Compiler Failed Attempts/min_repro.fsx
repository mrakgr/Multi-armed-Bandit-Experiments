type ArgsPrinter = ArgsPrinter with
    static member inline PrintArg(_: ArgsPrinter, (x1, x2)) = 
        let inline print_arg x = 
            let inline call (tok : ^T) = ((^T or ^in_) : (static member PrintArg: ArgsPrinter * ^in_ -> string) tok, x)
            call ArgsPrinter
        [|print_arg x1;print_arg x2|] |> String.concat ", "
    static member inline PrintArg(_: ArgsPrinter, (x1, x2, x3)) = 
        let inline print_arg x = 
            let inline call (tok : ^T) = ((^T or ^in_) : (static member PrintArg: ArgsPrinter * ^in_ -> string) tok, x)
            call ArgsPrinter
        [|print_arg x1;print_arg x2;print_arg x3|] |> String.concat ", "
