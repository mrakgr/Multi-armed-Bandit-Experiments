let rec method_0 ((var_0: System.Object)): (System.Diagnostics.DataReceivedEventArgs -> unit) =
    method_1((var_0: System.Object))
and method_1 ((var_1: System.Object)) ((var_0: System.Diagnostics.DataReceivedEventArgs)): unit =
    let (var_2: string) = var_0.get_Data()
    System.Console.WriteLine(var_2)
let (var_1: (System.Object -> (System.Diagnostics.DataReceivedEventArgs -> unit))) = method_0
System.Diagnostics.DataReceivedEventHandler(var_1)
