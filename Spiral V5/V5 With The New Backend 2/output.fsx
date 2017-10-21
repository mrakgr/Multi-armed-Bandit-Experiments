let rec method_0 ((var_0: System.Object)): (System.Diagnostics.DataReceivedEventArgs -> unit) =
    method_1((var_0: System.Object))
and method_1 ((var_1: System.Object)) ((var_0: System.Diagnostics.DataReceivedEventArgs)): unit =
    let (var_2: string) = var_0.get_Data()
    System.Console.WriteLine(var_2)
let (var_0: System.Diagnostics.Process) = System.Diagnostics.Process()
let (var_2: (System.Object -> (System.Diagnostics.DataReceivedEventArgs -> unit))) = method_0
let (var_3: System.Diagnostics.DataReceivedEventHandler) = System.Diagnostics.DataReceivedEventHandler(var_2)
var_0.ErrorDataReceived.AddHandler(var_3)
