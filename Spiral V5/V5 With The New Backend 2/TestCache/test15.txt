module SpiralExample
let cuda_kernels = """
extern "C" {
}
"""

let (var_0: string) = cuda_kernels
let (var_1: System.Text.StringBuilder) = System.Text.StringBuilder("Qwe", 128)
let (var_2: System.Text.StringBuilder) = var_1.Append(123L)
let (var_3: System.Text.StringBuilder) = var_1.AppendLine()
let (var_4: System.Text.StringBuilder) = var_1.Append(123s)
let (var_5: System.Text.StringBuilder) = var_1.AppendLine()
let (var_6: System.Text.StringBuilder) = var_1.Append("qwe")
let (var_7: System.Text.StringBuilder) = var_1.AppendLine()
let (var_8: string) = var_1.ToString()
System.Console.Write(var_8)
let (var_9: System.Collections.Generic.Dictionary<int64,int64>) = System.Collections.Generic.Dictionary<int64,int64>(128)
var_9.Add(1L, 2L)
var_9.get_Item(1L)
