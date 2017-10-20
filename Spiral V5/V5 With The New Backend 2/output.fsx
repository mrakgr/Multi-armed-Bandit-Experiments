let (var_0: string) = System.Environment.GetEnvironmentVariable("VS140COMNTOOLS")
let (var_1: bool) = Microsoft.FSharp.Core.Operators.isNull(var_0)
if var_1 then
    (failwith "VS140COMNTOOLS environment variable not found. Make sure VS2015 is installed.")
else
    ()
let (var_2: System.IO.DirectoryInfo) = System.IO.Directory.GetParent(var_0)
let (var_3: System.IO.DirectoryInfo) = var_2.get_Parent()
let (var_4: System.IO.DirectoryInfo) = var_3.get_Parent()
let (var_5: string) = var_4.get_FullName()
let (var_6: System.Diagnostics.ProcessStartInfo) = System.Diagnostics.ProcessStartInfo()
var_6.set_RedirectStandardOutput(true)
var_6.set_RedirectStandardError(true)
var_6.set_UseShellExecute(false)
var_6.set_FileName("nvcc_router.bat")
let (var_7: System.Diagnostics.Process) = System.Diagnostics.Process()
var_7.set_StartInfo(var_6)
var_6
