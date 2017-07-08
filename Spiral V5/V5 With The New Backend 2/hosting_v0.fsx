#load "../Scripts/load-project-release.fsx"

open Microsoft.FSharp.Compiler.SourceCodeServices
open Microsoft.FSharp.Compiler.Interactive.Shell

open System
open System.IO
open System.Text

// Intialize output and input streams
let sbOut = new StringBuilder()
let sbErr = new StringBuilder()
let inStream = new StringReader("")
let outStream = new StringWriter(sbOut)
let errStream = new StringWriter(sbErr)

// Build command line arguments & start FSI session
let argv = [| "fsi.exe" |]
let allArgs = Array.append argv [|"--noninteractive"|]

let fsiConfig = FsiEvaluationSession.GetDefaultConfiguration()
let fsiSession = FsiEvaluationSession.Create(fsiConfig, allArgs, inStream, outStream, errStream)

/// Evaluate expression & return the result
let evalExpression text =
    match fsiSession.EvalExpression(text) with
    | Some value -> printfn "%A" value.ReflectionValue
    | None -> printfn "Got no result!"

/// Evaluate expression & return the result, strongly typed
let evalExpressionTyped<'T> (text) = 
    match fsiSession.EvalExpression(text) with
    | Some value -> value.ReflectionValue :?> 'T
    | None -> failwith "Got no result!"

evalExpressionTyped<int> "42+1"  // gives '43'

