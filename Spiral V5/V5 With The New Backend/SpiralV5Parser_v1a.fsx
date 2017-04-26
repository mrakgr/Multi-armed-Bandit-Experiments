#load "SpiralV5CudaTypechecker_v7c'.fsx"
#r "../../packages/FParsec.1.0.2/lib/net40-client/FParsecCS.dll"
#r "../../packages/FParsec.1.0.2/lib/net40-client/FParsec.dll"
//#r "../../packages/FParsec-Pipes.0.3.1.0/lib/net45/FParsec-Pipes.dll"

open SpiralV5CudaTypechecker_v7c'
open FParsec

let comma = spaces >>. skipChar ',' >>. spaces
let standard_identifier = many1Satisfy isAsciiLetter |>> S
let between_parenths p = between (skipChar '(') (skipChar ')') p
let rec standard_pattern x = 
    let standard_pattern = sepBy standard_pattern comma |> between_parenths |>> SS
    (standard_identifier <|> standard_pattern) x

let standard_patterns = sepEndBy1 standard_pattern spaces

run standard_patterns "add (a,b) = a + b;"