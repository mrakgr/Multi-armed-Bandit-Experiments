open System.Collections.Generic

let bind a b ret =
    a <| fun a ->
        b a ret