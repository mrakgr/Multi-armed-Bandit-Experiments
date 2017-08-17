open System
open System.Collections.Generic

let rng = Random()

let d: Dictionary<int*int*int,int> = Dictionary(HashIdentity.Structural)
#time
for i = 1 to 1000000 do
    let x = rng.Next(),rng.Next(),rng.Next()
    match d.TryGetValue x with
    | true, v -> ()
    | false, _ ->
        d.[x] <- 0
    
let q: Dictionary<int,Dictionary<int,Dictionary<int,int>>> = Dictionary(HashIdentity.Structural)
for i = 1 to 1000000 do
    let x = rng.Next()
    match q.TryGetValue x with
    | true, q ->
        let x = rng.Next()
        match q.TryGetValue x with
        | true, q ->
            let x = rng.Next()
            match q.TryGetValue x with
            | true, q -> ()
            | false, _ -> q.[x] <- 0
        | false, _ -> q.[x] <- Dictionary(HashIdentity.Structural)
    | false, _ -> q.[x] <- Dictionary(HashIdentity.Structural)
        