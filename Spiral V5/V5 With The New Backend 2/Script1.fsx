let f n m =
    let rng = System.Random()
    let ar =
        Array.init n (fun i -> rng.Next(1,m) |> string)
        |> String.concat " "
    sprintf "%i\n%s" n ar
