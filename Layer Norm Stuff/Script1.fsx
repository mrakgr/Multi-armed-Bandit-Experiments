let str = "Hello World!"

// Convert to an int array and group by key.
let count_array = 
    str.ToCharArray() 
    |> Array.map int 
    |> Array.groupBy id // Groups them by ints
    // Since the ints are the same in this case, we convert the array length to counts.
    // We also convert k back to char.
    |> Array.map (fun (k,ar) -> char k, ar.Length) 
    
printfn "%A" count_array

