// I am going to have to make my own layer norm implementation.
// Well for the forward pass I'll try lift the kernels by Futhark, but
// I also need to figure out the backwards phase as well.

// I'll do it in this script here.

open System

let forward_layer_norm (a: float32[]) =
    let mean = Array.sum a / float32 a.Length

    let std = 
        Array.map (fun e -> (e - mean)*(e - mean)) a
        |> Array.sum
        |> fun x -> (x / float32 a.Length) |> sqrt

    Array.map (fun x -> (x - mean) / std) a, std, mean

let squared_error_cost (a: float32[]) =
    Array.map (fun e -> e*e) a
    |> Array.sum
    |> fun x -> 0.5f * x / float32 a.Length

let rng = Random()
let a = Array.init 20 (fun _ -> rng.NextDouble() + 1.0 |> float32)

let b,std,mean = forward_layer_norm a

let backward_layer_norm (output_primal: float32[]) (output_adjoint: float32) (input_primal: float32[]) std mean  = 
    