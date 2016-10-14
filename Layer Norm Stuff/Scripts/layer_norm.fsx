// I am going to have to make my own layer norm implementation.
// Well for the forward pass I'll try lift the kernels by Futhark, but
// I also need to figure out the backwards phase as well.

// I'll do it in this script here.

// Edit: Ern, I will bring in Diffsharp for this. I could write the 
// forward pass in one swoop, but as expected, the backwards pass
// will be difficult.

#load "load-references-release.fsx"

open System
open DiffSharp
open DiffSharp.AD.Float32

let forward_layer_norm' (a: float32[]) =
    let mean = Array.sum a / float32 a.Length

    let std = 
        Array.map (fun e -> (e - mean)*(e - mean)) a
        |> Array.sum
        |> fun x -> (x / float32 a.Length) |> sqrt

    Array.map (fun x -> (x - mean) / std) a, mean, std

let forward_layer_norm = forward_layer_norm' >> fun (x,_,_) -> x

let squared_error_cost (a: float32[]) =
    Array.map (fun e -> e*e) a
    |> Array.sum
    |> fun x -> 0.5f * x / float32 a.Length


let gradients (x: float32[]) =
    let epsilon = 0.001f
    let get_grad f (ar: float32[]) =
        [|
        for i=0 to ar.Length-1 do
            let orig = ar.[i]
            let cost() = f ar

            ar.[i] <- orig + epsilon
            let cost_plus_epsilon = cost()
            ar.[i] <- orig - epsilon
            let cost_minus_epsilon = cost()
            ar.[i] <- orig // Restore the original
            let approx_gradient = (cost_plus_epsilon - cost_minus_epsilon) / (2.0f * epsilon)
            yield approx_gradient
            |]

    let input_grad_fun x = forward_layer_norm x |> squared_error_cost
    let input_approx_gradients = get_grad input_grad_fun x
    let output_approx_gradients = get_grad squared_error_cost (forward_layer_norm x)
    input_approx_gradients, output_approx_gradients

// | Div_D_D(a, b) -> pushRec ((bx (d.A / b.P) a) :: (bx (d.A * (-a.P / (b.P * b.P))) b) :: t)
let forward_layer_norm_dv' (a: DV) =
    let mean = DV.mean a

    let std = 
        a - mean
        |> fun x -> x .* x
        |> DV.mean
        |> D.Sqrt

    (a - mean) / std, mean, std

let forward_layer_norm_dv = forward_layer_norm_dv' >> fun (x,_,_) -> x

let squared_error_cost_dv (a: DV) =
    0.5f * (a .* a |> DV.mean)

let rng = Random(42)
let a = Array.init 20 (fun _ -> rng.NextDouble() + 1.0 |> float32)

let input = a
let forward, mean, std = forward_layer_norm' a
let output = forward |> squared_error_cost

let diffsharp_input = DV(input) |> makeReverse 0u
let diffsharp_forward = diffsharp_input |> forward_layer_norm_dv
let diffsharp_output = diffsharp_forward |> squared_error_cost_dv

diffsharp_forward.A
reverseProp (D 1.0f) diffsharp_output

let input_approx_gradients, output_approx_gradients = gradients a // These are too imprecise.


    
