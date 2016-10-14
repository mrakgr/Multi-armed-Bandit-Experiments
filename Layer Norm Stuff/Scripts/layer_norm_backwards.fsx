let input =
  [|1.66810644f; 1.14090729f; 1.12551832f; 1.52276433f; 1.16843426f;
    1.26259267f; 1.72440839f; 1.51292276f; 1.17365122f; 1.76125062f;
    1.2345885f; 1.25732136f; 1.50560355f; 1.32023323f; 1.38096964f;
    1.26024461f; 1.51745105f; 1.03531885f; 1.81413734f; 1.57719684f|]

let std : float32 = 0.227641061f
let mean : float32 = 1.39818084f

let input_adjoints = // These are the targets for the backwards phase.
    [|1.49011614e-09f; 1.49011614e-09f; 3.12924371e-08f; 1.49011614e-09f;
      1.63912777e-08f; 1.49011614e-09f; 1.49011614e-09f; -5.96046457e-09f;
      1.49011614e-09f; -2.83122059e-08f; 1.49011614e-09f; 1.63912777e-08f;
      1.49011614e-09f; 1.49011614e-09f; 1.49011614e-09f; 1.63912777e-08f;
      -5.96046457e-09f; 1.49011614e-09f; -2.83122059e-08f; 1.49011614e-09f|]

let output_primals : float32 [] =
  [|1.18575096f; -1.13017201f; -1.19777393f; 0.547280371f; -1.00924933f;
    -0.595622659f; 1.43307865f; 0.504047573f; -0.98633182f; 1.59492218f;
    -0.718641639f; -0.618778884f; 0.471895128f; -0.342414558f; -0.075606741f;
    -0.605937421f; 0.523939788f; -1.5940094f; 1.82724726f; 0.786395907f|]

let output_adjoints =
    [|0.0592875443f; -0.0565086007f; -0.0598886907f; 0.0273640193f;
      -0.050462462f; -0.0297811311f; 0.0716539323f; 0.0252023768f;
      -0.0493165925f; 0.0797461048f; -0.035932079f; -0.0309389438f;
      0.0235947575f; -0.0171207283f; -0.00378033705f; -0.0302968677f;
      0.0261969864f; -0.07970047f; 0.0913623571f; 0.0393197946f|]

let backward_layer_norm (output_p: float32[]) (output_a: float32[]) (input_p: float32[]) mean std = 
    let div_a_b_a_adjoint er _ b_p = er / b_p
    let div_a_b_b_adjoint er a_p b_p = // The adjoint for the right of a division is the derivative of expr ^ -1
        er * (-a_p / (b_p * b_p))

    let l = input_p.Length
    let mutable mean_a = 0.0f
    let input_a = Array.create l 0.0f

    let a_minus_mean = Array.map (fun a -> a - mean) input_p
    let a_minus_mean_a = Array.create l 0.0f

//    let mean = DV.mean a

//    (a - mean) / std

    // First comes the derivative of (a - mean)
    // Looking at it now, I see that it appears in two places and it needs to be cached as well.
    // Here for the sake of brevity, I will just calculate it on the spot and save the optimizations for
    // when I write the Cuda kernels.

    let output_sum_a = Array.sum output_a

    // The adjoint of a_minus_mean
    for i=0 to a_minus_mean_a.Length-1 do
        a_minus_mean_a.[i] <- a_minus_mean_a.[i] + output_a.[i] / std

    // The top adjoint of std
    let std_a = Array.sum (Array.map2 (+) output_a a_minus_mean_a) / std

    // Now comes the pass through the std.

//    let std = 
//        a - mean
//        |> fun x -> x .* x
//        |> DV.mean
//        |> D.Sqrt // | Sqrt_D(a) -> pushRec ((bx (d.A / (D 2.f * d.P)) a) :: t) // d.P = sqrt a.P

    let std_a' =
        std_a
        |> fun er -> er / 2.0f * 

    // ...Ah, fuck this.
    // It is a fundamentally flawed idea to the backprop steps by hand.
    // My mind is starting to loop.

    // I wanted to do it because I wanted to do the forward and the backwards steps through the layer in 
    // a single pass each, but this kind of programming is not what I sought.

    // I guess currently, my programming has some weaknesses.
    // I am really bad at expressing really low level things like Cuda code efficiently
    // and on the other end here, this very high level stuff is similarly unproductive for me.

    // Even if I did this and the code ends up being faster, there is no way I could be satisfied by this.

    // Since it is not like I am on a strict deadline, maybe I will take this opportunity to learn about compilers
    // more. I need new methods and techniques in order to get better.

    ()