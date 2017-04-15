// 4/15/2017.

// Speed testing of the kernel launch speeds.

// I need to see how much all of this marshaling affects the speed.

// Edit: The conclussion is as follows:
// All the marshaling from and to algebraic datatypes definitely makes things a lot slower,
// but as I said, kernel launches by async is saving my ass here.

// Test results:

// Standard square map with one input and output
// f x = x * x
// 256*256(65k) arrays.

//let a1 = test1 10000 //0.493s
//let a2 = test2 10000 //0.126s
//let a3 = test3 10000 //0.099s

// f x y z = x * y * z, x + y + z

//let a1 = test1 10000 //0.583s
//let a2 = test2 10000 //0.188s
//let a3 = test3 10000 //0.101s

// f x y z = x * y * z, x + y + z
// The same as the second test except with 256*256*16(1Mb) arrays.

//let a1 = test1 10000 //0.661s
//let a2 = test2 10000 //0.201s
//let a3 = test3 10000 //0.172s

// Personally I think that this third test is the most representative of real life scenarios.
// The compiled mode will be a lot faster than the interpreted mode and once I put in a bunch of
// matrix multiplies before the map operations, the launch speeds will stop mattering.

// Sequential launches of these map operations are kind of the worst case scenario for this sort of thing.

// No doubt about it, these inefficiencies are painful for me and will be a major motivation in me making
// my own language eventually, but they are bearable for now.

// I need some dependent typing or metaprogramming to get through this hump.

// Originally I wanted to write the Cuda compiler using just functions. That was the third failed attempt
// to do this and it is sitting in the the Failed Attempts folder somewhere. If I had impredicative polymorphism
// and some dependent typing I could have done it.

// I wish somebody would post the `Type Driven Development with Idris` on library Genesis already so that I 
// may read it and figure out whether Idris is digestible for standard programming tasks. Last time I tried it
// I got way too hung up on theorem proving, which really has nothing to do with programming.

#load "SpiralV5Ops.fsx"
open System
open System.Diagnostics
open System.Collections.Generic

open SpiralV5CudaTypechecker_v6e
open SpiralV5CudaCodegen_v3a
open SpiralV5CudaInit
open SpiralV5DevVar
open SpiralV5Ops

open ManagedCuda
open ManagedCuda.BasicTypes
open ManagedCuda.VectorTypes
open ManagedCuda.CudaBlas

let reserve_tags n =
    let tags = ResizeArray()
    for i=1 to n do
        tags.Add(get_tag())
    tags

let reserved_tags = reserve_tags 100

let map =
    let n = TyV (reserved_tags.[0], PrimT UInt64T)
    fun (str: CudaStream) map_op (inputs: DM list) (outputs: DM list) ->
        let args = inputs @ outputs
        let total_size = total_size args.Head
        let block_size = 256UL
        let grid_size = min (2UL*numSm*(1024UL/block_size)) (divup total_size block_size)

        let dims = dim3(int block_size), dim3(int grid_size)

        match args with
        | x :: xs -> List.fold (fun x y -> guard_sizes x (size y)) (size x) xs |> ignore
        | [] -> ()

        let to_typechecking_form =
            let mutable i = 0
            let inc() = i <- i+1; i
            let conv (x : DM) = V' (reserved_tags.[inc()],GlobalArrayT([n],PrimT x.Type))
            fun inputs ->
                match inputs with
                | [x] -> conv x
                | x :: xs -> VV (List.map conv inputs)
                | [] -> B
        let ins = to_typechecking_form inputs
        let outs = to_typechecking_form outputs

        let map_op = 
            inl (VV [V "i";V "ins";V "outs"])
                (s [l (V "indexer") (inl (V "x") (ArrayIndex(V "x",[V "i"])))
                    l (V "ins") (VVMap(V "indexer",V "ins"))
                    mset (VVMap(V "indexer",V "outs")) (ap map_op (V "ins"))
                    ] B)

        let kernel = call_map (VV [map_op; CudaExpr.T n; ins; outs], dims)

        let get_ptrs x = List.map (fun (x: DM) -> box x.P.GetDevicePtr.Value) x |> List.toArray
        let args = [|[|box total_size|];get_ptrs inputs; get_ptrs outputs|] |> Array.concat
        fun () -> kernel.RunAsync(str.Stream,args)

//let f _ = dm_create [|256UL;256UL|] Float32T 2
//let in1, out1 = f(), f()
//let s = new CudaStream()
//
//#time
//let test1 num_iters =
//    for i=0 to num_iters do
//        let f = map s (inl (V "x") (V "x" * V "x")) [in1] [out1]
//        f()
//        cuda_context.Synchronize()
//
//let test2 num_iters =
//    for i=0 to num_iters do
//        let f = map s (inl (V "x") (V "x" * V "x")) [in1] [out1]
//        f()
//    cuda_context.Synchronize()
//
//let test3 num_iters =
//    let f = map s (inl (V "x") (V "x" * V "x")) [in1] [out1]
//    for i=0 to num_iters do
//        f()
//    cuda_context.Synchronize()
//
//let a1 = test1 10000 //0.493s
//let a2 = test2 10000 //0.126s
//let a3 = test3 10000 //0.099s
//#time

//let f _ = dm_create [|256UL;256UL|] Float32T 2
//let ins, outs = [f(); f(); f()], [f(); f()]
//let s = new CudaStream()
//let g ins outs = map s (inl (VV[V "x"; V "y"; V "z"]) (VV [V "x" * V "y" * V "z"; V "x" + V "y" + V "z"])) ins outs
//
//#time
//let test1 num_iters =
//    for i=0 to num_iters do
//        let f = g ins outs
//        f()
//        cuda_context.Synchronize()
//
//let test2 num_iters =
//    for i=0 to num_iters do
//        let f = g ins outs
//        f()
//    cuda_context.Synchronize()
//
//let test3 num_iters =
//    let f = g ins outs
//    for i=0 to num_iters do
//        f()
//    cuda_context.Synchronize()
//
//let a1 = test1 10000 //0.583s
//let a2 = test2 10000 //0.188
//let a3 = test3 10000 //0.101
//#time

//let f _ = dm_create [|256UL;256UL;16UL|] Float32T 2
//let ins, outs = [f(); f(); f()], [f(); f()]
//let s = new CudaStream()
//let g ins outs = map s (inl (VV[V "x"; V "y"; V "z"]) (VV [V "x" * V "y" * V "z"; V "x" + V "y" + V "z"])) ins outs
//
//#time
//let test1 num_iters =
//    for i=0 to num_iters do
//        let f = g ins outs
//        f()
//        cuda_context.Synchronize()
//
//let test2 num_iters =
//    for i=0 to num_iters do
//        let f = g ins outs
//        f()
//    cuda_context.Synchronize()
//
//let test3 num_iters =
//    let f = g ins outs
//    for i=0 to num_iters do
//        f()
//    cuda_context.Synchronize()
//
//let a1 = test1 10000 //0.661
//let a2 = test2 10000 //0.201
//let a3 = test3 10000 //0.172
//#time