﻿#load "SpiralV5CudaModules.fsx"
open SpiralV5
open SpiralV5DM
open SpiralV5CudaModules

let state (program: ResizeArray<_>) x = 
    Statement x |> program.Add
let states (program: ResizeArray<_>) statements = 
    List.iter (Statement >> program.Add) statements
let enter (program: ResizeArray<_>) body =
    Indent |> program.Add
    body program
    Dedent |> program.Add

let cudavar_var typ suffix =
    let name = [|varn "arg_"; suffix|] |> String.concat ""
    let arg = [|typ;" ";name|] |> String.concat ""
    arg, name

let cudavar_ar1d typ size suffix =
    let name = [|varn "arg_"; suffix|] |> String.concat ""
    let arg = [|typ;" ";name|] |> String.concat ""
    let var i = [|name;"[";i;"]"|] |> String.concat ""
    arg, var

let cudavar_ar2d typ (size_col,size_row as size) suffix =
    let name = [|varn "arg_"; suffix|] |> String.concat ""
    let arg = [|typ;" ";name|] |> String.concat ""
    let var (c,r) = [|name;"[";c;" * ";size_row;" + ";r;"]"|] |> String.concat ""
    arg, var

// Unlike in the previous version, this file generates the interface to the map modules using Text.
type CudaTypes =
    | CudaInt
    | CudaFloat

let meta_mapcoef_arg name f =
    List.mapi (fun i x -> 
        let create_var_fun, typ_, bound_size, sig_fun = f x
        let sig_name = sprintf "%s_sig%i" name i
        let sig_type, sig_expr = sig_fun sig_name
        let v = sprintf "%s %s %s" create_var_fun (quote typ_) bound_size
        let [arg;var] as x = 
            [sprintf "%s_arg%i" name i; sprintf "%s_var%i" name i]
        let names = String.concat ", " x
        sprintf """let %s = %s "" """ names v, arg, var, (sig_type, sig_expr))


module MapForwardArgs =
    let sig_dm_int x = sprintf "(%s: DM<int,int>)" x, sprintf "box %s.P.DevicePointer" x
    let sig_dm_float x = sprintf "(%s: DM<int,float32>)" x, sprintf "box %s.P.DevicePointer" x

    let sig_scalar_dm_int x = sprintf "(%s: DM<Scalar,int>)" x, sprintf "box %s.P.DevicePointer" x
    let sig_scalar_dm_float x = sprintf "(%s: DM<Scalar,float32>)" x, sprintf "box %s.P.DevicePointer" x

    let meta_map_ins_arg =
        function
        | CudaInt -> "cudavar_ar1d", "const int *", "size_var", sig_dm_int
        | CudaFloat -> "cudavar_ar1d", "const float *", "size_var", sig_dm_float
        |> meta_mapcoef_arg "ins"

    let meta_mapcoef_outs_arg =
        function
        | CudaInt -> "cudavar_ar1d", "int *", "size_var", sig_dm_int
        | CudaFloat -> "cudavar_ar1d", "float *", "size_var", sig_dm_float
        |> meta_mapcoef_arg "outs"

    let meta_map_redo_map_outs arg =
        function
        | CudaInt -> "cudavar_ar1d", "int *", "1", sig_scalar_dm_int
        | CudaFloat -> "cudavar_ar1d", "float *", "1", sig_scalar_dm_float
        |> meta_mapcoef_arg "outs"
        
let meta_consts_arg =
    function
    | CudaInt -> "cudavar_var", "const int", "", (fun x -> sprintf "(%s: int)" x, sprintf "box %s" x)
    | CudaFloat -> "cudavar_var", "const float", "", (fun x -> sprintf "(%s: float32)" x, sprintf "box %s" x)
    |> meta_mapcoef_arg "consts"

let meta_forward_body map_ins map_consts map_outs ins consts outs (program: ResizeArray<_>) =
    let state = state program
    let states = states program

    """let size_arg, size_var = cudavar_var "const int" "" """ |> state
    let print_and_get_arg_and_var map x = 
        let process_vars x = 
            x |> String.concat ", " |> fun x -> if x.Length > 0 then Some <| sprintf "(%s)" x else None
        List.map (fun (s,arg,var,sig_) -> state s; arg, var, sig_) (map x)
        |> List.unzip3 |> fun (arg,var,sig_) -> arg, process_vars var, sig_

    let ins_arg, ins_var, ins_sig = print_and_get_arg_and_var map_ins ins
    let consts_arg, consts_var, consts_sig = print_and_get_arg_and_var map_consts consts
    let outs_arg, outs_var, outs_sig = print_and_get_arg_and_var map_outs outs
    sprintf "let args = [size_arg; %s] |> String.concat \", \"" ([ins_arg;consts_arg;outs_arg] |> List.concat |> String.concat "; ") |> state

    let process_sigs (size_type, size_expr) ins consts outs =
        let ins_type = List.map fst ins |> String.concat ", "
        let consts_type = List.map fst consts |> String.concat ", "
        let outs_type = List.map fst outs |> String.concat ", "
        let lambda_args = sprintf "%s (%s) (%s) (%s)" size_type ins_type consts_type outs_type
        let exprs = List.concat [ins;consts;outs] |> List.map snd 
        let lambda_exprs = sprintf "[|%s|]" (size_expr :: exprs  |> String.concat "; ")
        sprintf "%s = %s" lambda_args lambda_exprs

    sprintf "let sigs %s" (process_sigs ("(size: int)", "box size") ins_sig consts_sig outs_sig) |> state
    sprintf "kernel args size_var %s sigs" (List.choose id [ins_var; consts_var; outs_var] |> String.concat " ") |> state

let meta_outer interface_name ins consts outs f =
    let program = ResizeArray()
    let make_name (prefix,l) =
        List.map (function
        | CudaFloat -> prefix + "f"
        | CudaInt -> prefix + "i") l
    let name =
        List.collect make_name ["i",ins;"c",consts;"o",outs]
        |> String.concat "_"
        |> (+) interface_name
        
    sprintf "let %s kernel = " name |> state program
    enter program (f ins consts outs)
    program

let mapcoef_forward_ii_ii_cf_of kernel = 
    let size_arg, size_var = cudavar_var "const int" "" 
    let ins_arg0, ins_var0 = cudavar_ar1d "const int *" size_var "" 
    let ins_arg1, ins_var1 = cudavar_ar1d "const int *" size_var "" 
    let consts_arg0, consts_var0 = cudavar_var "const float"  "" 
    let outs_arg0, outs_var0 = cudavar_ar1d "float *" size_var "" 
    let args = [size_arg; ins_arg0; ins_arg1; consts_arg0; outs_arg0] |> String.concat ", "
    let sigs (size: int) ((ins_sig0: DM<int,int>), (ins_sig1: DM<int,int>)) ((consts_sig0: float32)) ((outs_sig0: DM<int,float32>)) = [|box size; box ins_sig0.P.DevicePointer; box ins_sig1.P.DevicePointer; box consts_sig0; box outs_sig0.P.DevicePointer|]
    kernel (ins_var0, ins_var1) (consts_var0) (outs_var0) sigs

meta_forward_body MapForwardArgs.meta_map_ins_arg meta_consts_arg MapForwardArgs.meta_mapcoef_outs_arg
|> meta_outer "mapcoef_forward_" [CudaInt;CudaInt] [CudaFloat] [CudaFloat]
|> process_statements
|> printfn "%s"