#load "SpiralV5CudaModules.fsx"
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
type CudaType =
    | CudaInt
    | CudaFloat

let meta_arg name suffix f (i: int) (x: CudaType) =
    let suffix = if suffix = quote "" then "" else suffix
    let create_var_fun, typ_, bound_size, sig_fun = f x
    let sig_name = sprintf "%s_sig%i_%s" name i suffix
    let sig_type, sig_expr = sig_fun sig_name
    let v = sprintf "%s %s %s" create_var_fun (quote typ_) bound_size
    let [arg;var] as x = 
        [sprintf "%s_arg%i_%s" name i suffix; sprintf "%s_var%i_%s" name i suffix]
    let names = String.concat ", " x
    sprintf """let %s = %s %s """ names v suffix, arg, var, (sig_type, sig_expr)
        
module Args =
    type VarFun =
    | Var
    | Ar1D of string
    | Ar2D of string * string

    type SigFun =
    | SigAr1d
    | SigArScalar
    | SigArHostScalar

    type IsConst =
    | IsConst
    | NotConst

    type Suffix = 
    | NoSuffix
    | Primal
    | Adjoint

    let arg_template var_fun sig_fun name is_const suffix =
        let var_fun, size_var =
            match var_fun with
            | Var -> "cudavar_var", ""
            | Ar1D s -> "cudavar_ar1d", s
            | Ar2D (c,r) -> "cudavar_ar2d", sprintf "(%s,%s)" c r
        let suffix = 
            match suffix with
            | NoSuffix -> quote ""
            | Primal -> "primal"
            | Adjoint -> "adjoint"
        let is_const =
            match is_const with
            | IsConst -> "const"
            | NotConst -> ""
        function
         | CudaInt -> 
            let sig_1d_dm_int x = sprintf "(%s: DM<int,int>)" x, sprintf "box %s.P.DevicePointer" x
            let sig_scalar_dm_int x = sprintf "(%s: DM<Scalar,int>)" x, sprintf "box %s.P.DevicePointer" x
            let sig_int x = sprintf "(%s: int)" x, sprintf "box %s" x
            let sig_fun =
                match sig_fun with
                | SigAr1d -> sig_1d_dm_int
                | SigArScalar -> sig_scalar_dm_int
                | SigArHostScalar -> sig_int

            var_fun, is_const + " int *", size_var, sig_fun
         | CudaFloat -> 
            let sig_1d_dm_float x = sprintf "(%s: DM<int,float32>)" x, sprintf "box %s.P.DevicePointer" x
            let sig_scalar_dm_float x = sprintf "(%s: DM<Scalar,float32>)" x, sprintf "box %s.P.DevicePointer" x
            let sig_float x = sprintf "(%s: float32)" x, sprintf "box %s" x
            let sig_fun =
                match sig_fun with
                | SigAr1d -> sig_1d_dm_float
                | SigArScalar -> sig_scalar_dm_float
                | SigArHostScalar -> sig_float

            var_fun, is_const + " float *", size_var, sig_fun
        |> meta_arg name suffix

    let map_fun name is_const suffix = arg_template (Ar1D "size_var") SigAr1d name is_const suffix

    let meta_map_ins_forward_arg =
        [map_fun "ins" IsConst NoSuffix]
    let meta_map_ins_backward_arg =
        [map_fun "ins" IsConst Primal; map_fun "ins" NotConst Adjoint]

    let meta_mapcoef_outs_forward_arg =
        [map_fun "outs" NotConst NoSuffix]
    let meta_mapcoef_outs_backward_arg =
        [map_fun "outs" IsConst Primal; map_fun "outs" IsConst Adjoint]
    let meta_map_redo_map_outs_forward_arg =
        [arg_template (Ar1D "1") SigArScalar "outs" NotConst NoSuffix]
    let meta_map_redo_map_outs_backward_arg =
        [arg_template Var SigArHostScalar "outs" IsConst Primal
         arg_template Var SigArHostScalar "outs" IsConst Adjoint]

    let meta_consts_arg = [arg_template Var SigArHostScalar "consts" IsConst NoSuffix]

let meta_map1d_forward_body map_ins map_consts map_outs ins consts outs (program: ResizeArray<_>) =
    let map_ins, map_consts, map_outs = Args.meta_map_ins_backward_arg, Args.meta_consts_arg, Args.meta_mapcoef_outs_backward_arg
    let state = state program
    let states = states program

    """let size_arg, size_var = cudavar_var "const int" "" """ |> state
    let print_and_get_arg_and_var x = 
        let process_vars x = 
            x |> String.concat ", " |> fun x -> if x.Length > 0 then Some <| sprintf "(%s)" x else None
        List.map (List.map (fun (s,arg,var,sig_) -> state s; arg, var, sig_) >> List.unzip3) x
        |> List.unzip3 |> fun (arg,var,sig_) -> List.concat arg, process_vars (List.concat var), List.concat sig_

    let ins_arg, ins_var, ins_sig = print_and_get_arg_and_var (List.mapi (fun i x -> List.map (fun f -> f i x) map_ins) ins)
    let consts_arg, consts_var, consts_sig = print_and_get_arg_and_var (List.mapi (fun i x -> List.map (fun f -> f i x) map_consts) consts)
    let outs_arg, outs_var, outs_sig = print_and_get_arg_and_var (List.mapi (fun i x -> List.map (fun f -> f i x) map_outs) outs)
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

let mapcoef_forward_test =
    meta_map1d_forward_body Args.meta_map_ins_forward_arg Args.meta_consts_arg Args.meta_mapcoef_outs_forward_arg
    |> meta_outer "mapcoef_forward_" [CudaInt;CudaInt] [CudaFloat] [CudaFloat]
    |> process_statements

let mapcoef_backward_test =
    meta_map1d_forward_body Args.meta_map_ins_backward_arg Args.meta_consts_arg Args.meta_mapcoef_outs_backward_arg
    |> meta_outer "mapcoef_backward_" [CudaInt;CudaInt] [CudaFloat] [CudaFloat]
    |> process_statements

printfn "%s" mapcoef_backward_test
