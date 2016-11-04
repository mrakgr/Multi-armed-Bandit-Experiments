[<AutoOpen>]
module SpiralV4.Flame.CudaCodegen

open System
open System.Text
open System.Collections.Generic
open Microsoft.FSharp.Quotations
#if INTERACTIVE
#load "QuotationsParser.fsx"
#endif
open QuotationsParser

let rec print_type =
    function
    | TyInt -> "int"
    | TyBool -> "bool"
    | TyFloat32 -> "float"
    | TyFunc(_,ret) -> print_type ret
    | TyGlobalArray x -> 
        sprintf "global_array_%s" (print_type x)
    | TyGlobal2dArray x -> 
        sprintf "global_array_2d_%s" (print_type x)
    | TySharedArray(_,x) | TyShared2dArray(_,_,x) ->
        sprintf "__shared__ %s" (print_type x)
    | TyLocalArray(_,x) | TyLocal2dArray(_,_,x) ->         
        print_type x
    | TyTuple(x) -> sprintf "tuple_%s__" (Array.map print_type x |> String.concat "_")
    | TyUnit -> "void"

let codegen (exp: ParsedExpr, ctx: Context) =
    let program = StringBuilder()
    let pp (x: string) = 
        program.Append x |> ignore
    let ppln (x: string) = 
        program.AppendLine x |> ignore
    let rec generate_definitions indentation (ctx: Context) =
        let ind() =
            program.Append (String.replicate indentation " ") |> ignore
        let ind'() =
            program.Append (String.replicate (indentation+4) " ") |> ignore
        let generate_definition (x: Ty) =
            match x with
            | TyTuple x' ->
                // Declares the type.
                ind(); ppln "typedef struct {"
                Array.iteri <| fun i typ ->
                    ind'(); ppln (sprintf "%s arg%i;" (print_type typ) i)
                <| x'
                ind(); pp "} "; pp (print_type x); ppln ";"
                // The make tuple functions.
                ind(); pp "__device__ "; pp (print_type x)
                pp " make_tuple_"
                pp (Array.map print_type x' |> String.concat "_")
                pp "("; pp (print_type x'.[0]); pp " arg0"
                for i=1 to x'.Length-1 do
                    pp ", "; pp (print_type x'.[i]); pp (sprintf " arg%i" i)
                ppln "){"
                ind'(); pp (print_type x); ppln " r;"
                for i=0 to x'.Length-1 do
                    ind'(); ppln (sprintf "r.arg%i = arg%i;" i i)
                ind'(); ppln "return r;"
                ind(); ppln "}"
            | TyGlobalArray x' ->
                ind(); ppln "#pragma pack(1)"
                ind(); ppln "typedef struct {"
                ind'(); ppln "int length;"
                ind'(); ppln (sprintf "%s *pointer;" (print_type x'))
                ind(); pp "} "; pp (print_type x); ppln ";"
            | TyGlobal2dArray x' ->
                ind(); ppln "#pragma pack(1)"
                ind(); ppln "typedef struct {"
                ind'(); ppln "int num_cols; int num_rows;"
                ind'(); ppln (sprintf "%s *pointer;" (print_type x'))
                ind(); pp "} "; pp (print_type x); ppln ";"
            | x -> failwithf "Not supposed to be called on this(%A)." x
        Seq.iter generate_definition ctx.definitions

    let rec generate_code indentation (exp: ParsedExpr) =
        let inline g x = generate_code indentation x
        let inline gg x = generate_code (indentation+4) x
        let indent() =
            program.Append (String.replicate indentation " ") |> ignore
        match exp with
        | PReturn x ->
            indent()
            pp "return "; g x; ppln ";"
            indent()
        | PVar(var,_) -> pp var
        | PTupleGet(a,n) ->
            g a
            pp (sprintf ".arg%d" n)
        | PCall(x) ->
            match x with
            | PFAdd(a,b) -> pp "("; g a; pp " + "; g b; pp ")"
            | PFMult(a,b) -> pp "("; g a; pp " * "; g b; pp ")"
            | PFDiv(a,b) -> pp "("; g a; pp " / "; g b; pp ")"
            | PFMod(a,b) -> pp "("; g a; pp " % "; g b; pp ")"
            | PFLessThan(a,b) -> pp "("; g a; pp " < "; g b; pp ")"
            | PFLessThanOrEqual(a,b) -> pp "("; g a; pp " <= "; g b; pp ")"
            | PFEquality(a,b) -> pp "("; g a; pp " == "; g b; pp ")"
            | PFGreaterThan(a,b) -> pp "("; g a; pp " > "; g b; pp ")"
            | PFGreaterThanOrEqual(a,b) -> pp "("; g a; pp " >= "; g b; pp ")"
            | PFUnroll -> pp "#pragma unroll"
            | PFSyncthreads -> pp "__syncthreads();"
            | PFShuffleXor(a,b) -> pp "__shfl_xor("; g a; pp ", "; g b; pp ")"
            | PFShuffleUp(a,b) -> pp "__shfl_up("; g a; pp ", "; g b; pp ")"
            | PFShuffleDown(a,b) -> pp "__shfl_down("; g a; pp ", "; g b; pp ")"
            | PFShuffleSource(a,b) -> pp "__shfl("; g a; pp ", "; g b; pp ")"
        | PFunction(_,_) -> failwith "Should be a part of the let statement."
        | PLet((name,TyFunc(call_types, ret_typ)),PFunction((arg_name,_),body),rest) -> // For functions.
            indent()
            if name = "kernel_main" 
            then pp "__global__ "
            else pp "__device__ "
            pp (print_type ret_typ); pp " "; pp name; pp "("; pp (print_type call_types); pp " "; pp arg_name; ppln "){"
            gg body
            indent(); ppln "}"
            g rest
        | PLet((var,typ),body,rest) -> // For standard let statements.
            indent()
            pp (print_type typ); pp " "; pp var; pp " = "; gg body; ppln ";"
            g rest          
        | PValue(x) -> pp x
        | PIfThenElse(a,b,c) ->
            pp "(("; gg a; pp ") ? ("; gg b; pp ") : ("; gg c; pp "))"
        | PWhileLoop(a,b) ->
            indent(); pp "while ("; gg a; ppln ") {"
            gg b; 
            indent(); ppln "}"
        | PVarSet(name,b) ->
            indent()
            pp name; pp " = "; g b; ppln ";"
        | PSequential(a,b) ->
            g a
            g b
        | PForIntegerRangeLoop(name,lower_bound,upper_bound,body) ->
            indent()
            pp "for("; pp name; pp " = "; g lower_bound; pp "; "; pp name; pp " <= "; g upper_bound; pp "; "; pp name; ppln "++){"
            gg body
            ppln "}"
        | PDeclareArray(var,typ,rest) ->
            indent()
            pp (print_type typ); pp " "; pp var
            match typ with 
            | TyLocalArray(n,_) | TySharedArray(n,_) -> 
                pp (sprintf "[%i]" n)
            | TyLocal2dArray(nc,nr,_) | TyShared2dArray(nc,nr,_) ->
                pp (sprintf "[%i][%i]" nc nr)
            ppln ";"
            g rest
        | PGetArray(name,i) ->
            pp name; pp ".pointer["; g i; pp "]"
        | PSetArray(name,i,ex) ->
            indent()
            pp name; pp ".pointer["; g i; pp "] = "; g ex; ppln ";"
        | PApplication(name,args) ->
            pp name; pp "("
            if args.IsEmpty = false then g (args.Head)
            List.iter (fun arg -> pp ", "; g arg) args.Tail
            pp ")"
        | PNewTuple args ->
            let typ = args |> List.map (snd >> print_type) |> String.concat "_"
            pp (sprintf "make_tuple_%s(" typ)
            g (List.head args |> fst)
            List.iter (fun (x,_) -> pp ", "; g x) args.Tail
            pp ")"
        | PPropertyGet(v,name,[]) ->
            g v; pp "."; pp name 
        | PPropertyGet(v,name,args) ->
            g v; pp "."; pp name; pp "("; 
            g args.Head
            List.iter (fun x -> pp ", "; g x) args.Tail
            pp ")"
                

    ppln "//Kernel code:"
    ppln "extern \"C\" {"
    generate_definitions 4 ctx
    generate_code 4 exp
    ppln "}"
    program.ToString()

//let test =
//    <@
//    let ops (x,y) = x % y, x / y, x * y
//    let main(ar: CudaGlobalArray<int>) =
//        let q = CudaSharedArray(32)
//        ops (q.[0], q.[1])
//    ()
//    @>
//
//codegen (parse_exprs test) |> printfn "%s"