type Ty = 
    | Int
    | Float
    | Bool
    | Tuple of Ty list

// Here the language is split into two phases, inlining and typechecking. Edit: Forget about this.
type Expr = 
    | V of string
    | Inline of Expr * Expr list 
    | If of Expr * Expr * Expr
    
    // During typechecking, I want to freeze the env at the point of definition.
    // Without it, I'd have to make a special check for unbound variables.
    | Inlineable of string list * Expr * InlResult option ref 

    | Let of string * Expr * Expr // TODO: This should be a statement rather than an expression.

    | LitInt of int
    | LitFloat of float
    | LitBool of bool
    | T of Expr list // T stands for tuple
and InlResult =
    | InExpr of Expr
and InEnvType = Map<string,InlResult>

type InlExpr = 
    | InV of string
    | InIf of Expr * Expr * Expr
    | InLet of string * Expr * Expr // TODO: This should be a statement rather than an expression.
    | InLitInt of int
    | InLitFloat of float
    | InLitBool of bool
    | InT of Expr list // T stands for tuple

type TyExpr = InlExpr * Ty
type TyVar = string * Ty

type TypedExpr =
    | TyV of TyVar
    | TyIf of TyExpr * TyExpr * TyExpr
    | TyLet of TyVar * TyExpr * TyExpr
    | TyLitInt of int
    | TyLitFloat of float
    | TyLitBool of bool
    | TyT of TyExpr list // T stands for tuple



//let rec teval (env: EnvType) exp =
//    match exp with
//    | V x -> 
//        match Map.tryFind x env with
//        | Some x -> x
//        | None -> failwith "Variable not bound."
//    | Inline(l,r) ->
//        let l,r = teval env l, List.map (teval env) r
//        match l with
//        | Expr(Inlineable(x,b,env)) -> 
//            let env = // Extract the previous environment
//                match !env with
//                | Some x -> x
//                | None -> failwith "Previous env not found for inlineable."
//            let env = // Add the arguments to it.
//                List.fold (fun env (arg,typ) ->
//                    Map.add arg typ env
//                    ) env (List.zip x r)
//            match teval env b with
//            | Expr _ as x -> failwithf "Expected: base type\nGot: %A" x
//            | Ty _ as x -> x
//        | _ -> failwithf "Expected: inlineable, type\nGot: %A and %A instead" l r
//    | If(cond,tr,fl) ->
//        match teval env cond with
//        | Ty Bool -> ()
//        | cond -> failwithf "Expected: Bool\nGot: %A" cond
//        match teval env tr, teval env fl with
//        | Ty tr, Ty fl when tr = fl -> Ty tr
//        | tr, fl -> failwithf "Expected: Ty tr = Ty fl\nGot: %A = %A" tr fl
//    | Let(x,body,in_) ->
//        let body = teval env body
//        teval (Map.add x body env) in_
//    | LitInt _ -> Ty Int
//    | LitFloat _ -> Ty Float
//    | LitBool _ -> Ty Bool
//    | T x -> 
//        let to_ty = function
//            | Expr _ as x -> failwithf "Expected: base type\nGot: %A" x
//            | Ty x -> x
//        List.map (to_ty << teval env) x |> Tuple |> Ty
//    | Inlineable(_,_,r) as x -> 
//        r := Some env
//        Expr x
//
//let inl x y = Inlineable(x,y,ref None)
//let inap x y = Inline(x,y)
//let l x b i = Let(x,b,i)
//
//// Some tests
//let term1 = inap (inl ["x";"y"] (V "x")) [LitInt 1; LitInt 2] 
//let t1 = teval Map.empty term1
//
//let term2 =
//    l "inlineable" 
//        (inl ["x";"y"] (T [V "x";V "y"]))
//        (T [(inap (V "inlineable") [LitInt 1; LitInt 2]); 
//            (inap (V "inlineable") [LitFloat 1.5; LitInt 2])])
//let t2 = teval Map.empty term2
//
//let term3 =
//    l "inlineable" 
//        (inl ["x";"y"] (T [V "x";V "y"]))
//        (l "fun" 
//            (inl ["inl";"a";"b";"c";"d"] 
//                (T [inap (V "inl") [V "a";V "b"];
//                    inap (V "inl") [V "c";V "d"]]))
//            (inap (V "fun") [V "inlineable"; LitInt 1; LitInt 2; LitFloat 1.5; LitInt 2]))
//let t3 = teval Map.empty term3
