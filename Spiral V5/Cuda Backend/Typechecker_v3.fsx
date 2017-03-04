type Ty =
    | Unit
    | Int
    | Float
    | Bool
    | TVar of string

type TyV = int64 * string * Ty

type Expr = 
    | V of string
    | If of Expr * Expr * Expr
    | Inlineable of string list * Expr * EnvType option
    | Let of string * Expr * Expr
    | LitInt of int
    | LitFloat of float
    | LitBool of bool
    | Method of tag: int64 option * args: Expr list * body: Expr * return_type: Ty option
    | Apply of Expr * args: Expr list

// This is being compiled to STLC, not System F, so no type variables are allowed in the processed AST.
and TypedExpr =
    | TyV of TyV
    | TyIf of TypedExpr * TypedExpr * TypedExpr * Ty
    | TyLet of TyV * TypedExpr * TypedExpr * Ty
    | TyUnit
    | TyLitInt of int
    | TyLitFloat of float
    | TyLitBool of bool
    | TyMethod of tag: int64 * TyV list * TypedExpr * whole_type: Ty // Not just the return type. The TypedExpr body has the return type.
    | TyApply of TypedExpr * TypedExpr list * Ty

and ReturnCases =
    | RTypedExpr of TypedExpr
    | RExpr of Expr
    | RError of string

and EnvType = Map<string,ReturnCases>

let rec get_type = function
    | TyApply(_,_,t) | TyMethod(_,_,_,t) | TyV(_,_,t) | TyIf(_,_,_,t) | TyLet(_,_,_,t) -> t
    | TyLitInt _ -> Int
    | TyLitFloat _ -> Float
    | TyLitBool _ -> Bool
    | TyUnit -> Unit

let get_tag =
    let mutable x = 0L
    fun () -> 
        let x' = x
        x <- x + 1L
        x'

type ArgCases = Expr list * EnvType

open System.Collections.Generic
type Data =
    {
    // Immutable
    env : EnvType
    args : ArgCases list
    // Mutable
    memoized_functions : Dictionary<int64 * int64 list * Ty list * Ty, Expr * TypedExpr option> // For hoisted out global methods.
    }

type Result<'a,'b> = Succ of 'a | Fail of 'b

let d0() = {env=Map.empty;args=[];memoized_functions=Dictionary()}

let rec teval (d: Data) exp: ReturnCases =
    let hoist a b =
        let empty = get_tag(),"",Unit
        TyLet(empty, a, b, get_type b)
    match exp with
    | V x -> 
        match Map.tryFind x d.env with
        | Some (RTypedExpr _ as v) -> v
        | Some (RExpr v) -> teval d v
        | Some (RError _ as e) -> e
        | None -> RError <| sprintf "Variable %A not bound." x
    | If(cond,tr,fl) ->
        match teval d cond with
        | RTypedExpr cond' when get_type cond' = Bool -> 
            match teval d tr, teval d fl with
            | RTypedExpr tr, RTypedExpr fl -> 
                if get_type tr = get_type fl then
                    RTypedExpr <| TyIf(cond',tr,fl,get_type tr)
                else
                    RError "Types in branches of if do not match."
            | a, b -> RError <| sprintf "Expected both sides to be types and to be equal types.\nGot true: %A\nGot false: %A" a b
        | x -> RError <| sprintf "Expected bool in conditional.\nGot: %A" x

//    | Let(v,b,e) ->
//        match teval {d with args=[]} b with
//        | RDual(exp_to_be_hoisted,macro) ->
//            match teval {d with env=Map.add v (RExpr macro) d.env} e with
//            | RDual(exp_to_be_hoisted',macro') ->
//                RDual (hoist exp_to_be_hoisted exp_to_be_hoisted', macro')
//            | RTypedExpr exp_to_be_hoisted' ->
//                RTypedExpr (hoist exp_to_be_hoisted exp_to_be_hoisted')
//            | RExpr macro' ->
//                RDual(exp_to_be_hoisted,macro')
//            | RError _ as er -> er
//        | RTypedExpr b' ->
//            let b'_type = get_type b'
//            let v' = get_tag(),v,b'_type
//            match teval {d with env=Map.add v (RTypedExpr <| TyV v') d.env} e with
//            | RTypedExpr e' -> RTypedExpr(TyLet(v',b',e',get_type e'))
//            // If there aren't enough arguments to apply to the final expression, just save it for later evaluation.
//            | RExpr macro -> RDual(TyLet(v',b',TyUnit,Unit),macro)
//            | RDual (e',macro) -> RDual(hoist b' e',macro)
//            | RError er -> RError er
//        | RExpr _ as b -> teval {d with env=Map.add v b d.env} e
//        | RError _ as e -> e
//
//    | Inlineable(args,body) as orig -> 
//        match d.args with
//        | (cur_args,env'') :: other_args ->
//            let rec loop acc = function
//                | (arg :: ars, cur_arg :: crs) ->
//                    let cur_arg = teval {d with env=env''; args=[]} cur_arg
//                    match cur_arg with
//                    | RTypedExpr _ | RExpr _ -> loop (Map.add arg cur_arg acc) (ars,crs)
//                    | RDual _ -> RError "Duals cannot pass through Inlineable calls."
//                    | RError _ -> cur_arg
//                | [], [] -> teval {d with env=acc; args=other_args} body
//                | _ -> RError "The number of arguments in Inlineable does not match the number of arguments being applied."
//            loop env'' (args, cur_args)
//        | [] -> RExpr orig
