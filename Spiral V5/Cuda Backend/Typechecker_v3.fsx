type Ty =
    | Unit
    | Int
    | Float
    | Bool
    | TVar of string

type TyV = int64 * string * Ty
// No return type polymorphism like in Haskell for now. Local type inference only.
type TyMethod = int64 * int64 list * Ty list 

type Expr = 
    | V of string
    | If of Expr * Expr * Expr
    | Inlineable of string list * Expr * EnvType option
    | LitUnit
    | LitInt of int
    | LitFloat of float
    | LitBool of bool
    | Apply of Expr * args: Expr list
    | Method of (int64 * EnvType) option * args: string list * body: Expr * return_type: Ty option

// This is being compiled to STLC, not System F, so no type variables are allowed in the processed AST.
and TypedExpr =
    | TyV of TyV
    | TyIf of TypedExpr * TypedExpr * TypedExpr * Ty
    | TyLet of TyV * TypedExpr * TypedExpr * Ty
    | TyUnit
    | TyLitInt of int
    | TyLitFloat of float
    | TyLitBool of bool
    | TyMethodCall of TyMethod * TypedExpr list * Ty

and ReturnCases =
    | RTypedExpr of TypedExpr
    | RExpr of Expr
    | RError of string

and EnvType = Map<string,ReturnCases>

let rec get_type = function
    | TyV(_,_,t) | TyIf(_,_,_,t) | TyLet(_,_,_,t) -> t
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
    memoized_methods : Dictionary<TyMethod, TypedExpr> // For hoisted out global methods.
    sequences : Stack<TyV * TypedExpr>
    }

type Result<'a,'b> = Succ of 'a | Fail of 'b

let d0() = {env=Map.empty;args=[];sequences=Stack();memoized_methods=Dictionary()}

let inl x y = Inlineable(x,y,None)
let ap x y = Apply(x,y)

let term0 =
    let snd = inl ["a";"b"] (V "b")
    ap (inl ["x";"y";"z";"r"] (ap (V "r") [V "y";V "z"])) ([LitUnit;LitBool true;LitInt 5;snd])

let l v b e = Apply(Inlineable(v,e,None),b)

let term1 =
    let snd = inl ["a";"b"] (V "b")
    l ["x";"y";"z"] [LitUnit;LitBool true;LitInt 5] 
        (l ["q"] [snd] (ap (V "q") [V "y";V "z"]))

let term2 =
    let fst = inl ["a";"b"] (V "a")
    let snd = inl ["a";"b"] (V "b")
    l ["a";"b"] [LitInt 2;LitFloat 3.3] (ap (If(LitBool true,fst,snd)) [V "a";V "b"])

let sequences_to_typed_expr (sequences: Stack<_>) final_expr =
    let type_fin = get_type final_expr
    Seq.fold (fun exp (v,body) -> TyLet(v,body,exp,type_fin)) final_expr sequences

let rec teval (d: Data) exp: ReturnCases =
    match exp with
    | V x -> 
        match Map.tryFind x d.env with
        | Some (RTypedExpr _ as v) -> v
        | Some (RExpr v) -> teval d v
        | Some (RError _ as e) -> e
        | None -> RError <| sprintf "Variable %A not bound." x
    | Apply(expr,args) ->
        teval {d with args = (args,d.env) :: d.args} expr
    | If(cond,tr,fl) ->
        match teval d cond with
        | RTypedExpr cond' when get_type cond' = Bool -> 
            match teval d tr, teval d fl with
            | RTypedExpr tr, RTypedExpr fl -> 
                let type_tr, type_fl = get_type tr, get_type fl
                if type_tr = type_fl then
                    RTypedExpr <| TyIf(cond',tr,fl,type_tr)
                else
                    RError "Types in branches of if do not match."
            | a, b -> RError <| sprintf "Expected both sides to be types and to be equal types.\nGot true: %A\nGot false: %A" a b
        | x -> RError <| sprintf "Expected bool in conditional.\nGot: %A" x
    | Inlineable(args, body, None) as orig -> 
        teval d (Inlineable(args, body, Some d.env))
    | Inlineable(args, body, Some env) as orig -> 
        match d.args with
        | (cur_args,env'') :: other_args ->
            let rec loop acc = function
                | arg_name :: ars, arg_expr :: crs ->
                    match teval {d with env=env''; args=[]} arg_expr with
                    | RTypedExpr ty_exp ->
                        let b'_type = get_type ty_exp
                        let ty_arg: TyV = get_tag(),arg_name,b'_type
                        // Pushes the sequence onto the stack
                        d.sequences.Push(ty_arg,ty_exp)
                        // Binds the name to the said sequence's name and loops to the next argument
                        loop (Map.add arg_name (RTypedExpr(TyV ty_arg)) acc) (ars,crs)
                    | RExpr _ as exp ->
                        loop (Map.add arg_name exp acc) (ars,crs)
                    | RError er -> Fail er
                | [], [] -> Succ acc
                | _ -> Fail "Incorrect number of arguments in Inlineable application."
            match loop env (args,cur_args) with
            | Succ env -> teval {d with env=env} body
            | Fail er -> RError er
        | [] -> RExpr orig
    | Method(None, args, body, return_type) ->
        teval d (Method(Some(get_tag(),d.env), args, body, return_type))
    | Method(Some(tag,env), arg_names, body, return_type) as orig ->
        match d.args with
        | [] -> RExpr orig
        | (cur_args,env'') :: other_args ->
            let rec loop method_tags typed_exprs acc = function
                | arg_name :: ars, arg_expr :: crs ->
                    match teval {d with env=env''; args=[]} arg_expr with
                    | RTypedExpr ty_exp ->
                        let b'_type = get_type ty_exp
                        let ty_arg: TyV = get_tag(),arg_name,b'_type
                        loop method_tags (ty_exp :: typed_exprs) (Map.add arg_name (RTypedExpr(TyV ty_arg)) acc) (ars,crs)
                    | RExpr(Method(Some(tag',_),_,_,_) as met) as exp ->
                        loop (tag' :: method_tags) typed_exprs (Map.add arg_name (RExpr met) acc) (ars,crs)
                    | RExpr _ -> Fail "In Method application the only Expr type allowed to be passed via an argument is another Method."
                    | RError er -> Fail er
                | [], [] -> Succ(List.rev method_tags,List.rev typed_exprs,acc)
                | _ -> Fail "Incorrect number of arguments in Method application."
            match loop [] [] env (arg_names,cur_args) with
            | Succ(method_tags,typed_exprs,env) ->
                let arg_types = List.map get_type typed_exprs
                let method_: TyMethod = tag,method_tags,arg_types
                let eval_body body =
                    d.memoized_methods.Add(method_, body)
                    let x = RTypedExpr(TyMethodCall(method_,typed_exprs,get_type body))
                    match return_type with
                    | None -> x
                    | Some return_type when return_type = get_type body -> x
                    | Some _ -> RError "The evaluated return type does not match the one given in Method evaluation."
                match d.memoized_methods.TryGetValue method_ with
                | false, _ ->
                    let sequences' = Stack()
                    match teval {d with env=env;sequences=sequences'} body with
                    | RError _ as er -> er
                    | RExpr x -> RError "Only TypedExprs are allowed as returns from a Method's body evaluation."
                    | RTypedExpr body -> 
                        // All the intermediate expressions get sequenced in the Inlineable case. 
                        // The body here is just the final return hence the call to sequences_to_typed_expr.
                        eval_body (sequences_to_typed_expr sequences' body) 
                | true, body -> eval_body body
            | Fail er -> RError er
