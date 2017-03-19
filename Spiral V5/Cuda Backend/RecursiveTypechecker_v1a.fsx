open System.Collections.Generic

type Expr = 
    | V of string
    | If of Expr * Expr * Expr // can't do recursion without branching.
    | Apply of string * Expr list
    | Method of name: string * args: string list * body: Expr * else_: Expr
    | Let of string * Expr * Expr

    | LitUnit
    | LitInt of int
    | LitFloat of float
    | LitBool of bool
type Ty =
    | UnitT
    | Int32T
    | Float32T
    | BoolT
and TyV = string * Ty
and TypedExpr =
    | TyV of TyV
    | TyIf of TypedExpr * TypedExpr * TypedExpr * Ty
    | TyLet of TyV * TypedExpr * TypedExpr * Ty
    | TyMethodCall of name: string * TypedExpr list * Ty

    | TyLitUnit
    | TyLitInt of int
    | TyLitFloat of float
    | TyLitBool of bool

let get_type = function
    | TyV(_,t) -> t
    | TyIf(_,_,_,t) -> t
    | TyLet(_,_,_,t) -> t
    | TyMethodCall(_,_,t) -> t

    | TyLitUnit -> UnitT
    | TyLitInt _ -> Int32T
    | TyLitFloat _ -> Float32T
    | TyLitBool _ -> BoolT

// I am not going to go to any lengths to prevent name clashes for methods. 
// This particular version of the typechecker is just an experiment.
type Data = 
    {
    v_dict: Map<string,Ty>
    method_dict: Map<string,string list * Expr>
    method_dict': Dictionary<string * Ty list,Stack<unit -> TypedExpr>>
    current_stack: Stack<unit -> TypedExpr>
    }

let d0() = {v_dict=Map.empty;method_dict=Map.empty;method_dict'=Dictionary();current_stack=Stack()}

let rec tev (d: Data) exp = 
    match exp with
    | V x -> TyV(x,d.v_dict.[x])
    | If (cond,tr,fl) -> // can't do recursion without branching.
        let cond = tev d cond

        let mutable fl_result = None

        d.current_stack.Push <| fun _ -> 
            d.current_stack.Push <| fun _ -> failwith "Method is divergent."
            let x = tev d fl
            d.current_stack.Pop() |> ignore
            fl_result <- Some x
            x
        let tr = tev d tr
        d.current_stack.Pop() |> ignore

        match fl_result with
        | Some fl -> 
            let tr_type = get_type tr
            let fl_type = get_type fl
            if get_type cond <> BoolT || tr_type <> fl_type then
                failwith "get_type cond <> BoolT || tr_type <> fl_type"
            else
                TyIf(cond,tr,fl,tr_type)
        | None ->
            d.current_stack.Push <| fun _ -> tr
            let fl = tev d fl
            d.current_stack.Pop() |> ignore

            let tr_type = get_type tr
            let fl_type = get_type fl
            if get_type cond <> BoolT || tr_type <> fl_type then
                failwith "get_type cond <> BoolT || tr_type <> fl_type"
            else
                TyIf(cond,tr,fl,tr_type)

    | Apply (x,args) -> 
        let args = List.map (tev d) args
        let args' = List.map get_type args

        let n = x, args'

        let arg_names,method_ = d.method_dict.[x]
        if List.length args <> List.length arg_names then
            failwith "Arg sizes do not match."
        else
            
            match d.method_dict'.TryGetValue n with
            | false, _ ->
                let s = Stack()
                s.Push <| fun _ -> failwith "The method is divergent."
                d.method_dict'.Add(n,s)
                let method_typed_body = 
                    let v_dict = List.fold2 (fun m x y -> Map.add x y m) d.v_dict arg_names args'
                    tev {d with current_stack=s;v_dict=v_dict} method_
                let rec method_typed_body_as_closure _ = 
                    // It just returns method_typed_body, but makes sure to push itself back on the stack.
                    s.Push method_typed_body_as_closure
                    method_typed_body
                s.Push method_typed_body_as_closure
                TyMethodCall(x,args, get_type method_typed_body)
            | true, s ->
                TyMethodCall(x, args, get_type (s.Peek()()))
    | Method(n,args,body,rest) -> 
        tev {d with method_dict=d.method_dict.Add(n,(args,body))} rest
    | Let(v,b,e) ->
        let b = tev d b
        let v' = (v,get_type b)
        let e = tev {d with v_dict=d.v_dict.Add v'} e
        TyLet(v',b,e,get_type e)

    | LitUnit -> TyLitUnit
    | LitInt x -> TyLitInt x
    | LitFloat x -> TyLitFloat x
    | LitBool x -> TyLitBool x

let term1 = 
    let rec_call = Apply("meth",[LitBool true])
    Method("meth",["cond"],If(V "cond",rec_call,rec_call),rec_call)
let t1 = tev (d0()) term1