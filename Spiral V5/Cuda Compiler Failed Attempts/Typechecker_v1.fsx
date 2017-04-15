// 2/1/2017: To be continued...

// http://cs.brown.edu/courses/cs173/2012/book/types.html

type Number = int
type Symbol = string

type ExprC =
| NumC of Number
| IdC of Symbol
| AppC of f: ExprC * arg: ExprC
| PlusC of ExprC * ExprC
| MultC of ExprC * ExprC
| LamC of arg: Symbol * body: ExprC
//| RecC of name: Symbol * arg: Symbol * body: ExprC * use_: ExprC

type Env = Map<Symbol, Value>

and Value =
| NumV of Number
| ClosV of Symbol * ExprC * Env

type Constraints =
| EqCon of lhs: Term * rhs: Term

and Term =
| TExp of ExprC
| TVar of Symbol
| TNum
| TArrow of dom: Term * rng: Term

let rec cg e =
    match e with
    | NumC x -> [EqCon (TExp e, TNum)]
    | IdC x -> [EqCon (TExp e, TVar x)]
    | PlusC(l,r) | MultC(l,r) ->
        [cg l; cg r; [EqCon (TExp l, TNum);EqCon (TExp r, TNum);EqCon (TExp e, TNum)]] |> List.concat
    | LamC(a,b) -> [cg b;[EqCon (TExp e,TArrow(TVar a,TExp b))]] |> List.concat
    | AppC(f,a) ->
        [cg f; cg a; [EqCon (TExp f,TArrow(TExp a,TExp e))]] |> List.concat

type Substitution = (Term * Term) list

let lookup l sub = List.tryFind (fun x -> fst x = l) sub
let rec extend_replace var is sub =
    match sub with
    | [] -> (var, is) :: sub
    | (var', is') :: xs -> 
        if var = var' then failwith "Variable occurs twice in the mapping set."
        let rec replace cur = 
            match cur with
            | TExp _ | TVar _ | TNum -> if var <> cur then cur else is
            | TArrow (d,r) ->
                if var <> cur then TArrow(replace d, replace r)
                else is
        (var', replace is') :: extend_replace var is xs

let unify c =
    let rec unify c sub =
        printfn "Entering unify."
        printfn "c=%A\nsub=%A" c sub
        match c with
        | [] -> sub
        | EqCon(l,r) :: cs ->
            match l,r with
            | TVar _, _ | TExp _, _ ->
                match lookup l sub with
                | Some bound -> unify (EqCon(snd bound,r) :: cs) sub
                | None -> unify cs (extend_replace l r sub)
            | TNum, TNum -> unify cs sub
            | TArrow(d,r), TArrow(d',r') ->
                unify (EqCon(d,d') :: EqCon(r,r') :: cs) sub
            | _ -> failwithf "unify %A and %A" l r
    unify c []

let id = LamC("x",IdC "x")
let appId = AppC(id,NumC 5)

let q' = cg appId |> unify