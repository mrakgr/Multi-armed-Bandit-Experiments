// Translated from the SPJ 1987 book on implementing functional languages.

type Pattern =
| Var of Variable
| Con of Constructor * Pattern list

and Variable = string
and Constructor = string

let arity = function
    | "NIL" -> 0
    | "CONS" -> 2

let constructors = function
    | "NIL" | "CONS" -> ["NIL"; "CONS"]
    | x -> failwithf "Constructor %s not found." x

type Expression =
| Case of Variable * Clause list
| Fatbar of Expression * Expression
| Foo
| Error

and Clause = Clause of Constructor * Variable list * Expression

let rec subst exp to_ from =
    let subst exp = subst exp to_ from
    let f = function
        | v when v = from -> to_
        | v -> v
    match exp with
    | Fatbar(a,b) -> Fatbar(subst a, subst b)
    | Case(var,cl_list) -> 
        let g (Clause (con,vl,exp)) = Clause (con, List.map f vl, subst exp)
        Case(f var, List.map g cl_list)
    | x -> x

type Equation = Pattern list * Expression

let is_var = function
    | Var v :: ps, e -> true
    | _ -> false

let is_con = function
    | Con (c, ps') :: ps, e -> true
    | _ -> false

let get_con = function
    | Con (c, ps') :: ps, e -> c
    | _ -> failwith "Not a constructor."

let make_var (k: int) = "_u" + string k

let tack x xss = (x :: List.head xss) :: List.tail xss

let rec partition f = function
    | [] -> []
    | [x] -> [[x]]
    | (x::x'::xs) ->
        if f x = f x' then tack x (partition f (x'::xs))
        else [x] :: partition f (x'::xs)

//let odd x = x % 2 = 1
//let t1 = partition odd [1;3;2;4;1]

let rec match_ (k: int) (us: Variable list) (qs: Equation list) (def: Expression) = 
    match us with
    | [] ->
        let f a b = Fatbar(a,b)
        let g = List.choose (function [], e -> Some e | _ -> None) qs
        List.foldBack f g def
    | u :: us ->
        List.foldBack (match_var_con k (u :: us)) (partition is_var qs) def

and match_var_con k us qs def =
    match List.head qs with
    | x when is_var x -> match_var k us qs def
    | x when is_con x -> match_con k us qs def

and match_var k (u :: us) qs def =
    let qs = 
        List.choose (function
            | Var v :: ps, e -> Some (ps, subst e u v)
            | _ -> None
            ) qs
    match_ k us qs def
    
and match_con k (u :: us) qs def =
    let cs = constructors (get_con (List.head qs))
    Case(u,List.map (fun c -> match_clause c k (u :: us) (choose c qs) def) cs)

and match_clause c k (u :: us) qs def =
    let k' = arity c
    let us' = List.init k' (fun i -> make_var (1+i+k))
    let ps = 
        List.choose (function
            | Con(c, ps') :: ps, e -> Some (ps' @ ps, e)
            | _ -> None) qs
    Clause(c,us',match_ (k'+k) (us' @ us) ps def)

and choose c qs = List.filter (fun q -> get_con q = c) qs

let t1 = 
    match_ 3
        ["_u1";"_u2";"_u3"]
        [
        [Var "f"; Con ("NIL", []); Var "ys"], Foo
        [Var "f"; Con ("CONS", [Var "x"; Var "xs"]); Con("NIL",[])], Foo
        [Var "f"; Con ("CONS", [Var "x"; Var "xs"]); Con ("CONS", [Var "y"; Var "ys"])], Foo
        ]
        Error