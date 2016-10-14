/// http://apfelmus.nfshost.com/blog/2010/06/01-gadts-video.html

// Disappoingly, F#'s type system seems to be too weak to do anything interesting with
// DUs unlike with functions where it is great.

type Expr =
    | I of int
    | B of bool
    | Add of Expr * Expr
    | Eq of Expr * Expr

let get_i =
    function
    | I x -> x
    | _ -> failwith "Not int"

let get_b =
    function
    | B x -> x
    | _ -> failwith "Not bool"

let add x y = Add(x,y)
let eq x y = Eq(x,y)

let rec eval: Expr -> Expr = 
    function
    | Add(e1,e2) ->
        let e1 = eval e1 |> get_i
        let e2 = eval e2 |> get_i
        I (e1 + e2)
    | Eq(e1,e2) -> 
        let e1 = eval e1 |> get_i
        let e2 = eval e2 |> get_i
        B (e1 = e2)
    | x -> x

let expr1 = (I(5) |> add (I(1))) |> eq (I(6))

let x = eval expr1