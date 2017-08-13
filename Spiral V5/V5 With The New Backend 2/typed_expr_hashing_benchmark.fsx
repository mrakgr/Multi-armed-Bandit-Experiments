type TypedExpr =
    | TyV of int * Ty
    | TyVV of TypedExpr []
and Ty =
    | IntT
    | FloatT
    | StringT

let tag =
    let mutable i = 0
    fun () -> i <- i+1; i
let vv i next = TyVV (Array.init i (fun i -> next()))
let v t = TyV(tag(),t)

let vv' n () = vv 10 n
let v' t () = v t
let l = v' IntT |> vv' |> vv' |> vv' |> vv' |> vv' |> vv' |> vv' |> fun x -> x()

let rec free_vars = function
    | TyV (x,_) -> Set.singleton x
    | TyVV l -> Array.map free_vars l |> Set.unionMany

let rec hash_typed_expr = function
    | TyV (x,_) -> 1
    | TyVV l -> Array.fold (fun s x -> s + hash_typed_expr x) 0 l

#time
for i = 1 to 1 do hash_typed_expr l |> ignore // 0.2s
for i = 1 to 1 do l.GetHashCode() |> ignore // 0.5s
for i = 1 to 1 do free_vars l |> ignore // 4.7s

