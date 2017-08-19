type TypedExpr =
    | TyV of int * Ty
    | TyVV of TypedExpr list
and Ty =
    | IntT
    | FloatT
    | StringT

let tag =
    let mutable i = 0
    fun () -> i <- i+1; i
let vv i next = TyVV (List.init i (fun i -> next()))
let v t = TyV(tag(),t)

let vv' n () = vv 1000 n
let v' t () = v t
#time
let l = v' IntT |> vv' |> vv' |> fun x -> x()
#time
let rec free_vars = function
    | TyV (x,_) -> Set.singleton x
    | TyVV l -> List.map free_vars l |> Set.unionMany

let rec hash_typed_expr = function
    | TyV (x,_) -> 1
    | TyVV l -> List.fold (fun s x -> s + hash_typed_expr x) 0 l

#time
for i = 1 to 10 do hash_typed_expr l |> ignore // 
for i = 1 to 10 do l.GetHashCode() |> ignore // 
//for i = 1 to 1 do free_vars l |> ignore // 

