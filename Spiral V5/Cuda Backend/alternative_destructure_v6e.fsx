// Alternative destructure using the Y Combinator
// The code is so elegant that I had to save it, but using
// deep destructuring everywhere would probably be more effective
// than messing with Y Combinators and such.

let destructure des r = 
    match r with
    | TyLitBool _ | TyLitFloat _ | TyLitBool _ | TyUnit
    | Inlineable' _ | Method' _ | TyV _ -> r
    | TyVV(l,t) -> List.map des l |> fun x -> TyVV(x,t)
    | _ -> 
        match get_type r with
        | VVT tuple_types -> 
            let indexed_tuple_args = List.mapi (fun i typ -> 
                des <| TyIndexVV(r,TyLitInt i,typ)) tuple_types
            TyVV(indexed_tuple_args, VVT tuple_types)
        | _ -> make_tyv_and_push r
let destructure_deep = 
    // Y Combinator
    let rec y f x = f (y f) x
    y destructure
let destructure_shallow =
    destructure (destructure make_tyv_and_push)

