/// scalar constant value
type ConstSpecT = 
    | ConstInt of int
    | ConstDouble of double
    | ConstSingle of single
    | ConstBool of bool
        
    /// gets the value which must be of type 'T
    member this.GetValue() : 'T =
        match this with
        | ConstInt v -> v |> box |> unbox
        | ConstDouble v -> v |> box |> unbox
        | ConstSingle v -> v |> box |> unbox
        | ConstBool v -> v |> box |> unbox  

let a: int = ConstDouble 5.0 |> fun x -> x.GetValue()