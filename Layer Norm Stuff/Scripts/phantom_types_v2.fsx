// Let me try a different tack.

// Edit: No, this won't work. I really need the DUs to do any compiler related work.

let inline eval x =
    (^a : (member Eval: ^b) x)

type C<'a> = 
    | C of 'a
    member inline t.Eval = t |> fun (C(a)) -> a

type Add = 
    | Add of C<int> * C<int> 
    member inline t.Eval = t |> fun (Add(a,b)) -> eval a + eval b |> C

type Eq = 
    | Eq of C<int> * C<int> 
    member inline t.Eval = t |> fun (Eq(a,b)) -> eval a = eval b |> C
        
let add x y = Add(x,y)
let eq x y = Eq(x,y)

let expr1 = (C(5) |> add (C(1))) |> eq (C(6))

let x = eval expr1
