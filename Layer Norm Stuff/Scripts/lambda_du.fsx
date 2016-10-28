// I am super uninspired by quotations.
// I can't quite seem to find the right way to express what I want in code.

// I guess I'll try DUs as well.

type Var =
    | Int of name: string
    | Float of name: string
    | Tup2 of Var * Var
    | Tup3 of Var * Var * Var
    | Tup4 of Var * Var * Var * Var

    static member (+)(a: Var, b: Var) =
        Add(Eval a, Eval b)

    static member (*)(a: Var, b: Var) =
        Mult(Eval a, Eval b)

and Expr =
    | Lambda of Var * Expr
    | Eval of Var
    | Add of Expr * Expr
    | Mult of Expr * Expr

let a1 = Int "x"
let a2 = Int "y"
let a = Lambda(Tup2(a1,a2), a1 + a2)
