type Expr = 
    | If of Expr * Expr * Expr // can't do recursion without branching.
    | Apply of Expr * Expr list
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
and TyV = int64 * string * Ty
and TypedExpr =
    | TyV of TyV
    | TyIf of TypedExpr * TypedExpr * TypedExpr * Ty
    | TyLet of TyV * TypedExpr * TypedExpr * Ty
    | TyMethodCall of name: string * TypedExpr list * Ty

    | TyLitUnit
    | TyLitInt of int
    | TyLitFloat of float
    | TyLitBool of bool