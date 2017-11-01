type Ty =
    | DotNetType of TypedExpr
    | PrimT
    | ListT of Ty list
    | LamT of Ty * Ty

and Expr =
    | Ap of Expr * Expr
    | Prim of Ty
    | Var of string
    | List of Expr list
    | Lam of string list * Expr
    | Class of ExprClass

and ExprClass = {
    fullname : string
    methods : Map<string, Expr>
    }

and TypedExpr =
    | TyPrim of Ty
    | TyVar of string
    | TyList of TypedExpr list
    | TyLam of EnvTerm * string list * Expr
    | TyClass of TypedExprClass

and TypedExprClass = {
    fullname : string
    methods : Map<string, TypedExpr>
    }
    
and EnvTerm = Map<string,TypedExpr>

