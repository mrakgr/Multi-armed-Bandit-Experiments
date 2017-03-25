open System.Collections.Generic

type Ty =
    | UnitT
    | UInt32T
    | UInt64T
    | Int32T
    | Int64T
    | Float32T
    | Float64T
    | BoolT
    | VVT of Ty list
    | NominalT of string // for classes and such
    | GlobalArrayT of TyV list * Ty
    | SharedArrayT of TyV list * Ty
    | LocalArrayT of TyV list * Ty
    | ArrT of Ty list * Ty
    | TagT of int64
and TyV = int64 * string * Ty

// No return type polymorphism like in Haskell for now. Local type inference only.
and TyMethodKey = int64 * Ty // The key does not need to know the free variables.

and Expr = 
    | V of string // standard variable
    | If of Expr * Expr * Expr
    | HoistedIf of Expr * Expr * Expr
    | Inlineable of Expr * Expr
    | LitUnit
    | LitInt of int
    | LitFloat of float
    | LitBool of bool
    | Apply of Expr * args: Expr
    | Method of name: string * args: Expr * body: Expr

    // Tuple cases
    | IndexVV of Expr * Expr
    | VV of Expr list // tuple

    // Array cases
    | IndexArray of Expr * Expr list
    | CreateSharedArray of Expr list * Ty
    | CreateLocalArray of Expr list * Ty

    // Primitive operations on expressions.
    | Add of Expr * Expr
    | Sub of Expr * Expr
    | Mult of Expr * Expr
    | Div of Expr * Expr
    | Mod of Expr * Expr
    | LT of Expr * Expr
    | LTE of Expr * Expr
    | EQ of Expr * Expr
    | GT of Expr * Expr
    | GTE of Expr * Expr
    | LeftShift of Expr * Expr
    | RightShift of Expr * Expr
    | Syncthreads
    | ShuffleXor of Expr * Expr
    | ShuffleUp of Expr * Expr
    | ShuffleDown of Expr * Expr
    | ShuffleIndex of Expr * Expr
    | Log of Expr
    | Exp of Expr
    | Tanh of Expr
    | Neg of Expr
    // Cuda kernel constants
    | ThreadIdxX | ThreadIdxY | ThreadIdxZ
    | BlockIdxX | BlockIdxY | BlockIdxZ
    | BlockDimX | BlockDimY | BlockDimZ
    | GridDimX | GridDimY | GridDimZ
    // Mutable operations.
    | MSet of Expr * Expr * Expr
    | AtomicAdd of out: Expr * in_: Expr
    // Loops
    | While of Expr * Expr * Expr
    // Cub operations
    | CubBlockReduce of Expr * Expr option

    static member (+)(a,b) = Add(a,b)
    static member (-)(a,b) = Sub(a,b)
    static member (*)(a,b) = Mult(a,b)
    static member (/)(a,b) = Div(a,b)
    static member (%)(a,b) = Mod(a,b)

    static member (.=)(a,b) = EQ(a,b)
    static member (.<)(a,b) = LT(a,b)
    static member (.<=)(a,b) = LTE(a,b)
    static member (.>)(a,b) = GT(a,b)
    static member (.>=)(a,b) = GTE(a,b)

// This is being compiled to STLC, not System F, so no type variables are allowed in the processed AST.
and TypedExpr =
    // These two will not get code gen'd.
    // The difference from the past version of the typechecker is that now the TagT type exists.
    | Inlineable' of Expr * Expr * Env * Ty
    | Method' of name: string * args: Expr * body: Expr * Env * Ty
    
    | TyV of TyV
    | TyIf of TypedExpr * TypedExpr * TypedExpr * Ty
    | TyLet of TyV * TypedExpr // expression (in tuples) / statement (in seqs)
    | TyUnit
    | TyLitInt of int
    | TyLitFloat of float
    | TyLitBool of bool
    | TyMethodCall of TyMethodKey * TypedExpr * Ty
    
    // Tuple cases
    | TyIndexVV of TypedExpr * TypedExpr * Ty
    | TyVV of TypedExpr list * Ty

    // Seq
    | TySeq of TypedExpr * TypedExpr * Ty
        
    // Array cases
    | TyIndexArray of TypedExpr * TypedExpr list * Ty
    | TyCreateSharedArray of TypedExpr list * Ty
    | TyCreateLocalArray of TypedExpr list * Ty

    // Cuda kernel constants
    | TyThreadIdxX | TyThreadIdxY | TyThreadIdxZ
    | TyBlockIdxX | TyBlockIdxY | TyBlockIdxZ
    | TyBlockDimX | TyBlockDimY | TyBlockDimZ
    | TyGridDimX | TyGridDimY | TyGridDimZ
   
    // Primitive operations on expressions.
    | TyAdd of TypedExpr * TypedExpr * Ty
    | TySub of TypedExpr * TypedExpr * Ty
    | TyMult of TypedExpr * TypedExpr * Ty
    | TyDiv of TypedExpr * TypedExpr * Ty
    | TyMod of TypedExpr * TypedExpr * Ty
    | TyLT of TypedExpr * TypedExpr
    | TyLTE of TypedExpr * TypedExpr
    | TyEQ of TypedExpr * TypedExpr
    | TyGT of TypedExpr * TypedExpr
    | TyGTE of TypedExpr * TypedExpr
    | TyLeftShift of TypedExpr * TypedExpr * Ty
    | TyRightShift of TypedExpr * TypedExpr * Ty
    | TySyncthreads
    | TyShuffleXor of TypedExpr * TypedExpr * Ty
    | TyShuffleUp of TypedExpr * TypedExpr * Ty
    | TyShuffleDown of TypedExpr * TypedExpr * Ty
    | TyShuffleIndex of TypedExpr * TypedExpr * Ty
    | TyLog of TypedExpr * Ty
    | TyExp of TypedExpr * Ty
    | TyTanh of TypedExpr * Ty
    | TyNeg of TypedExpr * Ty
    // Mutable operations.
    | TyMSet of TypedExpr * TypedExpr // statement
    | TyAtomicAdd of TypedExpr * TypedExpr * Ty
    | TyWhile of TypedExpr * TypedExpr // statement
    // Cub operations
    | TyCubBlockReduce of TypedExpr * TypedExpr option * Ty

and Env = Map<string, TypedExpr>
// method key * method body * bound variables * used variables
and MethodCases =
    | MethodInEvaluation of Ty option * Stack<unit -> TypedExpr>
    | MethodDone of TyV list * TypedExpr * Set<TyV> * Set<TyV>
and MethodDict = Dictionary<TyMethodKey, MethodCases>
and TaggedDict = Dictionary<int64,TypedExpr>
// method key * method body * implicit arguments
and MethodImplDict = Dictionary<TyMethodKey, TyV list * TypedExpr * Set<TyV>>
and Data =
    {
    // Immutable
    env : Env
    // Mutable
    tagged_vars : TaggedDict // For looking up the the unapplied Inlineables and Methods
    memoized_methods : MethodDict // For hoisted out global methods.
    sequences : Stack<TypedExpr>
    used_variables : HashSet<TyV>
    current_stack : Stack<unit -> TypedExpr>
    }

type Result<'a,'b> = Succ of 'a | Fail of 'b

let rec get_type = function
    | Inlineable'(_,_,_,t) | Method'(_,_,_,_,t) -> t

    | TyV(_,_,t) | TyIf(_,_,_,t) | TyLet((_,_,t),_) -> t
    | TyLitInt _ -> Int32T
    | TyLitFloat _ -> Float32T
    | TyLitBool _ -> BoolT
    | TyUnit -> UnitT
    | TyMethodCall(_,_,t) -> t

    // Cuda kernel constants
    | TyThreadIdxX | TyThreadIdxY | TyThreadIdxZ
    | TyBlockIdxX | TyBlockIdxY | TyBlockIdxZ
    | TyBlockDimX | TyBlockDimY | TyBlockDimZ
    | TyGridDimX | TyGridDimY | TyGridDimZ -> Int32T

    // Tuple cases
    | TyVV(_,t) | TyIndexVV(_,_, t) -> t

    // Seq
    | TySeq(_,_,t) -> t

    // Array cases
    | TyIndexArray(_,_,t) | TyCreateLocalArray(_,t) | TyCreateSharedArray(_,t) -> t

    // Primitive operations on expressions.
    | TyAdd(_,_,t) | TySub(_,_,t) | TyMult(_,_,t)
    | TyDiv(_,_,t) | TyMod(_,_,t) -> t
    | TyLT _ | TyLTE _ | TyEQ _ | TyGT _
    | TyGTE _ -> BoolT
    | TyLeftShift(_,_,t) | TyRightShift(_,_,t) -> t
    | TySyncthreads -> UnitT
    | TyShuffleXor(_,_,t) | TyShuffleUp(_,_,t)
    | TyShuffleDown(_,_,t) | TyShuffleIndex(_,_,t) -> t
    | TyLog(_,t) | TyExp(_,t) | TyTanh(_,t)
    | TyNeg(_,t) -> t
    // Mutable operations.
    | TyAtomicAdd(_,_,t) -> t
    | TyMSet(_,_) -> UnitT
    // Loops
    | TyWhile(_,_) -> UnitT
    // Cub operations
    | TyCubBlockReduce(_,_,t) -> t

let rec is_numeric' a =
    match a with
    | UInt32T | UInt64T | Int32T | Int64T 
    | Float32T | Float64T -> true
    | _ -> false
let is_numeric a = is_numeric' (get_type a)

let is_atomic_add_supported a =
    match get_type a with
    | UInt32T | UInt64T | Int32T
    | Float32T | Float64T -> true
    | _ -> false

let rec is_float' a =
    match a with
    | Float32T | Float64T -> true
    | _ -> false
let is_float a = is_float' (get_type a)

let rec is_bool' a =
    match a with
    | BoolT -> true
    | _ -> false
let is_bool a = is_bool' (get_type a)

let rec is_int' a =
    match a with
    | UInt32T | UInt64T | Int32T | Int64T -> true
    | _ -> false
let is_int a = is_int' (get_type a)

let is_vv a =
    match get_type a with
    | VVT _ -> true
    | _ -> false

let get_tag =
    let mutable x = 0L
    fun () -> 
        let x' = x
        x <- x + 1L
        x'

let get_bound_variables (env: Env) =
    env
    |> Seq.choose (fun kv -> 
        match kv.Value with
        | TyV v -> Some v
        | _ -> None)
    |> Set

let map_fold_2_Er f state x y =
    let rec loop f state = function
        | x :: xs, y :: ys -> 
            let r, state = f state x y
            let rs, state = loop f state (xs,ys) 
            (r :: rs, state)
        | [], [] -> [], state
        | x -> failwith "Argument size mismatch in map_fold_2_Er."
    loop f state (x,y)

let get_body_from (stack: Stack<unit -> TypedExpr>) = stack.Peek()()

// Does macro expansion and takes note of the bound and 
// used variables in the method dictionary for the following passes.
let rec exp_and_seq (d: Data) exp: TypedExpr =
    let tev d exp = exp_and_seq d exp

    let dup_name_check (name_checker: HashSet<string>) arg_name f =
        match name_checker.Add arg_name || arg_name = "" || arg_name = "_" with
        | true -> f()
        | false -> failwithf "%s is a duplicate name in pattern matching." arg_name

    let bind_typedexpr_inl name_checker acc arg_name ty_exp =
        dup_name_check name_checker arg_name <| fun _ ->
            let v = get_tag(), arg_name, get_type ty_exp
            d.sequences.Push(TyLet(v,ty_exp))
            Map.add arg_name (TyV v) acc

    let bind_typedexpr_method name_checker acc arg_name ty_exp =
        dup_name_check name_checker arg_name <| fun _ ->
            let v = get_tag(), arg_name, get_type ty_exp
            TyLet(v,ty_exp), Map.add arg_name (TyV v) acc

    let traverse_inl t = List.fold2
    let traverse_method t f s a b = 
        map_fold_2_Er f s a b
        |> fun (l,s) -> TyVV(l,t), s

    let rec match_vv traverse bind acc l r =
        match l,r with
        | V x, r -> bind acc x r
        | VV l, TyVV(r,t) -> traverse t (match_vv traverse bind) acc l r
        | VV l, r ->
            match get_type r with
            | VVT x as t -> 
                let r = List.mapi (fun i t -> TyIndexVV(r,TyLitInt i,t)) x
                traverse t (match_vv traverse bind) acc l r
            | x -> failwithf "Unexpected arguments in destructuring.\nGot: %A" x
        | l, r -> failwithf "Expected V or VV on the left side.\nGot: %A" l

    let match_vv_inl dup_name_checker = match_vv traverse_inl (bind_typedexpr_inl dup_name_checker)
    let match_vv_method dup_name_checker = match_vv traverse_method (bind_typedexpr_method dup_name_checker)

    failwith "placeholder"