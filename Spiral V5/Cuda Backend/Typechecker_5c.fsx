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
    | VTT of Ty list // represents the value tuple
    | NominalT of string // for classes and such
    | ConstT of Ty
    | SharedT of Ty
    | ArrayT of TyV list * Ty
    | ArrT of Ty list * Ty
and TyV = int64 * string * Ty

type MethodArgs =
    | MATy of Ty
    | MATag of int64
    | MAVV of MethodArgs list
    | MAET of MethodArgs list
    | MAVT of MethodArgs list

type MethodCall =
    | MCTypedExpr of TyV * TypedExpr
    | MCTag of int64
    | MCVV of MethodCall list
    | MCET of MethodCall list
    | MCVT of MethodCall list

// No return type polymorphism like in Haskell for now. Local type inference only.
and TyMethodKey = int64 * MethodArgs // The key does not need to know the free variables.

and Expr = 
    | V of string // standard variable
    | If of Expr * Expr * Expr
    | HoistedIf of Expr * Expr * Expr
    | Inlineable of Expr * Expr * Env option
    | LitUnit
    | LitInt of int
    | LitFloat of float
    | LitBool of bool
    | Apply of Expr * args: Expr
    | Method of args: Expr * body: Expr * return_type: Ty option
    | HoistedMethod of int64 * Env * args: Expr * body: Expr * return_type: Ty option
    | VV of Expr list // immediately destructure
    | ET of Expr list // expression tuple

    // Value tuple cases
    | IndexVT of Expr * Expr
    | VT of Expr list // value tuple

    // Array cases
    | IndexArray of Expr * Expr list * Ty
    | CreateArray of Expr list * Ty

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
    | ShuffleSource of Expr * Expr
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
    | MSet of var: Expr * body: Expr
    | AtomicAdd of out: Expr * in_: Expr
    // Loops
    | While of Expr * Expr * Expr
    // Magic
    | Typecase of (Data -> Expr -> ReturnCases) * Expr

// This is being compiled to STLC, not System F, so no type variables are allowed in the processed AST.
and TypedExpr =
    | TyV of TyV
    | TyIf of TypedExpr * TypedExpr * TypedExpr * Ty
    | TyLet of TyV * TypedExpr * TypedExpr * Ty
    | TyUnit
    | TyLitInt of int
    | TyLitFloat of float
    | TyLitBool of bool
    | TyMethodCall of TyMethodKey * MethodCall * Ty
    | TyMethod of TyMethodKey * MethodCall * Ty
    | TyVT of TypedExpr list * Ty

    // Cuda kernel constants
    | TyThreadIdxX | TyThreadIdxY | TyThreadIdxZ
    | TyBlockIdxX | TyBlockIdxY | TyBlockIdxZ
    | TyBlockDimX | TyBlockDimY | TyBlockDimZ
    | TyGridDimX | TyGridDimY | TyGridDimZ

    // Array cases
    | TyIndexArray of TypedExpr * TypedExpr list * Ty
    | TyCreateArray of TypedExpr list * Ty
    
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
    | TyShuffleSource of TypedExpr * TypedExpr * Ty
    | TyLog of TypedExpr * Ty
    | TyExp of TypedExpr * Ty
    | TyTanh of TypedExpr * Ty
    | TyNeg of TypedExpr * Ty
    // Mutable operations.
    | TyMSet of TypedExpr * TypedExpr
    | TyAtomicAdd of TypedExpr * TypedExpr * Ty
    | TyWhile of TypedExpr * TypedExpr * TypedExpr * Ty

and ReturnCases =
    | RTypedExpr of TypedExpr
    | RExpr of Expr
    | RError of string

and Env = Map<string,ReturnCases>
// method key * method body * bound variables * used variables
and MethodDict = Dictionary<TyMethodKey, TyV list * TypedExpr * Set<TyV> * Set<TyV>>
// method key * method body * implicit arguments
and MethodImplDict = Dictionary<TyMethodKey, TyV list * TypedExpr * Set<TyV>>

and Sequence =
    | SeqLet of TyV * TypedExpr
    | SeqWhile of TypedExpr * TypedExpr

and ArgCases = Expr * Env
and Data =
    {
    // Immutable
    env : Env
    args : ArgCases list
    // Mutable
    memoized_methods : MethodDict // For hoisted out global methods.
    sequences : Stack<Sequence>
    used_variables : HashSet<TyV>
    }

type Result<'a,'b> = Succ of 'a | Fail of 'b

let rec get_type = function
    | TyV(_,_,t) | TyIf(_,_,_,t) | TyLet(_,_,_,t) -> t
    | TyLitInt _ -> Int32T
    | TyLitFloat _ -> Float32T
    | TyLitBool _ -> BoolT
    | TyUnit -> UnitT
    | TyMethodCall(_,_,t) | TyVT(_,t) -> t

    // Cuda kernel constants
    | TyThreadIdxX | TyThreadIdxY | TyThreadIdxZ
    | TyBlockIdxX | TyBlockIdxY | TyBlockIdxZ
    | TyBlockDimX | TyBlockDimY | TyBlockDimZ
    | TyGridDimX | TyGridDimY | TyGridDimZ -> Int32T

    // Array cases
    | TyIndexArray(_,_,t) | TyCreateArray(_,t) -> t

    // Primitive operations on expressions.
    | TyAdd(_,_,t) | TySub(_,_,t) | TyMult(_,_,t)
    | TyDiv(_,_,t) | TyMod(_,_,t) -> t
    | TyLT _ | TyLTE _ | TyEQ _ | TyGT _
    | TyGTE _ -> BoolT
    | TyLeftShift(_,_,t) | TyRightShift(_,_,t) -> t
    | TySyncthreads -> UnitT
    | TyShuffleXor(_,_,t) | TyShuffleUp(_,_,t)
    | TyShuffleDown(_,_,t) | TyShuffleSource(_,_,t) -> t
    | TyLog(_,t) | TyExp(_,t) | TyTanh(_,t)
    | TyNeg(_,t) -> t
    // Mutable operations.
    | TyMSet _ -> UnitT
    | TyAtomicAdd(_,_,t) -> t
    // Loops
    | TyWhile(_,_,_,t) -> t
    | TyMethod(_,_,t) -> t


let is_numeric a =
    match get_type a with
    | UInt32T | UInt64T | Int32T | Int64T 
    | Float32T | Float64T -> true
    | _ -> false

let is_atomic_add_supported a =
    match get_type a with
    | UInt32T | UInt64T | Int32T
    | Float32T | Float64T -> true
    | _ -> false

let is_float a =
    match get_type a with
    | Float32T | Float64T -> true
    | _ -> false

let is_bool a =
    match get_type a with
    | BoolT -> true
    | _ -> false

let is_int a =
    match get_type a with
    | UInt32T | UInt64T | Int32T | Int64T -> true
    | _ -> false

let is_vt a =
    match get_type a with
    | VTT _ -> true
    | _ -> false

let is_const a =
    let rec loop = function
        | ConstT _ -> true
        | SharedT x -> loop x
        | _ -> false
    loop (get_type a)

let get_tag =
    let mutable x = 0L
    fun () -> 
        let x' = x
        x <- x + 1L
        x'

let rec call_to_args = function
    | MCVV x -> MAVV (List.map call_to_args x)
    | MCTag x -> MATag x
    | MCTypedExpr(_,x) -> MATy (get_type x)
    | MCVT x -> MAVT (List.map call_to_args x)
    | MCET x -> MAET (List.map call_to_args x)

let sequences_to_typed_expr (sequences: Stack<Sequence>) final_expr =
    let type_fin = get_type final_expr
    Seq.fold (fun rest -> function 
        | SeqLet(v,body) -> TyLet(v,body,rest,type_fin)
        | SeqWhile(cond,body) -> TyWhile(cond,body,rest,type_fin)) final_expr sequences

let get_bound_variables (env: Env) =
    env
    |> Seq.choose (fun kv -> 
        match kv.Value with
        | RTypedExpr(TyV v) -> Some v
        | _ -> None)
    |> Set

let map_fold_2_Er f state (x,y) =
    if List.length x = List.length y then
        let rec loop f state = function
            | x :: xs, y :: ys ->
                match f state (x,y) with
                | Succ(r,state) ->
                    match loop f state (xs,ys) with
                    | Succ(rs,state) -> Succ (r::rs,state)
                    | Fail _ as er -> er
                | Fail er -> Fail er
            | [], [] -> Succ ([], state)
            | x -> failwith "Argument size mismatch in map_fold_2_Er."
        loop f state (x,y)
    else
        Fail <| sprintf "Argument size mismatch in map_fold_2_Er. Args: %A" (x,y)

let map_typed f x =
    let rec loop = function
        | x :: xs -> 
            match f x with
            | RTypedExpr x ->
                match loop xs with
                | Succ xs -> Succ (x :: xs)
                | Fail _ as er -> er
            | RExpr x -> Fail <| sprintf "Expected: typed expression.\nGot: %A" x
            | RError er -> Fail er
        | [] -> Succ []
    loop x
            

let mapResult f = function
    | Succ x -> Succ <| f x
    | Fail er -> Fail er

let mapFst f (a,b) = (f a, b)
let mapResultFst f = mapResult (mapFst f)

let rec fold_2_er f state (x,y) =
    if List.length x = List.length y then
        let rec loop f state = function
            | x :: xs, y :: ys ->
                match f state (x,y) with
                | Succ state ->
                    match loop f state (xs,ys) with
                    | Succ state -> Succ state
                    | Fail _ as er -> er
                | Fail er -> Fail er
            | [], [] -> Succ state
            | x -> failwith "Argument size mismatch in fold_2_er."
        loop f state (x,y)
    else
        Fail <| sprintf "Argument size mismatch in fold_2_er. Args: %A" (x,y)

let rec with_empty_seq (d: Data) expr =
    let d' = {d with sequences = Stack()}
    match exp_and_seq d' expr with
    | RExpr _ as x -> x
    | RTypedExpr x -> RTypedExpr <| sequences_to_typed_expr d'.sequences x
    | RError _ as er -> er

// Does macros expansion, sequtialization and takes note of the bound and 
// used variables in the method dictionary for the following passes.
and exp_and_seq (d: Data) exp: ReturnCases =
    let tev d exp = exp_and_seq d exp

    /// Patterm matching functions
    let add_bound_variable env arg_name ty_arg =
        let v = TyV ty_arg
        v, Map.add arg_name (RTypedExpr v) env

    let dup_name_check (name_checker: HashSet<string>) arg_name f =
        match name_checker.Add arg_name || arg_name = "" || arg_name = "_" with
        | true -> f()
        | false -> Fail <| sprintf "%s is a duplicate name in pattern matching." arg_name

    let bind_expr_fail acc (arg_name, exp) =
        Fail "Cannot bind untyped expressions in value structures like Vars."
    let bind_expr (name_checker: HashSet<string>) acc (arg_name, exp) =
        dup_name_check name_checker arg_name <| fun _ ->
            let exp = RExpr exp
            Succ (Map.add arg_name exp acc)
        
    let bind_typedexpr_fail acc (arg_name, ty_exp) =
        Fail "Cannot bind typed expressions in expression tuples."
    let bind_typedexpr'' acc (arg_name, ty_exp) =
        let b'_type = get_type ty_exp
        let ty_arg: TyV = get_tag(),arg_name,b'_type
        // Pushes the sequence onto the stack
        d.sequences.Push(SeqLet(ty_arg,ty_exp))
        // Binds the name to the said sequence's name and loops to the next argument
        add_bound_variable acc arg_name ty_arg
    let bind_typedexpr' a b = bind_typedexpr'' a b |> snd
    let bind_typedexpr (name_checker: HashSet<string>) acc (arg_name, ty_exp) =
        dup_name_check name_checker arg_name <| fun _ ->
            Succ (bind_typedexpr' acc (arg_name, ty_exp))
    let bind_template bind_expr bind_typedexpr eval_env acc (arg_name, right_arg) =
        match tev {d with env=eval_env; args=[]} right_arg with
        | RError er -> Fail er
        | RExpr exp -> bind_expr acc (arg_name, exp)
        | RTypedExpr ty_exp -> bind_typedexpr acc (arg_name, ty_exp)

    let bind_any name_checker = bind_template (bind_expr name_checker) (bind_typedexpr name_checker)
    let bind_expr_only name_checker = bind_template (bind_expr name_checker) bind_typedexpr_fail
    let bind_typedexpr_only name_checker = bind_template bind_expr_fail (bind_typedexpr name_checker)

    let match_v_template match_vv bind (eval_env: Env) (acc: Env) = function
        | V arg_name, right_arg ->
            bind acc (arg_name, right_arg)
        | VV args, right_arg ->
            match_vv eval_env acc (args, right_arg)
        | x -> Fail <| sprintf "Unexpected arguments in match_v.\n%A" x

    let bind_method name_checker acc (arg_name, exp) =
        dup_name_check name_checker arg_name <| fun _ ->
            match exp with
            | HoistedMethod(tag,_,_,_,_) -> 
                let exp' = MCTag tag
                Succ (exp', Map.add arg_name (RExpr exp) acc)
            | x -> Fail <| sprintf "Expected: method.\nGot: %A" x
    
    let bind_typedexpr_method name_checker acc (arg_name, ty_exp: TypedExpr) =
        dup_name_check name_checker arg_name <| fun _ ->
            let v = get_tag(),arg_name,get_type ty_exp
            let ty_arg = TyV v
            let acc = Map.add arg_name (RTypedExpr ty_arg) acc
            Succ (MCTypedExpr(v,ty_exp), acc)

    let bind_method_only name_checker = bind_template (bind_method name_checker) bind_typedexpr_fail
    let bind_ty_only name_checker = bind_template bind_expr_fail (bind_typedexpr_method name_checker)
    let bind_any_method name_checker = bind_template (bind_method name_checker) (bind_typedexpr_method name_checker)

    let traverse_generic tuple_constructor f s l = map_fold_2_Er f s l |> mapResultFst tuple_constructor

    let rec match_vt_template traverse bind (eval_env: Env) (acc: Env) (l,r) = 
        let match_vv = match_vt_template traverse bind
        match r with
        | VT r -> traverse (match_v_template match_vv (bind eval_env) eval_env) acc (l,r)
        | x -> Fail <| sprintf "Unexpected arguments in match_vt.\n%A" x

    let match_vt name_checker = match_vt_template fold_2_er (bind_typedexpr_only name_checker)
    let match_vt_method name_checker = match_vt_template (traverse_generic MCVT) (bind_ty_only name_checker)

    let rec match_et_template traverse bind (eval_env: Env) (acc: Env) (l,r) = 
        let match_vv = match_et_template traverse bind
        match r with
        | ET r -> traverse (match_v_template match_vv (bind eval_env) eval_env) acc (l,r)
        | x -> Fail <| sprintf "Unexpected arguments in match_et.\n%A" x

    let match_et name_checker = match_et_template fold_2_er (bind_expr_only name_checker)
    let match_et_method name_checker = match_et_template (traverse_generic MCET) (bind_method_only name_checker)

    let rec match_tyvt_template match_vt (eval_env: Env) (acc: Env) (l,r) = 
        match get_type r with
        | VTT x -> 
            

