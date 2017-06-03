// Edit: This algorithm is wrong. Check the last entry for details.

// 3/19/2017:

// The prototype for the recursive typechecker.

// Despite being local search, it can infer the return types of recursive and mutually
// recursive function without introducing metavariables or doing substitution and unification.

// It is guaranteed that the function will always return a well typed term instead of a metavariable
// unless it is divergent with this scheme. In fact, that is why the algorithm works in the first place
// because of that recurrent well-typedness guarantee.

// Check out the `idea for the next typechecker.txt` for more info.

// This algorithm that I just invented is called Recurrent Local Type Inference. RLTI (Reality) Algorithm for short.

// 4/3/2017:

// There was a bug where a submethod calls a supermethod which has been fixed right now.

// 4/4/2017:

// The fix was incorrect unfortunately. The simple case of a method recursively calling itself now crashes the typechecker.
// An idea I had last night is to instead of having each function hold its own stack, to use only a single global stack.
// This will both simplify the algorithm markedly and make it more roboust.

// I am not sure if this is sound, but it might be. I am decently sure that something along these lines should work.

// Edit: All the tests pass and the typechecker has been radically simplified as a result.
// Good enough for now.

// 4/15/2017:

// It turns out that the recursive case inside an If statement enters an infinite loop.
// It can be easily fixed by adding a channel inside tev_with_cur_stack and rather than peeking
// having the recursive methods stack be popped.

// It is too bad this idea did not occured to me 1.5 weeks ago, but now I am virtually certain that
// the algorithm is sound. Mutable channels are wonderful things.

// Edit: I can't believe I used `Pop |> ignore` instead of `Pop() |> ignore` and it still worked.
// I had this same bug in the previous versions. Ok, now it should be certain.

// Revisiting old code is a good habit. The only reason why I looked at this again was because I forgot
// how the thing worked and had to explain it to myself again.

// 5/25/2017: No change here, but it is possible to rewrite this example in CPS style which would get
// rid of the need to deal with stacks for recusive methods directly. For an example of how this is possible,
// check out `SpiralV5Language_v9a.fsx` which a rewrite of the Spiral typechecker in CPS style. Of special
// interest should be the `if_` and the `eval_method` functions.

// 6/3/2017: No, nevermind, this thing is wrong as will remain here for historical significance.

//let rec_error =
//    "RecError",
//    """
//fun rec loop x =
//    if true then
//        loop 1, 2, 3
//    else 
//        1, 2, 3
//    |> inl x,_,_ -> x
//fun top () = loop 1
//top ()
//    """
//let r = spiral_codegen default_dims rec_error
//
//  ("Types in branches of If do not match.
//Got: VVT
//  ([VVT ([PrimT Int64T; PrimT Int64T; PrimT Int64T],""); PrimT Int64T;
//    PrimT Int64T],"") and VVT ([PrimT Int64T; PrimT Int64T; PrimT Int64T],"")

// So close and yet so far away. For the toy language below I might be able to make it work
// by propagating the types upwards on recursion, but not all ops have straightforward types
// and it would lead to a massive effort in the real language. I'd be far better of using
// type annotations.

// Incidentally, this error here only occured to me while I was thinking of how I would type
// checkeck recursive datatypes. It is hillarious that the above example never occurred
// to me despite being at this for so long.

// Even though the following algorithm is wrong, it did lead me to quite a few other discoveries
// with CPS. The main language itself it so well designed that replacing this particular piece
// with annotations will not be a problem.

// What I will do on recursion is simply switch the typechecker from 'propagation' to 'checking'.
// Since I am doing inlining and partial evaluation in the Spiral language, I can't just return
// the type on an annotation the way standard typecheckers would, but returning it only on recursion
// would work just fine.

// Doing it like this will greatly simplify things once I finally decide to put in recursive datatypes
// into the language. I will be able to do things like streams and such which are untypable without sealing
// due to having infinite types.


open System.Collections.Generic

type Expr = 
    | V of string
    | If of Expr * Expr * Expr // can't do recursion without branching.
    | Apply of string * Expr list
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
    | TypeErrorT

and TyV = string * Ty
and TypedExpr =
    | TyV of TyV
    | TyIf of TypedExpr * TypedExpr * TypedExpr * Ty
    | TyLet of TyV * TypedExpr * TypedExpr * Ty
    | TyMethodCall of name: string * TypedExpr list * Ty

    | TyLitUnit
    | TyLitInt of int
    | TyLitFloat of float
    | TyLitBool of bool
    | TyTypeError of string

let get_type = function
    | TyV(_,t) -> t
    | TyIf(_,_,_,t) -> t
    | TyLet(_,_,_,t) -> t
    | TyMethodCall(_,_,t) -> t

    | TyLitUnit -> UnitT
    | TyLitInt _ -> Int32T
    | TyLitFloat _ -> Float32T
    | TyLitBool _ -> BoolT
    | TyTypeError _ -> TypeErrorT

// I am not going to go to any lengths to prevent name clashes for methods. 
// This particular version of the typechecker is just an experiment.
type Data = 
    {
    v_dict: Map<string,Ty>
    method_dict: Map<string,string list * Expr>
    method_dict': Dictionary<string * Ty list,Ty option>
    stack: Stack<unit -> TypedExpr>
    }

let d0() = {v_dict=Map.empty;method_dict=Map.empty;method_dict'=Dictionary();stack=Stack()}

let get_stack (stack: Stack<unit -> TypedExpr>) =
    if stack.Count > 0 then stack.Pop()()
    else failwith "The program is divergent."

let rec tev (d: Data) exp = 
    let tev_with_cur_stack e f =
        let mutable is_popped = false
        d.stack.Push (fun _ -> is_popped <- true; f())
        let x = tev d e
        if is_popped = false then d.stack.Pop() |> ignore
        x
    match exp with
    | V x -> TyV(x,d.v_dict.[x])
    | If (cond,tr,fl) -> // can't do recursion without branching.
        let cond = tev d cond

        let mutable fl_result = None
        let tr = 
            tev_with_cur_stack tr <| fun _ -> 
                let fl = tev d fl
                fl_result <- Some fl
                fl

        let fl = 
            match fl_result with
            | Some fl -> fl
            | None -> tev_with_cur_stack fl <| fun _ -> tr

        let tr_type = get_type tr
        let fl_type = get_type fl
        if get_type cond <> BoolT || tr_type <> fl_type then
            failwithf "get_type cond(%A) <> BoolT || tr_type(%A) <> fl_type(%A)" (get_type cond) tr_type fl_type
        else
            TyIf(cond,tr,fl,tr_type)

    | Apply (x,args) -> 
        let args = List.map (tev d) args
        let args' = List.map get_type args

        let n = x, args'

        let arg_names,method_ = d.method_dict.[x]
        if List.length args <> List.length arg_names then
            failwith "Arg sizes do not match."
        else
            let n' = d.method_dict'.TryGetValue n
            match n' with
            | false, _ ->
                d.method_dict'.Add(n,None)
                let method_typed_body = 
                    let v_dict = List.fold2 (fun m x y -> Map.add x y m) d.v_dict arg_names args'
                    tev {d with v_dict=v_dict} method_
                    
                d.method_dict'.[n] <- get_type method_typed_body |> Some
                TyMethodCall(x,args, get_type method_typed_body)

            | true, None ->
                let t = get_type (get_stack d.stack)
                d.method_dict'.[n] <- (Some t)
                TyMethodCall(x, args, t)
            | true, (Some t') -> 
                TyMethodCall(x, args, t')
                
    | Method(n,args,body,rest) as met -> 
        tev {d with method_dict=d.method_dict.Add(n,(args,body))} rest
    | Let(v,b,e) ->
        let b = tev d b
        let v' = (v,get_type b)
        let e = tev {d with v_dict=d.v_dict.Add v'} e
        TyLet(v',b,e,get_type e)

    | LitUnit -> TyLitUnit
    | LitInt x -> TyLitInt x
    | LitFloat x -> TyLitFloat x
    | LitBool x -> TyLitBool x

let term1 = // Correct
    let rec_call = Apply("meth",[LitBool true])
    Method("meth",["cond"],If(V "cond",LitInt 1,rec_call),rec_call)

let term2 = // Correct
    let rec_call = Apply("meth",[LitBool true])
    Method("meth",["cond"],If(V "cond",rec_call,LitInt 1),rec_call)

let term3 = // Error
    let rec_call = Apply("meth",[LitBool true])
    Method("meth",["cond"],If(V "cond",rec_call,rec_call),rec_call)

let term4 = 
    let rec_call = Apply("meth",[LitBool true])
    let if_ x = If(V "cond",x,rec_call)
    Method("meth",["cond"],if_ (if_ (if_ <| LitFloat 3.3)),rec_call)

let term5 = // Error
    let rec_call = Apply("meth",[LitBool true])
    Method("meth",["cond"],
        Let("x",If(V "cond",LitInt 1,rec_call),
            If(V "cond",LitFloat 1.5,rec_call)),rec_call)

let term6 = // Correct
    let rec_call = Apply("meth",[LitBool true])
    Method("meth",["cond"],
        Let("x",If(V "cond",LitFloat 2.5,rec_call),
            If(V "cond",LitFloat 1.5,rec_call)),rec_call)

let term7 = // Correct
    Method("q",["x"],
        Method("w",["y"],If (LitBool true, Apply("q",[V "y"]),LitFloat 5.5),Apply("w",[V "x"])),
        Apply("q",[LitFloat 3.3]))

let term8 = // Correct
    Method("q",["x"],
        Method("w",["y"],If (LitBool true, Apply("q",[V "y"]),LitInt 3),Apply("w",[V "x"])),
        Apply("q",[LitFloat 3.3]))

let term9 = // Error
    let rec_call = Apply("meth",[LitBool true])
    Method("meth",["cond"],rec_call,rec_call)

let term10 = // Correct
    let rec_call = Apply("meth",[LitBool true])
    let if_ x y = If(V "cond",x,y)
    Method("meth",["cond"],if_ (if_ (if_ rec_call rec_call) rec_call) (LitInt 3),rec_call)

let term11 = // Correct
    let rec_call = Apply("meth",[LitBool true])
    let rec_call2 = Apply("meth",[LitBool true])
    let if_ x y = If(V "cond",x,y)
    Method("meth2",["cond"], if_ (if_ rec_call2 rec_call2) rec_call2,
        Method("meth",["cond"],if_ (if_ (if_ rec_call rec_call) rec_call) (LitInt 3),rec_call))

try
    tev (d0()) term11
with e -> TyTypeError e.Message
