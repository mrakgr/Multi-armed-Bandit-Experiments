﻿#load "SpiralV5CudaTypechecker_v7e.fsx"
#r "../../packages/FParsec.1.0.2/lib/net40-client/FParsecCS.dll"
#r "../../packages/FParsec.1.0.2/lib/net40-client/FParsec.dll"
//#r "../../packages/FParsec-Pipes.0.3.1.0/lib/net45/FParsec-Pipes.dll"

open SpiralV5CudaTypechecker_v7e
open FParsec

type ParserExpr =
    | ParserStatement of (CudaExpr -> CudaExpr)
    | ParserExpr of CudaExpr

let comma = skipChar ',' .>> spaces
let underscore = skipChar '_' .>> spaces
let semicolon = skipChar ';' .>> spaces
let eq = skipChar '=' .>> spaces
let rdash = skipChar '\\' .>> spaces
let bar = skipChar '|' >>. spaces
let lam = skipString "->" .>> spaces
let set_me = skipString "<-" .>> spaces
let pppp = skipString "::" .>> spaces
let from = skipString "from" .>> spaces
let inl_ = skipString "inl" .>> spaces
let fun_ = skipString "fun" .>> spaces
let inl_rec = skipString "inl" .>> spaces .>> skipString "rec" .>> spaces
let fun_rec = skipString "fun" .>> spaces .>> skipString "rec" .>> spaces
let typecase = skipString "typecase" >>. spaces
let with_ = skipString "with" >>. spaces

let var_name = 
    many1Satisfy2L isAsciiLower (fun x -> isAsciiLetter x || isDigit x || x = ''' || x = '_') "identifier" .>> spaces
    >>=? function
        | "typecase" | "with" | "rec" | "if" | "then" | "else" | "inl" | "fun" as x -> fun _ -> 
            Reply(Error,messageError <| sprintf "%s not allowed as an identifier." x)
        | x -> preturn x
let type_name = 
    many1Satisfy2L isAsciiUpper (fun x -> isAsciiLetter x || isDigit x || x = ''' || x = '_') "Identifier" .>> spaces
        
let between_brackets l p r = between (skipChar l .>> spaces) (skipChar r .>> spaces) p
let rounds p = between_brackets '(' p ')'
let curlies p = between_brackets '{' p '}'
let quares p = between_brackets '[' p ']'

let pattern_identifier = 
    choice
        [
        underscore |>> fun _ -> S ""
        skipChar '*' >>. var_name |>> S'
        var_name |>> S
        ]

let pattern_tuple pattern = 
    sepBy1 pattern comma 
    |>> function 
        | _ :: _ :: _ as x -> SS x
        | x :: _ -> x
        | _ -> failwith "impossible"
    
let pattern_tuple' pattern = 
    let f = function
        | [last] -> preturn last
        | last :: rest ->
            match last with
            | S last -> preturn (SSS (List.rev rest) last)
            | _ -> fun _ -> Reply(FatalError, expected "standard identifier")
        | _ -> failwith "impossible"
    sepBy1 pattern pppp 
    >>= (List.rev >> f)
let pattern_rounds pattern = rounds (pattern <|>% SS [])
let pattern_type_name pattern = type_name >>. pattern_rounds pattern 
let pattern_active pattern = 
    pipe2 (rdash >>. pattern)
        (from >>. var_name)
        (fun pattern name -> F(pattern, V name))
let rec patterns s = 
    let cases s = choice [pattern_identifier; pattern_type_name pattern_identifier; pattern_rounds patterns; pattern_active patterns] s
    pattern_tuple (pattern_tuple' cases) s

let pattern_list = many1 patterns
    
let pbool = (skipString "false" |>> fun _ -> LitBool false) <|> (skipString "true" |>> fun _ -> LitBool true)

// FParsec has inbuilt parsers for ints and floats, but they will scan + and - which will wreak havoc with other parts
// of the library hence the custom number parsers.
let pnumber = 
    let pnumber = many1Satisfy isDigit
    pnumber >>= fun a ->
        choice [
            skipChar 'u' |>> (fun _ -> uint32 a |> LitUInt32)
            skipString "UL" |>> (fun _ -> uint64 a |> LitUInt64)
            skipString "L" |>> (fun _ -> int64 a |> LitInt64)
            skipChar '.' >>. pnumber >>= fun b ->
                let cfloat f = sprintf "%s.%s" a b |> f
                skipChar 'f' |>> fun _ -> cfloat float32 |> LitFloat32
                <|> fun _ -> Reply(cfloat float |> LitFloat64)
            fun _ -> Reply(int a |> LitInt32)
            ]

let lit = (pbool <|> pnumber) .>> notFollowedBy asciiLetter .>> spaces
let var = var_name |>> V
let if_then_else expr (s: CharStream<_>) =
    let i = s.Column
    let expr_indent expr (s: CharStream<_>) = if i <= s.Column then expr s else pzero s
    pipe3
        (skipString "if" >>. spaces1 >>. expr)
        (expr_indent (skipString "then" >>. spaces1 >>. expr))
        (opt (expr_indent (skipString "else" >>. spaces1 >>. expr)))
        (fun cond tr fl -> 
            let fl = match fl with Some x -> x | None -> VV []
            If(cond,tr,fl))
        s

let name = var_name

let case_inl_pat_statement expr = pipe2 (inl_ >>. patterns) (eq >>. expr) (fun pattern body -> l pattern body)
let case_inl_name_pat_list_statement expr = pipe3 (inl_ >>. name) pattern_list (eq >>. expr) (fun name pattern body -> l (S name) (inlr' "" pattern body))
let case_inl_rec_name_pat_list_statement expr = pipe3 (inl_rec >>. name) pattern_list (eq >>. expr) (fun name pattern body -> l (S name) (inlr' name pattern body))
let case_fun_name_pat_list_statement expr = pipe3 (fun_ >>. name) pattern_list (eq >>. expr) (fun name pattern body -> l (S name) (methr' "" pattern body))
let case_fun_rec_name_pat_list_statement expr = pipe3 (fun_rec >>. name) pattern_list (eq >>. expr) (fun name pattern body -> l (S name) (methr' name pattern body))

let statements expr = 
    [case_inl_pat_statement; case_inl_name_pat_list_statement; case_inl_rec_name_pat_list_statement
     case_fun_name_pat_list_statement; case_fun_rec_name_pat_list_statement]
    |> List.map (fun x -> x expr |> attempt)
    |> choice

let case_inl_pat_list_expr expr = pipe2 (inl_ >>. pattern_list) (lam >>. expr) (fun pattern body -> inl' pattern body)
let case_fun_pat_list_expr expr = pipe2 (fun_ >>. pattern_list) (lam >>. expr) (fun pattern body -> meth' pattern body)
let case_inl_rec_name_pat_list_expr expr = pipe3 (inl_rec >>. name) pattern_list (lam >>. expr) (fun name pattern body -> inlr' name pattern body)
let case_fun_rec_name_pat_list_expr expr = pipe3 (fun_rec >>. name) pattern_list (lam >>. expr) (fun name pattern body -> methr' name pattern body)

let case_lit expr = lit
let case_if_then_else expr = if_then_else expr 
let case_rounds expr = rounds (expr <|>% VV [])
let case_named_tuple expr = 
    pipe2 (skipChar '.' >>. type_name)
        (case_rounds expr)
        (fun name -> function
            | VV args -> VVNamed(args,name)
            | args -> VVNamed([args],name))
let case_var expr = var
 
let process_parser_exprs exprs = 
    let error_statement_in_last_pos _ = Reply(Error,messageError "Statements not allowed in the last position of a block.")
    let process_parser_expr a b on_fail on_succ =
        match a,b with
        | ParserStatement a, ParserExpr b -> a b |> ParserExpr |> on_succ
        | ParserExpr a, ParserExpr b -> l E a b |> ParserExpr |> on_succ
        | _, ParserStatement _ -> on_fail error_statement_in_last_pos

    let process_parser_exprs l =
        let rec loop = function
            | b :: a :: xs ->
                process_parser_expr a b 
                    id
                    (fun r -> loop (r :: xs))
            | x :: xs ->
                match x with
                | ParserExpr a -> preturn a 
                | ParserStatement a -> error_statement_in_last_pos
            | [] -> failwith "impossible"
        loop (List.rev l)

    match exprs with
    | _ :: _ -> process_parser_exprs exprs
    | _ -> preturn (VV [])

let indentations statements expressions (s: CharStream<_>) =
    let i = s.Column
    let inline if_ op tr (s: CharStream<_>) = if op i s.Column then tr s else pzero s
    let expr_indent expr =
        let mutable op = (=)
        let set_op op' x = op <- op'; x
        let semicolon s = if_ (<) (semicolon |>> set_op (<=)) s
        let expr s = if_ op (expr |>> set_op (=)) s
        many1 (expr .>> optional semicolon)
    expr_indent ((statements |>> ParserStatement) <|> (expressions i |>> ParserExpr)) >>= process_parser_exprs <| s

let application expr (s: CharStream<_>) =
    let i = s.Column
    let expr_up (s: CharStream<_>) = if i < s.Column then expr s else pzero s
    pipe2 expr (many expr_up) ap' s

let tuple expr i (s: CharStream<_>) =
    let expr_indent expr (s: CharStream<_>) = if i <= s.Column then expr s else pzero s
    sepBy1 (expr_indent (expr i)) (expr_indent comma)
    |>> function
        | _ :: _ :: _ as l -> VV l
        | x :: _ -> x
        | _ -> failwith "impossible"
    <| s

let case_typecase expr (s: CharStream<_>) =
    let i = s.Column
    let expr_indent expr (s: CharStream<_>) = if i <= s.Column then expr s else pzero s
    let pat_body = expr_indent patterns .>> expr_indent lam
    let pat_first = expr_indent (optional bar) >>. pat_body
    let pat = expr_indent bar >>. pat_body
    let body = expr_indent expr
    let case_case pat = pipe2 pat body (fun a b -> a,b)
    pipe3 (typecase >>. expr .>> with_)
        (case_case pat_first)
        (many (case_case pat))
        (fun e x xs -> match_ e (x :: xs)) s

let expressions expr =
    [case_inl_pat_list_expr; case_fun_pat_list_expr; case_inl_rec_name_pat_list_expr; case_fun_rec_name_pat_list_expr
     case_lit; case_if_then_else; case_rounds; case_var; case_named_tuple; case_typecase]
    |> List.map (fun x -> x expr |> attempt)
    |> choice

let mset expr i (s: CharStream<_>) = 
    let expr_indent expr (s: CharStream<_>) = if i < s.Column then expr s else pzero s
    pipe2 (expr i)
        (opt (expr_indent set_me >>. expr_indent (fun (s: CharStream<_>) -> expr s.Column s)))
        (fun l -> function
            | Some r -> MSet(l,r)
            | None -> l) s

let expr =
    let opp = new OperatorPrecedenceParser<_,_,_>()
    opp.AddOperator(InfixOperator("+", spaces, 60, Associativity.Left, fun x y -> Add(x,y)))
    opp.AddOperator(InfixOperator("-", spaces, 60, Associativity.Left, fun x y -> Sub(x,y)))
    opp.AddOperator(PrefixOperator("-", spaces, 100, true, Neg))
    opp.AddOperator(InfixOperator("*", spaces, 70, Associativity.Left, fun x y -> Mult(x,y)))
    opp.AddOperator(InfixOperator("/", spaces, 70, Associativity.Left, fun x y -> Div(x,y)))

    opp.AddOperator(InfixOperator("<=", spaces, 40, Associativity.None, fun x y -> LTE(x,y)))
    opp.AddOperator(InfixOperator("<", spaces, 40, Associativity.None, fun x y -> LT(x,y)))
    opp.AddOperator(InfixOperator("=", spaces, 40, Associativity.None, fun x y -> EQ(x,y)))
    opp.AddOperator(InfixOperator("<>", spaces, 40, Associativity.None, fun x y -> NEQ(x,y)))
    opp.AddOperator(InfixOperator(">", spaces, 40, Associativity.None, fun x y -> GT(x,y)))
    opp.AddOperator(InfixOperator(">=", spaces, 40, Associativity.None, fun x y -> GTE(x,y)))

    opp.AddOperator(InfixOperator("||", spaces, 20, Associativity.Right, fun x y -> And(x,y)))
    opp.AddOperator(InfixOperator("&&", spaces, 30, Associativity.Right, fun x y -> Or(x,y)))

    opp.AddOperator(InfixOperator("|>", spaces, 10, Associativity.Left, fun a f -> Apply(f,a)))
    opp.AddOperator(InfixOperator(">>", spaces, 10, Associativity.Left, fun a b -> inl (S "x") (Apply(b,Apply(a,V "x")))))
    opp.AddOperator(InfixOperator("<|", spaces, 10, Associativity.Left, fun f a -> Apply(f,a)))
    opp.AddOperator(InfixOperator("<<", spaces, 10, Associativity.Left, fun b a -> inl (S "x") (Apply(b,Apply(a,V "x")))))

    let operators expr i =
        opp.TermParser <- fun (s: CharStream<_>) -> if i <= s.Column then expr s else pzero s
        opp.ExpressionParser

    let rec expr s = indentations (statements expr) (mset (tuple (operators (application (expressions expr))))) s

    expr

open System.Collections.Generic
let tc body = 
    try
        let main_method, memo = 
            let d = data_empty default_dims
            exp_and_seq d body, d.memoized_methods
        let imemo = Dictionary(HashIdentity.Structural)
        //printfn "Done with typechecking. Going into closure conversion."
        //closure_conv imemo memo main_method |> ignore
        //Succ imemo
        Succ memo
    with e -> Fail (e.Message, e.StackTrace)

//let rec print_typed_ast expr = 
//    let print_fun = function
//        | MethodT ((a,b),_) -> sprintf "<method %s=%A>" a b
//        | InlineableT ((a,b),_) -> sprintf "<inlineable %s=%A>" a b
//    match expr with
//    | TyType f -> print_fun f
//    | TyV (a,b) -> sprintf "(V %i : %A)" a b
//    | TyIf (cond,tr,fl,t) ->
//        sprintf "(If %s then %s else %s : %A)" (print_typed_ast cond) (print_typed_ast tr) (print_typed_ast fl) t
//    | TyLet ((a,b),body,rest,t) -> //of TyV * TypedCudaExpr * TypedCudaExpr * CudaTy
//        sprintf "Let (%i : %A): %A = %s in\n %s" a b t (print_typed_ast body) (print_typed_ast rest)
//    | TyLitUInt32 x -> string x
//    | TyLitUInt64 x -> string x
//    | TyLitInt32 x -> string x
//    | TyLitInt64 x -> string x
//    | TyLitFloat32 x -> string x
//    | TyLitFloat64 x -> string x
//    | TyLitBool x -> string x
//    | TyMethodCall (f,b,t) -> sprintf "[%s <== %s : %A]" (print_fun (MethodT f)) (print_typed_ast b) t
    
    // Tuple cases
//    | TyVVIndex of TypedCudaExpr * TypedCudaExpr * CudaTy
//    | TyVV of TypedCudaExpr list * CudaTy

let prod_lam2 =
    """
fun rec meth cond l = if cond then meth cond l else l 1.5
meth true (inl x -> x)
    """

let fib =
    """
fun rec fib x = if x <= 0 then 0 else fib (x-1) + fib (x-2)
fib 5
    """

let fib_y =
    """
fun rec y f x = f (y f) x
fun fib r x = if x <= 0 then 0 else r (x-1) + r (x-2)
y fib 5
    """

let prod_lam3 = // Stackoverflow
    """
fun rec meth cond l = if cond then meth cond (inl x -> x) else l 1.5
meth true (inl x -> x)
    """

let result = run (spaces >>. expr) prod_lam3

let get = function Succ x -> x

let t = 
    match result with
    | Success (r,_,_) -> tc r

//for x in t do
//    match x.Value with
//    | MethodDone(sole_args, tyexpr, tag) ->
//        printfn "tyexpr=%A" (print_typed_ast tyexpr)
//        printfn "==="
