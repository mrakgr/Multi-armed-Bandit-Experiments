#load "SpiralV5Language_v8b.fsx"
#r "../../packages/FParsec.1.0.2/lib/net40-client/FParsecCS.dll"
#r "../../packages/FParsec.1.0.2/lib/net40-client/FParsec.dll"

open SpiralV5Language_v8b
open FParsec

type ParserExpr =
| ParserStatement of (CudaExpr -> CudaExpr)
| ParserExpr of CudaExpr

let comma = skipChar ',' .>> spaces
let dot = skipChar '.' >>. spaces
let grave = skipChar '`' >>. spaces
let underscore = skipChar '_' .>> spaces
let semicolon = skipChar ';' .>> spaces
let eq = skipChar '=' .>> spaces
let bar = skipChar '|' >>. spaces
let lam = skipString "->" .>> spaces
let set_me = skipString "<-" .>> spaces
let pppp = skipString "::" .>> spaces
let inl_ = skipString "inl" .>> spaces
let fun_ = skipString "fun" .>> spaces
let inl_rec = skipString "inl" .>> spaces .>> skipString "rec" .>> spaces
let fun_rec = skipString "fun" .>> spaces .>> skipString "rec" .>> spaces
let typecase = skipString "typecase" >>. spaces
let module_ = skipString "module" >>. spaces
let with_ = skipString "with" >>. spaces
let open_ = skipString "open" >>. spaces

let isAsciiIdStart c =
    isAsciiLetter c || c = '_'

let isAsciiIdContinue c =
    isAsciiLetter c || isDigit c || c = '_' || c = '''

let var_name = 
    many1Satisfy2L isAsciiLower isAsciiIdContinue "identifier" .>> spaces
    >>=? function
        | "open" | "module" | "typecase" | "with" | "rec" | "if" | "then" | "else" | "inl" | "fun" as x -> fun _ -> 
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
        underscore >>% (S "")
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
let pattern_type_name pattern = 
    pipe2
        (dot >>. type_name)
        (pattern_rounds pattern)
        (fun nam pat -> N(nam,pat))
let pattern_active pattern = 
    pipe2 (dot >>. var_name)
        (rounds pattern <|> pattern_identifier)
        (fun name pattern -> F(pattern, name))
let rec patterns s = 
    let cases s = choice [pattern_identifier; pattern_type_name pattern_identifier; pattern_rounds patterns; pattern_active patterns] s
    pattern_tuple (pattern_tuple' cases) s

let pattern_list = many1 patterns
    
let pbool = (skipString "false" >>% LitBool false) <|> (skipString "true" >>% LitBool true)
let pnumber : Parser<_,_> =
    let numberFormat =  NumberLiteralOptions.AllowFraction
                        ||| NumberLiteralOptions.AllowExponent
                        ||| NumberLiteralOptions.AllowHexadecimal
                        ||| NumberLiteralOptions.AllowBinary
                        ||| NumberLiteralOptions.AllowInfinity
                        ||| NumberLiteralOptions.AllowNaN

    let parser = numberLiteral numberFormat "number"

    let default_int x _ = int64 x |> LitInt64 |> Reply
    let default_float x _ = float32 x |> LitFloat32 |> Reply

    let followedBySuffix x default_ =
        let f str f = followedBy (skipString str) |>> fun _ -> f x
        choice
            [
            f "i8" (int8 >> LitInt8)
            f "i16" (int16 >> LitInt16)
            f "i32" (int32 >> LitInt32)
            f "i64" (int64 >> LitInt64)

            f "u8" (uint8 >> LitUInt8)
            f "u16" (uint16 >> LitUInt16)
            f "u32" (uint32 >> LitUInt32)
            f "u64" (uint64 >> LitUInt64)

            f "f32" (float32 >> LitFloat32)
            f "f64" (float >> LitFloat64)
            default_ x
            ]

    fun s ->
        let reply = parser s
        if reply.Status = Ok then
            let nl = reply.Result // the parsed NumberLiteral
            try 
                if nl.IsInteger then followedBySuffix nl.String default_int s
                else followedBySuffix nl.String default_float s
            with
            | :? System.OverflowException as e ->
                s.Skip(-nl.String.Length)
                Reply(FatalError, messageError e.Message)
        else // reconstruct error reply
            Reply(reply.Status, reply.Error)


let lit = (pbool <|> pnumber) |>> Lit .>> notFollowedBy (satisfy (fun c -> isDigit c || isAsciiIdStart c)) .>> spaces
let var = var_name |>> V

let expr_indent i op expr (s: CharStream<_>) = if op i s.Column then expr s else pzero s
let if_then_else expr (s: CharStream<_>) =
    let i = s.Column
    let expr_indent expr (s: CharStream<_>) = expr_indent i (<=) expr s
    pipe3
        (skipString "if" >>. spaces1 >>. expr)
        (expr_indent (skipString "then" >>. spaces1 >>. expr))
        (opt (expr_indent (skipString "else" >>. spaces1 >>. expr)))
        (fun cond tr fl -> 
            let fl = match fl with Some x -> x | None -> B
            Op(If,[cond;tr;fl]))
        s

let name = var_name

let case_inl_pat_statement expr = pipe2 (inl_ >>. patterns) (eq >>. expr) (fun pattern body -> l pattern body)
let case_inl_name_pat_list_statement expr = pipe3 (inl_ >>. name) pattern_list (eq >>. expr) (fun name pattern body -> l (S name) (inlr' "" pattern body))
let case_inl_rec_name_pat_list_statement expr = pipe3 (inl_rec >>. name) pattern_list (eq >>. expr) (fun name pattern body -> l (S name) (inlr' name pattern body))
let case_fun_name_pat_list_statement expr = pipe3 (fun_ >>. name) pattern_list (eq >>. expr) (fun name pattern body -> l (S name) (methr' "" pattern body))
let case_fun_rec_name_pat_list_statement expr = pipe3 (fun_rec >>. name) pattern_list (eq >>. expr) (fun name pattern body -> l (S name) (methr' name pattern body))
let case_open expr = open_ >>. expr |>> module_open

let statements expr = 
    [case_inl_pat_statement; case_inl_name_pat_list_statement; case_inl_rec_name_pat_list_statement
     case_fun_name_pat_list_statement; case_fun_rec_name_pat_list_statement; case_open]
    |> List.map (fun x -> x expr |> attempt)
    |> choice

let case_inl_pat_list_expr expr = pipe2 (inl_ >>. pattern_list) (lam >>. expr) (fun pattern body -> inl' pattern body)
let case_fun_pat_list_expr expr = pipe2 (fun_ >>. pattern_list) (lam >>. expr) (fun pattern body -> meth' pattern body)
let case_inl_rec_name_pat_list_expr expr = pipe3 (inl_rec >>. name) pattern_list (lam >>. expr) (fun name pattern body -> inlr' name pattern body)
let case_fun_rec_name_pat_list_expr expr = pipe3 (fun_rec >>. name) pattern_list (lam >>. expr) (fun name pattern body -> methr' name pattern body)

let case_lit expr = lit
let case_if_then_else expr = if_then_else expr 
let case_rounds expr = rounds (expr <|>% B)
let case_named_tuple expr = 
    pipe2 (dot >>. type_name)
        (case_rounds expr)
        (fun name -> function
            | VV (args, "") -> VV(args,name)
            | args -> VV([args],name))
let case_var expr = var

let case_typecase expr (s: CharStream<_>) =
    let i = s.Column
    let expr_indent expr (s: CharStream<_>) = expr_indent i (<=) expr s
    let pat_body = expr_indent patterns .>> expr_indent lam
    let pat_first = expr_indent (optional bar) >>. pat_body
    let pat = expr_indent bar >>. pat_body
    let body = expr_indent expr
    let case_case pat = pipe2 pat body (fun a b -> a,b)
    pipe3 (typecase >>. expr .>> with_)
        (case_case pat_first)
        (many (case_case pat))
        (fun e x xs -> match_ e (x :: xs)) s

let case_module expr = module_ >>% module_create

let case_apply_type expr = grave >>. expr |>> ap_ty
let case_apply_module expr = dot >>. var_name |>> ap_mod

let expressions expr =
    [case_inl_pat_list_expr; case_fun_pat_list_expr; case_inl_rec_name_pat_list_expr; case_fun_rec_name_pat_list_expr
     case_lit; case_if_then_else; case_rounds; case_var; case_named_tuple; case_typecase; case_module
     case_apply_type; case_apply_module]
    |> List.map (fun x -> x expr |> attempt)
    |> choice
 
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
    | _ -> preturn B

let indentations statements expressions (s: CharStream<_>) =
    let i = s.Column
    let inline if_ op tr (s: CharStream<_>) = expr_indent i op tr s
    let expr_indent expr =
        let mutable op = (=)
        let set_op op' x = op <- op'; x
        let semicolon s = if_ (<) (semicolon |>> set_op (<=)) s
        let expr s = if_ op (expr |>> set_op (=)) s
        many1 (expr .>> optional semicolon)
    expr_indent ((statements |>> ParserStatement) <|> (expressions i |>> ParserExpr)) >>= process_parser_exprs <| s

let application expr (s: CharStream<_>) =
    let i = s.Column
    let expr_up expr (s: CharStream<_>) = expr_indent i (<) expr s
    let ap_cr = expr |>> flip ap |> expr_up
    
    let f a l = List.fold (fun s x -> x s) a l
    pipe2 expr (many ap_cr) f s

let tuple expr i (s: CharStream<_>) =
    let expr_indent expr (s: CharStream<_>) = expr_indent i (<=) expr s
    sepBy1 (expr_indent (expr i)) (expr_indent comma)
    |>> function
        | _ :: _ :: _ as l -> vv l
        | x :: _ -> x
        | _ -> failwith "impossible"
    <| s


let mset expr i (s: CharStream<_>) = 
    let expr_indent expr (s: CharStream<_>) = expr_indent i (<) expr s
    pipe2 (expr i)
        (opt (expr_indent set_me >>. expr_indent (fun (s: CharStream<_>) -> expr s.Column s)))
        (fun l -> function
            | Some r -> Op(MSet,[l;r])
            | None -> l) s

let expr: Parser<_,unit> =
    let dict_operator = d0()
    let add_infix_operator assoc str prec op = dict_operator.Add(str, (prec, assoc, fun x y -> op x y))

    let binop op a b = Op(op,[a;b])

    let left_assoc_ops = 
        let f = add_infix_operator Associativity.Left
        let apply a b = binop Apply a b
        f "+" 60 <| binop Add
        f "-" 60 <| binop Sub
        f "*" 70 <| binop Mult
        f "/" 70 <| binop Div

        f "<|" 10 apply
        f "|>" 10 (flip apply)
        let compose a b = inl (S "x") (apply a (apply b (V "x")))
        f "<<" 10 compose
        f ">>" 10 (flip compose)

    let no_assoc_ops =
        let f str op = add_infix_operator Associativity.None str 40 (binop op)
        f "<=" LTE; f "<" LT; f "=" EQ; f ">" GT; f ">=" GTE

    let right_associative_ops =
        let f str prec op = add_infix_operator Associativity.Right str prec (binop op)
        f "||" 20 Or
        f "&&" 30 And
        f "::" 50 VVCons

    let poperator: Parser<_,_> =
        let f c = (isAsciiIdContinue c || isAnyOf [|' ';'\t';'\n';'\"';'(';')';'{';'}';'[';']'|] c) = false
        (many1Satisfy f .>> spaces)
        >>= fun token ->
            match dict_operator.TryGetValue token with
            | true, x -> preturn x
            | false, _ -> fail "unknown operator"

    let rec led poperator term left (prec,asoc,m) =
        match asoc with
        | Associativity.Left | Associativity.None -> tdop poperator term prec |>> m left
        | Associativity.Right -> tdop poperator term (prec-1) |>> m left
        | _ -> failwith "impossible"

    and tdop poperator term rbp =
        let rec f left =
            poperator >>= fun (prec,asoc,m as v) ->
                if rbp < prec then led poperator term left v >>= loop
                else pzero
        and loop left = attempt (f left) <|>% left
        term >>= loop

    let operators expr i (s: CharStream<_>) =
        let expr_indent expr (s: CharStream<_>) = expr_indent i (<=) expr s
        let op s = expr_indent poperator s
        let term s = expr_indent expr s
        tdop op term 0 s

    let rec expr s = indentations (statements expr) (mset (tuple (operators (application (expressions expr))))) s

    expr

let spiral_parse x = run (spaces >>. expr .>> eof) x
