#load "SpiralV5Language_v10c.fsx"
#r "../../packages/FParsec.1.0.2/lib/net40-client/FParsecCS.dll"
#r "../../packages/FParsec.1.0.2/lib/net40-client/FParsec.dll"

open SpiralV5Language_v10c
open FParsec

type ParserExpr =
| ParserStatement of (Expr -> Expr)
| ParserExpr of Expr

let pos' (s: CharStream<_>) = s.Name, s.Line, s.Column
let pos expr (s: CharStream<_>) = 
    (expr |>> pos (pos' s)) s

let patpos expr (s: CharStream<_>) = 
    let p = pos' s
    (expr |>> fun expr -> PatPos(p, expr)) s

let rec spaces_template spaces s = spaces >>. optional (followedByString "//" >>. skipRestOfLine true >>. spaces_template spaces) <| s
let spaces, spaces1 = spaces_template spaces, spaces_template spaces1
    
let isAsciiIdStart c = isAsciiLetter c || c = '_'
let isAsciiIdContinue c = isAsciiLetter c || isDigit c || c = '_' || c = '''

let var_name =
    many1Satisfy2L isAsciiIdStart isAsciiIdContinue "identifier" .>> spaces
    >>=? function
        | "match" | "function" | "with" | "open" | "module" 
        | "rec" | "if" | "then" | "else" | "inl" | "met" | "type" as x -> 
            fun _ -> Reply(Error,messageError <| sprintf "%s not allowed as an identifier." x)
        | x -> preturn x

let between_brackets l p r = between (skipChar l .>> spaces) (skipChar r .>> spaces) p
let rounds p = between_brackets '(' p ')'
let curlies p = between_brackets '{' p '}'
let quares p = between_brackets '[' p ']'

let patid c = skipChar c >>. spaces
let pat_tuple' pattern = rounds (many pattern)

let pat_e pattern = patid '_' >>% E
let pat_var pattern = var_name |>> PatVar
let pat_tuple pattern = pat_tuple' pattern |>> PatTuple
let pat_cons pattern = patid ''' >>. pat_tuple' pattern |>> PatCons
let pat_type pattern = patid '?' >>. tuple2 pattern pattern |>> PatType
let pat_active pattern = patid '^' >>. tuple2 var_name pattern |>> PatActive
let pat_or pattern = patid '|' >>. pat_tuple' pattern |>> PatOr
let pat_and pattern = patid '&' >>. pat_tuple' pattern |>> PatAnd
let pat_type_string pattern = patid '.' >>. var_name |>> PatTypeName

let rec patterns (s: CharStream<_>) =
    [|
    pat_e; pat_var; pat_tuple; pat_cons; pat_type
    pat_active; pat_or; pat_and; pat_type_string
    |] 
    |> Array.map (fun x -> x patterns)
    |> choice |> patpos <| s

let pattern_list = many patterns

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
        let f c l = 
            let l = Array.map (fun (k,m) -> skipString k |>> fun _ -> m x) l
            skipChar c >>. choice l
        choice
            [|
            f 'i'
                [|
                "8", int8 >> LitInt8
                "16", int16 >> LitInt16
                "32", int32 >> LitInt32
                "64", int64 >> LitInt64
                |]

            f 'u'
                [|
                "8", uint8 >> LitUInt8
                "16", uint16 >> LitUInt16
                "32", uint32 >> LitUInt32
                "64", uint64 >> LitUInt64
                |]

            f 'f'
                [|
                "32", float32 >> LitFloat32
                "64", float >> LitFloat64
                |]
            default_ x
            |]

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

let quoted_string =
    let normalChar = satisfy (fun c -> c <> '\\' && c <> '"')
    let unescape c = match c with
                     | 'n' -> '\n'
                     | 'r' -> '\r'
                     | 't' -> '\t'
                     | c   -> c
    let escapedChar = pstring "\\" >>. (anyOf "\\nrt\"" |>> unescape)
    between (pstring "\"") (pstring "\"")
            (manyChars (normalChar <|> escapedChar))
    |>> LitString

let lit = 
    choice 
        [|
        pbool
        pnumber .>> notFollowedBy (satisfy isAsciiIdContinue)
        quoted_string
        |] |>> Lit .>> spaces
    
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
    <| s

let name = var_name

let keywordChar = patid
let keywordString x = skipString x .>> spaces
let keywordString1 x = skipString x .>> spaces1

let comma = keywordChar ',' 
let grave = keywordChar '`' 
let pp = keywordChar ':'
let semicolon = keywordChar ';' 
let eq = keywordChar '=' 
let bar = keywordChar '|' 
let barbar = keywordString "||" 
let lam = keywordString "->"
let set_me = keywordString "<-"
let inl_ = keywordString "inl"
let met_ = keywordString "met"
let inl_rec = keywordString1 "inl" .>> keywordString "rec"
let met_rec = keywordString1 "met" .>> keywordString "rec"
let match_ = keywordString "match"
let function_ = keywordString "function"
let module_ = keywordString "module"
let with_ = keywordString "with"
let open_ = keywordString "open"

let inl_pat' (args: Pattern list) body = List.foldBack inl_pat args body
let meth_pat' args body = inl_pat' args (meth_memo body)
    
let case_inl_pat_statement expr = pipe2 (inl_ >>. patterns) (eq >>. expr) lp
let case_inl_name_pat_list_statement expr = pipe3 (inl_ >>. name) pattern_list (eq >>. expr) (fun name pattern body -> l name (inl_pat' pattern body)) 
let case_inl_rec_name_pat_list_statement expr = pipe3 (inl_rec >>. name) pattern_list (eq >>. expr) (fun name pattern body -> l_rec name (inl_pat' pattern body))

let case_met_pat_statement expr = pipe2 (met_ >>. patterns) (eq >>. expr) (fun pattern body -> lp pattern (meth_memo body))
let case_met_name_pat_list_statement expr = pipe3 (met_ >>. name) pattern_list (eq >>. expr) (fun name pattern body -> l name (meth_pat' pattern body))
let case_met_rec_name_pat_list_statement expr = pipe3 (met_rec >>. name) pattern_list (eq >>. expr) (fun name pattern body -> l_rec name (meth_pat' pattern body))

let case_open expr = open_ >>. expr |>> module_open

let statements expr = 
    [case_inl_pat_statement; case_inl_name_pat_list_statement; case_inl_rec_name_pat_list_statement
     case_met_pat_statement; case_met_name_pat_list_statement; case_met_rec_name_pat_list_statement; case_open]
    |> List.map (fun x -> x expr |> attempt)
    |> choice

let case_inl_pat_list_expr expr = pipe2 (inl_ >>. pattern_list) (lam >>. expr) inl_pat'
let case_met_pat_list_expr expr = pipe2 (met_ >>. pattern_list) (lam >>. expr) meth_pat'

let case_lit expr = lit
let case_if_then_else expr = if_then_else expr 
let case_rounds expr = rounds (expr <|>% B)
let case_var expr = var

let inline case_typex match_type expr (s: CharStream<_>) =
    let mutable i = None
    let expr_indent op expr (s: CharStream<_>) = expr_indent i.Value op expr s
    let pat_body = expr_indent (<=) patterns
    let pat = 
        let pat_meth = barbar >>. many1 pat_body |>> fun x -> true, x
        let pat_inl = bar >>. many1 pat_body |>> fun x -> false, x
        expr_indent (<=) (pat_meth <|> pat_inl) .>> lam
            
    let clause pat = tuple2 pat expr
    let set_col (s: CharStream<_>) = i <- Some (s.Column); Reply(())

    let with_clauses l = 
        List.map (function
            | (is_meth, x :: xs), body -> x, if is_meth then meth_pat' xs body else inl_pat' xs body
            | _ -> failwith "impossible"
            ) l
        |> PatClauses
    let pat_function l = Pattern (with_clauses l)
    let pat_match x l = ap (pat_function l) x

    match match_type with
    | true -> // function
        (function_ >>. set_col >>. many1 (clause pat)
        |>> pat_function) s    
    | false -> // match
        pipe2 (match_ >>. expr .>> with_ .>> set_col)
            (many1 (clause pat))
            pat_match s

let case_typeinl expr (s: CharStream<_>) = case_typex true expr s
let case_typecase expr (s: CharStream<_>) = case_typex false expr s

let case_module expr = module_ >>% module_create
let case_apply_type expr = grave >>. expr |>> ap_ty
let case_string_ty expr = keywordChar '.' >>. var_name |>> (LitString >> type_lit_create)
let case_type expr = keywordString "type" >>. (var <|> rounds expr) |>> type_create

let expressions expr (s: CharStream<_>) =
    ([case_inl_pat_list_expr; case_met_pat_list_expr; case_apply_type; case_string_ty; case_type
      case_lit; case_if_then_else; case_rounds; case_var; case_typecase; case_typeinl; case_module
      ]
    |> List.map (fun x -> x expr |> attempt)
    |> choice) s
 
let process_parser_exprs exprs = 
    let error_statement_in_last_pos _ = Reply(Error,messageError "Statements not allowed in the last position of a block.")
    let rec process_parser_exprs on_succ = function
        | [ParserExpr a] -> on_succ a
        | [ParserStatement _] -> error_statement_in_last_pos
        | ParserStatement a :: xs -> process_parser_exprs (a >> on_succ) xs
        | ParserExpr a :: xs -> process_parser_exprs (l "" (error_non_unit a) >> on_succ) xs
        | [] -> preturn B
            
    process_parser_exprs preturn exprs

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
        | x :: _ :: _ as l -> vv l
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

let annotations expr (s: CharStream<_>) = 
    let i = s.Column
    let expr_indent expr (s: CharStream<_>) = expr_indent i (<=) expr s
    pipe2 (expr_indent expr) (opt (expr_indent pp >>. expr_indent expr))
        (fun a -> function
            | Some b -> Op(TypeAnnot,[a;b])
            | None -> a) s

let expr: Parser<_,unit> =
    let dict_operator = d0()
    let add_infix_operator assoc str prec op = dict_operator.Add(str, (prec, assoc, op))

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
        let compose a b = inl "x" (apply a (apply b (V "x")))
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

    let poperator s =
        let f c = (isAsciiIdContinue c || isAnyOf [|'.';' ';'\t';'\n';'\"';'(';')';'{';'}';'[';']'|] c) = false
        let p = pos' s
        (many1Satisfy f .>> spaces)
        >>= fun token ->
            match dict_operator.TryGetValue token with
            | true, (prec,asoc,m) -> preturn (prec,asoc,fun a b -> Pos(p,m a b))
            | false, _ -> fail "unknown operator"
        <| s

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

    let rec expr s = pos (annotations (indentations (statements expr) (mset (tuple (operators (application (expressions expr))))))) s
    expr

let spiral_parse (name, code) = runParserOnString (spaces >>. expr .>> eof) () name code
