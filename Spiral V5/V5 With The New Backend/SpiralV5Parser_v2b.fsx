﻿#load "SpiralV5Language_v9c.fsx"
#r "../../packages/FParsec.1.0.2/lib/net40-client/FParsecCS.dll"
#r "../../packages/FParsec.1.0.2/lib/net40-client/FParsec.dll"

open SpiralV5Language_v9c
open FParsec

type ParserExpr =
| ParserStatement of (Expr -> Expr)
| ParserExpr of Expr

let spaces s = 
    let rec spaces' s: Reply<unit> = (spaces >>. optional (followedByString "//" >>. skipRestOfLine true >>. spaces')) s
    spaces' s

let comma = skipChar ',' .>> spaces
let dot = skipChar '.' >>. spaces
let grave = skipChar '`' >>. spaces
let underscore = skipChar '_' .>> spaces
let pp = skipChar ':' .>> spaces
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
let typeinl = skipString "typeinl" >>. spaces
let module_ = skipString "module" >>. spaces
let with_ = skipString "with" >>. spaces

let with_for_module = 
    skipString "mwith" >>. 
        ((skipChar '!' >>. spaces >>% ModuleWith)
        <|> (spaces >>% ModuleWith'))
let open_ = skipString "open" >>. spaces

let isAsciiIdStart c = isAsciiLetter c || c = '_'
let isAsciiIdContinue c = isAsciiLetter c || isDigit c || c = '_' || c = '''

let var_name = 
    many1Satisfy2L isAsciiLower isAsciiIdContinue "identifier" .>> spaces
    >>=? function
        | "match" | "function" | "mwith" | "with" | "open" | "module" 
        | "typecase" | "typeinl" | "rec" | "if" | "then" | "else" | "inl" | "fun" as x -> 
            fun _ -> Reply(Error,messageError <| sprintf "%s not allowed as an identifier." x)
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

let pattern_tuple' pattern = 
    let f = function
        | [last] -> preturn last
        | last :: rest -> R(List.rev rest, Some last) |> preturn
        | _ -> failwith "impossible"
    sepBy1 pattern pppp 
    >>= (List.rev >> f)

let pattern_tuple pattern = sepBy1 pattern comma |>> function [x] -> x | x -> SS x
let pattern_tuple_rounds pattern = rounds (sepBy pattern comma) |>> function [x] -> x | x -> SS x
let pattern_tuple_rounds' pattern = rounds (sepBy pattern comma) |>> SS

let rec pattern_type_name s = 
    pipe2
        (dot >>. type_name)
        pattern_rounds'
        (fun nam pat -> N(nam,pat)) s

and pattern_active s = 
    pipe2 (dot >>. var_name)
        (pattern_rounds <|> pattern_identifier)
        (fun name pattern -> F(pattern, name)) s

and pattern_cases s = choice [pattern_identifier; pattern_type_name; pattern_active; pattern_rounds] s
and pattern_template pattern_tuple s = pattern_tuple (pattern_tuple' pattern_cases) s
and pattern_rounds s = pattern_template pattern_tuple_rounds s
and pattern_rounds' s = pattern_template pattern_tuple_rounds' s

let patterns s = pattern_template pattern_tuple s

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

let pos (s: CharStream<_>) = Some (s.Name, s.Line, s.Column)
let pos_lit pos x = Lit(x,pos)

let lit s = 
    choice 
        [|
        pbool
        pnumber .>> notFollowedBy (satisfy isAsciiIdContinue)
        quoted_string
        |] |>> pos_lit (pos s)  .>> spaces
    <| s
    
let pos_var pos x = V(x,pos)
let var s = var_name |>> pos_var (pos s) <| s

let pos_op x v s = Op(x,v,pos s) |> Reply
let expr_indent i op expr (s: CharStream<_>) = if op i s.Column then expr s else pzero s
let if_then_else expr (s: CharStream<_>) =
    let i = s.Column
    let pos = pos s
    let expr_indent expr (s: CharStream<_>) = expr_indent i (<=) expr s
    pipe3
        (skipString "if" >>. spaces1 >>. expr)
        (expr_indent (skipString "then" >>. spaces1 >>. expr))
        (opt (expr_indent (skipString "else" >>. spaces1 >>. expr)))
        (fun cond tr fl -> 
            let fl = match fl with Some x -> x | None -> B
            Op(If,[cond;tr;fl],pos)
            )
        s

let name = var_name

let case_inl_pat_statement expr s = let p = pos s in pipe2 (inl_ >>. patterns) (eq >>. expr) (fun pattern body -> l pattern body p) s
let case_inl_name_pat_list_statement expr s = let p = pos s in pipe3 (inl_ >>. name) pattern_list (eq >>. expr) (fun name pattern body -> l (S name) (inlr' "" pattern body p) p) s
let case_inl_rec_name_pat_list_statement expr s = let p = pos s in pipe3 (inl_rec >>. name) pattern_list (eq >>. expr) (fun name pattern body -> l (S name) (inlr' name pattern body p) p) s
let case_fun_name_pat_list_statement expr s = let p = pos s in pipe3 (fun_ >>. name) pattern_list (eq >>. expr) (fun name pattern body -> l (S name) (methr' "" pattern body p) p) s
let case_fun_rec_name_pat_list_statement expr s = let p = pos s in pipe3 (fun_rec >>. name) pattern_list (eq >>. expr) (fun name pattern body -> l (S name) (methr' name pattern body p) p) s
let case_open expr s = let p = pos s in (open_ >>. expr |>> module_open p) s

let statements expr = 
    [case_inl_pat_statement; case_inl_name_pat_list_statement; case_inl_rec_name_pat_list_statement
     case_fun_name_pat_list_statement; case_fun_rec_name_pat_list_statement; case_open]
    |> List.map (fun x -> x expr |> attempt)
    |> choice

let case_inl_pat_list_expr expr s = let p = pos s in pipe2 (inl_ >>. pattern_list) (lam >>. expr) (fun pattern body -> inl' pattern body p) s
let case_fun_pat_list_expr expr s = let p = pos s in pipe2 (fun_ >>. pattern_list) (lam >>. expr) (fun pattern body -> meth' pattern body p) s
let case_inl_rec_name_pat_list_expr expr s = let p = pos s in pipe3 (inl_rec >>. name) pattern_list (lam >>. expr) (fun name pattern body -> inlr' name pattern body p) s
let case_fun_rec_name_pat_list_expr expr s = let p = pos s in pipe3 (fun_rec >>. name) pattern_list (lam >>. expr) (fun name pattern body -> methr' name pattern body p) s

let case_lit expr = lit
let case_if_then_else expr = if_then_else expr 
let case_rounds expr = rounds (expr <|>% B)
let case_named_tuple expr s = 
    let p = pos s
    pipe2 (dot >>. type_name)
        (case_rounds expr)
        (fun name -> function
            | VV (args, "", p) -> VV(args,name,p)
            | args -> VV([args],name,p)) s
let case_var expr = var

let inline case_typex match_type expr (s: CharStream<_>) =
    let mutable i = None
    let p = pos s
    let expr_indent op expr (s: CharStream<_>) = expr_indent i.Value op expr s
    let pat_body = expr_indent (<=) patterns .>> expr_indent (<=) lam
    let pat_first = expr_indent (<=) (optional bar) >>. pat_body
    let pat = expr_indent (<=) bar >>. pat_body
    let case_case pat = pipe2 pat expr (fun a b -> a,b)
    let set_col (s: CharStream<_>) = i <- Some (s.Column); Reply(())
    match match_type with
    | true -> // typeinl
        pipe2 (typeinl >>. set_col >>. case_case pat_first)
            (many (case_case pat))
            (fun x xs -> function_ (x :: xs) p) s    
    | false -> // typecase
        pipe3 (typecase >>. expr .>> with_ .>> set_col)
            (case_case pat_first)
            (many (case_case pat))
            (fun e x xs -> match_ e (x :: xs) p) s

let case_typeinl expr (s: CharStream<_>) = case_typex true expr s
let case_typecase expr (s: CharStream<_>) = case_typex false expr s

let case_module expr s = let p = pos s in (module_ >>% module_create p) s

let case_apply_type expr s = let p = pos s in (grave >>. expr |>> ap_ty p) s
let case_apply_module expr s = let p = pos s in (dot >>. var_name |>> ap_mod p) s

let expressions expr (s: CharStream<_>) =
    ([case_inl_pat_list_expr; case_fun_pat_list_expr; case_inl_rec_name_pat_list_expr; case_fun_rec_name_pat_list_expr
      case_lit; case_if_then_else; case_rounds; case_var; case_named_tuple; case_typecase; case_typeinl; case_module
      case_apply_type; case_apply_module]
    |> List.map (fun x -> x expr |> attempt)
    |> choice) s
 
let process_parser_exprs loop_initializer exprs = 
    let error_statement_in_last_pos _ = Reply(Error,messageError "Statements not allowed in the last position of a block.")
    let process_parser_expr a b on_fail on_succ =
        match a,b with
        | ParserStatement a, ParserExpr b -> a b |> ParserExpr |> on_succ
        | ParserExpr a, ParserExpr b -> l E (error_non_unit a) (get_pos a) b |> ParserExpr |> on_succ
        | _, ParserStatement _ -> on_fail error_statement_in_last_pos

    let process_parser_exprs l =
        let rec loop = function
            | b :: a :: xs -> process_parser_expr a b id (fun r -> loop (r :: xs))
            | x :: xs ->
                match x with
                | ParserExpr a -> preturn a 
                | ParserStatement a -> error_statement_in_last_pos
            | [] -> failwith "impossible"
        loop (loop_initializer (List.rev l))

    match exprs with
    | _ :: _ -> process_parser_exprs exprs
    | _ -> preturn B

let indentations statements expressions process_parser_exprs (s: CharStream<_>) =
    let i = s.Column
    let inline if_ op tr (s: CharStream<_>) = expr_indent i op tr s
    let expr_indent expr =
        let mutable op = (=)
        let set_op op' x = op <- op'; x
        let semicolon s = if_ (<) (semicolon |>> set_op (<=)) s
        let expr s = if_ op (expr |>> set_op (=)) s
        many1 (expr .>> optional semicolon)
    expr_indent ((statements |>> ParserStatement) <|> (expressions i |>> ParserExpr)) >>= process_parser_exprs <| s

let ppos s = Reply(pos s)

let application expr (s: CharStream<_>) =
    let i = s.Column
    let expr_up expr (s: CharStream<_>) = expr_indent i (<) expr s
    let ap_cr = pipe2 ppos expr (fun pos x y -> ap pos y x) |> expr_up
    
    let f a l = List.fold (fun s x -> x s) a l
    pipe2 expr (many ap_cr) f s

let tuple expr i (s: CharStream<_>) =
    let expr_indent expr (s: CharStream<_>) = expr_indent i (<=) expr s
    sepBy1 (expr_indent (expr i)) (expr_indent comma)
    |>> function
        | x :: _ :: _ as l -> vv (get_pos x) l
        | x :: _ -> x
        | _ -> failwith "impossible"
    <| s

let mset expr i (s: CharStream<_>) = 
    let expr_indent expr (s: CharStream<_>) = expr_indent i (<) expr s
    let p = pos s
    pipe2 (expr i)
        (opt (expr_indent set_me >>. expr_indent (fun (s: CharStream<_>) -> expr s.Column s)))
        (fun l -> function
            | Some r -> Op(MSet,[l;r],p)
            | None -> l) s

let annotations expr (s: CharStream<_>) = 
    let i = s.Column
    let expr_indent expr (s: CharStream<_>) = expr_indent i (<=) expr s
    pipe2 (expr_indent expr) (opt (ppos .>> expr_indent pp .>>. expr_indent expr))
        (fun a -> function
            | Some(p,b) -> Op(TypeAnnot,[a;b],p)
            | None -> a) s

let module_with expr (s: CharStream<_>) = 
    let i = s.Column
    let expr_indent expr (s: CharStream<_>) = expr_indent i (<=) expr s
    let init_module_create = process_parser_exprs <| fun l -> ParserExpr (module_create None) :: l
    let init_no = process_parser_exprs id
    pipe2 (expr_indent (expr init_no)) (opt (tuple3 ppos (expr_indent with_for_module) (expr_indent (expr init_module_create))))
        (fun a -> function
            | Some(p,op_with,b) -> Op(op_with,[a;b],p)
            | None -> a) s

let expr: Parser<_,unit> =
    let dict_operator = d0()
    let add_infix_operator assoc str prec op = dict_operator.Add(str, (prec, assoc, fun x y -> op x y))

    let binop op a b pos = Op(op,[a;b],pos)

    let left_assoc_ops = 
        let f = add_infix_operator Associativity.Left
        let apply a b = binop Apply a b
        f "+" 60 <| binop Add
        f "-" 60 <| binop Sub
        f "*" 70 <| binop Mult
        f "/" 70 <| binop Div

        f "<|" 10 apply
        f "|>" 10 (flip apply)
        let compose a b pos = inl (S "x") (apply a (apply b (V ("x",pos)) pos) pos) pos
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
        let p = pos s
        (many1Satisfy f .>> spaces)
        >>= fun token ->
            match dict_operator.TryGetValue token with
            | true, (prec,asoc,m) -> preturn (prec,asoc,fun a b -> m a b p)
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

    let rec expr s = annotations (module_with (indentations (statements expr) (mset (tuple (operators (application (expressions expr))))))) s
    expr

let spiral_parse (name, code) = runParserOnString (spaces >>. expr .>> eof) () name code
