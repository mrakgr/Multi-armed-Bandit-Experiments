#load "SpiralV5Language_v10b.fsx"
#r "../../packages/FParsec.1.0.2/lib/net40-client/FParsecCS.dll"
#r "../../packages/FParsec.1.0.2/lib/net40-client/FParsec.dll"

open SpiralV5Language_v10b
open FParsec

type ParserExpr =
| ParserStatement of (Expr -> Expr)
| ParserExpr of Expr

type Pattern =
| E
| PatVar of Pos * string
| PatTuple of Pos * Pattern list
| PatCons of Pos * Pattern list
| PatType of Pos * (Pattern * Pattern)
| PatActive of Pos * (string * Pattern)
| PatOr of Pos * Pattern list
| PatAnd of Pos * Pattern list
| PatClauses of Pos * (Pattern * Expr) list
| PatNameT of Pos * string

let pos (s: CharStream<_>) = Some (s.Name, s.Line, s.Column)
let ppos s = Reply(pos s)
let pos_lit pos x = Lit(x,pos)

let spaces s =
    let rec spaces' s: Reply<unit> = (spaces >>. optional (followedByString "//" >>. skipRestOfLine true >>. spaces')) s
    spaces' s
    
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
let pat_var pattern = tuple2 ppos var_name |>> PatVar
let pat_tuple pattern = tuple2 ppos (pat_tuple' pattern) |>> PatTuple
let pat_cons pattern = tuple2 ppos (patid ''' >>. pat_tuple' pattern) |>> PatCons
let pat_type pattern = tuple2 ppos (patid '?' >>. tuple2 pattern pattern) |>> PatType
let pat_active pattern = tuple2 ppos (patid '^' >>. tuple2 var_name pattern) |>> PatActive
let pat_or pattern = tuple2 ppos (patid '|' >>. pat_tuple' pattern) |>> PatOr
let pat_and pattern = tuple2 ppos (patid '&' >>. pat_tuple' pattern) |>> PatAnd
let pat_type_string pattern = tuple2 ppos (patid '.' >>. var_name) |>> PatNameT

let rec patterns (s: CharStream<_>) =
    [|
    pat_e; pat_var; pat_tuple; pat_cons; pat_type
    pat_active; pat_or; pat_and; pat_type_string
    |] 
    |> Array.map (fun x -> x patterns)
    |> choice
    |> fun p -> p s

let pattern_list = many patterns

let pattern_compile arg pat =
    let rec pattern_compile flag_is_var_type arg pat (on_succ: Lazy<_>) (on_fail: Lazy<_>) =
        let inline cp' arg pat on_succ on_fail = pattern_compile flag_is_var_type arg pat on_succ on_fail
        let inline cp arg pat on_succ on_fail = lazy cp' arg pat on_succ on_fail

        let pat_foldbacki f s l =
            let mutable len = 0
            let rec loop i l =
                match l with
                | x :: xs -> f (x,i) (loop (i+1) xs)
                | [] -> len <- i; s
            loop 0 l, len
            
        let pat_tuple pos l =
            pat_foldbacki
                (fun (pat,i) on_succ ->
                    let arg = tuple_index arg i pos
                    cp arg pat on_succ on_fail)
                on_succ
                l
            |> fun (on_succ,len) -> case_tuple arg (lit_int len pos) on_succ.Value on_fail.Value pos

        let pat_cons pos l = 
            pat_foldbacki
                (fun (pat,i) (on_succ, tuple_index') ->
                    let arg = tuple_index' arg i pos
                    cp arg pat on_succ on_fail, tuple_index)
                (on_succ, tuple_slice_from)
                l
            |> fun ((on_succ,_),len) -> case_cons arg (lit_int len pos) on_succ.Value on_fail.Value pos

        let inline force (x: Lazy<_>) = x.Value

        let pat_or pos l = List.foldBack (fun pat on_fail -> cp arg pat on_succ on_fail) l on_fail |> force
        let pat_and pos l = List.foldBack (fun pat on_succ -> cp arg pat on_succ on_fail) l on_succ |> force
        let pat_clauses pos l = List.foldBack (fun (pat, exp) on_fail -> cp arg pat (lazy exp) on_fail) l on_fail |> force

        match pat with
        | E -> on_succ.Value
        | PatVar (pos, x) -> 
            if flag_is_var_type then if_static (eq_type arg (V (x, pos)) pos) on_succ.Value on_fail.Value pos
            else l x arg pos on_succ.Value
        | PatTuple (pos, l) -> pat_tuple pos l
        | PatCons (pos, l) -> pat_cons pos l
        | PatType (pos,(typ,exp)) ->
            let on_succ = cp arg exp on_succ on_fail
            pattern_compile true arg typ on_succ on_fail
        | PatActive (pos,(a,b)) ->
            let v x = V (x, pos)
            cp' (ap pos (v a) arg) b on_succ on_fail
        | PatOr (pos, l) -> pat_or pos l
        | PatAnd (pos, l) -> pat_and pos l
        | PatClauses (pos, l) -> pat_clauses pos l
        | PatNameT (pos, x) ->
            let x = type_lit_create pos (LitString x)
            if_static (eq_type arg x pos) on_succ.Value on_fail.Value pos

    let pattern_compile_def_on_succ = lazy failwith "Missing a clause."
    let pattern_compile_def_on_fail = lazy error_type (Lit(LitString "Pattern matching cases are inexhaustive", None))
    pattern_compile false arg pat pattern_compile_def_on_succ pattern_compile_def_on_fail

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

let main_arg = " main_arg" // Note the empty space at the start. This variable name can only be used by the compiler.

let pattern_compile_single name pattern = inlr name main_arg (pattern_compile (V(main_arg,None)) pattern) None
let with_clause pos pattern els = PatClauses(pos,[pattern,els])

let l pattern body pos els =
    let v x = V(x,pos)
    ap pos (pattern_compile_single "" (with_clause pos pattern els)) body
let S p name = PatVar(p,name)

let rec inlr' name (args: Pattern list) body pos =
    let f arg body = pattern_compile_single name (with_clause pos arg body)
    match args with
    | x :: xs -> f x (inlr' "" xs body pos)
    | [] -> body
        
let methr' name args body pos = inlr' name args (meth_memo body) pos
    
let case_inl_pat_statement expr s = let p = pos s in pipe2 (inl_ >>. patterns) (eq >>. expr) (fun pattern body -> l pattern body p) s
let case_inl_name_pat_list_statement expr s = let p = pos s in pipe3 (inl_ >>. name) pattern_list (eq >>. expr) (fun name pattern body -> l (S p name) (inlr' "" pattern body p) p) s
let case_inl_rec_name_pat_list_statement expr s = let p = pos s in pipe3 (inl_rec >>. name) pattern_list (eq >>. expr) (fun name pattern body -> l (S p name) (inlr' name pattern body p) p) s
let case_met_pat_statement expr s = let p = pos s in pipe2 (met_ >>. patterns) (eq >>. expr) (fun pattern body -> l pattern (meth_memo body) p) s
let case_met_name_pat_list_statement expr s = let p = pos s in pipe3 (met_ >>. name) pattern_list (eq >>. expr) (fun name pattern body -> l (S p name) (methr' "" pattern body p) p) s
let case_met_rec_name_pat_list_statement expr s = let p = pos s in pipe3 (met_rec >>. name) pattern_list (eq >>. expr) (fun name pattern body -> l (S p name) (methr' name pattern body p) p) s
let case_open expr s = let p = pos s in (open_ >>. expr |>> module_open p) s

let statements expr = 
    [case_inl_pat_statement; case_inl_name_pat_list_statement; case_inl_rec_name_pat_list_statement
     case_met_pat_statement; case_met_name_pat_list_statement; case_met_rec_name_pat_list_statement; case_open]
    |> List.map (fun x -> x expr |> attempt)
    |> choice

let case_inl_pat_list_expr expr s = let p = pos s in pipe2 (inl_ >>. pattern_list) (lam >>. expr) (fun pattern body -> inlr' "" pattern body p) s
let case_met_pat_list_expr expr s = let p = pos s in pipe2 (met_ >>. pattern_list) (lam >>. expr) (fun pattern body -> methr' "" pattern body p) s

let case_lit expr = lit
let case_if_then_else expr = if_then_else expr 
let case_rounds expr = rounds (expr <|>% B)
let case_var expr = var

let inline case_typex match_type expr (s: CharStream<_>) =
    let mutable i = None
    let p = pos s
    let expr_indent op expr (s: CharStream<_>) = expr_indent i.Value op expr s
    let pat_body = expr_indent (<=) patterns
    let pat = 
        let pat_rec = barbar >>. many1 pat_body |>> fun x -> true, x
        let pat = bar >>. many1 pat_body |>> fun x -> false, x
        expr_indent (<=) (pat_rec <|> pat) .>> lam
            
    let clause pat = tuple2 pat expr
    let set_col (s: CharStream<_>) = i <- Some (s.Column); Reply(())

    let with_clauses pos l = 
        List.map (function
            | (is_rec, x :: xs), body -> x, if is_rec then methr' "" xs body None else inlr' "" xs body None
            | _ -> failwith "impossible"
            ) l
        |> fun l -> PatClauses(pos,l)
    let pat_function l = pattern_compile_single "" (with_clauses p l)
    let pat_match x l = ap p (pat_function l) x

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

let case_module expr s = let p = pos s in (module_ >>% module_create p) s
let case_apply_type expr s = let p = pos s in (grave >>. expr |>> ap_ty p) s
let case_string_ty expr s = let p = pos s in keywordChar '.' >>. var_name |>> (LitString >> type_lit_create p) <| s

let expressions expr (s: CharStream<_>) =
    ([case_inl_pat_list_expr; case_met_pat_list_expr; case_apply_type; case_string_ty
      case_lit; case_if_then_else; case_rounds; case_var; case_typecase; case_typeinl; case_module
      ]
    |> List.map (fun x -> x expr |> attempt)
    |> choice) s
 
let process_parser_exprs exprs = 
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
        let compose a b pos = inl "x" (apply a (apply b (V ("x",pos)) pos) pos) pos
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

    let rec expr s = annotations (indentations (statements expr) (mset (tuple (operators (application (expressions expr)))))) s
    expr

let spiral_parse (name, code) = runParserOnString (spaces >>. expr .>> eof) () name code
