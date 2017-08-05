module Spiral.Parse

open Spiral.Lang
open FParsec
open System.Collections.Generic

type Userstate = Dictionary<string, int * Associativity>
type ParserExpr =
| ParserStatement of (Expr -> Expr)
| ParserExpr of Expr

let pos' (s: CharStream<_>) = s.Name, s.Line, s.Column
let pos expr (s: CharStream<_>) = (expr |>> pos (pos' s)) s

let patpos expr (s: CharStream<_>) = 
    let p = pos' s
    (expr |>> fun expr -> PatPos(p, expr)) s

let rec spaces_template spaces s = spaces >>. optional (followedByString "//" >>. skipRestOfLine true >>. spaces_template spaces) <| s
let spaces, spaces1 = spaces_template spaces, spaces_template spaces1
    
let is_identifier_starting_char c = isAsciiLetter c || c = '_'
let is_identifier_char c = is_identifier_starting_char c || c = ''' || isDigit c 

let var_name =
    many1Satisfy2L is_identifier_starting_char is_identifier_char "identifier" .>> spaces
    >>=? function
        | "match" | "function" | "with" | "open" | "module" | "as" | "when"
        | "rec" | "if" | "then" | "else" | "inl" | "met" | "true" | "false" as x -> 
            fun _ -> Reply(Error,messageError <| sprintf "%s not allowed as an identifier." x)
        | x -> preturn x

let between_brackets l p r = between (skipChar l .>> spaces) (skipChar r .>> spaces) p
let rounds p = between_brackets '(' p ')'
let curlies p = between_brackets '{' p '}'
let quares p = between_brackets '[' p ']'

let keywordChar x = skipChar x .>> spaces
let keywordString x = skipString x .>> spaces
let keywordString1 x = skipString x .>> spaces1

let when_ = keywordString "when"
let as_ = keywordString "as"
let negate_ = keywordChar '-'
let comma = keywordChar ','
let dot = keywordChar '.'
let grave = keywordChar '`' 
let pp = keywordChar ':'
let semicolon = keywordChar ';' 
let eq = keywordChar '=' 
let bar = keywordChar '|' 
let amphersand = keywordChar '&'
let barbar = keywordString "||" 
let lam = keywordString "->"
let set_ref = keywordString ":="
let set_array = keywordString "<-"
let inl_ = keywordString "inl"
let met_ = keywordString "met"
let inl_rec = keywordString1 "inl" .>> keywordString "rec"
let met_rec = keywordString1 "met" .>> keywordString "rec"
let match_ = keywordString "match"
let function_ = keywordString "function"
let module_ = keywordString "module"
let with_ = keywordString "with"
let open_ = keywordString "open"
let cons = keywordString "::"
let active = keywordChar '^'
let type_' = keywordString "type"
let wildcard = keywordChar '_'

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

let quoted_char = 
    let normalChar = satisfy (fun c -> c <> '\\' && c <> ''')
    let unescape c = match c with
                     | 'n' -> '\n'
                     | 'r' -> '\r'
                     | 't' -> '\t'
                     | c   -> c
    let escapedChar = pchar '\\' >>. (anyOf "\\nrt'" |>> unescape)
    let a = (normalChar <|> escapedChar) .>> pchar ''' |>> LitChar
    let b = pstring "''" >>% LitChar '''
    pchar ''' >>. (a <|> b)

let quoted_string =
    let normalChar = satisfy (fun c -> c <> '\\' && c <> '"')
    let unescape c = match c with
                     | 'n' -> '\n'
                     | 'r' -> '\r'
                     | 't' -> '\t'
                     | c   -> c
    let escapedChar = pchar '\\' >>. (anyOf "\\nrt\"" |>> unescape)
    between (pchar '"') (pchar '"')
            (manyChars (normalChar <|> escapedChar))
    |>> LitString

let lit s = 
    choice 
        [|
        pbool
        pnumber .>> notFollowedBy (satisfy is_identifier_char)
        quoted_string
        quoted_char
        |] .>> spaces
    <| s

let pat_e = wildcard >>% E
let pat_var = var_name |>> PatVar
let pat_tuple pattern = sepBy1 pattern comma |>> function [x] -> x | x -> PatTuple x
let pat_cons pattern = sepBy1 pattern cons |>> function [x] -> x | x -> PatCons x
let pat_rounds pattern = rounds (pattern <|>% PatTuple [])
let pat_type pattern = tuple2 pattern (opt (pp >>. pattern)) |>> function a,Some b -> PatType(a,b) | a, None -> a
let pat_active pattern = (active >>. tuple2 var_name pattern |>> PatActive) <|> pattern
let pat_or pattern = sepBy1 pattern bar |>> function [x] -> x | x -> PatOr x
let pat_and pattern = sepBy1 pattern amphersand |>> function [x] -> x | x -> PatAnd x
let pat_type_lit = dot >>. (lit <|> (var_name |>> LitString)) |>> PatTypeLit
let pat_lit = lit |>> PatLit
let pat_when expr pattern = pattern .>>. (opt (when_ >>. expr)) |>> function a, Some b -> PatWhen(a,b) | a, None -> a
let pat_as pattern = pattern .>>. (opt (as_ >>. pattern )) |>> function a, Some b -> PatAnd [a;b] | a, None -> a

let (^<|) a b = a b // High precedence, right associative <| operator
let rec patterns expr s = // The order the pattern parsers are chained determines their precedence.
    pat_when expr ^<| pat_as ^<| pat_or ^<| pat_tuple ^<| pat_and ^<| pat_type ^<| pat_cons ^<| pat_active 
    ^<| choice [|pat_e; pat_var; pat_type_lit; pat_lit; pat_rounds (patterns expr)|] <| s
    
let pattern_list expr = many (patterns expr)
    
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

let is_operator c = (is_identifier_char c || isAnyOf [|' ';',';'\t';'\n';'\"';'(';')';'{';'}';'[';']'|] c) = false
let poperator (s: CharStream<Userstate>) = many1Satisfy is_operator .>> spaces <| s

let name = var_name <|> rounds poperator

let inl_pat' (args: Pattern list) body = List.foldBack inl_pat args body
let meth_pat' args body = inl_pat' args (meth_memo body)
    
let case_inl_pat_statement expr = pipe2 (inl_ >>. patterns expr) (eq >>. expr) lp
let case_inl_name_pat_list_statement expr = pipe3 (inl_ >>. name) (pattern_list expr) (eq >>. expr) (fun name pattern body -> l name (inl_pat' pattern body)) 
let case_inl_rec_name_pat_list_statement expr = pipe3 (inl_rec >>. name) (pattern_list expr) (eq >>. expr) (fun name pattern body -> l_rec name (inl_pat' pattern body))

let case_met_pat_statement expr = pipe2 (met_ >>. patterns expr) (eq >>. expr) (fun pattern body -> lp pattern (meth_memo body))
let case_met_name_pat_list_statement expr = pipe3 (met_ >>. name) (pattern_list expr) (eq >>. expr) (fun name pattern body -> l name (meth_pat' pattern body))
let case_met_rec_name_pat_list_statement expr = pipe3 (met_rec >>. name) (pattern_list expr) (eq >>. expr) (fun name pattern body -> l_rec name (meth_pat' pattern body))

let case_open expr = open_ >>. expr |>> module_open

let statements expr = 
    [case_inl_pat_statement; case_inl_name_pat_list_statement; case_inl_rec_name_pat_list_statement
     case_met_pat_statement; case_met_name_pat_list_statement; case_met_rec_name_pat_list_statement
     case_open]
    |> List.map (fun x -> x expr |> attempt)
    |> choice

let case_inl_pat_list_expr expr = pipe2 (inl_ >>. pattern_list expr) (lam >>. expr) inl_pat'
let case_met_pat_list_expr expr = pipe2 (met_ >>. pattern_list expr) (lam >>. expr) meth_pat'

let case_lit expr = lit |>> Lit
let case_if_then_else expr = if_then_else expr 
let case_rounds expr s = rounds (expr <|>% B) s
let case_var expr = name |>> V

let inline case_typex match_type expr (s: CharStream<_>) =
    let mutable i = None
    let expr_indent op expr (s: CharStream<_>) = expr_indent i.Value op expr s
    
    let clause = 
        let clause_template is_meth = 
            pipe2 (many1 (patterns expr) .>> lam) expr <| fun pat body ->
                match pat with
                | x :: xs -> x, if is_meth then meth_pat' xs body else inl_pat' xs body
                | _ -> failwith "impossible"

        poperator >>=? function
            | "|" -> clause_template false
            | "||" -> clause_template true
            | _ -> fail "not a pattern matching clause"
        |> expr_indent (<=) 
            
    let set_col (s: CharStream<_>) = i <- Some (s.Column); Reply(())

    let pat_function l = Pattern (PatClauses l)
    let pat_match x l = ap (pat_function l) x

    match match_type with
    | true -> // function
        (function_ >>. set_col >>. many1 clause
        |>> pat_function) s    
    | false -> // match
        pipe2 (match_ >>. expr .>> with_ .>> set_col)
            (many1 clause)
            pat_match s

let case_typeinl expr (s: CharStream<_>) = case_typex true expr s
let case_typecase expr (s: CharStream<_>) = case_typex false expr s

let case_module expr = module_ >>. expr |>> module_create
let case_for_cast expr = grave >>. expr |>> ap (v "for_cast")
let case_lit_lift expr = 
    let var = var_name |>> (LitString >> Lit >> ap (v "lit_lift"))
    let lit = expr |>> ap (v "lit_lift")
    dot >>. (var <|> lit)

let rec expressions expr s =
    let unary_ops = 
        [case_for_cast; case_lit_lift]
        |> List.map (fun x -> x (expressions expr))
        |> choice
    let rest = 
        [case_inl_pat_list_expr; case_met_pat_list_expr; case_lit; case_if_then_else
         case_rounds; case_typecase; case_typeinl; case_module; case_var]
        |> List.map (fun x -> x expr |> attempt)
        |> choice
    unary_ops <|> rest <| s
 
let process_parser_exprs exprs = 
    let error_statement_in_last_pos _ = Reply(Error,messageError "Statements not allowed in the last position of a block.")
    let rec process_parser_exprs on_succ = function
        | [ParserExpr a] -> on_succ a
        | [ParserStatement _] -> error_statement_in_last_pos
        | ParserStatement a :: xs -> process_parser_exprs (a >> on_succ) xs
        | ParserExpr a :: xs -> process_parser_exprs (l "" (error_non_unit a) >> on_succ) xs
        | [] -> preturn B
            
    process_parser_exprs preturn exprs

let indentations statements expressions (s: CharStream<Userstate>) =
    let i = s.Column
    let inline if_ op tr (s: CharStream<_>) = expr_indent i op tr s
    let expr_indent expr =
        let mutable op = (=)
        let set_op op' x = op <- op'; x
        let semicolon s = if_ (<) (semicolon |>> set_op (<=)) s
        let expr s = if_ op (expr |>> set_op (=)) s
        many1 (expr .>> optional semicolon)

    expr_indent ((statements |>> ParserStatement) <|> (expressions |>> ParserExpr)) >>= process_parser_exprs <| s

let application expr (s: CharStream<_>) =
    let i = s.Column
    let expr_up (s: CharStream<_>) = expr_indent i (<) expr s
    
    pipe2 expr (many expr_up) (List.fold ap) s

let tuple expr (s: CharStream<_>) =
    let i = s.Column
    let expr_indent expr (s: CharStream<_>) = expr_indent i (<=) expr s
    sepBy1 (expr_indent expr) (expr_indent comma)
    |>> function [x] -> x | x -> vv x
    <| s

let type_ expr =
    let type_parse (s: CharStream<_>) = 
        let i = s.Column
        let expr_indent expr (s: CharStream<_>) = expr_indent i (=) expr s
        many1 (expr_indent expr) |>> (List.map type_create >> List.reduce type_union >> type_create) <| s
    (type_' >>. type_parse) <|> expr

let mset statements expressions (s: CharStream<_>) = 
    let i = s.Column
    let expr_indent expr (s: CharStream<_>) = expr_indent i (<) expr s
    let op =
        (set_ref >>% fun l r -> Op(ArraySet,[l;B;r]) |> preturn)
        <|> (set_array >>% fun l r -> 
                let rec loop = function
                    | Pos(_,x) -> loop x
                    | Op(Apply,[a;b]) -> Op(ArraySet,[a;b;r]) |> preturn
                    | _ -> fail "Expected two arguments on the left of <-."
                loop l)

    (tuple2 expressions (opt (expr_indent op .>>. expr_indent statements))
    >>= function 
        | a,Some(f,b) -> f a b
        | a,None -> preturn a) s

let annotations expr (s: CharStream<_>) = 
    let i = s.Column
    let expr_indent expr (s: CharStream<_>) = expr_indent i (<=) expr s
    pipe2 (expr_indent expr) (opt (expr_indent pp >>. expr_indent expr))
        (fun a -> function
            | Some b -> Op(TypeAnnot,[a;b])
            | None -> a) s

let inbuilt_operators =
    let dict_operator = d0()
    let add_infix_operator assoc str prec = dict_operator.Add(str, (prec, assoc))

    let left_assoc_ops = 
        let f = add_infix_operator Associativity.Left
        f "+" 60; f "-" 60; f "*" 70; f "/" 70
        f "<|" 10; f "|>" 10; f "<<" 10; f ">>" 10

    let no_assoc_ops =
        let f str = add_infix_operator Associativity.None str 40
        f "<="; f "<"; f "="; f ">"; f ">="

    let right_associative_ops =
        let f str prec = add_infix_operator Associativity.Right str prec
        f "||" 20; f "&&" 30; f "::" 50
         
    dict_operator

let negate expr = attempt (negate_ >>. expr |>> (ap (v "negate"))) <|> expr

let operators expr (s: CharStream<_>) =
    let poperator (s: CharStream<Userstate>) =
        let dict_operator = s.UserState
        let p = pos' s
        let rec calculate on_fail on_succ op = 
                match dict_operator.TryGetValue op with
                | true, (prec,asoc) -> on_succ (prec,asoc)
                | false, _ -> on_fail op
        let on_succ orig_op (prec,asoc) = preturn (prec,asoc,fun a b -> Pos(p,ap' (v orig_op) [a; b]))
        let on_fail (orig_op: string) =
            let x = orig_op.TrimStart [|'.'|]
            let fail _ = fail "unknown operator"
            let on_succ = on_succ orig_op
            let rec on_fail i _ = if i < x.Length && i >= 0 then calculate (on_fail (i-1)) on_succ x.[0..i] else fail ()
            calculate (on_fail (x.Length-1)) on_succ x
        (poperator >>=? function
            | "->" -> fail "forbidden operator"
            | orig_op -> calculate on_fail (on_succ orig_op) orig_op) s

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

    let i = s.Column
    let expr_indent expr (s: CharStream<_>) = expr_indent i (<=) expr s
    let op s = expr_indent poperator s
    let term s = expr_indent expr s
    tdop op term 0 s

let rec expr s = pos ^<| annotations ^<| indentations (statements expr) (mset expr ^<| type_ ^<| tuple ^<| negate ^<| operators ^<| application ^<| expressions expr) <| s

let spiral_parse (name, code) = runParserOnString (spaces >>. expr .>> eof) inbuilt_operators name code
