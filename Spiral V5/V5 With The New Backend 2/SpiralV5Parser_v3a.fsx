#load "SpiralV5Language_v10a.fsx"
#r "../../packages/FParsec.1.0.2/lib/net40-client/FParsecCS.dll"
#r "../../packages/FParsec.1.0.2/lib/net40-client/FParsec.dll"

open SpiralV5Language_v10a
open FParsec

type ParserExpr =
| ParserStatement of (Expr -> Expr)
| ParserExpr of Expr

type Pattern =
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
let patid_cons = patid '''
let patid_type = patid '?'
let patid_active = patid '^'
let patid_or = patid '|'
let patid_and = patid '&'
let patid_namet = patid '.'

let pat_tuple' pattern = rounds (many pattern)

let pat_var pattern = tuple2 ppos var_name |>> PatVar
let pat_tuple pattern = tuple2 ppos (pat_tuple' pattern) |>> PatTuple
let pat_cons pattern = tuple2 ppos (patid_cons >>. pat_tuple' pattern) |>> PatCons
let pat_type pattern = tuple2 ppos (patid_type >>. tuple2 pattern pattern) |>> PatType
let pat_active pattern = tuple2 ppos (patid_active >>. tuple2 var_name pattern) |>> PatActive
let pat_or pattern = tuple2 ppos (patid_or >>. pat_tuple' pattern) |>> PatOr
let pat_and pattern = tuple2 ppos (patid_and >>. pat_tuple' pattern) |>> PatAnd
let pat_namet pattern = tuple2 ppos (patid_namet >>. var_name) |>> PatNameT

let rec patterns (s: CharStream<_>) =
    [|
    pat_var; pat_tuple; pat_cons; pat_type
    pat_active; pat_or; pat_and; pat_namet
    |] 
    |> Array.map (fun x -> x patterns)
    |> choice
    |> fun p -> p s

let pattern_list = patterns

let pattern_compile arg pat =
    let rec pattern_compile flag_is_var_type arg pat (on_succ: Lazy<_>) (on_fail: Lazy<_>) =
        let inline cp' arg pat on_succ on_fail = pattern_compile flag_is_var_type arg pat on_succ on_fail
        let inline cp arg pat on_succ on_fail = lazy cp' arg pat on_succ on_fail

        let inline pat_fold_template map_end map_on_succ map_on_fail tuple_start_indexer pos l =
            let len = List.length l
            List.foldBack (fun pat (on_succ, on_fail, indexer, i) -> 
                let arg = indexer arg i pos
                let on_succ' = map_on_succ arg pat on_succ on_fail
                let on_fail' = map_on_fail arg pat on_succ on_fail
                on_succ', on_fail', tuple_index, i-1
                ) l (on_succ, on_fail, tuple_start_indexer, len - 1)
            |> fun (on_succ,on_fail,_,_) ->
                map_end on_succ on_fail len

        let pat_tuple' tuple_start_indexer case_ pos l =
            pat_fold_template
                (fun on_succ _ len -> case_ arg (lit_int len pos) on_succ.Value on_fail.Value pos)
                cp (fun _ _ _ on_fail -> on_fail) // Accumulates the arguments into on_succ
                tuple_start_indexer pos l
        let pat_tuple pos l = pat_tuple' tuple_index case_tuple pos l
        let pat_cons pos l = pat_tuple' tuple_slice_from case_cons pos l

        let pat_or pos l = // The or pattern accumulates the patterns into the on_fail
            pat_fold_template
                (fun _ on_fail _ -> on_fail.Value)
                (fun _ _ on_succ _ -> on_succ) // id
                cp tuple_index pos l

        let pat_and pos l = // The and pattern accumulates the patterns into the on_succ
            pat_fold_template
                (fun on_succ _ _ -> on_succ.Value)
                cp (fun _ _ _ on_fail -> on_fail) // id
                tuple_index pos l

        match pat with
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
        | PatClauses (pos, l) ->
            pat_fold_template
                (fun on_succ _ _ -> on_succ.Value)
                (fun arg (pat, exp) on_succ on_fail -> cp arg pat (lazy exp) on_fail)
                (fun arg (pat,exp) on_succ on_fail -> on_succ)
                (fun arg _ _ -> arg) 
                pos l
        | PatNameT (pos, x) ->
            let x = ty_lit_create pos (LitString x)
            if_static (eq_type arg x pos) on_succ.Value on_fail.Value pos

    let pattern_compile_def_on_succ = lazy failwith "Missing a clause."
    let pattern_compile_def_on_fail = lazy error_type (Lit(LitString "Pattern matching cases are inexhaustive", None))

    pattern_compile false arg pat pattern_compile_def_on_succ pattern_compile_def_on_fail