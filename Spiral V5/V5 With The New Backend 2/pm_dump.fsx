let pattern_compile arg pat =
    let rec pattern_compile flag_is_var_type arg pat (on_succ: Lazy<_>) (on_fail: Lazy<_>) =
        let inline cp' arg pat on_succ on_fail = pattern_compile flag_is_var_type arg pat on_succ on_fail
        let inline cp arg pat on_succ on_fail = lazy cp' arg pat on_succ on_fail

        let inline pat_fold_template map_end map_on_succ map_on_fail tuple_start_indexer tuple_index pos l =
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
                tuple_start_indexer tuple_index pos l
        let pat_tuple pos l = pat_tuple' tuple_index case_tuple pos l
        let pat_cons pos l = pat_tuple' tuple_slice_from case_cons pos l

        let pat_pass map_end map_on_succ map_on_fail pos l =
            let tuple_pass v _ _ = v
            pat_fold_template map_end map_on_succ map_on_fail
                tuple_pass tuple_pass pos l

        let pat_or pos l = // The or pattern accumulates the patterns into the on_fail
            pat_pass
                (fun _ on_fail _ -> on_fail.Value)
                (fun _ _ on_succ _ -> on_succ) // id
                cp pos l

        let pat_and pos l = // The and pattern accumulates the patterns into the on_succ
            pat_pass
                (fun on_succ _ _ -> on_succ.Value)
                cp (fun _ _ _ on_fail -> on_fail) // id
                pos l

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
        | PatClauses (pos, l) ->
            pat_pass
                (fun on_succ on_fail _ -> 
                    printfn "I am in PatClauses's map_end. %A %A"  on_succ.Value on_fail.Value
                    let x = on_succ.Value
                    printfn "I am exiting PatClauses's map_end."
                    x
                    )
                (fun arg (pat, exp) on_succ on_fail -> cp arg pat (lazy exp) on_fail)
                (fun arg (pat,exp) on_succ on_fail -> on_succ)
                pos l
        | PatNameT (pos, x) ->
            let x = type_lit_create pos (LitString x)
            if_static (eq_type arg x pos) on_succ.Value on_fail.Value pos

    //let pattern_compile_def_on_succ = lazy failwith "Missing a clause."
    let pattern_compile_def_on_succ = lazy error_type (Lit(LitString "Missing a clause.", None)) // This is a compiler error. Will be removed.
    let pattern_compile_def_on_fail = lazy error_type (Lit(LitString "Pattern matching cases are inexhaustive", None))

    printfn "pat=%A" pat

    pattern_compile false arg pat pattern_compile_def_on_succ pattern_compile_def_on_fail

