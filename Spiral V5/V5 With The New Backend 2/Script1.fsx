"""
inl rec print_main buffer = 
    inl add = the ProgramNode >> buffer.Add
    inl r = print_main buffer
    function
    || .state x -> .Statement x |> add
    || .enter' f ->
        .Indent |> add
        f()
        .Dedent |> add
    || .enter f ->
        r .enter' <| inl _ -> 
            match f() with
            | "" -> ()
            | s -> state s
"""
