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
"""
inl module_statements buffer =
    inl state x = buffer.Add <| Statement x
    inl enter' f =
        buffer.Add .Indent
        f()
        buffer.Add .Dedent
    inl enter f = 
        enter' <| inl _ -> 
            match f() with
            | "" -> ()
            | s -> state s
    module
"""

[<Struct>]
type Q =
    val a: int
    val b: string

    new (q,w) = {a=q;b=w}

Q(1,"2")
