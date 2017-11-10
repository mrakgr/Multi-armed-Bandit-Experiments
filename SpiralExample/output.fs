Expected a heap mutable module, reference or an array the input to mutable set.
Got: TyList [TyV (0, ArrayT (ArtDotNetHeap,PrimT Int64T))]
Error trace on line: 2, column: 10 in file "test86".
inl ar = HostTensor.init 10 id
         ^
Error trace on line: 52, column: 5 in file "HostTensor".
    match Tuple.length size with
    ^
Error trace on line: 55, column: 9 in file "HostTensor".
        inl len :: dim_offsets = Tuple.scanr (inl (!dim_size dim) s -> dim * s) size 1
        ^
Error trace on line: 56, column: 9 in file "HostTensor".
        inl ty = type (f (Tuple.repeat num_dims 0))
        ^
Error trace on line: 57, column: 9 in file "HostTensor".
        inl ar = create ty len
        ^
Error trace on line: 58, column: 9 in file "HostTensor".
        inl rec loop offset index = function
        ^
Error trace on line: 70, column: 9 in file "HostTensor".
        loop (dyn 0) () (size,dim_offsets)
        ^
Error trace on line: 60, column: 17 in file "HostTensor".
                for {from to state=offset; body=inl {state=offset i} ->
                ^
Error trace on line: 63, column: 23 in file "HostTensor".
                    } |> ignore
                      ^
Error trace on line: 61, column: 18 in file "Core".
inl (>>) a b x = b (a x)
                 ^
Error trace on line: 42, column: 13 in file "Loops".
            if by < 0 then loop {d with check=match d with | {to} -> inl from -> from >= to | {near_to} -> inl from -> from > near_to}
            ^
Error trace on line: 43, column: 18 in file "Loops".
            else loop {d with check=match d with | {to} -> inl from -> from <= to | {near_to} -> inl from -> from < near_to}
                 ^
Error trace on line: 12, column: 9 in file "Loops".
        inl loop_body {check from by state body finally} as d =
        ^
Error trace on line: 24, column: 9 in file "Loops".
        match d with
        ^
Error trace on line: 26, column: 16 in file "Loops".
        | _ -> (met d -> loop_body d) {d with from=dyn from}
               ^
Error trace on line: 13, column: 13 in file "Loops".
            if check from then 
            ^
Error trace on line: 14, column: 17 in file "Loops".
                match kind with
                ^
Error trace on line: 20, column: 21 in file "Loops".
                    loop {d with state=body {state i=from}; from=from+by}
                    ^
Error trace on line: 61, column: 21 in file "HostTensor".
                    loop offset (i :: index) (size,dim_offsets)
                    ^
Error trace on line: 65, column: 17 in file "HostTensor".
                match f (Tuple.rev index) with
                ^
Error trace on line: 69, column: 21 in file "HostTensor".
                    set offset ar x
                    ^
Error trace on line: 49, column: 23 in file "HostTensor".
inl toa_iter2 f a b = toa_map2 (inl a b -> f a b; ()) a b |> ignore
                      ^
Error trace on line: 40, column: 5 in file "HostTensor".
    inl rec loop = function
    ^
Error trace on line: 46, column: 5 in file "HostTensor".
    loop (a,b)
    ^
Error trace on line: 45, column: 19 in file "HostTensor".
        | x, y -> f x y
                  ^
Error trace on line: 49, column: 44 in file "HostTensor".
inl toa_iter2 f a b = toa_map2 (inl a b -> f a b; ()) a b |> ignore
                                           ^
Error trace on line: 75, column: 47 in file "HostTensor".
    set = inl offset -> toa_iter2 (inl a b -> a offset <- b) 
                                              ^
