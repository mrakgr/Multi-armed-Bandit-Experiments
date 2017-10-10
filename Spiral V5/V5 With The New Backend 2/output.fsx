Expected a bool in conditional.
Got: FunT
  (map
     [("array_length", FunT (map [],FunTypeFunction ("x", Op <tag 34>)));
      ("f", FunT (map [("x", PrimT Int64T)],FunTypeFunction ("y", Op <tag 48>)));
      ("for'",
       FunT
         (map
            [("x",
              FunT
                ...
Error trace on line: 4, column: 1 in file "hacker_rank_8".
open Parsing
^
Error trace on line: 5, column: 1 in file "hacker_rank_8".
open Console
^
Error trace on line: 6, column: 1 in file "hacker_rank_8".
open Array
^
Error trace on line: 10, column: 1 in file "hacker_rank_8".
inl solve ar = 
^
Error trace on line: 15, column: 1 in file "hacker_rank_8".
inl show = function
^
Error trace on line: 19, column: 1 in file "hacker_rank_8".
inl parser = 
^
Error trace on line: 26, column: 1 in file "hacker_rank_8".
run_with_unit_ret (readall()) parser
^
Error trace on line: 116, column: 37 in file "Parsing".
inl run_with_unit_ret data parser = run data parser with_unit_ret
                                    ^
Error trace on line: 105, column: 5 in file "Parsing".
    match data with
    ^
Error trace on line: 106, column: 21 in file "Parsing".
    | _ : string -> parser .elem { ret with stream = string_stream data} { pos = if is_static data then 0 else dyn 0 }
                    ^
Error trace on line: 34, column: 37 in file "Parsing".
    parser = inl {on_succ} state -> on_succ state state
                                    ^
Error trace on line: 97, column: 9 in file "Parsing".
        stream {
        ^
Error trace on line: 90, column: 5 in file "Parsing".
    inl f idx = idx >= 0 && idx < string_length str
    ^
Error trace on line: 91, column: 5 in file "Parsing".
    match idx with
    ^
Error trace on line: 92, column: 48 in file "Parsing".
    | a, b when f a && f b | idx when f idx -> on_succ (str idx)
                                               ^
Error trace on line: 99, column: 32 in file "Parsing".
            on_succ = inl c -> on_succ c {state with pos=pos+1}
                               ^
Error trace on line: 47, column: 9 in file "Parsing".
        if cond then on_succ () state 
        ^
Error trace on line: 6, column: 51 in file "Parsing".
        || {parser_rec} {d with on_type} state -> parser_rec d .elem d state : on_type
                                                  ^
Error trace on line: 34, column: 37 in file "Parsing".
    parser = inl {on_succ} state -> on_succ state state
                                    ^
Error trace on line: 97, column: 9 in file "Parsing".
        stream {
        ^
Error trace on line: 90, column: 5 in file "Parsing".
    inl f idx = idx >= 0 && idx < string_length str
    ^
Error trace on line: 91, column: 5 in file "Parsing".
    match idx with
    ^
Error trace on line: 92, column: 48 in file "Parsing".
    | a, b when f a && f b | idx when f idx -> on_succ (str idx)
                                               ^
Error trace on line: 99, column: 32 in file "Parsing".
            on_succ = inl c -> on_succ c {state with pos=pos+1}
                               ^
Error trace on line: 47, column: 9 in file "Parsing".
        if cond then on_succ () state 
        ^
Error trace on line: 6, column: 51 in file "Parsing".
        || {parser_rec} {d with on_type} state -> parser_rec d .elem d state : on_type
                                                  ^
Error trace on line: 34, column: 37 in file "Parsing".
    parser = inl {on_succ} state -> on_succ state state
                                    ^
Error trace on line: 97, column: 9 in file "Parsing".
        stream {
        ^
Error trace on line: 90, column: 5 in file "Parsing".
    inl f idx = idx >= 0 && idx < string_length str
    ^
Error trace on line: 91, column: 5 in file "Parsing".
    match idx with
    ^
Error trace on line: 92, column: 48 in file "Parsing".
    | a, b when f a && f b | idx when f idx -> on_succ (str idx)
                                               ^
Error trace on line: 99, column: 32 in file "Parsing".
            on_succ = inl c -> on_succ c {state with pos=pos+1}
                               ^
Error trace on line: 47, column: 9 in file "Parsing".
        if cond then on_succ () state 
        ^
Error trace on line: 48, column: 14 in file "Parsing".
        else handler .elem d state
             ^
Error trace on line: 37, column: 33 in file "Parsing".
    parser = inl {on_succ} _ -> on_succ () state
                                ^
Error trace on line: 6, column: 51 in file "Parsing".
        || {parser_rec} {d with on_type} state -> parser_rec d .elem d state : on_type
                                                  ^
Error trace on line: 34, column: 37 in file "Parsing".
    parser = inl {on_succ} state -> on_succ state state
                                    ^
Error trace on line: 97, column: 9 in file "Parsing".
        stream {
        ^
Error trace on line: 90, column: 5 in file "Parsing".
    inl f idx = idx >= 0 && idx < string_length str
    ^
Error trace on line: 91, column: 5 in file "Parsing".
    match idx with
    ^
Error trace on line: 92, column: 48 in file "Parsing".
    | a, b when f a && f b | idx when f idx -> on_succ (str idx)
                                               ^
Error trace on line: 99, column: 32 in file "Parsing".
            on_succ = inl c -> on_succ c {state with pos=pos+1}
                               ^
Error trace on line: 47, column: 9 in file "Parsing".
        if cond then on_succ () state 
        ^
Error trace on line: 48, column: 14 in file "Parsing".
        else handler .elem d state
             ^
Error trace on line: 37, column: 33 in file "Parsing".
    parser = inl {on_succ} _ -> on_succ () state
                                ^
Error trace on line: 6, column: 51 in file "Parsing".
        || {parser_rec} {d with on_type} state -> parser_rec d .elem d state : on_type
                                                  ^
Error trace on line: 51, column: 29 in file "Parsing".
    parser = inl d state -> if cond then tr () .elem d state else fl () .elem d state
                            ^
Error trace on line: 34, column: 37 in file "Parsing".
    parser = inl {on_succ} state -> on_succ state state
                                    ^
Error trace on line: 97, column: 9 in file "Parsing".
        stream {
        ^
Error trace on line: 90, column: 5 in file "Parsing".
    inl f idx = idx >= 0 && idx < string_length str
    ^
Error trace on line: 91, column: 5 in file "Parsing".
    match idx with
    ^
Error trace on line: 92, column: 48 in file "Parsing".
    | a, b when f a && f b | idx when f idx -> on_succ (str idx)
                                               ^
Error trace on line: 99, column: 32 in file "Parsing".
            on_succ = inl c -> on_succ c {state with pos=pos+1}
                               ^
Error trace on line: 47, column: 9 in file "Parsing".
        if cond then on_succ () state 
        ^
Error trace on line: 6, column: 51 in file "Parsing".
        || {parser_rec} {d with on_type} state -> parser_rec d .elem d state : on_type
                                                  ^
Error trace on line: 34, column: 37 in file "Parsing".
    parser = inl {on_succ} state -> on_succ state state
                                    ^
Error trace on line: 97, column: 9 in file "Parsing".
        stream {
        ^
Error trace on line: 90, column: 5 in file "Parsing".
    inl f idx = idx >= 0 && idx < string_length str
    ^
Error trace on line: 91, column: 5 in file "Parsing".
    match idx with
    ^
Error trace on line: 92, column: 48 in file "Parsing".
    | a, b when f a && f b | idx when f idx -> on_succ (str idx)
                                               ^
Error trace on line: 99, column: 32 in file "Parsing".
            on_succ = inl c -> on_succ c {state with pos=pos+1}
                               ^
Error trace on line: 47, column: 9 in file "Parsing".
        if cond then on_succ () state 
        ^
Error trace on line: 6, column: 51 in file "Parsing".
        || {parser_rec} {d with on_type} state -> parser_rec d .elem d state : on_type
                                                  ^
Error trace on line: 34, column: 37 in file "Parsing".
    parser = inl {on_succ} state -> on_succ state state
                                    ^
Error trace on line: 97, column: 9 in file "Parsing".
        stream {
        ^
Error trace on line: 90, column: 5 in file "Parsing".
    inl f idx = idx >= 0 && idx < string_length str
    ^
Error trace on line: 91, column: 5 in file "Parsing".
    match idx with
    ^
Error trace on line: 92, column: 48 in file "Parsing".
    | a, b when f a && f b | idx when f idx -> on_succ (str idx)
                                               ^
Error trace on line: 99, column: 32 in file "Parsing".
            on_succ = inl c -> on_succ c {state with pos=pos+1}
                               ^
Error trace on line: 47, column: 9 in file "Parsing".
        if cond then on_succ () state 
        ^
Error trace on line: 48, column: 14 in file "Parsing".
        else handler .elem d state
             ^
Error trace on line: 37, column: 33 in file "Parsing".
    parser = inl {on_succ} _ -> on_succ () state
                                ^
Error trace on line: 6, column: 51 in file "Parsing".
        || {parser_rec} {d with on_type} state -> parser_rec d .elem d state : on_type
                                                  ^
Error trace on line: 34, column: 37 in file "Parsing".
    parser = inl {on_succ} state -> on_succ state state
                                    ^
Error trace on line: 97, column: 9 in file "Parsing".
        stream {
        ^
Error trace on line: 90, column: 5 in file "Parsing".
    inl f idx = idx >= 0 && idx < string_length str
    ^
Error trace on line: 91, column: 5 in file "Parsing".
    match idx with
    ^
Error trace on line: 92, column: 48 in file "Parsing".
    | a, b when f a && f b | idx when f idx -> on_succ (str idx)
                                               ^
Error trace on line: 99, column: 32 in file "Parsing".
            on_succ = inl c -> on_succ c {state with pos=pos+1}
                               ^
Error trace on line: 47, column: 9 in file "Parsing".
        if cond then on_succ () state 
        ^
Error trace on line: 48, column: 14 in file "Parsing".
        else handler .elem d state
             ^
Error trace on line: 37, column: 33 in file "Parsing".
    parser = inl {on_succ} _ -> on_succ () state
                                ^
Error trace on line: 47, column: 9 in file "Parsing".
        if cond then on_succ () state 
        ^
Error trace on line: 6, column: 51 in file "Parsing".
        || {parser_rec} {d with on_type} state -> parser_rec d .elem d state : on_type
                                                  ^
Error trace on line: 51, column: 29 in file "Parsing".
    parser = inl d state -> if cond then tr () .elem d state else fl () .elem d state
                            ^
Error trace on line: 22, column: 31 in file "Parsing".
    parser = inl {on_succ} -> on_succ x
                              ^
Error trace on line: 40, column: 58 in file "Parsing".
    parser = inl d -> a .elem {d with on_succ = inl x -> b x .elem d}
                                                         ^
Error trace on line: 22, column: 31 in file "Parsing".
    parser = inl {on_succ} -> on_succ x
                              ^
Error trace on line: 40, column: 58 in file "Parsing".
    parser = inl d -> a .elem {d with on_succ = inl x -> b x .elem d}
                                                         ^
Error trace on line: 64, column: 32 in file "Parsing".
inl (|>>) a f = a >>= inl x -> succ (f x)
                               ^
Error trace on line: 11, column: 5 in file "hacker_rank_8".
    inl r = Array.foldl (^^^) 0 ar
    ^
Error trace on line: 12, column: 5 in file "hacker_rank_8".
    if Array.forall ((=) 1) then r ^^^ 1
    ^
