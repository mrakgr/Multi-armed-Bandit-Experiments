TyLit
  (LitString
     "The by field should not be zero in loop as the program would diverge.")
Error trace on line: 2, column: 1 in file "loop4".
open Console
^
Error trace on line: 3, column: 1 in file "loop4".
open Loops
^
Error trace on line: 5, column: 1 in file "loop4".
for {from=6; to=3; by=0; state=0; body = inl {state i} ->
^
Error trace on line: 9, column: 1 in file "loop4".
|> writeline
^
Error trace on line: 40, column: 20 in file "Loops".
    | {by} as d -> for' d
                   ^
Error trace on line: 11, column: 5 in file "Loops".
    inl rec loop {check from to by state body} as d =
    ^
Error trace on line: 22, column: 5 in file "Loops".
    inl er_msg = "The by field should not be zero in loop as the program would diverge."
    ^
Error trace on line: 23, column: 5 in file "Loops".
    match d with
    ^
Error trace on line: 27, column: 5 in file "Loops".
    |> function 
    ^
Error trace on line: 28, column: 47 in file "Loops".
        | {by} when is_static by && by = 0 -> error_type er_msg
                                              ^
