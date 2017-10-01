Invalid use of apply. TyLit (LitInt64 1L) and TyLit (LitInt64 -2L)
Error trace on line: 2, column: 1 in file "test5".
inl f = function
^
Error trace on line: 6, column: 1 in file "test5".
inl a = f .Add 1 2
^
Error trace on line: 7, column: 9 in file "test5".
inl b = f .Sub 1 2
        ^
Error trace on line: 4, column: 20 in file "test5".
    || .Sub x y -> x - y
                   ^
