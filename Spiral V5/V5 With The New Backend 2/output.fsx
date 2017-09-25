`is_numeric a && get_type a = get_type b` is false.
a=TyVV [], b=TyLit (LitInt64 10L)
Error trace on line: 2, column: 1 in file "test1".
inl a = 5
^
Error trace on line: 3, column: 1 in file "test1".
inl b = 10
^
Error trace on line: 4, column: 1 in file "test1".
a + b
^
