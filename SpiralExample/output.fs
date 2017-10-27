Pattern miss error. The argument is TyV
  (1,
   UnionT
     (set
        [ListT [LitT (LitString "Some"); PrimT Int64T]; LitT (LitString "None")]))
Error trace on line: 2, column: 1 in file "test8".
type option_int = 
^
Error trace on line: 6, column: 1 in file "test8".
met x = box option_int .None
^
Error trace on line: 7, column: 1 in file "test8".
match x with
^
Error trace on line: 11, column: 5 in file "test8".
    match x with
    ^
