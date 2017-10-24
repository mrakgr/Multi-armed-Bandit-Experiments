A type cannot be found inside the assembly.
Error trace on line: 2, column: 1 in file "test15".
inl system = assembly_load .mscorlib
^
Error trace on line: 3, column: 1 in file "test15".
inl builder_type = system ."System.Text.StringBuilder"
^
Error trace on line: 4, column: 1 in file "test15".
inl b = builder_type ("Qwe", 128i32)
^
Error trace on line: 5, column: 1 in file "test15".
inl a x =
^
Error trace on line: 11, column: 1 in file "test15".
inl str = b.ToString()
^
Error trace on line: 12, column: 1 in file "test15".
inl console = ."System.Console" |> system
^
Error trace on line: 15, column: 1 in file "test15".
inl dictionary_type = ."System.Collections.Generic.Dictionary" |> system
^
Error trace on line: 16, column: 12 in file "test15".
inl dict = dictionary_type(int64, int64)(128i32)
           ^
