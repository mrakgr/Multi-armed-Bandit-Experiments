Variable "assembly_load" not bound.
Error trace on line: 2, column: 1 in file "Core".
inl type_lit_lift x = !TypeLitCreate(x)
^
Error trace on line: 3, column: 1 in file "Core".
inl assembly_load x = !DotNetAssemblyLoad(x)
^
Error trace on line: 4, column: 1 in file "Core".
inl assembly_load_file x = !DotNetAssemblyLoadFile(x)
^
Error trace on line: 6, column: 16 in file "Core".
inl mscorlib = assembly_load."mscorlib"
               ^
