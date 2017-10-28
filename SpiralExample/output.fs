Error in test88: Ln: 2 Col: 1
inl closure_type = (1 => 2)
^
Expecting: number, '!', '"', '"""', '\'', '(', '-', '.', '//', '@"', '[',
'cuda', 'false', 'function', 'if', 'if_dynamic', 'inm', 'join', 'match', 'met',
'open', 'print_env', 'print_expr', 'true', 'type' or '{'

The parser backtracked after:
  Error in test88: Ln: 2 Col: 5
  inl closure_type = (1 => 2)
      ^
  Expecting: '//' or 'rec'

The parser backtracked after:
  Error in test88: Ln: 2 Col: 5
  inl closure_type = (1 => 2)
      ^
  Expecting: '//'
  Other error messages:
    inl not allowed as an identifier.

The parser backtracked after:
  Error in test88: Ln: 2 Col: 18
  inl closure_type = (1 => 2)
                   ^
  Expecting: identifier, number, '!', '"', '"""', '#', '&', '\'', '(', ',',
  '->', '.', '//', ':', '::', '=>', '@', '@"', '[', '_', 'as', 'false', 'true',
  'when', '{' or '|'

The parser backtracked after:
  Error in test88: Ln: 2 Col: 20
  inl closure_type = (1 => 2)
                     ^
  Expecting: number, '!', '"', '"""', '\'', '-', '.', '//', '@"', '[', 'cuda',
  'false', 'function', 'if', 'if_dynamic', 'inl', 'inm', 'join', 'match', 'met'
  , 'open', 'print_env', 'print_expr', 'true', 'type' or '{'

  The parser backtracked after:
    Error in test88: Ln: 2 Col: 21
    inl closure_type = (1 => 2)
                        ^
    Expecting: '//'

  The parser backtracked after:
    Error in test88: Ln: 2 Col: 23
    inl closure_type = (1 => 2)
                          ^
    Expecting: identifier, number, '!', '"', '"""', '\'', '(', ')', ',', '-',
    '.', '//', ':', ':=', ';', '<-', '@"', '[', 'cuda', 'false', 'function',
    'if', 'if_dynamic', 'inl', 'inm', 'join', 'match', 'met', 'open',
    'print_env', 'print_expr', 'true', 'type' or '{'

    The parser backtracked after:
      Error in test88: Ln: 2 Col: 26
      inl closure_type = (1 => 2)
                               ^
      Expecting: '//'
