type token =
  | LPAREN
  | RPAREN
  | LBRACKET
  | RBRACKET
  | LBRACE
  | RBRACE
  | SEMI
  | COMMA
  | COLON
  | PLUS
  | MINUS
  | TIMES
  | DIVIDE
  | ASSIGN
  | INC
  | DEC
  | EQ
  | NEQ
  | LT
  | GT
  | LEQ
  | GEQ
  | AND
  | OR
  | NOT
  | IF
  | ELSE
  | WHILE
  | FOR
  | RETURN
  | TRUE
  | FALSE
  | INT
  | FLOAT
  | BOOL
  | VOID
  | STRING
  | MATRIX
  | ROWS
  | COLS
  | TRANSPOSE
  | TRACE
  | SUBMATRIX
  | NUM_LIT of (Ast.num)
  | STRING_LIT of (string)
  | ID of (string)
  | NULL
  | EOF

val program :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Ast.program
