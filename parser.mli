type token =
  | PLUS
  | MINUS
  | TIMES
  | DIVIDE
  | EOF
  | SEMI
  | EQUALS
  | LITERAL of (int)
  | NAME of (string)

val expr :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Ast.expr
