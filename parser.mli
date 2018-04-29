type token =
  | INT of (int)
  | T
  | F
  | CUT
  | FAIL
  | PLUS
  | MINUS
  | TIMES
  | DIV
  | MOD
  | EXP
  | LPAREN
  | RPAREN
  | PERIOD
  | COMMA
  | IF
  | ID of (string)
  | VAR of (string)
  | EQ
  | GRT
  | LST
  | GRE
  | LTE
  | LBRACKET
  | RBRACKET
  | BAR
  | EOL
  | EOF

val program :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> PrologInterpreter.program
val goal :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> (PrologInterpreter.goal)
