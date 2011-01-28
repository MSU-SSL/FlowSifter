exception Error

type token = 
  | TRUE
  | SUB
  | SEMI
  | RPAREN
  | REGEX of (string)
  | RBRACK
  | OR
  | NT of (string)
  | NOT
  | MUL
  | LT
  | LPAREN
  | LE
  | LBRACK
  | INT of (int)
  | GT
  | GE
  | FALSE
  | EQUAL
  | EOF
  | DIV
  | COMMA
  | COLONEQUALS
  | BRACK of (string)
  | BECOMES
  | AND
  | ADD


val spec: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Ns_types.str_spec_t)
val pred: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Ns_types.ParsedPCFG.predicates)
val act: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Ns_types.ParsedPCFG.actions)