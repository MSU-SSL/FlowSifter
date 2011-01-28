{
  open Ns_yac
  open Lexing
  open Printf
}

let digit = ['0'-'9']
let alpha = ['a'-'z''A'-'Z']
let eol = ('\010' | '\013' | "\013\010")
let id = (alpha | '_') (alpha | digit | '_')*
let blank = [' ' '\009' '\012']
let brack = '[' [^']']* ']'
let regex = '/' ([^'/']* '\\' '/')* [^'/']* '/'
let comment = '#' [^'\n']* '\n'

rule token = parse
  | "->"        { BECOMES }
(*  | "becomes"   { BECOMES } *)
  | ';'         { SEMI }
  | '('         { LPAREN }
  | ')'         { RPAREN }
  | brack  as s { BRACK s }
  | regex  as s { REGEX s }
  | digit+ as s { INT (int_of_string s) }
  | id     as s { NT s }
  | blank 	{ token lexbuf }
  | eol | comment { new_line lexbuf; token lexbuf }
  | _ as c { failwith (sprintf "Unknown token in spec: %c\n" c) }
  | eof         { EOF }

and act_token = parse
  | ('-'? digit+) as s { INT (int_of_string s) }
  | id     as s { NT s }
  | '(' { LPAREN }
  | ')' { RPAREN }
  | '[' { LBRACK }
  | ']' { RBRACK }
  | ',' {COMMA}
  | ';'         { SEMI }
  | '+' {ADD}
  | '-' {SUB}
  | '*' {MUL}
  | '/' {DIV}
  | ":=" {COLONEQUALS}
  | blank {act_token lexbuf}
  | _ as c { failwith (sprintf "Unknown token in action: %c\n" c) }
  | eof {EOF}

and pred_token = parse
  | "==" {EQUAL}
  | '<' {LT}
  | '>' {GT}
  | ">=" {GE}
  | "<=" {LE}
  | "not" {NOT}
  | "or" {OR}
  | "and" {AND}
  | "true" { TRUE }
  | "false" { FALSE }
  | blank {pred_token lexbuf}
  | ('-'? digit+) as s { INT (int_of_string s) }
  | id     as s { NT s }
  | '(' { LPAREN }
  | ')' { RPAREN }
  | '[' { LBRACK }
  | ']' { RBRACK }
  | ',' {COMMA}
  | ';'         { SEMI }
  | '+' {ADD}
  | '-' {SUB}
  | '*' {MUL}
  | '/' {DIV}
  | ":=" {COLONEQUALS}
  | _ as c { failwith (sprintf "Unknown token in predicate: %c\n" c) }
  | eof { EOF }
