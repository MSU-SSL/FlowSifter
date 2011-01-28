%{
  open Printf
  open Lexing
  open Ns_types
  open Ns_types.ParsedPCFG

  let is_act = ref false
  let cur_var = ref ""

  let check_var v =
    if not !is_act && !cur_var = "" then cur_var := v else
    if v <> !cur_var then failwith "ERROR(line %d): Non-unary expression"

  module VM = Ns_types.ParsedPCFG.VarMap
  let to_map l = List.fold_left (fun acc (v,e) -> VM.add v e acc) VM.empty l

%}

%token <string> NT
%token <string> BRACK
%token <string> REGEX
%token <int> INT
%token BECOMES SEMI LPAREN RPAREN
%token ADD MUL SUB DIV COMMA COLONEQUALS
%token EQUAL LT GT LE GE NOT AND OR TRUE FALSE
%token EOF LBRACK RBRACK

%left AND OR

%left ADD SUB
%left MUL DIV


%start spec act pred
%type <Ns_types.str_spec_t> spec
%type <Ns_types.ParsedPCFG.actions> act
%type <Ns_types.ParsedPCFG.predicates> pred
%%

spec: spec = ns_rule+ EOF { spec }

ns_rule:
  | NT BRACK? INT? BECOMES term* SEMI { ($1,$2,$3,$5) }
  | error { printf "ERROR(line %d): couldn't parse rule\n%!" ($startpos.pos_lnum); exit 1 }

term:
  | REGEX BRACK? { (Regex $1, $2) }
  | NT BRACK? { (Nt $1, $2) }
  | NT LPAREN term+ RPAREN { (Capture $3, Some $1) }
  | error { printf "ERROR(line %d): couldn't parse term\n%!" ($startpos.pos_lnum); exit 1 }


act: LBRACK act=separated_list(SEMI,act_chunk) RBRACK EOF { to_map act }
act_chunk: assign a_exp end_act { $1, $2 }
assign : NT COLONEQUALS  { cur_var := $1; is_act:=true; $1 }
end_act : { cur_var := ""; is_act:=false; }

pred: LBRACK pred=separated_list(SEMI, pred_chunk) RBRACK EOF { to_map pred }
pred_chunk : p_exp { let r = (!cur_var, $1) in cur_var := ""; r }

a_exp:
  | a_exp ADD a_exp { Plus($1,$3) }
  | a_exp MUL a_exp { Multiply($1,$3) }
  | a_exp SUB a_exp { Sub($1,$3) }
  | a_exp DIV a_exp { Divide($1,$3) }
  | var=NT LPAREN args=separated_list(COMMA, a_exp) RPAREN { Function(var, args) }
  | INT { Constant $1 }
  | NT { check_var $1; Variable }
  | LPAREN a_exp RPAREN { $2 }

p_exp:
  | a_exp EQUAL a_exp { Equal ($1,$3) }
  | a_exp LT a_exp { Lessthan ($1,$3) }
  | a_exp GT a_exp { Greaterthan ($1,$3) }
  | a_exp LE a_exp { LessthanEq ($1,$3) }
  | a_exp GE a_exp { GreaterthanEq ($1,$3) }
  | NOT p_exp { Not $2 }
  | p_exp AND p_exp { And ($1,$3) }
  | p_exp OR p_exp { Or ($1,$3) }
  | TRUE { Const true }
  | FALSE { Const false }
  | LPAREN p_exp RPAREN { $2 }

%%
