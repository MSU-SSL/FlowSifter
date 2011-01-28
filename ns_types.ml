open Batteries 

type action = string
type non_terminal = string
type predicate = string
type priority = int

module StringParams = struct 
  type var = string 
  type term = string 
  type nonterm = string 
  let print_var = String.print
  let print_term = String.print
  let print_nonterm = String.print
  let compare_nonterm = String.compare
end

module ParsedPCFG = PCFG.Make(StringParams)

type spec_t = ParsedPCFG.production list

type term_tok = Regex of string | Nt of string | Capture of term list
and term = term_tok * action option

type str_rule = non_terminal * predicate option * priority option * term list
type str_spec_t = str_rule list

