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

exception Invalid_arg_count of string
let wrong_args name = raise (Invalid_arg_count name)

let matches = ref 0

let ca_functions = ref 
  [ "pos", (fun (base_pos, sim_pos, flow_data) -> function [] -> base_pos + !sim_pos | _ -> wrong_args "pos");
    "bounds" ,
    (fun (base_pos, sim_pos, flow_data) -> function 
      | [start_pos; end_pos] when start_pos <= end_pos -> 
	(* BROKEN BY SPLICING CODE -- check bounds on start/end pos and current flow_data *)
	(*	let str = String.sub start_pos end_pos flow_data in *)
	(*	Printf.eprintf "***Match found in range %d, %d***\n" 
		start_pos end_pos;  *)
	incr matches;
	0
      | [s;e] -> 
	let str = try (String.sub flow_data (s-(min 10 s)) 20) with _ -> "??" in
	Printf.printf "zero-size match: %d to %d at %S\n" s e str;
	0
      | _ -> wrong_args "bounds"
    );
    "skip",
    (fun (base_pos, sim_pos, flow_data) ->  function 
      | [len] ->
	if len > 0 then	sim_pos := !sim_pos + len;
	0
      | _ -> wrong_args "skip" );
    "notify" ,
    (fun (base_pos, sim_pos, flow_data) ->  function [n] -> 
      (*      Printf.eprintf "*** Match found: %d ***\n" n;  *)
      incr matches; n 
      | _ -> wrong_args "skip" );
    "cur_byte",
    (fun (base_pos, sim_pos, flow_data) ->  function 
      | [] -> Char.code flow_data.[!sim_pos]
      | _ -> wrong_args "cur_byte" );
    "getnum",
    (fun (base_pos, sim_pos, flow_data) ->  function 
      | [] -> 
	let ret = ref 0 in
	while (let c = Char.code flow_data.[!sim_pos] in c >= 0x30 && c <= 0x39) do
	  let c = Char.code flow_data.[!sim_pos] in
	  ret := !ret * 10 + (c - 0x30);
	  incr sim_pos;
	done;
	!ret
      | _ -> wrong_args "getnum");
  ]

 let get_f str = 
    try List.assoc str !ca_functions
    with Not_found -> 
      failwith ("Unknown function name: " ^ str)

let register_f fn f = ca_functions := (fn,f) :: !ca_functions
