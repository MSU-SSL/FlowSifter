open Batteries
open Printf

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
open ParsedPCFG

type spec_t = production list

type term_tok = Regex of string | Nt of string | Capture of term list * string
and term = term_tok * action option

type str_rule = non_terminal * predicate option * priority option * term list
type str_spec_t = str_rule list


type ('a, 'b, 'pri) regular_rule = {prio : 'pri; rx : string option;
		     act: ('a * a_exp) list; nt : 'b option}
type pred = a_exp p_exp VarMap.t
type regular_grammar = (string, (pred * (string, string, int list) regular_rule) list) Map.t


type pred_arr = (int * a_exp p_exp) list
type regular_grammar_arr = (pred_arr * (int, int, int list) regular_rule) list array

type compiled_rules = int
type regular_grammar_opt = (int array -> (string -> int list -> int) -> compiled_rules) array

(* the decision of a DFA *)
type ca_next = (int * ca_state a_opt) list * int
(* a DFA state *)
and dfa_state = (unit, int array, ca_next) Regex_dfa.state
and dfa = dfa_state array * int

and ca_resume = Waiting of (string -> ca_resume)
and ca_state = {
  vars : int array; (* the counters *)
  ca : (ca_state -> ca_resume) array; (* for each node, given the current vars and parse state, generates a dfa *)
  mutable q : string -> ca_resume; (* the current state (as a continuation, waiting for more data) *)
  mutable base_pos: int; (* offset within flow of start of flow data *)
  mutable pos: int; (* index of the next char to be parsed in within current flow data *)
  mutable flow_data: string; (* the current hunk of data to be parsed *)
  mutable fail_drop : int; (* how many bytes have been dropped because the CA is off track *)
  mutable rerun_temp : int; (* temporary storage for rerunning *)
  mutable rerun : (int * ca_state a_opt) list; (* actions that need to be run still *)
}  


(****** Parsing functions ******)

let debug_ca = false
let print_fields = false

exception Invalid_arg_count of string
let wrong_args name = raise (Invalid_arg_count name)
exception Data_stall

let matches = ref 0

let zero_size = ref 0
let () = at_exit (fun () -> if !zero_size > 0 then Printf.eprintf "#Zero size matches: %d\n" !zero_size)
(*
let () = at_exit (fun () -> Hashtbl.iter (fun k v -> Printf.printf "%d %s\n" !v k) saves)
*)

let ca_functions = ref 
  [| "pos", (fun st -> function 
    | [] -> st.base_pos + st.pos
    | _ -> wrong_args "pos");
    "token", 
    (fun st -> function 
      | [start_pos] -> (* end_pos = pos() - 1 *)
	incr matches; 
	if debug_ca then printf "T"; 
	if debug_ca then printf "start_pos: %d \tbase_pos: %d \tsim_pos: %d\n" 
	  start_pos st.base_pos st.pos;
	if print_fields then 
	  if start_pos = -1 then 
	    printf "field: ...%s\n" (String.sub st.flow_data 0 st.pos)
	  else (
	    let start_pos = start_pos - st.base_pos in
	    if start_pos < 0 then 
	      printf "Field: ...%S\n" (String.sub st.flow_data 0 st.pos)
	    else 
	      printf "Field: %S\n" (String.sub st.flow_data start_pos (st.pos - start_pos))
	  );
	0
      | _ -> wrong_args "token" );
    "bounds" ,
    (fun _st -> function 
      | [_start_pos; _end_pos] -> 
	if debug_ca then printf "B"; incr matches; 
	(*	let token = 
		if start_pos < base_pos then 
		"..." ^ String.sub flow_data 0 (end_pos - base_pos + 1)
		else 
		String.sub flow_data (start_pos - base_pos) (end_pos - start_pos + 1)
		in
		log token;	*)
	0
      (* BROKEN BY SPLICING CODE -- check bounds on start/end pos and current flow_data *)
      (*	let str = String.sub start_pos end_pos flow_data in *)
      (*	Printf.eprintf "***Match found in range %d, %d***\n" 
		start_pos end_pos;  *)
      (*      | [_s1;_e] -> 
	      incr zero_size; 
      (*	let s = s1 - base_pos in let e = e - 1 in
	      Printf.printf "base_pos %d s1 %d e %d sim_pos %d\n%!" base_pos s1 e !sim_pos;
	      Printf.printf "zero-size match: %d to %d at %S\n\n%S\n\n" s1 e (String.head flow_data s) (String.tail flow_data s); *)
	      0 *)
      | _ -> wrong_args "bounds" );
    "skip",
    (fun st ->  function 
      | [len] when len >= 0 -> st.pos <- st.pos + len; len
      | _ -> wrong_args "skip" );
    "skip_to",
    (fun st -> function
      | [pos] -> if st.pos < pos then st.pos <- pos; pos
      | _ -> wrong_args "skip_to" );
    "notify" ,
    (fun st -> function [n] -> 
      if print_fields then Printf.printf "*** Match found: %d at pos %d***\n" n (st.base_pos + st.pos);
      if debug_ca then printf "N"; incr matches; n 
      | _ -> wrong_args "skip" );
    "cur_byte",
    (fun st -> function 
      | [] -> 
	if st.pos > String.length st.flow_data then raise Data_stall;	
	let ret = Char.code st.flow_data.[st.pos] in
	st.pos <- st.pos + 1;
	ret
      | _ -> wrong_args "cur_byte" );
    "cur_double_byte",
    (fun st -> function 
      | [] -> 
	(* FIXME: problem with the double-byte spanning packets *)
	if st.pos + 1 > String.length st.flow_data then raise Data_stall;	
	let ret = (Char.code st.flow_data.[st.pos]) * 256 + (Char.code st.flow_data.[st.pos+1])in
	st.pos <- st.pos + 2;
	ret
      | _ -> wrong_args "cur_double_byte" );
    "getnum",
    (fun st -> function 
      | [] -> 
	let rec dloop acc =
	  if st.pos >= String.length st.flow_data then 
	    (st.rerun_temp <- acc; raise Data_stall);
	  match st.flow_data.[st.pos] with
	    | '0'..'9' as c -> 
	      st.pos <- st.pos + 1; dloop ((Char.code c - 0x30) + acc * 10)
	    | _ -> st.rerun_temp <- 0; acc
	in
	dloop st.rerun_temp
      | _ -> wrong_args "getnum");
    "gethex",
    (fun st -> function 
      | [] -> 
	let rec hloop acc =
	  if st.pos >= String.length st.flow_data then
	    (st.rerun_temp <- acc; raise Data_stall);
	  match st.flow_data.[st.pos] with
	    | '0'..'9' as c -> 
	      st.pos <- st.pos + 1; hloop ((Char.code c - 0x30) + acc lsl 4)
	    | 'a'..'f' as c -> 
	      st.pos <- st.pos + 1; hloop ((Char.code c - 0x61 + 10) + acc lsl 4)
	    | 'A'..'F' as c -> 
	      st.pos <- st.pos + 1; hloop ((Char.code c - 0x41 + 10) + acc lsl 4)
	    | _ -> st.rerun_temp <- 0; acc
	in
	hloop 0
      | _ -> wrong_args "gethex");
  (*    "save", (fun (base_pos, _sim_pos, flow_data) -> function | [start_pos; end_pos] -> 
	let start_pos = start_pos - base_pos and end_pos = end_pos - base_pos in
	let str = try String.sub flow_data (start_pos+1) (end_pos - start_pos) |> String.trim |> String.lowercase with _ -> "??" in
	(try Hashtbl.find saves str |> incr with Not_found -> Hashtbl.add saves str (ref 1));
	if debug_ca then printf "S"; 
	incr matches; 0
	| _ -> wrong_args "save"
	);*)
  |]

let get_f_id str = 
    try Array.findi (fun (s,_) -> s=str) !ca_functions
    with Not_found -> 
      failwith ("Unknown function name: " ^ str)

let get_f id = snd (!ca_functions.(id))

let register_f fn f = ca_functions := Array.append !ca_functions [|(fn, f)|]
