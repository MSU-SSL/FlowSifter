open Batteries_uni
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

type spec_t = ParsedPCFG.production list

type term_tok = Regex of string | Nt of string | Capture of term list
and term = term_tok * action option

type str_rule = non_terminal * predicate option * priority option * term list
type str_spec_t = str_rule list

open ParsedPCFG

type ('a, 'b, 'pri) regular_rule = {prio : 'pri; rx : string option;
		     act: ('a * a_exp) list; nt : 'b option}
type pred = p_exp VarMap.t
type regular_grammar = (string, (pred * (string, string, int list) regular_rule) list) Map.t


type pred_arr = (int * p_exp) list
type regular_grammar_arr = (pred_arr * (int, int, int list) regular_rule) list array

type compiled_rules = int
type regular_grammar_opt = (int array -> (string -> int list -> int) -> compiled_rules) array


(****** Parsing functions ******)

let debug_ca = true

exception Invalid_arg_count of string
let wrong_args name = raise (Invalid_arg_count name)

let matches = ref 0
let saves = Hashtbl.create 100

let zero_size = ref 0
let () = at_exit (fun () -> if !zero_size > 0 then Printf.eprintf "#Zero size matches: %d\n" !zero_size)
(*
let () = at_exit (fun () -> Hashtbl.iter (fun k v -> Printf.printf "%d %s\n" !v k) saves)
*)

let ca_functions = ref 
  [ "pos", (fun (base_pos, sim_pos, _flow_data) -> function 
    | [] -> base_pos + !sim_pos 
    | _ -> wrong_args "pos");
    "token", 
    (fun (_base_pos, _sim_pos, _flow_data) -> function 
      | [_start_pos] -> (* end_pos = pos() - 1 *)
	if debug_ca then printf "T"; incr matches; 0
      | _ -> wrong_args "token" );
    "bounds" ,
    (fun (_base_pos, _sim_pos, _flow_data) -> function 
      | [_start_pos; _end_pos] -> if debug_ca then printf "B"; incr matches; 0
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
    (fun (_base_pos, sim_pos, _flow_data) ->  function 
      | [len] when len >= 0 -> sim_pos := !sim_pos + len; len
      | _ -> wrong_args "skip" );
    "skip_to",
    (fun (_base_pos, sim_pos, _flow_data) -> function
      | [pos] -> if !sim_pos < pos then sim_pos := pos; pos
      | _ -> wrong_args "skip_to" );
    "notify" ,
    (fun (_base_pos, _sim_pos, _flow_data) ->  function [n] -> 
      (*      Printf.eprintf "*** Match found: %d ***\n" n;  *)
      if debug_ca then printf "N"; incr matches; n 
      | _ -> wrong_args "skip" );
    "cur_byte",
    (fun (_base_pos, sim_pos, flow_data) ->  function 
      | [] -> Char.code flow_data.[!sim_pos]
      | _ -> wrong_args "cur_byte" );
    "getnum",
    (fun (_base_pos, sim_pos, flow_data) ->  function 
      | [] -> 
	let ret = ref 0 in
	while (let c = Char.code flow_data.[!sim_pos] in c >= 0x30 && c <= 0x39) do
	  let c = Char.code flow_data.[!sim_pos] in
	  ret := !ret * 10 + (c - 0x30);
	  incr sim_pos;
	done;
	!ret
      | _ -> wrong_args "getnum");
    "save", (fun (base_pos, _sim_pos, flow_data) -> function | [start_pos; end_pos] -> 
      let start_pos = start_pos - base_pos and end_pos = end_pos - base_pos in
      let str = try String.sub flow_data (start_pos+1) (end_pos - start_pos) |> String.trim |> String.lowercase with _ -> "??" in
      (try Hashtbl.find saves str |> incr with Not_found -> Hashtbl.add saves str (ref 1));
      if debug_ca then printf "S"; 
      incr matches; 0
      | _ -> wrong_args "save"
    );
  ]

 let get_f str = 
    try List.assoc str !ca_functions
    with Not_found -> 
      failwith ("Unknown function name: " ^ str)

let register_f fn f = ca_functions := (fn,f) :: !ca_functions
