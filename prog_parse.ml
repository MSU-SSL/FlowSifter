open Batteries

open Ns_types
open Simplify
open ParsedPCFG
open Ns_parse

let pkt_skip = ref 0
let pkt_partial_skip = ref 0
let parse_fail = ref 0
let parsers = ref 0

let () = at_exit (fun () -> Printf.printf "#Parsers generated: %d  Parse failures: %d\n" !parsers !parse_fail)
(* generates an incremental parser for the language defined by a 
   protocol grammar specification and extraction language *)
let gen_parser p e = 
  let proto = parse_file_as_spec p 
  and extr = parse_file_as_extraction e in
  let ca0 = merge_cas ~proto ~extr |> regularize in
  let ca,var_count = destring extr.start ca0 in
  let ca_cache = ref Map.empty in
(*  fill_cache ca_cache ca; *)
  let compile_ca = compile_ca ca_cache in
  let ca = optimize ca compile_ca in
  let dfa0 = ca.(0) (Array.make var_count 0) (0, ref 0, "") in
  fun () -> (* allow creating many parsers *)
    incr parsers;
    let vars = Array.make var_count 0 in    
    let q = ref (init_state dfa0 0)
    and skip_left = ref 0
    and base_pos = ref 0 in
    let rec parse str =
      let len = String.length str in
      if !skip_left = 0 then 
	try 
	  q := simulate_ca_string ~ca ~vars skip_left base_pos str !q;
	  base_pos := !base_pos + len;
	with Ns_parse.Parse_failure -> 
	  incr parse_fail;
      else (* skip_left > 0 *)
	if !skip_left >= len then ( (* skip the packet entirely *)
	  skip_left := !skip_left - len; 
	  base_pos := !base_pos + len; 
	  incr pkt_skip
	) else (* parse only part of the packet *)
	  let str_to_parse = String.tail str !skip_left in
	  incr pkt_partial_skip;
	  skip_left := 0;
	  parse str_to_parse
    in
    parse

let new_parser s e = gen_parser s e
let add_data p d = p d
let get_event_count _p = !Ns_types.matches
let reset_parser _p = assert false
let delete_parser _p = ()

(*
let _ = Callback.register "gen_parser" gen_parser
 *)
