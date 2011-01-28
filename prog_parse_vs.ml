open Batteries

open Ns_types
open Simplify
open ParsedPCFG
open Ns_parse

(* generates an incremental parser for the language defined by a 
   protocol grammar specification and extraction language *)
let gen_parser ~boost ~stride p e = 
  let proto = parse_file_as_spec p 
  and extr = parse_file_as_extraction e in
  let ca = merge_cas ~proto ~extr |> regularize in
  let ca_cache = ref Map.empty in
  let () = at_exit (fun () -> Printf.printf "parsing automata generated: %d tcam entries\n" (Map.enum !ca_cache |> Enum.map (snd |- Vsdfa.tcam_size) |> Enum.reduce (+) )) in
(*  fill_cache ca_cache ca; *)
  let compile_ca = compile_ca_vs ~boost ~stride ~ca_cache in
  let dfa0 = get_rules (fun _ _ -> 0) ca extr.start Map.empty |> compile_ca in
  let default_q = (dfa0, dfa0.Regex_dfa.q0,(max_int, ([], None, -1))) in
  fun () -> (* allow creating many parsers *)
    let vars = ref Map.empty in    
    let q = ref default_q
    and base_pos = ref 0 in
    let sim_str = simulate_ca_string_vs ~compile_ca ~ca ~vars base_pos in
    fun d -> (* take input for the parser *)
      try 
	q := sim_str d !q; base_pos := !base_pos + (String.length d)
      with No_next_state -> 
	q := default_q; base_pos := !base_pos + (String.length d)

let new_parser = gen_parser ~boost:7 ~stride:2 "spec.ca" "extr.ca"
let add_data p d = p d
let get_event_count _p = !Ns_parse.matches
let reset_parser _p = assert false
let delete_parser _ = ()


(*
let _ = Callback.register "gen_parser" gen_parser
 *)
