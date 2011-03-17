open Batteries_uni
open Ns_types
open Simplify
open ParsedPCFG

let pkt_skip = ref 0
let fail_drop = ref 0

(*
let () = at_exit (fun () -> 
  Printf.printf "#Bytes dropped after desync: %a, total packet data: %d\n" Ean_std.print_size_B !fail_drop !pkt_data
)
  *)

(* generates an incremental parser for the language defined by a 
   protocol grammar specification and extraction language *)
let gen_parser p e = 
  let proto = Ns_parse.parse_file_as_spec p 
  and extr = Ns_parse.parse_file_as_extraction e in
  let ca,var_count = Ns_parse.merge_cas ~proto ~extr |> Ns_parse.regularize 
(*    |> Ns_parse.dechain extr.start*)
    |> Ns_parse.destring extr.start 
    |> first Ns_run.optimize_preds
  in
  fun () -> (* allow creating many parsers *)
    let vars = Array.make var_count 0 in    
    let dfa0 = ca.(0) vars (0, ref 0, "") in
    let q = ref (Some (Ns_run.init_state dfa0 0))
    and skip_left = ref 0
    and base_pos = ref 0 in
    let rec parse str =
      if !skip_left = 0 then 
	match !q with
	  | None -> fail_drop := !fail_drop + String.length str
	  | Some q_in ->
	    q := Ns_run.simulate_ca_string 
	      ~ca ~vars fail_drop skip_left base_pos str q_in;
	    base_pos := !base_pos + String.length str;
      else (* skip_left > 0 *)
	let len = String.length str in
	if !skip_left >= len then ( (* skip the packet entirely *)
	  skip_left := !skip_left - len; 
	  base_pos := !base_pos + len; 
	  incr pkt_skip
	) else (* parse only part of the packet *)
	  let str_to_parse = String.tail str !skip_left in
	  skip_left := 0;
	  parse str_to_parse
    in
    parse

let new_parser s e = gen_parser s e
let add_data p _ d = p d
let get_event_count _p = !Ns_types.matches
let reset_parser _p = assert false
let delete_parser _p = ()

(*
let _ = Callback.register "gen_parser" gen_parser
 *)
