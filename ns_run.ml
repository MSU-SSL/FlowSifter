open Batteries_uni
open Printf
open Ns_types
open ParsedPCFG

let get_all_rule_groups (ca:regular_grammar_arr) =
  let groups_of_rlist rs =
    let no_pred,with_pred = 
      List.partition (fun (p,_r) -> List.length p = 0) rs 
    in
    let rec subsets = function
	[] -> []
      | (_p,r)::t -> (List.map (List.cons r) (subsets t)) @ (subsets t)
    in
    let no_pred_rules = List.map snd no_pred in
    List.map ((@) no_pred_rules) (subsets with_pred) |> List.enum
  in
  Array.enum ca |> map groups_of_rlist |> Enum.flatten

let run_act state vars (var, act) =
  vars.(var) <- eval_a_exp state act vars.(var)

type 'a opt_rules = ('a option * ('a -> 'a option) * ('a option -> 'a option -> 'a option))
(* rules for handling decisions when creating the DFA *)
let dec_rules comp =
  let lowest_precedence a b = match a,b with
    | None, None -> None
    | Some r, None | None, Some r -> Some r
    | Some r1, Some r2 -> if comp r1 r2 = -1 then Some r1 else Some r2 in
  (None, (fun i -> Some i),lowest_precedence: 'a opt_rules)

type ca_data = (int * a_exp) list * int
type 'a pri_dec = {pri: int; item: 'a} 

let comp_dec d1 d2 = Int.compare d1.pri d2.pri

let dec_eq a b = match a,b with
    Some {pri=p1; item=(ps1,n1)}, Some {pri=p2; item=(ps2,n2)} ->
      p1 = p2 && n1 = n2 && List.length ps1 = List.length ps2 && List.for_all2 (fun (v1,e1) (v2,e2) -> v1 = v2 && aeq e1 e2) ps1 ps2
| _ -> false

let print_dfa_dec oc d =
  let so = Option.print Int.print in
  Printf.fprintf oc "{%d,%a,%a}%!" 
    d.pri (List.print print_action) (fst d.item) so (snd d.item)


(*let comp_p = Point.create "comp_ca"
let comp_t = Time.create "comp_ca" *)

let make_rx_pair r =
  let to_pair rx = 
    ({pri=r.prio; item=(r.act,Option.default (-1) r.nt)} : ca_data pri_dec), 
    (rx |> String.lchop |> String.rchop), (* remove /rx/'s slashes *)
    [`Extended]
  in
  Option.map to_pair r.rx 
    
(* Compiles a list of rules into an automaton with decisions of
   (priority, action, nt) *)
let compile_ca rules =
      List.enum rules
      |> Enum.filter_map make_rx_pair
      |> Pcregex.rx_of_dec_strings ~anchor:true
      |> Minreg.of_reg
      |> Regex_dfa.build_dfa ~labels:false (dec_rules comp_dec)
      |> Regex_dfa.minimize ~dec_comp:dec_eq
      |> Regex_dfa.to_array

let fill_cache cached_compile ca = 
  Enum.iter (cached_compile |- ignore) (get_all_rule_groups ca)

let print_vars oc m = 
  Array.print Int.print oc m
(*
let sim_p = Point.create "sim"
let sim_t = Time.create "sim"
*)
let parsed_bytes = ref 0

exception Parse_complete

type ('a, 'b) resume_ret = 
  | End_of_input of 'a * int * 'b * int
  | Dec of 'b * int

let rec resume_arr qs input pri decision dec_pos q pos =
  if pos >= String.length input then (
    if debug_ca then printf "EOI: q:%d pri:%d dec_pos:%d\n" q.Regex_dfa.id pri dec_pos;
    End_of_input (q,pri,decision,dec_pos)
  ) else
    let q_next_id = Array.unsafe_get q.Regex_dfa.map (Char.code (String.unsafe_get input pos)) in
    if debug_ca then printf "DFA %C->%d " input.[pos] q_next_id;
    if q_next_id = -1 then Dec (decision, dec_pos)
    else
      let q = Array.unsafe_get qs q_next_id in
      let pos = pos+1 in 
      match q.Regex_dfa.dec with
	| Some d when d.pri <= pri -> 
	  if debug_ca then printf "M%d " d.pri;
	  resume_arr qs input d.pri d.item pos q pos
	| _ -> 
	  resume_arr qs input pri decision dec_pos q pos
(*
let test_dfa = [{pri=1; item=0}, "[abcxyz].*[bahd].*\n", []] |> List.enum |> Pcregex.rx_of_dec_strings |> Minreg.of_reg |> Regex_dfa.build_dfa ~labels:false (dec_rules comp_dec) |> Regex_dfa.minimize ~dec_comp:(=) |> Regex_dfa.to_array

open Benchmark

let () = 
  throughput1 3 (resume_arr test_dfa.Regex_dfa.qs (String.create (1024*1024/8)) 99 2 0 test_dfa.Regex_dfa.q0) 0 |> tabulate
*)
let null_state = -1

let init_state dfa pos =
  let q0 = dfa.Regex_dfa.q0 in
  match q0.Regex_dfa.dec with 
      Some {pri=p; item=i} -> (dfa.Regex_dfa.qs, q0, p, i, pos, "")
    | None -> (dfa.Regex_dfa.qs, q0, max_int, ([], null_state), pos, "")

let ca_trans = ref 0 

(* let () = at_exit (fun () -> printf "#CA Transitions: %d\n" !ca_trans)  *)

let is_printable c = c = '\n' || (Char.code c >= 32 && Char.code c <= 126)
let clean_unprintable s = String.map (fun x -> if is_printable x then x else '.') s

let rec simulate_ca_string ~ca ~vars fail_drop skip_left base_pos flow_data (qs, q, pri, item, ri, tail_data) = 
  if debug_ca then Printf.printf "P:%s\n" (clean_unprintable flow_data);
  let flow_len = String.length flow_data in
  let pos = ref 0 in
  let rec run_d2fa qs q pri item ri tail_data =
    if !pos >= flow_len then ( (* skipped past end of current packet *)
      skip_left := !pos - flow_len; 
      Some (qs, q, pri, item, !base_pos + ri, "")
    ) else if !pos < 0 then ( (* handle DFA backtrack into previous packet *)
      (* TODO: optimize backtracking? *)
      let new_ri = ri + !base_pos in
      base_pos := !base_pos + !pos;
      simulate_ca_string ~ca ~vars fail_drop skip_left base_pos (tail_data ^ flow_data) (qs, q, pri, item, new_ri, "")
    ) else
      let dfa_result = resume_arr qs flow_data pri item ri q !pos in
      match dfa_result with
	| Dec ((acts,q_next),pos_new) -> 
	  if debug_ca then printf "CA: %d @ pos %d(%d)\n" q_next (pos_new + !base_pos) pos_new;
	  incr ca_trans; 
	  parsed_bytes := !parsed_bytes + (pos_new - !pos);
	  pos := pos_new;
	  let ca_state = (!base_pos, pos, flow_data) in
	  if acts <> [] then List.iter (run_act ca_state vars) acts;
	  if q_next = null_state then (
	    fail_drop := !fail_drop + (flow_len - pos_new);
	    None
	  ) else
	    let dfa = ca.(q_next) vars ca_state in
	    ( match dfa.Regex_dfa.q0.Regex_dfa.dec with
	      | None -> 
		run_d2fa dfa.Regex_dfa.qs dfa.Regex_dfa.q0 max_int ([],null_state) !pos ""
	      | Some {pri=p; item=i} ->
		run_d2fa dfa.Regex_dfa.qs dfa.Regex_dfa.q0 p i !pos ""
	    )
	| End_of_input (q_final, pri, item, ri) -> 
	  parsed_bytes := !parsed_bytes + (String.length flow_data - !pos);
	  let tail_out = 
	    if ri < 0 then tail_data ^ flow_data 
	    else String.tail flow_data ri 
	  in
	  Some (qs,q_final,pri,item, !base_pos + ri,tail_out)
		
  in
  run_d2fa qs q pri item (ri - !base_pos) tail_data

