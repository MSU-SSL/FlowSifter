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

let run_act rerun state vars (var, act) =
  try vars.(var) <- val_a_exp state vars.(var) act
  with Ns_types.Data_stall -> rerun := (var,act) :: !rerun;

type 'a opt_rules = ('a option * ('a -> 'a option) * ('a option -> 'a option -> 'a option) * ('a -> 'a -> int))
(* rules for handling decisions when creating the DFA *)
let dec_rules comp =
  let lowest_precedence a b = match a,b with
    | None, None -> None
    | Some r, None | None, Some r -> Some r
    | Some r1, Some r2 -> if comp r1 r2 = -1 then Some r1 else Some r2 in
  (None, (fun i -> Some i),lowest_precedence, comp: 'a opt_rules)

type ca_data = (int * a_exp) list * int
type 'a pri_dec = {pri: int; item: 'a} 

let comp_dec d1 d2 = Int.compare d1.pri d2.pri

let dec_eq a b = match a,b with
  | Some {pri=p1; item=(ps1,n1)}, Some {pri=p2; item=(ps2,n2)} ->
      p1 = p2 && n1 = n2 && List.length ps1 = List.length ps2 && 
        List.for_all2 (fun (v1,e1) (v2,e2) -> v1 = v2 && aeq e1 e2) ps1 ps2
  | _ -> false

let print_dfa_dec oc d =
  let so = Option.print Int.print in
  Printf.fprintf oc "{%d,%a,%a}%!" 
    d.pri (List.print print_action) (fst d.item) so (snd d.item)

let print_iaction oc (i,e) = fprintf oc "(%d) := %a" i (print_a_exp ("(" ^ string_of_int i ^ ")")) e
let print_reg_rule oc rr = 
  let so = Option.print String.print in
  let io = Option.print Int.print in
  Printf.fprintf oc "{p:%d; rx:%a acts:%a nt: %a}" 
    rr.prio so rr.rx (List.print print_iaction) rr.act io rr.nt

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
(*  printf "Making dfa of \n%a\n%!" (List.print print_reg_rule) rules;*)
  List.enum rules
  |> Enum.filter_map make_rx_pair
  |> Pcregex.rx_of_dec_strings ~anchor:true
  |> Minreg.of_reg
  |> Nfa.build_dfa ~labels:false (dec_rules comp_dec) 
  |> Regex_dfa.minimize ~dec_comp:dec_eq
  |> Regex_dfa.to_array


let fill_cache cached_compile ca = 
  Enum.iter (cached_compile |- ignore) (get_all_rule_groups ca)

let null_env = (0,ref 0, "")

(* get the decisions from a CA for the given NT(q) and with predicates
   satisfying vars *)

let get_rules_bits_aux var_sat pr_list =
  let pred_satisfied pred = List.for_all var_sat pred in
  let bitvect a (p,_) = if pred_satisfied p then (a lsl 1) + 1 else a lsl 1 in
  List.fold_left bitvect 0 pr_list

let get_rules_bits state pr_list vars =
  let var_satisfied (v,p) = val_p_exp state vars.(v) p in
  get_rules_bits_aux var_satisfied pr_list 

let get_rules_bits_uni state pr_list i =
  let var_satisfied (_v,p) = val_p_exp state i p in
  get_rules_bits_aux var_satisfied pr_list 

let is_univariate_predicate rs =
  let v = ref None in
  let is_v x = match !v with None -> v := Some x; true | Some v -> v = x in
  let test (p,_) = List.for_all (fun (v,pexp) -> is_v v && is_clean_p pexp) p in
  if List.for_all test rs then !v else None

let get_rules_v i rules =
  let var_satisfied (_,p) = val_p_exp null_env i p in
  let pred_satisfied pred = List.for_all var_satisfied pred in
  List.filter_map (fun (p,e) -> if pred_satisfied p then Some e else None) rules

let rec get_comb_aux i = function
  | [] -> []
  | (_p,rs)::t when i land 1 = 1 -> rs :: get_comb_aux (i lsr 1) t
  | _::t (* i land 1 = 0 *) -> get_comb_aux (i lsr 1) t

let get_comb i rs = get_comb_aux i (List.rev rs)

(*let rules_p = Point.create "rules"*)

let var_max = 255

(** Removes predicate checks at runtime for non-terminals with no predicates *)
let optimize_preds ca =
  let opt_prod _i rules =
    if List.for_all (fun (p,_) -> List.length p = 0) rules then
      let dfa = List.map snd rules |> compile_ca in
      if Ns_types.debug_ca then 
	printf "#DFA: %d\n%a\n" _i (Regex_dfa.print_array_dfa (fun oc {item=(_,q)} -> Int.print oc q)) dfa;
      (fun _ _ -> dfa)
    else 
      if List.length rules < 20 then (*TODO: PARTITION RULES BY PREDICATE *)
	let cas = Array.init (1 lsl (List.length rules)) 
	  (fun i -> get_comb i rules |> compile_ca) 
	in
	match is_univariate_predicate rules with
	    Some v -> 
	      let get_rs i = cas.(get_rules_bits_uni null_env rules i) in
	      let rule_map = Array.init (var_max+1) get_rs in
	      (fun vars _ -> rule_map.(vars.(v) land var_max))
	  | None ->  
	    (fun vars parse_state ->
	      cas.(get_rules_bits parse_state rules vars) )
      else (
	printf "Cannot optimize rules, too many rules:\n%a\n%!" (List.print ~sep:"\n" print_reg_rule) (List.map snd rules);
	exit 1;
      )
  in
  Array.mapi opt_prod ca

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
    if q_next_id = -1 then (
      if debug_ca then printf "%C done" input.[pos]; 
      Dec (decision, dec_pos)
    ) else if q_next_id = q.Regex_dfa.id then ( (* TODO: TEST OPTIMIZATION *)
      if debug_ca then printf "%C" input.[pos];
      let dec_pos = if q.Regex_dfa.dec = None then dec_pos else (pos+1) in
      resume_arr qs input pri decision dec_pos q (pos+1)
    ) else
      let q = Array.unsafe_get qs q_next_id in
      if debug_ca then printf "%C->%d " input.[pos] q_next_id;
      let pos = pos+1 in 
      match q.Regex_dfa.dec with
	| Some d when d.pri <= pri -> 
	  if debug_ca then printf "M%d " (snd d.item);
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

type ca_next = (int * a_exp) list * int
type 'a state = ('a option, int array, ca_next pri_dec option) Regex_dfa.state

type 'a ca_resume = 
  | Done 
  | Dfa of 'a state array * 'a state * int * ca_next * int * string
  | Ca of (int * a_exp) list * int

let init_state dfa pos =
  let q0 = dfa.Regex_dfa.q0 in
  match q0.Regex_dfa.dec with 
      Some {pri=p; item=i} -> Dfa (dfa.Regex_dfa.qs, q0, p, i, pos, "")
    | None -> Dfa (dfa.Regex_dfa.qs, q0, max_int, ([], null_state), pos, "")

let ca_trans = ref 0 

(* let () = at_exit (fun () -> printf "#CA Transitions: %d\n" !ca_trans)  *)

let rec simulate_ca_string ~ca ~vars fail_drop skip_left base_pos flow_data =
  let flow_len = String.length flow_data in
  let pos = ref 0 in
  let rerun = ref [] in
  let rec run_d2fa qs q pri item ri tail_data =
    if !pos >= flow_len then ( (* skipped past end of current packet *)
      skip_left := !pos - flow_len; 
      let ri_pos = !base_pos + ri in
      base_pos := !base_pos + flow_len;
      Dfa (qs, q, pri, item, ri_pos, "")
    ) else if !pos < 0 then ( (* handle DFA backtrack into previous packet *)
      (* TODO: optimize backtracking? *)
      let new_ri = ri + !base_pos in
      base_pos := !base_pos + !pos;
      simulate_ca_string ~ca ~vars fail_drop skip_left base_pos 
	(tail_data ^ flow_data) (Dfa (qs, q, pri, item, new_ri, ""))
    ) else
      let dfa_result = resume_arr qs flow_data pri item ri q !pos in
      match dfa_result with
	| Dec ((acts,q_next),pos_new) -> 
	  parsed_bytes := !parsed_bytes + (pos_new - !pos);
	  pos := pos_new;
	  run_ca acts q_next; (* Run the CA *)
	| End_of_input (q_final, pri, item, ri) -> 
	  parsed_bytes := !parsed_bytes + (String.length flow_data - !pos);
	  let tail_out = 
	    if ri < 0 then tail_data ^ flow_data 
	    else String.tail flow_data ri 
	  in
	  let ri_pos = !base_pos + ri in
	  base_pos := !base_pos + flow_len;
	  Dfa (qs,q_final,pri,item, ri_pos,tail_out)
  and run_ca acts q_next =
    if debug_ca then printf "\nCA: %d @ pos %d(%d)" 
      q_next (!pos + !base_pos) !pos;
    incr ca_trans; 
    let ca_state = (!base_pos, pos, flow_data) in
    if acts <> [] then List.iter (run_act rerun ca_state vars) acts;
    if !rerun <> [] then Ca (!rerun, q_next)
    else if q_next = null_state then (
      fail_drop := !fail_drop + (flow_len - !pos);
      Done
    ) else
      let dfa = ca.(q_next) vars ca_state in
      match dfa.Regex_dfa.q0.Regex_dfa.dec with
	| None -> 
	  run_d2fa dfa.Regex_dfa.qs dfa.Regex_dfa.q0 
	    max_int ([],null_state) 
	    !pos ""
	| Some {pri=p; item=i} ->
	  run_d2fa dfa.Regex_dfa.qs dfa.Regex_dfa.q0 
	    p i 
	    !pos ""
  in
  function
    | Dfa (qs, q, pri, item, ri, tail_data) ->
      run_d2fa qs q pri item (ri - !base_pos) tail_data
    | Ca (runme,q_next) -> run_ca runme q_next
    | Done -> fail_drop := !fail_drop + flow_len; Done

