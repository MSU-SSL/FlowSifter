open Batteries
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

let run_act st (var, act) =
  try
    let new_val = val_a_opt Ns_types.get_f st st.vars.(var) act in
    if debug_ca then printf "$%d := %d " var new_val;
    st.vars.(var) <- new_val
  with Ns_types.Data_stall -> st.rerun <- (var,act) :: st.rerun;

type ca_data = (int * a_exp) list * int

let print_dfa_dec oc (a,b) =
  let so = Option.print Int.print in
  Printf.fprintf oc "{%a,%a}%!"
    (List.print print_action) a so b

let print_iaction oc (i,e) = fprintf oc "(%d) := %a" i (print_a_exp ("(" ^ string_of_int i ^ ")")) e
let print_reg_rule oc rr =
  let so = Option.print String.print in
  let io = Option.print Int.print in
  Printf.fprintf oc "{p:%d; rx:%a acts:%a nt: %a}"
    rr.prio so rr.rx (List.print print_iaction) rr.act io rr.nt

(*let comp_p = Point.create "comp_ca"
let comp_t = Time.create "comp_ca" *)

let make_rx_pair r =
  let rx = (* remove /rx/'s slashes *)
    match r.rx with
      | None -> ""
      | Some rx -> (rx |> String.lchop |> String.rchop)
  in
  (r.act,Option.default (-1) r.nt),
  rx, [`Extended; `Pri r.prio]

let merge_dec (_act1,nt1 as a) (_act2, nt2 as b) = if nt1 > nt2 then a else b

let gen_arr_dfa optimize_state_acts rules =
  let dec_ops  = {Regex_dfa.dec0 = ([],-1); merge = merge_dec; cmp = compare} in
  (* duplicate of dec_opts for typing purposes, because dec_opts isn't
     fully polymorphic *)
  let new_dops = {Regex_dfa.dec0 = ([],-1); merge = merge_dec; cmp = compare} in
  List.enum rules
  |> Enum.map make_rx_pair
  |> Pcregex.rx_of_dec_strings ~anchor:true
  |> Minreg.of_reg
  |> Nfa.build_dfa ~labels:false dec_ops
  |> Regex_dfa.minimize
  |> Regex_dfa.to_array
  |> Regex_dfa.map_dec new_dops optimize_state_acts

let gen_vsdfa ~boost ~stride optimize_state_acts rules =
  let null_dec = [],-1 in
  let dec_ops  = {Regex_dfa.dec0 = null_dec; merge = merge_dec; cmp = compare} in
  (* duplicate of dec_opts for typing purposes, because dec_opts isn't
     fully polymorphic *)
  let new_dops = {Regex_dfa.dec0 = null_dec; merge = merge_dec; cmp = compare} in
  List.enum rules
  |> Enum.map make_rx_pair
  |> Pcregex.rx_of_dec_strings ~anchor:true
  |> Minreg.of_reg
  |> Nfa.build_dfa ~labels:false dec_ops
  |> Regex_dfa.minimize
  |> D2fa.of_dfa
  |> Vsdfa.of_d2fa Int.compare
       (*       |> tap (fun _ -> eprintf "Vsdfa built\n%!") *)
  |> Vsdfa.increase_stride_all ~cmp:Int.compare ((=) ([],-1)) ~com_lim:max_int (stride-1)
       (*       |> tap (fun _ -> eprintf "Stride increased\n%!") *)
  |> Vsdfa.boost ~cmp:Int.compare ((=) ([],-1)) ~loop_lim:160 ~boost:(boost-1)
  |> Regex_dfa.map_dec new_dops optimize_state_acts

(* Compiles a list of rules into an automaton with decisions of
   (priority, action, nt) *)
let compile_ca_gen gen_dfa rules =
  let freeze_acts acts = List.map (fun (v,a) -> v, freeze_a get_f a) acts in
  let optimize_state_acts {Regex_dfa.id=id;pri=pri;label=label; map=map; dec=(acts, q_next); dec_pri=dec_pri} =
    {Regex_dfa.id=id; pri=pri; label=label; map=map; dec=(freeze_acts acts, q_next); dec_pri=dec_pri}
  in
(*  printf "Making dfa of \n%a\n%!" (List.print print_reg_rule) rules;*)
  match rules with
    | [{rx=None; act=act; nt=nt}] ->
      `Ca (freeze_acts act, Option.default (-1) nt)
    | _ ->
      `Dfa (gen_dfa optimize_state_acts rules)

let compile_ca rules = compile_ca_gen gen_arr_dfa rules
let compile_ca_vs ~boost ~stride rules = compile_ca_gen (gen_vsdfa ~boost ~stride) rules


let fill_cache cached_compile ca =
  Enum.iter (cached_compile %> ignore) (get_all_rule_groups ca)

let null_env = (0,ref 0, "")

(* get the decisions from a CA for the given NT(q) and with predicates
   satisfying vars *)

let get_rules_bits_aux var_sat pr_list =
  let pred_satisfied pred = List.for_all var_sat pred in
  let bitvect a (p,_) = if pred_satisfied p then (a lsl 1) + 1 else a lsl 1 in
  List.fold_left bitvect 0 pr_list

let get_rules_bits state pr_list =
  let var_satisfied (v,p) = val_p_exp Ns_types.get_f state state.vars.(v) p in
  get_rules_bits_aux var_satisfied pr_list

let get_rules_bits_uni state pr_list i =
  let var_satisfied (_v,p) = val_p_exp Ns_types.get_f state i p in
  get_rules_bits_aux var_satisfied pr_list

let is_univariate_predicate rs =
  let v = ref None in
  let is_v x = match !v with None -> v := Some x; true | Some v -> v = x in
  let test (p,_) = List.for_all (fun (v,pexp) -> is_v v && is_clean_p pexp) p in
  if List.for_all test rs then !v else None

(*
let get_rules_v i rules =
  let var_satisfied (_,p) = val_p_opt Ns_types.get_f null_env i p in
  let pred_satisfied pred = List.for_all var_satisfied pred in
  List.filter_map (fun (p,e) -> if pred_satisfied p then Some e else None) rules
*)
let rec get_comb_aux i = function
  | [] -> []
  | (_p,rs)::t when i land 1 = 1 -> rs :: get_comb_aux (i lsr 1) t
  | _::t (* i land 1 = 0 *) -> get_comb_aux (i lsr 1) t

let get_comb i rs = get_comb_aux i (List.rev rs)

(*let rules_p = Point.create "rules"*)

(*let var_max = 255*)
let print_iact_opt oc (i,e) = match e with
  | Fast_a _ -> fprintf oc "$%d := Fast" i
  | Slow_a e -> fprintf oc "$%d := %a" i (print_a_exp ("$" ^ string_of_int i)) e



let print_vars oc m =
  Array.print Int.print oc m
(*
let sim_p = Point.create "sim"
let sim_t = Time.create "sim"
*)

exception Parse_complete

type ('a, 'b) resume_ret =
  | End_of_input of 'a * int * 'b * int
  | Dec of 'b * int
open Regex_dfa

let rec resume_arr qs input pri decision dec_pos q pos =
  if pos >= String.length input then (
    if debug_ca then printf "EOI: q:%d pri:%d dec_pos:%d\n" q.id pri dec_pos;
    End_of_input (q,pri,decision,dec_pos)
  ) else
    let q_next_id = Array.unsafe_get q.map (Char.code (String.unsafe_get input pos)) in
    if debug_ca then printf "%C->%d " input.[pos] q_next_id;
    if q_next_id = -1 then
      Dec (decision, dec_pos)
(*
   ) else if q_next_id = q.id then ( (* TODO: TEST OPTIMIZATION *)
      if debug_ca then printf "%C" input.[pos];
      let dec_pos = if q.dec = None then dec_pos else (pos+1) in
      resume_arr qs input pri decision dec_pos q (pos+1)
 *)
    else
      let q = Array.unsafe_get qs q_next_id in
      if debug_ca then printf "(%d)" q.pri;
      let pos = pos+1 in
      if q.pri < pri then
	Dec (decision, dec_pos)
      else
	if q.dec_pri >= pri then
	  resume_arr qs input q.dec_pri q.dec pos q pos
	else
	  resume_arr qs input pri decision dec_pos q pos
(*
let test_dfa = [{pri=1; item=0}, "[abcxyz].*[bahd].*\n", []] |> List.enum |> Pcregex.rx_of_dec_strings |> Minreg.of_reg |> Regex_dfa.build_dfa ~labels:false (dec_rules comp_dec) |> Regex_dfa.minimize ~dec_comp:(=) |> Regex_dfa.to_array

open Benchmark

let () =
  throughput1 3 (resume_arr test_dfa.Regex_dfa.qs (String.create (1024*1024/8)) 99 2 0 test_dfa.Regex_dfa.q0) 0 |> tabulate
*)
let null_state = -1

(*let init_state dfa pos = match dfa with
  | `Dfa dfa -> let q0 = dfa.q0 in Dfa (dfa.qs, q0, q0.dec_pri, q0.dec, pos, "")
  | `Ca (acts, q_next) -> Ca (acts, q_next)
*)
(* let () = at_exit (fun () -> printf "#CA Transitions: %d\n" !ca_trans)  *)

let bookkeep st s = st.pos <- 0; st.flow_data <- s

let rec skip_to_pos resume st s =
  let flow_len = String.length s in
  if st.base_pos + flow_len <= st.pos then (
    st.base_pos <- st.base_pos + flow_len;
    Waiting (skip_to_pos resume st)
  ) else (* parse part of the packet *)
    resume (String.tail s (st.pos - st.base_pos))

let rec done_f st s = st.fail_drop <- st.fail_drop + String.length s; Waiting (done_f st)

let rec run_d2fa qs q pri item ri tail_data st =
  let flow_len = String.length st.flow_data in
  if st.pos >= flow_len then ( (* skipped past end of current packet *)
    st.base_pos <- st.base_pos + flow_len;
    let resume = (fun s -> bookkeep st s; run_d2fa qs q pri item (ri - flow_len) "" st) in
    if st.pos > flow_len then Waiting (skip_to_pos resume st) else Waiting resume
  ) else if st.pos < 0 then ( (* handle DFA backtrack into previous packet *)
    (* TODO: optimize backtracking? *)
    st.base_pos <- st.base_pos + st.pos;
    run_d2fa qs q pri item (ri - st.pos) "" {st with flow_data = tail_data ^ st.flow_data}
  ) else
      let dfa_result = resume_arr qs st.flow_data pri item ri q st.pos in
      match dfa_result with
	| Dec ((acts,q_next),pos_new) ->
	  st.pos <- pos_new;
	  run_ca acts q_next st; (* Run the CA *)
	| End_of_input (q_final, pri, item, ri) ->
	  let tail_out =
	    if ri < 0 then tail_data ^ st.flow_data
	    else String.tail st.flow_data ri
	  in
	  st.base_pos <- st.base_pos + flow_len;
	  Waiting (fun s -> bookkeep st s; run_d2fa qs q_final pri item (ri - flow_len) tail_out st)
and run_ca acts q_next st =
  if debug_ca then printf "\nCA: %d @ pos %d(%d)" q_next (st.pos + st.base_pos) st.pos;
  if acts <> [] then List.iter (run_act st) acts;
  if st.rerun <> [] then (* need more input to satisfy functions *)
    Waiting (fun s -> bookkeep st s; run_ca st.rerun q_next st)
  else if q_next = null_state then ( (* CA has no next state to go to *)
    st.fail_drop <- st.fail_drop + (String.length st.flow_data - st.pos);
    Waiting (done_f st)
  ) else
    if st.pos >= String.length st.flow_data then (* No more data to process, need more *)
      Waiting (fun s -> bookkeep st s; run_ca [] q_next st)
    else
      st.ca.(q_next) st

(** Removes predicate checks at runtime for non-terminals with no predicates *)
let optimize_preds_gen compile_ca run_dfa ca =
  let link_run_fs _i = function
    | `Dfa dfa ->
(*      if Ns_types.debug_ca then
	printf "#DFA: %d\n%a\n" i (Regex_dfa.print_array_dfa (fun oc (_,q) -> Int.print oc q)) dfa; *)
      let q0 = dfa.q0 in
      (fun st -> run_dfa dfa.qs q0 q0.dec_pri q0.dec st.pos "" st)
    | `Ca (acts, next_ca) ->
      if Ns_types.debug_ca then
	printf "#CA: %a %d\n" (List.print print_iact_opt) acts next_ca;
      (run_ca acts next_ca)
  in
  let opt_prod idx rules =
    if List.for_all (fun (p,_) -> List.length p = 0) rules then
      List.map snd rules |> compile_ca |> link_run_fs idx
    else
      if List.length rules < 20 then (*TODO: PARTITION RULES BY PREDICATE *)
	let cas = Array.init (1 lsl (List.length rules))
	  (fun ci -> get_comb ci rules |> compile_ca |> link_run_fs idx)
	in
	(fun st -> cas.(get_rules_bits st rules) st)
      else (
	printf "Cannot optimize rules, too many rules:\n%a\n%!" (List.print ~sep:"\n" print_reg_rule) (List.map snd rules);
	exit 1;
      )
  in
  Array.mapi opt_prod ca

let optimize_preds ca = optimize_preds_gen compile_ca run_d2fa ca
