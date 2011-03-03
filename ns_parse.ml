open Batteries_uni
open Printf
open Ns_types
open Simplify
open ParsedPCFG

(*open Ocamlviz *)

let capture_counter = let x = ref 0 in fun () -> incr x; !x

let parse_preds s = 
  try 
    Lexing.from_string s |> Ns_yac.pred Ns_lex.pred_token 
  with Ns_yac.Error -> 
    failwith (sprintf "Failed to parse %s as predicate" s)

let parse_acts s = 
  match s with 
      None -> VarMap.empty 
    | Some s -> 
	  try 
	    Lexing.from_string s |> Ns_yac.act Ns_lex.act_token
	  with Ns_yac.Error ->
	    failwith (sprintf "Failed to parse %s as action" s)

let hook_act act_map l = (Nonterm "@EPSILON", act_map) :: l

let full_parse (nt,predopt,priopt,rules) =
  let pri = Option.default default_priority priopt in
  let preds = match predopt with None -> VarMap.empty | Some str -> parse_preds str in
  let rec expand_term_act acc = function
    | Capture rules, Some func ->
	let cap_id = "capture(" ^ (capture_counter () |> string_of_int) ^")" in
	let get_pos = Function ("pos", get_f "pos", []) in
	let start_cap = VarMap.add cap_id get_pos VarMap.empty in
	let end_exp = Function (func, get_f func, [Variable; Sub (get_pos, Constant 1)]) in
	let end_cap = VarMap.add cap_id end_exp VarMap.empty in
	List.fold_left expand_term_act (hook_act start_cap acc) rules
          |> hook_act end_cap
    | Capture _, None -> failwith "Cannot have a capture without a function to send it to"
    | Nt x, act -> (Nonterm x, parse_acts act) :: acc
    | Regex x, act -> (Term x, parse_acts act) :: acc
  in
  let items = List.fold_left expand_term_act [] rules |> List.rev in
(*  printf "pri: %d\n" pri; *)
  {name=nt; predicates=preds; priority=pri; expression = items}

(* TYPE: proto:grammar -> extr:grammar -> grammar *)
let merge_cas : proto:grammar -> extr:grammar -> grammar = 
  fun ~proto ~extr -> approx_grammar ~x_grammar:extr ~p_grammar:proto

type var_id = string
type ca_state = string
type ('a, 'b) regular_rule = {prio : int; rx : string option;
		     act: ('a * a_exp) list; nt : 'b option}
type pred = Ns_types.ParsedPCFG.p_exp Ns_types.ParsedPCFG.VarMap.t
type regular_grammar = (ca_state, (pred * (string, string) regular_rule) list) Map.t


type pred_arr = (int * Ns_types.ParsedPCFG.p_exp) list
type regular_grammar_arr = (pred_arr * (int, int) regular_rule) list array

type compiled_rules = int
type regular_grammar_opt = (int array -> (string -> int list -> int) -> compiled_rules) array

let print_reg_rule oc rr = 
  let so = Option.print String.print in
  Printf.fprintf oc "{p:%d; rx:%a acts:%a nt: %a}%!" 
    rr.prio so rr.rx (List.print print_action) rr.act so rr.nt

let print_reg_ca oc (ca: regular_grammar) = 
  Map.print String.print (Pair.print (print_varmap print_pred) print_reg_rule |> List.print ~first:"\n  " ~sep:"\n  " ~last:"\n") oc ca;
  fprintf oc "Total: %d states\n" (Map.fold (fun _ x -> x+1) ca 0)

let print_iaction oc (i,e) = fprintf oc "(%d) := %a" i (print_a_exp ("(" ^ string_of_int i ^ ")")) e

let print_ipred oc (i,e) = print_p_exp (string_of_int i) oc e


let print_opt_rule oc iirr =
  let so = Option.print String.print in
  let io = Option.print Int.print in
  Printf.fprintf oc "{p:%d; rx:%a acts:%a nt: %a}%!" 
    iirr.prio so iirr.rx (List.print print_iaction) iirr.act io iirr.nt
  
let print_rule oc x = Pair.print (List.print print_ipred) print_opt_rule oc x

let print_rules oc = function
  | [r] -> print_rule oc r
  | l -> List.print ~first:"\n  " ~sep:"\n  " ~last:"\n" print_rule oc l

let print_reg_ds_ca oc (ca: regular_grammar_arr) = 
  Array.iteri (Regex_dfa.index_print print_rules oc) ca

exception Non_regular_rule of production

let merge_rx r s = 
  (r |> String.rchop) ^ (s |> String.lchop) 
  |> tap (eprintf "MERGE_RX: %s + %s -> %s\n" r s )

let regularize : grammar -> regular_grammar = fun grammar ->
  let make_r r =  
    let rec head_tail = function
      | [(Term a, head_act); (Nonterm b, tail_act)] ->  
	  {prio = r.priority; 
	   rx   = Some a; 
	   act  = 
	      act_act_compose head_act tail_act |> VarMap.enum |> List.of_enum;
	   nt   = Some b } 
      | [(Term a, head_act)] ->
	  {prio = r.priority;
	   rx   = Some a;
	   act  = VarMap.enum head_act |> List.of_enum;
	   nt   = None}
      | (Term a, head_act) :: (Term b, bhead_act) :: t -> head_tail ((Term (merge_rx a b), (act_act_compose head_act bhead_act)) :: t)
      | [] ->
	  {prio = r.priority;
	   rx   = None;
	   act  = [];
	   nt   = None } 
      | _ -> raise (Non_regular_rule r) 
	  
    in
    ( r.predicates, (head_tail r.expression ) )
  in 
  let make_regular_g g = 
    NTMap.enum g.rules |> map (second (List.map make_r)) |> Map.of_enum
  in 
  grammar |> normalize_grammar |> idle_elimination |> make_regular_g ;; 


let destring : (string -> regular_grammar -> (regular_grammar_arr * int)) = 
  fun start ca ->
    let next_avail_var = ref 0 in
    let var_ht = Hashtbl.create 10 in
    let to_int_v var = try Hashtbl.find var_ht var with Not_found -> Ref.post_incr next_avail_var |> tap (fun i -> Hashtbl.add var_ht var i) in
    let next_avail_ca = ref 0 in
    let ca_ht = Hashtbl.create 10 in
    let to_int_c ca_label = 
      try Hashtbl.find ca_ht ca_label
      with Not_found -> Ref.post_incr next_avail_ca
        |> tap (fun i -> Hashtbl.add ca_ht ca_label i) 
    (*	|> tap (fun i -> printf "CA %d from %s\n" i ca_label) *)
    in
    to_int_c start |> ignore; (* make sure start state is #0 *)
    let fix_pair (v,a) = (to_int_v v, a) in
    let fix_rule (r: (string, string) regular_rule) = {r with act = List.map fix_pair r.act; nt = Option.map to_int_c r.nt} in
    let fix_pred (p:pred) = VarMap.enum p |> Enum.map fix_pair |> List.of_enum in
    let pmap = Map.enum ca |> 
	Enum.map (fun (ca, pro) -> to_int_c ca, (List.map (fun (p,r) -> fix_pred p, fix_rule r) pro)) in
    Enum.force pmap;

    let ret = Array.create !next_avail_ca (Obj.magic 0) in
    Enum.iter (fun (ca, pro) -> ret.(ca) <- pro) pmap;
(*    printf "varmap: %a\nca_statemap: %a\n ca: %a\n"
      (Hashtbl.print String.print Int.print) var_ht
      (Hashtbl.print String.print Int.print) ca_ht
      print_reg_ds_ca ret; *)
    ret, !next_avail_var
  


(* get the decisions from a CA for the given NT(q) and with predicates
   satisfying vars *)
let get_rules state pr_list vars =
  let var_satisfied (v,p) = eval_p_exp state p vars.(v) in
  let pred_satisfied pred = List.for_all var_satisfied pred in
  List.fold_left (fun acc (p,_) -> let a = acc lsl 1 in if pred_satisfied p then a + 1 else a ) 0 pr_list

let is_univariate_predicate rs =
  let v = ref None in
  let is_v x = match !v with None -> v := Some x; true | Some v -> v = x in
  let test (p,_) = List.for_all (fun (v,pexp) -> is_v v && is_clean_p pexp) p in
  if List.for_all test rs then 
    !v
  else
    None

let get_rules_v i rules =
  let var_satisfied (_,p) = eval_p_exp (0,ref 0, "") p i in
  let pred_satisfied pred = List.for_all var_satisfied pred in
  List.filter_map (fun (p,e) -> if pred_satisfied p then Some e else None) rules
  
(*let rules_p = Point.create "rules"*)

let var_max = 255

(** Removes predicate checks at runtime for non-terminals with no predicates *)
let optimize ca compile_ca =
    let opt_prod rules =
      if List.for_all (fun (p,_) -> List.length p = 0) rules then
	let dfa = List.map snd rules |> compile_ca in
	(fun _ _ -> dfa)
      else match is_univariate_predicate rules with
	  Some v -> 
	    let rule_map = 
	      Array.init (var_max+1) (fun i -> get_rules_v i rules |> compile_ca)
	    in
	    (fun vars _ -> rule_map.(vars.(v) land var_max))
	| None ->  
	  let cas = Array.init (1 lsl (List.length rules)) (fun _ -> assert false) in
	  (fun vars parse_state ->
	    cas.(get_rules parse_state rules vars) )
    in
    Array.map opt_prod ca


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

let act_evals = ref 0

(* sets [var] to the result of [act] in the state in [acc] *)

(*
let run_act get_f vars (var,act) =
  let new_val = ref 0 in
  (* modify vars, storing the new value in new_val *)
  let ret = Map.modify_def 0 var (fun old_val -> new_val := eval_a_exp get_f act old_val; !new_val) vars in
  (* before returning the new vars, update the max_vals map *)
  max_vals := Map.modify_def !new_val var (max !new_val) !max_vals;
  ret
  *)

(*let upd_p = Point.create "rules_upd" *)

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
let compile_ca ca_cache =
  (* TODO: pre-compile all possible rule combinations from the given ca *)
  (fun rules ->
    (*    Point.observe comp_p;
	  Time.start comp_t; *)
    try Map.find rules !ca_cache
    with Not_found -> 
      List.enum rules
      |> Enum.filter_map make_rx_pair
      |> Pcregex.rx_of_dec_strings ~anchor:true
      |> Minreg.of_reg
       (*       |> tap (eprintf "Making DFA from: %a %!" (Minreg.printp ~dec:false) ) *)
      |> Regex_dfa.build_dfa ~labels:false (dec_rules comp_dec)
      |> Regex_dfa.minimize ~dec_comp:dec_eq
      |> Regex_dfa.to_array
  (*       |> tap (Regex_dfa.size |- eprintf "(%d states)\n%!") *)
  (*       |> D2fa.of_dfa *)
      |> tap (fun dfa -> ca_cache := Map.add rules dfa !ca_cache) 
  (* |> tap (fun _ -> Time.stop comp_t) *)
  )


let fill_cache cache ca =
  let comp = compile_ca cache in
  Enum.iter (comp |- ignore) (get_all_rule_groups ca)

let print_vars oc m = 
  Array.print Int.print oc m
(*
let sim_p = Point.create "sim"
let sim_t = Time.create "sim"
*)
let parsed_bytes = ref 0

exception Parse_complete

type 'a resume_ret = 
  | End_of_input of 
      ('a, int array, ca_data pri_dec option) Regex_dfa.state * int 
    * ca_data * int
  | Dec of ca_data * int

let rec resume_arr qs input pri item ri q i =
  if i >= String.length input then (
(*    printf "EOI: q:%d pri:%d ri:%d\n" q.Regex_dfa.id pri ri;*)
    End_of_input (q,pri,item,ri)
  ) else
    let q_next_id = q.Regex_dfa.map.(Char.code input.[i]) in
(*  printf "DFA %C->%d " input.[i] q_next_id; *)
    if q_next_id = -1 then 
      Dec (item,ri)
    else
      let q = qs.(q_next_id) in
      let next_i = i+1 in
      match q.Regex_dfa.dec with
	| Some d when d.pri <= pri ->
	  resume_arr qs input d.pri d.item next_i q next_i
	| _ -> 
	  resume_arr qs input pri item ri q next_i

let null_state = -1

let init_state dfa pos =
  let q0 = dfa.Regex_dfa.q0 in
  match q0.Regex_dfa.dec with 
      Some {pri=p; item=i} -> (dfa.Regex_dfa.qs, q0, p, i, pos, "")
    | None -> (dfa.Regex_dfa.qs, q0, max_int, ([], null_state), pos, "")

(*let ca_trans = ref 0 *)

(*let () = at_exit (fun () -> printf "#CA Transitions: %d\n" !ca_trans) *)

let rec simulate_ca_string ~ca ~vars fail_drop skip_left base_pos flow_data (qs, q, pri, item, ri, tail_data) = 
  (*Printf.printf "P:%s\n" flow_data; *)
  let flow_len = String.length flow_data in
  let pos = ref 0 in
  let ca_state = (!base_pos, pos, flow_data) in
  let rec run_d2fa qs q pri item ri tail_data =
    if !pos >= flow_len then ( (* skipped past end of current packet *)
      skip_left := !pos - flow_len; 
      Some (qs, q, pri, item, !base_pos + ri, "")
    ) else if !pos < 0 then ( (* handle DFA backtrack into previous packet *)
      (* TODO: optimize? *)
      let new_ri = ri + !base_pos in
      base_pos := !base_pos + !pos;
      simulate_ca_string ~ca ~vars fail_drop skip_left base_pos (tail_data ^ flow_data) (qs, q, pri, item, new_ri, "")
    ) else
      let dfa_result = resume_arr qs flow_data pri item ri q !pos in
      match dfa_result with
	| Dec ((acts,q_next),pos_new) -> 
(*	  printf "CA: %d @ pos %d(%d)\n" q_next (pos_new + !base_pos) pos_new;  *)
(*	  incr ca_trans; *)
	  parsed_bytes := !parsed_bytes + (pos_new - !pos);
	  pos := pos_new;
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
(*	  printf "ri_out: %d, last_pos: %d\n" (!base_pos + ri) (!base_pos + flow_len);*)
	  let tail_out = if ri < 0 then tail_data ^ flow_data else String.tail flow_data ri in
	  Some (qs,q_final,pri,item, !base_pos + ri,tail_out)
		
  in
(*  printf "ri_in: %d " ri;*)
  run_d2fa qs q pri item (ri - !base_pos) tail_data

exception Unknown_nonterminal of string
let verify_grammar g = 
  let is_defined (pi,_) = 
    match pi with Term _ -> () | 
	Nonterm nt -> try NTMap.find nt g.rules |> ignore 
	with Not_found -> raise (Unknown_nonterminal nt) in
  NTMap.iter (fun _ ps -> List.iter 
		(fun p -> 
		   try List.iter is_defined p.expression
		   with Unknown_nonterminal nt -> 
		     printf "\nUnknown nonterminal %s in rule: %a\n" 
		       nt print_production p
		) 
		ps 
	     )
    g.rules

let parse_spec_file fn =
  let epsilon_rule = {name="@EPSILON"; predicates=VarMap.empty; 
		      expression=[]; priority=50} in
  let rules = 
    try 
      Lexing.from_channel (open_in fn)
        |> Ns_yac.spec Ns_lex.token
	|> List.map full_parse
    with x -> printf "Failed to parse %s\n" fn; raise x
  in
  let start =
    match rules with [] -> failwith "Empty grammar"
      | h::_ -> h.name
  in
  let index rs =
    let add_to_map acc r =
      try
	let old = NTMap.find r.name acc in
	NTMap.add r.name (old @ [r]) acc
      with Not_found -> NTMap.add r.name [r] acc
    in
    List.backwards rs |> tap (fun e -> Enum.push e epsilon_rule) |> Enum.fold add_to_map NTMap.empty 
  in
  {start=start; rules=rules |> index}

let parse_file_as_spec fn = parse_spec_file fn |> tap verify_grammar

let parse_file_as_extraction = parse_spec_file
