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
	let end_exp = Function (func, get_f func, [Variable]) in
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
    let v_counter = Ean_std.make_counter 0 in
    let {Std.get=int_of_v} = Std.cache_ht (fun _ -> v_counter ()) 10 in
    let nt_counter = Ean_std.make_counter 0 in
    let {Std.get=int_of_nt} = Std.cache_ht (fun _ -> nt_counter ()) 10 in
    int_of_nt start |> ignore; (* make sure start state is #0 *)
    let fix_pair (v,a) = (int_of_v v, a) in
    let fix_rule (r: (string, string) regular_rule) = {r with act = List.map fix_pair r.act; nt = Option.map int_of_nt r.nt} in
    let fix_pred (p:pred) = VarMap.enum p |> Enum.map fix_pair |> List.of_enum in
    let pmap = Map.enum ca |> 
	Enum.map (fun (ca, pro) -> int_of_nt ca, (List.map (fun (p,r) -> fix_pred p, fix_rule r) pro)) in
    Enum.force pmap;

    let nt_count = nt_counter () in
    let var_count = v_counter () in
    let ret = Array.create nt_count (Obj.magic 0) in
    Enum.iter (fun (ca, pro) -> ret.(ca) <- pro) pmap;
(*    printf "varmap: %a\nca_statemap: %a\n ca: %a\n"
      (Hashtbl.print String.print Int.print) var_ht
      (Hashtbl.print String.print Int.print) ca_ht
      print_reg_ds_ca ret; *)
    ret, var_count
  


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
