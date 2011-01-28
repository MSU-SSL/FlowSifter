open Batteries
open Printf
open Ns_types
open Simplify
open ParsedPCFG

open Ocamlviz

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
	let cap_id = "capture" ^ (capture_counter () |> string_of_int) in
	let get_pos = Function ("pos", []) in
	let start_cap = VarMap.add cap_id get_pos VarMap.empty in
	let end_exp = Function (func, [Variable; Sub (get_pos, Constant 1)]) in
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

let print_iaction oc (i,e) = fprintf oc "(%d) := %a" i (print_a_exp (string_of_int i)) e

let print_ipred oc (i,e) = print_p_exp (string_of_int i) oc e


let print_opt_rule oc iirr =
  let so = Option.print String.print in
  let io = Option.print Int.print in
  Printf.fprintf oc "{p:%d; rx:%a acts:%a nt: %a}%!" 
    iirr.prio so iirr.rx (List.print print_iaction) iirr.act io iirr.nt
  
let print_reg_ds_ca oc (ca: regular_grammar_arr) = 
  Array.iteri (Regex_dfa.index_print (Pair.print (List.print print_ipred) print_opt_rule |> List.print ~first:"\n  " ~sep:"\n  " ~last:"\n") oc) ca

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
  


let pred_checks = ref 0

(* get the decisions from a CA for the given NT(q) and with predicates
   satisfying vars *)
let get_rules get_f pr_list vars =
  let var_satisfied (v,p) = 
    incr pred_checks; 
    let var_val = vars.(v) in
    eval_p_exp get_f p var_val in
  let pred_satisfied pred = List.for_all var_satisfied pred in
  List.filter_map (fun (p,e) -> if pred_satisfied p then Some e else None) pr_list

let rules_p = Point.create "rules"

let optimize ca compile_ca =
    let opt_prod pr_list =
      if List.for_all (fun (p,_) -> p = []) pr_list then
	let l = List.map snd pr_list |> compile_ca in
	(fun _ _ -> l)
      else
	(fun vars get_f -> Point.observe rules_p; get_rules get_f pr_list vars |> compile_ca )
    in
    Array.map opt_prod ca


let get_all_rule_groups (ca:regular_grammar_arr) =
  let groups_of_rlist rs =
    let no_pred,with_pred = 
      List.partition (fun (p,_r) -> p = []) rs 
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

let upd_p = Point.create "rules_upd"

let run_act get_f vars (var, act) =
  Point.observe upd_p;
  vars.(var) <- eval_a_exp get_f act vars.(var)

type 'a opt_rules = ('a option * ('a -> 'a option) * ('a option -> 'a option -> 'a option))
(* rules for handling decisions when creating the DFA *)
let dec_rules comp =
  let lowest_precedence a b = match a,b with
    | None, None -> None
    | Some r, None | None, Some r -> Some r
    | Some r1, Some r2 -> if comp r1 r2 = -1 then Some r1 else Some r2 in
  (None, (fun i -> Some i),lowest_precedence: 'a opt_rules)

let comp_dec (p1,_,_) (p2,_,_) = Int.compare p1 p2

type ca_var_state = (string,int) Map.t
type dfa_dec = (int * (int * a_exp) list * int option)

let print_dfa_dec oc (p,al,q) =
  let so = Option.print String.print in
  Printf.fprintf oc "{%d,%a,%a}%!" 
    p (List.print print_action) al so q


(*let comp_p = Point.create "comp_ca"
let comp_t = Time.create "comp_ca" *)

let make_rx_pair r =
  let to_pair rx = 
    (r.prio,r.act,r.nt:dfa_dec), 
    (rx |> String.lchop |> String.rchop) (* remove /rx/'s slashes *)
  in
  Option.map to_pair r.rx 
    
(* Compiles a list of rules into an automaton with decisions of
   (priority, action, nt) *)
let compile_ca ~ca_cache =
  (* TODO: pre-compile all possible rule combinations from the given ca *)
  (fun rules ->
(*    Point.observe comp_p;
    Time.start comp_t; *)
    ( try Map.find rules !ca_cache
      with Not_found ->
       List.enum rules
       |> Enum.filter_map make_rx_pair
       |> Pcregex.rx_of_dec_strings ~anchor:true
       |> Minreg.of_reg
(*       |> tap (eprintf "Making DFA from: %a %!" (Minreg.printp ~dec:false) ) *)
       |> Regex_dfa.build_dfa ~labels:false (dec_rules comp_dec)
       |> Regex_dfa.minimize 
       |> Regex_dfa.to_array
(*       |> tap (Regex_dfa.size |- eprintf "(%d states)\n%!") *)
(*       |> D2fa.of_dfa *)
       |> tap (fun dfa -> ca_cache := Map.add rules dfa !ca_cache)
    )(* |> tap (fun _ -> Time.stop comp_t) *)
  )


let fill_cache cache ca =
  let comp = compile_ca ~ca_cache:cache in
  Enum.iter (comp |- ignore) (get_all_rule_groups ca)

let print_vars oc m = 
  Array.print Int.print oc m

exception Done of ((string,string) regular_rule list,
		   (dfa_dec Minreg.t, int Batteries_uni.IMap.t, dfa_dec option)
		     Regex_dfa.state Regex_dfa.fa) Batteries.Map.t

exception Invalid_arg_count of string
let wrong_args name = raise (Invalid_arg_count name)

let debug_ca = true
let matches = ref 0
and dfa_trans = ref 0 
and ca_trans = ref 0
let orig_stream : (int*char) LazyList.t ref = ref LazyList.nil

let rec simulate_ca ~compile_ca ~(ca:regular_grammar_arr) ~vars ?child_ca q0 (stream:(int * char) LazyList.t) =
  let sim_pos = ref 0 
  and stream = ref stream 
  and q = ref (Some q0) 
  in


  (* functions for simulating the ca *)
  let get_f = function
      "pos" -> (function [] -> !sim_pos | _ -> wrong_args "pos")
    | "bounds" -> 
	(function 
	   | [start_pos; end_pos] when end_pos >= start_pos -> 
	       let str = !orig_stream |> LazyList.drop (start_pos-1) |> LazyList.take (end_pos-start_pos+1) |> LazyList.map snd |> LazyList.to_list |> String.implode in
	       Printf.eprintf "***Match found in range %d, %d: %s***\n"
		 start_pos end_pos str;
	       incr matches;
	       0
	   | [s;e] -> Printf.printf "***null_match***"; 0
	   | _ -> wrong_args "bounds"
	)
(*
    | "child_parse" -> 
	(function 
	   | [-1] -> (* give rest of stream to child parser *)
	       (match child_ca with
		    None -> ()
		  | Some ca -> 
		      simulate_ca ~compile_ca ~ca ~vars "X" !stream;
	       );
	       stream := LazyList.nil;
	       0
	   | [len] ->
	       let for_sub, for_parent = 
		 if LazyList.would_at_fail !stream len 
		 then !stream, LazyList.nil 
		 else LazyList.split_at len !stream 
	       in
	       stream := for_parent;
	       ( match child_ca with
		     None -> ()
		   | Some ca -> 
		       simulate_ca ~compile_ca ~ca ~vars "X" for_sub;
	       );
	       len
	   | _ -> wrong_args "child_parse"
	)
 *)
    | "skip" -> 
	( function 
	   | [len] ->
	       stream := 
		 if LazyList.would_at_fail !stream len 
		 then LazyList.nil 
		 else LazyList.drop len !stream;
	       len
	   | _ -> wrong_args "skip"
	)
    | "notify" ->
	( function [n] -> 
	    Printf.eprintf "*** Match found: %d ***\n" n; 
	    incr matches; n 
	    | _ -> wrong_args "skip" )
    | "cur_byte" -> 
	( function 
	      [] -> (
		match LazyList.get !stream with 
		    None -> -1 
		  | Some ((_,c),_) -> Char.code c
	      ) 
	    | _ -> wrong_args "cur_byte")
    | _  -> failwith "unknown function"
  in
  while !q <> None && not (LazyList.is_empty !stream) && !ca_trans < 300 do
    let dfa = get_rules get_f ca.(Option.get !q) vars |> compile_ca in
    let (acts, q_next, stream_new),tquery_count = 
      D2fa.run_stream dfa !stream in
    let new_pos = (match LazyList.get stream_new with Some ((p,_),_) -> p | None -> -1) in
    if new_pos = !sim_pos && q_next = !q then (
      eprintf "CA failed to progress from state %d, exiting.\n" (Option.get !q);
      q := None;
    ) else (
      sim_pos := new_pos;
      stream   := stream_new;
      dfa_trans := !dfa_trans + tquery_count;
      incr ca_trans;
      List.iter (run_act get_f vars) acts;
      q        := q_next;
      if debug_ca then 
	eprintf "\nDT%d CT%d At position %d, transitioning to state %a, vars:%a.\n%!" 
	!dfa_trans !ca_trans !sim_pos (Option.print Int.print) q_next print_vars vars; 
    )
  done;
  let stream_head = 
    (if LazyList.would_at_fail !stream 8 then !stream else LazyList.take 8 !stream)
    |> LazyList.map snd 
  in
  let print_stream = LazyList.print (fun oc c -> Char.escaped c |> String.print oc) in
  eprintf "Stopped parsing at: %a\n" print_stream stream_head;
  ()


let inner_skip = ref 0
let skip_over = ref 0

(* functions for simulating the ca *)
let get_f_str (base_pos, sim_pos, flow_data) = function
  | "pos" -> (function [] -> base_pos + !sim_pos | _ -> wrong_args "pos")
  | "bounds" -> 
    (function 
      | [start_pos; end_pos] when start_pos <= end_pos -> 
(* BROKEN BY SPLICING CODE -- check bounds on start/end pos and current flow_data *)
(*	let str = String.sub start_pos end_pos flow_data in *)
(*	Printf.eprintf "***Match found in range %d, %d***\n" 
	  start_pos end_pos;  *)
	incr matches;
	0
      | [s;e] -> 0
      | _ -> wrong_args "bounds"
    )
  | "skip" -> 
    ( function 
      | [len] ->
	if len > 0 then	sim_pos := !sim_pos + len;
	inner_skip := !inner_skip + len;
	0
      | _ -> wrong_args "skip"
    )
  | "notify" ->
    ( function [n] -> 
(*      Printf.eprintf "*** Match found: %d ***\n" n;  *)
      incr matches; n 
      | _ -> wrong_args "skip" )
  | "cur_byte" -> 
    ( function 
      | [] -> Char.code flow_data.[!sim_pos]
      | _ -> wrong_args "cur_byte")
  | "getnum" as fname ->
    ( function 
      | [] -> 
	let ret = ref 0 in
	while (let c = Char.code flow_data.[!sim_pos] in c >= 0x30 && c <= 0x39) do
	  let c = Char.code flow_data.[!sim_pos] in
	  ret := !ret * 10 + (c - 0x30);
	  incr sim_pos;
	done;
	!ret
      | _ -> wrong_args fname)
  | f  -> failwith ("unknown function: " ^ f)

let sim_p = Point.create "sim"
let sim_t = Time.create "sim"

let resu_p = Point.create "resume"
let resu_t = Time.create "resume"

let upd_t = Time.create "rules_upd"
let rules_t = Time.create "rules"

let resume_calls = ref 0
let () = at_exit (fun () -> Printf.printf "#resume_calls: %d\n" !resume_calls)

exception Parse_failure

let rec simulate_ca_string ~ca ~vars base_pos (flow_data:string) pst = 
  let flow_len = String.length flow_data in
  let pos = ref 0 in
  let get_f = get_f_str (!base_pos, pos, flow_data) in
  let rec run_d2fa (dfa0,q as pst) =
    if !pos > flow_len then
      ( incr skip_over; (pst, !pos - flow_len) )
    else
      let pri, ret = 
	match q.Regex_dfa.dec with 
	    Some (pri,act,caq) -> pri, (act,caq,!pos)
	  | None -> max_int, ([],None,!pos) in
(*      Point.observe resu_p; Time.start resu_t; *)
      incr resume_calls;
      let dfa_result = Regex_dfa.resume_arr dfa0 flow_data pri ret q !pos in
(*      Time.stop resu_t; *)
      match dfa_result with
	| `Dec (acts,Some q_next,pos_new) -> 
	  pos := pos_new; 
	  incr ca_trans;
(*	  Time.start upd_t; *)
	  if acts <> [] then List.iter (run_act get_f vars) acts;
(*	  Time.stop upd_t;
	  Time.start rules_t;*)
	  let dfa = ca.(q_next) vars get_f in
(*	  Time.stop rules_t;  *)
	  (*	eprintf "\nCT%d At position %d, transitioning to state %s, vars:%a.\n%!"
		!ca_trans !pos q_next print_vars !vars; *)
	  run_d2fa (dfa, dfa.Regex_dfa.q0)
	| `Dec (_acts, None, _pos_new) -> raise Parse_failure
	| `End_of_input q_j -> (dfa0,q_j),0
  in
  run_d2fa pst

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



(* Compiles a list of rules into an automaton with decisions of
   (priority, action, nt) *)
let compile_ca_vs ~boost ~stride ~ca_cache =
  (* TODO: pre-compile all possible rule combinations from the given ca *)
  let make_rx_pair r =
    let to_pair rx = 
      (r.prio,r.act,r.nt:dfa_dec), 
      (rx |> String.lchop |> String.rchop) (* remove /rx/'s slashes *)
    in
    Option.map to_pair r.rx 
  in
  (fun rules ->
     try 
       Map.find rules !ca_cache
     with Not_found ->
       List.enum rules
       |> Enum.filter_map make_rx_pair
       |> Pcregex.rx_of_dec_strings ~anchor:true
       |> Minreg.of_reg
(*       |> tap (eprintf "Making DFA from: %a %!" (Minreg.printp ~dec:false) ) *)
       |> Regex_dfa.build_dfa ~labels:false (dec_rules comp_dec)
       |> Regex_dfa.minimize 
(*       |> tap (Regex_dfa.size |- eprintf "(%d states)\n%!") *)
       |> D2fa.of_dfa
       |> Vsdfa.of_d2fa
(*       |> tap (fun _ -> eprintf "Vsdfa built\n%!") *)
       |> Vsdfa.increase_stride_all ((=) None) ~com_lim:max_int (stride-1) 
(*       |> tap (fun _ -> eprintf "Stride increased\n%!") *)
       |> Vsdfa.boost ((=) None) ~loop_lim:160 ~boost:(boost-1)
       |> tap (fun dfa -> ca_cache := Map.add rules dfa !ca_cache)
  )

exception No_next_state

let rec simulate_ca_string_vs ~compile_ca ~(ca:regular_grammar_arr) vars base_pos (flow_data:string) pst = 
  let pos = ref 0 in
  let ca_state = ref (-1) in
  let get_f = get_f_str (!base_pos, pos, flow_data) in
  let rec run_d2fa (dfa0,q_i,pri,ret) =
    match Vsdfa.resume dfa0 flow_data pri ret q_i !pos with
      | `Dec (acts,Some ca_next,pos_new) -> 
	pos := pos_new; 
	incr ca_trans;
	ca_state := ca_next;
	List.iter (run_act get_f vars) acts;
	let dfa = get_rules get_f ca.(ca_next) vars |> compile_ca in
(*	printf "\nCT%d At position %d, transitioning to state %s, vars:%a.\n%!"
	  !ca_trans !pos ca_next print_vars !vars; *)
	(* continue parsing starting with the next automaton *)
	run_d2fa (dfa, dfa.Regex_dfa.q0, max_int, ([], None, -1))
      | `Dec (_acts, None, _pos_new) -> 
(*	printf "Flow parse failure, resetting\n%s@@%s\n" (String.head flow_data !pos) (String.tail flow_data (!pos));
	printf "CA state: %s\n" !ca_state; *)
	raise No_next_state
      | `Need_more_input (q, i, pri, ret) -> (dfa0, q, pri, ret)
  in
(*  Point.observe sim_p;
  Time.start sim_t; *)
  run_d2fa pst (*|> tap (fun _ -> Time.stop sim_t)  *)

let () = at_exit (fun () -> printf "#ca transitions: %d\n" !ca_trans)
