open Batteries
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
  let pri = match priopt with None -> default_priority | Some p -> [p] in
  let preds = match predopt with None -> VarMap.empty | Some str -> parse_preds str in
  let rec expand_term_act acc = function
    | Capture (rules, func), act ->
	let cap_id = "capture" ^ (capture_counter () |> string_of_int) in
	let get_pos = Function ("pos", get_f_id "pos", []) in
	let start_cap = VarMap.add cap_id get_pos VarMap.empty in
	let end_exp = Function (func, get_f_id func, [Variable]) in
	let end_cap = VarMap.add cap_id end_exp (parse_acts act) in
	List.fold_left expand_term_act (hook_act start_cap acc) rules
          |> hook_act end_cap
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
  Printf.fprintf oc "{p:%a; rx:%a acts:%a nt: %a}%!" 
    (List.print Int.print) rr.prio so rr.rx (List.print print_action) rr.act so rr.nt

let print_rrule oc (preds,body) = fprintf oc "%a%a" (print_varmap print_pred) preds print_reg_rule body
let print_reg_rules oc = function
  | [] -> ()
  | l -> List.print ~first:"\n#  " ~sep:"\n#  " ~last:"\n" print_rrule oc l

let print_reg_ca oc (ca: regular_grammar) = 
  Map.print String.print print_reg_rules oc ca;
  fprintf oc "Total: %d states\n" (Map.fold (fun _ x -> x+1) ca 0)

let print_iaction oc (i,e) = fprintf oc "$%d := %a" i (print_a_exp ("$" ^ string_of_int i)) e
let print_iact_opt oc (i,e) = match e with 
  | Fast_a _ -> fprintf oc "$%d := Fast" i
  | Slow_a e -> fprintf oc "$%d := %a" i (print_a_exp ("$" ^ string_of_int i)) e
let print_ipred oc (i,e) = print_p_exp ("$" ^ string_of_int i) oc e


let print_opt_rule oc iirr =
  Printf.fprintf oc "%s %a #%d" 
(*    (List.print ~first:"" ~last:"" ~sep:"." Int.print) iirr.prio *)
    (Option.default "" iirr.rx)
    (List.print ~first:"[" ~last:"]" ~sep:"; " print_iaction) iirr.act 
    (Option.default (-1) iirr.nt)
  
let print_rule i oc = function 
  | [],r -> fprintf oc "#%2d -> %a\n" i print_opt_rule r 
  | p,r -> fprintf oc "#%2d %a -> %a\n" i (List.print print_ipred) p print_opt_rule r
let print_rules i oc lst = 
  let lst = List.sort (fun (_,a) (_,b) -> compare a.prio b.prio) lst in
  List.print ~first:"" ~sep:"" ~last:"" (print_rule i) oc lst

let print_reg_ds_ca oc (ca: regular_grammar_arr) = 
  Array.iteri (fun i rs -> print_rules i oc rs) ca

exception Non_regular_rule of production

let merge_rx r s = 
  (r |> String.rchop) ^ (s |> String.lchop) 
(*  |> tap (eprintf "MERGE_RX: %s + %s -> %s\n" r s )*)

let prune_unreachable start rg =
  let reachable = ref (Set.singleton start) in
  let c = ref !reachable in
  let close s = Set.iter (fun nt -> List.iter (fun (_,{nt=next}) -> Option.may (fun x -> c := Set.add x !c) next) rg.(nt)) s in
  close !reachable;
  while Set.is_empty !c |> not do
    let new_rs = Set.diff !c !reachable in
    reachable := Set.union !reachable !c; 
    c := Set.empty; 
    close new_rs; 
  done;
  Array.range rg |> Enum.iter (fun i -> if not (Set.mem i !reachable) then rg.(i) <- [])

let regularize : grammar -> regular_grammar = fun grammar ->
  let make_r r =  
    let rec head_tail = function
      | [(Term a, head_act); (Nonterm b, tail_act)] ->  
	{ prio = r.priority; 
	  rx   = Some a; 
	  act  = act_act_compose head_act tail_act |> VarMap.enum |> List.of_enum;
	  nt   = Some b } 
      | [(Term a, head_act)] ->
	{ prio = r.priority;
	  rx   = Some a;
	  act  = VarMap.enum head_act |> List.of_enum;
	  nt   = None}
      | (Term a, head_act) :: (Term b, bhead_act) :: t -> 
	head_tail ((Term (merge_rx a b), (act_act_compose head_act bhead_act)) :: t)
      | [] ->
	{ prio = r.priority;
	  rx   = None;
	  act  = [];
	  nt   = None } 
      | [(Nonterm a, head_act)] -> 
	{ prio = r.priority; 
	  rx   = None; 
	  act  = VarMap.enum head_act |> List.of_enum; 
	  nt   = Some a } 
      | x -> print_endline (Std.dump x); raise (Non_regular_rule r) 
	  
    in
    ( r.predicates, (head_tail r.expression ) )
  in 
  let make_regular_g g = 
    NTMap.enum g.rules |> map (second (List.map make_r)) |> Map.of_enum
  in 
  grammar |> normalize_grammar |> idle_elimination |> make_regular_g

let destring : (string -> regular_grammar -> (regular_grammar_arr * int)) = 
  fun start ca ->
    let v_counter = Ean_std.make_counter 0 in
    let {Cache.get=int_of_v} = Cache.make_ht (fun _ -> v_counter ()) 10 in
    let nt_counter = Ean_std.make_counter 0 in
    let {Cache.get=int_of_nt} = Cache.make_ht (fun _ -> nt_counter ()) 10 in
    int_of_nt start |> ignore; (* make sure start state is #0 *)
    let fix_pair_a (v,a) = (int_of_v v, a) in
    let fix_pair_p (v,p) = (int_of_v v, p) in
    let fix_rule (r: (string, string, 'prio) regular_rule) = {r with act = List.map fix_pair_a r.act; nt = Option.map int_of_nt r.nt} in
    let fix_pred (p:pred) = VarMap.enum p |> Enum.map fix_pair_p |> List.of_enum in
    let pmap = Map.enum ca |> 
	Enum.map (fun (ca, pro) -> int_of_nt ca, (List.map (fun (p,r) -> fix_pred p, fix_rule r) pro)) in
    Enum.force pmap;

    let nt_count = nt_counter () in
    let var_count = v_counter () in
    let ret = Array.create nt_count (Obj.magic 0) in
    Enum.iter (fun (ca, pro) -> ret.(ca) <- pro) pmap;
(*    printf (* "varmap: %a\nca_statemap: %a\n "*)"ca: %a\n"
(*      (Hashtbl.print String.print Int.print) var_ht
      (Hashtbl.print String.print Int.print) ca_ht*)
      print_reg_ds_ca ret; *)
    ret, var_count

let compose_preds p1 p2 =
  let pred_order (v1,_exp) (v2,_exp2) = Int.compare v1 v2 in
  let p1 = List.sort pred_order p1 in
  let p2 = List.sort pred_order p2 in
  let rec merge = function
    | [], x | x, [] -> x
    | (v1,_ as a)::t1, ((v2,_)::_ as b) when v1 < v2 -> a::(merge (t1,b))
    | ((v1,_)::_ as a), ((v2,_ as b)::t2) when v1 > v2 -> b::(merge (a,t2))
    | (v1,e1)::t1, ((_v2,e2)::t2) (*when v1 = v2*) -> (v1, p_compose e1 e2)::(merge (t1,t2))
  in
  merge (p1,p2)

let dechain (rg: regular_grammar_arr) =
  let is_chain_rule rs = function (_,{rx=Some _; act=[]; nt=Some nt}) -> List.length rs = 1 || List.length rg.(nt) = 1 | _ -> false in
  let merge p rx pri (p1, rr) = 
    compose_preds p p1, {rr with rx=Some (Option.map_default (merge_rx rx) rx rr.rx); prio = pri @ rr.prio} 
  in 
  let can_improve rs = List.exists (is_chain_rule rs) rs in
  let elim_chain_rules nt = 
    let dechained = function
      | (p, {rx=Some rx; act=[]; prio=pri; nt=Some next}) when List.length rg.(nt) = 1 || List.length rg.(next) = 1 ->
	List.map (merge p rx pri) rg.(next)
      | r -> [r] (* not an improvable rule *)
    in
    rg.(nt) <- List.map dechained rg.(nt) |> List.flatten
  in
  let find_fixable () = Array.Exceptionless.findi can_improve rg in
  let chain_state = ref (find_fixable ()) in
  while !chain_state <> None do
    let nt = Option.get !chain_state in
(*    print_reg_ds_ca stdout rg;
    printf "Merging forward state %d\n%!" nt;     *)
    elim_chain_rules nt;
    prune_unreachable 0 rg;
    chain_state := find_fixable ()
  done;
(*  printf "Done dechaining\n%!";*)
  if debug_ca then print_reg_ds_ca stdout rg;
  rg

let flatten_priorities ca =
  let prio_map rs =
    let prios = List.map (fun (_p,r) -> r.prio) rs |> List.sort_unique (List.make_compare Int.compare) in
    (fun prio -> List.index_of prio prios |> Option.get |> (+) 1)
  in
  let fix_pri m (p,r) = p,{r with prio = m r.prio} in
  Array.map (fun rs -> List.map (fix_pri (prio_map rs)) rs) ca

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
		      expression=[]; priority=[50]} in
  let rules = 
    try 
      Lexing.from_channel (open_in fn)
        |> Ns_yac.spec Ns_lex.token
	|> List.map full_parse
    with x -> printf "Failed to parse:\n%s\n" fn; raise x
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
