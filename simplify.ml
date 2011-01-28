open Batteries_uni
open Ns_types
open ParsedPCFG
let (|>) x f = f x (** sort of like unix pipe *)

(************************************************************************)
(** Grammar Simplification                                              *)
(************************************************************************)

(* nonterm.name mapping function generator *)
let make_map label = fun nt -> nt ^ "<" ^ label ^ ">" ;;

let exp2str = IO.to_string print_exp ;; 


let rec break_regular r (rev_behind:expression) (forward:expression) =
  let helper expr result = (* this constructs the proper map *)
    let label = r.name ^ 
      "<" ^ (exp2str (List.rev rev_behind)) ^ "|" ^ (exp2str forward) ^ ">"
    in
    let x = {r with name = label; 
	       predicates = VarMap.empty; 
	       expression =  expr} 
    in 
    (NTMap.add x.name [x] result, x) 
  in

  match rev_behind,forward with
    | [], l when List.length l <= 2 -> (* short lists don't break *)
	NTMap.empty, r 
    | [], h::t -> 
	let result, lastrule = break_regular r [h] t in
	let out_exp = [h; (Nonterm lastrule.name, VarMap.empty)] in
	let x = { r with expression = out_exp } in
	(NTMap.add x.name [x] result, x)
    | _, [] -> assert false (* should never happen *)
    | _, [_] -> 
	helper forward NTMap.empty
    | _, [_a; (Nonterm _b, _c)] -> 
	helper forward NTMap.empty
    | _, [a;b] -> 
	let result, lastrule = break_regular r (a :: rev_behind) [b] in 
	helper [a; (Nonterm lastrule.name, VarMap.empty) ] result 
    | _, a::(_::_ as tl) -> 
	let result, lastrule = break_regular r (a :: rev_behind) tl in
	helper  [a; (Nonterm lastrule.name, VarMap.empty)] result
	 
let rec break_nonregular grammar r = match r.expression with
  | (Nonterm a, action)::tl when is_subgrammar_regular grammar a ->
      let newgrammar = project grammar a (make_map (exp2str tl)) in
      let append_tail_if_terminal r = 
	if is_terminal_expr r.expression then  
	  {r with expression = (r.expression @ tl)}
	else r
      in
      let out_exp = [(Nonterm newgrammar.start, action)] in
      let newrule = {r with expression = out_exp }
      in
      NTMap.map (List.map append_tail_if_terminal) newgrammar.rules
      |> NTMap.add newrule.name [newrule]
  | ((Term _a, _action)) as h :: tl -> 
      let res = break_nonregular grammar {r with expression=tl} in
      if NTMap.is_empty res then res
      else
	let temp = List.hd (NTMap.find r.name res) in
	let newrule = { r with expression = h :: temp.expression} in
	NTMap.add newrule.name [newrule] res
  | _ ->  NTMap.empty (* No new rules are generated *) 

(* returns a new map that contains a rewritten r and 
   the subgrammar that supports r
   if no rewriting occurs then an empty Map *)
let normalize_rule grammar r = 
  if is_regular r then 
    break_regular r [] r.expression |> fst
  else 
    break_nonregular grammar r ;;

(* let ng_count = ref 5 *)
(* Apply rule normalization all the rules in the grammar 
   until grammar is normal or no changes are made *)  
let rec normalize_grammar grammar = 
  let merge_maps = NTMap.fold NTMap.add in
  let repeat_normalize = ref false in
  let normalize_r  (r_acc, map_acc) r =
    (* print_production stdout r ; print_newline () ; *)
    let result = normalize_rule grammar r
      (* |> tap (print_rules stdout) *)
    in
    if NTMap.is_empty result then  
      (	r::r_acc, map_acc )
    else (
      repeat_normalize := true;
      (	(List.hd (NTMap.find r.name result))::r_acc, 
	merge_maps result map_acc )
    )
  in
    
  let normalize_map key r_list map_acc = 
    let nr_list, nmap = 
      List.fold_left normalize_r ([], map_acc) r_list in
    NTMap.add key nr_list nmap
  in

  let nrules = 
    NTMap.fold normalize_map grammar.rules NTMap.empty
  in
  let g = prune_grammar {grammar with rules = nrules} 
    (* |> tap (print_grammar stdout) *)
  in 
  if !repeat_normalize then normalize_grammar g else g

open Printf
(** determines if a grammar can be safely normalized **)
let can_normalize_grammar grammar = 
  let memory = ref  NTMap.empty in
  let rec can_normalize nt =
    try 
      NTMap.find nt !memory 
    with Not_found ->
      if (is_subgrammar_regular grammar nt) then 
	(
	  memory := NTMap.add nt true !memory;
	  (* printf "%s is regular: %b\n" nt true; *)
	  true
	)
      else
	(
	  memory := NTMap.add nt false !memory ;
	  let t    = nonterminals_r grammar.rules nt in
	  let c    = 
	    if is_nt_only_final_rules nt grammar.rules 
	    then NTSet.remove nt t 
	    else t
	  in
	  let bool = NTSet.for_all can_normalize c in
	  memory := NTMap.add nt bool !memory ;
	  (* printf "%s is regular: %b\n" nt bool; *)
	  bool
	)
	
  in
  can_normalize grammar.start ;; 

let can_normalize grammar nt = can_normalize_grammar {grammar with start = nt} ;;

(** generates the list of replacements rules for the approximation of 
    a nonterm in a grammar **)
let approx_counter = "@ac" ;;

let pred_is_zero = Equal (Variable, Constant 0) ;;
let pred_greater_zero = Greaterthan (Variable, Constant 0) ;;
let act_inc = Plus (Variable, Constant 1) ;;
let act_dec = Sub (Variable, Constant 1) ;;
let set_one = Constant 1 ;;

(* We not longer handle wraparound semantics
   We need to add that *)
let make_nt_approximate grammar nt = 
  let newterm = nt^"@a" in
  let sg = subgrammar grammar nt in
  let starts = start sg in
  let stops = stop sg in
  let xrementor nti action item acc =
    {name = nti;
     predicates = VarMap.empty;
     expression =  [ (Term item, VarMap.empty); 
		     (Nonterm newterm, 
		      VarMap.add approx_counter action VarMap.empty) 
		   ] ;
     priority = default_priority} :: acc

  in
 ( TSet.fold (xrementor nt set_one) starts
     (if has_episilon grammar nt then 
	[{name=nt;predicates=VarMap.empty;expression=[];priority=100}]
      else []
     ),
  [
    {name = newterm; 
     predicates = VarMap.add approx_counter pred_is_zero VarMap.empty;
     expression = [];
     priority = 25 ;};  (* escape rule *)
    {name = newterm; 
     predicates = VarMap.add approx_counter pred_greater_zero VarMap.empty; 
     expression = [(Term "/./", VarMap.empty); (Nonterm newterm, VarMap.empty)];
     priority = 500 ;} (* absorption rule *)
  ] |> 
    TSet.fold (xrementor newterm act_inc) starts |> 
    TSet.fold (xrementor newterm act_dec) stops 
 ) ;;

(** Approximate a grammar with an extraction specification 
    grammar is not normalized! *)
let ntmap_union m1 m2 = NTMap.fold NTMap.add m1 m2 ;;

let approx_grammar ~x_grammar ~p_grammar =
  let nt = x_grammar.start in
  let process nti acc_map =
    ( (* Printf.printf "%s : " nti ; *)
    if NTMap.mem nti acc_map then
      (
(* 	Printf.printf "In accumulator %s\n" nti ; *)
	acc_map (* no processing needed *) 
      )
    else if can_normalize p_grammar nti then
      (
(*	 Printf.printf "Normalizing %s\n" nti ;  *)
	let c = closure p_grammar nti in
	NTSet.fold 
	  (fun ntitem acc -> 
	     NTMap.add ntitem (NTMap.find ntitem p_grammar.rules) acc)
	  c acc_map
      )
    else
      (
(*	 Printf.printf "approximating %s\n" nti ; *)
	let s1, s2 = (make_nt_approximate p_grammar nti) in
	NTMap.add (nti^"@a") s2 (NTMap.add nti s1 acc_map)
      )
    ) 
  in
  let c = closure_x 
    (x_grammar.rules) (* |> tap (print_rules stdout) *)
    nt 
(*|> tap
	( fun foo ->
	  Printf.printf "Closure :" ;
	  NTSet.iter (fun nti -> Printf.printf "%s," nti) foo ;
	  Printf.printf "\n"
	) *)
  in
  (* Printf.printf "Tick\n" ; *)
  {x_grammar with rules = NTSet.fold process c x_grammar.rules}
    

