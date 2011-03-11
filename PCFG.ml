open Batteries_uni
open Printf

(** Defines Predicated Context Free Grammars *)

module type Names_t = sig
  type var (* Type of variable names *)
  type term (* Type of terminals (Probably a regex) *)
  type nonterm (* Type of name for non terminals *)
  val print_var : 'a IO.output -> var -> unit
  val print_term : 'a IO.output -> term -> unit
  val print_nonterm : 'a IO.output -> nonterm -> unit
  val compare_nonterm : nonterm -> nonterm -> int
end

module Make (Name : Names_t) = struct
  (* Two Unary expression a_exp and p_exp have an implicit variable bound to them *)

  type parser_state = int * int ref * string

  (* This is an arithimatic expression *)
  type a_exp =
    | Plus of a_exp * a_exp
    | Sub of a_exp * a_exp
    | Multiply of a_exp * a_exp
    | Divide of a_exp * a_exp
    | Function of string * (parser_state -> int list -> int) * a_exp list
    | Constant of int  (* This is a specific number used to the define
			  the transformation of Variable *)
    | Variable (* This reference the uniary variable of the expression *)
    | Fast_a of (int -> int)

  (* This is a predicate expression *)
  type p_exp =
      Equal of a_exp * a_exp
    | Lessthan of a_exp * a_exp
    | Greaterthan of a_exp * a_exp
    | LessthanEq of a_exp * a_exp
    | GreaterthanEq of a_exp * a_exp
    | Not of p_exp
    | And of p_exp * p_exp
    | Or of p_exp * p_exp
    | Const of bool
    | Fast_p of (int -> bool)

  (* These types represent an expression that is bound to a specific variable *)
  module VarMap = Map.Make(struct type t = Name.var let compare = compare end)
  module VarSet = Set.Make(struct type t = Name.var let compare = compare end)

  type actions = a_exp VarMap.t
  type predicates = p_exp VarMap.t

  (* Define the components of a PCFG rule *)
  type productionitem =
      Nonterm of Name.nonterm
    | Term of Name.term

  type expression = (productionitem * actions) list


  (* Define a PCFG rule *)
  type  production =
      { name : Name.nonterm;
	predicates : predicates;
	expression : expression;
	priority : int
      }

  let default_priority = 50
    
  module NT = struct
    type t = Name.nonterm
    let compare = Pervasives.compare
  end
  module NTMap = Map.Make(NT)
  module NTSet = Set.Make(NT)


  module T = struct
    type t = Name.term
    let compare = Pervasives.compare
  end
  module TSet = Set.Make(T)


  (* Define a PCFG *)
  type grammar =
      {
	start : Name.nonterm;
	rules : production list NTMap.t
      }

  type t = grammar

  let rec print_a_exp var oc exp =
    let rec loop = function
      |	Plus (l,r)     -> loop l; IO.nwrite oc " + "; loop r
      |	Sub (l,r)      -> loop l; IO.nwrite oc " - "; loop r
      |	Multiply (l,r) -> loop l; IO.nwrite oc " * "; loop r
      |	Divide (l,r)   -> loop l; IO.nwrite oc " / "; loop r
      | Function (s,_,es) -> fprintf oc "%s(%a)" s (List.print (print_a_exp var) ~sep:",") es
      | Constant x -> Int.print oc x
      | Variable -> Name.print_var oc var
      | Fast_a _ -> IO.nwrite oc "(FAST)"
    in
    loop exp


  let rec print_p_exp var oc =
    let print_a = print_a_exp var in
    let rec loop = function
      |	Equal (l,r) -> fprintf oc "%a = %a" print_a l print_a r
      |	Lessthan (l,r) -> fprintf oc "%a < %a" print_a l print_a r
      |	Greaterthan (l,r) -> fprintf oc "%a > %a" print_a l print_a r
      |	LessthanEq (l,r) -> fprintf oc "%a <= %a" print_a l print_a r
      | GreaterthanEq (l,r) -> fprintf oc "%a >= %a" print_a l print_a r
      | Not e         -> IO.nwrite oc "not "; loop e
      |	Or (l,r)      -> loop l; IO.nwrite oc " or "; loop r
      |	And (l,r)     -> loop l; IO.nwrite oc " and "; loop r
      | Const true -> IO.nwrite oc "true"
      | Const false -> IO.nwrite oc "false"
      | Fast_p _ -> IO.nwrite oc "(FAST)"
    in
    loop

  (* Evaluates an arthimatic expression where value is bound to the
     implicit variable *)
  let rec val_a_exp s v = function
    | Variable -> v
    | Constant c -> c
    | Function (_, f, exp_list) -> f s (List.map (val_a_exp s v) exp_list)
    | Divide (a,b) -> (val_a_exp s v a) / (val_a_exp s v b)
    | Multiply (a,b) -> (val_a_exp s v a) * (val_a_exp s v b)
    | Sub (a,b) -> (val_a_exp s v a) - (val_a_exp s v b)
    | Plus (a,b) -> (val_a_exp s v a) + (val_a_exp s v b)
    | Fast_a f -> f v

    (* This bind value to the implicit variable and evaluates the p_exp*)
  let rec val_p_exp s v = function
    | Const const -> const
    | Or (a,b) ->  (val_p_exp s v a) || (val_p_exp s v b)
    | And (a,b) ->  (val_p_exp s v a) && (val_p_exp s v b)
    | Not a ->  not (val_p_exp s v a)
    | Greaterthan (a,b) -> (val_a_exp s v a) >  (val_a_exp s v b)
    | Lessthan (a,b) -> (val_a_exp s v a) <  (val_a_exp s v b)
    | GreaterthanEq (a,b) -> (val_a_exp s v a) >=  (val_a_exp s v b)
    | LessthanEq (a,b) -> (val_a_exp s v a) <=  (val_a_exp s v b)
    | Equal (a,b) -> (val_a_exp s v a) =  (val_a_exp s v b)
    | Fast_p f -> f v


  let rec aeq ae1 ae2 = match ae1,ae2 with
    | Function (s1,_,e1), Function (s2,_,e2) -> 
      s1 = s2 && List.length e1 = List.length e2 && List.for_all2 aeq e1 e2
    | Plus(a,b), Plus(c,d)
    | Sub(a,b), Sub(c,d)
    | Multiply(a,b), Multiply(c,d)
    | Divide(a,b), Divide(c,d) 
      -> aeq a c && aeq b d
    | Variable, Variable -> true
    | Constant a, Constant b -> a = b
    | Fast_a f, Fast_a g -> f == g
    | _ -> false

  let rec peq p1 p2 = match p1, p2 with
    | Const c, Const d -> c=d
    | Or(a,b), Or(c,d) 
    | And(a,b), And(c,d) -> peq a c && peq b d
    | Not a, Not b -> peq a b
    | Greaterthan (a,b), Greaterthan (c,d)
    | Lessthan (a,b), Lessthan (c,d)
    | GreaterthanEq (a,b), GreaterthanEq (c,d)
    | LessthanEq (a,b), LessthanEq (c,d)
    | Equal (a,b), Equal(c,d) -> aeq a c && aeq b d
    | Fast_p f, Fast_p g -> f == g
    | _ -> false


(*
  (* Creates a lookup table for a p_exp for a range of variable values, assuming functions return zero *)
  let make_p_table exp size = Array.init size (eval_p_exp (0,ref 0,"") exp) ;;

  (* Makes a list of values that will be true within the given p_table *)
  let make_true_list p_table =
    let p_enum =
      Array.enum p_table |> Enum.group (fun x -> x) |> Enum.map Enum.count
    in
    let rec bool_interval start value c_list  =
      match Enum.get p_enum with
	| Some s ->
	  let nextidx = start + s in
	  let nextval = not value in
	  let c_list' =
	    if value
	    then (start, (nextidx - 1)) :: c_list
	    else c_list
	  in
	  bool_interval nextidx nextval c_list'
	| None -> List.rev c_list
    in
    bool_interval 0 p_table.(0) []
*)
      

  let rec is_clean_a = function
    | Variable | Constant _ | Fast_a _ -> true
    | Function _ -> false
    | Divide (a,b) | Multiply (a,b) | Sub (a,b) | Plus (a,b) -> 
      is_clean_a a && is_clean_a b

  let rec is_clean_p = function
    | Const _ | Fast_p _ -> true 
    | Not a -> 
      is_clean_p a
    | Or (a,b) | And (a,b) -> 
      is_clean_p a && is_clean_p b
    | Greaterthan (a,b) | GreaterthanEq (a,b)
    | LessthanEq (a,b) | Lessthan (a,b) 
    | Equal (a,b) -> 
      is_clean_a a && is_clean_a b

  let rec freeze_a = function
    | Plus(Variable, Constant 1) -> Fast_a (fun x -> x+1)
    | Sub (Variable, Constant 1) -> Fast_a (fun x -> x+1)
    | Plus(Variable, Constant c) -> Fast_a (fun x -> x+c)
    | Sub (Variable, Constant c) -> Fast_a (fun x -> x+c)
    | a -> a

  let rec freeze_p = function
    | Equal(Variable, Constant 0) -> Fast_p (fun x -> x=0)
    | Greaterthan(Variable, Constant 0) -> Fast_p (fun x -> x>0)
    | a -> a

  (* Composition functions *)

  let a_compose sub_a_expr orig_a_expr =
    let rec subst = function
      | Variable -> sub_a_expr
      | Constant _const as x -> x
      | Function (name, fn, exp_list) -> Function (name, fn, List.map subst exp_list)
      | Divide (a,b) -> Divide (subst a,subst b)
      | Multiply (a,b) ->  Multiply (subst a, subst b)
      | Sub (a,b) -> Sub (subst a,subst b)
      | Plus (a,b) -> Plus (subst a, subst b)
      | Fast_a _ -> assert false (* can't compose with a fast *)
    in
      subst orig_a_expr ;;

  let p_compose p_l p_r = And (p_l,p_r) ;;

  let ap_compose sub_a_expr =
    let subst = a_compose sub_a_expr
    in
      function
	  (Const _) as x -> x
	| (Or (_,_)) as x ->  x
	| (And (_,_)) as x ->  x
	| (Not _) as x ->  x
	| Greaterthan (a,b) -> Greaterthan (subst a, subst b)
	| Lessthan (a,b) -> Lessthan (subst a,subst b)
	| GreaterthanEq (a,b) -> GreaterthanEq (subst a,subst b)
	| LessthanEq (a,b) ->  LessthanEq (subst a, subst b)
	| Equal (a,b) ->  Equal (subst a,subst b)
	| Fast_p _ -> assert false (* can't compose with this *)


  let gen_compose compose_f sub_map orig_map =
    let sub_set = VarSet.of_enum (VarMap.keys sub_map)
    in
    let total_set = VarSet.union sub_set (VarSet.of_enum (VarMap.keys orig_map))
    in
    let compose x acc =
      try
	let orig = VarMap.find x orig_map in
	try
	  let sub = VarMap.find x sub_map in
	  VarMap.add x (compose_f sub orig) acc

	with Not_found ->
	  VarMap.add x orig acc
      with Not_found ->
	VarMap.add x (VarMap.find x sub_map) acc

    in
    VarSet.fold compose total_set VarMap.empty ;;

  let act_act_compose = gen_compose a_compose ;;
  let pred_pred_compose = gen_compose p_compose ;;

  let act_pred_compose sub_map orig_map =
    let sub_set = VarSet.of_enum (VarMap.keys sub_map)
    in
    let total_set = VarSet.union sub_set (VarSet.of_enum (VarMap.keys orig_map))
    in
    let compose x acc =
      try
	let orig = VarMap.find x orig_map in
	try
	  let sub = VarMap.find x sub_map in
	  VarMap.add x (ap_compose sub orig) acc

	with Not_found ->
	  VarMap.add x orig acc
      with Not_found ->
	 acc

    in
    VarSet.fold compose total_set VarMap.empty ;;

  let print_action oc (n,e) = fprintf oc "%a := %a" Name.print_var n (print_a_exp n) e
  let print_pred oc (n,e) = print_p_exp n oc e

  let print_pi oc = function
      Nonterm nt -> Name.print_nonterm oc nt
    | Term t -> Name.print_term oc t

  let print_varmap print_one oc vm = 
    if not (VarMap.is_empty vm) then
      VarMap.enum vm |>
	  Enum.print ~first:"[" ~last:"]" ~sep:"; " print_one oc

  let print_exp oc e =
    let print_one oc (pi,amap) =
      print_pi oc pi;
      print_varmap print_action oc amap;
    in
    List.print ~first:"" ~last:";" ~sep:" " print_one oc e

  let print_production oc p =
    Name.print_nonterm oc p.name;
    print_varmap print_pred oc p.predicates;
    if p.priority <> default_priority then Int.print oc p.priority;
    IO.nwrite oc " -> ";
    print_exp oc p.expression

  let print_rules oc r  = 
    Printf.printf "[\n" ;
    NTMap.iter (fun k ps ->
		  fprintf oc "Key: %a --\n    " Name.print_nonterm k;
		  List.print ~first:"" ~last:"\n" ~sep:"\n    "
		    print_production oc ps) r ;
    Printf.printf "]\n"

  let print_grammar oc g =
    fprintf oc "Start: %a =============\n%a"
      Name.print_nonterm g.start
      print_rules g.rules

  (** extract a list of nonterminals from the production p *)
  let get_nts p =
    p.expression |> List.enum |>
	Enum.filter_map (function Term _,_ -> None | Nonterm n,_ -> Some n)

  exception Nonterminal_not_found of string

  (** return the set of nonterminal names that nontermimal, nt, can
      produce with a single rewrite *)
  let gen_nonterminals_r f_get_nts rules nt =
    try
      let add_nt acc prod = f_get_nts prod |> Enum.fold (flip NTSet.add) acc in
      NTMap.find nt rules
        |> List.fold_left add_nt NTSet.empty
    with Not_found ->
      let err = (Name.print_nonterm |> IO.to_string) nt in
      raise (Nonterminal_not_found err)
  ;;

  let nonterminals_r    = gen_nonterminals_r get_nts ;;

  let nonterminals g nt = nonterminals_r g.rules nt

  let nonterminals_x rules nt =
    try
      nonterminals_r rules nt
    with Nonterminal_not_found _ ->
      NTSet.empty
  ;;

  (** return a set of reachable nonterminals from nt in grammar *)
  let closure_gen get_nonterminals rules nt =
    let rec closure_aux c_nt visited_set =
      let next_visited_set = NTSet.add c_nt visited_set in
      let next_visits = NTSet.diff (get_nonterminals rules c_nt) next_visited_set
      in
      NTSet.fold closure_aux next_visits next_visited_set
    in
    closure_aux nt NTSet.empty
  ;;

  let closure g nt = closure_gen nonterminals_r g.rules nt
  let closure_x r nt = closure_gen nonterminals_x r nt

  (** grammar projection functions *)

  (** returns the subgrammar that starts at nt *)
  let subgrammar grammar nt =
    let c = closure grammar nt in
    let rules = NTMap.filteri (fun k _ -> NTSet.mem k c) grammar.rules in
    { start = nt; rules = rules }
  ;;

  (** return the subgrammar where nonterminals have been
      remapped to new nonterminals *)
  (** rename : grammar -> grammar *)
  (** map_f : Name.nonterm -> Name.nonterm *)
  let rename map_f grammar =
    let rename_item = function
      | (Nonterm n,  m) -> ((Nonterm (map_f n)), m)
      | x -> x
    in
    let rename_prod x  =  {x with
			     name = map_f x.name;
			     expression = List.map rename_item x.expression;
			   }
    in
      { start = map_f grammar.start;
	rules = NTMap.fold 
	  (fun k v acc -> 
	     NTMap.add (map_f k) (List.map rename_prod v) acc
	  ) 
	  grammar.rules 
	  NTMap.empty
      } ;;

  (** project a subgrammar and renaming nonterm according to map *)
  let project grammar nt map = subgrammar grammar nt |> rename map ;;

  (** prune grammars of unreachable nonterminals **)
  let prune grammar closure_set =
    let test x _a = NTSet.mem x closure_set in
    {grammar with rules = NTMap.filteri test grammar.rules} ;;

  let prune_grammar g = closure g g.start |> prune g ;;

  (*
  let prune_grammar g = printf "PRUNING: \n%a\n%!" print_grammar g; prune_grammar g
  *)

  (** Rule analysis functions *)
  let is_terminal = function (Term _a, _) -> true | (Nonterm _,_) -> false ;;

  let is_terminal_expr expr = List.for_all is_terminal expr ;;

  (** returns  true if production is syntactically regular *)
  let  is_regular p = 
    let rec aux = function
	[]  -> true
      | [_] -> true
	  (* Final element can be either term or nonterm we don't care *)
      | (Term _a,_) :: b -> aux b
      | _ -> false
    in
    aux p.expression 
	(*|> tap (fun x -> if x then Printf.printf "true " else Printf.printf "false ")*)
  ;;

  let is_nt_only_final nt p =
    let rec aux = function
	[] -> true
      | [_] -> true
      | (Term _a, _) :: b -> aux b
      | (Nonterm a, _) :: _b when a = nt -> false
      | _ -> true
    in
    aux p.expression

  let is_nt_only_final_rules nt rules = 
    NTMap.find nt rules |> List.for_all (is_nt_only_final nt) ;;

  (** return true if grammar starting at nt is syntactically regular *)
  let is_subgrammar_regular grammar nt =
    let test_productions key =
      NTMap.find key grammar.rules |> List.for_all is_regular
    in
      closure grammar nt |> NTSet.for_all test_productions  ;;

  exception Not_idle

  (** Idle rule detection and elimination **)
  let extract_idle = function
    | [(Nonterm a, b)] -> (a,b)
    | _ -> raise Not_idle;;

  let modify_last_elem f l =
    let rec loop = function (* not tail recursive - can overflow stack *)
	[] -> []
      | [last] -> [f last]
      | h::t -> h::loop t
    in
    loop l

  exception Infinite_Unwind

  let get_idle_nts p = match p.expression with
    | [(Nonterm n, _)] -> List.enum [n]
    | _ -> List.enum []
  ;;

  let idle_nonterminals = gen_nonterminals_r get_idle_nts  ;;

  let idle_closure g nt = closure_gen idle_nonterminals g.rules nt

 
  let is_idle grammar =
    let is_rule_idle r = 
      try ignore(extract_idle r.expression); true
      with Not_idle -> false
    in 
    NTMap.enum grammar.rules 
    |> Enum.exists (fun (_k,v) -> List.exists is_rule_idle v);;

  (** new idle_elimination series **)

  (** returns a list of rules and the updated memory **)
  let hoist_rule rules rul memory =
    let hoist (nt_child, act) = 
      let hoist_r new_rule =
	let combined_expr =
	  modify_last_elem (fun (pi,a) -> pi,act_act_compose act a)
	    new_rule.expression
	in(* TODO: CHECK PRIORITY OF CREATED RULE *)
	{ rul with name=rul.name; 
	    expression = combined_expr;
	    predicates  = pred_pred_compose 
	    rul.predicates
	    (act_pred_compose act new_rule.predicates)
	}
      in
      NTMap.find nt_child rules |> List.map hoist_r 
    in
    try
      let nt,_ as item = extract_idle rul.expression in 
      let nt_set = NTMap.find rul.name memory in
      if NTSet.mem nt nt_set then [], memory
      else hoist item, NTMap.add rul.name (NTSet.add nt nt_set) memory
    with Not_idle -> [rul], memory

  let add_rule m r = 
    try NTMap.modify r.name (List.cons r) m
    with Not_found -> NTMap.add r.name [r] m

  (** the new idle_elimination **)

  let idle_elimination grammar =
    (* initial output -- 
       definitely no idle rules here, 
       so while loop will be run once *)
    let ret = ref grammar.rules in
    (* make initial memory *)
    let mem = ref (NTMap.mapi (fun k _ -> NTSet.singleton k) grammar.rules) in

    while is_idle {grammar with rules = !ret} do
      let rules = !ret in
      let per_rule r = 
	let new_rules, new_mem = hoist_rule rules r !mem in
	mem := new_mem;
	ret := List.fold_left add_rule !ret new_rules
      in
      ret := NTMap.empty;
      NTMap.iter (fun _k rl -> List.iter per_rule rl) rules
    done;
    prune_grammar {grammar with rules = !ret}

    
      

  (** finding starting and stopping terminals **)

  let has_episilon grammar nt =
    let rec check_episilon cnt memory =
      let is_episilon mem r = match r.expression with
	| [] -> true
	| (Nonterm x, _) :: _rest -> check_episilon x mem
	| _ -> false
      in
	if NTSet.mem cnt memory then
	  false (* No episilon on this path *)
	else
	  NTMap.find cnt grammar.rules |> List.exists (is_episilon (NTSet.add cnt memory))
    in
      check_episilon nt NTSet.empty;;


  (** find the starting (stopping) terminals of a grammar (usually is a subgrammar) *)
  let start_or_stop grammar_start get_expr =
    let rec start_main grammar memory_acc =
      let rec start_express (acc,memory) express =
	let recurse_case x = 
	  start_main 
	    (subgrammar grammar x)
	    (NTMap.add grammar.start TSet.empty memory) (* prevent infty *)
	in
	match express with
	  | [] -> (acc,memory)
	  | (Term x,_) :: _rest ->
	      let mem = try NTMap.find grammar.start memory
	                with Not_found -> TSet.empty in
	      (TSet.add x acc,
	       NTMap.add grammar.start (TSet.add x mem) memory)
	  | (Nonterm x,_) :: rest when has_episilon grammar x ->
	      start_express (recurse_case x) rest
	  |  (Nonterm x,_) :: _rest ->
	       recurse_case x
      in (*start_express *)
      try (* get from memoized values *)
	(NTMap.find grammar.start memory_acc, memory_acc) 
      with Not_found -> (* build solution *)
	NTMap.find grammar.start grammar.rules
	|> List.fold_left
	    (fun acc x -> start_express acc (get_expr x.expression))
	    (TSet.empty, memory_acc)
    in (*start_main*)
    
    let result, _ = start_main grammar_start NTMap.empty in result

  let start grammar = start_or_stop grammar (fun x -> x)
  let stop  grammar = start_or_stop grammar (fun x -> List.rev x)


end
