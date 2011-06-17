open Batteries_uni
open Printf

open Minreg
open Ean_std
open Regex_dfa

let debug = false

let dtap = if debug then fun f x -> f x; x else fun _f x -> x

type qset = int Set.t
type 'a nfa_label = {n_label: 'a; mutable epsilon: qset}
type ('a,'b) nfa = ('a nfa_label, qset IMap.t, 'b) fa

(* no epsilon transitions *)
type ('a,'b) multi_dfa = ('a, qset IMap.t, 'b) fa * qset

let qcmp a b = Enum.compare Int.compare (Set.enum a) (Set.enum b)
let qeq a b = qcmp a b = 0

let print_ids dfa = iter (fun i -> printf "%d) %d  " i dfa.qs.(i).id) (0 --^ size dfa)
let print_iset oc s = Set.print ~first:"{" ~sep:"," ~last:"}" Int.print oc s

let print_tmap oc v = IMap.iter_range (print_range Int.print oc) v
let print_ntmap oc v = IMap.iter_range (print_range print_iset oc) v

let print_mdfa write_label write_dec oc nfa =
  let print_q oc {id=id;label=l;map=map;dec=dec;dec_pri=dp;pri=p} =
    if id > 0 then fprintf oc "%a pri:%d\n#%a d:%a(%d)\n"
      write_label l p
      print_ntmap map
      write_dec dec dp;
  in
  print_fa print_q oc nfa

let print_nfa write_label write_dec oc nfa =
  let write_label oc {n_label=nlabel; epsilon=eps} =
    if Set.is_empty eps then 
      write_label oc nlabel
    else
      fprintf oc "%a eps: %a" write_label nlabel print_iset eps
  in
  print_mdfa write_label write_dec oc nfa


 (** ROUTINE TO FACTOR REGULAR EXPRESSIONS * Reorganizes a regex into
     an almost-DFA-like structure *)
let rec factor_rxs ~dec_comp rxs =
  if Set.is_empty rxs || Set.cardinal rxs = 1 then rxs else
    match Set.choose rxs with
      | Concat (rxh::_,_) ->
	let put_in_group rx (tls,rst) = match rx with
	  | Concat (rxa::tl,red) when Minreg.compare ~dec_comp rxa rxh = 0 ->
	    (Set.add (Concat (tl,red)) tls,rst)
	  | rx -> (tls, Set.add rx rst)
	in
	let tails,rest = Set.fold put_in_group rxs (Set.empty, Set.empty) in
	let ftails = factor_rxs ~dec_comp tails in
	let merged =
	  if Set.cardinal ftails = 1 then
	    match Set.choose ftails with
		Concat (chars,_) ->
		  Concat (rxh::chars, false)
	      | _ -> assert false
	  else
	    Concat ([rxh;Union ftails],false)
	in
	Set.add merged (factor_rxs ~dec_comp rest)
      | rx ->
	let factored_tail = factor_rxs ~dec_comp (Set.remove rx rxs) in
	Set.add rx factored_tail

let factor_rx ~dec_comp = function
  | Union rxs ->
    let concat,other = Set.partition (function Concat _ -> true | _ -> false) rxs in
    Union (Set.union (factor_rxs ~dec_comp concat) other)
  | x -> x

let factor_rx =
  if debug then 
    fun ~dec_comp x -> 
      printf " Pre_factor: %a\n" (Minreg.printp ~dec:true) x;
      factor_rx ~dec_comp x |> tap (printf "Post_factor: %a\n" (Minreg.printp ~dec:true))
  else
    factor_rx

(* rules for handling decisions when creating the DFA *)
let merge_opt merge a b = match a,b with
  | None, None -> None
  | Some r, None | None, Some r -> Some r
  | Some r1, Some r2 -> Some (merge r1 r2)

let opt_ops merge cmp = {dec0=None; merge=merge_opt merge; cmp=Option.compare ~cmp}
let list_ops cmp = {dec0=[]; merge=(@); cmp=List.make_compare cmp}


(** BUILD A NFA FROM A GIVEN REGEXP and some decision handling data *)
let build_nfa dop reg =
  let get_idx = make_counter 0 in
  let qs = ref Vect.empty in
  let new_state ?(label=None) ?(epsilon=Set.empty)
      ?(map=IMap.empty) ?(pri=0) ?(dec_pri=(-1)) ?(dec=dop.dec0) () =
    let idx = get_idx () in
     (*    if idx mod 100 = 1 then eprintf "N %d @ %.2f\n%!" idx (Sys.time ()); *)
    let q = {label = {n_label = label; epsilon = epsilon}; 
	     dec_pri = dec_pri; pri=pri; map=map; 
	     dec=dec; id = idx} in
    vect_set_any qs idx q;
    q
  in
  let rec loop = function
    | Value v -> let out_q = new_state () in let in_q = new_state ~map:(IMap.set_to_map v (Set.singleton out_q.id)) () in in_q,out_q
    | Accept (d,p) -> let q = new_state ~dec:d ~dec_pri:p () in q,q
    | Union rl ->
      let ins, outs = Set.enum rl |> List.of_enum |> List.map loop |> List.split in
      let in_q = new_state ~epsilon:(List.enum ins |> map (fun i -> i.id) |> Set.of_enum) () in
      let out_q = new_state () in
      let eps = Set.singleton out_q.id in
      List.iter (fun o -> o.label.epsilon <- eps) outs;
      in_q, out_q
    | Kleene r ->
      let in1,out1 = loop r in
      let out0 = new_state () in
      let in0 = new_state ~epsilon:(Set.singleton in1.id |> Set.add out0.id) () in
      out1.label.epsilon <- (Set.singleton in0.id);
      in0,out0
    | Concat ([],_) -> let q = new_state () in q,q
    | Concat (Value v::t,_red) ->
      let in1,out1 = loop (Concat (t,_red)) in
      new_state ~map:(IMap.set_to_map v (Set.singleton in1.id)) (), out1
    | Concat (rx::t,_red) ->
      let in0,out0 = loop rx in
      let in1,out1 = loop (Concat (t, _red)) in
      out0.label.epsilon <- (Set.singleton in1.id);
      in0, out1
  in
  let q0, _ = reg |> factor_rx ~dec_comp:dop.cmp |> loop in
  let qs = Vect.map Option.get !qs |> Vect.to_array in
  ({dop=dop;qs=qs; q0=q0} : ('a,'b) nfa)

let merge_nfa_maps m1 m2 =
  let merge_dst d1o d2o = match d1o,d2o with
      None, Some d | Some d, None -> Some d
    | Some d1, Some d2 -> Some (Set.union d1 d2)
    | None, None -> assert false
  in
  IMap.union merge_dst m1 m2 |> IMap.map ~eq:qeq identity

type 'a hmap_status = Unmade | In_progress | Done of 'a

let simplify_nfa nfa = (* remove in-epsilon-only states, hoisting their transitions to their epsilon parents *)
  let remove = Array.create (size nfa) true in
  let keep d = remove.(d) <- false in
  let set_keeps q = IMap.iter (fun _c dsts -> Set.iter keep dsts) q.map in
  let hoisted_maps = Array.create (size nfa) Unmade in
  let merge q q2 =
    let map = merge_nfa_maps q.map q2.map in
    let epsilon = Set.union q.label.epsilon q2.label.epsilon in
    let dec, dec_pri = 
      if q.dec_pri < q2.dec_pri then q2.dec, q2.dec_pri
      else if q.dec_pri > q2.dec_pri then q.dec, q.dec_pri
      else (* q.dec_pri = q2.dec_pri *) nfa.dop.merge q.dec q2.dec, q.dec_pri
    in
    {q with map = map; label={q.label with epsilon = epsilon}; dec= dec; dec_pri = dec_pri}
  in
  let merge_hmap i q = match hoisted_maps.(i) with
    | In_progress -> q (* loop - don't merge *)
    | Done q2 -> merge q q2
    | Unmade -> assert false
  in
  (* hoist state i and all its children *)
  let rec hoist_targets i =
    if hoisted_maps.(i) = Unmade then (
      let q = nfa.qs.(i) in
      hoisted_maps.(i) <- In_progress; (* loop handling *)
      let merge_targets _ _ dst acc = Set.union acc dst in
      let to_hoist = IMap.fold_range merge_targets q.map q.label.epsilon in
      let to_hoist = Set.filter (fun i -> remove.(i)) to_hoist in
      Set.iter hoist_targets to_hoist;
      let q = Set.fold merge_hmap to_hoist q in
      q.label.epsilon <- Set.filter (fun i -> not remove.(i)) q.label.epsilon;
      hoisted_maps.(i) <- Done q
    )
  in
  let null_q = {id=(-1); map=IMap.empty; dec=nfa.dop.dec0; pri=0; dec_pri=(-1);
		label={n_label=nfa.q0.label.n_label; epsilon=Set.empty};}  in
  let q_with_hoisted_map q =
    match hoisted_maps.(q.id),remove.(q.id) with
      | Unmade,_ | In_progress,_ -> assert false
      | _, true -> null_q
      | Done q,false-> q
  in
  (* don't remove start state *)
  remove.(nfa.q0.id) <- false;
  (* mark needed states *)
  Array.iter set_keeps nfa.qs;
(*  let num_to_remove = Array.filter identity remove |> Array.length in
  if debug then printf "Simplify_nfa: removing %d of %d nfa states\n" num_to_remove (size nfa);  *)
  iter hoist_targets (Array.range hoisted_maps);
  map_qs q_with_hoisted_map nfa

(* return the set of all states reachable from [n_id] by epsilon moves, including [n_id] *)
let get_eps_closure nfa n_id =
  let rec loop acc added =
    let add_epsilon n acc =
      Set.fold Set.add nfa.qs.(n).label.epsilon acc
    in
    let all = Set.fold add_epsilon added acc in
    let added = Set.diff all acc in
    if Set.is_empty added then acc
    else loop all added
  in
  let n_ids = (Set.singleton n_id) in
  loop n_ids n_ids

(* for each transition, [a -> b], replace it with [a -> states_reachable_by_epsilon(b)] *)
let close_transitions : ('a, 'b) nfa -> ('a, 'b) multi_dfa = fun nfa ->
  let q_with_closed_map q =
    let mod_dsts dsts = Set.enum dsts |> map (get_eps_closure nfa) |> Enum.reduce Set.union in
    {id=q.id; pri=q.pri; label = q.label.n_label; map = IMap.map ~eq:qeq mod_dsts q.map; 
     dec=q.dec; dec_pri = q.dec_pri}
  in
  map_qs q_with_closed_map nfa, get_eps_closure nfa nfa.q0.id

let print_nfa_label _oc _l = ()
let print_nfa_dec oc d = Std.dump d |> String.print oc

let propogate_priorities mdfa =
  let rev_map = ref MultiPMap.empty in
  let pri_roots = ref Set.empty in
  let add_tr ~src ~dst = rev_map := MultiPMap.add dst src !rev_map in
  let add_rev_transitions q = 
    let id = q.id in 
    IMap.iter_range (fun _ _ dests -> Set.iter (fun dst -> add_tr ~src:id ~dst) dests) q.map;
    if q.dec_pri > 0 then pri_roots := Set.add id !pri_roots;
  in
  (* build the reverse map and list of states with positive dec_pri *)
  Array.iter add_rev_transitions mdfa.qs;
  let rev_map = !rev_map in
  let new_pri = ref !pri_roots in
  let propogate_pri i = 
    let pri = mdfa.qs.(i).pri in 
    let set_pri j = 
      let q = mdfa.qs.(j) in 
      if q.pri < pri then (pri_roots := Set.add j !pri_roots; q.pri <- pri)
    in
    MultiPMap.find i rev_map |> PSet.iter set_pri
  in
  (* initial transfer of dec_pri -> pri *)
  Set.iter (fun i -> mdfa.qs.(i).pri <- mdfa.qs.(i).dec_pri) !pri_roots;
  while not (Set.is_empty !new_pri) do
    pri_roots := Set.empty;
    Set.iter propogate_pri !new_pri;
    new_pri := !pri_roots;
  done;
  ()

let build_dfa ~labels dop reg =
  ignore labels;
  let mdfa,q0 = build_nfa dop reg 
    |> dtap (printf "NFA0: \n%a" (print_nfa print_nfa_label print_nfa_dec)) 
    |> simplify_nfa
    |> dtap (printf "NFA1: \n%a" (print_nfa print_nfa_label print_nfa_dec))
    |> close_transitions in
  propogate_priorities mdfa;
  if debug then (
    printf "NFA built %.3f (%d states)\n%!" (Sys.time()) (Array.enum mdfa.qs // (fun q -> q.id <> -1) |> Enum.count); 
    printf "NFA2: \n%a" (print_mdfa print_nfa_label print_nfa_dec) mdfa;
    printf "q0: %d\n" mdfa.q0.id;  
  );
  let states = ref Vect.empty in
  let make_node get_id ns id =
    (* get the transitions for that character from the nfa *)
(*    if id mod 100 = 1 then eprintf "N %d @ %.2f\n%!" id (Sys.time ()); *)
    let map =
      Set.fold (fun n acc -> merge_nfa_maps acc mdfa.qs.(n).map) ns IMap.empty
      |> IMap.map (get_id |- Id.to_int)
    in
    let dec =
      Set.fold (fun n acc -> dop.merge acc mdfa.qs.(n).dec) ns dop.dec0
    in
    let q = {label=(); pri=0; dec_pri=(-1); id=id; map=map; dec=dec} in
    vect_set_any states id q;
    ()
  in
  let set_compare s1 s2 = Enum.compare Int.compare (Set.enum s1) (Set.enum s2) in
  let id_map = map_id_set ~comp:set_compare ~min_id:0 make_node in
  let q0 = id_map.get_id q0 |> Id.to_int in
  let qs = Vect.map Option.get !states |> Vect.to_array in
  {dop=dop; qs=qs; q0=qs.(q0)} |> check_ids
