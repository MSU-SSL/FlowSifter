open Batteries_uni
open Printf

open Minreg
open Ean_std
open Regex_dfa

let debug = false

let dtap = if debug then fun f x -> f x; x else fun _f x -> x

type qset = int Set.t
type 'a nfa_label = {n_label: 'a; mutable epsilon: qset}
type ('a,'b) nfa_state = ('a nfa_label, qset IMap.t, 'b) state
type ('a,'b) nfa = ('a,'b) nfa_state fa

let print_ids dfa = iter (fun i -> printf "%d) %d  " i dfa.qs.(i).id) (0 --^ size dfa)
let print_iset oc s = Set.print ~first:"{" ~sep:"," ~last:"}" Int.print oc s

let print_tmap oc v = IMap.iter_range (print_range Int.print oc) v
let print_ntmap oc v = IMap.iter_range (print_range print_iset oc) v



let print_nfa write_label write_dec oc nfa =
  let print_q oc {id=id;label={n_label=nlabel; epsilon=eps};map=map;dec=dec} =
    if id > 0 then fprintf oc "%a %a\n#%a d:%a\n"
      write_label nlabel
      (fun oc e -> if Set.is_empty e then () else fprintf oc " eps: %a" print_iset e) eps
      print_ntmap map
      write_dec dec;
  in
  print_fa print_q oc nfa

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

(** BUILD A NFA FROM A GIVEN REGEXP and some decision handling data *)
let build_nfa (dec0, of_dec, _, dec_comp) reg =
  let get_idx = make_counter 0 in
  let qs = ref Vect.empty in
  let new_state ?(label=None) ?(epsilon=Set.empty)
      ?(map=IMap.empty) ?(dec=dec0) () =
    let idx = get_idx () in
     (*    if idx mod 100 = 1 then eprintf "N %d @ %.2f\n%!" idx (Sys.time ()); *)
    let q = { label = {n_label = label; epsilon = epsilon};
	      map = map; dec = dec; id = idx} in
    vect_set_any qs idx q;
    q
  in
  let dup x = (x,x) in
  let rec loop = function
    | Value v -> let out_q = new_state () in let in_q = new_state ~map:(IMap.set_to_map v (Set.singleton out_q.id)) () in in_q,out_q
    | Accept id -> new_state ~dec:(of_dec id) () |> dup
    | Union rl ->
      let ins,outs =
	Set.enum rl |> List.of_enum |> List.map loop |> List.split
      in
      let in_q = new_state ~epsilon:(List.map (fun x -> x.id) ins|> List.enum |> Set.of_enum) () in
      let out_q = new_state () in
      let eps = Set.singleton out_q.id in
      List.iter (fun oi -> oi.label.epsilon <- eps) outs;
      in_q, out_q
    | Kleene r ->
      let in1,out1 = loop r in
      let out0 = new_state () in
      let in0 = new_state ~epsilon:(Set.singleton in1.id |> Set.add out0.id) () in
      out1.label.epsilon <- (Set.singleton in0.id);
      in0,out0
    | Concat ([],_) -> new_state () |> dup
    | Concat (Value v::t,_red) ->
      let in1,out1 = loop (Concat (t,_red)) in
      new_state ~map:(IMap.set_to_map v (Set.singleton in1.id)) (), out1
    | Concat (rx::t,_red) ->
      let in0,out0 = loop rx in
      let in1,out1 = loop (Concat (t, _red)) in
      out0.label.epsilon <- (Set.singleton in1.id);
      in0, out1
  in
  let q0, _ = reg |> factor_rx ~dec_comp |> loop in
  let qs = Vect.map Option.get !qs |> Vect.to_array in
  ({qs=qs; q0=q0} : ('a,'b) nfa)

let merge_iset = Set.union (*(List.rev_append a b) |> List.sort_unique Int.compare *)

let merge_nfa_maps m1 m2 =
  let merge_dst d1o d2o = match d1o,d2o with
      None, Some d | Some d, None -> Some d
    | Some d1, Some d2 -> Some (merge_iset d1 d2)
    | None, None -> assert false
  in
  IMap.union merge_dst m1 m2

type 'a hmap_status = Unmade | In_progress | No_change | Done of (qset IMap.t * qset * 'a)

let simplify_nfa (dec0, _of_dec, dec_merge, _dec_comp) nfa = (* remove in-epsilon-only states *)
  let remove = Array.create (size nfa) true in
  let set_unremovable d = remove.(d) <- false in
  let set_targets_unremovable q =
    IMap.iter (fun _c dsts -> Set.iter set_unremovable dsts) q.map
  in
  let hoisted_maps = Array.create (size nfa) Unmade in
  let merge_hmap i (m,e,d) = match hoisted_maps.(i) with
    | In_progress -> m,e,d
    | No_change -> let q = nfa.qs.(i) in
		   (merge_nfa_maps m q.map), (merge_iset e q.label.epsilon), (dec_merge d q.dec)
    | Done (m2,e2,d2) ->  (merge_nfa_maps m m2), (merge_iset e e2), (dec_merge d d2)
    | Unmade -> assert false
  in
  let rec hoist_removed_targets i =
    match hoisted_maps.(i) with
      |	Done _ | In_progress | No_change -> ()
      | Unmade ->
	hoisted_maps.(i) <- In_progress;
	let to_hoist = ref Set.empty in
	IMap.iter_range (fun _ _ dst ->
	  to_hoist := Set.union (Set.filter (fun i -> remove.(i)) dst) !to_hoist)
	    nfa.qs.(i).map;
	let eps_remove, eps_keep = Set.partition (fun i -> remove.(i)) nfa.qs.(i).label.epsilon in
	to_hoist := Set.union eps_remove !to_hoist;
	if Set.is_empty !to_hoist then
	  hoisted_maps.(i) <- No_change
	else begin
	  Set.iter hoist_removed_targets !to_hoist;
	  let med = Set.fold merge_hmap !to_hoist (nfa.qs.(i).map,eps_keep,nfa.qs.(i).dec) in
	  hoisted_maps.(i) <- Done med
	end
  in
  let null_q = {id=(-1); map=IMap.empty; dec=dec0;
		label={n_label=nfa.q0.label.n_label; epsilon=Set.empty};}  in
  let q_with_hoisted_map q =
    match hoisted_maps.(q.id),remove.(q.id) with
      | Unmade,_ | In_progress,_ -> assert false
      | _, true -> null_q
      | No_change,false -> q
      | Done (m,e,d),false->
	{q with map = m; label={q.label with epsilon = e}; dec = d}
  in
  (* don't remove start state *)
  remove.(nfa.q0.id) <- false;
  (* mark removable states *)
  Array.iter set_targets_unremovable nfa.qs;
  let num_to_remove = Array.filter identity remove |> Array.length in
  if debug then printf "Simplify_nfa: removing %d of %d nfa states\n" num_to_remove (size nfa); 
  iter hoist_removed_targets (Array.range hoisted_maps);
  map_qs q_with_hoisted_map nfa

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

let close_transitions nfa =
  let q_with_closed_map q =
    let new_map = IMap.map (fun dsts -> Set.enum dsts |> map (get_eps_closure nfa) |> Enum.reduce Set.union ) q.map in
    {q with map=new_map}
  in
  map_qs q_with_closed_map nfa

let print_nfa_label _oc _l = ()
let print_nfa_dec oc d = Std.dump d |> String.print oc

let build_dfa ~labels (dec0, _of_dec, dec_merge, _dec_comp as dec_rules) reg =
  ignore labels;
  let nfa = build_nfa dec_rules reg 
		   |> dtap (printf "NFA0: \n%a" (print_nfa print_nfa_label print_nfa_dec)) 
		   |> simplify_nfa dec_rules
		   |> dtap (printf "NFA1: \n%a" (print_nfa print_nfa_label print_nfa_dec)) 
		   |> close_transitions in
  if debug then (
    printf "NFA built %.3f (%d states)\n%!" (Sys.time()) (Array.length nfa.qs); 
    printf "NFA2: \n%a" (print_nfa print_nfa_label print_nfa_dec) nfa;
    printf "q0: %d\n" nfa.q0.id;  
  );
  let states = ref Vect.empty in
  let make_node get_id ns id =
    (* get the transitions for that character from the nfa *)
(*    if id mod 100 = 1 then eprintf "N %d @ %.2f\n%!" id (Sys.time ()); *)
    let map =
      Set.fold (fun n acc -> merge_nfa_maps acc nfa.qs.(n).map) ns IMap.empty
      |> IMap.map (get_id |- Id.to_int)
    in
    let dec =
      Set.fold (fun n acc -> dec_merge acc nfa.qs.(n).dec) ns dec0
    in
    let q = {label = None; id=id; map=map; dec=dec} in
    vect_set_any states id q;
    ()
  in
  let set_compare s1 s2 = Enum.compare Int.compare (Set.enum s1) (Set.enum s2) in
  let id_map = map_id_set ~comp:set_compare ~min_id:0 make_node in
  let q0 = get_eps_closure nfa nfa.q0.id |> id_map.get_id |> Id.to_int in
  let qs = Vect.map Option.get !states |> Vect.to_array in
  {qs=qs; q0=qs.(q0)} |> check_ids
