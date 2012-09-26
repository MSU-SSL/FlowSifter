open Ean_std

open Batteries
open Printf

module D = Decider
module RS = Ruleset
open RS.Rule

type ('a,'b) sol = { cost : Cost.t; rset : ('a,'b) rul Vect.t}
    (* an array of 'a sol has an implicit default color based on the
       position in the array *)

(*
let print_isol i x =
  printf "Default: %d" i;
  RS.print (fun oc _ -> IO.nwrite oc "Dec") stdout x.rset
let print_candidates sa = Array.iteri print_isol sa
*)
(* let rr_build_rt = Time.create "RR.Building_RT" *)
(* let rr_rule_removed = Point.create "RR.rule_removed" *)

let build_rule_table rule_count afdd = (* build lookup [rule -> rule list list] *)
  (* Time.start rr_build_rt; *)
  let rt = Array.make rule_count [] in
  let leaf_set = Fdd.node_sets afdd in
  let f_leaf ris =
    let ris_r = ref ris in
    let ins ri = rt.(ri) <- ris_r :: rt.(ri) in
    List.iter ins ris in
  Set.iter f_leaf leaf_set;
  (* Time.stop rr_build_rt; *)
  rt

let filter_rules rset rule_table =
  let dec_i i = (Vect.get rset i).dec in
  let keep r ts =
    let needed_for rs = match !rs with
	(* a rule is needed for a terminal node if it's the only rule
	   or the first rule shadowing a different second *)
	[] -> assert false
      | [rn] when r != rn -> assert false
      | [_rn] (* r = rn *) -> true
      | r1 :: r2 :: _t when r1 == r && dec_i r1 != dec_i r2 -> true
      | _ -> false
    in
    List.exists needed_for ts in
  let keep_r = ref [] in
  for i = Vect.length rset - 1 downto 0 do
    let ts = rule_table.(i) in
    if (keep i ts)
    then keep_r := (Vect.get rset i) :: !keep_r
    else begin
      (* Point.observe rr_rule_removed; *)
      List.iter (fun rs -> rs := list_remove i !rs) ts;
    end
  done;
  Vect.of_list !keep_r

let redundancy_removal rset =
  Fdd.make_allmatch Fdd.insert_range rset
      |> build_rule_table (Vect.length rset)
      |> filter_rules rset


(* let rr_point = Point.create "RR" *)
(* let rr_timer = Time.create "RR" *)
(* let redundancy_removal rs =  *)
(*   Point.observe rr_point; *)
(*   Time.start rr_timer;  *)
(*   let ret = redundancy_removal_int rs in  *)
(*   Time.stop rr_timer;  *)
(*   ret *)

let ternary_rr rset =
  Fdd.make_allmatch Fdd.insert_entry rset
      |> build_rule_table (Vect.length rset)
      |> filter_rules rset

(* let redundancy_removal x = log_f "Red. Removal" redundancy_removal x *)
(* let redundancy_removal x = x |> rs_stats |> redundancy_removal |> rs_stats   *)

(*** BEGIN TCAM RAZOR ***)
(* let razor1_point = Point.create "Razor1d" *)
(* let razor1d_timer = Time.create "Razor1d" *)

(* internal razor 1d function -- use razor_1d_c or razor_1d_i for complete/incomplete tables *)
let razor_1d_int weight_dec (bdd, decs, of_prefix) =
  (* Utility function *)
  let dec_count = Array.length decs in
  let get_weight i = weight_dec decs.(i) in
(*  eprintf "Decs: %a\n" (Array.print print_any) decs; *)
  (* return a solution array for a range with a particular decision *)
  let unit_sol d pred =
    let base_sol d2 =
      if d = d2 then
	{cost = weight_dec d2;
	 rset = Vect.empty}
      else
	{cost = Cost.add (weight_dec d) (weight_dec d2);
	 rset = Vect.singleton {pred=pred; dec=d} }
    in
    Array.map base_sol decs
  in
  let unite prefix sol1 sol2 = (* merge two solutions *)
    (* the cost of putting default def under overlays sol1.(old) & sol2.(old) *)
    let cost def old_def =
      let sum_old =
	Cost.add
	  (Cost.sub sol1.(old_def).cost (get_weight old_def))
	  sol2.(old_def).cost in
      if def = old_def then sum_old
      else Cost.add sum_old (get_weight def)
    in
    (* finds the solution pair that merges best over the given default *)
    let make_sol def =
      let best_dec = Enum.arg_min (cost def) (0--^dec_count) in
      let cost = cost def best_dec in
      if best_dec = def then
	{cost = cost;
	 rset = RS.join sol1.(best_dec).rset sol2.(best_dec).rset}
      else
	{cost = cost;
	 rset = RS.join sol1.(best_dec).rset sol2.(best_dec).rset
	    ~extra:{pred = of_prefix prefix; dec = decs.(best_dec)}
	}
    in
    Array.init dec_count make_sol
  in
  let rec reduce_bdd prefix = function
    | Bdd.Leaf d -> unit_sol d (of_prefix prefix)
    | Bdd.Node (l,r) ->
	unite prefix (reduce_bdd (0::prefix) l) (reduce_bdd (1::prefix) r)
  in
  (* recursively find the best solutions with each default *)
  (* Point.observe razor1_point; *)
  (* Time.start razor1d_timer; *)
  let ret = reduce_bdd [] bdd in
  (* Time.stop razor1d_timer; *)
  ret

(*let r1dc_p = Point.create "razor_1dc"
let r1dc_t = Time.create "razor_1dc" *)

let razor_1d_c weight_f (bdd, decs, of_prefix) =
(*  Point.observe r1dc_p; Time.start r1dc_t; *)
  let bdd = Bdd.map Option.get bdd in (* remove Some from all bdd leaves *)
  let sols = razor_1d_int weight_f (bdd, decs, of_prefix) in
  let best = Array.range decs |> Enum.arg_min (fun i -> sols.(i).cost) in
  let add_default d sol =
    {sol with rset = Vect.append {pred=of_prefix []; dec=d} sol.rset} in
  sols.(best) |> add_default decs.(best) (*|> tap (fun _ -> Time.stop r1dc_t)*)

(*let r1di_p = Point.create "razor_1di"
let r1di_t = Time.create "razor_1di" *)

(* Like razor_1d_complete but:
 * * Deals with whether the BDD is incomplete
 * * Handles the resulting sol array
 *)
let razor_1d_i weight_f (bdd, decs, of_prefix) =
  if Bdd.is_incomplete bdd then begin
(*    Point.observe r1di_p;
    Time.start r1di_t; *)
    let dec_count = Array.length decs in
    let decs = Array.init (dec_count + 1)
      (fun i -> if i < dec_count then Some decs.(i) else None) in
    let clean_dec d =
      match d.dec with None -> None | Some dec -> Some {d with dec=dec} in
    let weight_dec =
      function None -> Cost.of_int (1 lsl 48) | Some d -> weight_f d in
    let sols = razor_1d_int weight_dec (bdd, decs, of_prefix) in
    let sol = sols.(dec_count) in (* the one with default None *)
    let r = {sol with rset = Vect.filter_map clean_dec sol.rset} in
(*    Time.stop r1di_t; *)
    r
  end
  else
    razor_1d_c weight_f (bdd, decs, of_prefix)

(* runs razor such that a given decision [def] has high cost,
   and returns the incomplete classifier without [def] *)
let razor_1d_no_default def weight_f (bdd, decs, of_prefix) =
  let weight_dec = function
    | x when x = def -> Cost.of_int (1 lsl 48)
    | x -> weight_f x in
  let def_id = Array.findi (fun x -> x = def) decs in
  let bdd = Bdd.map Option.get bdd in (* remove Some from all bdd leaves *)
  let sols = razor_1d_int weight_dec (bdd, decs, of_prefix) in
  sols.(def_id).rset


let range_of_prefix bit_width p =
  let missing = bit_width - List.length p in
  let v0 = List.fold_right (fun d acc -> (acc lsl 1) lor d) p 0 in
  let min = v0 lsl missing in
  let max = min lor ((1 lsl missing) - 1) in
  [(min,max)]

let tcam_entry_of_prefix widths =
    List.backwards
    |- map (function 0 -> Zero | 1 -> One | _ -> assert false)
    |- Tcam.Entry.of_enum ~fields:widths

(*let sod_p = Point.create "struct_of_dec"
let sod_t = Time.create "struct_of_dec"*)

let struct_of_dec ?bit_width decider =
(*  Point.observe sod_p; Time.start sod_t; *)
  let bit_width = match bit_width with
      None -> (try decider |> IMap.domain |> ISet.max_elt |> bits_v with _ -> 1)
    | Some bw -> bw
  in
  assert (bit_width <= Sys.word_size - 2);
  (* Preprocess the tree into the form we'll use *)
  let bdd = Bdd.of_imap bit_width decider
  and of_prefix = range_of_prefix bit_width in
  let decs = D.decs decider |> Set.enum |> Array.of_enum in
(*  Time.stop sod_t; *)
  (bdd, decs, of_prefix)

let struct_of_tcam ?perm tcam =
  let widths = Tcam.Entry.widths (Vect.last tcam).pred in
  let bdd = Bdd.of_tcam ?perm tcam (* encode the ruleset as a bdd *)
  and of_prefix = tcam_entry_of_prefix widths in
  let decs = Tcam.decs tcam |> Set.enum |> Array.of_enum in
  (bdd, decs, of_prefix)

(* let razor_point = Point.create "Razor" *)
(* let rr_skip_point = Point.create "RR skipped" *)

let razor_gen ~to_struct ?rr ?(post = identity) ?(rr_lim = 1000) ?bit_width root =
  let rr = Option.default identity rr in
  (* Point.observe razor_point; *)
  let rr_opt rs =
    if Vect.length rs > rr_lim then ((* Point.observe rr_skip_point; *) rs) else rr rs in
  let rec razor_node = function
      Fdd.T d -> Vect.make 1 {pred=[]; dec=d}
    | Fdd.NT d ->
	(* map from nodes to ints *)
	(* run razor on the nodes as we make their ids *)
	let node_map = Fdd.map_id (fun _ n _id -> razor_node n) in
	(* node_map id requests will recursively call razor_node on children *)
	let get_child_sol n = node_map.get_data (node_map.get_id n) in
	let weight n = get_child_sol n |> Vect.length |> Cost.of_int in

	let sol = (to_struct ?bit_width d |> razor_1d_c weight).rset |> post in

	RS.merge_w_children get_child_sol sol |> rr_opt
  in
  razor_node root |> rr

let razor ?post ?rr_lim ?bit_width root =
  razor_gen ~to_struct:struct_of_dec ~rr:redundancy_removal
    ?post ?rr_lim ?bit_width root

(* let razor_timer = Time.create "Razor" *)
 let razor_fns fns rng r =
(*   Time.start razor_timer;  *)
   {RS.fns=fns; rng=rng; rs=razor r}
(*       |> tap (fun _ -> Time.stop razor_timer) *)

let razor_1d_easy dt = (razor_1d_i (fun _ -> Cost.unit) dt).rset

let raz_dec ?bit_width m = struct_of_dec ?bit_width m |> razor_1d_easy
let raz_tcam ?perm t = struct_of_tcam ?perm t |> razor_1d_easy

let raz_no_def d m =
  struct_of_dec m |> razor_1d_no_default d (fun _ -> Cost.unit)

let sr1d id def_id table map =
  let ret = raz_dec map in
  let rsp = Ruleset.print_rv (List.print print_rng) Int.print in
  fprintf stderr "%d: def -> %d\nPRE:\n%a\nPOST:\n%a\n" id def_id rsp table rsp ret;
  ()

(*** END TCAM RAZOR ***)

(*** BEGIN BITWEAVING ***)
module Entry = Tcam.Entry
let bitweave_divide t = (* divide the TCAM ruleset into pieces for BitWeaving *)
  let add_to_division divs e =
    if Vect.for_all (fun x -> Entry.compatible e.pred x.pred) (Vect.last divs)
    then Vect.modify divs (Vect.length divs - 1) (Vect.append e)
    else Vect.append (Vect.singleton e) divs
  in
  if Vect.is_empty t then Vect.empty
  else
    let e0, rest = Vect.shift t in
    Vect.fold add_to_division (Vect.singleton (Vect.singleton e0)) rest

let comp_partition_any_order t =
  (* partition the TCAM ruleset for BitWeaving, not preserving the
  order of input vector *)
  let add_to_partition prt v =
    try
      let i = Vect.findi (Vect.for_all (Entry.compatible v)) prt in
      Vect.modify prt i (Vect.append v)
    with Not_found -> Vect.append (Vect.singleton v) prt
  in
  Vect.fold add_to_partition Vect.empty t

let column_star_counts es =
  let count_stars i =
    let acc_star acc e = if Entry.get e.pred i = Star then acc+1 else acc in
    Vect.fold acc_star 0 es in
  Array.init (Tcam.width_rv es) count_stars

let chain_comp c1 c2 = fun x y -> match c1 x y with 0 -> c2 x y | d -> d

let permutation_by_count part =
  let count = column_star_counts part in
  let bits = Array.length count in
  let perm_array = 0 --^ bits |> Array.of_enum in
  Array.stable_sort (fun i j -> compare count.(i) count.(j)) perm_array;
  perm_array

let inv_perm p =
  let perm_inv = Array.create (Array.length p) (-1) in
  Array.iteri (fun i j -> perm_inv.(j) <- i) p;
  perm_inv

(* make sure to return groups in decreasing order of prefix size *)
let partition_by_bitmask_dec w etcam =
  let groups = Array.create (w+1) [] in (* TODO: use set instead of list *)
  let push i e =
(*    printf "Entry %a has prefix length %d (w=%d)\n" Entry.print e.pred i w; *)
    groups.(i) <- e :: groups.(i) in
  (* push each entry into an array slot *)
  iter (fun e -> push (Entry.prefix_len e.pred) e) etcam;
  (* return the enum of the list of each slot shortest prefix first *)
  let order = chain_comp (fun r1 r2 -> compare r1.dec r2.dec)
    (fun r s -> compare (List.length r.pred) (List.length s.pred)) in
  Array.map (List.group order) groups
  |> Array.backwards |> map List.enum |> Enum.flatten


let bitmerge_group (type t0) g =
  let compare_r = chain_comp
    (fun x y -> List.length x.pred - List.length y.pred) Pervasives.compare in
  let module Set=Set.Make(struct type t = t0 Tcam.t_rul let compare = compare_r end) in
  let g = Array.of_list g in
  let len = Array.length g in
  let used = Array.create len false in
  let ret = ref Set.empty in
  let merge i j =
    ret := Set.add (Entry.merge g.(i) g.(j)) !ret;
    used.(i) <- true; used.(j) <- true
  in
  for i = 0 to len - 1 do
    for j = i+1 to len - 1 do
      if Entry.mergable g.(i) g.(j) then merge i j
    done
  done;
  let some_used = ref false in
  for i = 0 to len-1 do
    if used.(i) then
      some_used := true
    else
      ret := Set.add g.(i) !ret
  done;
  (Set.enum !ret), !some_used

let enum_map f e = Enum.map f e |> Enum.flatten

let bitmerge tcam = if Vect.length tcam < 2 then tcam else
  let w = Tcam.width_rv tcam in
  let rec loop etcam =
    let groups = partition_by_bitmask_dec w etcam in
    (* Enum.print ~first:"Merge Groups:\n" ~sep:"\n" ~last:"\n" (List.print (RS.Rule.print Entry.print Int.print)) stdout (Enum.clone groups);    *)
    let group_results = map bitmerge_group groups in
    let no_merges = Enum.for_all (fun (_,some_used) -> not some_used) (Enum.clone group_results)
    and entries = enum_map fst group_results in
    if no_merges then Vect.of_enum entries else loop entries
  in
  loop (Vect.enum tcam)

let unweave perm div =
  let perm i = perm.(i) in
  let perm_filt e = {e with pred=Entry.permute perm e.pred} in
  Vect.map perm_filt div


let bitweave_div div =
  (* weave |- to_map |- razor_incomplete |- of_rs |- unweave *)
  (* printf "Division: \n%a" (RS.print_rv Entry.print Int.print) div;  *)
  (* printf "Permutation: %a\n" (Array.print Int.print) perm_a;  *)
  (* printf "After permutation: \n%a" (RS.print_rv (Entry.print_perm perm) Int.print) div;  *)
  (* printf "Razor_sol: \n%a" (Ruleset.print_rv Entry.print Int.print) opt_div;  *)
  (* printf "Before unweave: \n%a" (RS.print_rv Entry.print Int.print) merged;  *)
  (* printf "Out: \n%a" (RS.print_rv Entry.print Int.print) out;  *)
  let perm_a = permutation_by_count div in
  raz_tcam ~perm:(fun i -> perm_a.(i)) div |> bitmerge |> unweave (inv_perm perm_a)

let bitweave tcam =
  if Vect.is_empty tcam then tcam
  else
    bitweave_divide tcam |> Vect.map bitweave_div |> Vect.reduce Vect.concat

let bitweave_rs rs = {rs with RS.rs = bitweave rs.RS.rs }

let trazor ?rr_lim ?bit_width root =
  let to_struct ?bit_width decider =
    let bit_width = match bit_width with
	None -> decider |> IMap.domain |> ISet.max_elt |> bits_v
      | Some bw -> bw
    in
    assert (bit_width <= Sys.word_size - 2);
    (* Preprocess the tree into the form we'll use *)
    let bdd = Bdd.of_imap bit_width decider
    and of_prefix = tcam_entry_of_prefix [bit_width] in
    let decs = D.decs decider |> Set.enum |> Array.of_enum in
    (bdd, decs, of_prefix)
  in
  razor_gen ~to_struct ~post:bitmerge ~rr:ternary_rr ?rr_lim ?bit_width root


(* Generic compression functions *)

let compress rs =
  if Ruleset.is_complete rs then
    RS.wrap (raz_tcam |- bitmerge) rs
  else
    RS.wrap bitweave rs
