open Batteries
open Printf

open Regex_dfa
open D2fa
open Ean_std

let max_stride = 7 (* building a table of a vs_map with stride > this fails *)

(* var_s must not be a single-node FDD by itself *)
type vs_map = { var : (int*int) Fdd.t;
		max : (int*int) Fdd.t;
		stride: int;
		one : int IMap.t }

type ('a,'b) vmd2fa = ('a defer_label, vs_map, 'b) fa

type ('a,'b) t = ('a,'b) vmd2fa

let count map = Fdd.enum map |> Enum.count
let pf = Fdd.print (Tuple2.printn Int.print)
let pfs str fdd = IO.nwrite stdout str; pf stdout fdd; IO.nwrite stdout "\n"

let pair_comp cmp = Tuple2.compare ~cmp1:cmp ~cmp2:Int.compare

let vm_state_of_dstate ~cmp dq =
  let vsmap =
    IMap.map (Fdd.of_term ~cmp) dq.map |> Fdd.of_fdd_imap ~cmp
    |> Fdd.map_dec ~cmp:(pair_comp cmp) (fun q -> q,1)
  in
  {dq with map = {var = vsmap; max=vsmap; stride = 1; one = dq.map}}

let of_d2fa cmp : (('a,'b) d2fa -> ('a,'b) t) = fun d2fa ->
  map_qs (vm_state_of_dstate ~cmp) d2fa


(*
let rec d1 qs qi sg =
  try IMap.find sg qs.(qi).map.one_s
  with Not_found -> d1 qs qs.(qi).label.defer.id sg
*)
let complexity q = Fdd.enum q.map.var |> Enum.count
(*  let m = q.map.var in
  Fdd.walk () *)

let incr_stride fdd = Fdd.map_dec ~cmp:(Fdd.get_cmp fdd) (fun (q,s) -> q,(s+1)) fdd

let increase_stride1 ~cmp is_empty_dec vmd q =
  let new_stride = q.map.stride + 1 in
  let fdd_branch qi =
    if qi <> -1 && is_empty_dec vmd.qs.(qi).dec
    (* extend the transitions through this node *)
    then incr_stride vmd.qs.(qi).map.max
    (* can't extend through this node *)
    else Fdd.of_term ~cmp:(pair_comp cmp) (qi,1)
  in
  let {Cache.get=fdd_branch;_} = Cache.make_ht fdd_branch 10 in
  let new_max = IMap.map fdd_branch q.map.one |> Fdd.of_fdd_imap ~cmp:(pair_comp cmp) in
  let new_var = Fdd.union ~min_key:0 ~max_key:(num_chars-1) ~cmp:(Fdd.get_cmp new_max) (fun _a b -> b) q.map.var new_max in
(*  printf "State: %d\n" q.id;
  pfs "Old Var:" q.map.var;
  pfs "New Max:" new_max;
  pfs "New Var:" new_var;
  Fdd.fdump (Pair.print2 Int.print) stdout new_var;
  flush_all (); *)
  {q with map = {q.map with var = new_var; max = new_max; stride = new_stride}}

let increase_stride1 ~cmp is_empty_dec ~com_lim vmd q =
  if complexity q > com_lim then q else increase_stride1 ~cmp is_empty_dec vmd q

let increase_stride1_all ~cmp is_empty_dec ~com_lim vmd =
  {vmd with qs = Array.map (increase_stride1 ~cmp is_empty_dec ~com_lim vmd) vmd.qs}

let increase_stride_all ~cmp is_empty_dec ~com_lim k vmd =
  power k (increase_stride1_all ~cmp is_empty_dec ~com_lim) vmd

let boosted = ref 0

let boost_if_loop ~cmp is_empty_dec ~loop_lim ~boost q =
  let boost_q q =
    let new_stride = q.map.stride + 1 in
    let ext_one = incr_stride q.map.max in
    let fdd_branch qi = if qi <> q.id then Fdd.of_term ~cmp (qi,1) else ext_one in
    let new_max = IMap.map fdd_branch q.map.one |> Fdd.of_fdd_imap ~cmp in
    let new_var = Fdd.union ~cmp ~min_key:0 ~max_key:(num_chars-1) (fun _a b -> b) q.map.var new_max in
    {q with map = {q.map with var = new_var; max = new_max; stride = new_stride}}
  in
  let loop_count =
    IMap.fold_range (fun lo hi v acc -> if v = q.id then acc+(hi-lo+1) else acc) q.map.one 0
  in
(*  printf "Loop count: %d, is_empty: %B\n" loop_count (is_empty_dec q.dec); *)
  if loop_count < loop_lim || not (is_empty_dec q.dec) then
    q
  else
    (incr boosted; power boost boost_q q)

let boost ~cmp is_empty_dec ?(loop_lim=100) ~boost vmd =
  {vmd with qs = Array.map (boost_if_loop ~cmp:(pair_comp cmp) is_empty_dec ~loop_lim ~boost) vmd.qs}

(*
let print_dot ~id oc vmd =
  fprintf oc "subgraph %s {\n" id;
  let print_q oc i {label={defer=def};map={var_s = map};dec=dec} =
    (match def with None -> () | Some q_def -> fprintf oc "%s%d -> %s%d [style=dotted];\n" id i id q_def.id);
    fprintf oc "%s%d [label=\"%d " id i i;
    List.print Int.print oc dec;
    fprintf oc "\"];\n";
    let trans =
      let add_rev acc map =
	VSMap.fold (fun k q acc -> MultiPMap.add q k acc)
	  map acc in
      Array.fold_left add_rev (MultiPMap.create Int.compare Pervasives.compare) map
    in
    MultiPMap.iter (fun q lhset ->
		      fprintf oc "%s%d -> %s%d [label=\"" id i id q;
		      PSet.print ~first:"" ~last:"" ~sep:" " Vstride.print oc lhset;
		      fprintf oc "\"];") trans;

    fprintf oc "\n"
  in
  Array.iteri (print_q oc) vmd.qs;
  fprintf oc "}\n"
*)

module RS = Ruleset
open RS.Rule

type vst = (nrange,int) rul Vect.t Array.t

let stars i = Enum.repeat ~times:i (0,num_chars-1) |> List.of_enum

let add_transition max_stride tr q acc =
  let lists = List.map (fun iset -> ISet.enum iset |> List.of_enum) tr in
  let lists = lists @ List.map (fun x -> [x])(stars (max_stride - List.length tr)) in
  let rl = List.n_cartesian_product lists in
  List.fold_left (fun acc range_list -> RS.append {pred=range_list;dec=q} acc) acc rl

(* RUN A GIVEN VMD ON A character list *)
let run_vmd vmd char_ll =
  let next_q (q,cll) =
    try
      let ((qid,_stride),cll',n) = Fdd.var_match q.map.var cll 0 in
      assert(_stride = n);
      Some ((qid,n), (vmd.qs.(qid), cll'))
    with Not_found -> None
  in
  Enum.unfold (vmd.q0, char_ll) next_q

let ctrs = Array.create (max_stride+1) 0

(*let () = at_exit (fun () -> Array.print ~first:"VS Counters:[|" ~last:"|]\n"
  Int.print stdout ctrs) *)

let rec resume dfa flow pri ret q i =
  if i >= String.length flow then
    `Need_more_input (q, i, pri, ret)
  else
    try
      (* printf "q(%d).map.var: %a\n" q.id (Fdd.print (Pair.print2
	 Int.print)) q.map.var; *)
      let (qnext, stride),i = Fdd.var_match_str q.map.var flow i in
      if qnext = -1 then
	`Dec ret
      else
	let q = dfa.qs.(qnext) in
	ctrs.(stride) <- ctrs.(stride) + 1;
	match q.Regex_dfa.dec with
	  | Some (pri',act,caq) when pri' <= pri ->
	    resume dfa flow pri' (act,caq,i) q i
	  | Some _ | None ->
	    resume dfa flow pri ret q i
    with
      | Not_found ->
	(match q.label.defer with
	  | Root | Unknown ->
	    `Dec ret (* return highest priority match so far *)
	  | To q_parent -> (* or recurse if there's a state to defer to *)
	    resume dfa flow pri ret dfa.qs.(q_parent) i)


let filter_root q =
  let test (d,_) = d <> q.id in
  if D2fa.is_root q
  then {q with map = {q.map with var = Fdd.filter_dec test q.map.var}}
  else q

let tcam_of_rs rs = Tcam.of_fw_ruleset (List.init max_stride (fun _ -> bits_per_char)) rs

let table_of_q_no_loop q =
  Fdd.enum q.map.var
      |> Enum.filter (fun {dec=(d,_)} -> d <> q.id)
      |> Vect.of_enum

let table_of_q q = Fdd.enum q.map.var |> Vect.of_enum

let boosted_table_of_root ~cmp ~def_dec boost q =
  let table = table_of_q_no_loop q in
  let add_star_fields rs _ =
    Vect.map (fun x -> {pred = (0,num_chars-1)::x.pred;
			  dec=Tuple2.map2 ((+) 1) x.dec}) rs in
  let boost_rv =
    if boost > 1 then
      (* TODO: handle empty one-stride table *)
      Optimizers.raz_no_def q.id q.map.one
	   |> Vect.map (fun x -> {x with dec=(x.dec,1)})
	(*     |> Tcam.of_fw_ruleset [bits_per_char] *)
	   |> Enum.repeat ~times:boost
	   |> Enum.scan add_star_fields
	   |> tap Enum.junk (* throw away the 0-star version - it's already in vs_table *)
	   |> Enum.reduce Vect.concat
    else Vect.empty
  in
  let default =
    {pred = List.init boost (fun _ -> (0,num_chars-1)); dec = q.id,boost} in
  Vect.concat table boost_rv
	   |> Vect.append default
	   |> Fdd.make_rv ~cmp:(pair_comp cmp) ~default:(def_dec,0)

let boosted_table_of_q ~cmp ~def_dec ?(boost=7) q =
  if D2fa.is_root q then (
(*    printf "R%!";  *)
    boosted_table_of_root ~cmp ~def_dec boost q
      |> Optimizers.trazor ~bit_width:bits_per_char ~cmp:(pair_comp cmp)
  ) else (
(*    printf "D%!";  *)
    table_of_q q
      |> tcam_of_rs
      |> Optimizers.bitweave ~cmp:(pair_comp cmp)
  )


let rs_table ~cmp ~def_dec ?(boost=7) vsdfa = Array.map (boosted_table_of_q ~cmp ~def_dec ~boost) vsdfa.qs
let rs_table_lazy ~cmp ~def_dec ?(boost=7) vsdfa = Array.enum vsdfa.qs |> map (boosted_table_of_q ~cmp ~def_dec ~boost)

let trim_fields rs =
  let trim_pred ({pred=pred; dec=(_,stride)} as r) =
    if List.length pred > stride
    then
      let keep,_drop = List.split_at stride pred in
      assert (List.for_all Tcam.Entry.is_stars _drop);
      {r with pred=keep}
    else if List.length pred < stride
    then assert false else r
  in
  Vect.map trim_pred rs

(*
let to_rs_dfa ?(boost=7) vsdfa =
  Regex_dfa.map_qs (boosted_table_of_q ~boost |- trim_fields) vsdfa

let to_rs_dfa_noopt vsdfa = Regex_dfa.map_qs (table_of_q |- tcam_of_rs) vsdfa
*)

let stats_vst vst = Array.enum vst |> Enum.map Vect.length

let tcam_size ~cmp ~def_dec vsdfa = Array.enum vsdfa.qs |> Enum.map (boosted_table_of_q ~cmp ~def_dec ~boost:1 |- Vect.length) |> Enum.reduce (+)
