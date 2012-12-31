open Batteries
open Ean_std
open Printf
(* open Ocamlviz *)

(*
let fdd_node_tag = Tag.create ~size:true ~count:true ~period:1000 "fdd node"
let afdd_node_tag = Tag.create ~size:true ~count:true ~period:1000 "afdd node"
*)
module D = Decider
module RS = Ruleset
open RS.Rule

type 'a tree = NT of 'a tree Decider.t | T of 'a
type 'a t = {fdd : 'a tree; cmp: 'a -> 'a -> int}

let get_cmp {cmp;_} = cmp
let get_tree {fdd} = fdd

let rec compare_aux cmp x y = match (x,y) with
  | T c, T d -> cmp c d
  | NT d1, NT d2 -> D.compare (compare_aux cmp) d1 d2
  | T _, NT _ -> -1
  | NT _, T _ -> 1

let compare {fdd;cmp} {fdd=fdd2;cmp=cmp2} =
  assert (cmp == cmp2); compare_aux cmp fdd fdd2

let empty cmp = { fdd=NT (D.empty (fun a b -> compare_aux cmp a b = 0)); cmp}

let rec enum_aux = function
    T c -> Enum.singleton {pred=[]; dec=c}
  | NT d -> D.enum d |> Enum.map (fun (min,max,ch) -> enum_aux ch |> Enum.map (fun r -> {r with pred=(min,max)::r.pred})) |> Enum.concat

let enum {fdd} = enum_aux fdd

let equal t1 t2 = compare t1 t2 = 0

let hash t = Hashtbl.hash_param 20 100 t

let map_id f = map_id_set ~min_id:0 (*~comp:compare*) f

let rec map_dec_aux f = function
    T c -> T (f c)
  | NT d -> NT (IMap.map (map_dec_aux f) d)

let map_dec ~cmp f {fdd;_} = {cmp; fdd=map_dec_aux f fdd}

(* lookup a decision in a FDD given a list of fields to match on *)
let rec decide = function (* vals, fdd *)
  | [], _ -> failwith "Fdd.decide: not enough values"
  | [v], T d -> D.lookup d v
  | _, T _ -> failwith "Fdd.decide: too many values"
  | h::t, NT d -> decide (t,D.lookup d h)

let rec var_match_aux fdd cll n = match fdd, LazyList.get cll with
    T d, _ -> d, cll, n
  | NT _, None -> raise End_of_file
  | NT m, Some (chr, tl) -> var_match_aux (D.lookup m (Char.code chr)) tl (n+1)

let var_match {fdd} cll n = var_match_aux fdd cll n

let rec var_match_str_aux fdd flow_data n = match fdd with
  | T d -> d, n
  | NT m ->
    if n >= String.length flow_data then
      var_match_str_aux (D.lookup m 0xff) flow_data (n+1)
    else
      let fdd' = flow_data.[n] |> Char.code |> D.lookup m in
      var_match_str_aux fdd' flow_data (n+1)

let var_match_str {fdd} flow_data n = var_match_str_aux fdd flow_data n

let walk
    ?(f_pre=(fun _ _->())) ?(f_post=(fun _ _->()))
    ~f_node_rng ~f_leaf fdd =
  let queue = Queue.create () in
  let node_map = map_id (fun _ n _id -> Queue.add n queue) in
  let subtree a b d = f_node_rng a b (node_map.get_id d) d in
  let act n =
    let id = node_map.get_id n in
    f_pre n id;
    (* enqueues unvisited child nodes *)
    (match n with T c -> f_leaf c | NT d -> D.iter subtree d );
    f_post n id
  in
  let rec loop () =
    try act (Queue.take queue); loop ()
    with Queue.Empty -> () in
  let _ = node_map.get_id fdd in (* prime the work queue *)
  loop ()

let node_sets {fdd} =
  let rec loop n acc = match n with
    | NT d -> Set.fold loop (D.decs d) acc
    | T d -> Set.add d acc
  in
  loop fdd Set.empty

let pipe_stats str fdd =
  let nodes = ref 0 and leaves = ref 0 in
  let f_leaf _ = incr leaves and f_node_rng _ _ _ _ = incr nodes in
  walk ~f_leaf ~f_node_rng fdd;
  eprintf "(%s) Nodes: %d  Leaves: %d\n" str !nodes !leaves;
  fdd

let print print_dec chan fdd =
  enum fdd |> Enum.print ~sep:"\n" (print_rule (List.print (Tuple2.printn Int.print)) print_dec) chan
(*
let dec_dump d =
  if Obj.is_block (Obj.repr d) then
    let os = IO.output_string () in
    fdump2 os hex_string_of_int (Obj.magic d);
    IO.close_out os
  else
    string_of_int (Obj.magic d)
    *)


let compact x = x

let reduce = function
  | {fdd=T _} as e -> e
  | {cmp; fdd=root} -> (* nonterminal compaction *)
      let unique_add ht n =
	try Hashtbl_param.find ht n
	with Not_found -> Hashtbl_param.add ht n n; n
      in
      let make_ht n = Hashtbl_param.create hash (compare_aux cmp) n in
      let rec make_umap ht_lev = (* recursive step *)
	(* get all children as hash_set *)
	let ht_chi = make_ht 1024 in
	let add_child _node = function
	  | T _ -> assert false
	  | NT d -> D.iter (fun _ _ c -> ignore(unique_add ht_chi c)) d
	in
	Hashtbl_param.iter add_child ht_lev;
	(* reduce the children *)
	let ht_chi =
	  match Hashtbl_param.choose ht_chi with
	      (T _, _) -> ht_chi (* children are terminal *)
	    | (NT _,_) -> make_umap ht_chi (* need to reduce children *)
	in
	(* reduce the current level *)
	let ht_ulev = make_ht (Hashtbl_param.length ht_lev * 2) in
	let add_reduced _n = function T _ -> assert false
	  | NT d -> (* repoint to reduced children *)
	    let d' = D.map (fun c -> Hashtbl_param.find ht_chi c) d in
	    NT d' |> unique_add ht_ulev
	in
	(* change my hashset into hashmap with nodes pointing to their
	   reduced version *)
	Hashtbl_param.map add_reduced ht_lev
      in
      let ht_root = make_ht 1 in
      Hashtbl_param.add ht_root root root;
      let ht_root' = make_umap ht_root in
      {cmp; fdd=Hashtbl_param.find ht_root' root}

(* let reduce x = x |> pipe_stats "pre" |> reduce |> pipe_stats "post" *)
let reduce fdd = log_f "Reducing" reduce fdd

(*
let reduce_timer = Time.create "FDD.reduce"
let reduce x =
  Time.start reduce_timer;
  let ret = reduce x in
  Time.stop reduce_timer;
  ret
*)
(* prints the fdd to the given channel *)
let fdump stringify chan fdd =
  let f_leaf color =
    Printf.fprintf chan "%a " stringify color
  and f_node_rng min max id _d =
    Printf.fprintf chan "(%X-%X):T%d " min max (Id.to_int id)
  and f_pre _n id = Printf.fprintf chan "Table %i -- " (Id.to_int id);
  and f_post _n _id = Printf.fprintf chan "\n"
  in
  Printf.fprintf chan "-------------------------\n";
  walk ~f_pre ~f_post ~f_node_rng ~f_leaf fdd;
  Printf.fprintf chan "-------------------------\n"

let decisions fdd =
  let out = ref Set.empty in
  let f_leaf c = out := Set.add c !out
  and f_node_rng _min _max _id _d = () in
  walk ~f_leaf ~f_node_rng fdd;
  Set.enum !out

let rec slice_aux nrange fdd =
  match (nrange, fdd) with
      ([], T d) -> T d
    | ((lo,hi) :: t, NT d) ->
	NT (D.sub lo hi d |> D.map (slice_aux t))
    | _, _ -> assert false

let slice nrange t = {t with fdd = slice_aux nrange t.fdd}

let make_empty ~eq default range =
  let rec make_aux = function
    | [] -> T default
    | (low,hi)::t -> NT (D.make_default ~eq low hi (make_aux t))
  in
  make_aux range

let of_imap x = NT (IMap.map (fun i -> T i) x)
let of_fdd_imap ~cmp x = {cmp; fdd=NT (D.map (fun {fdd;_} -> fdd) x)}
let of_term ~cmp x = {cmp; fdd = T x}

let insert_range merge {pred=pred; dec=act} {cmp; fdd} =
  let rec insert_aux = function (* pred, fdd *)
      [], T d -> T (merge (Some d) act)
    | [], NT _ -> T (merge None act)
    | (low,hi)::t, ((T _) as n)->
	(* insert a non-terminal *)
	let child = insert_aux (t, n)
	and as_d = D.default ~eq:(BatOrd.eq_comp (compare_aux cmp))  n in
	NT (D.add low hi child as_d)
    | (low,hi)::t, NT d ->
	(* modify the nonterminal *)
	let sub_fdd = D.sub low hi d
	and add_child low hi child acc =
	  D.add low hi (insert_aux (t,child)) acc in
	NT (D.fold add_child sub_fdd d)
  in
  {cmp; fdd=insert_aux (pred,fdd)}

let insert_entry merge {pred=entry; dec=act} {cmp;fdd} =
  let rec insert_aux = function (* pred, fdd *)
      [], T d -> T (merge (Some d) act)
    | [], NT _ -> T (merge None act)
    | f::t, ((T _) as n)->
	(* insert a non-terminal *)
	let child = insert_aux (t, n)
	and as_d = D.default  ~eq:(BatOrd.eq_comp (compare_aux cmp)) n in
	let add_rng acc (low,hi) = D.add low hi child acc in
	NT (Tcam.Entry.enum_ranges_f f |> fold add_rng as_d)
    | f::t, NT d ->
	(* modify the nonterminal *)
	let add_rng acc (low,hi) =
	  let sub_fdd = D.sub low hi d
	  and add_child low hi child acc =
	    D.add low hi (insert_aux (t,child)) acc in
	  IMap.fold_range add_child sub_fdd acc
	in
	NT (Tcam.Entry.enum_ranges_f f |> fold add_rng d)
  in
  {cmp; fdd=insert_aux (entry,fdd)}



let insert fdd rul = insert_range (fun _ x -> x) rul fdd

let permute_list il l = List.map (List.at l) il
let make_rv ?perm ~cmp ~default rv =
  let permute r =
    match perm with
	None -> r
      | Some p -> {r with pred = permute_list p r.pred}
  in
  Vect.backwards rv |> Enum.map permute |> Enum.fold insert {cmp; fdd=T default}

let make ?perm rs = make_rv ?perm rs.RS.rs

(* let make_timer = Time.create "FDD.make" *)
(* let make ?perm x =  *)
(*   Time.start make_timer;  *)
(*   let ret = make ?perm x in  *)
(*   Time.stop make_timer;  *)
(*   ret *)

(* let make ?perm x = log_f "Making FDD" (make ?perm) x *)

let make_allmatch cmp inserter rules =
  let rule_count = Vect.length rules in
  let fdd = ref {cmp=List.compare cmp; fdd=T []} in
  (* create lists of rule numbers *)
  for i = rule_count - 1 downto 0 do
    let merge old _x = match old with None -> [i] | Some l -> i :: l in
    (*      if i mod 100 = 0 then eprintf "%d %!" i; *)
    fdd := inserter merge (Vect.get rules i) !fdd;
  done;
  !fdd

(* let amake_timer = Time.create "FDD.make_allmatch" *)
(* let make_allmatch x =  *)
(*   Time.start amake_timer;  *)
(*   let ret = make_allmatch x in  *)
(*   Time.stop amake_timer;  *)
(*   ret *)

(*BROKEN IMap Union code
  let find_cuts d1 d2 =
    Enum.merge (<) (D.enum_cuts d1) (D.enum_cuts d2) |> Enum.uniq in
  let union_nodes comb d1 d2 =
    let aux (d,last) cut =
      if cut = max_int then (d,max_int) else
	let c1 = try Some(D.lookup d1 cut) with Not_found -> None
	and c2 = try Some(D.lookup d2 cut) with Not_found -> None in
	let dout = match c1,c2 with
	    Some c1, Some c2 -> D.add last cut (comb c1 c2) d
	  | None, Some c | Some c, None -> D.add last cut c d
	  | None, None -> assert false
	in
	dout, cut+1
    in
    let min_key = try min (D.min_domain d1) (D.min_domain d2) with Not_found -> 0 in
    Enum.fold aux (D.empty, min_key) (find_cuts d1 d2) |> fst
  in *)


(** MERGE TWO FDDs *)
let union ?min_key ?max_key ~cmp f_dec {fdd=fdd1;_} {fdd=fdd2;_} =
  let eq = BatOrd.eq_comp (compare_aux cmp) in
  let rec traverse n1 n2 = match (n1,n2) with
    | T d1, T d2 -> T (f_dec d1 d2)
    | NT d1 , NT d2 -> NT (IMap.union traverse d1 d2)
    | NT _, T a -> traverse n1 (NT (D.default ~eq ?min_key ?max_key (T a)))
    | T a, NT _ -> traverse (NT (D.default ~eq ?min_key ?max_key (T a))) n2
  in
  {cmp = cmp; fdd=traverse fdd1 fdd2}

let cover f1 f2 = union ~cmp:f2.cmp (fun _ d2 -> d2) f1 f2

let split base nrange fdd =
  cover base (slice nrange fdd),
  cover fdd (slice nrange base)

(*
let union_timer = Time.create "FDD.union"
let union f t1 t2 =
  Time.start union_timer;
  let ret = union f t1 t2 in
  Time.stop union_timer;
  ret
*)

let packet_to_range ?(perm=[0;1;2;3;4]) p =
  let a = [|p.proto; p.src; p.dst; p.sp; p.dp|] in
  List.map (fun i -> a.(i)) perm
let decide_p ?perm p t = decide ((packet_to_range ?perm p),t)

let of_enum cmp e = Enum.fold insert (empty cmp) e

let filter_dec p {fdd; cmp} =
  enum_aux fdd |> Enum.filter (fun r -> p r.dec) |> of_enum cmp
