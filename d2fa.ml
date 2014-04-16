(** D2FA library for compressed DFA, not actively used by flowsifter
    for speed reasons. *)

open Batteries
open Printf

open Regex_dfa
open Ean_std

type deferment = Root | To of int | Unknown
type 'label defer_label = {
  d_label : 'label; (* the original label for the state *)
  mutable defer : deferment
}
type ('label, 'dec) d2fa = ('label defer_label, int IMap.t, 'dec) fa

type ('a, 'b) t = ('a, 'b) d2fa

let is_root q = q.label.defer = Root

module Edge = struct
  type t = int (* pack 3x 16-bit ints into a single value *)
  let _ = assert (max_int = 4611686018427387903) (* need 63-bit ints *)
  let a_shift = 0
  let mask = 65535 (* 2^16 - 1 *)
  let b_shift = 16
  let w_shift = 32
  let compare (x:int) y = Pervasives.compare x y
  let pack a b w = (w lsl w_shift) lor (b lsl b_shift) lor (a)
  let unpack x = (x land mask, (x lsr b_shift) land mask, (x lsr w_shift) land mask)
end

module EdgeSet = Set.Make(Edge)

let print_es oc es =
  EdgeSet.print ~first:"{" ~sep:"" ~last:"}"
    (fun oc x -> let i,j,w = Edge.unpack x in
		 Printf.fprintf oc "(%d, %d, %d)" i j w) oc es

let print_table t =
  IMap.iter_range (fun i j q -> printf "(%d,%d)->%d " i j q) t; print_newline()

(* let edges = Value.observe_int_ref "d2fa candidate edges" (ref 0) *)

(* build the table of commonalities between each pair of states *)
let build_srg ?(min_weight = 10) dfa =
  let n = Array.length dfa.qs in
  let es = ref EdgeSet.empty in
  let counts = Array.create (max_commonality+1) 0 in
  let report_scale = ref 100 in
  let count = ref 0 in
  let inner_count = ref 100 in
  for i = 0 to n-1 do
(*    printf "%d: " i; *)
    for j = i+1 to n-1 do
      let w = commonality dfa.qs.(i) dfa.qs.(j) in
      counts.(w) <- counts.(w) + 1;
(*      Printf.printf "I:"; print_table dfa.qs.(i).map;
      printf "J:"; print_table dfa.qs.(j).map;
      printf "--------------\n"; *)
      if w > min_weight then begin
(*	printf "%d,%d " j w; *)
	es := EdgeSet.add (Edge.pack i j w) !es;
	incr count;
	decr inner_count;
	if !inner_count <= 0 then begin
(*	  printf "%.2f: D2FA Deferment Edges: %d\n%!" (Sys.time ()) !count; *)
	  if !count >= 10 * !report_scale then
	    report_scale := !report_scale * 10;
	  inner_count := !report_scale;
	end
      end;
    done;
(*    printf "\n"; *)
  done;
(*  eprintf "#Commonalities: ";
  Array.iteri (fun i c -> if c > 0 then eprintf "%d of %d," c i) counts;
  eprintf "\n"; *)
  !es

let build_srg d = wrap build_srg "D2fa.build_srg" d

let kruskal_roots n es loopers =
  let ds = Array.init n (fun i -> Disj_set.singleton i) in
  let has_loop = Array.copy loopers in
  let find i = Disj_set.find_label ds.(i) in
  let union i j =
    Disj_set.union ds.(i) ds.(j);
    let is_looper = loopers.(i) || loopers.(j) in
    if is_looper then begin
      has_loop.(find i) <- true;
      has_loop.(find j) <- true;
    end
  in

  let try_join e t =
    let i,j,_ = Edge.unpack e in
(*    Printf.printf "Kruskal - Joining %d and %d..." i j;  *)
    let ipos = find i and jpos = find j in
    if ipos = jpos || (has_loop.(ipos) && has_loop.(jpos)) then
      (* can't make a loop or join two trees with self-looping states *)
      ((*print_string "failed\n";*) t)
    else ((*print_string "succeeded\n";*) union ipos jpos; EdgeSet.add e t) in
  let forest = EdgeSet.fold try_join es EdgeSet.empty in
(*  Array.print ~first:"Cl: [" ~last:"]\n" (List.print Int.print) stdout cl;  *)
  let groups = Array.create n [] in
  let add_to_group i n =
    let g = Disj_set.find_label n in groups.(g) <- i::groups.(g)
  in
  Array.iteri add_to_group ds;
  let trees =
    Array.filter_map (function [] | [_] -> None | lst -> Some lst) groups
  in
  forest, trees

let loop_count dfa i = IMap.fold_range (fun lo hi dec acc -> if dec = i then acc + (hi-lo+1) else acc) dfa.qs.(i).map 0

let is_self_loop limit dfa i =
  loop_count dfa i > limit
(*     |> tap (fun x -> if x then printf "Self-loop found on: %d\n" i) *)


let build_sf ?(self_loop_limit=128) dfa = (* debug wrapper *)
  let es = build_srg dfa in
(*  print_string "High-weight Edge Set: "; print_es es; print_newline (); *)
  let n = Array.length dfa.qs in
  let loops = Array.init n (is_self_loop self_loop_limit dfa) in
  kruskal_roots n es loops

(*
let build_sf dfa =
  let spanning_forest, clusters = build_sf dfa in
  eprintf "#Spanning Forest: %a\n" print_es spanning_forest;
  spanning_forest, clusters
*)
let build_sf ?self_loop_limit d = wrap (build_sf ?self_loop_limit) "D2fa.build_sf" d

module MPM = MultiPMap

(* build edge_distances and deferment-edge lookup through multi-set *)
let defs_of_sf n sf =
  let deferments = EdgeSet.fold (fun e acc -> let i,j,_ = Edge.unpack e in MPM.add i j (MPM.add j i acc)) sf
    (MPM.create Int.compare Int.compare) in
  let distances = Array.make_matrix n n max_int in
  let set_dist e = let i,j,w = Edge.unpack e in distances.(i).(j) <- w; distances.(j).(i) <- w in
  EdgeSet.iter set_dist sf;
  distances, deferments

let total_dist ds nodes node =
(* printf "Total dist: %dx%d ds, nodes: %a, node: %d%!\n" (Array.length ds) (Array.length ds.(0)) (List.print Int.print) nodes node; *)
  let nodes = Array.of_list nodes in
  let dist i j = ds.(nodes.(i)).(nodes.(j)) in
  let dist_a = Array.map (fun j -> ds.(node).(j)) nodes in
  let rec dij ns =
    if ns = [] then () else
      let min_dist_i = Enum.arg_min (fun i -> dist_a.(i)) (List.enum ns) in
      let update_dist j dj =
	let dm = dist_a.(min_dist_i) + dist min_dist_i j in
	if dm > 0 && dm < dj then dist_a.(j) <- dm
      in
      Array.iteri update_dist dist_a;
      dij (List.remove ns min_dist_i)
  in
  dij (Array.range nodes |> List.of_enum);
  Array.reduce max dist_a (* TODO: INVESTIGATE (+) AS METRIC *)

let total_degree ds nodes node =
  List.enum nodes
  |> map (fun i -> if ds.(i).(node) = max_int then 0 else ds.(i).(node))
  |> Enum.sum

let degree ds x = Array.enum ds.(x) |> filter ((>) 0) |> Enum.count

(* get the center of all the nodes *)
let get_center ds = function
    [] | [_] -> assert false (* no empty or single-node clusters *)
  | [x;_] -> x (* either node is the center *)
  | nodes -> Enum.arg_max (total_degree ds nodes) (List.enum nodes)

(*
let get_root vs = maxarg loop_count (List.enum vs)
*)

let reduce_transitions defs qs center =
  let reduce _ _ d1 d2 = match d1, d2 with None, None -> assert false
    (* -1 is failure transition, can't defer on this character *)
    | None, Some _ -> Some (-1)
    | Some x1, Some x2 when x1 = x2 -> None
    | Some x, _ -> Some x
  in
  let defer_to parent child =
    match qs.(child) with
      | {label={defer = Unknown}} as cq ->
	(* reduce the child's transition map *)
	let new_map = IMap.merge ~eq:Int.equal reduce cq.map qs.(parent).map in
	let label = {cq.label with defer=To parent} in
	qs.(child) <- {cq with map=new_map; label=label };
	true
      | _ -> false (* anything else is a loop *)
  in
  let rec defer p = MPM.find p defs |> Set.PSet.iter (fun c -> if defer_to p c then defer c )
  in
  (*  Printf.printf "Deferring from center: %d\n" center; *)
  qs.(center).label.defer <- Root;
  defer center

let of_dfa ?self_loop_limit dfa =
  let (forest, trees) = build_sf ?self_loop_limit dfa in
  (* Array.print ~first:"#Trees: [" ~last:"]\n" (List.print Int.print) stderr trees; *)
  let n = Array.length dfa.qs in
  let (_dists, defs) = wrap (defs_of_sf n) "Defs_of_sf" forest in
  let centers = wrap (Array.map (get_center _dists)) "Get_center" trees in
(*  let centers = wrap (Array.map List.hd) "Get_root" trees in *)
(*  Array.print ~first:"#Centers: [" ~last:"]\n" Int.print stderr centers; *)
  let qs = Array.map (fun i -> {i with label={d_label = i.label; defer=Unknown}} ) dfa.qs in
  Array.iter (reduce_transitions defs qs) centers;
  {dfa with qs = qs; q0 = qs.(dfa.q0.id)}


let merge_maps parent child = IMap.fold_range IMap.add_range child parent
let undefer d2fa q =
  let rec get_full_map q = match q.label.defer with
      Unknown | Root -> q.map
    | To q_parent ->
	let par_map = get_full_map d2fa.qs.(q_parent) in
	merge_maps par_map q.map
  in
  {q with label = {q.label with defer = Root}; map=get_full_map q}

let to_dfa_q d q =
  let q_new = undefer d q in {q_new with label = q.label.d_label}

let to_dfa d2fa = map_qs (to_dfa_q d2fa) d2fa

(*  LISP FORMAT
# comment line
<no of states>
(defptr (char1 state1 char2 state2 ...) <accepted rule numbers>)
...
*)

let print_lisp oc d2fa =
  fprintf oc "# Made by MSU's regex_dfa\n%d\n" (Array.length d2fa.qs);
  let print_tmap oc v = IMap.iter (fun i qi -> fprintf oc "%d %d " i qi) v in
  let print_q oc {label={defer=def};map=map;dec=dec;id=id} =
    (match def with Unknown -> fprintf oc "( -1 ( "
       | Root -> fprintf oc "( %d ( " id
       | To q_def -> fprintf oc "( %d ( " q_def);

    print_tmap oc map; IO.nwrite oc ") ";
    if dec <> [] then List.print Int.print oc dec; IO.nwrite oc " )";
  in
  print_fa ~ids:false print_q oc d2fa

let print_list_dec oc dec = if dec <> [] then List.print Int.print oc dec

let print_q_gen ~print_m ~print_dec oc i {label={d_label=lab; defer=def};map=map;dec=dec} =
  let id = "N" in
  (match def with Unknown | Root -> () | To q_def -> fprintf oc "%s%d -> %s%d [style=dotted];\n" id i id q_def);
  fprintf oc "%s%d [label=\"%d %a\"]; //%a\n" id i i print_dec dec Unit.print lab;
  let trans =
    IMap.fold_range (fun lo hi q acc -> MultiPMap.add q (lo,hi) acc)
      map MultiPMap.empty in
  MultiPMap.iter (fun q lhset ->
		    fprintf oc "%s%d -> %s%a [label=\"" id i id print_m q;
		    Set.PSet.print ~first:"" ~last:"" ~sep:" " print_rng oc lhset;
		    fprintf oc "\"];") trans;

  fprintf oc "\n"

let print_dot_gen ~print_m ~print_dec ~id oc d2fa =
  fprintf oc "digraph %s {\n" id;
  Array.iteri (print_q_gen ~print_m ~print_dec oc) d2fa.qs;
  fprintf oc "}\n"

let print_dot ~id oc d2fa = print_dot_gen ~print_m:Int.print ~print_dec:print_list_dec ~id oc d2fa

let print _ = print_dot

let print_stats oc d2fa =
  let size_counts = Array.create (num_chars+1) 0 in
  let size qmap = IMap.fold_range (fun lo hi _ acc -> hi - lo + 1 + acc) qmap 0 in
  Array.iter (fun q -> let i = size q.map in size_counts.(i) <- size_counts.(i) + 1) d2fa.qs;
  Array.print ~first:"Transition range Counts: [" ~last:"]\n" Int.print oc size_counts;
  fprintf oc "Total transition ranges: %d\n" (Array.reduce (+) size_counts);
(*  Array.enum d2fa.qs |> map opt_table_of_q |> map Vect.length |> print_statistics ~oc:oc; *)
  ()

let run d2fa chars =
  let state_count = Array.length d2fa.qs in
  let next_q (q,_) c =
    let q = d2fa.qs.(q) and c = Char.code c in
    let rec loop q n =
      if n > state_count then failwith (sprintf "deferment loop detected - q%d, c%d" q.id c);
      try (IMap.find c q.map, n)
      with Not_found ->
	match (q.label.defer) with
	    Root | Unknown -> loop d2fa.q0 (n+1)
	  | To q_def -> loop d2fa.qs.(q_def) (n+1)
    in
    loop q 0
  in
  scanl next_q (d2fa.q0.id,0) chars

let print_trace dfa oc trace =
  let last = Enum.reduce (fun _ y -> y) (Enum.clone trace) in
  Enum.print Int.print oc trace;
  printf ": ";
  List.print Int.print oc dfa.qs.(last).dec;
  IO.write oc '\n'

let print_accept dfa oc trace =
  trace /@ (fun q -> dfa.qs.(q).dec)
    // (fun x -> x <> [])
    |> Enum.print ~last:"\n" (List.print Int.print) oc

let qmap_size qmap =
  IMap.fold_range (fun lo hi _ acc -> hi - lo + 1 + acc) qmap 0

(* compression ratio - smaller is better *)
let compression d2fa =
  let total = Array.length d2fa.qs * Regex_dfa.num_chars |> float in
  let needed = Array.enum d2fa.qs |> map (fun q -> qmap_size q.map) |> Enum.sum |> float in
  needed /. total


let add_pair (a,b) (c,d) = a+c, b+d

let summarize oc dfa =
  let tr_count, range_count =
    Array.enum dfa.qs |>
	map (fun q -> IMap.enum q.map |>
	    Enum.fold (fun (t,r) (i,j,_q) -> (t+j-i+1, r+1)) (0,0))
    |> Enum.reduce add_pair in
  let deferred, root =
    Array.fold_left (fun (def, root) {label={defer=d}} ->
      match d with
	  Unknown -> (def, root)
	| To _ -> (def+1, root)
	| Root -> (def,root+1))
      (0,0) dfa.qs in
  Printf.fprintf oc "D2FA %d states, %d transitions (%d ranges)\n" (Array.length dfa.qs) tr_count range_count;
  (*  let finals = Array.fold_left (fun a q -> if q.dec = [] then a else q.dec :: a) [] dfa.qs in
      List.print ~last:"]\n" (List.print Int.print) oc finals; *)
  Printf.fprintf oc "D2FA: %d deferred, %d roots, %d not deferred\n%!" deferred root (Array.length dfa.qs - deferred - root);
  Printf.fprintf oc "TCAM entries required: %d\n%!" (tcam_size dfa);
  ()

let debug_d2fa = false

let run_stream d2fa chars =
  let best_pri, ret =
    match d2fa.q0.dec with
	Some (pri,act,caq) -> ref pri, ref (act,caq,chars)
      | None -> ref max_int, ref ([],None,chars) in
  let rec get_next_id q c = (* handle deferments *)
    try IMap.find (Char.code c) q.map
    with Not_found ->
      match q.label.defer with
	| To q' -> if debug_d2fa then eprintf "D%d %!" q'; get_next_id d2fa.qs.(q') c
	| Root | Unknown -> -1
  in
  let rec next_state q cs n =
    match LazyList.get cs with
      |	None -> !ret,n (* return here *)
      | Some ((_,c),cs') ->
	  let q_next_id = get_next_id q c in
	  if debug_d2fa then
	    eprintf "%s:%d->%d %!" (Char.escaped c) q.id q_next_id;
	  if q_next_id = -1 then
	    !ret,n (* return here *)
	  else if q_next_id >= Array.length d2fa.qs then
	    failwith "referenced state %d invalid"
	  else
	    let q_next = d2fa.qs.(q_next_id) in
	    (match q_next.dec with
	       | Some (pri,act,caq) when pri <= !best_pri ->
		   best_pri := pri;
		   ret := act,caq,cs'
	       | _ -> () );
	    next_state q_next cs' (n+1)

  in
  next_state d2fa.q0 chars 1

exception Done

let resume dfa q0 input offset =
  let best_pri, ret =
    match dfa.qs.(q0).dec with
	Some (pri,act,caq) -> ref pri, ref (act,caq,offset)
      | None -> ref max_int, ref ([],None,offset) in
  let rec get_next_id q c = (* handle deferments *)
    try IMap.find (Char.code c) q.map
    with Not_found ->
      match q.label.defer with
	| To q' ->
	  if debug_d2fa then eprintf "D%d %!" q';
	  get_next_id dfa.qs.(q') c
	| Root | Unknown -> -1
  in
  let rec next_state q i =
    let q = dfa.qs.(q) in
    try
      let c = input.[i] in
      let q_next_id = get_next_id q c in
      if q_next_id = -1 then
	`Dec !ret
      else (
	(match q.dec with
	  | Some (pri,act,caq) when pri <= !best_pri ->
	    best_pri := pri;
	    ret := act,caq,i
	  | _ -> () );
	next_state q_next_id (i+1)
      )
    with
      | Invalid_argument _ -> `End_of_input q.id
  in
  next_state q0 offset

(*
type 'a btree = L of 'a | N of 'a btree * 'a btree

let shadow_encode d2fa =
  let h = Array.fold_left (fun acc q -> Heap.insert acc (1,L q))
    Heap.empty d2fa.qs
  in
  let rec process h =
    if Heap.size h < 2 then
      Heap.find_min h
    else
      let (w1,t1) = Heap.find_min h in
      let h = Heap.del_min h in
      let (w2,t2) = Heap.find_min h in
      let h = Heap.del_min h in
      let h = Heap.insert h (1+Int.max w1 w2, N (t1,t2)) in
      process h
  in
  let (k,t) = process h in
  *)
