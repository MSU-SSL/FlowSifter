open Batteries_uni
open Printf
open Minreg

(* magic constants *)
let bits_per_char = 8
let num_chars = (1 lsl bits_per_char)
let max_commonality = num_chars

let map_base = IMap.empty

let map_of_val s v = IMap.set_to_map s v

type 'a norm_regex = int list * 'a Minreg.t IMap.t

(* takes norm_regex enum and returns a single norm_regex *)
let merge2 ~dec_comp e1 e2 = 
  (*  eprintf "Merging %a with %a\n%!" (Option.print Minreg.printp) e1 (Option.print Minreg.printp) e2; *)
  match (e1,e2) with
    | None, None -> assert false
    | None, Some x | Some x, None -> Some x
    | Some e1, Some e2 when Minreg.compare ~dec_comp e1 e2 = 0 -> Some e1
    | Some e1, Some e2 -> 
      Some (reduce_union e1 e2)

let merge_dlists d1 d2 = if d1 = [] then d2 else if d2 = [] then d1 else
    List.rev_append d1 d2 |> List.sort_unique Pervasives.compare
(* for constructing DFAs that use lists as decisions *)
let merging_dec_lists = ([],(fun i -> [i]), merge_dlists)

let merging_dec_sets = ([], ISet.singleton, ISet.union)
(*
  let last_depth = Value.observe_int_ref "cur-depth canonized" (ref 0) 
  and last_width = Value.observe_int_ref "cur-width canonized" (ref 0) 
*)
let reduce_pair ~dec_comp merge_d (d1, m1) (d2,m2) = 
  if IMap.is_empty m1 && IMap.is_empty m2 then
    (merge_d d1 d2), m1
  else
    (merge_d d1 d2), (IMap.union (merge2 ~dec_comp) m1 m2) 

(*
  let can_found = Value.observe_int_ref "can_found" (ref 0) 
  and can_calc = Value.observe_int_ref "can_calc" (ref 0) 
*)

let canonize (nul_d, merge_d, dec_comp) rx =
  let merge norms = Enum.reduce (reduce_pair ~dec_comp merge_d) norms in

  let canonized = ref (Map.create (Minreg.compare ~dec_comp)) in

  let rec canon rx =
    try Map.find rx !canonized (*|> tap (fun _ -> incr can_found)*)
    with Not_found ->
      (* insert a dummy value to prevent loops *)
      canonized := Map.add rx (nul_d, map_base) !canonized;
      let ret = match rx with
	| Concat (Value x::t, red) ->
	  nul_d, map_of_val x (Concat (t,red))
	| Union x when Set.is_empty x -> nul_d, map_base
	| Union x -> merge (Set.enum x |> map canon |> tap Enum.force)
	| Concat ((Kleene x) :: t, red) ->
	  let tl = Concat (t,red) in
	  canon (union2 tl (concat [x;Kleene x;tl]))
	| Value x -> nul_d, (map_of_val x epsilon)
	| Kleene x -> union2 epsilon (concat [x; Kleene x]) |> canon
	| Concat (Union u :: t, _) ->
	  union (Set.map (append t) u) |> canon
	| Accept (i,p) -> (i,p), map_base
	| Concat (Concat _ :: _, _)
	| Concat ([],_) | Concat (Accept _::_,_) -> assert false
      in
      if false then (
	eprintf "#Canonizing: %a\n%!" (Minreg.printp ~dec:false) rx;
	eprintf "#Result: %a\n%!" print_norm_regexp ret; 
      );
      canonized := Map.add rx ret !canonized;
      ret
  in
  reduce rx |> canon

(*
  let c_time = Time.create "Canonize"
  let canonize x = Time.start c_time; let r = canonize x in Time.stop c_time; r
*)

open Ean_std

type 'a dec_ops = {
  dec0 : 'a;
  merge: 'a -> 'a -> 'a;
  cmp: 'a -> 'a -> int;
}

type ('a,'b, 'c) state = {
  id    : int; (* the index of this state in the state array *)
  pri   : int; (* the priority of the highest match reachable from this state *)
  label : 'a; (* a label for this state - used for deferments *)
  map   : 'b; (* the map to next states *)
  dec   : 'c; (* a decision type, usually option or list *)
  dec_pri: int; (* the priority of this decision *)
}

type ('a,'b,'c) fa = {
  qs: ('a,'b,'c) state array;
  q0: ('a,'b,'c) state;
  dop: 'c dec_ops
}

type 'a dfa = ('a, int IMap.t, int list) fa

let size {qs=qs} = Array.length qs
let trans {qs=qs} = 
  let trans_q acc q = 
    IMap.fold_range (fun lo hi _ (rngs,inds) -> rngs+1, inds+hi-lo+1) q.map acc
  in
  Array.fold_left trans_q (0,0) qs
let map_qs f fa = 
  let qs = Array.map f fa.qs in 
  {qs=qs; q0=qs.(fa.q0.id); dop=fa.dop}

let decs fa = Array.map (fun q -> q.dec) fa.qs

let print_ids dfa = iter (fun i -> printf "%d) %d  " i dfa.qs.(i).id) (0 --^ size dfa)

let check_ids dfa = assert (for_all (fun i -> if dfa.qs.(i).id <> i then (eprintf "ID FAIL: id %d found at position %d" dfa.qs.(i).id i; print_ids dfa; assert false) else true ) (0 --^ size dfa)); dfa

let index_print print_v oc i v = fprintf oc "#%d) %a\n" i print_v v

let print_fa ?(ids=true) print_q oc {qs=qs; q0=_q0} =
  (*  IO.nwrite oc "#States:\n"; *)
  if ids then 
    Array.iteri (index_print print_q oc) qs
  else 
    Array.print ~first:"" ~last:"" ~sep:"\n" print_q oc qs;
  (*  Printf.fprintf oc "#Start: %a\n" print_q q0; *)
  ()

let print_tmap oc v = IMap.iter_range (print_range Int.print oc) v

let print_dfa write_label ?(write_dec=(List.print Int.print)) oc dfa =
  let print_q oc {label=label;map=map;dec=dec} = 
    fprintf oc "%a\n#%a %a\n" write_label label print_tmap map write_dec dec;
  in
  print_fa print_q oc dfa

let print_dec oc dec = if dec <> [] then List.print Int.print oc dec

let print_dot_dfa ~id ~print_dec oc dfa =
  fprintf oc "digraph %s {\n" id;
  let print_q oc i {label=_label; map=map; dec=dec; id=q_id} =
    fprintf oc "%s%d [label=\"%dx%d " id i i q_id;
    print_dec oc dec;
    fprintf oc "\"];\n";
    let trans = 
      IMap.fold_range (fun lo hi q acc -> MultiPMap.add q (lo,hi) acc) 
	map (MultiPMap.create Int.compare Pair.compare) in
    MultiPMap.iter (fun q lhset -> 
      fprintf oc "%s%d -> %s%d [label=\"" id i id q;
      PSet.print ~first:"" ~last:"" ~sep:" " print_rng oc lhset;
      fprintf oc "\"];") trans;
    fprintf oc "\n"
  in
  Array.iteri (print_q oc) dfa.qs;
  fprintf oc "}\n"

let print_dfa _ ~id oc d = print_dot_dfa ~print_dec ~id oc d

let count_qs f qs = Array.enum qs |> map f |> Enum.sum

let summarize_dfa ~id oc dfa = 
  let tr_count = count_qs (fun q -> IMap.enum q.map |> Enum.count) dfa.qs in
  fprintf oc "#DFA %s: %d states, %d transition ranges\n" id (Array.length dfa.qs) tr_count;
  (*  
      let finals = Array.fold_left (fun a q -> if q.dec = [] then a else q.dec :: a) [] dfa.qs in
      List.print ~last:"]\n" (List.print Int.print) oc finals; 
  *)
  ()

(*let max_depth = Value.observe_int_ref "Depth (Max)" (ref 0) *)

let print_int_range oc x y = if y > x then fprintf oc "%d-%d " x y else fprintf oc "%d " x

let reachable {qs=qs; q0=s0} =
  let rec loop reached news =
    let reached = ISet.union reached news in
    (*    print_string "Reached: "; ISet.iter_range (print_int_range stdout) reached; print_newline ();  *)
    (* build the set of next hops from a node *)
    let nexts q = IMap.fold_range (fun _ _ -> ISet.add) qs.(q).map ISet.empty in
    let next = ISet.fold (fun i acc -> ISet.union acc (nexts i)) news ISet.empty in
    (* print_string "Next: "; ISet.iter_range (print_int_range stdout) next; print_newline ();  *)
    let news = ISet.diff next reached in
    if ISet.is_empty news then reached else loop reached news
  in
  loop ISet.empty (ISet.singleton s0.id)

let reachable x = log_f "Reachable" reachable x

let remove_unreachable dfa = 
  let keep = reachable dfa |> ISet.elements |> Array.of_list in
  (*   printf "#Reachable states: %d\n%!" (Array.length keep); *)
  let n = Array.length dfa.qs in
  let rep_state = Array.make n (-1) in
  Array.iteri (fun i r -> rep_state.(r) <- i) keep;
  let mod_tr tr = IMap.fold_range (fun lo hi q acc -> if rep_state.(q) = -1 then assert false else IMap.add_range lo hi rep_state.(q) acc) tr IMap.empty in
  let mod_state pos i = {dfa.qs.(i) with map = mod_tr dfa.qs.(i).map; id = pos} in
  let qs = Array.mapi mod_state keep in
  (*  let mod_state i = {dfa.qs.(i) with map = mod_tr dfa.qs.(i).map} in
      let qs = Array.map mod_state keep in *)
  { dfa with qs = qs; q0 = qs.(rep_state.(dfa.q0.id)) } |> check_ids

let remove_unreachable x = log_f "Remove Unreachable" remove_unreachable x

let quotient rep {qs=qs; q0=q0; dop=dop} =
  let n = Array.length qs in
  (* what elements need to be kept as representatives *)
  let range = Enum.fold (fun a i -> ISet.add (rep i) a) ISet.empty (0--^n) 
	      |> ISet.elements |> Array.of_list in
  let rep_state = Array.make n (-1) in
  (* range states get mapped to their position *)
  Array.iteri (fun i r -> rep_state.(r) <- i) range;
  (* redundant states get mapped to their rep's state *)
  Enum.iter (fun i -> rep_state.(i) <- rep_state.(rep i)) (0--^n);
  let mod_tr tr = IMap.map (fun i -> rep_state.(i)) tr in
  let mod_state pos i = {qs.(i) with map = mod_tr qs.(i).map; id = pos} in
  let qs = Array.mapi mod_state range in
  {dop=dop; qs = qs; q0 = qs.(rep_state.(q0.id)) } |> check_ids

(* let quotient x = log_f "Quotient" quotient x *)

let print_matrix m2 =
  printf "     ";
  Array.iteri (fun i _ -> printf "%3d " i) m2; 
  printf "\n";
  Array.iteri (fun i r -> 
    printf "%3d) " i; 
    Array.iter (fun v -> printf "%3d " v) r;
    printf "\n";
  ) m2

let print_bmatrix n m =
  Enum.iter (fun i -> 
    printf "#%2d) " i; 
    Enum.iter (fun j -> print_string (if BitSet.is_set m (i*n+j) then "1" else "0")) (0--^n);
    printf "\n";
  ) (0--^n)

let commonality ?(eq = (=)) q1 q2 =
  let same_dec lo hi d1 d2 acc = 
    match d1, d2 with 
	Some q1, Some q2 when eq q1 q2 -> acc+(hi-lo+1) 
      | _ -> acc 
  in
  IMap.fold2_range same_dec q1.map q2.map 0

let difference ?(eq = (=)) q1 q2 =
  let diff_dec lo hi d1 d2 acc = 
    match d1,d2 with 
	Some q1, Some q2 when eq q1 q2 -> acc 
      | None, None -> assert false
      | _ -> acc + (hi-lo+1) 
  in
  IMap.fold2_range diff_dec q1.map q2.map 0

let is_same ?(eq = (=)) q1 q2 =
  let diff_dec _lo _hi d1 d2 = 
    match d1,d2 with 
	Some q1, Some q2 when eq q1 q2 -> true
      | None, None -> assert false
      | _ -> false
  in
  IMap.forall2_range diff_dec q1.map q2.map

(* let pass = Value.observe_int_ref "dist pass" (ref 0) *)

let dist_test ~dec_comp dfa =
  let n = Array.length dfa.qs in

  (* POSSIBLE OPTIMIZATION FOR DEPENDENCIES
     let edge_in = Array.make n [] in
     Array.iteri (fun i qi -> IMap.iter_range 
     (fun _ _ j -> 
     edge_in.(j) <- i :: edge_in.(j)
     ) qi.map) dfa.qs;
  *)
  
  (* SOME CODE IN THIS FUNCTION FROM fjavac project *)
  let m = BitSet.create_full (n*n) in
  let pos i j = i * n + j in
  let eq i j = BitSet.is_set m (pos i j) in
  let set_not_eq i j = BitSet.unset m (pos i j); BitSet.unset m (pos j i) in

  for i = 0 to n-1 do
    for j = i+1 to n-1 do
      (* states with different finalness are unequal *)
      if not (dec_comp dfa.qs.(i).dec dfa.qs.(j).dec) then set_not_eq i j
    done
  done;
  (*  print_bmatrix n m;   *)
  let more = ref true in
  while !more do
    more := false;
    for i = 0 to n-1 do
      for j = i+1 to n-1 do(* distinct pairs (i,j) *)
        if eq i j then     (* only check if some equal states aren't *)
	  (* if on some a, these two map to non-same states, then they're
	     distinguishable *)
	  let same = is_same ~eq dfa.qs.(i) dfa.qs.(j) in
	  (*	  printf "D(%d,%d) = %d " i j diff;   *)
	  if not same then ( set_not_eq i j; more := true; )
      done;
    done;
  (*    print_bmatrix n m;  *)
  done;
  m

(* let dist_test x = log_f "Dist Test" dist_test x *)

let print_diffs rep dfa = 
  let print_d i q = 
    let j = rep i in
    if j <> i then begin
      printf "#Node %d replaced by %d:\n" i j;
      printf "#%d: " i; Minreg.print stdout q.label; print_newline();
      print_tmap stdout q.map; print_newline();
      print_tmap stdout (IMap.map rep q.map); print_newline();

      printf "#%d: " j; Minreg.print stdout dfa.qs.(j).label; print_newline();
      print_tmap stdout dfa.qs.(j).map; print_newline();
      print_tmap stdout (IMap.map rep dfa.qs.(j).map); print_newline();
      print_newline(); 
    end
  in
  Array.iteri print_d dfa.qs
    
let minimize ?(dec_comp=Pervasives.(=)) dfa =
  let dfa = remove_unreachable dfa in
  let m = dist_test ~dec_comp dfa in (* returns a matrix of state equivalences *)
  (* first state equal to each state *)
  let n = Array.length dfa.qs in
  let rep i = Enum.find (fun j -> BitSet.is_set m (i*n+j)) (0--^n) in
  (*  print_diffs rep dfa; *)
  quotient rep dfa

(*let minimize dfa = 
  let ret = minimize dfa in
  eprintf "Minimized-dfa: %a" (summarize_dfa ~id:"dfa") ret;
  ret
*)

(* runs the dfa on the entire enum - produces the trace of all states
   gone through *)
let run_dfa_trace dfa enum =
  let next_q q c = 
    match q with None -> None | Some q -> 
      try Some(IMap.find (Char.code c) dfa.qs.(q).map) with Not_found -> None 
  in
  scanl next_q (Some dfa.q0.id) enum

let get_dec dfa qid = Option.map (fun q -> dfa.qs.(q).dec) qid
(* runs a dfa *)

let run_dfa_dec dfa e = run_dfa_trace dfa e |> Enum.reduce (fun _ b -> b) |> get_dec dfa

let run_dfa_stream dfa enum = 
  let rec next_q qi =
    match dfa.qs.(qi).dec with
      | Some dec -> dec
      | None -> match Enum.get enum with
	  None -> failwith "End of stream reached without match"
	  | Some (_,c) -> 
	    try IMap.find (Char.code c) dfa.qs.(qi).map |> next_q
	    with Not_found -> 
	      failwith (sprintf "Character %c has no transition at state %d" c qi)
  in
  next_q dfa.q0.id 

let print_trace oc dfa trace =
  let last = Enum.reduce (fun _ y -> y) (Enum.clone trace) in
  let qopt_to_str = function None -> "X" | Some q -> string_of_int q in
  let qopt_to_dec_l = function None -> [] | Some q -> dfa.qs.(q).dec in
  fprintf oc "%a: %a\n" 
    (Enum.print (fun oc qo -> IO.nwrite oc (qopt_to_str qo))) trace
    (List.print Int.print) (qopt_to_dec_l last);
  ()

let to_array dfa = 
  let to_arr m = 
    Array.init 256 (fun i -> try IMap.find i m with Not_found -> -1) in
  map_qs (fun q -> {q with map = to_arr q.map}) dfa

let print_array_dfa print_dec oc dfa =
  let aprint ~first ~last oc a = 
    String.print oc first;
    let last_c = ref a.(0) in
    let last_pos = ref 0 in
    for i = 1 to Array.length a - 1 do
      if a.(i) <> !last_c then begin
	if !last_c <> -1 then 
	  if !last_pos = i-1 then (
	    fprintf oc "%C:%d " (Char.chr !last_pos) !last_c;
	  ) else (
	    fprintf oc "%C-%C:%d " (Char.chr !last_pos) (Char.chr (i-1)) !last_c;
	  );
	last_c := a.(i);
	last_pos := i;
      end
    done;
    let i = Array.length a in
    if !last_c <> -1 then 
      if !last_pos = i-1 then (
	fprintf oc "%C:%d " (Char.chr !last_pos) !last_c;
      ) else (
	fprintf oc "%C-%C:%d " (Char.chr !last_pos) (Char.chr (i-1)) !last_c;
      );
    String.print oc last;
  in
  let print_q oc q = 
    match q.dec with
      | None -> aprint ~first:"[" ~last:"]" oc q.map 
      | Some d -> aprint ~first:"<" ~last:">" oc q.map; print_dec oc d;
  in
  print_fa print_q oc dfa


module RS = Ruleset
open RS.Rule

let table_of_q q = RS.table_of_map q.map

let tcam_of_rs rs = Tcam.of_fw_ruleset [bits_per_char] rs
let opt_table_of_q q = Optimizers.raz_dec q.map |> tcam_of_rs |> Optimizers.bitweave
let opt_itable_of_q q = table_of_q q |> (fun rs -> rs.RS.rs) |> tcam_of_rs |> Optimizers.bitweave

let boosted_table_of_q ?(boost_stride=7) q = 
  let m = q.map and id = q.id in
  if IMap.is_empty m then failwith "Cannot boost empty table" else
    let add_transition lo hi q acc = if q <> id then Vect.append {pred=[lo,hi];dec=q} acc else acc in
    let range = 0, IMap.domain m |> ISet.max_elt in
    let one_step_map = IMap.fold_range add_transition m Vect.empty in
    let default = {pred = Enum.repeat ~times:boost_stride range |> List.of_enum;
		   dec = id} in
    let prepend_star_fields rv _ = Vect.map (fun x -> {x with pred=range::x.pred}) rv in
    Enum.repeat ~times:boost_stride one_step_map 
	      |> Enum.scan prepend_star_fields
	      |> Enum.reduce RS.join |> Vect.append default

let tcam_size_q q = opt_itable_of_q q |> Vect.length

let dfa_tcam_size dfa = 
  Array.enum dfa.qs |> map tcam_size_q |> Enum.sum

(*
let to_rs_dfa dfa = map_qs opt_itable_of_q dfa
 *)

let tcam_size dfa =
  let get_map x = x.map in
  Array.enum dfa.qs |> map (get_map |- Optimizers.raz_dec |- Vect.length) |> Enum.reduce (+)

