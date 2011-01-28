open Batteries
open Printf
open Ean_std
open Ocamlviz

(* let ruleset_tag = Tag.create ~size:true ~count:true ~period:1000 "Ruleset" *)
module Rule = struct 
  type ('a, 'b) rul = { pred: 'a; dec: 'b }
      
  (** FW Rule : predicate (nrange) -> decision ('a) **)
  type 'a fw_rul = (nrange, 'a) rul

  let print_nrange oc nr =
    List.print ~first:"" ~last:"" ~sep:"," 
      (Pair.print Int.print Int.print) oc nr

  (** ditto with rule **)
  let print_rule print_pred print_dec chan r = 
    Printf.fprintf chan "%a,%a" print_pred r.pred print_dec r.dec
      
  let print = print_rule

  (** print rule to stdout **)
(*  let print_irule r = print_rule Int.print stdout r; print_newline() *)

    
end

open Rule


 (* a ruleset *)
type ('a,'b) t = { fns : string list; rng: 'a; rs: ('a,'b) rul Vect.t }

(* a ruleset using ranges *)
type 'a fw_rs = (nrange, 'a) t

(* import vect's get, length, dims etc. to operate on t *)
let get rs i = Vect.get rs.rs i
let length rs = Vect.length rs.rs
let dims rs = List.length rs.rng

let field_widths_nr nrange = 
  let bits_needed (_lo,hi) = 
    let rec loop v = if v = 1 then 0 else 1+loop(v lsr 1) in
    loop hi+1
  in
  List.map bits_needed nrange

let field_widths rs = field_widths_nr rs.rng

let set_fieldnames fns rs = {rs with fns = fns}
let nulldef d = { fns=[]; rng=[]; rs=Vect.make 1 {pred=[]; dec=d} }

let last_r rset = Vect.last rset.rs
let first_r rset = Vect.first rset.rs
let vect_without_last v = Vect.remove (Vect.length v - 1) 1 v

let without_final_default rset = 
  let len = Vect.length rset.rs in
  if len <= 1 then {rset with rs = Vect.empty}
  else {rset with rs = vect_without_last rset.rs}


let empty fns nrng = {fns = fns; rng = nrng; rs = Vect.empty}
let singleton_r fn r = {fns = [fn]; rng = r.pred; rs = Vect.singleton r}
let singleton fn rng r = {fns = [fn]; rng = rng; rs = r}

let append r rs = {rs with rs = Vect.append r rs.rs}
let is_empty rset = Vect.is_empty rset.rs

let map f rset = {rset with rs = Vect.map f rset.rs}
let map_dec f rset = map (fun r -> {pred=r.pred; dec=f r.dec}) rset
let map_pred f rset = {fns=rset.fns; rng = f rset.rng; 
       rs = Vect.map (fun r -> {pred=f r.pred; dec=r.dec}) rset.rs}

(* let partition f rset = let rs1,rs2 = Vect.partition f rset.rs in {rset with rs=rs1}, {rset with rs=rs2} *)

let exists p rset = Vect.exists p rset.rs
let fold f a rset = Vect.fold f a rset.rs

let filter p rset = {rset with rs = Vect.filter p rset.rs}

let enum t = Vect.enum t.rs

(* import stops here *)

(* build a ruleset from an IMap *)
let table_of_map m = 
  let empty_rs = empty ["IMap"] [0,1] in
  if IMap.is_empty m then empty_rs else
    let add_transition lo hi q acc = append {pred=[lo,hi];dec=q} acc in
    let range = 0, IMap.domain m |> ISet.max_elt in
    IMap.fold_range add_transition m (empty ["IMap"] [range])


let merge_range a b = 
  let rec loop = function
      [],[] -> []
    | (w,x)::at, (y,z)::bt when y = 1 + x -> (w,z)::(loop (at,bt))
    | _ -> assert false
  in
  loop (a,b)

(** Joins two ruleset by concatenation [with an extra rule at the end] **)
let join ?extra rs1 rs2 =
  match extra with None -> Vect.concat rs1 rs2
    | Some r -> Vect.append r (Vect.concat rs1 rs2)


(** Given parent ruleset vect and one function that maps decisions to
    tables, compose the n+1 dim table where children are n dim **)
let merge_w_children get_child par =
  let add_parent acc pr = 
    (* parent's rules will have one range in their field *)
    let parent_range = List.hd pr.pred in 
    let child = get_child pr.dec in
    let add_child acc cr =
      Vect.append {pred = parent_range :: cr.pred; dec = cr.dec} acc in
    Vect.fold add_child acc child
  in
  Vect.fold add_parent Vect.empty par

(** predicate on whether or not nr is a prefix rule  **)
let is_prefix nr = 
  let is_rng_prefix (lo,hi) = 
    (lo = lo land hi) && (hi = lo lor hi) &&
      (let d = hi - lo in (d land (d+1)) = 0) in
  List.for_all is_rng_prefix nr

(** given a list of rules count the number of prefix rules **)
let count_prefix t = 
  Vect.fold (fun a r -> if is_prefix r.pred then a+1 else a) 0 t.rs

let print_rv print_pred print_dec chan rv =
  Vect.print ~first:"" ~sep:"\n" ~last:"\n" (print_rule print_pred print_dec) 
    chan rv


(** print a ruleset **)
let print ?(full=false) print_pred print_dec chan rs = 
  if full then begin
    Printf.fprintf chan "! Comment here\n" ;
    List.print ~first:"" ~last:"\n" ~sep:"," String.print chan rs.fns;
    Printf.fprintf chan "%a\n" print_pred rs.rng;
  end;
  print_rv print_pred print_dec chan rs.rs

(** Idemponent but prints some stats on rs (dimensions and size) **)
let rs_stats rs = 
  eprintf "RS: %dD - %d rules\n%!" (dims rs) (length rs)

(** Check to see if the range and rules are properly formed since
    data type does not guarantee this **)
let is_consistent ?(d=0) {rng=range; rs=rules} =
  if d > 0 then assert (List.length range = d);
  let check_field_containment (lo1, hi1) (lo2,hi2) =
    if lo1 > lo2 || hi1 < hi2 then failwith "range containment failure"
  in
  let in_range {pred = rf} = List.iter2 check_field_containment range rf in
  try Vect.iter in_range rules 
  with Invalid_argument _ -> failwith "range length mismatch"

(** returns a ruleset that is parsed from chan **)
let read_rules chan = 
  let read_comma_delim_line ch = 
    let line = IO.read_line ch in String.nsplit line "," in
  let pair_int_of_string s = 
    try String.split s ":"|> Pair.map int_of_string 
    with Not_found -> 
      failwith (Printf.sprintf "':' not found in %s" s) in
  let _comment = IO.read_line chan in
  let names = read_comma_delim_line chan in
  let ranges = read_comma_delim_line chan |> List.map pair_int_of_string in
  let dim = List.length ranges in
  assert (List.length ranges = dim); 
  let read_rule c = 
    let parts = 
      try read_comma_delim_line c 
      with IO.No_more_input -> raise Done in 
    (*print_endline (dump parts);*)
    assert (List.length parts = dim + 1);
    let rec loop acc = function 
	[] -> raise Done (* didn't get any parts - empty line? *)
      | [d] -> {pred=List.rev acc; dec= int_of_string d}
      | h :: t -> loop (pair_int_of_string h :: acc) t
    in
    loop [] parts
  in
  let rules = ref Vect.empty in
  until_exn (fun () -> rules := Vect.append (read_rule chan) !rules);
  {fns = names; rng = ranges; rs = !rules}

(** wrapping read_rules with a loggin mechanism **)
let read_rules x = log_f "Reading" read_rules x

let wrap f rs = {fns=rs.fns; rng=rs.rng; rs=f rs.rs}

(* check if a ruleset is complete *)

let is_complete rs =
  (* quick check last rule for default-ness *)
  if (last_r rs).pred = rs.rng then true
  else assert false (* NEED TO IMPLEMENT FULL TEST *)

