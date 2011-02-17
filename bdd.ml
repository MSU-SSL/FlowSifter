open Batteries_uni
open Ean_std
open Printf

open Ruleset.Rule
module E = Tcam.Entry

type 'dec t = Leaf of 'dec | Node of 'dec t * 'dec t

let empty d = Leaf d

let print print_dec oc bdd =
  let rec walk path = function
      Leaf d -> 
	List.print Int.print ~first:"" ~sep:"" ~last:"" oc (List.rev path); 
	Printf.fprintf oc ",%a\n" print_dec d;
    | Node (l, r) -> 
	walk (0::path) l;
	walk (1::path) r
  in
  walk [] bdd
      
(* overwrites what's in tree, non-tail-recursive *)
(* Given a tbit enum and decision, update the bdd to return that decision for the path in the enum *)
let add_entry bdd_in (tbits, dec) = 
(*  printf "%a, dec %d\n" (Enum.print ~first:"" ~last:"" ~sep:"" tbit_print) (Enum.clone tbits) dec;*)
  let rec loop bdd =
    match Enum.get tbits, bdd with
      | _, (Leaf Some v as lf) when v = dec -> lf
      | Some One, Node (l,r) -> Node (l,loop r)
      | Some One, (Leaf _ as x) -> Node (x, loop x)
      | Some Zero, Node (l,r) -> Node (loop l, r)
      | Some Zero, (Leaf _ as x) -> Node (loop x, x)
      | Some Star, _ -> Leaf (Some dec) (* Assumes prefix entries *)
      | None, Leaf _ -> Leaf (Some dec)
      | None, Node _ -> failwith "partial entry in longer fdd"
  in
  let bdd_out = loop bdd_in in
(*  print Int.print stdout bdd_out; *)
  bdd_out

let of_tcam ?perm tcam = 
  Vect.backwards tcam 
  |> map (fun r -> E.tbits ?perm r.pred, r.dec) 
  |> fold add_entry (Leaf None)

let of_imap bits imap =
  (* let dom = IMap.domain imap in  *)
  (* let min,max = ISet.min_elt dom, ISet.max_elt dom in *)
  (* assert (min >= 0); assert (max < 1 lsl bits); *)
  let rec make_node min max =
    match (IMap.from min (IMap.until max imap)) |> IMap.enum |> Enum.get with
	None -> Leaf None
      | Some (lo,hi,_) when lo=min && hi = max -> 
	  Leaf (Some (IMap.find min imap))
      | _ -> 
	  let med = (min/2 + max/2) in
	  Node (make_node min med, make_node (med+1) max)
  in
  make_node 0 (1 lsl bits - 1)
  
let is_incomplete bdd =
  let rec loop = function
      Leaf None -> true
    | Leaf (Some _) -> false
    | Node (l,r) -> loop l || loop r
  in
  loop bdd

let rec map f = function
    Leaf x -> Leaf (f x)
  | Node (l,r) -> Node (map f l, map f r)
