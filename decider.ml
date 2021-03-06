(** Abstraction of 1d classifier; decider takes a single value and returns the decision for that value; used as a building-block for FDD *)

open Batteries

type 'a t = 'a IMap.t

let empty eq = IMap.empty ~eq
let make_default ~eq min max v = IMap.add_range min max v (empty eq)
let default ~eq ?(min_key=min_int) ?(max_key=max_int) d =
  IMap.add_range min_key max_key d (empty eq)

let add min max c t = IMap.add_range min max c t

let iter f t = IMap.iter_range f t
let fold f a t = IMap.fold_range f a t
let enum t = IMap.enum t
let enum_cuts t = enum t |> map (fun (_n1,n2,_v) -> n2)
let min_domain t = enum t |> Enum.peek |> Option.get |> (fun (n1,_,_) -> n1)
let map f t = IMap.map f t

let lookup d i = IMap.find i d
let get_eq d = IMap.get_dec_eq d

let sub min max d = d |> IMap.from min |> IMap.until max

let of_enum ~eq e =
  let add_acc acc (lo,hi,c) = IMap.add_range lo hi c acc in
  Enum.fold add_acc (empty eq) e

let decs d =
  let accum _ _ c acc = Set.add c acc in
  fold accum d Set.empty

let compare_rng cmp (min, max, c1) (lo, hi, c2) =
  match Int.compare min lo with
      0 -> (match Int.compare max hi with
		0 -> cmp c1 c2
	      | x -> x)
    | x -> x

let compare dec_cmp d1 d2 =
  BatEnum.compare (compare_rng dec_cmp) (enum d1) (enum d2)

let print oc d =
  enum d |> Enum.print (fun oc (min,max,_) -> Printf.fprintf oc "(%d,%d,xx)" min max) oc

let print_all vp oc d =
  enum d |> Enum.print (fun oc (min,max,v) -> Printf.fprintf oc "(%d,%d,%a)" min max vp v) oc

let lookupi d i = Printf.printf "%a %d\n" print d i; IMap.find i d
let domain d = IMap.domain d
