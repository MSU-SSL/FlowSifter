open Batteries

type 'a t = 'a IMap.t

let empty = IMap.empty
let make_default min max v = IMap.add_range ~eq:(=) min max v empty
let default ?(min_key=min_int) ?(max_key=max_int) d = 
  IMap.add_range min_key max_key d empty
    
let add min max c t = IMap.add_range ~eq:(=) min max c t
  
let iter f t = IMap.iter_range f t
let fold f a t = IMap.fold_range f a t
let enum t = IMap.enum t
let enum_cuts t = enum t |> map (fun (_n1,n2,_v) -> n2) 
let min_domain t = enum t |> Enum.peek |> Option.get |> (fun (n1,_,_) -> n1)
let map f t = IMap.map f t
  
let lookup d i = IMap.find i d


let sub min max d = d |> IMap.from min |> IMap.until max

let of_enum e = 
  let add_acc acc (lo,hi,c) = IMap.add_range ~eq:(==) lo hi c acc in
  Batteries.fold add_acc IMap.empty e

let decs d = 
  let accum _ _ c acc = PSet.add c acc in
  fold accum d (PSet.create Pervasives.compare)
		   
let compare_rng (min, max, c1) (lo, hi, c2) = 
  match Pervasives.compare min lo with 
      0 -> (match Pervasives.compare max hi with
		0 -> Pervasives.compare c1 c2
	      | x -> x)
    | x -> x

let compare d1 d2 = BatEnum.compare compare_rng (enum d1) (enum d2)

let print oc d =
  enum d |> Enum.print (fun oc (min,max,_) -> Printf.fprintf oc "(%d,%d,xx)" min max) oc

let print_all vp oc d =
  enum d |> Enum.print (fun oc (min,max,v) -> Printf.fprintf oc "(%d,%d,%a)" min max vp v) oc

let lookupi d i = Printf.printf "%a %d\n" print d i; IMap.find i d
