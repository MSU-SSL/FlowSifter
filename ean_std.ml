open Batteries_uni
open Printf

let (/!) a b = float a /. float b

let print_size_B oc x =
  if x < 1024 then
    fprintf oc "%d B" x
  else if x < 1024 * 1024 then
    fprintf oc "%.1f KiB" (float x /. 1024.)
  else if x < 1024 * 1024 * 1024 then
    fprintf oc "%.1f MiB" (float x /. 1024. /. 1024.)
  else if x < 1024 * 1024 * 1024 * 1024 then
    fprintf oc "%.1f GiB" (float x /. 1024. /. 1024. /. 1024.)

(** This is a list of ranges **)
type nrange = (int * int) list
let range_print oc r = List.print (Pair.print Int.print Int.print) oc r 

type tbit = Star | One | Zero
let tbit_print oc tb = IO.write oc (match tb with Star -> '*' | One -> '1' | Zero -> '0')

type 'a packet_hdr = {src: 'a; dst: 'a; sp: int; dp: int; proto: int}

(* returns the minimum number of bits to represent a value *)
let rec bits_v v = if v = 0 then 0 else 1 + bits_v (v lsr 1)

let bits_v2 v =
  let b = [|0x2; 0xC; 0xF0; 0xFF00; 0xFFFF0000; 0x7FFFFFFF00000000|] in
  let s = [|1; 2; 4; 8; 16; 32|] in
  let r = ref 0 in
  let v = ref v in
  for i = 5 downto 0 do
    if !v land b.(i) = 1 then 
      (let s = s.(i) in v := !v lsr s; r := !r lor s)
  done;
  !r;;

(* doesn't seem to work *)
let bits_v3 v =
  assert (max_int < 1 lsl 30 || v < 1 lsl 30); (* only works for <32-bit values *)
  assert (v > 0);
  let multiplyDeBruijnBitPosition = 

    [| 0; 9; 1; 10; 13; 21; 2; 29; 11; 14; 16; 18; 22; 25; 3; 30;
       8; 12; 20; 28; 15; 17; 24; 7; 19; 27; 23; 6; 26; 5; 4; 31 |]
  in
  let v = v lor (v asr 1) in
  let v = v lor (v asr 2) in
  let v = v lor (v asr 4) in
  let v = v lor (v asr 8) in
  let v = v lor (v asr 16) in
  multiplyDeBruijnBitPosition.((v * 0x022fdd63cc95386d) asr 58)

let print_any oc x = IO.nwrite oc (dump x)

let print_char oc i =
  IO.nwrite oc (Char.escaped (Char.chr i))

let print_range print_v oc lo hi v = 
  if lo < hi then begin
    print_char oc lo;
    IO.nwrite oc "-";
    print_char oc hi;
  end else 
    print_char oc lo;
  IO.nwrite oc "=>";
  print_v oc v;
  IO.nwrite oc "  "

let print_rng oc p = Pair.print print_char print_char oc p

let all_pairs lst =
  let rec loop acc = function 
      [] -> acc
    | h :: t ->
	loop (List.fold_left (fun acc tx -> (h,tx)::acc) acc t) t
  in
  loop [] lst

let all_pairs2 (xs,ys) =
  List.fold_left (fun acc x -> List.fold_left (fun acc y -> (x,y)::acc) acc ys) [] xs

let rec power n f x = if n < 1 then x else power (n-1) f (f x)

let number_enum e = Enum.combine (Enum.range 1, e)

(** type Id.t is new int; **)
module Id : 
sig 
  type 'a t
  val of_int : int -> 'a t
  val to_int : 'a t -> int 
end = struct 
  type 'a t = int let of_int i = i let to_int t = t
end
(*
let wrap f label = 
  let timer = Ocamlviz.Time.create label in
  let point = Ocamlviz.Point.create label in
  fun x -> 
    Ocamlviz.Point.observe point; Ocamlviz.Time.start timer;
    let r = f x in
    Ocamlviz.Time.stop timer;
    r

let wrap2 f label = 
  let timer = Ocamlviz.Time.create label in
  let point = Ocamlviz.Point.create label in
  fun x y -> 
    Ocamlviz.Point.observe point; Ocamlviz.Time.start timer;
    let r = f x y in
    Ocamlviz.Time.stop timer;
    r
*)
let wrap f _label = f
let wrap2 f _label = f

let hex_string_of_int oc i = fprintf oc "%X" i


(* type ('a, 'b) id_rul = ('a Id.t, 'b) rul *)

exception Done
(** Until f raises Done do f () **)
let until_exn f = try while true do f () done with Done -> ()
let until_eof f = try while true do f () done with End_of_file -> ()
(** fold over v with f breaking on Done **)
let fold_until_exn f v = 
  let rec loop v = loop (f v) in try loop v with Done -> ()

(** returns next power of 2 - 1 **)
let max2 n = 
  let rec log2 n = if n = 1 then 0 else 1 + log2 (n asr 1) in
  1 lsl (1 + log2 n) - 1

(** Conditional application (f x if b else x) **)
let iff b f x = if b then f x else x

(** Given e ('a) and a 'a list, remove the first e from the list **)
let rec list_remove e = function 
  | h :: t when h == e -> t 
  | h :: t -> h :: list_remove e t
  | [] -> raise Not_found 

(** Dynamic vect extension on assignment
    v.(i) <- x even for i > length v
    Postcondition: length v <= i **)
let vect_set_any v i x = 
  let len = Vect.length !v in
(*  eprintf "VSA: vl: %d i: %d %!" len i; *)
  if i < len then v := Vect.set !v i (Some x)
  else (* i > len *) 
    v := Vect.concat !v (Vect.make (i-len) None) |>  Vect.append (Some x);
(*  let len = Vect.length !v in
  eprintf " VSA: vl: %d \n" len; *)
  ()

(** Dynamic vect read 
    returns None if i > length v 
    postcondition length v' = length v **)
let vect_get_any v i = 
  let len = Vect.length !v in
  if i >= len then None else Vect.get !v i



(* general 'a <-> Id.t map generation *)
(** returns a non-pure function that is a counter **)
let make_counter start = 
  let x = ref (start-1) in 
  (fun () -> incr x; !x)

(** Generates an id_map from f, which is function to run first time
    each item is given an id **)

(** this is a return type of aweful goodness **)
type ('a, 'b) id_map = { get_id : 'a -> 'a Id.t; 
			 get_val : 'a Id.t -> 'a;
			 get_data : 'a Id.t -> 'b }

let map_id_param ?(min_id=1) ?(hash=Hashtbl.hash) ?(comp=Pervasives.compare) f = 
  let ht = Hashtbl_param.create hash comp 10_000
  and vect = ref Vect.empty 
  and counter = make_counter min_id in
  let rec get_id x = 
    try Id.of_int (Hashtbl_param.find ht x)
    with Not_found -> 
      let id = counter() in
      Hashtbl_param.add ht x id; (* add to the list of known values *)
      let b = f get_id x id in
      vect_set_any vect id (x,b);
      (*printf "GENERATING ID: %d\n" id;*)
      Id.of_int id (* return its id *)
  and get_vec id = let id = Id.to_int id in
    match vect_get_any vect id with 
	Some x -> x
      | None -> 
	  let err_msg = sprintf 
	    "Fdd.map_id(get_val): Index out of bounds: %d outside 0..%d" 
	    id ((Vect.length !vect)-1) in
	  invalid_arg err_msg; 
  in
  let get_val i = fst (get_vec i) and get_data i = snd (get_vec i) in
  {get_id = get_id; get_val = get_val; get_data = get_data} 

let map_id_set ?(min_id=1) ?(comp=Pervasives.compare) f = 
  let map = ref (PMap.create comp)
  and vect = ref Vect.empty 
  and counter = make_counter min_id in
  let rec get_id x = 
    try Id.of_int (PMap.find x !map)
    with Not_found -> 
      let id = counter() in
      map := PMap.add x id !map; (* add to the list of known values *)
      let b = f get_id x id in
      vect_set_any vect id (x,b);
      (* printf "GENERATING ID: %d\n%!" id; *)
      Id.of_int id (* return its id *)
  and get_vec id = let id = Id.to_int id in
    match vect_get_any vect id with 
	Some x -> x
      | None -> 
	  let err_msg = sprintf 
	    "Fdd.map_id_set(get_val): Index out of bounds: %d outside 0..%d" 
	    id ((Vect.length !vect)-1) in
	  invalid_arg err_msg; 
  in
  let get_val i = fst (get_vec i) and get_data i = snd (get_vec i) in
  {get_id = get_id; get_val = get_val; get_data = get_data} 


let map_int f = 
  let v = ref Vect.empty in
  let get_id n = 
    if vect_get_any v n = None then vect_set_any v n (f n);
    Id.of_int n
  and get_data id = 
    let n = Id.to_int id in
    match vect_get_any v n with
	None -> failwith "Not a valid ID"
      | Some x -> x
  in
  { get_id = get_id; get_val = Id.to_int; get_data = get_data }

let simple_map_int = {get_id = Id.of_int; get_val = Id.to_int; get_data = ignore}

(* *END* general 'a <-> Id.t map generation *)

(* Logging with times *)
(** Toggle logging on or off **)
let log_times = ref false

(** Wrap (f x) with a time logging function that has the label s **)
let log_f s f x = 
  if !log_times then begin
    let t1 = Sys.time () in
    eprintf "%s: Entering @ %.2f %!" s t1;
    let out = f x in
    let t2 = Sys.time () in
    eprintf "-- Leaving @ %.2f ( %.3f) \n%!" t2 (t2 -. t1);
    out
  end else f x

(*
open Ocamlviz

let log_f s f =  (* LOCKS UP THE RECEIVER *)
  let timer = Time.create s in
  fun x -> 
    Time.start timer;
    let out = f x in
    Time.stop timer;
    out
*)

(* Fischer-Krause ordered permutation generator *)
let rec firstup = function 
    [] -> raise Done
  | a :: (b :: _ as r) when a < b -> r
  | _ :: t -> firstup t
let rec firstless a = function [] -> []
  | (h::_) as l when h < a -> l 
  | _ :: t -> firstless a t
let rec genrev p q r s = 
  if p = r then s else 
    if p = q 
    then genrev (List.tl p) q r ((List.hd r)::s)
    else genrev (List.tl p) q r ((List.hd p)::s)
let nextperm p = 
  match firstup p with
      [] -> raise Done
    | (rh::rt) as r -> 
	match firstless rh p with
	    [] -> raise Done
	  | (qh::_qt) as q -> genrev p q r ((qh)::rt)


(* type Cost.t is new int with add and sub; *)
module Cost : 
sig 
  type t 
  val of_int : int -> t 
  val to_int : t -> int 
  val add : t -> t -> t 
  val sub : t -> t -> t
  val max_val : t
  val unit : t
  val free : t
end = struct 
  type t = int 
  let of_int i = i 
  let to_int t = t 
  let add = Int.Safe_int.(+) 
  let sub = Int.Safe_int.(-) 
  let max_val = max_int 
  let unit = 1
  let free = 0
end



(* print various statistics for an int enum *)
let print_statistics ?(oc=stdout) e =
  match Enum.get e with
      None -> fprintf oc "Empty enumeration - no statistics\n"
    | Some x0 ->
	let m = ref (float x0) 
	and k = ref 1 
	and s = ref 0. in
	let t = ref 0 in
	Enum.iter (fun x -> 
		     t := !t + x;
		     incr k; 
		     let x = float x in 
		     let mk = !m +. (x -. !m)/.(float !k) in 
		     s := !s +. (x -. !m) *. (x -. mk);
		     m := mk) e;
	let stdev = sqrt(!s /. float (!k-1)) in
	fprintf oc "N: %d Sum: %d Mean: %.1f Stdev: %.1f\n" !k !t !m stdev
