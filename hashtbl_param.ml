(** Utility library to implement polymorphic hashtbl with
    runtime-determined hash and compare *)

  (* We do dynamic hashing, and resize the table and rehash the
     elements when buckets become too long. *)

module Enum = BatEnum

type ('a, 'b) t =
    { mutable size: int;                        (* number of elements *)
      mutable data: ('a, 'b) bucketlist array;  (* the buckets *)
      hash : 'a -> int;                         (* the hash function *)
      compare : 'a -> 'a -> int;                (* compare function *)
    }

and ('a, 'b) bucketlist =
    Empty
  | Cons of 'a * 'b * ('a, 'b) bucketlist

let create hash comp initial_size =
  let s = min (max 1 initial_size) Sys.max_array_length in
  { hash=hash; compare=comp; size = 0; data = Array.make s Empty }

let clear h =
  for i = 0 to Array.length h.data - 1 do
    h.data.(i) <- Empty
  done;
  h.size <- 0

let copy h = { h with data = Array.copy h.data }
  
let length h = h.size
  
let resize hashfun tbl =
  let odata = tbl.data in
  let osize = Array.length odata in
  let nsize = min (2 * osize + 1) Sys.max_array_length in
  if nsize <> osize then begin
    let ndata = Array.create nsize Empty in
    let rec insert_bucket = function
        Empty -> ()
      | Cons(key, data, rest) ->
          insert_bucket rest; (* preserve original order of elements *)
          let nidx = (hashfun key) mod nsize in
          ndata.(nidx) <- Cons(key, data, ndata.(nidx)) in
    for i = 0 to osize - 1 do
      insert_bucket odata.(i)
    done;
    tbl.data <- ndata;
  end

let add h key info =
  let i = (h.hash key) mod (Array.length h.data) in
  let bucket = Cons(key, info, h.data.(i)) in
  h.data.(i) <- bucket;
  h.size <- succ h.size;
  if h.size > Array.length h.data lsl 1 then resize h.hash h

let rec find_rec compare key = function
    Empty ->
      raise Not_found
  | Cons(k, d, rest) ->
      if compare key k = 0 then d else find_rec compare key rest

let find h key =
  match h.data.((h.hash key) mod (Array.length h.data)) with
      Empty -> raise Not_found
    | Cons(k1, d1, rest1) ->
	if h.compare key k1 = 0 then d1 else
	  match rest1 with
	      Empty -> raise Not_found
	    | Cons(k2, d2, rest2) ->
		if h.compare key k2 = 0 then d2 else
		  match rest2 with
		      Empty -> raise Not_found
		    | Cons(k3, d3, rest3) ->
			if h.compare key k3 = 0 then d3 
			else find_rec h.compare key rest3


let iter f h =
  let rec do_bucket = function
      Empty ->
        ()
    | Cons(k, d, rest) ->
        f k d; do_bucket rest in
  let d = h.data in
  for i = 0 to Array.length d - 1 do
    do_bucket d.(i)
  done

let fold f h init =
  let rec do_bucket b accu =
    match b with
	Empty ->
          accu
      | Cons(k, d, rest) ->
          do_bucket rest (f k d accu) in
  let d = h.data in
  let accu = ref init in
  for i = 0 to Array.length d - 1 do
    accu := do_bucket d.(i) !accu
  done;
  !accu

let map f h =
  let rec loop = function
    | Empty -> Empty
    | Cons (k,v,next) -> Cons (k,f k v,loop next)
  in
  { h with
      data = Array.map loop h.data; 
  }

let replace h key info =
  let rec replace_bucket = function
      Empty ->
        raise Not_found
    | Cons(k, i, next) ->
        if h.compare k key = 0
        then Cons(k, info, next)
        else Cons(k, i, replace_bucket next) in
  let i = (h.hash key) mod (Array.length h.data) in
  let l = h.data.(i) in
  try
    h.data.(i) <- replace_bucket l
  with Not_found ->
    h.data.(i) <- Cons(key, info, l);
    h.size <- succ h.size;
    if h.size > Array.length h.data lsl 1 then resize h.hash h

let choose h = (* runs outside the array if it's empty *)
  let rec aux buck = 
    match h.data.(buck) with
	Empty -> aux (buck+1)
      | Cons (k, i, _) -> (k,i)
  in
  aux 0

let enum h =
  let rec make ipos ibuck idata icount =
    let pos = ref ipos in
    let buck = ref ibuck in
    let hdata = ref idata in
    let hcount = ref icount in
    let force() =
      (** this is a hack in order to keep an O(1) enum constructor **)
      if !hcount = -1 then begin
	hcount := h.size;
	hdata := Array.copy h.data;
      end;
    in
    let rec next() =
      force();
      match !buck with
	| Empty ->
	    if !hcount = 0 then raise Enum.No_more_elements;
	    incr pos;
	    buck := Array.unsafe_get !hdata !pos;
	    next()
	| Cons (k,i,next_buck) ->
	    buck := next_buck;
	    decr hcount;
	    (k,i)
    in
    let count() =
      if !hcount = -1 then h.size else !hcount
    in
    let clone() =
      force();
      make !pos !buck !hdata !hcount
    in
    Enum.make ~next ~count ~clone
  in		
  make (-1) Empty (Obj.magic()) (-1)
