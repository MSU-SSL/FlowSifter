open Batteries
open Printf

open Ean_std
module RS = Ruleset
open RS.Rule

module Entry = struct

  type bitset = int (* 63-bit set *)
  type field = {width: int; mask: bitset; bits: bitset}
  type t = field list

  let width fs = List.fold_left (fun acc f -> acc + f.width) 0 fs
  let default w = { width = w; mask = 0; bits = 0; }
  let widths fs = List.fold_left (fun acc f -> f.width :: acc) [] fs |> List.rev
  let is_stars fld = fld.mask = 0
  let match_fld fld x = fld.mask land x = fld.bits

  let normalize fld = {fld with bits = fld.bits land (lnot fld.mask)}

  let is_prefix_f f = 
    let rec loop x = 
      if x = 0 then true 
      else if x land 1 = 0 then loop (x asr 1) 
      else ((x+1) land x) = 0 in
    loop f.mask

  let get_fld i fld =
    let shift = fld.width - (i + 1) in
    if (fld.mask lsr shift) land 1 = 0 then Star
    else if (fld.bits lsr shift) land 1 = 1 then One else Zero

  let get a i = 
    let rec loop i = function
	[] -> Star
      | h :: _ when i < h.width -> get_fld i h
      | h :: t -> loop (i-h.width) t
    in
    loop i a
     
  let tbits ?(perm=identity) a = 
    0 --^ width a       |> map (fun i -> get a (perm i))

  let tbits_rev ?(perm=identity) a = 
    (width a - 1) --- 0 |> map (fun i -> get a (perm i))

  let is_prefix t =
    let not_star = function Star -> false | _ -> true
    and is_star  = function Star -> true | _ -> false in
    tbits t |> Enum.drop_while not_star |> Enum.drop_while is_star |> Enum.is_empty


  let of_enum ?(fields=[]) e =
    let get_field w = 
      let rec loop i (m,b) = 
	if i = w then {width=w; mask=m; bits=b}
	else match Enum.get e with 
	    None | Some Star -> loop (i+1) (m lsl 1,b lsl 1)
	  | Some One  -> loop (i+1) (m lsl 1 + 1, b lsl 1 + 1)
	  | Some Zero -> loop (i+1) (m lsl 1 + 1, b lsl 1)
      in
      loop 0 (0,0)
    in
    let rec aux acc = function 
	[] when Enum.count e >= 63 -> aux (get_field 63 :: acc) []
      | [] when Enum.is_empty e -> List.rev acc
      | [] (* 0 < count < 63 *) -> aux (get_field (Enum.count e) :: acc) []
      | h::t -> aux (get_field h :: acc) t
    in
    aux [] fields

  let permute ?fields perm a = 
    let fields = match fields with None -> widths a | Some fs -> fs in
    tbits ~perm:perm a |> of_enum ~fields

  (* given a prefix range, return its field *)
  let of_perfect_range w lo hi =
    let width_mask = (1 lsl w) - 1 in
    let mask = lnot(hi land (lnot lo)) land width_mask
    and bits = lo land width_mask in
    assert (hi = bits + (lnot mask) land width_mask);
    assert (lo = bits land mask);
    {width = w; mask = mask; bits = bits}

  (* returns the number of trailing 1s in the binary representation of x *)
  let right_ones x =
    let rec loop c x = if x land 1 = 0 then c else loop (c+1) (x lsr 1) in
    loop 0 x

  let right_zeros x = (* by reduction to right_ones *)
    let x = (x lxor (x-1)) lsr 1 in 
    let rec loop c x = if x = 0 then c else loop (c+1) (x lsr 1) in
    loop 0 x

(*
  make a field out of a range
  Examples:
   of_range [2,4] = [001*; 0100]
   of_range [1,7] = [0001; 001*; 01**]
*)
  let rec of_range w (lo, hi) = 
(*    let w = Option.default (bits_v hi) w in *)
(*    printf "of_range: %d - %d\n" lo hi; *)
    if lo < 0 then failwith "Entry.of_range: Negative lo";
    if hi lsr w > 0 then failwith (sprintf "Entry.of_range: Out of range hi: %d w: %d" hi w);
    if lo > hi then [] else
      let lo_max = lo + (1 lsl (right_zeros lo) - 1) in
      if lo_max = hi then 
	[of_perfect_range w lo lo_max]
      else if lo_max < hi then
	of_perfect_range w lo lo_max :: of_range w (lo_max+1, hi)
      else 
	let hi_min = if hi = max_int then 0 else 
	  hi - (1 lsl (right_ones hi) - 1) in
	if hi_min = lo then
	  [of_perfect_range w hi_min hi]
	else 
	  of_perfect_range w hi_min hi :: of_range w (lo, hi_min-1)

  let to_range f =
    assert (is_prefix_f f);
    let lo = f.bits land f.mask in
    (lo, lo lor (lnot f.mask) land (1 lsl f.width - 1))
      
  let field_to_string {width=w; mask=m; bits=b} =
    let out = String.create w in
    let rec loop pos m b =
      if pos < 0 then () else begin
	out.[pos] <- 
	  if m land 1 = 0 then '*'
	  else if b land 1 = 0 then '0'
	  else '1';
	loop (pos-1) (m lsr 1) (b lsr 1)
      end
    in
    loop (w-1) m b;
    out

  let field_of_string s =
    let w = String.length s in    
    let m = ref 0 and b = ref 0 in
    for i = w-1 downto 0 do
      ( match s.[i] with
	    '*' -> ()
	  | '1' -> incr m; incr b
	  | '0' -> incr m
	  | _ -> invalid_arg "Entry.of_string: Found unknown character"
      );
      m := !m lsl 1;
      b := !b lsl 1;
    done

  let print oc fa =
(*    Array.print (fun oc {mask=m;bits=b} -> fprintf oc "w%d m%a b%a" (width fa) Int.xprint m Int.xprint b) oc fa; *)
    let print_f oc f = IO.nwrite oc (field_to_string f) in
    List.print ~first:"" ~sep:" " ~last:"" print_f oc fa

  let print_tbit oc tb = 
    IO.write oc (match tb with Star -> '*' | One -> '1' | Zero -> '0')

  let print_perm perm oc a =
(*    Array.print (fun oc {mask=m;bits=b} -> fprintf oc "m%a b%a" Int.xprint m Int.xprint b) oc a; *)
    tbits ~perm a |> iter (print_tbit oc)

  let of_nrange ws nr = 
    let rec loop = function
	_,[] -> []
      | [], _ -> failwith "Insufficient field widths"
      | w::t,a::u -> of_range w a :: loop (t,u)
    in
    let field_exps = loop (ws,nr) in
    List.n_cartesian_product field_exps

  let of_nrange x y = try of_nrange x y with Failure _ -> 
    eprintf "Failure converting range: %a (widths: %a)\n%!" (List.print (Pair.print Int.print Int.print)) y (List.print Int.print) x;
    failwith "Exiting"

  let of_rule ws r = 
    of_nrange ws r.pred |> List.map (fun p -> {pred = p; dec = r.dec})

  let to_ranges p = List.map to_range p

  (* Adds an entry to an IMap using the given permutation.  
     Post-permutation, this entry *MUST* be prefix form *)
  let add_to_map ?perm r map =
    assert (width r.pred < Sys.word_size);
    let e = tbits ?perm r.pred in
    let acc_tbit (lo,hi) = function 
	Star -> (lo lsl 1, hi lsl 1 + 1)
      | One -> (lo lsl 1 + 1, hi lsl 1 + 1)
      | Zero -> (lo lsl 1, hi lsl 1)
    in
    let (min,max) = fold acc_tbit (0,0) e in
    IMap.add_range min max r.dec map


    (* Fields are compatible if one's mask is a subset of the other *)
  let compatible_f f1 f2 = 
    let comm = f1.mask land f2.mask in
    comm = f1.mask || comm = f2.mask

  (* Entries are compatible if their corresponding fields are
     compatible *IN THE SAME DIRECTION* *)
  let compatible a1 a2 =
    let rec loop dir = function
      | [], [] -> true
      | x::xs, y::ys when x.mask = y.mask -> loop dir (xs,ys)
      | x::xs, y::ys -> 
	  let comm = x.mask land y.mask in
	  if x.mask = comm then (* x has more stars *)
	    if dir = -1 then false else loop 1 (xs,ys)
	  else if y.mask = comm then (* y has more stars *)
	    if dir = 1 then false else loop (-1) (xs,ys)
	  else false
      | [], _ -> dir = -1 (* only if x has more stars *)
      | _,[] -> dir = 1 (* only if y has more stars *)
    in
    loop 0 (a1,a2)
      
  let mergable_f e1 e2 = assert (e1.mask == e2.mask); 
    let diff = e1.bits lxor e2.bits in
    diff land (diff-1) = 0 (* one bit different *)

  let mergable_f_points e1 e2 = assert (e1.mask == e2.mask); 
    let diff = e1.bits lxor e2.bits in
    if diff = 0 then 0 
    else if diff land (diff-1) = 0 then 1 
    else 2

  let merge_f e1 e2 =
    let diff = e1.bits lxor e2.bits in
    if (diff land (diff-1) <> 0) then (
      printf "Tried to merge %s with %s\n" (field_to_string e1) (field_to_string e2);
      assert false
    );
    let ndiff = lnot diff in
    {e1 with mask = e1.mask land ndiff; bits = e1.bits land ndiff}

  let mergable r1 r2 = 
    r1.dec = r2.dec 
    && List.length r1.pred = List.length r2.pred 
    && List.for_all2 (fun a b -> a.mask = b.mask) r1.pred r2.pred
    && 
    let mfp = List.map2 mergable_f_points r1.pred r2.pred in
(*    printf "%a and %a have merge-points: %a\n" print r1.pred print r2.pred (Array.print Int.print) mfp; *)
    List.reduce (+) mfp = 1
(*((0--^Array.length r1.pred) |> fold (fun i acc -> acc + mergable_f_points r1.pred.(i) r2.pred.(i)) 0) = 1 *)

  let merge r1 r2 = 
    assert (r1.dec=r2.dec);
    {r1 with pred = List.map2 merge_f r1.pred r2.pred}

  let same_bitmask_dec (a1,d1) (a2,d2) = 
    d1 = d2 && List.length a1 = List.length a2 
    && List.for_all2 (fun x y -> x.mask = y.mask) a1 a2

  let prefix_len_f f =
    let width_mask f = (1 lsl f.width) - 1 in
    let tail f = (width_mask f) lxor f.mask in
    let tail_bits f = bits_v (tail f) in
    f.width - tail_bits f

  let prefix_len a1 = 
    let rec loop total = function
	[] -> total
      | f::fs when prefix_len_f f = f.width -> loop (total + f.width) fs
      | f::_ -> total + prefix_len_f f
    in
    loop 0 a1

  let first_star f = (* returns the highest 1-bit set in f.mask *)
    let a = f.mask lor (f.mask asr 1) in
    let a = a lor (a asr 2) in
    let a = a lor (a asr 4) in
    let a = a lor (a asr 8) in
    let a = a lor (a asr 16) in
    let a = if Sys.word_size = 64 then a lor (a asr 32) else a in
    a land (lnot (a asr 1))

  let first_star_zero f = (* set the star to 0 *)
    let f1 = first_star f in
    {f with mask = f.mask - f1; bits = f.bits land (lnot f1)} 

  let first_star_one f = (* set the star to 1 *)
    let f1 = first_star f in
    {f with mask = f.mask - f1; bits = f.bits lor f1} 

  let rec enum_ranges_f f = 
    if is_prefix_f f then 
      Enum.singleton (to_range f)
    else 
      Enum.append 
	(enum_ranges_f (first_star_one f)) 
	(enum_ranges_f (first_star_zero f))
	
  (* let enum_ranges_f =  *)
  (*   tap (field_to_string |- print_string) *)
  (*   |- enum_ranges_f  *)
  (*   |- tap (Enum.clone |- Enum.print (Pair.print Int.print Int.print) stdout) *)

  let enum_ranges e =
    List.map (enum_ranges_f |- List.of_enum) e |> List.n_cartesian_product


end


type 'a tcam_rs = (Entry.t, 'a) rul Vect.t

let of_fw_ruleset = fun widths rv ->
  (*  eprintf "Rules: %d %a\n" (Ruleset.length rs) (List.print Int.print) widths; *)
  let append_list v l = List.fold_left (fun acc r -> Vect.append r acc) v l in
  let add_rule acc r = 
    let es = Entry.of_rule widths r in
        (*eprintf "Rule: %a becomes Entries: %a\n" (Ruleset.Rule.print range_print Int.print) r (List.print (RS.Rule.print Entry.print Int.print)) es; *)
    append_list acc es in
  Vect.fold add_rule Vect.empty rv
    
let to_fw_ruleset t = Ruleset.map_pred Entry.to_ranges t

let width t = Entry.width (Ruleset.first_r t).pred
let width_rv t = Entry.width (Vect.last t).pred

let decs t = Vect.fold (fun acc r -> PSet.add r.dec acc) PSet.empty t
