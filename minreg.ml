(* Minimizing regex library - identical to pcregex's regex type,
   except has a boolean of whether that subtree is reduced 

   The routines here attempt to simplify regex by replacing redundant
   constructs, such as x** -> x*, (x|x), etc.
*)

open Batteries
open Printf

(* BEGIN MIN_REGEX *)

type 'a t =
  | Value of ISet.t
  | Union of 'a t Set.t
  | Concat of 'a t list * bool
  | Kleene of 'a t
  | Accept of 'a * int

let epsilon = Concat ([],true)

let rec compare ~dec_comp x y = match (x,y) with
    Union a, Union b -> Enum.compare (compare ~dec_comp) (Set.enum a) (Set.enum b)
  | Accept (a,_), Accept (b,_) -> dec_comp a b
  | Concat (a,_), Concat (b,_) -> List.compare (compare ~dec_comp) a b
  | Kleene a, Kleene b -> compare ~dec_comp a b
  | a,b -> Pervasives.compare a b

let rec concat_elim acc = function
    [] -> List.rev acc
  | Concat (x,_) :: t -> concat_elim acc (x@t)
  | h :: t -> concat_elim (h::acc) t

let rec reduce = function
  | Union s when Set.cardinal s = 1 -> Set.choose s
  | Union _ as e -> e
  | Concat ([],_) -> Concat ([],true)
  | Concat ([x],_) -> reduce x
  | Concat (l,false) -> Concat (concat_elim [] l |> List.map reduce, true)
  | Concat (_,true) as e -> e
  | Kleene x -> Kleene (reduce x)
  | Value _ as e -> e
  | Accept _ as e -> e

let append t u = Concat (u::t, false)
let union s = Union (Set.map reduce s)
let union_unsafe s = Union s
let union2 a b = Union (Set.add (reduce a) (Set.singleton (reduce b))) |> reduce
let union_with s = function
    Union s1 -> Union (Set.union s s1)
  | e -> Union (Set.add (reduce e) s)
let union_sets s1 s2 = Union (Set.union s1 s2)
let concat l = Concat (l,false)
let concat_unsafe l = Concat (l,true)

let reduce_union t1 t2 =
  match reduce t1, reduce t2 with
    | Union a, Union b -> union_sets a b
    | Union a, b | b, Union a -> Union (Set.add b a)
    | a, b -> Union (Set.add a (Set.singleton b))

let roots = Hashtbl.create 10

let add_root rx str = Hashtbl.add roots rx str

let rec print oc = function
  | x when Hashtbl.mem roots x -> IO.nwrite oc (Hashtbl.find roots x)
  | Union s when Set.mem epsilon s -> print oc (Union (Set.remove epsilon s)); IO.write oc '?'
  | Union s -> Set.print ~first:"(" ~sep:"|" ~last:")" print oc s
  | Concat ([], _) -> ()
  | Concat (h::t,_) -> print oc h; print oc (Concat (t,true))
  | Kleene (Concat (regl,_)) -> List.print ~first:"(" ~sep:"" ~last:")" print oc regl;  IO.write oc '*'
  | Kleene reg -> print oc reg; IO.write oc '*'
  | Value a -> Pcregex.print_iset oc a
  | Accept (i,p) -> fprintf oc "{{%d:%d}}" p i

let rec printp ?(dec=true) oc = function
  | Union s when Set.mem epsilon s -> printp ~dec oc (Union (Set.remove epsilon s)); IO.write oc '?'
  | Union s -> Set.print ~first:"(" ~sep:"|" ~last:")" (printp ~dec) oc s
  | Concat ([], _) -> ()
  | Concat (h::t,_) -> printp ~dec oc h; printp ~dec oc (Concat (t,true))
  | Kleene (Concat (regl,_)) -> List.print ~first:"(" ~sep:"" ~last:")" (printp ~dec) oc regl;  IO.write oc '*'
  | Kleene reg -> printp ~dec oc reg; IO.write oc '*'
  | Value a -> Pcregex.print_iset oc a
  | Accept (x,p) -> fprintf oc "{{%d:%s}}" p (if dec then dump x else "x")

let rec printdp decp oc = function
  | Union s when Set.mem epsilon s -> printdp decp oc (Union (Set.remove epsilon s)); IO.write oc '?'
  | Union s -> Set.print ~first:"(" ~sep:"|" ~last:")" (printdp decp) oc s
  | Concat ([], _) -> ()
  | Concat (h::t,_) -> printdp decp oc h; printdp decp oc (Concat (t,true))
  | Kleene (Concat (regl,_)) -> List.print ~first:"(" ~sep:"" ~last:")" (printdp decp) oc regl;  IO.write oc '*'
  | Kleene reg -> printdp decp oc reg; IO.write oc '*'
  | Value a -> Pcregex.print_iset oc a
  | Accept (x,p) -> fprintf oc "{{%d:%a}}" p decp x


let print_inner_norm_regex oc rmap =
   IO.write oc '(';
   IMap.iter_range (Ean_std.print_range print oc) rmap;
   IO.write oc ')'

let print_norm_regex oc (_acc, rmap) = print_inner_norm_regex oc rmap

let print_norm_regexp oc (_, rmap) =
   IO.write oc '(';
   IMap.iter_range (Ean_std.print_range printp oc) rmap;
   IO.write oc ')'

(* let red_p = Point.create "MReduce" *)
(* let red_t = Time.create "MReduce" *)
(* let reduce x =  *)
(*   Point.observe red_p;  *)
(*   Time.start red_t;  *)
(* (\*  printf "#RXPRE:%a\n" print_mregex x;  *\) *)
(*   let r = reduce x in  *)
(*   Time.stop red_t;  *)
(* (\*  printf "#RXPST:%a\n" print_mregex x;  *\) *)
(*   r *)

let rec tag_red = function
  | Pcregex.Union l -> Union (List.fold_left (fun acc e -> Set.add (tag_red e) acc) Set.empty l)
  | Pcregex.Concat l -> Concat (List.map tag_red l,true)
  | Pcregex.Kleene r -> Kleene (tag_red r)
  | Pcregex.Value a -> Value a
  | Pcregex.Accept (i,p) -> Accept (i,p)

let of_reg reg = Pcregex.reduce reg |> tag_red

let hash x = Hashtbl.hash (IO.to_string print x)

let rec depth = function
  | Value _ -> 1
  | Accept _ -> 1
  | Kleene x -> 1 + depth x
  | Union s -> 1 + (Set.enum s |> map depth |> Enum.reduce max)
  | Concat (l,_) -> 1 + (List.enum l |> map depth |> Enum.reduce max)

let rec width = function
  | Value _ -> 1
  | Accept _ -> 1
  | Kleene x -> max 1 (width x)
  | Union s -> max (Set.cardinal s) (Set.enum s |> map width |> Enum.reduce max)
  | Concat ([],_) -> 0
  | Concat (l,_) -> max (List.length l) (List.enum l |> map width |> Enum.reduce max)
