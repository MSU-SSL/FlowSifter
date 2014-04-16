(** This module parses much of Perl-Compatable Regular Expressions 
  *  
  * It is part of the NetSifter project
  * 
  *)
  
open Batteries

type char_token = [ 
                  | `Char of int
		  | `Escape of char list
		  ]

type stream_token = 
  [ char_token
  | `Class of string
  | `Substart
  | `Substop
  | `Caret (* ^ *)
  | `Any (* . *)
  | `Alternate (* | *)
  | `Repeat of int * int option (* {3,} {2,4} {8} *)
  ]

type class_token = 
    [ char_token (* `Char of int, `Escape of char list *)
    | `Range of int * int
    | `Class of string
    ]

(** RegEx type *)
type ('a,'b) t =
  | Union of ('a,'b) t list
  | Concat of ('a,'b) t list
  | Kleene of ('a,'b) t
  | Value of 'a
  | Accept of 'b * int

let epsilon = Concat []

let catch_escape = function
  | '\\'::'x'::h1::h2::t -> 
      Some (`Char (int_of_string (Printf.sprintf "0x%c%c" h1 h2)), t)
  | '\\'::'x'::'{'::t -> 
      let rec loop acc = function
	  '}' :: t -> 
	    Some (`Char (int_of_string (String.implode (List.rev acc))), t)
	| c :: t -> loop (c::acc) t
	| [] -> failwith "End reached looking for }"
      in
      loop ['x';'0'] t
  | '\\'::'c'::x::t -> 
      let to_control_char c = 
	let upper_code = Char.uppercase c |> Char.code in
	(upper_code lxor 0x60) in
      Some (`Char (to_control_char x), t)
  | '\\'::'n'::t -> Some (`Char (Char.code '\n'),t)
  | '\\'::'r'::t -> Some (`Char (Char.code '\r'),t)
  | '\\'::'t'::t -> Some (`Char (Char.code '\t'),t)
  | '\\'::'a'::t -> Some (`Char 0x07,t) (* bell *)
  | '\\'::'e'::t -> Some (`Char 0x1b,t) (* escape *)
  | '\\'::'f'::t -> Some (`Char 0x0c,t) (* form feed *)
  | '\\'::'v'::t -> Some (`Char 0x0b,t) (* vertical tab *)
  | '\\'::x::t -> Some (`Escape [x], t)
  | c::t -> Some (`Char (Char.code c), t)
  | [] -> None
  
let catch_class char_list = 
  let acc = Buffer.create 15 in
  let rec aux count = function
    | '['::':'::'a'::'l'::'n'::'u'::'m'::':'::']'::b -> 
	Buffer.add_string acc "a-zA-Z0-9"; aux count b
    | '['::':'::'s'::'p'::'a'::'c'::'e'::':'::']'::b -> 
	Buffer.add_string acc " \\t\\r\\n\\v\\f"; aux count b
(* TODO: COMPLETE LIST OF POSIX BRACKET EXPRESSIONS FROM HERE:
   http://www.regular-expressions.info/posixbrackets.html *)
    | '['::b ->  (Buffer.add_char acc '[') ; aux (count+1) b 
(*    | ']'::b when Buffer.length acc = 0 -> Buffer.add_char acc ']'; aux count b *)
    | ']'::b when count == 0 -> `Class (Buffer.contents acc), b
    | ']'::b when count != 0 ->  (Buffer.add_char acc ']') ; aux (count-1) b 
    | a::b -> (Buffer.add_char acc a) ; aux count b
    | [] -> `Class (Buffer.contents acc), []
  in  
    aux 0 char_list 

let rec get_int acc = function
    ('0'..'9' as d) :: t -> get_int (10*acc + (Char.code d - 0x30)) t
  | t -> acc, t

let catch_range chars =
  let (v0,chars) = get_int 0 chars in
  match chars with
      ',' :: chars -> 
	let (v1,chars) = get_int 0 chars in
	( match chars with 
	      '}'::chars -> `Repeat (v0,if v1 = 0 then None else Some v1), chars
	    | _ -> failwith "Expecting } for range"
	)
	  
    | '}':: chars -> `Repeat (v0,Some v0), chars
    | _ -> failwith "Expecting } or , for range"

let stream_tokenize ~extended str = 
  let rec make_token = function 
    | [] -> None
    | '\\'::_ as l -> catch_escape l
    | '['::t -> Some ( catch_class t )
    | '(' :: '?' :: 'i' :: ':' :: t -> Some ( `Substart_i true, t )
    | '(' :: '?' :: '-' :: 'i' :: ':' :: t -> Some ( `Substart_i false, t )
    | '(' :: '?' :: ':' :: t 
    | '(' :: t  
	-> Some ( `Substart, t )
    | ')'::t -> Some ( `Substop, t )
    | '^'::t -> Some ( `Caret, t )
    | '.'::t -> Some ( `Any, t )
    | '|'::t -> Some ( `Alternate, t )
    | '?'::t -> Some ( `Repeat (0,Some 1), t )
    | '*'::t -> Some ( `Repeat (0,None), t )
    | '+'::t -> Some ( `Repeat (1,None), t )
    | '{'::t -> Some ( catch_range t )
    | c::t 
	when extended && ((c == '\n') || (c==' ') 
	  || (c=='\r') || (c=='\t')) -> make_token t
    | c::t   -> Some ( (`Char (Char.code c)), t )
  in
    Enum.unfold (String.explode str) make_token ;;
	    

let class_tokenize str = 
  let token_stream = Enum.unfold (String.explode str) catch_escape |> List.of_enum in
  let rec parse_class = function 
    | [] -> None
    | `Char a::`Char 45 (* - *)::`Char b::t -> Some (`Range (a,b), t)
    | `Char c::t   -> Some ( `Char c, t )
    | `Escape e::t -> Some (`Escape e,t)
  in
    match token_stream with
      | `Char 94 (* ^ *) :: t -> (false, Enum.unfold t parse_class)
      | b -> (true, Enum.unfold b parse_class) ;;

let iset_of_class any set_of_escape str = 
  let aux acc = function
    | `Char c -> ISet.add c acc
    | `Range (lo,hi) -> ISet.add_range lo hi acc
    | `Escape x -> ISet.union (set_of_escape x) acc
  in
  let (positive, tokens) = class_tokenize str in
  let listed = fold aux ISet.empty tokens in
  if positive then listed else ISet.diff any listed

let rev_compare x y = - (Pervasives.compare x y)

let rec union_elim acc = function
    [] -> List.sort_unique Pervasives.compare acc
  | Union x :: t -> union_elim acc (x@t)
  | h :: t -> union_elim (h::acc) t
let rec concat_elim acc = function
    [] -> List.rev acc
  | Concat x :: t -> concat_elim acc (x@t)
  | h :: t -> concat_elim (h::acc) t
let rec reduce = function
  | Union [x] -> reduce x
  | Union l -> Union (union_elim [] l |> List.map reduce)
  | Concat [] as e -> e
  | Concat [x] -> reduce x
  | Concat l -> Concat (concat_elim [] l |> List.map reduce)
  | Kleene x -> Kleene (reduce x)
  | Value _ as e -> e
  | Accept _ as e -> e 

let rec print_regex printa ~root oc = function 
  | x when x == root -> IO.nwrite oc "ROOT"
  | Union [Concat []; reg] -> print_regex printa ~root oc reg; IO.write oc '?'
  | Union regl -> List.print ~first:"(" ~sep:"|" ~last:")" (print_regex printa ~root) oc regl
  | Concat regl -> List.print ~first:"" ~sep:"" ~last:"" (print_regex printa ~root) oc regl
  | Kleene (Concat regl) -> List.print ~first:"(" ~sep:"" ~last:")" (print_regex printa ~root) oc regl;  IO.write oc '*'
  | Kleene reg -> print_regex printa ~root oc reg; IO.write oc '*'
  | Value a -> printa oc a
  | Accept (i,p) -> Printf.fprintf oc "{{%d:%d}}" p i

let print_char oc i =
  IO.nwrite oc (Char.escaped (Char.chr i))

let print_range oc lo hi = 
  if lo = hi-1 then begin
    print_char oc lo;
    print_char oc hi;
  end else if lo < hi then begin
    print_char oc lo;
    IO.nwrite oc "-";
    print_char oc hi;
  end else 
    print_char oc lo
(*  IO.nwrite oc "  "*)

let print_iset oc set = 
  if ISet.cardinal set > 1 then IO.write oc '[';
  ISet.iter_range (print_range oc) set;
  if ISet.cardinal set > 1 then IO.write oc ']'

let print_iregex oc = print_regex print_iset ~root:(Obj.magic 0) oc

(* Returns a regex that matches any character in the string *)
let iset_of_string str =
  let add_char acc c = ISet.add (Char.code c) acc in
  String.fold_left add_char ISet.empty str

(**  Takes a ascii str and returns a ISet.t  t
     assume that the regex is not anchored unless explicitly anchored *)
let ascii_escapes = function (* TODO: implement more \. escapes *)
      'n' -> iset_of_string "\n"
    | 'r' -> iset_of_string "\r"
    | 't' -> iset_of_string "\t"
    | 'a' -> ISet.singleton 0x07 (* bell *)
    | 'e' -> ISet.singleton 0x1b (* escape *)
    | 'f' -> ISet.singleton 0x0c (* form feed *)
    | 'v' -> ISet.singleton 0x0b (* vertical tab *)
    | 'd' -> iset_of_string "0123456789"
    | 's' -> iset_of_string " \t\r\n"
    | x -> iset_of_string (String.of_char x)

let ascii_any = ISet.add_range 0 255 ISet.empty

let regex_of_ascii_str ~dec str modifiers = 
  let ignore_case = ref (List.mem `Ignore_case modifiers) in
  let extended = List.mem `Extended modifiers in
  let pri = try List.find_map (function `Pri x -> Some x | _ -> None) modifiers 
            with Not_found -> 0 in
  (* TODO: add more options here *)
  let stream = stream_tokenize ~extended str in
  let value_of_escape = function (* TODO: Implement more escape types *)
      | [] -> failwith "End of string after escape"
      | [x] -> ascii_escapes x
      | _ -> failwith "Unknown escape sequence"
  in
  let regex_of_class str = Value (iset_of_class ascii_any value_of_escape str) in
  let dup_n rx n = Concat (List.make n rx) in
  let rec zero_thru_n rx n = if n > 0 then Union [epsilon; Concat [rx; zero_thru_n rx (n-1)]] else epsilon in
  let rec aux acc = 
    let mod_head f = match acc with [] -> failwith ("Modifying token found without anything to modify: " ^ str)
      | h :: t -> aux (f h :: t) in
    match Enum.get stream with
    | None -> Concat (List.rev acc)
    | Some (`Char a) ->
      let s = 
	if !ignore_case then 
	  let c = Char.chr a |> Char.lowercase in
	  let c_up = Char.uppercase c in
	  ISet.singleton (Char.code c) |> ISet.add (Char.code c_up)
	else
	  ISet.singleton a
      in
      aux ((Value s)::acc)
    | Some (`Escape a) -> 
	aux (Value (value_of_escape a)::acc)
    | Some (`Class a) -> 
	aux ((regex_of_class a)::acc) 
    | Some (`Substart_i new_ignore) -> 
      let outer = !ignore_case in
      ignore_case := new_ignore;
      let inner_rx = aux [] in
      ignore_case := outer;
      aux (inner_rx :: acc)
    | Some (`Substart) -> 
	aux ((aux [] )::acc) 
    | Some (`Substop) ->  Concat (List.rev acc) 
    | Some (`Caret) -> aux (Value (iset_of_string "^")::acc)
    | Some (`Any) -> aux ((Value ascii_any)::acc)
    | Some (`Alternate) -> (* This is tricky *)
	aux [Union [Concat (List.rev acc) ;aux [] ] ]
    | Some (`Repeat (m,None)) -> (* unbounded *)
	mod_head (fun g -> Concat [dup_n g m; Kleene g])
    | Some (`Repeat (0, Some n)) ->
	mod_head (fun g -> zero_thru_n g n)
    | Some (`Repeat (m, Some n)) ->
	mod_head (fun g -> Concat [dup_n g m; zero_thru_n g (n-m)])
  in 
  let rx = 
    match Enum.peek stream with
      | Some (`Caret) -> 
	  Enum.junk stream;
	  reduce (aux [])
      | _ -> reduce (Concat [Kleene (Value ascii_any); aux []])
  in
  reduce (Concat [rx; Accept (dec,pri)])
 ;;
    
let first_char rx = 
  let rxlen = String.length rx in
  if (rxlen < 2) || (rx.[0] <> '/') || (rx.[rxlen - 1] <> '/') then ISet.empty
  else
    match rx.[1] with
      | '\\' -> ascii_escapes rx.[2]
      | '(' -> assert false
      | '[' -> assert false
      | '^' -> assert false
      | '.' -> ascii_any
      | x -> ISet.singleton (Char.code x)
	
let match_char iset c = ISet.mem (Char.code c) iset

let regex_match match_val rx lst = 
  let rec loop = function 
  | Value _, [] -> None
  | Value v, c::t -> if match_val v c then Some t else None
  | Union [], _ -> None
  | Union (h::t), str -> (* does this backtrack properly? *)
      ( match loop (h,str) with
	  None -> loop (Union t, str)
	| Some t -> Some t )
  | Concat [], str -> Some str
  | Concat (h::t), str -> 
      ( match loop (h,str) with
	    None -> None
	  | Some str_t -> loop (Concat t, str_t) )
  | Kleene rx, str -> loop (Union [epsilon; Concat [rx; Kleene rx]],str)
  | Accept _, str -> Some str
  in
  loop (rx,lst)

let regex_match_iset rx str = 
  match regex_match match_char rx (String.explode str) with
      Some [] -> true
    | Some _ -> false (* partial match *)
    | None -> false

let join_regex e_rx = Union (List.of_enum e_rx)

let mod_if b f x = if b then f x else x

let rx_of_dec_strings ?(anchor=false) ?(allow_comments=false) ?(max_regs=max_int) rxs =
  rxs 
  |> mod_if allow_comments 
      (Enum.filter (fun (_d,r,_o) -> String.length r = 0 || r.[0] <> '#'))
  |> Enum.take max_regs
  |> mod_if anchor (Enum.map (fun (d,r,o) -> (d,"^" ^ r,o)))
  |> Enum.map (fun (dec, rx, modifiers) -> regex_of_ascii_str ~dec rx modifiers)
  |> join_regex
