open Batteries
open Printf

let buf_size = 100;;

Gc.set { (Gc.get()) with Gc.minor_heap_size = 1_000_000; } ;;

module type Parser = sig 
  type t
  val id : string
  val new_parser : unit -> t
  val delete_parser : t -> unit
  val add_data : t -> string -> unit
  val get_event_count : unit -> int
end

module Null : Parser = struct
  type t = unit
  let id = "null"
  let new_parser () = ()
  let delete_parser () = ()
  let add_data () _ = ()
  let get_event_count () = 0
end

let trace_len = ref (-1.)

module Flow : Parser = struct 
  type t = string -> unit
  let id = "sifter"
  let extr_ca = Sys.argv.(2)
  include Prog_parse
  let new_parser = new_parser "spec.ca" extr_ca
    
  let () = at_exit (fun () -> 
(*    Printf.printf "#Bytes skip()ed: %d  Times this exceeded the current packet: %d bytes asked to skip trans-packet: %d\n" !Ns_parse.inner_skip !Ns_parse.skip_over !Prog_parse.total_skip; 
    Printf.printf "#Packets skipped entirely: %d Packets skipped partially: %d\n" !Prog_parse.pkt_skip !Prog_parse.pkt_partial_skip;
    Printf.printf "#Skip missed: %d\n" !Prog_parse.skip_missed; *)
    let trace_len = !trace_len /. 8. in
    let parsed = float !Ns_parse.parsed_bytes /. float (1024 * 1024) in
    Printf.printf "#Trace payload length: %.1f MB Bytes parsed: %.1f Percentage of flow parsed: %2.1f\n" trace_len parsed (100. *. parsed /. trace_len)
  )
end

module Pac : Parser = struct
  include Anypac
  let id = "pac"
end

let rep_cnt = int_of_string Sys.argv.(1) 
let pre_assemble = rep_cnt < 0
let rep_cnt = abs rep_cnt
(* argv.2 is only used in flowsifter *)
let fns = Sys.argv |> Array.to_list |> List.drop 3 

(* let is_http ((_sip, _dip, sp, dp),_,_) = sp = 80 || dp = 80 *)

let max_flows = ref 0
let conc_flows = ref 0
let flow_count = ref 0

let read_file_as_str fn =
  let ic = Pervasives.open_in_bin fn in
  let len = (Pervasives.in_channel_length ic) in
  printf "#Reading %s (len %d)...%!" fn len;
  let old_gc = Gc.get() in
  Gc.set {old_gc with Gc.space_overhead = 0};
  let ret = String.create len in
  Gc.set old_gc;
  Pervasives.really_input ic ret 0 len;

  Pervasives.close_in ic;
  printf "done\n%!";
  ret

let list_avg l = List.reduce (+.) l /. (List.length l |> float)

let get_vmsize () =
  let pid = Unix.getpid () in
  let fn = sprintf "/proc/%d/stat" pid in
  let stat_data = File.with_file_in fn (IO.read_all) in
  let fields = String.nsplit stat_data " " in
  if List.length fields > 22 then
    List.nth fields 22 
  else
    "Not enough fields in " ^ stat_data

module EBuf = struct
  type t = string * int
  let add_data (pbuf, plen) data offset = 
    let blen = String.length pbuf in
    let dlen = String.length data in
    let have_len = ref blen in
    let need_len = offset + dlen in
    while !have_len < need_len do
      have_len := 2 * !have_len;
    done;
    let pbuf = 
      if !have_len <> blen then
	let new_buf = String.create !have_len in
	String.blit pbuf 0 new_buf 0 blen;
	new_buf
      else
	pbuf
    in
    String.blit data 0 pbuf offset dlen;
    (pbuf, plen + dlen)
  let get_data (pbuf, plen) = String.head pbuf plen
  let get_exp (pbuf,plen) = plen
end

module SList = struct
  type t = {queue: (int * string) list}
  let rec add_pkt_aux offset data = function 
    | (off,_)::t as l when offset < off -> (offset,data) :: l
    | (off,_ as e)::t (* offset >= off *) -> e :: add_pkt_aux offset data t
    | [] -> [(offset,data)]
  let add_pkt t data offset = {queue = add_pkt_aux offset data t.queue}
  let rec get_data_aux str = function
    | [] -> ()
    | (o,d)::t -> String.blit d 0 str o (String.length d); get_data_aux str t
  let get_all t = 
    if t.queue = [] then "" else
      let len = List.map (fun (on,pn) -> on + String.length pn) t.queue |> List.max in
      let str = (String.create len) in
      get_data_aux str t.queue;
      str
  let get_exp = function
    | {queue=[]} -> 1
    | {queue=(on,pn)::_} -> on + String.length pn
  let rec count_ready_aux acc = function
    | [_] -> acc + 1
    | (o,p)::((a,b)::_ as tl) when a = o + String.length p -> count_ready_aux (acc+1) tl
    | _ -> acc
  let count_ready {queue=q} = count_ready_aux 0 q
  let get_one = function
    | {queue=[]} -> raise Not_found
    | {queue=h::t} -> h, {queue=t}
  let singleton d = {queue=[(0,d)]}
  let empty = {queue=[]}
end
module PB = SList

let round_num = ref (-1)

module Run (P: Parser) = struct

  let ht = Hashtbl.create 1000
  let last_time = ref (Sys.time ())

  let post_round time =
    Hashtbl.iter (fun _ p -> P.delete_parser p; decr conc_flows) ht;
    Hashtbl.clear ht;
(*    Gc.compact (); *)
    printf "%s\t%d\t%4.2f\n%!" P.id !round_num time

  let act_packet (flow, data, fin) =
    let p = 
      try Hashtbl.find ht flow 
      with Not_found -> P.new_parser ()	|> tap (Hashtbl.add ht flow) 
    in
    P.add_data p data;
    if fin then (Hashtbl.remove ht flow; P.delete_parser p;)
	
  let act_flow fl = 
    let p = P.new_parser () in
    P.add_data p fl;
    P.delete_parser p
end
;;

let parseable = 
  let ht = Hashtbl.create 1000 in
  fun (flow, offset, data, (syn,ack,fin)) ->
    let exp = 
      try Hashtbl.find ht flow 
      with Not_found -> 
	incr flow_count;
	incr conc_flows;
	if !max_flows < !conc_flows then max_flows := !conc_flows;
	(ref (offset+1)) |> tap (Hashtbl.add ht flow)
    in
    let dlen = String.length data in
    let should_parse = dlen > 0 && offset = !exp in
    if should_parse then exp := !exp + dlen;
    if fin then (Hashtbl.remove ht flow; decr conc_flows);
    if should_parse then Some (flow,data,fin) else None

let ht2 = Hashtbl.create 50000
let assemble strs =
  let flows = ref Vect.empty in
  let act_pkt ((sip,dip,sp,dp as flow), offset, data, (syn,ack,fin)) = 
    (*    if offset < 0 || offset > 1 lsl 25 then printf "Offset: %d, skipping\n%!" offset 
	  else *)
    let prev, isn = 
      try Hashtbl.find ht2 flow 
      with Not_found -> PB.empty, offset + 1 in
    let offset = offset - isn in
(*    if snd prev > 0 && data <> ""
    then printf 
      "(%lx,%lx,%d,%d): joining %d bytes at offset %d to prev of %d bytes (close: %B)\n" 
      sip dip sp dp (String.length data) offset (snd prev) fin;  *)
    let joined, isn = (* TODO: handle pre-arrivals? *)
      if data = "" || offset = -1 then
	prev, isn (* no change *)
      else if offset < 0 || offset > 160_000 + (PB.get_exp prev) then (
	(* assume new flow *)
(*	printf "offset: %d expected: %d, assuming missing fin\n%!" offset (snd prev); *)
	flows := Vect.append (PB.get_all prev) !flows; 
	(PB.singleton data), offset
      ) else 
	PB.add_pkt prev data offset, isn
    in
    if fin then (
      flows := Vect.append (PB.get_all joined) !flows;
      Hashtbl.remove ht2 flow
    ) else
      Hashtbl.replace ht2 flow (joined, isn)
  in
  Enum.map Pcap.to_pkt_stream strs |> Enum.concat |> Enum.iter act_pkt;
  let stream_len = Vect.enum !flows |> Enum.map String.length |> Enum.reduce (+) in
(*  printf "# %d streams re-assembled, total_len: %d\n" (Vect.length !flows) stream_len; *)
  let flows2 = ref Vect.empty in
  Hashtbl.iter (fun (sip,dip,sp,dp) (j,_) -> let j = PB.get_all j in if String.length j > 0 then ((*Printf.printf "(%lx,%lx,%d,%d): %S\n" sip dip sp dp (j);*) flows2 := Vect.append j !flows2)) ht2;
  let stream_len2 = Vect.enum !flows2 |> Enum.map String.length |> Enum.reduce (+) in
(*  printf "# %d streams un-fin'ed, total_len: %d\n" (Vect.length !flows2) stream_len2; *)
  trace_len := float ((stream_len + stream_len2) * 8) /. float (1024 * 1024);
  Vect.concat !flows !flows2

let main_loop post_round f xs =
  let ret = ref [] in
  round_num := 0;
  while incr round_num; !round_num <= rep_cnt do
    let tpre = Sys.time () (*and vm_pre = get_vmsize ()*) in
    Vect.iter f xs;
    let time = Sys.time () -. tpre in
    post_round time;
(*    printf "#vm_pre: %s\tvm_post: %s\n" vm_pre (get_vmsize ()); *)
    ret := time :: !ret
  done;
  Gc.compact ();
  List.rev !ret

let mbit_size v =
  let byte_size = 
    Vect.fold_left (fun acc (_,x,_) -> acc + String.length x) 0 v 
  in
  float (byte_size * 8) /. float (1024 * 1024)

let pre_parse strs = 
  Enum.map Pcap.to_pkt_stream strs
  |> Enum.concat 
  |> Enum.filter_map parseable
  |> Vect.of_enum
  |> tap (fun xs -> trace_len := mbit_size xs)

let print_header () = 
  Gc.compact();
  let t0 = Sys.time () in
  printf "# Init time: %4.2f\n" t0;
  printf "parser\tround\ttime\n%!"

 
let () = 
  if fns = [] then failwith "Not enough arguments";
  let strs = List.enum fns |> Enum.map read_file_as_str in
  let tpac,tflow,tnull = (
    if pre_assemble then 
      let xs = assemble strs in
      print_header ();
      (let module M = Run(Pac) in main_loop M.post_round M.act_flow xs),
      (let module M = Run(Flow) in main_loop M.post_round M.act_flow xs),
      (let module M = Run(Null)  in main_loop M.post_round M.act_flow xs)
    else
      let xs = pre_parse strs in
      print_header ();
      (let module M = Run(Pac) in main_loop M.post_round M.act_packet xs),
      (let module M = Run(Flow) in main_loop M.post_round M.act_packet xs),
      (let module M = Run(Null)  in main_loop M.post_round M.act_packet xs)
  )
  in

  let rates ts= List.map2 (-.) ts tnull |> List.map (fun t -> !trace_len /. t) in
  let fr,pr = rates tflow, rates tpac in
  printf "#Flow rates: %a\n" (List.print Float.print) (rates tflow);
  printf "# Pac rates: %a\n" (List.print Float.print) (rates tpac);
  printf "#speed ratio: %4.2f\n" (list_avg fr /. list_avg pr);
  ()


let () = 
  Printf.printf "#completed in %4.2fs, (f:%d vs p:%d) events\n" 
    (Sys.time ()) (Flow.get_event_count ()) (Pac.get_event_count ())
