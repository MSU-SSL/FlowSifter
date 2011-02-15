open Batteries
open Printf

(* Print out packets from a tcpdump / libpcap / wireshark capture file.
 * $Id$
 *
 * To test this, capture some data using:
 *   /usr/sbin/tcpdump -s 1500 -w /tmp/dump
 * then analyze it using:
 *   ./libpcap /tmp/dump
 *
 * The file format is documented here:
 *   http://wiki.wireshark.org/Development/LibpcapFileFormat
 *
 * libpcap endianness is determined at runtime.
 *)

let kept_packets = ref 0
let kept_bytes = ref 0 
let dropped_packets_non_tcp = ref 0
let dropped_packets_bad_tcp = ref 0
let dropped_packets_v6 = ref 0
let dropped_packets_eth = ref 0
let dropped_packets_non_eth = ref 0

(*
let () = at_exit (fun () ->  
  Printf.printf "#Dropped packets (non-tcp, bad-tcp, ipv6, eth, non-eth): %d, %d, %d, %d, %d\n" !dropped_packets_non_tcp !dropped_packets_bad_tcp !dropped_packets_v6 !dropped_packets_eth !dropped_packets_non_eth;
  Printf.printf "#Kept packets: %d of %d bytes\n" !kept_packets !kept_bytes
)
*)

let rec main () =
  if Array.length Sys.argv <= 1 then failwith "libpcap dumpfile";
  let endian, file_header, network, (bits: Bitstring.bitstring) = 
    Bitstring.bitstring_of_file Sys.argv.(1) |> libpcap_header in

  (* Read the packets and print them out. *)
  BatEnum.from_loop bits (libpcap_packet endian file_header)
  |> BatEnum.filter_map (decode_packet network)
  |> BatEnum.iter print_packet
      
and print_packet (_,_,p,(_,_,fin)) = printf "%d:%B " (String.length p) fin
(* Determine the endianness (at runtime) from the magic number. *)
and endian_of = function
  | 0xa1b2c3d4_l -> Bitstring.BigEndian
  | 0xd4c3b2a1_l -> Bitstring.LittleEndian
  | _ -> assert false

and libpcap_header bits =
  bitmatch bits with
    | { ((0xa1b2c3d4_l|0xd4c3b2a1_l) as magic) : 32; (* magic number *)
	major : 16 : endian (endian_of magic);     (* version *)
	minor : 16 : endian (endian_of magic);
	timezone : 32 : endian (endian_of magic);  (* timezone correction (secs)*)
	_ : 32 : endian (endian_of magic); (* always 0 apparently *)
	snaplen : 32 : endian (endian_of magic);  (* max length of capt pckts *)
	network : 32 : endian (endian_of magic);  (* data link layer type *)
	rest : -1 : bitstring
      } ->
      (endian_of magic), (major, minor, timezone, snaplen), network, rest

    | { _ } ->
      failwith "not a libpcap/tcpdump packet capture file"

and libpcap_packet e file_header bits =
  bitmatch bits with
    | { ts_sec : 32 : endian (e); (* packet timestamp seconds *)
	ts_usec : 32 : endian (e);  (* packet timestamp microseconds *)
	incl_len : 32 : endian (e); (* packet length saved in this file *)
	orig_len : 32 : endian (e); (* packet length originally on wire *)
	pkt_data : Int32.to_int incl_len*8 : bitstring;
	rest : -1 : bitstring
      } ->
      pkt_data, rest

    | { _ } -> raise BatEnum.No_more_elements

and decode_eth pkt_data =
  (*  let (ts_sec, ts_usec, _, orig_len) = pkt_header in *)
  (*  printf "%ld.%ld %ldB " ts_sec ts_usec orig_len; *)

  bitmatch pkt_data with
    | { d0 : 8; d1 : 8; d2 : 8; d3 : 8; d4 : 8; d5 : 8; (* ether dest *)
	s0 : 8; s1 : 8; s2 : 8; s3 : 8; s4 : 8; s5 : 8; (* ether src *)
	0x0800 : 16;    (*0x0800 = IPv4 *)              (* ethertype *)
	packet : -1 : bitstring                         (* payload *)
      } -> (
      try decode_ip packet 
      with Failure _ -> 
	printf "%x:%x:%x:%x:%x:%x < %x:%x:%x:%x:%x:%x "
          d0 d1 d2 d3 d4 d5 s0 s1 s2 s3 s4 s5;
        incr dropped_packets_eth; 
	None
    )
    | { d0 : 8; d1 : 8; d2 : 8; d3 : 8; d4 : 8; d5 : 8; (* ether dest *)
	s0 : 8; s1 : 8; s2 : 8; s3 : 8; s4 : 8; s5 : 8; (* ether src *)
	0x86DD : 16;    (*0x86dd = IPv6 *)              (* ethertype *)
	packet : -1 : bitstring                         (* payload *)
      } -> (
      try decode_ip packet 
      with Failure _ -> 
	printf "%x:%x:%x:%x:%x:%x < %x:%x:%x:%x:%x:%x "
          d0 d1 d2 d3 d4 d5 s0 s1 s2 s3 s4 s5;
        incr dropped_packets_eth; 
	None
    )
    | { _ } ->
      incr dropped_packets_non_eth; None
and decode_ip packet =
  bitmatch packet with
    | { 4 : 4;                       (* IPv4 *)
	5 : 4; tos : 8; length : 16;
	identification : 16; flags : 3; fragoffset : 13;
	ttl : 8; protocol : 8; checksum : 16; sip : 32; dip : 32;
	contents : (length * 8 - 160) : bitstring } ->
(*      printf "IP len: %d " length; *)
      if protocol = 6 then  (* TCP decode *)
	decode_tcp sip dip contents
      else
	(incr dropped_packets_non_tcp; None)
    | { 6 : 4;                      (* IPv6 *)
	tclass : 8; flow : 20;
	length : 16; nexthdr : 8; ttl : 8;
	_(*source*) : 128 : bitstring;
	_(*dest*) : 128 : bitstring;
	_(*payload*) : -1 : bitstring } ->
      incr dropped_packets_v6; None
    | { _ } -> failwith "Not IPv4 or IPv6"
and decode_tcp sip dip packet =
  bitmatch packet with
      { s_port:16; d_port: 16; seqno: 32; ackno: 32; 
	d_off: 4; _misc: 6; 
	urg:1; ack:1; psh:1; rst:1; syn: 1; fin: 1;
	_window: 16; _checksum: 16; _urg_ptr: 16;
	_: (d_off-5)*32: bitstring;
	payload : -1 : bitstring } ->
(*	printf "TCP seqno: %ld\n" seqno; *)
	incr kept_packets;
	kept_bytes := !kept_bytes + (Bitstring.bitstring_length payload lsr 3);
	Some ((sip,dip,s_port,d_port), Int32.to_int seqno, 
	      Bitstring.string_of_bitstring payload, (syn,ack,fin))
    | { _ } -> incr dropped_packets_bad_tcp; None
and decode_packet = function
  | 101l -> decode_ip
  | 1l -> decode_eth
  | linktype -> failwith ("Unknown pcap Linktype: " ^ Int32.to_string linktype)

(* let () = main () *)

let to_pkt_stream str =
  let endian, file_header, network, bits = 
    Bitstring.bitstring_of_string str |> libpcap_header in
  (* Read the packets and Enum them out. *)
  BatEnum.from_loop bits (libpcap_packet endian file_header)
  |> BatEnum.filter_map (decode_packet network)

(*** IMPLEMENTATIONS OF reassembly buffers ***)

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
  let get_exp (_pbuf,plen) = plen
end

module SList = struct
  type t = {queue: (int * string) list}
  let rec add_pkt_aux offset data = function 
    | (off,_)::_ as l when offset < off -> (offset,data) :: l
    | e::t (* offset >= off *) -> e :: add_pkt_aux offset data t
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
    | (o,p)::((a,_)::_ as tl) when a = o + String.length p -> count_ready_aux (acc+1) tl
    | _ -> acc
  let count_ready {queue=q} = count_ready_aux 0 q
  let get_one = function
    | {queue=[]} -> raise Not_found
    | {queue=h::t} -> h, {queue=t}
  let singleton d = {queue=[(0,d)]}
  let empty = {queue=[]}
end
module PB = SList

let read_file_as_str ?(verbose=false) fn =
  let ic = Pervasives.open_in_bin fn in
  let len = (Pervasives.in_channel_length ic) in
  if verbose then printf "#Reading %s (len %d)...%!" fn len;
  let old_gc = Gc.get() in
  Gc.set {old_gc with Gc.space_overhead = 0};
  let ret = String.create len in
  Gc.set old_gc;
  Pervasives.really_input ic ret 0 len;
  Pervasives.close_in ic;
  if verbose then printf "done\n%!";
  ret


let max_conc = ref 0
let conc_flows = ref 0
let flow_count = ref 0

(*** FLOW REASSEMBLY ***)
let parseable = 
  let ht = Hashtbl.create 1000 in
  fun (flow, offset, data, (_syn,_ack,fin)) ->
    let exp = 
      try Hashtbl.find ht flow 
      with Not_found -> 
	incr flow_count;
	incr conc_flows;
	if !max_conc < !conc_flows then max_conc := !conc_flows;
	(ref (offset+1)) |> tap (Hashtbl.add ht flow)
    in
    let dlen = String.length data in
    let should_parse = (dlen > 0 && offset = !exp) || fin in
    if should_parse then exp := !exp + dlen;
    if fin then (Hashtbl.remove ht flow; decr conc_flows);
    if should_parse then Some (flow,data,fin) else None

let ht2 = Hashtbl.create 50000
let assemble fns =
  let strs = Enum.map read_file_as_str fns in
  let flows = ref Vect.empty in
  let act_pkt ((_sip,_dip,_sp,_dp as flow), offset, data, (_syn,_ack,fin)) = 
    (*    if offset < 0 || offset > 1 lsl 25 then printf "Offset: %d, skipping\n%!" offset 
	  else *)
    let prev, isn = 
      try Hashtbl.find ht2 flow 
      with Not_found -> 
	incr flow_count; incr conc_flows; 
	if !max_conc < !conc_flows then max_conc := !conc_flows;
	PB.empty, offset + 1 in
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
      decr conc_flows;
      Hashtbl.remove ht2 flow
    ) else
      Hashtbl.replace ht2 flow (joined, isn)
  in
  Enum.map to_pkt_stream strs |> Enum.concat |> Enum.iter act_pkt;
  let stream_len = Vect.enum !flows |> Enum.map String.length |> Enum.reduce (+) in
(*  printf "# %d streams re-assembled, total_len: %d\n" (Vect.length !flows) stream_len; *)
  let flows2 = ref Vect.empty in
  Hashtbl.iter (fun (_sip,_dip,_sp,_dp) (j,_) -> let j = PB.get_all j in if String.length j > 0 then ((*Printf.printf "(%lx,%lx,%d,%d): %S\n" sip dip sp dp (j);*) flows2 := Vect.append j !flows2)) ht2;
  let stream_len2 = Vect.enum !flows2 |> Enum.map String.length |> Enum.reduce (+) in
(*  printf "# %d streams un-fin'ed, total_len: %d\n" (Vect.length !flows2) stream_len2; *)
  let trace_len = float ((stream_len + stream_len2) * 8) /. float (1024 * 1024) in
  printf "#Flows pre-assembled (len: %.2f mbit max_conc: %d flows: %d)\n" trace_len !max_conc !flow_count;
  Vect.concat !flows !flows2, trace_len

let mbit_size v =
  let byte_size = 
    Vect.fold_left (fun acc (_,x,_) -> acc + String.length x) 0 v 
  in
  float (byte_size * 8) /. float (1024 * 1024)

let mbit_sizef v =
  let byte_size = 
    Vect.fold_left (fun acc x -> acc + String.length x) 0 v 
  in
  float (byte_size * 8) /. float (1024 * 1024)

(*** FILTER DUPLICATE/OUT-OF-ORDER PACKETS ***)
let pre_parse fns = 
  let packet_stream = 
    fns
    /@ read_file_as_str
    /@ to_pkt_stream
    |> Enum.concat 
    |> Enum.filter_map parseable
    |> Vect.of_enum
  in
  let trace_len = mbit_size packet_stream in
  printf "#Flows pre-filtered for parsability (len: %.2f mbit max_conc: %d flows: %d)\n" trace_len !max_conc !flow_count;
  packet_stream, trace_len


(***  INPUT FROM AN ENUM OF FILES - ONE FLOW PER FILE ***)
let max_conc = ref 0
let conc_flows = ref 0
let flow_count = ref 0

let make_flows fns = 
  max_conc := 1; flow_count := Enum.count fns;
  fns |> Enum.map (read_file_as_str ~verbose:false) |> Vect.of_enum |> (fun v -> v, mbit_sizef v)
    |> tap (fun (_,l) -> printf "#Flows read from file (len: %.2f mbit max_conc: %d flows: %d)\n" l 1 !flow_count;)

let make_packets fns =
  flow_count := Enum.count fns;
  let flows = fns |> Enum.map (read_file_as_str ~verbose:false) |> Ean_std.number_enum in
  let ret = Enum.empty () in
  let send_byte_count () = 
    match Random.int 5 with
	0 -> 0
      | 1 -> Random.int 50
      | 2 -> Random.int 200
      | 3 -> Random.int 1000
      | 4 -> 1000 + Random.int 500
      | _ -> 1500
  in
  let split_rand (id,s) = 
    let c = send_byte_count () in 
    let h,t = String.head s c, String.tail s c in
    if t = "" then 
      (id,h,true), None
    else
      (id,h,false), Some (id, t)
  in
  let rec loop ifp =
    if Enum.is_empty flows && Enum.is_empty ifp then () else (
      Enum.get flows |> Option.may (Enum.push ifp);
      let l = Enum.count ifp in 
      if l > !max_conc then max_conc := l;
      let send, keep = ifp |> Enum.map split_rand |> Enum.uncombine in
      Random.shuffle send |> Array.enum |> Enum.push ret;
      Enum.filter_map identity keep |> loop
    )
  in
  loop (Enum.empty ());
  Enum.concat ret |> Vect.of_backwards |> (fun v -> v, mbit_size v)
    |> tap (fun (v,l) -> 
      printf "#Flows packetized (len: %.2f mbit max_conc: %d flows: %d)\n" 
	l !max_conc !flow_count;
(*      Vect.print (fun oc (fid,data,fin) -> fprintf oc "%d (%B): %S\n\n" fid fin data) stdout v; *)
    )
