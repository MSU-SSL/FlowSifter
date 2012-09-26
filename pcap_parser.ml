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

let debug = false

let kept_packets = ref 0
let kept_bytes = ref 0
let dropped_packets_non_tcp = ref 0
let dropped_packets_bad_tcp = ref 0
let dropped_packets_bad_udp = ref 0
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

and print_packet (_,_,_,p,(_,_,fin)) = printf "%d:%B " (String.length p) fin
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

and libpcap_packet e _file_header bits =
  bitmatch bits with
    | { ts_sec : 32 : endian (e); (* packet timestamp seconds *)
	_ts_usec : 32 : endian (e);  (* packet timestamp microseconds *)
	incl_len : 32 : endian (e); (* packet length saved in this file *)
	_orig_len : 32 : endian (e); (* packet length originally on wire *)
	pkt_data : Int32.to_int incl_len*8 : bitstring;
	rest : -1 : bitstring
      } ->
      (ts_sec, pkt_data), rest

    | { _ } -> raise BatEnum.No_more_elements

and decode_eth (ts,pkt_data) =
  (*  let (ts_sec, ts_usec, _, orig_len) = pkt_header in *)
  (*  printf "%ld.%ld %ldB " ts_sec ts_usec orig_len; *)

  bitmatch pkt_data with
    | { _d0 : 8; _d1 : 8; _d2 : 8; _d3 : 8; _d4 : 8; _d5 : 8; (* ether dest *)
	_s0 : 8; _s1 : 8; _s2 : 8; _s3 : 8; _s4 : 8; _s5 : 8; (* ether src *)
	0x0800 : 16;    (*0x0800 = IPv4 *)              (* ethertype *)
	packet : -1 : bitstring                         (* payload *)
      } -> (
      try decode_ip (ts,packet)
      with Failure _ ->
(*	printf "%x:%x:%x:%x:%x:%x < %x:%x:%x:%x:%x:%x "
          d0 d1 d2 d3 d4 d5 s0 s1 s2 s3 s4 s5; *)
        incr dropped_packets_eth;
	None
    )
    | { _d0 : 8; _d1 : 8; _d2 : 8; _d3 : 8; _d4 : 8; _d5 : 8; (* ether dest *)
	_s0 : 8; _s1 : 8; _s2 : 8; _s3 : 8; _s4 : 8; _s5 : 8; (* ether src *)
	0x86DD : 16;    (*0x86dd = IPv6 *)              (* ethertype *)
	packet : -1 : bitstring                         (* payload *)
      } -> (
      try decode_ip (ts,packet)
      with Failure _ ->
(*	printf "%x:%x:%x:%x:%x:%x < %x:%x:%x:%x:%x:%x "
          d0 d1 d2 d3 d4 d5 s0 s1 s2 s3 s4 s5; *)
        incr dropped_packets_eth;
	None
    )
    | { _ } ->
      incr dropped_packets_non_eth; None
and decode_ip (ts,packet) =
  bitmatch packet with
    | { 4 : 4;                       (* IPv4 *)
	5 : 4; _tos : 8; length : 16;
	_identification : 16; _flags : 3; _fragoffset : 13;
	_ttl : 8; protocol : 8; _checksum : 16; sip : 32; dip : 32;
	contents : (length * 8 - 160) : bitstring } ->
(*      printf "IP len: %d " length; *)
      if protocol = 6 then  (* TCP decode *)
	decode_tcp ts sip dip contents
      else if protocol = 17 then (* UDP decode *)
	decode_udp ts sip dip contents
      else
	(incr dropped_packets_non_tcp; None)
    | { 6 : 4;                      (* IPv6 *)
	_tclass : 8; _flow : 20;
	_length : 16; _nexthdr : 8; _ttl : 8;
	_(*source*) : 128 : bitstring;
	_(*dest*) : 128 : bitstring;
	_(*payload*) : -1 : bitstring } ->
      incr dropped_packets_v6; None
    | { _ } -> failwith "Not IPv4 or IPv6"
and decode_tcp ts sip dip packet =
  bitmatch packet with
      { s_port:16; d_port: 16; seqno: 32; _ackno: 32;
	d_off: 4; _misc: 6;
	_urg:1; ack:1; _psh:1; _rst:1; syn: 1; fin: 1;
	_window: 16; _checksum: 16; _urg_ptr: 16;
	_: (d_off-5)*32: bitstring;
	payload : -1 : bitstring } ->
(*	printf "TCP seqno: %ld\n" seqno; *)
	incr kept_packets;
	kept_bytes := !kept_bytes + (Bitstring.bitstring_length payload lsr 3);
	Some (ts, (sip,dip,s_port,d_port), Int32.to_int seqno,
	      Bitstring.string_of_bitstring payload, (syn,ack,fin))
    | { _ } -> incr dropped_packets_bad_tcp; None
and decode_udp ts sip dip packet =
  bitmatch packet with
      { s_port:16; d_port: 16;
	length: 16; _checksum: 16;
	payload : (length-8)*8 : bitstring } ->
(*	printf "TCP seqno: %ld\n" seqno; *)
	incr kept_packets;
	kept_bytes := !kept_bytes + (Bitstring.bitstring_length payload lsr 3);
	Some (ts, (sip,dip,s_port,d_port), 0,
	      Bitstring.string_of_bitstring payload, (true,false,true))
    | { _ } -> incr dropped_packets_bad_udp; None
and decode_packet = function
  | 101l -> decode_ip
  | 1l -> decode_eth
  | linktype -> failwith ("Unknown pcap Linktype: " ^ Int32.to_string linktype)

let packet (ts:int32) contents = (ts, Bitstring.bitstring_of_string contents)

(* let () = main () *)

let to_pkt_stream str =
  let endian, file_header, network, bits =
    Bitstring.bitstring_of_string str |> libpcap_header in
  (* Read the packets and Enum them out. *)
  BatEnum.from_loop bits (libpcap_packet endian file_header)
  |> BatEnum.filter_map (decode_packet network)

(*** IMPLEMENTATIONS OF reassembly buffers ***)
module SList = struct
  (* (offset * data) list, sorted by offset in increasing order *)
  type t = {ts: int32; exp: int; buf: (int * string) list}

  let shift_offset x t = if debug then printf "Adj%d " (-x); List.map (fun (o,d) -> o - x, d) t
  let rec remove_until cutoff = function
    | (off,d)::t when off + String.length d < cutoff -> remove_until cutoff t
    | l -> l
  let rec add_pkt_aux offset data = function
    | (off,_)::_ as l when offset < off -> (offset,data) :: (remove_until (off+String.length data) l)
      (* drop the new packet if it overlaps completely an old one *)
    | (o,d)::_ as l when o + String.length d >= offset + String.length data -> l
    | e::t (* offset >= off *) -> e :: add_pkt_aux offset data t
    | [] -> [(offset,data)]
  let add_pkt t data offset ts =
    let e = max t.exp (offset + String.length data) in
    let b = if String.length data > 0 then add_pkt_aux offset data t.buf else t.buf in
    let ts = max ts t.ts in
    if offset < 0 then (if debug then eprintf "O"; {ts=ts; exp=e-offset; buf=shift_offset offset b}) else {ts=ts;exp=e;buf=b}
  let rec blit_data str low (o,d) =
    if debug then eprintf "Bl:%dB@%d " (String.length d) o;
    String.blit d 0 str (o-low) (String.length d)
  let get_all t =
    match t.buf with
      |	[] -> if debug then eprintf "E"; ""
      | (min_offset,_)::_ ->
	let max_end = List.fold_left (fun e (o,d) -> max e (o+String.length d)) 0 t.buf in
	if debug then eprintf "Flow len computed: %d expected: %d, Packet (offset,len)s: %a\n" max_end t.exp (Int.print |> Tuple2.printn |> List.print) (List.map (Tuple2.map2 String.length) t.buf);
(*	if max_end <> t.exp then assert false; *)
	let len = max_end - min_offset in
(*	if len > 50_000_000 then assert false; *)
	let str = String.create len in
	List.iter (blit_data str min_offset) t.buf;
	if debug then eprintf "\n";
	str
  let rec get_exp t = t.exp
(*  let rec count_ready_aux acc = function
    | [_] -> acc + 1
    | (o,p)::((a,_)::_ as tl) when a = o + String.length p -> count_ready_aux (acc+1) tl
    | _ -> acc
  let count_ready t = count_ready_aux 0 t *)
  let get_one = function
    | {buf=[]} -> raise Not_found
    | {buf=h::t} as s -> h, {s with buf=t}
  let avail_offset = function {buf=[]} -> max_int | {buf=(o,_)::_} -> o
  let empty = {ts=0l;exp=0; buf=[]}
  let shift_offset x t = List.map (fun (o,d) -> o - x, d) t
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

(* get a single flow of packets from an enum of filenames *)
let packets_of_files fns =
  fns /@ read_file_as_str /@ to_pkt_stream
  |> Enum.reduce (Enum.merge (fun (ts1,_,_,_,_) (ts2,_,_,_,_) -> ts1 < ts2))

let trace_size_v v =
  Vect.fold_left (fun acc (_,x,_,_) -> acc + String.length x) 0 v
let trace_size a =
  Array.fold_left (fun acc (_,x,_,_) -> acc + String.length x) 0 a

let max_conc = ref 0
let conc_flows = ref 0
let flow_count = ref 0

let count_new_flow () = incr flow_count; incr conc_flows; max_conc := max !max_conc !conc_flows; if debug then eprintf "N"
let count_done_flow () = decr conc_flows; if debug then eprintf "D"

let has_content (_,p,_,_) = not (String.is_empty p)
let push_v vr x = vr := Vect.append x !vr

(*** FLOW REASSEMBLY ***)
let ht2 = Hashtbl.create 50000
let assemble count fns =
  let flows = ref Vect.empty in
  let push_flow x = if has_content x then push_v flows x else if debug then eprintf "E" in
  let act_pkt (ts,(_sip,_dip,_sp,_dp as flow), seq_no, data, (_syn,_ack,fin)) =
    if Vect.length !flows < count then
    (*    if seq_no < 0 || seq_no > 1 lsl 25 then printf "Seq_No: %d, skipping\n%!" seq_no
	  else *)
    let buffer, init_seq_no =
      try Hashtbl.find ht2 flow
      with Not_found -> count_new_flow (); (ref PB.empty, ref seq_no) |> tap (Hashtbl.add ht2 flow)
    in
    let rel_seq_no = seq_no - !init_seq_no in
    let time_diff = Int32.sub ts !buffer.PB.ts in
    if rel_seq_no <> 0 && time_diff > 120l then ( (* autoclose after 2 minutes idle *)
      if debug then eprintf "A%ld " time_diff;
      push_flow (flow, PB.get_all !buffer, true, !init_seq_no);
      buffer := PB.empty; init_seq_no := seq_no;
    );
    buffer := PB.add_pkt !buffer data rel_seq_no ts;
    (* fix out of order syn packet *)
    if rel_seq_no < 0 then init_seq_no := seq_no;

    if fin then ( (* TODO: handle out-of-order fin *)
      push_flow (flow, PB.get_all !buffer, true, !init_seq_no);
      count_done_flow();
      Hashtbl.remove ht2 flow
    )
  in
  packets_of_files fns |> Enum.iter act_pkt;
(*  let stream_len = trace_size_v !flows in
  printf "# %d streams re-assembled (max:%d), total_len: %d\n" (Vect.length !flows) count stream_len;
  printf "# flow_count: %d conc_flows: %d max_conc: %d\n" !flow_count !conc_flows !max_conc; *)
  let flows2 = ref Vect.empty in
  Hashtbl.iter (fun (_sip,_dip,_sp,_dp as fid) (b,_) -> let d = PB.get_all !b in if String.length d > 0 then ((*Printf.printf "(%lx,%lx,%d,%d): %S\n" sip dip sp dp (j);*) push_v flows2 (fid,d,false,0))) ht2;
(*  let stream_len2 = trace_size_v !flows2 in
  printf "# %d streams un-fin'ed, total_len: %d\n" (Vect.length !flows2) stream_len2;  *)
  let all_flows = Vect.concat !flows !flows2 in
(*  printf "# Flows pre-assembled (len: %a max_conc: %d flows: %d)\n" Ean_std.print_size_B (stream_len + stream_len2) !max_conc !flow_count;
  printf "# Flows in vect: %d\n" (Vect.length all_flows);  *)
  Vect.enum all_flows


(*** FILTER DUPLICATE/OUT-OF-ORDER PACKETS ***)
let parseable =
  let ht = Hashtbl.create 1000 in
  fun (ts, flow, offset, data, (_syn,_ack,fin)) ->
    let exp,buf =
      try Hashtbl.find ht flow
      with Not_found -> count_new_flow(); (ref (offset+1), ref PB.empty) |> tap (Hashtbl.add ht flow)
    in
    let dlen = String.length data in
    let should_parse = (dlen > 0 && offset = !exp) || fin in
    if not should_parse && offset > !exp then buf := PB.add_pkt !buf data offset ts;
    if should_parse then exp := !exp + dlen;
    let data = ref data in
    while PB.avail_offset !buf <= !exp do
      let (o,d),t = PB.get_one !buf in
      let dlen = String.length d in
      buf := t;
      if o < !exp then
	if o + dlen < !exp then
	  (* take the tail of the packet *)
	  ( data := !data ^ (String.tail d (!exp - o)); exp := !exp + (dlen - (!exp - o)); )
	else () (* packet is strictly in the past, drop *)
      else
	  ( data := !data ^ d; exp := !exp + dlen; )
    done;
    if fin then (Hashtbl.remove ht flow; count_done_flow ());
    (flow, !data, fin, offset)

let pre_parse fns = packets_of_files fns /@ parseable (* very stateful check of whether we should parse that packet *)

(***  INPUT FROM AN ENUM OF FILES - ONE FLOW PER FILE ***)

let make_flows fns =
  max_conc := 1; flow_count := Enum.count fns;
  fns |> Enum.map (read_file_as_str ~verbose:false)
    |> Enum.map (fun s -> ((0l,0l,1,unique()),s,true,0)) (* forge the fin flag, flow id and offset *)
(*|> (fun v -> v, trace_size v)
    |> tap (fun (_,l) -> printf "#Flows read from file (len: %a max_conc: %d flows: %d)\n" Ean_std.print_size_B l 1 !flow_count;)
*)

let make_packets flows =
  let flow_ids = Enum.range 0 /@ (fun i -> (0l,0l,0,i)) in
  let flows = Enum.combine (flow_ids, flows) in
  let ret = ref Vect.empty in
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
      (id,h,true,-1), None
    else
      (id,h,false,-1), Some (id, t)
  in
  let rec loop ifp =
    if Enum.is_empty flows && Enum.is_empty ifp then () else (
      Enum.get flows |> Option.may (Enum.push ifp);
      let l = Enum.count ifp in
      if l > !max_conc then max_conc := l;
      let send, keep = ifp |> Enum.map split_rand |> Enum.uncombine in
      Random.shuffle send |> Array.iter (push_v ret);
      Enum.filter_map identity keep |> loop
    )
  in
  loop (Enum.empty ());
  Vect.enum !ret

(*    |> tap (fun (_v,l) ->
      printf "#Flows packetized (len: %a max_conc: %d flows: %d packets:%d)\n"
	Ean_std.print_size_B l !max_conc !flow_count (Vect.length _v);
(*      Vect.print (fun oc (fid,data,fin) -> fprintf oc "%d (%B): %S\n\n" fid fin data) stdout v; *)
    )*)

let make_packets_files fns =
  let flows = fns |> Enum.map (read_file_as_str ~verbose:false) in
  make_packets flows

(** ENUM OF FILES -> ENUM OF DEST IPs**)
