open BatStd
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

let () = at_exit (fun () ->  
  Printf.printf "#Dropped packets (non-tcp, bad-tcp, ipv6, eth, non-eth): %d, %d, %d, %d, %d\n" !dropped_packets_non_tcp !dropped_packets_bad_tcp !dropped_packets_v6 !dropped_packets_eth !dropped_packets_non_eth;
  Printf.printf "#Kept packets: %d of %d bytes\n" !kept_packets !kept_bytes
)

let rec main () =
  if Array.length Sys.argv <= 1 then failwith "libpcap dumpfile";
  let endian, file_header, network, (bits: Bitstring.bitstring) = 
    Bitstring.bitstring_of_file Sys.argv.(1) |> libpcap_header in

  (* Read the packets and print them out. *)
  BatEnum.from_loop bits (libpcap_packet endian file_header)
  |> BatEnum.filter_map (decode_packet network)
  |> BatEnum.iter print_packet
      
and print_packet (_,_,p,fin) = printf "%d:%B " (String.length p) fin
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
	contents : -1 : bitstring } ->
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
	Some ((sip,dip,s_port,d_port), seqno, 
	      Bitstring.string_of_bitstring payload, fin)
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

