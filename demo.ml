open Pcap
open Batteries
open Printf

(*let proto_to_str = function 6 -> "TCP" | 1 -> "ICMP" | 17 -> "UDP" | _ -> "Unknown" *)

let gen_string ptr len =
  let ret = String.create len in
  String.unsafe_blit ptr 0 ret 0 len;
  ret

(* callback *)
let handle_packet (s:string) (h:pcap_pkthdr) (t:string) =
(*  let proto = try (int_of_char (String.unsafe_get t 23)) with _ -> 0 in *)
  let t = gen_string t h.caplen in
  let res = Pcap_parser.packet (Int32.of_int h.ts.tv_sec) t |> Pcap_parser.decode_eth in
  let str = match res with Some _ -> "ok" | None -> "fail" in
  printf "Packet data: %d bytes, packet structure: %s\n%!" h.len str

let main () =
  let dev = pcap_lookupdev () in
  printf "Opening interface %s ...\n%!" dev;
  let openlive = pcap_open_live dev 1500 1 0 in
  printf "Filtering only port 80\n";
  let (_,my_ip, mask) = pcap_lookupnet dev in
  let _,filter = pcap_compile openlive "port 80" 0 my_ip in
  pcap_setfilter openlive filter |> ignore;
  printf "Handling packets\n%!";
  pcap_loop openlive (-1) handle_packet "" |> ignore;
  flush_all ();
  pcap_close openlive
    

let () = main ()
