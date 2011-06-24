open Pcap
open Batteries_uni
open Printf

let debug = true

let new_parser = Prog_parse.new_parser "spec.ca" "extr.ca"

let is_printable c = c = '\n' || (Char.code c >= 32 && Char.code c <= 126) 
let clean_unprintable s = String.map (fun x -> if is_printable x then x else '.') s 
let print_ip oc x = 
  let x = (Int32.to_int x) in
  fprintf oc "%d.%d.%d.%d" (x lsr 24 land 255) (x lsr 16 land 255) (x lsr 8 land 255) (x land 255)
let print_flow oc (a,b,c,d) = fprintf oc "(%a,%a,%d,%d)" print_ip a print_ip b c d


let flow_table = Hashtbl.create 1000
let act_packet (_ts, flow, _seq_no, data, (_syn, _ack, fin)) =
  if String.length data = 0 && not fin then () else
  let p, is_new = 
    try Hashtbl.find flow_table flow, false
    with Not_found -> 
      new_parser () |> tap (Hashtbl.add flow_table flow), true
  in
  let ev_pre = !Ns_types.matches in
  p data;
  if fin then Hashtbl.remove flow_table flow;
  if debug && (is_new || !Ns_types.matches <> ev_pre) then Printf.printf "\nP%a:\n%s\n" print_flow flow (clean_unprintable data);
  if debug then eprintf "Flows: %d\n%!" (Hashtbl.length flow_table)


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
  let payload_size = match res with Some (_,_,_,d,_ as p) -> act_packet p; String.length d | None -> -1 in
  match payload_size with
    | -1 -> (* ERROR *)
      printf "Packet data: %d bytes, failed to parse as eth: %S" h.len t;
    | 0 -> () (* ignore empty packets *)
    | x -> 
      printf "Packet data: %d(%d) bytes, events: %d\n%!" h.len x (!Ns_types.matches)

let main () =
  let dev = pcap_lookupdev () in
  printf "Opening interface %s ...\n%!" dev;
  let openlive = pcap_open_live dev 16636 1 0 in
  printf "Filtering only port 80\n%!";
  let ok,my_ip,mask = pcap_lookupnet dev in
  if ok <> 0 then failwith "Couldn't lookup net info on dev";
  let ok,filter = pcap_compile openlive "port 80" 0 my_ip in
  if ok <> 0 then failwith "Couldn't compile pattern";
  if pcap_setfilter openlive filter <> 0 then failwith "Couldn't set filter";
  printf "Handling packets\n%!";
  pcap_loop openlive (-1) handle_packet "" |> ignore;
  flush_all ();
  pcap_close openlive
    

let () = main ()
