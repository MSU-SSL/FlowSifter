open Pcap
open Batteries
open Printf

let debug = true

let is_printable c = c = '\n' || (Char.code c >= 32 && Char.code c <= 126) 
let clean_unprintable s = String.map (fun x -> if is_printable x then x else '.') s 
let print_ip oc x = 
  let x = (Int32.to_int x) in
  fprintf oc "%d.%d.%d.%d" (x lsr 24 land 255) (x lsr 16 land 255) (x lsr 8 land 255) (x land 255)
let print_flow oc (a,b,c,d) = fprintf oc "(%a,%a,%d,%d)" print_ip a print_ip b c d


let flow_table = Hashtbl.create 1000
let flow_lift new_parser (_ts, flow, _seq_no, data, (_syn, _ack, fin)) =
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
let handle_packet act_packet (s:string) (h:pcap_pkthdr) (t:string) =
(*  let proto = try (int_of_char (String.unsafe_get t 23)) with _ -> 0 in *)
  let t = gen_string t h.caplen in
  Pcap_parser.packet (Int32.of_int h.ts.tv_sec) t 
  |> Pcap_parser.decode_eth 
  |> Option.may act_packet

let pcap_capture filter handler =
  let dev = pcap_lookupdev () in
  printf "Opening interface %s ...\n%!" dev;
  let openlive = pcap_open_live dev 16636 1 0 in
  if filter <> "" then (
    printf "Filtering only port 80\n%!";
    let ok,my_ip,mask = pcap_lookupnet dev in
    if ok <> 0 then failwith "Couldn't lookup net info on dev";
    let ok,filter = pcap_compile openlive filter 0 my_ip in
    if ok <> 0 then failwith "Couldn't compile pattern";
    if pcap_setfilter openlive filter <> 0 then failwith "Couldn't set filter";
  );
  printf "Handling packets\n%!";
  pcap_loop openlive (-1) handler "" |> ignore;
  pcap_close openlive

let pcap_act filter new_parser = 
  pcap_capture filter (flow_lift new_parser |> handle_packet)

let main () =
  let new_parser = Prog_parse.new_parser "spec.ca" "extr.ca" in
  pcap_act "port 80" new_parser;
  flush_all ()

(*let () = main () *)
  
