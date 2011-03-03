open Batteries_uni
open Printf

let set_mhs n = Gc.set { (Gc.get()) with Gc.minor_heap_size = n; } 
let () = set_mhs 250_000;;


type parser_t = [ `Null | `Sift | `Pac ]
let p_to_string = function `Null -> "Null" | `Sift -> "Sift" | `Pac -> "Pac "

type mode = Pcap | Mux | Gen
(*** GLOBAL CONFIGURATION ***)
let check_mem_per_packet = ref false
let parse_by_flow = ref false
let rep_cnt = ref 1
let fns = ref []
let extr_ca = ref "extr.ca"
let parsers : parser_t list ref = ref [`Sift; `Pac]
let baseline = ref true
let mode = ref Pcap
let min_lev = ref 0
let gen_count = ref 0
(*** ARGUMENT HANDLING ***)

open Arg2
let set_main x = Unit (fun () -> parsers := [x])
let set_mode x = Unit (fun () -> mode := x)
let args = 
  [ (Name "packets", [Clear parse_by_flow], [], 
     "Pass individual packets to the parser");
    (Both ('f', "flows"), [Set parse_by_flow], [], 
     "Assemble the pcap files into flows"); 
    (Both ('m', "mux"), [set_mode Mux], [], 
     "Mux the given flow files into a simulated pcap");
    (Both ('g', "gen"), [set_mode Gen; Int_var min_lev; Int_var gen_count], [],
     "Generate SOAP traffic to parse (need min_lev, count)");
    (Both ('n', "rep-cnt"), [Int_var rep_cnt], [],
     "Set the number of repetitions to do");
    (Both ('b', "no-baseline"), [Clear baseline], [], "Don't use null parser as baseline");
    (Both ('s', "sift"), [set_main `Sift], [], "Run the Flowsifter parser");
    (Both ('p', "pac"), [set_main `Pac], [], "Run the pac parser");
    (Name "null", [Unit (fun () -> parsers := [])], [], "Run only the null parser");
    (Both ('x', "extr"), [String_var extr_ca], [], 
     "Use the given extraction grammar");
    (Name "seed", [Int (fun i -> Random.init i)], [], "Set the random number seed");
    (Name "mhs", [Int set_mhs], [], "Set the minor heap size");
    (Name "mem", [Set check_mem_per_packet; Clear parse_by_flow], [], "Do memory testing");
  ]
and usage_info = "bench-?pac [options] <trace_data.pcap>"
and descr = "FlowSifter Simulation library"
and notes = "by Eric Norige"

let () = Arg2.parse args (fun x -> fns := x :: !fns) usage_info descr notes

(*** PARSER MODULES ***)

type 'a parser_f = {
  id : string;
  new_parser : unit -> 'a;
  delete_parser : 'a -> unit;
  add_data : 'a -> string -> unit;
(*  get_event_count : unit -> int; *)
}

let parser_count = ref 0

let null_p = {
  id = "null";
  new_parser = (fun () -> incr parser_count);
  delete_parser = (fun () -> decr parser_count);
  add_data = (fun () _ -> ());
(*  get_event_count = (fun () -> 0); *)
}
  
let trace_len = ref (-1)
  
let sift_p = {
  id = "sift";
  new_parser = Prog_parse.new_parser "spec.ca" !extr_ca;
  delete_parser = Prog_parse.delete_parser;
  add_data = Prog_parse.add_data;
(*  get_event_count = Prog_parse.get_event_count; *)
}
    
let () = at_exit (fun () -> 
    (*    Printf.printf "#Bytes skip()ed: %d  Times this exceeded the current packet: %d bytes asked to skip trans-packet: %d\n" !Ns_parse.inner_skip !Ns_parse.skip_over !Prog_parse.total_skip; 
	  Printf.printf "#Packets skipped entirely: %d Packets skipped partially: %d\n" !Prog_parse.pkt_skip !Prog_parse.pkt_partial_skip;
	  Printf.printf "#Skip missed: %d\n" !Prog_parse.skip_missed; *)
  Printf.printf "#Trace payload length: %d Bytes parsed: %a Percentage of flow parsed: %2.1f\n" !trace_len Ean_std.print_size_B !Ns_parse.parsed_bytes (100. *. float !Ns_parse.parsed_bytes /. float !trace_len)
)
  

let pac_p = {
  id = "pac";
  new_parser = Anypac.new_parser;
  delete_parser = Anypac.delete_parser;
  add_data = Anypac.add_data;
(*  get_event_count = Anypac.get_event_count; *)
}

(* let is_http ((_sip, _dip, sp, dp),_,_) = sp = 80 || dp = 80 *)

(*** HELPER FUNCTIONS ***)

let list_avg l = if l = [] then failwith "list_avg: empty list" else List.reduce (+.) l /. (List.length l |> float) 

let vm_size_fn = 
  let pid = Unix.getpid () in
  sprintf "/proc/%d/stat" pid

let get_vmsize () =
  let stat_data = File.with_file_in vm_size_fn (IO.read_all) in
  let fields = String.nsplit stat_data " " in
  if List.length fields > 22 then
    List.nth fields 22 |> int_of_string
  else
    failwith "Couldn't read stat data"


let get_ocaml_mem () = Gc.full_major(); 8 * (Gc.stat ()).Gc.live_words

(*** WRAPPER FOR PARSERS TO HANDLE PACKETS AND FLOWS ***)

let conc_flows = ref 0
let packet_ctr = ref 0

let mem0 = ref 0
let mem0gc = ref 0

let mem () = max (get_vmsize () - !mem0) (get_ocaml_mem() - !mem0gc)

let run_id = ref ""

let run pr = 

  let ht = Hashtbl.create 1000 in
  let mem_ctr = ref (Ean_std.make_counter 1) in

  let post_round round time =
    Hashtbl.iter (fun _ p -> pr.delete_parser p; decr conc_flows) ht;
    Hashtbl.clear ht; assert (!parser_count = 0);
    if (!conc_flows <> 0) then (
      printf "#ERR: conc_flows = %d after cleanup\n" !conc_flows;
      conc_flows := 0
    ); 
    Gc.compact ();
    packet_ctr := 0;
    mem_ctr := Ean_std.make_counter 1;
    if !check_mem_per_packet then printf "#";
    printf "%s\t%s\t%d\t%4.3f\t%d\n%!" !run_id pr.id round time !trace_len
  in

  let act_packet (flow, data, fin) =
    let p = 
      try Hashtbl.find ht flow 
      with Not_found -> 
	incr conc_flows;
	pr.new_parser () |> tap (Hashtbl.add ht flow) 
    in
    incr packet_ctr;
    pr.add_data p data;
    if !check_mem_per_packet && !packet_ctr land 0xff = 0 then (
      printf "%s %s %d %d %d\n" !run_id pr.id !packet_ctr !conc_flows (mem ());
    );
    if fin then (
      decr conc_flows;
      Hashtbl.remove ht flow; pr.delete_parser p;
    )
  in	
  post_round, act_packet
;;


(*** RUN FUNCTION IN A LOOP AND INSTRUMENT ***)
let main_loop (post_round, f) rep_cnt xs =
  mem0 := get_vmsize (); mem0gc := get_ocaml_mem ();
  let tpre = Sys.time () in
  for round_num = 0 to rep_cnt do
    Vect.iter f xs;
  done;
  let time = (Sys.time () -. tpre) /. float rep_cnt in
  post_round rep_cnt time;
  time

let print_header () = Gc.compact()
(*  let t0 = Sys.time () in*)
 (* printf "# Init time: %4.2f\n" t0; *)
(*  printf "parser\tround\ttime\tmem\tparsers\tleak\n%!" *)

(*** PARSER STRUCTURE ***)
let get_fs = function
  | `Null -> run null_p
  | `Sift -> run sift_p
  | `Pac -> run pac_p

let expand_dirs e = 
  let rec exp fn = 
    if Sys.is_directory fn then 
      Sys.files_of fn |> Enum.map (fun x -> if String.tail x 1 = "/" then fn ^ x else fn ^ "/" ^ x) |> Enum.map exp |> Enum.flatten
    else 
      Enum.singleton fn 
  in
  Enum.map exp e |> Enum.flatten

module P = PathGen.OfString
let filename pathfile = P.of_string pathfile |> P.name 

(*** MAIN ***) 
let main () = 
  run_id := if !mode = Gen then 
      sprintf "gen_%d_%d" !min_lev !gen_count 
    else 
      String.concat "," (List.map filename !fns);
(*  let fns = List.enum !fns |> expand_dirs in *)
  let main_loops chunks =
    if Vect.is_empty chunks then failwith "No packets to parse";
    let main_null = get_fs `Null |> main_loop in
    let main_others = List.map (get_fs |- main_loop) !parsers in
    print_header ();
    let a = 
      if !baseline && not !check_mem_per_packet 
      then main_null !rep_cnt chunks 
      else 0.
    in
    let b = List.map (fun f -> f !rep_cnt chunks) main_others in
    a,b
  in
  let gen_pkt () = 
    let b = Buffer.create 2000 in 
    Genrec.output_soap !min_lev (Buffer.add_string b); 
    Buffer.contents b 
  in
  let packets = 
    match !parse_by_flow, !mode with
      |	true, Pcap ->  List.enum !fns |> Pcap.assemble trace_len
      | false, Pcap -> List.enum !fns |> Pcap.pre_parse trace_len 
      | true, Mux -> List.enum !fns |> Pcap.make_flows trace_len
      | false, Mux ->  List.enum !fns |> Pcap.make_packets_files trace_len
      | true, Gen -> Enum.from gen_pkt |> Enum.take !gen_count |> Pcap.make_flows trace_len
      | false, Gen -> Enum.from gen_pkt |> Enum.take !gen_count |> Pcap.make_packets trace_len
  in
  let _tnull,_ts = main_loops packets in
(*
  let rates = List.map (fun t -> float !trace_len /. (t -. tnull) /. 1024. /. 1024. *. 8.) ts in
  List.iter2 (fun p rs -> 
    printf "#%s rates (mbps): %a\n" (p_to_string p) Float.print rs) !parsers rates;
  (match rates with [_] -> () | [fr;pr] -> printf "#speed ratio: %4.2f\n" (fr /. pr) | _ -> printf "#wrong number of rates\n");
  Printf.printf "#completed in %4.2fs, (sift: %d pac: %d ) events\n" 
    (Sys.time ()) (Prog_parse.get_event_count ()) (Anypac.get_event_count ());
*)
  ()

let () = main ()
