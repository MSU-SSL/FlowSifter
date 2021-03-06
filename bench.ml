(** The main benchmarking executable to compare FlowSifter with either
    BinPAC or UltraPAC(netshield).  Lots of command-line options and
    configurable running of parsers.  *)

open Batteries
open Printf
open Ean_std

let set_mhs n = Gc.set { (Gc.get()) with Gc.minor_heap_size = n; }
let () = set_mhs 250_000;;

let is_http ((_sip, _dip, sp, dp),_,_,_) = sp = 80 || dp = 80

let pac = if exe = "./bench-bpac" then "bpac" else if exe = "./bench-upac" then "upac" else "siftc"

type parser_t = [ `Null | `Sift | `Pac ]
let p_to_string = function `Null -> "Null" | `Sift -> "Sift" | `Pac -> "Pac "

type mode = Pcap | Mux | Gen
type main = Parse | Stat | Diff of int | Dump of int | Tcam
(*** GLOBAL CONFIGURATION ***)
let check_mem_per_packet = ref false
let parse_by_flow = ref false
let rep_cnt = ref 1
let fns = ref []
let extr_ca = ref "extr.ca" (*(if pac="bpac" then "extr-b.ca" else "extr-u.ca")*)
let parsers : parser_t list ref = ref [`Sift; `Pac]
let baseline = ref true
let mode = ref Pcap
let min_lev = ref 0
let gen_flows = ref 0
let packet_skip = ref 0
let packet_limit = ref (max_int/2)
let main = ref Parse
let filter = ref (fun _ -> true)
(*** ARGUMENT HANDLING ***)

open Arg2
let set_main x = Unit (fun () -> parsers := [x])
let set_mode x = Unit (fun () -> mode := x)
let args =
  [ (Name "packets", [Clear parse_by_flow], [],
     "Pass individual packets to the parser");
    (Name "skip", [Int_var packet_skip], [], "Skip this many packets");
    (Both ('l', "limit"), [Int_var packet_limit], [], "Limit to first n packets");
    (Both ('f', "flows"), [Set parse_by_flow], [],
     "Assemble the pcap files into flows");
    (Both ('m', "mux"), [set_mode Mux], [],
     "Mux the given flow files into a simulated pcap");
    (Both ('d', "diff"), [Int (fun skip -> main := Diff skip; parse_by_flow := true)], [],
     "Compare flowsifter with *PAC on events");
    (Name "stat", [Unit (fun () -> main := Stat)], [],
     "Print flow statistics on the input packets only");
    (Both ('g', "gen"), [set_mode Gen; Int_var min_lev; Int_var gen_flows], [],
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
    (Name "dump", [Int (fun i -> main := Dump i)], [], "Dump packet i to stdout and exit");
    (Name "tcam", [Unit (fun () -> main := Tcam)], [], "Generate tcam ruleset counts");
    (Name "filter-http", [Unit (fun () -> filter := is_http)], [], "Filter input to HTTP flows only");
  ]
and usage_info = "bench-?pac [options] <trace_data.pcap>"
and descr = "FlowSifter Simulation library"
and notes = "by Eric Norige"

let () = Arg2.parse args (fun x -> fns := x :: !fns) usage_info descr notes

(*** PARSER MODULES ***)

open Anypac

type 'a parser_f = {
  p_type : parser_t;
  id : string;
  new_parser : unit -> 'a;
  delete_parser : 'a -> unit;
  add_data : 'a -> direction -> string -> unit;
  get_event_count : unit -> int;
}

let null_p = {
  p_type = `Null;
  id = "null";
  new_parser = (fun () -> ());
  delete_parser = (fun () -> ());
  add_data = (fun () _ _ -> ());
  get_event_count = (fun () -> 0);
}

let trace_len = ref (-1)

let sift_p = {
  p_type = `Sift;
  id = "sift"; (*(if pac = "bpac" then "sift-b" else "sift-u");*)
  new_parser = Prog_parse.new_parser "http.pro" !extr_ca;
(*  new_parser = Prog_parse.new_parser "dns.pro" "dns.ext"; *)
  delete_parser = Prog_parse.delete_parser;
  add_data = Prog_parse.add_data;
  get_event_count = Prog_parse.get_event_count;
}
(*
let () = at_exit (fun () ->
    (*    Printf.printf "#Bytes skip()ed: %d  Times this exceeded the current packet: %d bytes asked to skip trans-packet: %d\n" !Ns_parse.inner_skip !Ns_parse.skip_over !Prog_parse.total_skip;
	  Printf.printf "#Packets skipped entirely: %d Packets skipped partially: %d\n" !Prog_parse.pkt_skip !Prog_parse.pkt_partial_skip;
	  Printf.printf "#Skip missed: %d\n" !Prog_parse.skip_missed; *)
  Ns_parse.parsed_bytes := !Ns_parse.parsed_bytes / !rep_cnt;
  Printf.printf "#Trace payload length: %d Bytes parsed: %a Percentage of flow parsed: %2.1f\n" !trace_len Ean_std.print_size_B !Ns_parse.parsed_bytes (100. *. float !Ns_parse.parsed_bytes /. float !trace_len )
)
 *)

(*
let siftc_p = {
  p_type = `Sift;
  id = "siftc";
  new_parser = Siftc.new_parser;
  delete_parser = Siftc.delete_parser;
  add_data = Siftc.add_data;
  get_event_count = Siftc.get_event_count;
}
  *)

let pac_p = {
  p_type = `Pac;
  id = pac;
  new_parser = Anypac.new_parser;
  delete_parser = Anypac.delete_parser;
  add_data = Anypac.add_data;
  get_event_count = Anypac.get_event_count;
}

(*** HELPER FUNCTIONS ***)

let list_avg l = if l = [] then failwith "list_avg: empty list" else List.reduce (+.) l /. (List.length l |> float)
(*
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
*)

external get_vmsize : unit -> int = "get_vmsize"

let get_ocaml_mem () = Gc.full_major(); 8 * (Gc.stat ()).Gc.live_words

(*** WRAPPER FOR PARSERS TO HANDLE PACKETS AND FLOWS ***)

let packet_ctr = ref 0

let mem0 = ref 0
let mem0gc = ref 0

let mem_vm () = (get_vmsize () - !mem0)
let mem_gc () = (get_ocaml_mem() - !mem0gc)

let run_id = ref ""

let ediff = ref 0


let is_printable c = c = '\n' || (Char.code c >= 32 && Char.code c <= 126)
let clean_unprintable s = String.map (fun x -> if is_printable x then x else '.') s
let print_ip oc x =
  let x = (Int32.to_int x) in
  fprintf oc "%d.%d.%d.%d" (x lsr 24 land 255) (x lsr 16 land 255) (x lsr 8 land 255) (x land 255)
let print_flow oc (a,b,c,d) = fprintf oc "(%a,%a,%d,%d)" print_ip a print_ip b c d
let get_dir (_,_,sp,_dp) = if sp = 80 then Downflow else Upflow


let run pr =

  let ht = Hashtbl.create 1000 in
  let mem_ctr = ref (make_counter 1) in
  let null_t = ref 0.0 in
  let last_events = ref 0 in

  let mem = match pr.p_type with `Sift | `Null -> mem_gc | `Pac -> mem_vm in

  let reset_parsers () =
    Hashtbl.iter (fun _ p -> pr.delete_parser p) ht;
    Hashtbl.clear ht;
  in

  let post_round round time =
    packet_ctr := 0;
    mem_ctr := make_counter 1;
    let tsize = float (!trace_len * 8) /. 1024. /. 1024. /. 1024. in
    let gbps =
      if pr.p_type = `Null
      then ( null_t := time; tsize /. time )
      else tsize /. (time -. !null_t)
    in
    let pct_parsed = match pr.p_type with `Null -> 0. | `Pac -> 100.
      | `Sift -> (1 (*!Ns_run.parsed_bytes*) / (!rep_cnt + 2) * 100) /! !trace_len in
    let dropped = match pr.p_type with `Null -> 100. | `Pac -> 0.
      | `Sift -> (1 (*!Prog_parse.fail_drop*) / (!rep_cnt + 2) * 100) /! !trace_len in
    if !check_mem_per_packet then printf "#";
    printf "%s\t%s\t%d\t%4.3f\t" !run_id pr.id round time;
    printf "%d\t%.2f\t%d\t%d\t%d\t%.1f\t%.1f\n%!"
      !trace_len gbps (mem()) (Hashtbl.length ht) (pr.get_event_count()) pct_parsed dropped
  in

  let act_packet (flow, data, fin, _off) =
    incr packet_ctr;
    let dir = get_dir flow in
    if Ns_types.debug_ca && (match !main with Diff _ -> false | _ -> true) then
      Printf.printf "\nP%a:\n%S\n" print_flow flow data;
    (try
       let ctx = Hashtbl.find ht flow in
       pr.add_data ctx dir data;
       if fin then ( Hashtbl.remove ht flow; pr.delete_parser ctx; )
     with Not_found ->
       let ctx = pr.new_parser () in
       pr.add_data ctx dir data;
       if fin then pr.delete_parser ctx
       else Hashtbl.add ht flow ctx
    );
    if !check_mem_per_packet && !mem0 > 0 then (
      printf "%s %s %d %d\n" !run_id pr.id !packet_ctr (mem ());
    );
    let events = pr.get_event_count () in
    ediff := events - !last_events;
    last_events := events;
  in

  reset_parsers, post_round, act_packet
;;


(*** RUN FUNCTION IN A LOOP AND INSTRUMENT ***)
let main_check_mem (_reset_parsers, _post_round, f) _rep_cnt xs =
  Gc.compact ();
  mem0gc := get_ocaml_mem (); mem0 := get_vmsize ();
  Array.iter f xs

let main_debug (reset_parsers, post_round, f) rep_cnt xs =
  Gc.compact ();
  Array.iter f xs;
  printf "\n";
  post_round rep_cnt 0.;
  reset_parsers ()

let main_loop (reset_parsers, post_round, f) rep_cnt xs =
  Array.iter f xs;
  reset_parsers ();
  let tpre = Sys.time () in
  for round_num = 1 to rep_cnt do
    Array.iter f xs;
    reset_parsers ();
  done;
  let time = (Sys.time () -. tpre) /. float rep_cnt in
  Gc.compact ();
  mem0gc := get_ocaml_mem (); mem0 := get_vmsize ();
  Array.iter f xs;
  post_round rep_cnt time;
  reset_parsers ()


(*** PARSER STRUCTURE ***)
let get_fs = function
  | `Null -> run null_p
  | `Sift -> run sift_p
  | `Pac -> run pac_p
(*  | `Sift2 -> run siftc_p *)

module String = struct
  include String
  let head ~len str = head str len
end

let diff_loop ?(start_i=0) flows =
  printf "Diff_loop: packet count=%d\n" (Array.length flows);
  let diffs = ref 0 in
  let close_count = ref 0 in
  let (_,_,act_packet_sift) = get_fs `Sift and (_,_,act_packet_pac) = get_fs `Pac in
  for i = start_i to Array.length flows - 1 do
    let run_sift, run_pac =
      if !parse_by_flow then
        (fun () -> act_packet_sift flows.(i)),
        (fun () -> act_packet_pac flows.(i))
      else
        let packets = Pcap_parser.packetize_flow flows.(i) in
        (fun () -> List.iter act_packet_sift packets),
        (fun () -> List.iter act_packet_pac packets)
    in
    run_sift ();
    let sift_ediff = !ediff in
    run_pac ();
    let group, pre =
      if abs (!ediff - sift_ediff) > 4 then ( incr diffs; "WRONG", "WP" )
      else if !ediff <> sift_ediff then ( incr close_count; "CLOSE", "CP" )
      else "OK", "OP"
    in
    let (flow, data, _fin, off) = flows.(i) in
    if (!ediff <> sift_ediff) then (
      printf "*****************************************************\n";
      printf "%s: Sift: %d events, %s: %d events pos:%d \n" group sift_ediff pac !ediff i;
      printf "*****************************************************\n";
      printf "%s%a@%d:\n%s\n%!" pre print_flow flow off (data |> String.head ~len:1024 |> clean_unprintable);
      printf "*****************************************************\n\n";
    )
  done;
  let float_count = float (Array.length flows) in
  (*  printf "Packets with more than two events difference:\n%a\n" (List.print Int.print) (List.rev !wrongs); *)
  printf "Total different flows: %d (%.2f%%) %d close (%.2f%%)\n" !diffs (100. *. float !diffs /. float_count) !close_count (100. *. float !close_count /. float_count)

let expand_dirs e =
  let rec exp fn =
    if Sys.is_directory fn then
      Sys.files_of fn |> Enum.map (fun x -> if String.tail x 1 = "/" then fn ^ x else fn ^ "/" ^ x) |> Enum.map exp |> Enum.flatten
    else
      Enum.singleton fn
  in
  Enum.map exp e |> Enum.flatten

module P = Incubator.PathGen.OfString
let filename pathfile = P.of_string pathfile |> P.name

let gen_pkt () =
  let b = Buffer.create 2000 in
  Genrec.output_soap !min_lev (Buffer.add_string b);
  Buffer.contents b

let trace_size a =
  Array.fold_left (fun acc (_,x,_,_) -> acc + String.length x) 0 a

let empty_usage () =
  Arg2.usage ~keywords:args ~usage:usage_info ~descr ~notes;
  exit 1



(*** MAIN ***)
let main () =
  run_id := if !mode = Gen then
      sprintf "Soap %d" !min_lev
    else
      String.concat "," (List.map filename !fns);
  let gen_count = !packet_skip + !packet_limit in
  if !mode <> Gen && !fns = [] then empty_usage ();
  let packet_enum =
    match !parse_by_flow, !mode with
      |	true, Pcap ->  List.enum !fns |> Pcap_parser.assemble gen_count
      | false, Pcap -> List.enum !fns |> Pcap_parser.pre_parse
      | true, Mux ->   List.enum !fns |> Pcap_parser.make_flows_files
      | false, Mux ->  List.enum !fns |> Pcap_parser.make_packets_files
      | true, Gen ->   Enum.from gen_pkt |> Enum.take !gen_flows |> Pcap_parser.make_flows
      | false, Gen -> Enum.from gen_pkt |> Enum.take !gen_flows |> Pcap_parser.make_packets
  in
  let packets = packet_enum |> Enum.filter !filter |> Enum.skip !packet_skip |> Enum.take !packet_limit |> Array.of_enum in
  if packets = [| |] then failwith "No packets";
  trace_len := trace_size packets;
  let run l p = l (get_fs p) !rep_cnt packets in
  match !main with
    | Tcam ->
      let entries = Prog_parse_vs.gen_tcam_ruleset ~boost:1 ~stride:1 "http.pro" "extr.ca" in
      Array.iter (fun ((caq,pchk,rs), _tcam_qs) -> Printf.printf "CA state: %d pred: %a TCAM Cost: TODO Rules:\n%a\n\n" caq Ns_run.print_predchk pchk (List.print Ns_parse.print_opt_rule) rs) entries;
      Printf.printf "Total TCAM Entries: %d\n" (Array.enum entries |> map (snd %> Array.enum %> map Vect.length) |> Enum.flatten |> reduce (+))
    | Stat  -> printf "%s: %d packets, total length %d\n" !run_id (Array.length packets) !trace_len
    | Diff n -> diff_loop ~start_i:n packets
    | Dump p_num ->
      let (_,kapack,_,_) = packets.(p_num) in print_string kapack
    | _ when !check_mem_per_packet ->
      List.iter (run main_check_mem) !parsers
    | _ when Ns_types.debug_ca ->
      List.iter (run main_debug) !parsers
    | Parse ->
(*      printf "#run_id\tpr.id\tround\ttime\tlen\tgbps\tmem\tc.flows\tevents\tpct_parsed\tdropped\n"; *)
      if !baseline then run main_loop `Null; List.iter (run main_loop) !parsers

let () = main ()
