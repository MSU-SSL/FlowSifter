open Batteries
open Printf

let () = Gc.set { (Gc.get()) with Gc.minor_heap_size = 1_000_000; } ;;

type parser = [`Null | `Sift | `Pac]
let p_to_string = function `Null -> "Null" | `Sift -> "Sift" | `Pac -> "Pac "
(*** GLOBAL CONFIGURATION ***)

let check_mem_per_packet = false
let pre_assemble = ref (not check_mem_per_packet)
let rep_cnt = ref 1
let fns = ref []
let extr_ca = ref "extr.ca"
let parsers : parser list ref = ref [`Null; `Sift; `Pac]

(*** ARGUMENT HANDLING ***)

open Arg2
let set_main x = Unit (fun () -> parsers := [x])
let args = 
  [ (Name "no-assemble", [Clear pre_assemble], [], 
     "Don't assemble the pcap files into flows");
    (Both ('a', "assemble"), [Set pre_assemble], [], 
     "Assemble the pcap files into flows");
    (Both ('n', "rep-cnt"), [Int_var rep_cnt], [],
     "Set the number of repetitions to do");
    (Both ('s', "sift"), [set_main `Sift], [], "Run the Flowsifter parser");
    (Both ('p', "pac"), [set_main `Pac], [], "Run the pac parser");
    (Both ('x', "extr"), [String_var extr_ca], [], 
     "Use the given extraction grammar");
  ]
and usage_info = "bench-?pac [options] <trace_data.pcap>"
and descr = "FlowSifter Simulation library"
and notes = "by Eric Norige"

let () = Arg2.parse args (fun x -> fns := x :: !fns) usage_info descr notes


(*** PARSER MODULES ***)

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
  include Prog_parse
  let new_parser = new_parser "spec.ca" !extr_ca
    
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

(* let is_http ((_sip, _dip, sp, dp),_,_) = sp = 80 || dp = 80 *)

(*** HELPER FUNCTIONS ***)

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

let list_avg l = if l = [] then failwith "list_avg: empty list" else List.reduce (+.) l /. (List.length l |> float) 

let get_vmsize () =
  let pid = Unix.getpid () in
  let fn = sprintf "/proc/%d/stat" pid in
  let stat_data = File.with_file_in fn (IO.read_all) in
  let fields = String.nsplit stat_data " " in
  if List.length fields > 22 then
    List.nth fields 22 
  else
    "Not enough fields in " ^ stat_data


(*** WRAPPER FOR PARSERS TO HANDLE PACKETS AND FLOWS ***)

let conc_flows = ref 0

module Run (P: Parser) = struct

  let ht = Hashtbl.create 1000
  let last_time = ref (Sys.time ())

  let post_round round time =
    Hashtbl.iter (fun _ p -> P.delete_parser p; decr conc_flows) ht;
    Hashtbl.clear ht;
(*    if (!conc_flows <> 0) then (
      printf "#ERR: conc_flows = %d after cleanup\n" !conc_flows;
      conc_flows := 0
    ); *)
(*    Gc.compact (); *)
    if not check_mem_per_packet then printf "%s\t%d\t%4.2f\n%!" P.id round time

  let act_packet (flow, data, fin) =
    let p = 
      try Hashtbl.find ht flow 
      with Not_found -> 
	if check_mem_per_packet then incr conc_flows;
	P.new_parser ()	|> tap (Hashtbl.add ht flow) 
    in
    P.add_data p data;
    if check_mem_per_packet then printf "%d,%s\n" !conc_flows (get_vmsize ());
    if fin then (
      if check_mem_per_packet then decr conc_flows;
      Hashtbl.remove ht flow; P.delete_parser p;
    )
	
  let act_flow fl = 
    let p = P.new_parser () in
    P.add_data p fl;
    P.delete_parser p
end
;;


(*** RUN FUNCTION IN A LOOP AND INSTRUMENT ***)
let main_loop post_round f rep_cnt xs =
  let ret = ref [] in
  let round_num = ref 0 in
  while incr round_num; !round_num <= rep_cnt do
    let vm_pre = get_vmsize () in
    let tpre = Sys.time () in
    Vect.iter f xs;
    let time = Sys.time () -. tpre in
    let vm_dirty = get_vmsize () and flows_cleaned = !conc_flows in
    post_round !round_num time;
    let vm_post = get_vmsize () in
    printf "#vm_pre: %s\tvm_post: %s\n" vm_pre vm_post;
    if flows_cleaned > 0 then 
      printf "#vm_dirty: %s\tconc_flows: %d\tper_flow: %d\n" vm_dirty flows_cleaned ((int_of_string vm_dirty - int_of_string vm_post) / flows_cleaned);
    ret := time :: !ret
  done;
  List.rev !ret

let print_header () = 
  Gc.compact();
  let t0 = Sys.time () in
  printf "# Init time: %4.2f\n" t0;
  printf "parser\tround\ttime\n%!"

(*** PARSER STRUCTURE ***)
let run_fs_flow = 
  [
    `Null, (let module M = Run(Null)  in main_loop M.post_round M.act_flow);
    `Sift, (let module M = Run(Flow) in main_loop M.post_round M.act_flow);
    `Pac,  (let module M = Run(Pac) in main_loop M.post_round M.act_flow);
  ]
  
let run_fs_packet = 
  [
    `Null, (let module M = Run(Null)  in main_loop M.post_round M.act_packet);
    `Sift, (let module M = Run(Flow) in main_loop M.post_round M.act_packet);
    `Pac,  (let module M = Run(Pac) in main_loop M.post_round M.act_packet);
  ]  

(*** MAIN ***) 
let main () = 
  if !fns = [] then failwith "Not enough arguments";
  let strs = List.enum !fns |> Enum.map read_file_as_str in
  let main_loops_perf pre_process acts =
    let main_null = List.assoc `Null acts in
    let main_others = List.map (fun p -> List.assoc p acts) !parsers in
    let xs,len = pre_process strs in
    trace_len := len;
    print_header ();
    let a = main_null !rep_cnt xs in
    let b = List.map (fun f -> f !rep_cnt xs) main_others in
    a,b
  in
  let main_loops_mem pre_process acts =
    [0.], List.map (fun p -> (List.assoc p acts) !rep_cnt (pre_process strs|>fst)) !parsers
  in
  let main_loops = if check_mem_per_packet then main_loops_mem else main_loops_perf in
  let tnull,ts = 
    if !pre_assemble then 
      main_loops Pcap.assemble run_fs_flow 
    else 
      main_loops Pcap.pre_parse run_fs_packet
  in
  let tnull_avg = list_avg tnull in
  let rates = List.map (List.map (fun t -> !trace_len /. (t -. tnull_avg))) ts in
  List.iter2 (fun p rs -> 
    printf "#%s rates: %a\n" (p_to_string p) (List.print Float.print) rs) !parsers rates;
  (match rates with [_] -> () | [fr;pr] -> printf "#speed ratio: %4.2f\n" (list_avg fr /. list_avg pr) | _ -> printf "#wrong number of rates\n");
  Printf.printf "#completed in %4.2fs, (sift: %d pac: %d ) events\n" 
    (Sys.time ()) (Flow.get_event_count ()) (Pac.get_event_count ());
  ()

let () = main ()
