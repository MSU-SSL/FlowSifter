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
let mux = ref false
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
    (Both ('m', "mux"), [Set mux], [], 
     "Mux the given flow files into a simulated pcap");
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

type 'a parser_f = {
  id : string;
  new_parser : unit -> 'a;
  delete_parser : 'a -> unit;
  add_data : 'a -> string -> unit;
(*  get_event_count : unit -> int; *)
}

let null_p = {
  id = "null";
  new_parser = (fun () -> ());
  delete_parser = (fun () -> ());
  add_data = (fun () _ -> ());
(*  get_event_count = (fun () -> 0); *)
}
  
let trace_len = ref (-1.)
  
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
  let trace_len = !trace_len /. 8. in
  let parsed = float !Ns_parse.parsed_bytes /. float (1024 * 1024) in
  Printf.printf "#Trace payload length: %.1f MB Bytes parsed: %.1f Percentage of flow parsed: %2.1f\n" trace_len parsed (100. *. parsed /. trace_len)
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

let run pr = 

  let ht = Hashtbl.create 1000 in

  let post_round round time =
    Hashtbl.iter (fun _ p -> pr.delete_parser p; decr conc_flows) ht;
    Hashtbl.clear ht;
(*    if (!conc_flows <> 0) then (
      printf "#ERR: conc_flows = %d after cleanup\n" !conc_flows;
      conc_flows := 0
    ); *)
(*    Gc.compact (); *)
    if not check_mem_per_packet then printf "%s\t%d\t%4.2f\n%!" pr.id round time
  in

  let act_packet (flow, data, fin) =
    let p = 
      try Hashtbl.find ht flow 
      with Not_found -> 
	if check_mem_per_packet then incr conc_flows;
	pr.new_parser () |> tap (Hashtbl.add ht flow) 
    in
    pr.add_data p data;
    if check_mem_per_packet then printf "%d,%s\n" !conc_flows (get_vmsize ());
    if fin then (
      if check_mem_per_packet then decr conc_flows;
      Hashtbl.remove ht flow; pr.delete_parser p;
    )
  in	
  let act_flow fl = 
    let p = pr.new_parser () in
    pr.add_data p fl;
    pr.delete_parser p
  in
  post_round, act_packet, act_flow

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
let get_fs = function
  | `Null -> run null_p
  | `Sift -> run sift_p
  | `Pac -> run pac_p

let act_packet (p,ap,_af) = main_loop p ap
let act_flow (p,_ap,af) = main_loop p af

let make_flows fns = fns |> Vect.of_enum |> (fun v -> v, Pcap.mbit_sizef v)

let make_packets fns =
  let flows = fns |> Enum.map Pcap.read_file_as_str |> Ean_std.number_enum in
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
    if Enum.is_empty ifp then () else (
      Enum.get flows |> Option.may (Enum.push ifp);
      let send, keep = ifp |> Enum.map split_rand |> Enum.uncombine in
      Random.shuffle send |> Array.enum |> Enum.push ret;
      Enum.filter_map identity keep |> loop
    )
  in
  loop (Enum.empty ());
  Enum.concat ret |> Vect.of_enum |> (fun v -> v, Pcap.mbit_size v)


(*** MAIN ***) 
let main () = 
  if !fns = [] then failwith "Not enough arguments";
  let fns = List.enum !fns in
  let main_loops_perf pre_process act =
    let main_null = get_fs `Null |> act in
    let main_others = List.map (get_fs |- act) !parsers in
    let xs,len = pre_process fns in
    trace_len := len;
    print_header ();
    let a = main_null !rep_cnt xs in
    let b = List.map (fun f -> f !rep_cnt xs) main_others in
    a,b
  in
  let main_loops_mem pre_process act =
    [0.], List.map (fun p -> (get_fs p |> act) !rep_cnt (pre_process fns |>fst)) !parsers
  in
  let main_loops = if check_mem_per_packet then main_loops_mem else main_loops_perf in
  let tnull,ts = 
    match !pre_assemble, !mux with
      |	true, false ->
	main_loops Pcap.assemble act_flow 
      | false, false ->
	main_loops Pcap.pre_parse act_packet
      | true, true ->
	main_loops make_flows act_flow
      | false, true -> 
	main_loops make_packets act_packet
  in
  let tnull_avg = list_avg tnull in
  let rates = List.map (List.map (fun t -> !trace_len /. (t -. tnull_avg))) ts in
  List.iter2 (fun p rs -> 
    printf "#%s rates: %a\n" (p_to_string p) (List.print Float.print) rs) !parsers rates;
  (match rates with [_] -> () | [fr;pr] -> printf "#speed ratio: %4.2f\n" (list_avg fr /. list_avg pr) | _ -> printf "#wrong number of rates\n");
  Printf.printf "#completed in %4.2fs, (sift: %d pac: %d ) events\n" 
    (Sys.time ()) (Prog_parse.get_event_count ()) (Anypac.get_event_count ());
  ()

let () = main ()
