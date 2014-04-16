(** Quick program for benchmarking performance of parsing PCAP files
    into packets, not more than that *)

open Batteries

open Ean_std
open Prog_parse_vs

let main_pcap fn rep_cnt =
  let get_parser,del_parser = cache (fun _ -> new_parser ()) 1000 in
  let add_pkt (flow, data, fin) =
    let f_parser = get_parser flow in
    f_parser data;
    if fin then del_parser flow;
  in
  let x = ref rep_cnt in
  while !x <> 0 do
    Pcap.to_pkt_stream fn |> Enum.iter add_pkt;
    decr x;
  done;
  ()
  
let () = 
  let num = int_of_string Sys.argv.(1) in
  let fn = Sys.argv.(2) in
  if num > 0 then (* num is thread count, run forever *)
    let tid = Thread.create (main_pcap fn) (-1) in
    for i = 2 to num do
      Thread.create (main_pcap fn) (-1) |> ignore;
    done;
    Thread.join tid
  else (* num is rep_cnt, process fn that many times *)
    let num = ~- num in
    main_pcap fn num


let () = Printf.printf "completed in %4.2fs\n" (Sys.time ())

