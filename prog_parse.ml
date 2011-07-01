open Batteries
open Ns_types
open Simplify
open ParsedPCFG

let pkt_skip = ref 0
let fail_drop = ref 0

(*
let () = at_exit (fun () -> 
  Printf.printf "#Bytes dropped after desync: %a, total packet data: %d\n" Ean_std.print_size_B !fail_drop !pkt_data
)
  *)

(* generates an incremental parser for the language defined by a 
   protocol grammar specification and extraction language *)
let gen_parser p e = 
  let proto = Ns_parse.parse_file_as_spec p 
  and extr = Ns_parse.parse_file_as_extraction e in
  let ca,var_count = 
    Ns_parse.merge_cas ~proto ~extr 
    |> Ns_parse.regularize 
    |> Ns_parse.destring extr.start in
  let ca = Ns_parse.dechain ca 
(*    |>tap (Printf.printf "Grammar:\n%a\n" Ns_parse.print_reg_ds_ca)*)
    |> Ns_parse.flatten_priorities 
    |> Ns_run.optimize_preds 
  in
  fun () -> (* allow creating many parsers *)
    let st = {
      vars = Array.make var_count 0;
      ca=ca;
      q=(fun _ -> assert false);
      base_pos = 0;
      pos = 0;
      flow_data = "";
      fail_drop = 0;
      rerun_temp = 0;
      rerun = [];
    }
    in (* vars default to 0 *)
    let Waiting first_act = ca.(0) st in
    st.q <- first_act;
    (fun s -> let Waiting next_act = st.q s in st.q <- next_act)
    

(* helper functions for having the same interface as bpac and upac *)
let new_parser s e = gen_parser s e
let add_data p _ d = p d
let get_event_count _p = !Ns_types.matches
let reset_parser _p = assert false
let delete_parser _p = ()

(*
let _ = Callback.register "gen_parser" gen_parser
 *)
