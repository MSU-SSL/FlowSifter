(** Programmatic parser with variable stride (experimental).  Just
    like prog_parse.ml but tries to build a variable stride
    automaton. *)

open Batteries

open Ns_types
open Simplify
open ParsedPCFG
open Ns_parse

(** Removes predicate checks at runtime for non-terminals with no predicates *)
let optimize_preds_gen compile_ca _run_dfa ca =
  let opt_prod _idx rules =
    if List.for_all (fun (p,_) -> List.length p = 0) rules then
      List.map snd rules |> compile_ca |> ignore
    else
      if List.length rules < 20 then (*TODO: PARTITION RULES BY PREDICATE *)
	Array.init (1 lsl (List.length rules))
	  (fun ci -> Ns_run.get_comb ci rules |> compile_ca) |> ignore
      else (
	Printf.printf "Cannot optimize rules, too many rules:\n%a\n%!" (List.print ~sep:"\n" print_reg_rule) (List.map snd rules);
	exit 1;
      )
  in
  Array.mapi opt_prod ca


(* generates an incremental parser for the language defined by a
   protocol grammar specification and extraction language *)
let gen_parser ~boost ~stride p e =
   let ca_cache = ref Map.empty in
(*   let cmp = Int.compare in
   let def_dec = -1 in
   let () = at_exit (fun () ->
     Map.values !ca_cache |> Enum.map (Vsdfa.tcam_size ~cmp ~def_dec) |> Enum.reduce (+)
     |> Printf.printf "parsing automata generated: %d tcam entries\n" ) in
*)
(* (\*  fill_cache ca_cache ca; *\) *)
   let compile_ca _ rs = Ns_run.compile_ca_vs ~boost ~stride rs
     |> tap (function `Dfa d -> ca_cache := Map.add rs d !ca_cache | `Ca _ -> () )
   in

  let proto = parse_file_as_spec p
  and extr = parse_file_as_extraction e in
  let ca,_var_count = merge_cas ~proto ~extr |> regularize
    |> Ns_parse.destring extr.start in
  let _ca = Ns_parse.dechain ca
    |> Ns_parse.flatten_priorities
    |> Ns_run.optimize_preds_gen compile_ca (fun _ -> assert false)
  in
  fun () -> assert false

let new_parser = gen_parser ~boost:7 ~stride:2 "http.pro" "extr.ca"
let add_data p d = p d
let get_event_count _p = !Ns_types.matches
let reset_parser _p = assert false
let delete_parser _ = ()

(* computes all the DFAs for a tcam ruleset *)
let gen_tcam_ruleset ~boost ~stride p e =
  let dfa_cache = ref Map.empty in
  let add_cache rs = function
    | `Dfa d -> dfa_cache := Map.add rs d !dfa_cache
    | `Ca (_acts,_next) -> ()
  in
  let compile_ca (caq,p) rs = Ns_run.compile_ca_vs ~boost ~stride rs |> tap (add_cache (caq,p,rs))in
  let proto = parse_file_as_spec p
  and extr = parse_file_as_extraction e in
  let ca,_var_count = merge_cas ~proto ~extr |> regularize
    |> Ns_parse.destring extr.start in
  let _ca = Ns_parse.dechain ca
    |> Ns_parse.flatten_priorities
    |> Ns_run.optimize_preds_gen compile_ca (fun _ -> assert false)
  in
  let cmp = Int.compare in
  let def_dec = -1 in
  Map.enum !dfa_cache |> Enum.map (Tuple2.map2 (Vsdfa.tcam_rs ~cmp ~def_dec)) |> Array.of_enum

(*
let _ = Callback.register "gen_parser" gen_parser
 *)
