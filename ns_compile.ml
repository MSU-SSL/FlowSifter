open Batteries
open Printf
open Ns_types
open ParsedPCFG

(************************************************************************)
(** Routines to output a C++ program that flowsifter-parses a pcap file *)
(************************************************************************)

(* print an arithmetic expression to an output channel oc *)
let rec print_aexp v oc = function
  | Plus (a,b) -> bool_exp v oc '+' a b
  | Sub (a,b) -> bool_exp v oc '-' a b
  | Multiply (a,b) -> bool_exp v oc '-' a b
  | Divide (a,b) -> bool_exp v oc '-' a b
  | Function (name, _, aes) ->
    List.print (print_aexp v) ~first:(name ^ "(") ~last:")" ~sep:", " oc aes
  | Variable -> fprintf oc "st.v%d" v
  | Constant c -> fprintf oc "%d" c
and bool_exp v oc op a b =
  fprintf oc "(%a %c %a)" (print_aexp v) a op (print_aexp v) b

let rec print_pexp v oc = function
  | Equal (a,b) -> fprintf oc "(%a == %a)" (print_aexp v) a (print_aexp v) b
  | Lessthan (a,b) -> fprintf oc "(%a < %a)" (print_aexp v) a (print_aexp v) b
  | Greaterthan (a,b) -> fprintf oc "(%a > %a)" (print_aexp v) a (print_aexp v) b
  | LessthanEq (a,b) -> fprintf oc "(%a <= %a)" (print_aexp v) a (print_aexp v) b
  | GreaterthanEq (a,b) -> fprintf oc "(%a >= %a)" (print_aexp v) a (print_aexp v) b
  | Not a -> fprintf oc "(! %a)" (print_pexp v) a
  | And (a,b) -> fprintf oc "(%a && %a)" (print_pexp v) a (print_pexp v) b
  | Or (a,b) -> fprintf oc "(%a && %a)" (print_pexp v) a (print_pexp v) b
  | Const c -> fprintf oc "%B" c


let declare_uint oc v = fprintf oc "  unsigned int %s;\n" v

let declare_vars oc vs =
  fprintf oc "struct state {\n";
  List.print declare_uint ~first:"" ~last:"" ~sep:"" oc vs;
  declare_uint oc "base_pos";
  declare_uint oc "pos";
  fprintf oc "char* flow_data;\n";
  declare_uint oc "fail_drop";
  declare_uint oc "rerun_temp";
  fprintf oc "};\n"

let print_act oc (var, act) =
  fprintf oc "st.v%d = %a;\n" var (print_aexp var) act

let print_acts oc acts =
  List.print print_act ~first:"" ~last:"" ~sep:"\n" oc acts

let print_pred oc idx preds =
  let print_expr oc ps = List.print (fun oc (v,pe) -> print_pexp v oc pe) ~first:"(" ~last:")" ~sep:" && " oc ps in
  fprintf oc "bool p%d = %a;\n" idx print_expr preds


let merge_dec (_act1,nt1 as a) (_act2, nt2 as b) = if nt1 > nt2 then a else b
let dec_ops = {Regex_dfa.dec0 = ([],-1); merge = merge_dec; cmp = compare}

type dfa =
    (unit, int array, (int * Ns_types.ParsedPCFG.a_exp) list * int) Regex_dfa.fa

let dfa_ht : (dfa, int) Hashtbl.t = Hashtbl.create 20

let dfaid dfa =
  try Hashtbl.find dfa_ht dfa
  with Not_found ->
    (Hashtbl.length dfa_ht) |> tap (Hashtbl.add dfa_ht dfa)


(* print code to evaluate *)
let rules_eval oc = function
  | [{rx=None; act; nt; prio=_}] ->
    let qnext = Option.default (-1) nt in
    fprintf oc "st.q = CA%d;\n%abreak;\n" qnext print_acts act;
  | rules ->
    let dfa = List.enum rules
      |> Enum.map Ns_run.make_rx_pair
      |> Pcregex.rx_of_dec_strings ~anchor:true
      |> Minreg.of_reg
      |> Nfa.build_dfa ~labels:false dec_ops
      |> Regex_dfa.minimize
      |> Regex_dfa.to_array in
    fprintf oc "st.q = DFA%d;\nbreak;\n" (dfaid dfa)

(* print a function for a CA nonterminal *)
let ca_trans oc idx rules =
  fprintf oc "void CA%d(state& st) {" idx;
  if List.for_all (fun (p,_) -> List.length p = 0) rules then
    (* no predicates; go directly to CA state w/ actions or DFA*)
    List.map snd rules |> rules_eval oc
  else ( (* deal with predicates *)
    List.iteri (fun i (p,_) -> print_pred oc i p) rules;
    (* GENERATE IDX *)
    fprintf oc "switch (idx) {\n";
    for i = 0 to (1 lsl (List.length rules)) - 1 do
      fprintf oc "case %d:\n" i;
      Ns_run.get_comb i rules |> rules_eval oc;
      fprintf oc "break;\n";
    done;
    fprintf oc "}\n"; (* close switch *)
  );
  fprintf oc "}\n" (* end function *)

let main p e =
  let proto = Ns_parse.parse_file_as_spec p
  and extr = Ns_parse.parse_file_as_extraction e in
  printf "P1\n";
  let ca, _var_count =
    Ns_parse.merge_cas ~proto ~extr
    |> Ns_parse.regularize
    |> Ns_parse.destring extr.start in
  printf "P2\n";
  let ca = Ns_parse.dechain ca
(*    |>tap (Printf.printf "Grammar:\n%a\n" Ns_parse.print_reg_ds_ca)*)
    |> Ns_parse.flatten_priorities
(*    |> Ns_run.optimize_preds *)
  in
(*  let oc = File.open_out (e ^ ".c") in *)
  let oc = stdout in
  Array.iteri (ca_trans oc) ca

let () = main "http.pro" "extr.ca"

(*
int CA (state st) {
  while (st.ok && <have_data>) {
    if (st.acts != NULL) run_act(st.acts);
    switch(st.q) {
    case 0: // <NAME0>
      bool p0 = <pred_check0>;
      ...
      bool pn = <pred_checkn>;
      int idx = (((p0 *2 + p1) * 2 + p2) * 2 + p3...;
      q0t[idx](&st); // call the DFA, which will return new values
                     // for st.q and st.acts
      break;
    case 1: // <NAME1>
      q1f(&st); // direct call of DFA, no idx table
      break;
    default: st.ok = false; break;
    }
  }
}

*)