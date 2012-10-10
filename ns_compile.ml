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
  | Variable -> fprintf oc "v%d" v
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

let declare_vars oc var_count =
  declare_uint oc "matches";
  fprintf oc "struct state {\npublic:\n";
  for i = 0 to var_count - 1 do
    declare_uint oc ("v" ^ string_of_int i);
  done;
  declare_uint oc "base_pos";
  declare_uint oc "fdpos";
  fprintf oc "  unsigned char* flow_data;\n";
  fprintf oc "  size_t flow_data_length;\n";
  declare_uint oc "fail_drop";
  declare_uint oc "rerun_temp";
  declare_uint oc "dfa_best_pri";
  declare_uint oc "dfa_best_q";
  declare_uint oc "dfa_best_pos";
  declare_uint oc "dfa_pri";
  declare_uint oc "dfa_q";
  fprintf oc "  void (state::*q)();\n"


let print_builtins say =
  say "  int pos() { return fdpos; }";
  say "  int skip(int i) { fdpos += i; return fdpos; }";
  say "  int skip_to(int i) { fdpos = i; return i; }";
  say "  int notify(int i) { matches++; return 0; }";
  say "  int cur_byte() { return 30; } //FIXME";
  say "  int cur_double_byte() { return 20; } //FIXME";
  say "  int getnum() { return 10; } //FIXME";
  say "  int gethex() { return 0x0f; } //FIXME";
  say "  int token(int start_pos) { matches++; return 0; }";
  say "  int bounds(int start_pos, int end_pos) { matches++; return 0; }";
  ()
    (*TODO MORE BUILTINS *)

let print_act oc (var, act) =
  fprintf oc "v%d = %a; " var (print_aexp var) act

let print_acts oc acts =
  List.print print_act ~first:"" ~last:"" ~sep:"" oc acts

let print_pred oc idx preds =
  let print_expr oc ps = List.print (fun oc (v,pe) -> print_pexp v oc pe) ~first:"(" ~last:")" ~sep:" && " oc ps in
  fprintf oc "    bool p%d = %a;\n" idx print_expr preds;
  sprintf "p%d" idx

open Regex_dfa

let merge_dec (_act1,nt1 as a) (_act2, nt2 as b) = if nt1 > nt2 then a else b
let dec_ops = {dec0 = ([],-1); merge = merge_dec; cmp = compare}

type dfa =
    (unit, int array, (int * Ns_types.ParsedPCFG.a_exp) list * int) fa

let dfa_type _dfa =
  if Array.length _dfa.qs < 256 then "unsigned char"
  else if Array.length _dfa.qs < 1 lsl 16 then "unsigned short"
  else "unsigned int"

let print_dfa_table oc dfa id =
  let print_tr oc q = Array.print ~first:"" ~last:"\n" ~sep:"," Int.print oc q.map in
  let print_pri oc q = Int.print oc q.pri in
  let print_vect oc printer = (* prints something for each dfa state *)
    (* IO.nwrite oc "{}" *)
    Array.print ~first:"{" ~last:"}" ~sep:"," printer oc dfa.qs
  in

  let num_states = Array.length dfa.qs in
  fprintf oc "%s dfa_tr%d[%d] = %a;\n" (dfa_type dfa) id (num_states * 256)
    print_vect print_tr;
  fprintf oc "char dfa_pri%d[%d] = %a;\n" id num_states print_vect print_pri

let print_dfa oc dfa id =
  let say x = fprintf oc (x ^^ "\n") in
  say "void DFA%d() {" id;
  say "  while (fdpos < flow_data_length) {";
  say "    dfa_q = dfa_tr%d[(dfa_q << 8) | flow_data[fdpos++]];" id;
  say "    if (dfa_q == (%s)(-1)) {q = NULL; return;}" (dfa_type dfa);
  say "    dfa_pri = dfa_pri%d[dfa_q];" id;
  say "    if (dfa_pri < dfa_best_pri) break;";
  say "    if (dfa_pri > dfa_best_pri) { dfa_best_pri = dfa_pri; dfa_best_pos = fdpos; dfa_best_q = dfa_q; }";
  say "  } // no more parsing of flow - maybe run actions, maybe wait for more data";
  say "  if (fdpos < flow_data_length) {";
  say "    switch(dfa_best_q) {";
  Array.iteri (fun i q ->
               let acts, qnext = q.dec in
               if qnext != -1 then
                 say "    case %d: %aq=&state::CA%d; break;" i print_acts acts qnext) dfa.qs;

  say "    default: printf(\"No qnext!\"); // Nothing";
  say "    }";
  say "    fdpos = dfa_best_pos;";
  say "  } //done parsing this packet, wait for more data";
  say "}\n"

let dfa_ht : (dfa, int) Hashtbl.t = Hashtbl.create 20

let dfaid dfa =
  try Hashtbl.find dfa_ht dfa
  with Not_found ->
    (Hashtbl.length dfa_ht) |> tap (Hashtbl.add dfa_ht dfa)


(* print code to evaluate *)
let rules_eval oc = function
  | [{rx=None; act; nt; prio=_}] ->
    let qnext = Option.default (-1) nt in
    fprintf oc " q = &state::CA%d; %a" qnext print_acts act;
  | rules ->
    let dfa = List.enum rules
      |> Enum.map Ns_run.make_rx_pair
      |> Pcregex.rx_of_dec_strings ~anchor:true
      |> Minreg.of_reg
      |> Nfa.build_dfa ~labels:false dec_ops
      |> Regex_dfa.minimize
      |> Regex_dfa.to_array in
    fprintf oc " dfa_q = %d; dfa_best_pri = 0; q = &state::DFA%d;" dfa.q0.id (dfaid dfa)

(* print a function for a CA nonterminal *)
let ca_trans oc idx rules =
  fprintf oc "  void CA%d() {" idx;
  if List.for_all (fun (p,_) -> List.length p = 0) rules then
    (* no predicates; go directly to CA state w/ actions or DFA*)
    List.map snd rules |> rules_eval oc
  else ( (* deal with predicates *)
    let vars = List.mapi (fun i (p,_) -> print_pred oc i p) rules in
    (* GENERATE IDX *)
    fprintf oc "    int idx = 0";
    List.iteri (fun i pi -> fprintf oc " + (%s << %d)" pi i) (List.rev vars);
    fprintf oc ";\n";
    fprintf oc "    switch (idx) {\n";
    for i = 0 to (1 lsl (List.length rules)) - 1 do
      fprintf oc "    case %d:" i;
      Ns_run.get_comb i rules |> rules_eval oc;
      fprintf oc "break;\n";
    done;
    fprintf oc "    }\n"; (* close switch *)
  );
  fprintf oc "  }\n" (* end function *)

let print_includes say =
  say "#include <stdlib.h>";
  say "#include <stdbool.h>";
  say "#include <inttypes.h>";
  say "#include <assert.h>";
  say "#include <stdio.h>";
  say "#include <fcntl.h>";
  say "#include <string.h> // general string handling";
  say "#define CALL_MEMBER_FN(object,ptrToMember)  ((object).*(ptrToMember))"


let gen_header oc var_count =
  declare_vars oc var_count;
  print_builtins (fun x -> IO.nwrite oc x; IO.write oc '\n')


let print_read_file say =
  say "  void read_file(char* filename) {
    FILE* fd = fopen(filename, \"r\");

    printf(\"Subject: %s\\n\", basename(filename));
    fseek (fd , 0 , SEEK_END);
    flow_data_length = ftell (fd);
    rewind (fd);

    // allocate memory to contain the whole file:
    flow_data = (unsigned char *) malloc (sizeof(char)*flow_data_length);
    if (flow_data == NULL) {fputs (\"Memory error\",stderr); exit (2);}

    // copy the file into the buffer:
    size_t result = fread (flow_data,1,flow_data_length,fd);
    if (result != flow_data_length) {fputs (\"Reading error\",stderr); exit (3);}
  }"

let end_parser_object oc = IO.nwrite oc "};\n"

let print_main say =
  say "int main(int argc, char* argv[]) {";
  say "  if (argc != 2) { printf (\"fs [trace_file]\\n\"); exit(1); }";
  say "  state st;";
  say "  st.read_file(argv[1]);";
  say "  st.q = &state::CA0;";
  say "  while (st.q != NULL) CALL_MEMBER_FN(st, st.q)();";
  say "  return 0;";
  say "}";
  say ""


let main p e outfile =
  printf "Compiling %s and %s to %s\n" p e outfile;
  let proto = Ns_parse.parse_file_as_spec p
  and extr = Ns_parse.parse_file_as_extraction e in
  let ca, var_count =
    Ns_parse.merge_cas ~proto ~extr
    |> Ns_parse.regularize
    |> Ns_parse.destring extr.start in
  let ca = Ns_parse.dechain ca
(*    |>tap (Printf.printf "Grammar:\n%a\n" Ns_parse.print_reg_ds_ca)*)
    |> Ns_parse.flatten_priorities
(*    |> Ns_run.optimize_preds *)
  in
(*  let oc = File.open_out (e ^ ".c") in *)
  let buf = IO.output_string () in
  let oc = File.open_out outfile in
  let say x = IO.nwrite oc x; IO.write oc '\n' in
  Array.iteri (ca_trans buf) ca;
  print_includes say;
  Hashtbl.iter (print_dfa_table oc) dfa_ht; (* print the DFA data tables *)
  gen_header oc var_count;
  Hashtbl.iter (print_dfa oc) dfa_ht; (* print the dfa handling functions *)
  IO.nwrite oc (IO.close_out buf);
  print_read_file say;
  end_parser_object oc;
  print_main say



let () = main "http.pro" "extr.ca" "fs.c"

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
