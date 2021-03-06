(************************************************************************)
(** Routines to output a C++ program that flowsifter-parses a pcap file *)
(************************************************************************)

open Batteries
open Printf
open Ns_types
open ParsedPCFG

let debug = false
let starts = false
let print_matches = false

(* the user-defined functions that consume input, and might need resumption *)
let consuming_functions = [| "getnum"; "gethex" |]
let act_resumes : string list ref = ref []

let rec is_consuming_act = function
  | Plus (a,b) | Sub (a,b) | Multiply (a,b) | Divide(a,b) ->
    is_consuming_act a || is_consuming_act b
  | Function (n, _, _) -> Array.mem n consuming_functions
  | Variable | Constant _ -> false

let is_consuming_vact (_,act) = is_consuming_act act

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

let declare_vars dfa_count oc var_count =
  let say x = fprintf oc (x ^^ "\n") in
  fprintf oc "//Global values\n";
  declare_uint oc "matches=0";
  declare_uint oc "flows=0";
  declare_uint oc "skip_b=0";
  if starts then say "int dfa_starts[%d] = {0};" dfa_count;
  fprintf oc "#define PRI_DEF(dfa) ((dfa_pri##dfa[0] > 0x80) ? dfa_pri##dfa[0] & 0x7f : 0)\n";
  fprintf oc "//Structure that holds the parsing state for each flow\n";
  fprintf oc "struct state {\npublic:\n";
  fprintf oc "//CA counters\n";
  for i = 0 to var_count - 1 do
    declare_uint oc ("v" ^ string_of_int i);
  done;
  fprintf oc "//State of flow: position, data, length\n";
  declare_uint oc "base_pos"; (* offset within flow of current flow_data start *)
  declare_uint oc "fdpos"; (* position within current flow data *)
  fprintf oc "  const unsigned char* flow_data;\n";
  fprintf oc "  size_t flow_data_length;\n"; (* length of flow data *)
  fprintf oc "//Temp variable for resuming functions between packets\n";
  declare_uint oc "rerun_temp";
  fprintf oc "//LPDFA state\n";
  declare_uint oc "dfa_best_pri";
  declare_uint oc "dfa_best_q";
  declare_uint oc "dfa_best_pos";
  declare_uint oc "dfa_pri";
  declare_uint oc "dfa_q";
  fprintf oc "  void (state::*q)();\n";
  fprintf oc "  state() : ";
  for i=0 to var_count-1 do
    fprintf oc "v%d(0), " i;
  done;
  fprintf oc "base_pos(0), fdpos(0), flow_data(NULL), flow_data_length(0), rerun_temp(0), dfa_best_pri(PRI_DEF(0)), dfa_best_q(0), dfa_best_pos(0), dfa_pri(PRI_DEF(0)), dfa_q(0), q(&state::CA0) {flows++;}\n";
  fprintf oc "  void reset() { base_pos = 0; fdpos=0; flow_data=NULL; flow_data_length=0; rerun_temp=0; dfa_best_pri=PRI_DEF(0); dfa_best_q=0; dfa_best_pos=0; dfa_pri=PRI_DEF(0); dfa_q=0; q=&state::CA0; flows++; }\n";
  ()

let print_builtins oc =
  let say x = fprintf oc (x ^^ "\n") in
  say "  int pos() { return fdpos; }";
  say "  int drop_tail() { fdpos += 9999999; return fdpos; }";
  say "  int skip(int i) { skip_b += i; fdpos += i; return fdpos; }";
  say "  int skip_to(int i) { skip_b += (i - fdpos); fdpos = i; return i; }";
  say "  int notify(int i) { matches++; return 0; }";
  say "  int cur_byte() { return flow_data[fdpos]; }";
  say "  int cur_double_byte() { return 20; } //FIXME";
  say "
  int getnum() {
    int acc = rerun_temp;
    while (fdpos < flow_data_length &&
           flow_data[fdpos] >= '0' && flow_data[fdpos] <= '9') {
      acc = acc * 10 + (flow_data[fdpos] - '0');
      fdpos++;
    }
    if (fdpos >= flow_data_length) {
      rerun_temp = acc; throw 0; //FIXME ACROSS PACKETS
    } else {
      rerun_temp = 0; return acc;
    }
  } ";
  say "
  int gethex() {
    int acc = rerun_temp;
    while (fdpos < flow_data_length &&
           (flow_data[fdpos] >= '0' && flow_data[fdpos] <= '9') ||
           (flow_data[fdpos] >= 'a' && flow_data[fdpos] <= 'f') ||
           (flow_data[fdpos] >= 'A' && flow_data[fdpos] <= 'F')) {
      char val = (flow_data[fdpos] >= '0' && flow_data[fdpos] <= '9') ? flow_data[fdpos] - '0' : (flow_data[fdpos] >= 'a' && flow_data[fdpos] <= 'f') ? flow_data[fdpos] - 'a' + 10 : flow_data[fdpos] - 'A' + 10;
      acc = acc * 16 + val;
      fdpos++;
    }
    if (fdpos >= flow_data_length) {
      rerun_temp = acc; throw 0;
    } else {
      rerun_temp = 0;
      return acc;
    }
  }  //FIXME ACROSS PACKETS";
  if print_matches then
    say "  int token(int start_pos) { matches++; printf(\"T:%%d-%%d: \\\"%%.*s\\\" \\n \", start_pos, fdpos, fdpos-start_pos+2, flow_data + start_pos - 1); return 0; }"
  else
    say "  int token(int start_pos) { matches++; return 0; }";
  say "  int bounds(int start_pos, int end_pos) { matches++; return 0; }";
  ()
    (*TODO MORE BUILTINS *)

let print_act_raw oc (var, act) =
  fprintf oc "v%d = %a; " var (print_aexp var) act

let print_act oc (var, act) =
(*  if is_consuming_act act then (* need to add exception handler and resume function *)
    let res_name = "actres_" ^ string_of_int (List.length !act_resumes) in
    fprintf oc "try { %a } catch (int) { q = state::%s; }"

  else *)
    print_act_raw oc (var, act);
  if debug then fprintf oc "printf(\"v%d:=(%a)==%%d \", v%d); " var (print_aexp var) act var

let print_acts oc acts =
  List.print print_act ~first:"" ~last:"" ~sep:"" oc acts

let print_pred oc idx preds =
  if preds = [] then "1" else
    let print_expr oc ps = List.print (fun oc (v,pe) -> print_pexp v oc pe) ~first:"(" ~last:")" ~sep:" && " oc ps in
    fprintf oc "    bool p%d = %a;\n" idx print_expr preds;
    if debug then fprintf oc "    printf(\"p%d=%%d \",p%d);\n" idx idx;
    sprintf "p%d" idx

let print_1pred oc idx (v,pe) =
  fprintf oc "    bool p%d = %a;\n" idx (print_pexp v) pe;
  if debug then fprintf oc "    printf(\"p%d=%%d \",p%d);\n" idx idx;
  sprintf "p%d" idx

let print_caref oc q =
  if q = -1 then fprintf oc "NULL" else fprintf oc "&state::CA%d" q

let rec print_act_fun oc (acts,qnext) id =
  let say x = fprintf oc (x ^^ "\n") in
  say "void Act%d() {" id;
  say "  try {";
  say "    %a return state::CA%d();" print_acts acts qnext;
  say "  } catch (int) { q=&state::Act%d; }" id;
  say "}\n"

(* instantiations of actions *)
type dec = (int * Ns_types.ParsedPCFG.a_exp) list * int
let act_ht : (dec, int) Hashtbl.t = Hashtbl.create 20

let actid d =
  try Hashtbl.find act_ht d
  with Not_found ->
    Hashtbl.length act_ht
    |> tap (Hashtbl.add act_ht d)

open Regex_dfa

let merge_dec (_act1,nt1 as a) (_act2, nt2 as b) = if nt1 > nt2 then a else b
let dec_ops = {dec0 = ([],-1); merge = merge_dec; cmp = compare}

type dfa = (unit, int array, dec) fa
type regex = dec Minreg.t
let is_accepting = function ([], -1) -> false | _ -> true
let is_accepting_q q = if is_accepting q.dec then 0x80 else 0
let print_dec oc (al,qn) =
  List.print ~first:"" ~sep:" " ~last:"" print_act_raw oc al;
  fprintf oc "Q%d" qn
let print_regex oc rx = Minreg.printdp print_dec oc rx

let dfa_type dfa =
  if Array.length dfa.qs < 256 then "unsigned char"
  else if Array.length dfa.qs < 1 lsl 16 then "unsigned short"
  else "unsigned int"

let dfa_type_max dfa = match Array.length dfa.qs with x when x < 255 -> "0xff" | x when x < 1 lsl 16 - 1 -> "0xffff" | _ -> "0xffffffff"

let print_dfa_table oc dfa ((regex: regex), id) =
  let qmax = dfa_type_max dfa in
  let safe_print oc x = if x = ~-1 then String.print oc qmax else Int.print oc x in
  let print_tr oc q i qn =
    safe_print oc qn;
    if i != Array.length q.map - 1 then IO.write oc ',';
    if i mod 16 = 15 then IO.write oc '\n'
  in
  let print_trs oc q = Array.iteri (print_tr oc q) q.map; IO.write oc '\n' in
  let print_pri oc q =
    assert(q.pri < 127); Int.print oc (is_accepting_q q lor q.pri)
  in
  let print_vect oc printer = (* prints something for each dfa state *)
    (* IO.nwrite oc "{}" *)
    Array.print ~first:"{\n" ~last:"}" ~sep:"," printer oc dfa.qs
  in

  let num_states = Array.length dfa.qs in
  fprintf oc "// Regex: %a\n" print_regex regex;
  fprintf oc "const %s dfa_tr%d[%d] = %a;\n" (dfa_type dfa) id (num_states * 256)
    print_vect print_trs;
  fprintf oc "const unsigned char dfa_pri%d[%d] = %a;\n" id num_states print_vect print_pri

let print_dfa_fun oc (dfa: dfa) (regex,id) =
  let say x = fprintf oc (x ^^ "\n") in
  say "//RX: %a" print_regex regex;
  say "void DFA%d() {" id;
  if debug then say "  printf(\"D%d \");" id;
  if starts then say "  dfa_starts[%d]++;" id;
  say "  unsigned int dq = dfa_q;";
  say "  unsigned int dp = dfa_pri;";
  say "  unsigned int fdp = fdpos;";
  say "  while (fdp < flow_data_length) {";
  if debug then
    say "    nice(flow_data[fdp]); printf(\"q%%03dp%%1d@%%02d(%%2s) \", dq, dp, fdp, charbuf);";
  say "    dq = dfa_tr%d[(dq << 8) | flow_data[fdp]]; // load qnext" id;
  say "    fdp++;";
  say "    if (dq == %s) break; //quit if qnext = -1" (dfa_type_max dfa);
  say "    dp = dfa_pri%d[dq]; //load this state's priority" id;
  if debug then say "if (dp < dfa_best_pri) printf(\"pri_quit\\n\"); ";
  say "    if (dp < dfa_best_pri) break; // quit if low priority";
  say "    if (dp & 0x80) {  // accept state";
  if debug then say "      printf(\"pp:%%d@%%d \", dp & 0x7f, fdp);";
  say "      dfa_best_pri = dp & 0x7f; dfa_best_pos = fdp; dfa_best_q = dq;";
  say "    }";
  say "  } // no more parsing of flow - maybe run actions, maybe wait for more data";
  say "  dfa_q = dq;";
  say "  dfa_pri = dp;";
  say "  fdpos = fdp;";
  if debug then
    say "  printf(\"q%%03d@%%02d \", dfa_q, fdpos);";
  say "  if (fdpos < flow_data_length || dfa_q == %s) {" (dfa_type_max dfa);
  if debug then say "    printf(\"DBQ:%%d \", dfa_best_q);";
  say "    fdpos = dfa_best_pos;";
  say "    switch(dfa_best_q) {";
  let print_case i q =
    match q.dec with
      | acts, -1 -> (* no qnext *)
	say "    case %d: %aq=NULL; break;" i print_acts acts;
      | [], qnext -> (* no actions *)
	say "    case %d: return CA%d(); break;" i qnext;
      | acts, qnext ->
	let cons, non_cons = List.partition is_consuming_vact acts in
	match cons with
	  | [] -> (* no consuming actions; just run them and go to qnext *)
	    say "    case %d: %areturn CA%d(); break;" i print_acts acts qnext;
	  | _ -> (* goto action function after running all consuming actions *)
	    say "    case %d: %areturn Act%d(); break;" i print_acts non_cons (actid (cons, qnext));
  in
  Array.iteri print_case dfa.qs;
  say "    }";
  say "    dfa_best_q = 0; dfa_pri = 0;";
  say "    if (fdpos < 0) { printf(\"NEED LAST PACKET\\n\"); }";
  say "  } else { ";
  if debug then say "  printf(\"input break \");" else say "0;";
  say "    q=&state::DFA%d;" id;
  say "  }";
  if debug then say "  printf (\"X@%%02dp%%1d\\n\", fdpos, dfa_pri & 0x7f);";
  say "}\n"


(* instantiations of lpdfa *)
let dfa_ht : (dfa, regex * int) Hashtbl.t = Hashtbl.create 20

let dfaid regex dfa =
  try Hashtbl.find dfa_ht dfa |> snd
  with Not_found ->
    let id = Hashtbl.length dfa_ht in
    Hashtbl.add dfa_ht dfa (regex, id);
    id

let iset_ht : (string, int) Hashtbl.t = Hashtbl.create 20

let iset_to_array is = String.init 256 (fun i -> if ISet.mem i is then '1' else '0')
let isetid is =
  let arr = iset_to_array is in
  try Hashtbl.find iset_ht arr
  with Not_found -> (* insert and return length of hashtbl as id *)
    Hashtbl.length iset_ht |> tap (Hashtbl.add iset_ht arr)


let print_iset_arr oc arr id =
  let print_string_commas oc arr = List.print Char.print ~first:"" ~last:"" ~sep:"," oc (String.explode arr) in
  fprintf oc "const char iset%d[256] = {%a};\n" id print_string_commas arr


(* print code to evaluate *)
let rules_eval oc idx = function
  | [{rx=None; act; nt=None; prio=_}] ->
    fprintf oc " %a; q=NULL;" print_acts act;
  | [{rx=None; act; nt=Some nt; prio=_}] ->
    fprintf oc " %a; CA%d();" print_acts act nt;
  | rules ->
    let parsed_regex = List.enum rules
      |> Enum.map Ns_run.make_rx_pair
      |> Pcregex.rx_of_dec_strings ~anchor:true
      |> Minreg.of_reg in
    let open Minreg in
    ( match parsed_regex with
      | Concat([Kleene(Value iset); Accept((act, nt),_)],_) ->
	let isid = isetid iset in
 	fprintf oc "  {\n";
(*	fprintf oc "    char iset[256] = {%a};\n" print_iset_chars iset; *)
	fprintf oc "    for(; fdpos < flow_data_length; fdpos++) {\n";
	fprintf oc "      if (!iset%d[flow_data[fdpos]]) break;\n" isid;
	fprintf oc "    }\n";
	fprintf oc "    if (fdpos < flow_data_length) {\n";
	fprintf oc "      q=%a; %a\n" print_caref nt print_acts act;
	fprintf oc "    } else {\n";
	fprintf oc "      q=%a; //wait for more data\n" print_caref idx;
	fprintf oc "    }\n";
	fprintf oc "  }";
      | Concat([Value iset1; Kleene(Value iset2); Accept((act, nt),_)],_) ->
	let isid1 = isetid iset1 in
	let isid2 = isetid iset2 in
 	fprintf oc "  {\n";
(*	fprintf oc "    char iset1[256] = {%a};\n" print_iset_chars iset1;
 	fprintf oc "    char iset2[256] = {%a};\n" print_iset_chars iset2; *)
	fprintf oc "    if (rerun_temp != 1 && !iset%d[flow_data[fdpos]]) {\n" isid1;
	fprintf oc "      q=NULL; //parse failure\n";
	fprintf oc "    }\n";
	fprintf oc "    for(; fdpos < flow_data_length; fdpos++) {\n";
	fprintf oc "      if (!iset%d[flow_data[fdpos]]) break;\n" isid2;
	fprintf oc "    }\n";
	fprintf oc "    if (fdpos < flow_data_length) {\n";
	fprintf oc "      rerun_temp = 0; q=%a; %a\n" print_caref nt print_acts act;
	fprintf oc "    } else {\n";
	fprintf oc "      rerun_temp = 1; q=%a; //wait for more data\n" print_caref idx;
	fprintf oc "    }\n";
	fprintf oc "  }";
      | regex ->
	let dfa = regex
          |> Nfa.build_dfa ~labels:false dec_ops
	  |> Regex_dfa.minimize
	  |> Regex_dfa.to_array in
	let dfa_id = dfaid regex dfa in
	fprintf oc " dfa_q = %d;  dfa_best_pri=PRI_DEF(%d); dfa_best_q=%d; return DFA%d(); " dfa.q0.id dfa_id dfa.q0.id dfa_id
    );
    fprintf oc "/* %a */ " print_regex parsed_regex

(* print a function for a CA nonterminal *)
let ca_trans oc idx rules =
  fprintf oc "  void CA%d() {\n" idx;
  if debug then fprintf oc "    printf(\"CA%d\");\n" idx;
  if List.for_all (fun (p,_) -> List.length p = 0) rules then
    (* no predicates; go directly to CA state w/ actions or DFA*)
    List.map snd rules |> (rules_eval oc idx)
  else ( (* deal with predicates *)

    (* PRED-based tables *)
    let (preds: Ns_types.pred_arr) = Ns_run.unique_predicates rules in
    let vars = List.mapi (fun i p -> print_1pred oc i p) preds in
    fprintf oc "    int idx = 0"; (* incomplete statement *)
    List.iteri (fun i pi -> fprintf oc " | (%s << %d)" pi i) (List.rev vars);
    fprintf oc ";\n"; (* end of idx declaration *)
    if debug then fprintf oc "    printf(\" P%%x \", idx);\n";
    fprintf oc "    switch (idx) {\n";
    for i = 0 to (1 lsl (List.length preds)) - 1 do
      fprintf oc "    case %d:" i;
      Ns_run.get_pred_comb i preds rules |> (rules_eval oc idx);
      fprintf oc "break;\n";
    done;
    fprintf oc "    }\n"; (* close switch *)

(* THIS CODE IS RULE-BASED PREDICATE EVALUATION
    let vars = List.mapi (fun i (p,_) -> print_pred oc i p) rules in
    (* GENERATE IDX *)
    fprintf oc "    int idx = 0";
    List.iteri (fun i pi -> fprintf oc " | (%s << %d)" pi i) (List.rev vars);
    fprintf oc ";\n";
    if debug then fprintf oc "    printf(\" P%%x \", idx);\n";
    fprintf oc "    switch (idx) {\n";
    for i = 0 to (1 lsl (List.length rules)) - 1 do
      fprintf oc "    case %d:" i;
      Ns_run.get_comb i rules |> rules_eval oc;
      fprintf oc "break;\n";
    done;
    fprintf oc "    }\n"; (* close switch *)
*)
  );
  fprintf oc "  }\n" (* end function *)

let print_includes oc =
  let say x = fprintf oc (x ^^ "\n") in
  say "
#include <stdlib.h>
#include <stdbool.h>
#include <inttypes.h>
#include <assert.h>
#include <stdio.h>
#include <string.h> // general string handling
#include <string>
#include <vector>
#define CALL_MEMBER_FN(object,ptrToMember)  ((object).*(ptrToMember))
using namespace std;
char charbuf[3];
void nice(unsigned char c) { if (c >= 0x20 && c <= 0x7e) sprintf(charbuf, \" %%c\", c); else sprintf(charbuf, \"%%02x\", c);}
"


let gen_header oc var_count =
  declare_vars (Hashtbl.length dfa_ht) oc var_count;
  print_builtins oc



let end_parser_object oc = IO.nwrite oc "};\n"

let main p e outfile =
  printf "Compiling %s and %s to %s\n" p e outfile;
  let proto = Ns_parse.parse_file_as_spec p
  and extr = Ns_parse.parse_file_as_extraction e in
  let ca, var_count =
    Ns_parse.merge_cas ~proto ~extr
  |> Ns_parse.regularize
(*  |> tap (Printf.printf "Grammar reg:\n%a\n" Ns_parse.print_reg_ca) *)
  |> Ns_parse.destring extr.start
  in
  let ca = ca
(*  |> tap (Printf.printf "Grammar ds:\n%a\n" Ns_parse.print_reg_ds_ca) *)
  |> Ns_parse.dechain
  |> Ns_parse.flatten_priorities
  |> tap (Printf.printf "Final Grammar:\n%a\n" Ns_parse.print_reg_ds_ca_flat)
(*    |> Ns_run.optimize_preds *)
  in
(*  let oc = File.open_out (e ^ ".c") in *)
  let buf = IO.output_string () in
  let oc = File.open_out outfile in
  let grammar_str = IO.to_string Ns_parse.print_reg_ds_ca_flat ca in
  let c_comment_gram = String.nreplace ~str:grammar_str ~sub:"*/" ~by:"* /" in
  fprintf oc "/* CA for final grammar:\n%s*/\n" c_comment_gram;
  (* buffer the CA transitions *)
  Array.iteri (ca_trans buf) ca;
  (* print the C includes *)
  print_includes oc;
  (* print the DFA data tables *)
  Hashtbl.iter (print_dfa_table oc) dfa_ht;
  (* print the iset arrays for optimized DFAs *)
  Hashtbl.iter (print_iset_arr oc) iset_ht;
  (* print the CA struct header *)
  gen_header oc var_count;
  (* print the dfa handling functions *)
  Hashtbl.iter (print_dfa_fun oc) dfa_ht;
  (* print the action functions *)
  Hashtbl.iter (print_act_fun oc) act_ht;
  (* write the CA transition functions that were buffered *)
  IO.nwrite oc (IO.close_out buf);
  (* close the CA struct *)
  end_parser_object oc


(*let () = main "dyck.pro" "dyck.ext" "fs.c" *)
let () =
  if Array.length Sys.argv < 3 then
    printf "Usage: ns_compile <protocol grammar> <extraction grammar>\n"
  else
    main Sys.argv.(1) Sys.argv.(2) "fs_lib.h"

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
