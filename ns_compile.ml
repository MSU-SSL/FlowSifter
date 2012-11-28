open Batteries
open Printf
open Ns_types
open ParsedPCFG

(************************************************************************)
(** Routines to output a C++ program that flowsifter-parses a pcap file *)
(************************************************************************)

let debug = false
let print_matches = false

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
  declare_uint oc "flows";
  declare_uint oc "skip_b";
  fprintf oc "#define PRI_DEF(dfa) ((dfa_pri##dfa[0] > 0x80) ? dfa_pri##dfa[0] : 0)\n";
  fprintf oc "struct state {\npublic:\n";
  for i = 0 to var_count - 1 do
    declare_uint oc ("v" ^ string_of_int i);
  done;
  declare_uint oc "base_pos"; (* offset within flow of current flow_data start *)
  declare_uint oc "fdpos"; (* position within current flow data *)
  fprintf oc "  const unsigned char* flow_data;\n";
  fprintf oc "  size_t flow_data_length;\n"; (* length of flow data *)
  declare_uint oc "fail_drop";
  declare_uint oc "rerun_temp";
  declare_uint oc "dfa_best_pri";
  declare_uint oc "dfa_best_q";
  declare_uint oc "dfa_best_pos";
  declare_uint oc "dfa_pri";
  declare_uint oc "dfa_q";
  fprintf oc "  void (state::*q)();\n";
  fprintf oc "  state() : base_pos(0), fdpos(0), flow_data(NULL), flow_data_length(0), fail_drop(0), rerun_temp(0), dfa_best_pri(PRI_DEF(0)), dfa_best_q(0), dfa_best_pos(0), dfa_pri(PRI_DEF(0)), dfa_q(0), q(&state::CA0) {flows++;}\n"

let print_builtins say =
  say "  int pos() { return fdpos; }";
  say "  int drop_tail() { fdpos += 9999999; return fdpos; }";
  say "  int skip(int i) { skip_b += i; fdpos += i; return fdpos; }";
  say "  int skip_to(int i) { skip_b += (i - fdpos); fdpos = i; return i; }";
  say "  int notify(int i) { matches++; return 0; }";
  say "  int cur_byte() { return flow_data[fdpos]; }";
  say "  int cur_double_byte() { return 20; } //FIXME";
  say "  int getnum() { int acc = 0; while (flow_data[fdpos] >= '0' && flow_data[fdpos] <= '9') { acc = acc * 10 + (flow_data[fdpos] - '0'); fdpos++; } return acc; } //FIXME ACROSS PACKETS";
  say "
  int gethex() {
    int acc = 0;
    while ((flow_data[fdpos] >= '0' && flow_data[fdpos] <= '9') ||
           (flow_data[fdpos] >= 'a' && flow_data[fdpos] <= 'f') ||
           (flow_data[fdpos] >= 'A' && flow_data[fdpos] <= 'F')) {
      char val = (flow_data[fdpos] >= '0' && flow_data[fdpos] <= '9') ? flow_data[fdpos] - '0' : (flow_data[fdpos] >= 'a' && flow_data[fdpos] <= 'f') ? flow_data[fdpos] - 'a' + 10 : flow_data[fdpos] - 'A' + 10;
      acc = acc * 16 + val;
      fdpos++;
    }
    return acc;
  }  //FIXME ACROSS PACKETS";
  if print_matches then
    say "  int token(int start_pos) { matches++; printf(\"T:%d-%d: %.*s  \", start_pos, fdpos, fdpos-start_pos+1, flow_data + start_pos - 1); return 0; }"
  else
    say "  int token(int start_pos) { matches++; return 0; }";
  say "  int bounds(int start_pos, int end_pos) { matches++; return 0; }";
  ()
    (*TODO MORE BUILTINS *)

let print_act_raw oc (var, act) =
  fprintf oc "v%d = %a; " var (print_aexp var) act

let print_act oc (var, act) =
  print_act_raw oc (var, act);
  if debug then fprintf oc "printf(\"v%d=%a=%%d \", v%d); " var (print_aexp var) act var

let print_acts oc acts =
  List.print print_act ~first:"" ~last:"" ~sep:"" oc acts

let print_pred oc idx preds =
  if preds = [] then "1" else
    let print_expr oc ps = List.print (fun oc (v,pe) -> print_pexp v oc pe) ~first:"(" ~last:")" ~sep:" && " oc ps in
    fprintf oc "    bool p%d = %a;\n" idx print_expr preds;
    sprintf "p%d" idx

open Regex_dfa

let merge_dec (_act1,nt1 as a) (_act2, nt2 as b) = if nt1 > nt2 then a else b
let dec_ops = {dec0 = ([],-1); merge = merge_dec; cmp = compare}

type dec = (int * Ns_types.ParsedPCFG.a_exp) list * int
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

let print_caref oc q = if q = -1 then fprintf oc "&state::CA0" else fprintf oc "&state::CA%d" q
let print_dfa oc dfa (regex,id) =
  let say x = fprintf oc (x ^^ "\n") in
  say "void DFA%d() {" id;
  if debug then say "  printf(\"D%d \");" id;
  say "  while (fdpos < flow_data_length) {";
  if debug then
    say "    printf(\"q%%03dp%%1d@%%02d(%%2s) \", dfa_q, dfa_pri & 0x7f, fdpos, nice(flow_data[fdpos]));";
  say "    dfa_q = dfa_tr%d[(dfa_q << 8) | flow_data[fdpos++]]; // load qnext" id;
  say "    if (dfa_q == %s) break; //quit if qnext = -1" (dfa_type_max dfa);
  say "    dfa_pri = dfa_pri%d[dfa_q]; //load this state's priority" id;
  if debug then say "if (dfa_pri < dfa_best_pri) printf(\"RX: %a pri_quit\\n\"); " print_regex regex;
  say "    if (dfa_pri < dfa_best_pri) break; // quit if low priority";
  say "    if (dfa_pri & 0x80) {  // accept state";
  if debug then say "      printf(\"pp:%%d@%%d \", dfa_pri & 0x7f, fdpos);";
  say "      dfa_best_pri = dfa_pri & 0x7f; dfa_best_pos = fdpos; dfa_best_q = dfa_q;";
  say "    }";
  say "  } // no more parsing of flow - maybe run actions, maybe wait for more data";
  if debug then
    say "  printf(\"q%%03d@%%02d \", dfa_q, fdpos);";
  say "  if (fdpos < flow_data_length || dfa_q == %s) {" (dfa_type_max dfa);
  if debug then say "    printf(\"DBQ:%%d \", dfa_best_q);";
  say "    fdpos = dfa_best_pos;";
  say "    switch(dfa_best_q) {";
  Array.iteri (fun i q ->
               let acts, qnext = q.dec in
               if qnext = -1 then
		 say "    case %d: q=&state::CA0; dfa_best_pos++; break;" i
	       else
                 say "    case %d: %aq=%a; break;" i print_acts acts print_caref qnext)
    dfa.qs;
(*  say "    default: %s; dfa_best_pos=fdpos; q=&state::CA0; // Nothing"
    (if debug then "printf(\"No act; RESET \")" else "0"); *)
  say "    }";
  say "    dfa_best_q = 0; dfa_pri = 0;";
  say "    if (fdpos < 0) { printf(\"NEED LAST PACKET\\n\"); }";
  say "  } else { ";
  if debug then say "  printf(\"input break \");" else say "0;";
  say "  }";
  if debug then say "  printf (\"X@%%02dp%%1d\\n\", fdpos, dfa_pri & 0x7f);";
  say "}\n"

let dfa_ht : (dfa, regex * int) Hashtbl.t = Hashtbl.create 20

let dfaid regex dfa =
  try Hashtbl.find dfa_ht dfa |> snd
  with Not_found ->
    let id = Hashtbl.length dfa_ht in
    Hashtbl.add dfa_ht dfa (regex, id);
    id

(* print code to evaluate *)
let rules_eval oc = function
  | [{rx=None; act; nt; prio=_}] ->
    let qnext = Option.default (-1) nt in
    fprintf oc " q = %a; %a" print_caref qnext print_acts act;
  | rules ->
    let regex = List.enum rules
      |> Enum.map Ns_run.make_rx_pair
      |> Pcregex.rx_of_dec_strings ~anchor:true
      |> Minreg.of_reg in
    printf "Generating DFA for regex %a\n%!" print_regex regex;
    let dfa = regex
      |> Nfa.build_dfa ~labels:false dec_ops
      |> Regex_dfa.minimize
      |> Regex_dfa.to_array in
    let dfa_id = dfaid regex dfa in
    fprintf oc " dfa_q = %d; dfa_best_pri=PRI_DEF(%d); dfa_best_q=%d; q = &state::DFA%d; /* %a */ " dfa.q0.id dfa_id dfa.q0.id dfa_id print_regex regex

(* print a function for a CA nonterminal *)
let ca_trans oc idx rules =
  fprintf oc "  void CA%d() {\n" idx;
  if debug then fprintf oc "    printf(\"CA%d\");\n" idx;
  if List.for_all (fun (p,_) -> List.length p = 0) rules then
    (* no predicates; go directly to CA state w/ actions or DFA*)
    List.map snd rules |> rules_eval oc
  else ( (* deal with predicates *)
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
  );
  fprintf oc "  }\n" (* end function *)

let print_includes say =
  say "
#include <stdlib.h>
#include <stdbool.h>
#include <inttypes.h>
#include <assert.h>
#include <stdio.h>
#include <fcntl.h>
#include <string.h> // general string handling
#include <sys/time.h> // for gettimeofday
#include <pcap/pcap.h>
#include <limits.h>
#include <utility>
#include <unordered_map>
#include <netinet/if_ether.h>
#include <netinet/tcp.h>
#include <netinet/ip.h>
#define CALL_MEMBER_FN(object,ptrToMember)  ((object).*(ptrToMember))
using namespace std;
char charbuf[2];
char* nice(unsigned char c) { if (c >= 0x20 && c <= 0x7e) sprintf(charbuf, \" %c\", c); else sprintf(charbuf, \"%02x\", c); return charbuf;}
"


let gen_header oc var_count =
  declare_vars oc var_count;
  print_builtins (fun x -> IO.nwrite oc x; IO.write oc '\n')


let print_read_file say =
  say "  void read_file(char* filename) {
    FILE* fd = fopen(filename, \"r\");
";
  if debug then say "    printf(\"Subject: %s\\n\", basename(filename));";
  say "    fseek (fd , 0 , SEEK_END);
    flow_data_length = ftell (fd);
    rewind (fd);

    // allocate memory to contain the whole file:
    unsigned char* data = (unsigned char *) malloc (sizeof(char)*flow_data_length);
    if (data == NULL) {fputs (\"Memory error\",stderr); exit (2);}

    // copy the file into the buffer:
    size_t result = fread (data,1,flow_data_length,fd);
    if (result != flow_data_length) {fputs (\"Reading error\",stderr); exit (3);}
    flow_data = data;
  }
"

let end_parser_object oc = IO.nwrite oc "};\n"

(* Single-flow main *)
let print_main say =
  say "int main(int argc, char* argv[]) {";
  say "  if (argc != 2) { printf (\"fs [trace_file]\\n\"); exit(1); }";
  say "  state st;";
  say "  st.read_file(argv[1]);";
  say "  struct timeval t0,tf;";
  say "  gettimeofday(&t0, NULL);";
  say "  while (st.q != NULL && st.fdpos < st.flow_data_length) CALL_MEMBER_FN(st, st.q)();";
  say "  gettimeofday(&tf, NULL);";
  say "  double t0s = t0.tv_sec+(t0.tv_usec/1000000.0);";
  say "  double tfs = tf.tv_sec+(tf.tv_usec/1000000.0);";
  say "  double time_used = tfs - t0s;";
  say "  double gbps = ((double) st.flow_data_length) * 8 / time_used / 1000000000;";
  say "  printf(\"\\nMatches: %d Bytes: %lu Time: %.3fs Rate: %.2fGbps\\n\", matches, st.flow_data_length, time_used, gbps);";
  say "  return 0;";
  say "}";
  say ""

(* multi-flow-enabled main *)
let print_pcap_main say =
  say "
struct four_tuple {
  uint32_t src; uint32_t dest;
  uint16_t src_p; uint16_t dest_p;
  four_tuple(ip* iph, tcphdr* tcph) : src(iph->ip_src.s_addr), dest(iph->ip_dst.s_addr), src_p(tcph->source), dest_p(tcph->dest) {}
  four_tuple() : src(0), dest(0), src_p(0), dest_p(0) {}
  bool operator==(const four_tuple& ft) const { return src==ft.src && dest == ft.dest && src_p == ft.src_p && dest_p == ft.dest_p; }
};

template <class T>
inline void hash_combine(std::size_t & seed, const T & v)
{
  std::hash<T> hasher;
  seed ^= hasher(v) + 0x9e3779b9 + (seed << 6) + (seed >> 2);
}


namespace std {
  template <> struct hash<four_tuple> {
    inline size_t operator()(const four_tuple& ft) const {
      size_t seed=0;
      hash_combine(seed, ft.src);
      hash_combine(seed, ft.dest);
      hash_combine(seed, ft.src_p);
      hash_combine(seed, ft.dest_p);
      return seed;
    }
  };
}

pcap_t* pcap;
size_t bytes_processed = 0;
unordered_map<four_tuple, state> flow_table;
four_tuple null_ft = four_tuple();
void handler(u_char*, const struct pcap_pkthdr* h, const u_char* bytes) {
  bool end_of_flow;
  const u_char* bytes0 = bytes;
  struct ether_header *eptr = (struct ether_header *) bytes;
  bytes += sizeof(ether_header);
  auto et = ntohs(eptr->ether_type);";
  if debug then say "printf(\"ethertype: %u \", et);";
  say "
  if (et != ETHERTYPE_IP) return;
  struct ip* iph = (struct ip*) bytes;
  bytes += iph->ip_hl * 4;
  uint16_t ip_len = ntohs(iph->ip_len);
  uint8_t ipt = iph->ip_p;";
  if debug then say "printf(\"IP_PROTOCOL: %u len:%u \", ipt, ip_len);";
  say "
  if (ipt != IPPROTO_TCP) return;
  struct tcphdr* tcp_header = (struct tcphdr*) bytes;";
  if debug then say "printf(\"doff:%d \",tcp_header->doff);";
  say "
  bytes += tcp_header->doff * 4;
  end_of_flow = tcp_header->fin == 1;
  four_tuple ft = four_tuple(iph, tcp_header);
  if (ft == null_ft) return; // do nothing if it's not a tcp packet
  state& st = flow_table[ft]; // generates a new fa state if one doesn't exist
  //parse the current packet
  st.flow_data = bytes;
  st.flow_data_length = ip_len - (bytes - (const u_char*)iph);";
  if debug then
    say "  printf(\"\\nPKT(%luB):%.30s\\n\", st.flow_data_length, bytes);";
  say "
  while (st.fdpos < st.flow_data_length && st.q != NULL)
    CALL_MEMBER_FN(st, st.q)();

  if (st.fdpos < st.flow_data_length) {
    bytes_processed += st.fdpos;
  } else {
    st.base_pos += st.flow_data_length;
    st.fdpos -= st.flow_data_length;
    st.dfa_best_pos -= st.flow_data_length;
    if (st.dfa_best_pos < 0) { printf(\"MAYBE NEED LAST PACKET\\n\"); }
    bytes_processed += st.flow_data_length;
  }

  if (end_of_flow || st.q == NULL) flow_table.erase(ft);";
  if debug then
    say "if (flows > 2) pcap_breakloop(pcap);";
  say "
//  printf(\"Flows: %d Matches: %d Bytes: %lu \\n\", flows, matches, bytes_processed);
}

char errbuf[PCAP_ERRBUF_SIZE];

int main(int argc, char* argv[]) {
  if (argc != 2) { printf (\"fs [trace file]\\n\"); exit(1); }
  pcap = pcap_open_offline(argv[1], errbuf);
  if (pcap == NULL) { printf (\"Error opening file\\n\"); exit(2); }
  matches=0;
  struct timeval t0,tf;
  gettimeofday(&t0, NULL);
  int err = pcap_loop(pcap, -1, handler, NULL);
  gettimeofday(&tf, NULL);
  double t0s = t0.tv_sec+(t0.tv_usec/1000000.0);
  double tfs = tf.tv_sec+(tf.tv_usec/1000000.0);
  double time_used = tfs - t0s;
  double gbps = ((double) bytes_processed) * 8 / time_used / 1000000000;
  printf(\"\\nFlows: %d Matches: %d Bytes: %lu Skipped: %u Time: %.3fs Rate: %.2fGbps\\n\", flows, matches, bytes_processed, skip_b, time_used, gbps);
  return 0;
}
"

let main p e outfile =
  printf "Compiling %s and %s to %s\n" p e outfile;
  let proto = Ns_parse.parse_file_as_spec p
  and extr = Ns_parse.parse_file_as_extraction e in
  let ca, var_count =
    Ns_parse.merge_cas ~proto ~extr
    |> Ns_parse.regularize
    |> tap (Printf.printf "Grammar reg:\n%a\n" Ns_parse.print_reg_ca)
    |> Ns_parse.destring extr.start
  in
  let ca = ca
  |> tap (Printf.printf "Grammar ds:\n%a\n" Ns_parse.print_reg_ds_ca)
  |> Ns_parse.dechain
  |> tap (Printf.printf "Grammar dechained:\n%a\n" Ns_parse.print_reg_ds_ca)
  |> Ns_parse.flatten_priorities
(*    |> Ns_run.optimize_preds *)
  in
(*  let oc = File.open_out (e ^ ".c") in *)
  let buf = IO.output_string () in
  let oc = File.open_out outfile in
  let say x = IO.nwrite oc x; IO.write oc '\n' in
  (* buffer the CA transitions *)
  Array.iteri (ca_trans buf) ca;
  (* print the C includes *)
  print_includes say;
  (* print the DFA data tables *)
  Hashtbl.iter (print_dfa_table oc) dfa_ht;
  (* print the CA struct header *)
  gen_header oc var_count;
  (* print the dfa handling functions *)
  Hashtbl.iter (print_dfa oc) dfa_ht;
  (* write the CA transition functions that were buffered *)
  IO.nwrite oc (IO.close_out buf);
  (* print the function to read a file *)
  print_read_file say;
  (* close the CA struct *)
  end_parser_object oc;
  (* print the main function *)
  print_pcap_main say (* PCAP INPUT *)
(*  print_main say (* NON_PCAP INPUT *) *)



(*let () = main "dyck.pro" "dyck.ext" "fs.c" *)
let () = main Sys.argv.(1) Sys.argv.(2) Sys.argv.(3)

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
