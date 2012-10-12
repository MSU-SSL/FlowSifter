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
  declare_uint oc "flows";
  fprintf oc "struct state {\npublic:\n";
  for i = 0 to var_count - 1 do
    declare_uint oc ("v" ^ string_of_int i);
  done;
  declare_uint oc "base_pos";
  declare_uint oc "fdpos";
  fprintf oc "  const unsigned char* flow_data;\n";
  fprintf oc "  size_t flow_data_length;\n";
  declare_uint oc "fail_drop";
  declare_uint oc "rerun_temp";
  declare_uint oc "dfa_best_pri";
  declare_uint oc "dfa_best_q";
  declare_uint oc "dfa_best_pos";
  declare_uint oc "dfa_pri";
  declare_uint oc "dfa_q";
  fprintf oc "  void (state::*q)();\n";
  fprintf oc "  state() : base_pos(0), fdpos(0), flow_data(NULL), flow_data_length(0), fail_drop(0), rerun_temp(0), dfa_best_pri(0), dfa_best_q(0), dfa_best_pos(0), dfa_pri(0), dfa_q(0), q(&state::CA0) {flows++;}\n"

let print_builtins say =
  say "  int pos() { return fdpos; }";
  say "  int skip(int i) { fdpos += i; return fdpos; }";
  say "  int skip_to(int i) { fdpos = i; return i; }";
  say "  int notify(int i) { matches++; return 0; }";
  say "  int cur_byte() { return flow_data[fdpos]; } //FIXME";
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

let dfa_type dfa =
  if Array.length dfa.qs < 256 then "unsigned char"
  else if Array.length dfa.qs < 1 lsl 16 then "unsigned short"
  else "unsigned int"

let dfa_type_max dfa = match Array.length dfa.qs with x when x < 255 -> "0xff" | x when x < 1 lsl 16 - 1 -> "0xffff" | _ -> "0xffffffff"

let print_dfa_table oc dfa id =
  let qmax = dfa_type_max dfa in
  let safe_print oc x = if x = ~-1 then String.print oc qmax else Int.print oc x in
  let print_tr oc q = Array.print ~first:"" ~last:"\n" ~sep:"," safe_print oc q.map in
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
  say "    printf(\"D%d.%%03d@%%2x \", dfa_q, flow_data[fdpos]);" id;
  say "    dfa_q = dfa_tr%d[(dfa_q << 8) | flow_data[fdpos++]];" id;
  say "    if (dfa_q == %s) {dfa_q = %d; return;}" (dfa_type_max dfa) dfa.q0.id;
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
#include <utility>
#include <unordered_map>
#include <netinet/if_ether.h>
#include <netinet/tcp.h>
#include <netinet/ip.h>
#define CALL_MEMBER_FN(object,ptrToMember)  ((object).*(ptrToMember))
using namespace std;
"


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
    unsigned char* data = (unsigned char *) malloc (sizeof(char)*flow_data_length);
    if (flow_data == NULL) {fputs (\"Memory error\",stderr); exit (2);}

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
  say "  printf(\"\\nMatches: %d Bytes: %lu Time: %.3fs Rate: %.2f\\n\", matches, st.flow_data_length, time_used, gbps);";
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
  auto et = ntohs(eptr->ether_type);
//  printf(\"%u, %u\", et, ETHERTYPE_IP);
  if (et != ETHERTYPE_IP) return;
  struct ip* iph = (struct ip*) bytes;
  bytes += iph->ip_hl * 4;
  uint8_t ipt = iph->ip_p;
//  printf(\"%u,%u\", ipt, IPPROTO_TCP);
  if (ipt != IPPROTO_TCP) return;
  struct tcphdr* tcp_header = (struct tcphdr*) bytes;
  bytes += tcp_header->doff * 4;
  end_of_flow = tcp_header->fin == 1;
  four_tuple ft = four_tuple(iph, tcp_header);
  if (ft == null_ft) return; // do nothing if it's not a tcp packet
  state& st = flow_table[ft];
  //parse the current packet
  st.flow_data_length = h->caplen - (bytes - bytes0);
  st.fdpos = 0;
  st.flow_data = bytes;
  printf(\"\\n%lu %.30s\\n\", st.flow_data_length, bytes);
  while (st.q != NULL && st.fdpos < st.flow_data_length)
    CALL_MEMBER_FN(st, st.q)();

  bytes_processed += st.flow_data_length;
  if (end_of_flow) flow_table.erase(ft);
  if (flows > 2) pcap_breakloop(pcap);
}

char errbuf[PCAP_ERRBUF_SIZE];

int main(int argc, char* argv[]) {
  if (argc != 2) { printf (\"fs [trace file]\\n\"); exit(1); }
  pcap = pcap_open_offline(argv[1], errbuf);
  if (pcap == NULL) { printf (\"Error opening file\\n\"); exit(2); }
  struct timeval t0,tf;
  gettimeofday(&t0, NULL);
  int err = pcap_loop(pcap, -1, handler, NULL);
  gettimeofday(&tf, NULL);
  double t0s = t0.tv_sec+(t0.tv_usec/1000000.0);
  double tfs = tf.tv_sec+(tf.tv_usec/1000000.0);
  double time_used = tfs - t0s;
  double gbps = ((double) bytes_processed) * 8 / time_used / 1000000000;
  printf(\"\\nFlows: %d Matches: %d Bytes: %lu Time: %.3fs Rate: %.2f\\n\", flows, matches, bytes_processed, time_used, gbps);
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
  print_pcap_main say



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
