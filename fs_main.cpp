#include "fs_lib.h"

#include <sys/time.h> // for gettimeofday
#include <pcap/pcap.h>
#include <netinet/if_ether.h>
#include <netinet/tcp.h>
#include <netinet/ip.h>
#include <limits.h>
#include <utility>
#include <unordered_map>

//char charbuf[2];
//char* nice(unsigned char c) { if (c >= 0x20 && c <= 0x7e) sprintf(charbuf, " %c", c); else sprintf(charbuf, "%02x", c); return charbuf;}

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
unordered_map<four_tuple, uint32_t> initial_sequence_number;
four_tuple null_ft = four_tuple();
struct packet {
  four_tuple ft;  // the 4-tuple that identifies this flow
  string payload; // the byte contents of the flow
  bool fin;   // whether this packet is final packet of flow
  uint32_t off; // offset of packet
  packet(four_tuple ft, const char* bytes, size_t len, bool fin, uint32_t off) : ft(ft), payload(bytes, len), fin(fin), off(off) {}
};
vector<packet> packets;

void gen_packets(u_char*, const struct pcap_pkthdr* h, const u_char* bytes) {
  bool end_of_flow;
  const u_char* bytes0 = bytes;
  struct ether_header *eptr = (struct ether_header *) bytes;
  bytes += sizeof(ether_header);
  auto et = ntohs(eptr->ether_type);
#ifdef DEBUG
  printf("ethertype: %u ", et);
#endif
  if (et != ETHERTYPE_IP) return;
  struct ip* iph = (struct ip*) bytes;
  bytes += iph->ip_hl * 4;
  uint16_t ip_len = ntohs(iph->ip_len);
  uint8_t ipt = iph->ip_p;
#ifdef DEBUG
  printf("IP_PROTOCOL: %u len:%u ", ipt, ip_len);
#endif
  if (ipt != IPPROTO_TCP) return;
  struct tcphdr* tcp_header = (struct tcphdr*) bytes;
#ifdef DEBUG
  printf("doff:%d ",tcp_header->doff);
#endif
  bytes += tcp_header->doff * 4;
  end_of_flow = tcp_header->fin == 1;
  four_tuple ft = four_tuple(iph, tcp_header);
  if (ft == null_ft) return; // do nothing if it's not a tcp packet
  uint32_t off; // byte offset for this packet
  if (tcp_header->syn == 1) {
    initial_sequence_number[ft] = tcp_header->seq;
    off = 0;
  } else {
    off = tcp_header->seq - initial_sequence_number[ft];
  }
  uint16_t len = ip_len - (bytes - (const u_char*)iph);
  if (len > 0)
    packets.push_back(packet(ft, (const char*) bytes, len, end_of_flow, off));
  if (end_of_flow) initial_sequence_number.erase(ft);
}

inline void run_fs(packet& p, state& st) {
    st.flow_data = (const u_char*) p.payload.c_str();
    st.flow_data_length = p.payload.size();
#ifdef DEBUG
    printf("\nPKT(%luB):%.30s\n", st.flow_data_length, st.flow_data);
#endif
  while (st.fdpos < st.flow_data_length && st.q != NULL)
    CALL_MEMBER_FN(st, st.q)();

  if (st.fdpos < st.flow_data_length) {
    bytes_processed += st.fdpos;
  } else {
    st.base_pos += st.flow_data_length;
    st.fdpos -= st.flow_data_length;
    st.dfa_best_pos -= st.flow_data_length;
    if (st.dfa_best_pos < 0) { printf("MAYBE NEED LAST PACKET\n"); }
    bytes_processed += st.flow_data_length;
  }
}

void run_parse() {
  for (auto i = packets.begin(); i != packets.end(); i++) {
    packet& p = *i;
    if (p.fin && p.off == 0) { //singleton flow
      state st;
      run_fs(p, st);
    } else if (p.fin) { //final packet of flow
      auto j = flow_table.find(p.ft);
      run_fs(p, (*j).second);
      flow_table.erase(j);
    } else if (p.off == 0) { // initial packet of flow
      state& st = flow_table[p.ft]; // creates new state
      run_fs(p,st);
    } else { // middle packet
      state& st = flow_table[p.ft]; // does not create new state
      run_fs(p,st);
    }
//  if (p.fin || st.q == NULL) flow_table.erase(p.ft);";
#ifdef DEBUG
    if (flows > 200) pcap_breakloop(pcap);
#endif
  }
}

char errbuf[PCAP_ERRBUF_SIZE];

int main(int argc, char* argv[]) {
  if (argc != 2) { printf ("fs [trace file]\n"); exit(1); }
  pcap = pcap_open_offline(argv[1], errbuf);
  if (pcap == NULL) { printf ("Error opening file\n"); exit(2); }
  int err = pcap_loop(pcap, -1, gen_packets, NULL); // parse pcap into packets
  matches=0;
  bytes_processed=0;
  struct timeval t0,tf;
  gettimeofday(&t0, NULL);
  run_parse();
  gettimeofday(&tf, NULL);
  double t0s = t0.tv_sec+(t0.tv_usec/1000000.0);
  double tfs = tf.tv_sec+(tf.tv_usec/1000000.0);
  double time_used = tfs - t0s;

  double gbps = ((double) bytes_processed) * 8 / time_used / 1000000000;
  printf("Flows: %d Matches: %d Bytes: %lu Skipped: %u Time: %.3fs Rate: %.2fGbps\n\n", flows, matches, bytes_processed, skip_b, time_used, gbps);
  //print dfa_starts
#ifdef STARTS
  printf("DFA starts: ");
#endif
    //    for id = 0 to Hashtbl.length dfa_ht do
    //      say "  printf(\"%d:%%d\\n\",dfa_starts[%d]);" id id;
    //    done;
    //  );
  return 0;
  }
