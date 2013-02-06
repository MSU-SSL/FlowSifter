#include FSLIB

int bytes_processed = 0;

extern "C"
state* siftc_new_parser() {
  return new state();
}

extern "C"
void siftc_add_data(state* stp, char* str, size_t len) {
  state& st = *stp;
  st.flow_data = (const u_char*) str;
  st.flow_data_length = len;
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
  st.flow_data = NULL;
}

extern "C"
size_t siftc_get_event_count() {
  return matches;
}

extern "C"
void siftc_delete_parser(state* stp) {
  delete stp;
}
