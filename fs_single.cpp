#include "fs_lib.h"
#include <fcntl.h>
#include <sys/time.h> // for gettimeofday

void read_file(char* filename, unsigned char*& out, int& len) {
  FILE* fd = fopen(filename, "r");
#ifdef DEBUG
  printf("Subject: %s\n", basename(filename));
#endif
  fseek (fd , 0 , SEEK_END);
  len = ftell (fd);
  rewind (fd);

  // allocate memory to contain the whole file:
  unsigned char* data = (unsigned char *) malloc (sizeof(char)*len);
  if (data == NULL) {fputs ("Memory error",stderr); exit (2);}

  // copy the file into the buffer:
  size_t result = fread (data,1,len,fd);
  if (result != len) {fputs ("Reading error",stderr); exit (3);}
  out = data;
}

//single-flow main file for flowsifter

int main(int argc, char* argv[]) {
  if (argc != 2) { printf ("fs [trace_file]\n"); exit(1); }
  state st;
  read_file(argv[1], st.flow_data, st.flow_data_length);
  struct timeval t0,tf;
  gettimeofday(&t0, NULL);
  while (st.q != NULL && st.fdpos < st.flow_data_length) CALL_MEMBER_FN(st, st.q)();
  gettimeofday(&tf, NULL);
  double t0s = t0.tv_sec+(t0.tv_usec/1000000.0);
  double tfs = tf.tv_sec+(tf.tv_usec/1000000.0);
  double time_used = tfs - t0s;
  double gbps = ((double) st.flow_data_length) * 8 / time_used / 1000000000;
  printf("\nMatches: %d Bytes: %lu Time: %.3fs Rate: %.2fGbps\n", matches, st.flow_data_length, time_used, gbps);
  return 0;
}
