#include <assert.h>
#include <stdarg.h>
#include <stdio.h>
#include <cstdlib>
#include <string>
#include <memory>
#include <fcntl.h>
#include "http-baseconn.h"

#define FILEPATH      "http_conversation.txt"
#define DATALEN       100000

#define BPAC 0
#define UPAC 1
#define FLOW 2

#if PARSER == BPAC
#include "http_pac.h"
#elif PARSER == UPAC
#include "http_pac_fast.h"
#elif PARSER == FLOW
extern "C" {
#include "flow.h"
}
#endif
/*
 * Optionally pass an integer parameter
 * to specify where to cut the message
 * to emulate stop-and-go parsing
 *
 */

int main(int argc, char* argv[])
{
#if PARSER==UPAC
  binpac::uint8 data[DATALEN];
  binpac::FastParser *uparser;
#elif PARSER==BPAC
  BaseConn *conn = new BaseConn();
  binpac::uint8 data[DATALEN];
  binpac::HTTP::HTTP_Conn *parser;
#elif PARSER==FLOW
  char data[DATALEN];
  value *parser;
#endif
  int cut = 1500;
  int len;
  int fd;
  struct stat st;
  int ultrapac = 1;

  if(argc == 2)
    cut = atoi(argv[1]);

  if((fd = open(FILEPATH, O_RDONLY)) < 0)
    return 1;

  len = lseek(fd, 0L, SEEK_END);
  lseek(fd, 0L, SEEK_SET);

  printf("Length: %d  Cut: %d\n", len, cut);

  printf("Initializing parser...");
#if PARSER==UPAC
  uparser = new binpac::FastParser(new binpac::SimpleFlowBuffer());
#elif PARSER==BPAC
  parser = new binpac::HTTP::HTTP_Conn(conn);
#elif PARSER==FLOW
  parser_init(argv);
  *parser = new_parser();
#endif
  
  printf("ready\n");
#if PARSER != FLOW
  try {
#endif
    while ((len = read(fd, data, cut)) > 0) {
      printf("pushing %d bytes of data\n", len);
#if PARSER==UPAC
      uparser->flowbuffer->NewData((binpac::const_byteptr) data, (binpac::const_byteptr) (data+len));
      uparser->FuncParsingFlow();
#elif PARSER==BPAC
      parser->NewData(1, data, data + len + 1);
#elif PARSER==FLOW
      new_data(parser, data, len);
#endif
    }

#if PARSER != FLOW
  } catch ( binpac::Exception const &e ) 
    {
      printf("Exception raised around pos %d", (int) lseek(fd, 0L, SEEK_CUR));
    }
#endif
  printf("\nMessage parsed:\n\n");
  printf("    method:      %s\n", conn->method);
  printf("    uri:         %s\n", conn->uri);
  printf("    version:     %s\n\n", conn->version);

close:
  printf("closing\n");
  close(fd);
end:
  return(0);
}



//    		parser->flowbuffer->NewData((const_byteptr)dataBegin, (const_byteptr)dataEnd);
//            parser->FuncParsingFlow();
