#include <iostream>
#include <assert.h>
#include <stdarg.h>
#include <stdio.h>
#include <cstdlib>
#include <string>
#include <memory>
#include <fcntl.h>

extern "C" {
#include <caml/memory.h>
#include <caml/mlvalues.h>
}

#include "http_pac.h"
#include "../http-baseconn.h"

namespace binpac { namespace HTTP {extern int events; } }

extern "C" value new_parser (value v_unit) {
  CAMLparam1 (v_unit);
  // use HTTP_Flow?
  //  cout << "b";
  binpac::HTTP::HTTP_Conn* p = new binpac::HTTP::HTTP_Conn(new BaseConn());
  //  p->
  CAMLreturn ((value) p);
}

extern "C" value add_data (value v_prsr, value v_dir, value v_str) {
  CAMLparam3 (v_prsr, v_dir, v_str);
  binpac::HTTP::HTTP_Conn *parser = (binpac::HTTP::HTTP_Conn*) v_prsr;
  const char* str = String_val(v_str);
  const char* end = str + caml_string_length(v_str) + 1;
  try {
    parser->NewData(Bool_val(v_dir), (binpac::uint8*) str, (binpac::uint8*) end);
  } catch ( binpac::Exception const &e ) {
    //    printf("Binpac raised: %s\n", e.c_msg());  fflush(stdout);
  }

  CAMLreturn (Val_unit);
}

extern "C" value get_event_count (value v_unit) {
  CAMLparam1 (v_unit);
  value ret = Val_int(binpac::HTTP::events);
  CAMLreturn (ret);
}

extern "C" value reset_parser (value v_prsr) {
  CAMLparam1 (v_prsr);
  binpac::HTTP::HTTP_Conn *parser = (binpac::HTTP::HTTP_Conn*) v_prsr;
  cout << "r";
  //  parser->FlowEOF(true);
  //nothing to do?
  CAMLreturn (Val_unit);  
}

extern "C" value delete_parser (value v_prsr) {
  CAMLparam1 (v_prsr);
  binpac::HTTP::HTTP_Conn *parser = ((binpac::HTTP::HTTP_Conn*) v_prsr);
  delete parser;
  CAMLreturn (Val_unit);  
}
