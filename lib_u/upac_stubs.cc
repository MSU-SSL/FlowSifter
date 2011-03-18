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

#include "http_pac_fast.h"

namespace binpac { extern int match_events; }


extern "C" value new_parser (value v_unit) {
  CAMLparam1 (v_unit);
  //  cout << "u";
  binpac::ConnParser* uparser = new binpac::ConnParser();
  uparser->client->is_orig = true;
  value p = (value) uparser;
  CAMLreturn (p);
}

extern "C" value add_data (value v_prsr, value v_dir, value v_str) {
  CAMLparam3 (v_prsr, v_dir, v_str);
  binpac::ConnParser* uparser = (binpac::ConnParser*) v_prsr;
  const char* data = String_val(v_str);
  const char* end = data + caml_string_length(v_str) + 1;
  int d = Int_val(v_dir);
  try {
    binpac::FastParser* p = (d == 0) ? uparser->server : uparser->client;
    p->flowbuffer->NewData((binpac::const_byteptr) data, (binpac::const_byteptr) end);
    p->FuncParsingFlow();
    //    printf("*** ul: %d\n%s\n", Int_val(v_len), data);
  } catch ( binpac::Exception const &e ) {
    printf("Ultrapac raised\n");
  }

  CAMLreturn (Val_unit);
}

extern "C" value get_event_count (value v_unit) {
  CAMLparam1 (v_unit);
  value ret = Val_int(binpac::match_events);
  CAMLreturn (ret);
}


extern "C" value delete_parser (value v_prsr) {
  CAMLparam1 (v_prsr);
  binpac::ConnParser* parser = (binpac::ConnParser*) v_prsr;
  delete parser;
  CAMLreturn (Val_unit);  
}


extern "C" value reset_parser (value v_prsr) {
  CAMLparam1 (v_prsr);
  binpac::ConnParser* parser = (binpac::ConnParser*) v_prsr;
  parser->server->Reset();
  parser->client->Reset();
  CAMLreturn (Val_unit);  
}
