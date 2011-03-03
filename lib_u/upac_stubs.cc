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
  value p = (value) new binpac::FastParser(new binpac::SimpleFlowBuffer());
  CAMLreturn (p);
}

extern "C" value add_data (value v_prsr, value v_str) {
  CAMLparam2 (v_prsr, v_str);
  binpac::FastParser* uparser = (binpac::FastParser*) v_prsr;
  const char* data = String_val(v_str);
  const char* end = data + caml_string_length(v_str) + 1;
  try {
    uparser->flowbuffer->NewData((binpac::const_byteptr) data, (binpac::const_byteptr) end);
    uparser->FuncParsingFlow();
    //    printf("*** ul: %d\n%s\n", Int_val(v_len), data);
  } catch ( binpac::Exception const &e ) {
    printf("Ultrapac raised\n");
  }

  CAMLreturn (Val_unit);
}

extern "C" value get_event_count (value v_unit) {
  CAMLparam1 (v_unit);
  //  binpac::FastParser* parser = (binpac::FastParser*) v_prsr;
  value ret = Val_int(binpac::match_events);
  CAMLreturn (ret);
}

extern "C" value reset_parser (value v_prsr) {
  CAMLparam1 (v_prsr);
  binpac::FastParser* parser = (binpac::FastParser*) v_prsr;
  //  cout << "r";
  parser->Reset();
  //  delete (binpac::FastParser*) v_prsr;
  
  //  v_prsr = (value) new binpac::FastParser(new binpac::SimpleFlowBuffer());
  CAMLreturn (Val_unit);  
}


extern "C" value delete_parser (value v_prsr) {
  CAMLparam1 (v_prsr);
  binpac::FastParser* parser = (binpac::FastParser*) v_prsr;
  delete parser;
  CAMLreturn (Val_unit);  
}


