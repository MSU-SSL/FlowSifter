#include <caml/mlvalues.h>

void* siftc_new_parser();
void siftc_add_data(void* stp, char* str, size_t len);
void siftc_delete_parser(void* stp);
size_t siftc_get_event_count();

//Provide C stubs to connect ocaml to
/*
external new_parser : unit -> t = "new_parser"
external add_data : t -> direction -> string -> unit = "add_data"
external get_event_count : unit -> int = "get_event_count"
//external reset_parser : t -> unit = "reset_parser"
external delete_parser : t -> unit = "delete_parser"
 */

CAMLprim value new_parser(value unit) {
  return (value) siftc_new_parser();
}

CAMLprim value add_data(value stp, value ignore, value str_data) {
  char* str = String_val(str_data);
  size_t len = caml_string_length(str_data);
  siftc_add_data((void*)stp, str, len);
  return Val_int(0);
}

CAMLprim value get_event_count(value unit) {
  return Val_int(siftc_get_event_count());
}

/*
CAMLprim value reset_parser(state* stp) {
  stp->reset();
  return Val_int(0);
}
*/

CAMLprim value delete_parser(value stp) {
  siftc_delete_parser((void*)stp);
  return Val_int(0);
}
