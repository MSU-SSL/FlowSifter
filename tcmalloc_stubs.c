#include <caml/memory.h>
#include <caml/mlvalues.h>
#include <google/malloc_extension_c.h>

value get_vmsize (value v_unit) {
  CAMLparam1 (v_unit);
  size_t v = 0;
  MallocExtension_GetNumericProperty("generic.current_allocated_bytes", &v);
  CAMLreturn (Val_int(v));  
}
