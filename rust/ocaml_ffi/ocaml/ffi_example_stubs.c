extern void hello_from_rust();

#include <caml/memory.h>

CAMLprim value ffi_example_rust_hello(value unit) {
  CAMLparam1(unit);
  hello_from_rust();
  CAMLreturn (Val_unit);
}
