#include <unistd.h>
#include <caml/memory.h>
#include <caml/fail.h>
#include <caml/unixsupport.h>

CAMLprim value num_cpus_logical(value unit)
{
    CAMLparam1(unit);
    long cpus = sysconf(_SC_NPROCESSORS_ONLN);
    if (cpus < 1)
    {
        CAMLreturn(Val_long(1));
    }
    else
    {
        CAMLreturn(Val_long(cpus));
    }
}