#include "scm.h"

__attribute__((__cdecl__)) extern scm_t scheme_entry();

int main(int argc, const char **argv)
{
  scm_t val = scheme_entry();
  show(val);
  printf("\n");
  return 0;
}

scm_t make_int(int x)
{
  scm_t v = {
      .tag = FIXNUM_TAG,
      .val = x};
  return v;
}

void show(scm_t val)
{
  if (val.tag == FIXNUM_TAG)
  {
    // integer
    printf("%d", val.val.i);
  }
}
