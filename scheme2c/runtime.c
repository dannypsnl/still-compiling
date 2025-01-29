#include "scm.h"

extern scm_t scheme_entry();

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
      .tag = INT_TAG,
      .val = x};
  return v;
}

scm_t make_bool(bool x)
{
  scm_t v = {
      .tag = BOOL_TAG,
      .val = x};
  return v;
}
scm_t make_char(char x)
{
  scm_t v = {
      .tag = CHAR_TAG,
      .val = x};
  return v;
}

bool to_bool(scm_t val)
{
  if (val.tag == BOOL_TAG)
  {
    return val.val.b;
  }
  else
  {
    return false;
  }
}

void show(scm_t val)
{
  if (val.tag == INT_TAG)
  {
    printf("%d", val.val.i);
  }
  else if (val.tag == CHAR_TAG)
  {
    printf("#\\%c", val.val.c);
  }
  else if (val.tag == BOOL_TAG)
  {
    if (val.val.b)
    {
      printf("#t");
    }
    else
    {
      printf("#f");
    }
  }
}
