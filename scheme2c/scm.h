#pragma once
#include <stdio.h>

typedef enum
{
  FIXNUM_TAG
} scm_tag;

typedef struct
{
  scm_tag tag;
  union Val
  {
    int i;
  } val;
} scm_t;

scm_t make_int(int x);
void show(scm_t val);
