#pragma once
#include <stdio.h>
#include <stdbool.h>

typedef enum
{
  INT_TAG,
  CHAR_TAG,
  BOOL_TAG,
} scm_tag;

typedef struct
{
  scm_tag tag;
  union Val
  {
    int i;
    char c;
    bool b;
  } val;
} scm_t;

scm_t make_int(int x);
scm_t make_bool(bool x);
scm_t make_char(char x);
bool to_bool(scm_t x);
void show(scm_t val);
