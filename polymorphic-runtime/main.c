#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>

typedef struct _ListInt
{
  int first;
  struct _ListInt *second;
} ListInt;

void print_list_int(ListInt *l);

bool empty(ListInt *l)
{
  if (l == NULL)
  {
    return true;
  }
  else
  {
    return false;
  }
}

void single(int i, ListInt *out)
{
  out->first = i;
  out->second = NULL;
}
void cons(int i, ListInt *l, ListInt *out)
{
  out->first = i;
  out->second = l;
}

void map(int (*f)(int), ListInt *l, ListInt *out)
{
  if (l == NULL)
  {
    out = NULL;
    return;
  }
  else
  {
    out->first = f(l->first);
    out->second = l->second;
    map(f, l->second, out->second);
    return;
  }
}

int add1(int i)
{
  return i + 1;
}

void print_list_int(ListInt *l)
{
  if (l != NULL)
  {
    printf("%d", l->first);
    print_list_int(l->second);
  }
  else
  {
    printf("\n");
  }
}

int main()
{
  ListInt *l = malloc(sizeof(ListInt));
  single(3, l);
  ListInt *l2 = malloc(sizeof(ListInt));
  cons(2, l, l2);
  ListInt *l3 = malloc(sizeof(ListInt));
  cons(1, l2, l3);

  ListInt *out = malloc(sizeof(ListInt));
  map(add1, l3, out);
  print_list_int(out);
}
