#include <stdint.h>
#include <stdlib.h>

uint64_t cons(uint64_t car, uint64_t cdr) {
  uint64_t *pair = (uint64_t *)malloc(2 * sizeof(uint64_t));
  pair[0] = car;
  pair[1] = cdr;
  return (uint64_t)pair;
}
uint64_t car(uint64_t p) { return ((uint64_t *)p)[0]; }
uint64_t cdr(uint64_t p) { return ((uint64_t *)p)[1]; }

uint64_t vector_ref(uint64_t vec, uint64_t index) {
  return ((uint64_t *)vec)[index];
}
