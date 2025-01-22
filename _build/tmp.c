#define EXPORT __attribute__((visibility("default")))
EXPORT
double add_double_in_C(double a, double b) { return a+b+3; }