# closure conversion

The closure conversion is a process that

1. find out lambda, compute their free variables
2. capture free variables into closure
3. lift lambda & refers to them from closures

## problem of the compiler

This compiler skip the remove complex operands and explicate control, therefore, some valid expression cannot be written.
