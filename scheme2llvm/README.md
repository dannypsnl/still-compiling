# scheme 2 LLVM

1. CPS conversion (for `call/cc`)
2. Closure conversion
3. Compile to LLVM
   - Tail call optimization (use musttail of LLVM)

## Features

1. integers
2. floating numbers
3. pairs/lists
4. vectors
5. booleans
6. strings
7. logical operators, e.g. `and`, `or`, comparison operators, and some predicates
8. control flow, e.g. `if`
9. `let` bindings
10. `define` form
11. continuation `call/cc`
12. IO, e.g. `displayln`
