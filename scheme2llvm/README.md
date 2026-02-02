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
6. logical operators, e.g. `and`, `or`, comparison operators, and some predicates
7. control flow, e.g. `if`
8. `let` bindings
9. `define` form
10. continuation `call/cc`
11. IO, e.g. `displayln`
