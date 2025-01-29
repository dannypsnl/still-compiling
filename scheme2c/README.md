# scheme to C

This project compiles a tiny subset of scheme to C, the point is

1. lowering `let` to `begin (set! ...)`
2. remove complex operands from `if`
3. simplify expression in `set!`, C won't accept `(if ...)` and `(begin ...)` (block is not a value in C, unlike Scala or Rust). This step also about control flow, `set!` be seen as a statement, not an expression
4. explicate control flow by forwarding return statement
