# still-compiling

## Parsing

- [combinator: megaparsack](parser/combinator)
- [manual parser](parser/manual)

## (Hygienic) Macro system

- [binding as sets of scopes](binding-as-sets-of-scopes/)

## Runtime Encoding & Middle passes

- [closure conversion](closure-conversion)
- [cps conversion](cps-conversion.rkt), ref: http://churchturing.org/y/90-min-scc.pdf

## Optimization

- [local value numbering](local-value-numbering.rkt)
- [register allocation](register-allocate.rkt)
- [low-level ANF](scheme2c/)
- [liveness analysis](ir.rkt)
