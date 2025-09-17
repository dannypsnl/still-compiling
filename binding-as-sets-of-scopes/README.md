# binding as sets of scopes

The paper shows an essential way to implement hygienic macro, which seems a lot better than renaming based algorithm.

- Binding as sets of scopes: https://dl.acm.org/doi/10.1145/2837614.2837620

## Scopes Set

```scheme
(let ([x 1])
  (lambda (y)
    z))
```

- the `let` form corresponds to a scope $a_{let}$.
- the `lambda` form corresponds to a $b_{lam}$.
- The set of scopes associated with `z` is $\{a_{let},b_{lam}\}$.

### Example

```scheme
(let ([x 1])
  (let-syntax ([m (syntax-rules () [(m) x])])
    (lambda (x)
      (m))))
```

The right-hand side of `m` binding has the scope set $\{a_{let}\}$, while the final `m` has the scope $\{a_{let}, b_{ls}, c_{lam}\}$; now expand `m` (we can obviously find the correct `m`) we get `x`, but this associated with scope $\{a_{let}, d_{intro}\}$, the $d_{intro}$ is about the macro expansion.

## Use-site scope

Let `m` be a macro, and `(m m-id)` will be expanded to

```scheme
(lambda (x)
  (let ([m-id 1])
    x))
```

Therefore, if we provide `x` to `m` then `(m x)` is

```scheme
(lambda (x)
  (let ([x 1])
    x))
```

The semantic get changed when `m-id` equals to `x`, and that change is not expected! We first look why the matched binding is wrong:

```scheme
(lambda (x{lam1})
  (let ([x{lam1, let1} 1])
    x{lam1, let1}))
```

The solution from paper is adding use-site scope, provided `x` for `(m x)` will get a `use1` scope, therefore

```scheme
(lambda (x{lam1})
  (let ([x{lam1, let1, use1} 1])
    x{lam1, let1}))
```
