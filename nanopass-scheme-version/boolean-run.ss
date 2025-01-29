(import  (chezscheme) (boolean) (nanopass))

(with-output-language (Lb Expr)
  (define example
    `(if (if (equal? e #f) (equal? #t #f) (equal? #f #f)) 5 6))
  )

(pretty-print (mf example))
