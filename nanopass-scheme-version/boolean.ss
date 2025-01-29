(library (boolean)
  (export mf Lb)
  (import (nanopass) (rnrs))

  (define (constant? c)
    (or (integer? c) (boolean? c)))

  (define-language Lb
    (terminals
      (symbol (x))
      (constant (c)))
    (Expr (e)
      x
      c
      (equal? e0 e1)
      (if e0 e1 e2)))

  (define-pass mb : Lb (e) -> Lb ()
    (Expr : Expr (e) -> Expr ()
      [(if ,[e0] ,[e1] ,[e2]) `(if ,e0 ,e1 ,e2)]
      [,x `(equal? ,(mf x) #f)]
      [,c `(equal? ,(mf c) #f)]
      ))
  (trace-define-pass mf : Lb (e) -> Lb ()
    (Expr : Expr (e) -> Expr ()
      [(equal? ,[e0] ,[e1])
       (if (and (boolean? e0) (boolean? e1) (equal? e0 e1))
         #t
         `(equal? ,e0 ,e1))]
      [(if ,e0 ,[e1] ,[e2])
        `(if ,(mb e0) ,e1 ,e2)]
      [,x x]
      [,c c]))

)
