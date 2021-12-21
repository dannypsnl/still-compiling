#lang racket
(require nanopass)

(define-language L
  (terminals
   (symbol [x])
   (number [n]))
  (Expr (e)
        x
        n
        (+ e ...)))

(define-pass inline-add : L (e) -> L ()
  (p : Expr (e) -> Expr ()
     [(+ ,[e] ...)
      (if (andmap number? e)
          `,(apply + e)
          `(+ ,e ...))]))

(define-parser parse L)
(inline-add (parse '(+ (+ 1 2) a)))
