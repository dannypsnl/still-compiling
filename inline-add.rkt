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
     [(+ ,n ...) (apply + n)]
     [(+ ,[e] ...)
      (define number?-e (filter number? e))
      (cond
        [(> (length number?-e) 0)
         `(+ ,(apply + number?-e) ,(filter (compose not number?) e) ...)]
        [else `(+ ,e ...)])]))

(define-parser parse L)
(define f (compose inline-add parse))
(f '(+ (+ 1 2) a))
(f '(+ 2 a 3))
