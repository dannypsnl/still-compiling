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
      (define number?-e (filter number? e))
      (define non-number?-e (filter (compose not number?) e))
      (cond
        [(and (> (length number?-e) 0)
              (> (length non-number?-e) 0))
         `(+ ,(apply + number?-e) ,non-number?-e ...)]
        [(> (length number?-e) 0)
         `,(apply + number?-e)]
        [else
         `(+ ,e ...)])]))

(define-parser parse L)
(inline-add (parse '(+ (+ 1 2) a)))
(inline-add (parse '(+ 2 a 3)))
