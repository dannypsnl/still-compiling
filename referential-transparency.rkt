#lang racket
(require nanopass)

(define-language L
  (terminals
   (symbol (x))
   (number (n)))
  (Expr (e)
        x
        n
        (+ e0 e1)
        (λ (x) e)
        (e0 e1)))

(define-pass replace : L (body id new-e) -> L ()
  (T : Expr (e) -> Expr ()
     [,x (guard (eq? x id))
         new-e]))
(define-pass app : L (e) -> L ()
  (T : Expr (e) -> Expr ()
     [((λ (,x) ,e) ,e1)
      (replace e x e1)]))

(define-parser parse-L L)
((compose app
          app
          parse-L)
 '(((λ (a) (λ (b) (+ a b)))
    1)
   2))
