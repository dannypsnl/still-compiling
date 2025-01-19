#lang racket
(require nanopass)

(define (constant? c)
  (or (integer? c)))
(define-language L0
  (terminals
   (constant (c))
   (symbol (x)))
  (Expr (e body)
        (let (x e) body)
        x
        c))

(define-language L1
  (extends L0)
  (Expr (e)
        (- (let (x e) body))
        (+ (begin e* ... e)
           (set! x e))))

(define-pass remove-let : L0 (e) -> L1 ()
  (Expr : Expr (e) -> Expr ()
        [(let (,x ,[e])
           ,body)
         `(begin
            (set! ,x ,e)
            ,body)]
        [,x x]
        [,c c]))


(with-output-language (L0 Expr)
  (remove-let
   `(let (x 1)
      x)))
