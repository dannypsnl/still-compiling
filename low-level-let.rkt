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

(define-pass low-level-let : L0 (e) -> L1 ()
  (Expr : Expr (e) -> Expr ()
        [(let (,x ,[e])
           ,body)
         `(begin
            (set! ,x ,e)
            ,body)]))

(define-pass explicit-control : L1 (e) -> L1 ()
  (Expr : Expr (e) -> Expr ()
        [(begin ,[e*] ... ,[e])
         `(begin ,e* ... ,e)]
        [(set! ,x (begin ,[e*] ... ,[e]))
          `(begin ,e* ...
            (set! ,x ,e))]))

(define passes
  (compose
    explicit-control
    low-level-let))
(with-output-language (L0 Expr)
  (passes
   `(let (x (let (y 1) y))
      x)))
