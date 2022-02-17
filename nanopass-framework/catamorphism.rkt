#lang nanopass

(define (op? v)
  (member v '(+ - * / ^)))
(define-language HighLevelIR
  (terminals
   (symbol (name))
   (op (op))
   (integer (int)))
  (Expr (expr)
        name
        int
        (binary op expr0 expr1)))

(define-parser parse HighLevelIR)

(define-pass simplify-without-catamorphism : (HighLevelIR Expr) (e) -> (HighLevelIR Expr) ()
  (Expr : Expr (e) -> Expr ()
        [(binary ,op ,expr0 ,expr1)
         (match* {op expr0 expr1}
           [{'^ x 2} `(binary * ,x ,x)]
           [{'* x 2} `(binary + ,x ,x)]
           [{'* 2 x} `(binary + ,x ,x)]
           [{_ _ _} e])]))
(define-pass simplify : (HighLevelIR Expr) (e) -> (HighLevelIR Expr) ()
  (Expr : Expr (e) -> Expr ()
        [(binary ,op ,[expr0] ,[expr1])
         (match* {op expr0 expr1}
           [{'^ x 2} `(binary * ,x ,x)]
           [{'* x 2} `(binary + ,x ,x)]
           [{'* 2 x} `(binary + ,x ,x)]
           [{_ _ _} e])]))

(define test-p (parse '(binary * 2 (binary * 2 4))))
; simplify implies "reduction in strength"
(simplify-without-catamorphism test-p)
; '(binary + (binary * 2 4) (binary * 2 4))
(simplify test-p)
; '(binary + (binary + 4 4) (binary + 4 4))