#lang nanopass

(define (op? sym)
  (member sym '(+ - * / = >= > <= <)))

(define-language IR
  (terminals
   (symbol (name))
   (op (op))
   (integer (int)))
  (Stmt (stmt)
        (assign name expr)
        (condbr cond int)
        (br int))
  (Expr (expr cond)
   name
   int
   ; only binary operator here
   (op expr0 expr1)))

(define-parser parse IR)
(parse '(assign a 1))
(parse '(assign b 2))
(parse '(assign c (+ a b)))
