#lang racket
(module+ test
  (require rackunit
           "../llvm.rkt")

  (check-equal? (compile-and-run '((car (cons 1 2)))) 1)
  (check-equal? (compile-and-run '((cdr (cons 1 2)))) 2)
  (check-equal? (compile-and-run '((list 1 2 3))) '(1 2 3)))
