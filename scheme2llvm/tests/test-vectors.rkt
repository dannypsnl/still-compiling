#lang racket
(module+ test
  (require rackunit
           "../llvm.rkt")

  (check-equal? (compile-and-run '((vector-ref (vector 10 20 30) 1))) 20)
  (check-equal? (compile-and-run '((vector 1 2 3))) #(1 2 3)))
