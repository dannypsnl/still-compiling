#lang racket
(module+ test
  (require rackunit
           "../llvm.rkt")

  (check-equal? (compile-and-run '(((lambda (x) (+ x 1)) 41))) 42))
