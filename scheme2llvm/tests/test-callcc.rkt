#lang racket
(module+ test
  (require rackunit
           "../llvm.rkt")

  (check-equal? (compile-and-run '((+ 1 (call/cc (lambda (k) (k 10))))))
                11))
