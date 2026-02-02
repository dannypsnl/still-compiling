#lang racket
(module+ test
  (require rackunit
           "../llvm.rkt")

  (check-equal? (compile-and-run '((define (make-adder n)
                                     (lambda (m) (+ m n)))
                                   ((make-adder 2) 3)))
                5)

  (check-equal? (compile-and-run '((define (f x) (* x 2))
                                   (f 3)))
                6))
