#lang racket
(module+ test
  (require rackunit
           "../llvm.rkt")

  (check-equal? (compile-and-run '(((lambda (fib) (fib fib 15))
                                    (lambda (self n)
                                      (if (< n 2)
                                          n
                                          (+ (self self (- n 1))
                                             (self self (- n 2))))))))
                610))
