#lang racket
(module+ test
  (require rackunit
           "../llvm.rkt")

  (check-equal? (compile-and-run '(#t)) #t)
  (check-equal? (compile-and-run '(#f)) #f)
  (check-equal? (compile-and-run '((not #f))) #t)
  (check-equal? (compile-and-run '((not #t))) #f)
  (check-equal? (compile-and-run '((boolean? #t))) #t)
  (check-equal? (compile-and-run '((boolean? 1))) #f))
