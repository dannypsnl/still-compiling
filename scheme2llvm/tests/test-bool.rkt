#lang racket
(module+ test
  (require rackunit
           "../llvm.rkt")

  (check-equal? (compile-and-run '(#t)) #t)
  (check-equal? (compile-and-run '(#f)) #f)
  (check-equal? (compile-and-run '((not #f))) #t)
  (check-equal? (compile-and-run '((not #t))) #f)
  (check-equal? (compile-and-run '((boolean? #t))) #t)
  (check-equal? (compile-and-run '((boolean? 1))) #f)

  ;; and
  (check-equal? (compile-and-run '((and))) #t)
  (check-equal? (compile-and-run '((and 1))) 1)
  (check-equal? (compile-and-run '((and 1 2))) 2)
  (check-equal? (compile-and-run '((and #f 2))) #f)
  (check-equal? (compile-and-run '((and 1 2 3))) 3)
  (check-equal? (compile-and-run '((and 1 #f 3))) #f)

  ;; or
  (check-equal? (compile-and-run '((or))) #f)
  (check-equal? (compile-and-run '((or 1))) 1)
  (check-equal? (compile-and-run '((or #f 2))) 2)
  (check-equal? (compile-and-run '((or 1 2))) 1)
  (check-equal? (compile-and-run '((or #f #f 3))) 3)
  (check-equal? (compile-and-run '((or #f #f #f))) #f))
