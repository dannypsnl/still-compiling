#lang racket
(module+ test
  (require rackunit
           "../llvm.rkt")

  (check-equal? (compile-and-run '((+ 1 2))) 3)
  (check-equal? (compile-and-run '((+ 1 2 3))) 6)
  (check-equal? (compile-and-run '((- 10 3))) 7)
  (check-equal? (compile-and-run '((* 4 5))) 20)
  (check-equal? (compile-and-run '((/ 10 2))) 5)
  (check-equal? (compile-and-run '((if (< 1 2) 10 20))) 10)
  (check-equal? (compile-and-run '((if (> 1 2) 10 20))) 20)
  (check-equal? (compile-and-run '((if (= 3 3) 1 0))) 1)
  (check-equal? (compile-and-run '((if (<= 2 2) 1 0))) 1)
  (check-equal? (compile-and-run '((if (>= 3 2) 1 0))) 1))
