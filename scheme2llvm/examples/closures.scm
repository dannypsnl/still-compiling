(define (make-adder n)
  (lambda (m) (+ m n)))
(displayln ((make-adder 2) 3))
