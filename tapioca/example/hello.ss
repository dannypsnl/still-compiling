(: x : i32)
(define x 1)

(: y : int)
(define y 1)

(: i : int -> int)
(define i (lambda (x) x))

(: f : int int -> int)
(define (f a b)
  (let ([b a])
    b))

(: g : int int -> int)
(define g
  (lambda (a b)
    b))
