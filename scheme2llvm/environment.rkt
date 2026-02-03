#lang racket
(provide make-env
         bind!
         lookup
         extend)

(struct env (map parent))
(define (make-env #:map [m (make-hash)] #:parent [p #f])
  (env m p))

(define (bind! e x v)
  (hash-set! (env-map e) x v))
(define (lookup e x)
  (hash-ref (env-map e) x
            (lambda ()
              (cond
                [(env-parent e)
                 (lookup (env-parent e) x)]
                [else (error 'env "variable ~a not found" x)]))))
(define (extend e)
  (make-env #:parent e))
