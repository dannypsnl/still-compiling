#lang racket
(provide newenv insert lookup)

(struct tyenv
  (current-map parent))
(define (newenv [parent #f])
  (tyenv (make-hash) parent))

(define (identifier->lookup x)
  (syntax->datum x))

(define (insert env x ty)
  (hash-set! (tyenv-current-map env) (identifier->lookup x) ty))

(define (lookup env x)
  (hash-ref (tyenv-current-map env) (identifier->lookup x)))
