#lang racket

(require "lexer.rkt")

(define (parse-binary l)
  (define left (next l))
  (define op (next l))
  (define right (next l))
  (must 'number left)
  (must 'number right))

(define (must ty tok)
  (unless (eq? (token-typ tok) ty)
    (error 'must "~a but ~a" ty (token-typ tok))))

(define (next l)
  (channel-get (lexer-items l)))
