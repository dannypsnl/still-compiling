#lang racket/base

(require "lexer.rkt"
         parser-tools/lex
         megaparsack
         megaparsack/text
         megaparsack/parser-tools/lex
         data/applicative
         data/monad)

(define (tokens input-port)
  (port-count-lines! input-port)
  (let loop ([v (lex input-port)])
    (cond
      [(void? (position-token-token v)) (loop (lex input-port))]
      [(eof-object? (position-token-token v)) '()]
      [else (cons v (loop (lex input-port)))])))

(define number/p (syntax/p (token/p 'NUMBER)))
(define string/p (syntax/p (token/p 'STRING)))
(define identifier/p (syntax/p (token/p 'IDENTIFIER)))
(define form/p
  (syntax/p
   (do (token/p '|(|)
     [e* <- (many/p expr/p)]
     (token/p '|)|)
     (pure e*))))
(define quote/p
  (syntax/p
   (do (token/p '|'|)
     [e <- expr/p]
     (pure (list 'quote e)))))
(define expr/p
  (or/p number/p
        string/p
        identifier/p
        quote/p
        form/p))

(define (parse-expr input-port)
  (parse-result!
   (parse-tokens expr/p (tokens input-port))))

(module+ test
  (require rackunit)

  (define (parse str)
    (syntax->datum (parse-expr (open-input-string str))))

  (check-equal? (parse "(+ 1 2 3)")
                '(+ 1 2 3))
  (check-equal? (parse "(1 (2 3 4) 5 6 7)")
                '(1 (2 3 4) 5 6 7))
  (check-equal? (parse "'(1 2 3)")
                ''(1 2 3))
  (check-equal? (parse "'1")
                ''1)
  (check-equal? (parse "1")
                1)
  (check-equal? (parse "(println \"hello\")")
                '(println "hello"))
  (check-equal? (parse "(define foo (+ 1 2 3))")
                '(define foo (+ 1 2 3))))
