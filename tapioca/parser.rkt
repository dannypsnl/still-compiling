#lang racket/base
(provide parse-file)
(require "lexer.rkt"
         parser-tools/lex
         megaparsack
         megaparsack/text
         megaparsack/parser-tools/lex
         data/applicative
         data/monad)
 (require racket/syntax-srcloc)

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
(define plist/p
  (syntax/p
   (do (token/p '|(|)
     [e* <- (many/p notation/p)]
     (token/p '|)|)
     (pure e*))))
(define blist/p
  (syntax/p
   (do (token/p '|[|)
     [e* <- (many/p notation/p)]
     (token/p '|]|)
     (pure e*))))
(define quote/p
  (syntax/p
   (do (token/p '|'|)
     [e <- notation/p]
     (pure (list 'quote e)))))
(define notation/p
  (or/p number/p
        string/p
        identifier/p
        quote/p
        plist/p
        blist/p))

(define file/p
  (do [e* <- (many/p notation/p)]
    (pure e*)))

(define (parse-notation input-port)
  (parse-result!
   (parse-tokens notation/p (tokens input-port))))

(define (parse-file filename)
  (define input-port (open-input-file filename #:mode 'text))
  (parse-result!
    (parse-tokens file/p (tokens input-port) filename)))

(module+ test
  (require rackunit)

  (define (parse str)
    (syntax->datum (parse-notation (open-input-string str))))

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

(module+ main
  (println (parse-file "example/hello.ss")))
