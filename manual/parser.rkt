#lang racket

(require "lexer.rkt")

; ast
(struct expr () #:transparent)
(struct binary expr (op left right) #:transparent)

; parse
(define (parse-expr p left-hand-side previous-primary)
  (define lhs (if left-hand-side
                  left-hand-side
                  (parse-unary p)))

  (let loop ([lookahead (peek p)])
    (when (>= (precedence lookahead) previous-primary)
      (define operator lookahead)
      (take p)
      (define rhs (parse-unary p))
      (set! lookahead (peek p))
      (let loop ()
        (when (or (> (precedence lookahead) (precedence operator))
                  (and (right-assoc? lookahead)
                       (= (precedence lookahead) (precedence operator))))
          (set! rhs (parse-expr p rhs (precedence lookahead)))
          (set! lookahead (peek p))
          (loop)))
      (set! lhs (binary (token-typ operator)
                        lhs rhs))
      (loop lookahead)))

  lhs)

(define (parse-unary p)
  (define tok (peek p))
  (case (token-typ tok)
    [(number) (take p)
              (string->number (token-val tok))]
    [(true) (take p)
            'true]
    [(false) (take p)
             'false]
    [(identifier) (take p)
                  (token-val tok)]
    [else (error 'unknown "~a" tok)]))

; helper
(struct parser (name lexer tokens offset)
  #:mutable
  #:transparent)

(define (parse name input)
  (define lexer (lex name input))
  (define p (parser name lexer (vector) 0))
  (parse-expr p #f 1))

(define (peek p [n 0])
  (get-token p (+ (parser-offset p) n)))
(define (take p)
  (define origin (parser-offset p))
  (set-parser-offset! p (add1 origin))
  (get-token p origin))
(define (consume p . wants)
  (predict p wants))
(define (predict p . wants)
  (for ([i (length wants)]
        [want wants])
    (define tok (peek p i))
    (unless (eq? (token-typ tok) want)
      (error 'unexpected-token "want ~a, got ~a" want (token-typ tok)))))

(define (get-token p fixed-offset)
  (when (vector-empty? (parser-tokens p))
    (get-token-from-lex p))
  (define tokens (parser-tokens p))
  (define last-token (vector-ref tokens (sub1 (vector-length tokens))))
  (case (token-typ last-token)
    [(EOF) last-token]
    [else (get-token-from-lex p)
          (set! tokens (parser-tokens p))])
  (vector-ref tokens fixed-offset))
(define (get-token-from-lex p)
  (define l (parser-lexer p))
  (define new-last-token (channel-get (lexer-items l)))
  (set-parser-tokens! p
                      (vector-append (parser-tokens p) (vector new-last-token))))

(define (right-assoc? token) #f)
(define (precedence token)
  (case (token-typ token)
    [(add sub) 2]
    [(mul div) 3]
    [else 0]))

(module+ test
  (require rackunit)

  (check-equal? (parse "parsing" (open-input-string "12 + 23 * 34"))
                (binary 'add 12 (binary 'mul 23 34))))

