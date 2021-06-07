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
  (define p (parser name lexer (stream) 0))
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
  (when (stream-empty? (parser-tokens p))
    (increase-token-stream p))
  (define tokens (parser-tokens p))
  (if (>= fixed-offset (stream-length tokens))
      (let ([last-token (stream-ref tokens (sub1 (stream-length tokens)))])
        (case (token-typ last-token)
          [(EOF) last-token]
          [else (increase-token-stream p)
                (get-token p fixed-offset)]))
      (stream-ref tokens fixed-offset)))
(define (increase-token-stream p)
  (define l (parser-lexer p))
  (define new-last-token (channel-get (lexer-tokens l)))
  (set-parser-tokens! p
                      (stream-append (parser-tokens p) (stream new-last-token))))

(define (right-assoc? token)
  (case (token-typ token)
    [(^) #t]
    [else #f]))
(define (precedence token)
  (define op** '((eq)
                 (and or)
                 (add sub)
                 (mul div ^)))
  (define m (make-hash))
  (for ([i (length op**)]
        [op* op**])
    (for ([op op*])
      (hash-set! m op (+ 2 i))))
  (hash-ref m (token-typ token) 0))

(module+ test
  (require rackunit)

  (check-equal? (parse "parsing" (open-input-string "12 + 23 * 34"))
                (binary 'add 12 (binary 'mul 23 34)))

  (test-case "increase token stream automatically"
             (define lexer (lex "" (open-input-string "12 + 23 * 34")))
             (define p (parser "" lexer (stream) 0))
             (check-equal? (get-token p 4)
                           (token 'number "34" (pos 1 12))))

  (test-case "right assoc"
             (check-equal? (parse "parsing" (open-input-string "12 ^ 23 ^ 34"))
                           (binary '^ 12 (binary '^ 23 34))))

  (check-equal? (parse "parsing" (open-input-string "true and true = true or false"))
                (binary 'eq
                        (binary 'and 'true 'true)
                        (binary 'or 'true 'false))))
