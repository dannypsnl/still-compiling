#lang racket

(provide lex)

; export
(define (lex name input-port)
  (define l (lexer name
                   input-port
                   ; state
                   lex-white-space
                   ; line column
                   1 0
                   ; start offset
                   0 0
                   (make-channel)))
  (thread (λ () (run l)))
  l)

; state functions
(define (lex-white-space l)
  (let loop ([c (peek l)])
    (when (or (eof-object? c)
              (char-whitespace? c)
              (end-of-line? c))
      (next l)
      (loop (peek l))))
  (ignore l)

  (match (peek l)
    [(? eof-object?) (emit l 'EOF)
                     #f]
    [#\+ (next l)
         (emit l 'add)
         lex-white-space]
    [#\- (next l)
         (emit l 'sub)
         lex-white-space]
    [#\* (next l)
         (emit l 'mul)
         lex-white-space]
    [#\/ (next l)
         (emit l 'div)
         lex-white-space]
    [(? char-numeric?) lex-number]
    [(? alpha-numeric?) lex-identifier]
    [c (error 'unknown "don't know what to do with: `~a`" c)]))

(define (lex-identifier l)
  (let loop ([c (peek l)])
    (when (alpha-numeric? c)
      (next l)
      (loop (peek l))))

  (emit l 'identifier)
  lex-white-space)

(define (lex-number l)
  (when (scan-number? l)
    (error 'bad-number-syntax "bad number syntax: `~a`"
           (read-string (- (lexer-offset l) (lexer-start l))
                        (lexer-input l))))
  (emit l 'number)
  lex-white-space)

; helpers
(struct pos (line column) #:transparent)

(struct lexer
  (name
   input ; list char
   state
   ; for report position as format: (line, pos)
   line column
   ; lexing helpers
   offset start
   items)
  #:mutable
  #:transparent)

(struct token (typ val pos) #:transparent)

(define (run lexer)
  (when (lexer-state lexer)
    (set-lexer-state! lexer
                      ((lexer-state lexer) lexer))
    (run lexer)))

(define (next l)
  (set-lexer-offset! l (add1 (lexer-offset l)))
  (set-lexer-column! l (add1 (lexer-column l)))
  (define c (peek-char (lexer-input l)
                       (- (lexer-offset l) (lexer-start l))))
  (if (eof-object? c)
      c
      (when (end-of-line? c)
        (set-lexer-line! l (add1 (lexer-line l)))
        (set-lexer-column! l 0))))

(define (peek l) (peek-char (lexer-input l)
                            (- (lexer-offset l) (lexer-start l))))

(define (new-item l ty value)
  (channel-put (lexer-items l) (token ty value (pos (lexer-line l) (lexer-column l)))))

(define (emit l ty)
  (define value
    (read-string (- (lexer-offset l) (lexer-start l))
                 (lexer-input l)))
  (match value
    [(? eof-object?) (new-item l 'EOF value)]
    ["true" (new-item l 'true value)]
    ["false" (new-item l 'false value)]
    [else (new-item l ty value)])
  (set-lexer-start! l (lexer-offset l)))

(define (accept? l valid)
  (if (and (char? (peek l))
           (string-contains? valid (string (peek l))))
      (let ()
        (next l)
        #t)
      #f))

(define (accept-run l valid)
  (when (and (char? (peek l))
             (string-contains? valid (string (peek l))))
    (next l)))

(define (ignore l)
  (read-string (- (lexer-offset l) (lexer-start l))
               (lexer-input l))
  (set-lexer-start! l (lexer-offset l)))

(define (scan-number? l)
  (define digits "0123456789")
  (when (and (accept? l "0") (accept? l "xX"))
    (set! digits "0123456789abcdefABCDEF"))
  (accept-run l digits)
  (when (accept? l ".")
    (accept-run l digits))
  (when (accept? l "eE")
    (accept? l "+-")
    (accept-run l "0123456789"))
  ; Next thing mustn't be alphanumeric.
  (define c (peek l))
  (if (alpha-numeric? c)
      (let ()
        (next l)
        #f)
      #t))

(define (alpha-numeric? c)
  (if (eof-object? c)
      #f
      (or (char-ci=? c #\_)
          (char-alphabetic? c)
          (char-numeric? c))))

(define (end-of-line? c)
  (match c
    [#\newline #t]
    [else #f]))

; Test
(module+ test
  (require rackunit)

  (test-case "lexing"
             (define l (lex "test" (open-input-string "31+12abc")))
             (check-equal? (channel-get (lexer-items l))
                           (token 'number "31" (pos 1 2)))
             (check-equal? (channel-get (lexer-items l))
                           (token 'add "+" (pos 1 3)))
             (check-equal? (channel-get (lexer-items l))
                           (token 'number "12" (pos 1 5)))
             (check-equal? (channel-get (lexer-items l))
                           (token 'identifier "abc" (pos 1 8))))

  (test-case "spacing"
             (define l (lex "test" (open-input-string "  abc")))
             (check-equal? (channel-get (lexer-items l))
                           (token 'identifier "abc" (pos 1 5))))

  (test-case "keywords"
             (define l (lex "test" (open-input-string "true false")))
             (check-equal? (channel-get (lexer-items l))
                           (token 'true "true" (pos 1 4)))
             (check-equal? (channel-get (lexer-items l))
                           (token 'false "false" (pos 1 10))))

  )

