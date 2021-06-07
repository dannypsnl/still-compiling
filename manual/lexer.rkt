#lang racket

(provide lex lexer-tokens
         (struct-out token)
         (struct-out pos))

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

(define keyword*
  '("true" "false" "and" "or"))

; state functions
(define (lex-white-space l)
  (let loop ([c (peek l)])
    (when (and (not (eof-object? c))
               (or (char-whitespace? c)
                   (end-of-line? c)))
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
    [#\^ (next l)
         (emit l '^)
         lex-white-space]
    [#\= (next l)
         (emit l 'eq)
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
  (when (not (scan-number? l))
    (error 'bad-number-syntax "bad number syntax: `~a`"
           (peek-string (- (lexer-offset l) (lexer-start l))
                        0 (lexer-input l))))
  (emit l 'number)
  lex-white-space)

; helpers
(struct pos (line column) #:transparent)

(struct lexer
  (name
   input ; list char
   state
   ; for report position as format: (line, column)
   line column
   ; lexing helpers
   offset start
   tokens)
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
  (channel-put (lexer-tokens l) (token ty value (pos (lexer-line l) (lexer-column l)))))

(define keyword=>keyword-type
  (make-hash
   (map (λ (word)
          (cons word (string->symbol word)))
        keyword*)))
(define (emit l ty)
  (define value
    (read-string (- (lexer-offset l) (lexer-start l))
                 (lexer-input l)))
  (match value
    [(? eof-object?) (new-item l 'EOF value)]
    [else (if (hash-ref keyword=>keyword-type value #f)
              (new-item l (hash-ref keyword=>keyword-type value) value)
              (new-item l ty value))])
  (set-lexer-start! l (lexer-offset l)))

(define (accept? l valid)
  (cond
    [(and (char? (peek l))
          (string-contains? valid (string (peek l))))
     (next l)
     #t]
    [else #f]))

(define (accept-run l valid)
  (let loop ([c (peek l)])
    (when (and (char? c) (string-contains? valid (string c)))
      (next l)
      (loop (peek l)))))

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
  (cond
    [(alpha-numeric? (peek l))
     (next l)
     #f]
    [else #t]))

(define (alpha-numeric? c)
  (and (char? c)
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
             (define l (lex "test" (open-input-string "31+12 abc")))
             (check-equal? (channel-get (lexer-tokens l))
                           (token 'number "31" (pos 1 2)))
             (check-equal? (channel-get (lexer-tokens l))
                           (token 'add "+" (pos 1 3)))
             (check-equal? (channel-get (lexer-tokens l))
                           (token 'number "12" (pos 1 5)))
             (check-equal? (channel-get (lexer-tokens l))
                           (token 'identifier "abc" (pos 1 9))))

  (test-case "spacing"
             (define l (lex "test" (open-input-string "  abc")))
             (check-equal? (channel-get (lexer-tokens l))
                           (token 'identifier "abc" (pos 1 5))))

  (test-case "keywords"
             (define l (lex "test" (open-input-string "true false")))
             (check-equal? (channel-get (lexer-tokens l))
                           (token 'true "true" (pos 1 4)))
             (check-equal? (channel-get (lexer-tokens l))
                           (token 'false "false" (pos 1 10))))

  (test-case "longer expression"
             (define l (lex "test" (open-input-string "1 + 2 * 3")))
             (check-equal? (channel-get (lexer-tokens l))
                           (token 'number "1" (pos 1 1)))
             (check-equal? (channel-get (lexer-tokens l))
                           (token 'add "+" (pos 1 3)))
             (check-equal? (channel-get (lexer-tokens l))
                           (token 'number "2" (pos 1 5)))
             (check-equal? (channel-get (lexer-tokens l))
                           (token 'mul "*" (pos 1 7)))
             (check-equal? (channel-get (lexer-tokens l))
                           (token 'number "3" (pos 1 9))))

  )
