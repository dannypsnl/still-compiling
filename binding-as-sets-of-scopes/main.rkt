#lang racket
(require nanopass
         syntax/parse)
(require racket/set)

(struct bind-id
  (written-name scopes-set)
  #:transparent)

(define-language L0
  (terminals
    (bind-id (x))
    (number (n)))
  (Expr (e body)
    (let ([x* e*] ...) body* ... body)
    (lambda (x* ...) body* ... body)
    x
    n))

(define current-scopes-set (make-parameter (set)))
(define renaming-map (make-hash))
(define (insert-renaming x)
  (hash-update! renaming-map (bind-id-written-name x)
    (lambda (l) (cons x l)) ; updater
    (list) ; default value
    ))
(define (find-binding x)
  (define bs (hash-ref renaming-map (bind-id-written-name x)))
  (for/first ([b bs]
             #:when (subset? (bind-id-scopes-set b) (bind-id-scopes-set x)))
    b))

#| simple macro concept
1. we store a pair that (scopes-set stx)
2. when expand we replay the scopes-set for stx expansion
3. therefore, the output expression will reference to current identifier
|#
(define macros (make-hash))
(define (store id-stx stx)
  (hash-set! macros (syntax->datum id-stx)
    (cons (current-scopes-set) stx)))
(define (load-macro id-stx)
  (hash-ref macros (syntax->datum id-stx) #f))

(define (expand-expr stx)
  (with-output-language (L0 Expr)
    (syntax-parse stx
      #:datum-literals (let lambda let-syntax syntax)
      [(let-syntax [m:id (syntax e)] body)
        (store #'m #'e)
        (expand-expr #'body)]
      [(lambda (x:id ...) body* ... body)
        (parameterize ([current-scopes-set (set-add (current-scopes-set) (gensym 'lam))])
          (define xs (ids #'(x ...)))
          (for ([x xs]) (insert-renaming x))
          (define b* (map expand-expr (syntax->list #'(body* ...))))
          (define b (expand-expr #'body))
          `(lambda (,xs ...) ,b* ... ,b))]
      [(let ([x:id e] ...) body* ... body)
        (define es (map expand-expr (syntax->list #'(e ...))))
        (parameterize ([current-scopes-set (set-add (current-scopes-set) (gensym 'let))])
          (define xs (ids #'(x ...)))
          (for ([x xs]) (insert-renaming x))
          (define b* (map expand-expr (syntax->list #'(body* ...))))
          (define b (expand-expr #'body))
          `(let ([,xs ,es] ...)
            ,b* ... ,b))]
      [n:number (syntax->datum #'n)]
      [x:id
        (match (load-macro #'x)
          [#f (stx->bind-id #'x)]
          [(cons scopes macro-stx)
            (parameterize ([current-scopes-set (set-add scopes (gensym 'intro))])
              (expand-expr macro-stx))])]
      [else (error 'syntax "unknown expression ~a" stx)])))

(define (ids stx)
  (map stx->bind-id (syntax->list stx)))
(define (stx->bind-id stx)
  (bind-id (syntax->datum stx) (current-scopes-set)))

(trace-define-pass renaming : (L0 Expr) (e) -> (L0 Expr) ()
  (Expr : Expr (e) -> Expr ()
    [,x (find-binding x)]))

(module+ main
  (define passes
    (compose
      renaming
      expand-expr))

  (passes #'(let ([x 1])
                  (let ([y 2])
                    x)))

  (passes #'(let ([x 1])
                  (let ([x 2])
                    x)))

  (passes #'(let ([x 1])
                  (let-syntax [m #'x]
                    (let ([x 2])
                      m))))
  )
