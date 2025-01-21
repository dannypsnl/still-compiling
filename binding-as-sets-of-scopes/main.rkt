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

(define (expand-expr stx)
  (with-output-language (L0 Expr)
    (syntax-parse stx
      #:datum-literals (let lambda)
      [(lambda (x:id ...) body* ... body)
        (parameterize ([current-scopes-set (set-add (current-scopes-set) (gensym 'lam))])
          (define xs (ids #'(x ...)))
          (for ([x xs]) (insert-renaming x))
          (define b* (map expand-expr (syntax->list #'(body* ...))))
          (define b (expand-expr #'body))
          `(lambda (,xs ...) ,b* ... ,b))]
      [(let ([x:id e] ...) body* ... body)
        (parameterize ([current-scopes-set (set-add (current-scopes-set) (gensym 'let))])
          (define xs (ids #'(x ...)))
          (for ([x xs]) (insert-renaming x))
          (define es (map expand-expr (syntax->list #'(e ...))))
          (define b* (map expand-expr (syntax->list #'(body* ...))))
          (define b (expand-expr #'body))
          `(let ([,xs ,es] ...)
            ,b* ... ,b))]
      [n:number (syntax->datum #'n)]
      [x:id (find-binding (stx->bind-id #'x))]
      [else (error 'syntax "unknown expression ~a" stx)])))

(define (ids stx)
  (map stx->bind-id (syntax->list stx)))
(define (stx->bind-id stx)
  (bind-id (syntax->datum stx) (current-scopes-set)))

(module+ main
  (expand-expr #'(let ([x 1])
                  (let ([y 2])
                    x)))

  (expand-expr #'(let ([x 1])
                  (let ([x 2])
                    x)))
  )
