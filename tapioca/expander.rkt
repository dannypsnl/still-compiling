#lang racket
(provide Tapioca expand-tapioca)
(require "parser.rkt")
(require nanopass
         syntax/parse)

(define (stx-number? stx)
  (syntax-parse stx
    [x:number #t]
    [else #f]))

(define-language Tapioca
  (terminals
    (syntax (loc))
    ((identifier (x)) . => . syntax->datum)
    ((stx-number (n)) . => . syntax-e)
    )
  (Top (d)
    (: loc x t)
    (define loc (x0 x1 ...) e)
    (define loc x e)
  )
  (Expr (e)
    n
    x
    (begin loc e0 ... e1)
    (lambda loc (x ...) e)
    (let loc ([x e0] ...) e)
  )
  (Type (t)
    x
    (-> loc (t0 ...) t1)
  )
  (Bind (b)
    (check e t)
  )
  (Unify (u)
    ; expected t0, actual t1
    (unify t0 t1)
  ))

(define (expand-tapioca notation)
  (with-output-language (Tapioca Top)
    (syntax-parse notation
      #:datum-literals (: define require)
      [(require pkg ...) 'require]
      [(: m : ty ...+) `(: ,notation ,#'m ,(expand-type #'(ty ...)))]
      [(define (f params ...) body* ... body)
        (define p* (syntax->list #'(params ...)))
        (define b* (map expand-expr (syntax->list #'(body* ...))))
        `(define ,notation
          (,#'f ,p* ...)
          (begin ,notation
            ,b* ...
            ,(expand-expr #'body)))]
      [(define m e) `(define ,notation ,#'m ,(expand-expr #'e))]
      [else (error 'syntax "unknown top-level: ~a" notation)])))
(define (expand-type notation)
  (with-output-language (Tapioca Type)
    (syntax-parse notation
      #:datum-literals (->)
      [(t ... -> rt)
        (define ts (map expand-type (syntax->list #'(t ...))))
        (define r (expand-type #'rt))
        `(-> ,notation (,ts ...) ,r)]
      [(m:id) #'m]
      [m:id #'m]
      [else (error 'syntax "unknown type ~a" notation)])))
(define (expand-expr notation)
  (with-output-language (Tapioca Expr)
    (syntax-parse notation
      #:datum-literals (let lambda)
      [(lambda (x:id ...) body* ... body)
        (define params (syntax->list #'(x ...)))
        (define b* (map expand-expr (syntax->list #'(body* ...))))
        (define b (expand-expr #'body))
        `(lambda ,notation (,params ...)
          (begin ,notation ,b* ... ,b))]
      [(let ([x:id e] ...) body* ... body)
        (define xs (syntax->list #'(x ...)))
        (define es (map expand-expr (syntax->list #'(e ...))))
        (define b* (map expand-expr (syntax->list #'(body* ...))))
        (define b (expand-expr #'body))
        `(let ,notation
          ([,xs ,es] ...)
          (begin ,notation ,b* ... ,b))]
      [n:number #'n]
      [x:id #'x]
      [else (error 'syntax "unknown expression ~a" notation)])))

(module+ main
  (for ([notation (parse-file "example/hello.ss")])
    (println (expand-tapioca notation)))
  )
