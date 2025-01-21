#lang racket
(require "parser.rkt"
         "expander.rkt"
         "tyenv.rkt")
(require nanopass
         reporter)

(define-pass collect-ty : (Tapioca Top) (d env) -> * (bool)
  (T : Top (d) -> * ()
    [(: ,loc ,x ,t)
      (insert env x t)
      #f]
    [(define ,loc ,x ,e) #t]
    [(define ,loc (,x0 ,x1 ...) ,e)
      #t])
  (T d))

(define (infer env e)
  (nanopass-case (Tapioca Expr) e
    [,n #'int-literal]
    [,x (lookup env x)]
    [(begin ,loc ,e0 ... ,e1)
      (infer env e1)]
    [(let ,loc ([,x ,e0] ...) ,e)
      (infer env e)]
    [(lambda ,loc (,x ...) ,e)
      (raise "cannot infer lambda")]
    ))

(define-pass check-pass : (Tapioca Bind) (env b) -> * ()
  (B : Bind (b) -> * ()
    [(check (lambda ,loc (,x0 ...) ,e) (-> ,loc1 (,t0 ...) ,t1))
      (define env- (newenv env))
      (for ([x x0]
            [t t0])
        (insert env- x t))
      (check env- e t1)
      ]
    [(check ,e ,t)
      (unify t (infer env e))])
  (B b))
(define (check env e t)
  (with-output-language (Tapioca Bind)
    (check-pass env `(check ,e ,t))))

(define-pass unify-pass : (Tapioca Unify) (u) -> * ()
  (U : Unify (u) -> * ()
    [(unify (-> ,loc (,t0 ...) ,t1) (-> ,loc1 (,t2 ...) ,t3))
      (for ([e t0]
            [a t2])
        (unify e a))
      (unify t1 t3)]
    [(unify ,t0 ,t1)
      (match (cons (syntax->datum t0) (syntax->datum t1))
        [(cons 'i32 'int-literal) (void)]
        [(cons 'int 'int-literal) (void)]
        [(cons 'int 'int) (void)]
        [_ (raise
          (report
            #:error-code "E0001"
            #:message "type mismatching"
            #:target t1
            #:labels (list
                       (label t0 "expected type"
                         #:color 'blue)
                       (label t1 "actual type"
                         #:color 'red))
            #:hint (format "expected type `~a`, found type `~a`" t0 t1)))])])
  (U u))
(define (unify expected actual)
  (with-output-language (Tapioca Unify)
    (unify-pass `(unify ,expected ,actual))))

(module+ main
  (define tops
    (for/list ([notation (parse-file "example/hello.ss")])
      (expand-tapioca notation)))
  (define env (newenv))
  (define defs (filter (lambda (t) (collect-ty t env)) tops))

  (for ([d defs])
    (nanopass-case (Tapioca Top) d
      [(define ,loc ,x ,e)
        (check env e (lookup env x))]
      [(define ,loc (,x0 ,x1 ...) ,e)
        (define lam (with-output-language (Tapioca Expr)
          `(lambda ,loc (,x1 ...) ,e)))
        (check env lam (lookup env x0))]))
  )
