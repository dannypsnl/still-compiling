#lang curly-fn nanopass
(require syntax/parse)

(define (stx-number? stx)
  (syntax-parse stx
    [x:number #t]
    [else #f]))

(define-language L0
  (terminals
   (syntax [stx])
   ((identifier (x)) . => . syntax->datum)
   ((stx-number (n)) . => . syntax-e))
  (Expr (e body)
        n
        x
        ; abstraction
        (λ stx (x* ...) e) => (λ (x* ...) e)
        ; application
        (stx e e* ...) => (e e* ...)))

(define-pass parse : * (stx) -> L0 ()
  (Expr : * (stx) -> Expr (expr)
        (syntax-parse stx
          #:literals (λ)
          ; lambda form
          [(λ (param*:id ...) expr)
           `(λ ,stx (,(syntax->list #'(param* ...)) ...) ,(parse #'expr))]
          [(f arg* ...)
           `(,stx ,(parse #'f) ,(map parse (syntax->list #'(arg* ...))) ...)]
          ; literal expression
          [x #:when (ormap (λ (pred?) (pred? stx)) (list identifier? stx-number?))
             #'x]
          [else (error 'syntax "unknown form: ~a" stx)]))
  (Expr stx))

(define-pass unique-subst : L0 (e env) -> L0 ()
  (Expr : Expr (e) -> Expr ()
        [,x (cdr (assoc (syntax->datum x) env))]
        [(λ ,stx (,x* ...) ,e)
         (define new-id* (map unique-id* x*))
         (define new-env (map #{cons (syntax->datum %1) %2}
                              x* new-id*))
         `(λ ,stx (,new-id* ...)
            ,(unique-subst e (append new-env env)))]))
(define-pass unique-id* : L0 (e) -> L0 ()
  (Expr : Expr (e) -> Expr ()
        [,x (syntax-property
             x
             'lookup (gensym (syntax->datum x)))]
        [(λ ,stx (,x* ...) ,e)
         (define new-id* (map unique-id* x*))
         (define env (map #{cons (syntax->datum %1) %2}
                          x* new-id*))
         `(λ ,stx (,new-id* ...) ,(unique-subst e env))]
        [(,stx ,[e] ,[e*] ...) `(,stx ,e ,e* ...)]))

(define (make-env x* e*)
  (map #{cons (syntax-property %1 'lookup) %2}
       x* e*))
(define (lookup x env)
  (define rel (assoc (syntax-property x 'lookup) env))
  (and rel (cdr rel)))
(define-pass β-subst : L0 (e env) -> L0 ()
  (Expr : Expr (e) -> Expr ()
        [,x (or (lookup x env) x)]))
(define-pass β-reduce : L0 (e) -> L0 ()
  (Expr : Expr (e) -> Expr ()
        [(λ ,stx (,x* ...) ,e)
         `(λ ,stx (,x* ...) ,e)]
        [(,stx ,[e] ,[e*] ...)
         (nanopass-case (L0 Expr) e
                        [(λ ,stx (,x* ...) ,e)
                         (β-subst e (make-env x* e*))]
                        [else `(,stx ,e ,e* ...)])]))

(define (all origin)
  ((compose β-reduce
            unique-id*
            parse)
   origin))

(all #'x)
(all #'((λ (x) x) 1))
(all #'((λ (x) x) a))
(all #'(λ (x) (λ (x) x)))
(all #'(λ (y) (λ (x) y)))
(all #'((((λ (x) (λ (x) x)) a) b) 1))
(all #'((((λ (x) (λ (x) x)) b) a) 1))
(all #'(a b))
