#lang nanopass
(require racket/sandbox)

(define-language L0
  (terminals
   (symbol [x])
   (number [n]))
  (Expr (e body)
        x
        n
        (vector e* ...)
        (begin body* ... body)
        (lambda (x* ...) body* ... body)
        (define (x x* ...)
          body* ... body)
        (define x e)
        (e e* ...)))

(define-language L1
  (extends L0)
  (Expr (e body)
        (- (define (x x* ...) body* ... body))))
(define-pass remove-define-lambda : L0 (e) -> L1 ()
  (Expr : Expr (e) -> Expr ()
        [(define (,x ,x* ...) ,[body*] ... ,[body])
         `(define ,x
            (lambda (,x* ...)
              ,body* ... ,body))]))

(define-language L2
  (extends L1)
  (Expr (e body)
        (- (lambda (x* ...) body* ... body))
        (+ (lambda (x* ...) e))))
(define-pass begin-wrapping : L1 (e) -> L2 ()
  (Expr : Expr (e) -> Expr ()
        [(lambda (,x* ...) ,[body*] ... ,[body])
         (if (empty? body*)
             `(lambda (,x* ...) ,body)
             `(lambda (,x* ...)
                (begin ,body* ... ,body)))]))

(define-language L3
  (extends L2)
  (Expr (e body)
        (+ (prim e e* ...))))
(define-pass explicit-prim-call : L2 (e) -> L3 ()
  (Expr : Expr (e) -> Expr ()
        [(,[e] ,[e*] ...)
         (if (member e '(+ - * / cons car cdr vector vector-ref))
             `(prim ,e ,e* ...)
             `(,e ,e* ...))]))

(define-pass freevars : L3 (e) -> * ()
  (Expr : Expr (e) -> * ()
        [,x (set x)]
        [(lambda (,x* ...) ,e)
         (set-subtract (freevars e)
                       (list->set x*))]
        [(begin ,body* ... ,body)
         (apply set-union (map freevars (cons body body*)))]
        [(,e ,e* ...) (apply set-union (map freevars (cons e e*)))]
        [(prim ,e ,e* ...) (apply set-union (map freevars e*))]
        [(define ,x ,e)
         (freevars e)]
        [else (set)]))

(define-language L4
  (extends L3))

(define-pass replace-free : L4 (e $env fvs) -> L4 ()
  (Expr : Expr (e) -> Expr ()
        [,x (guard (set-member? fvs x))
            `(prim vector-ref ,$env ,(index-of (set->list fvs) x))]))
(define-pass closure-conversion : L3 (e) -> L4 ()
  (Expr : Expr (e) -> Expr ()
        [(lambda (,x* ...) ,[body])
         (define $lifted-function-name (gensym 'lifted))
         (define $env (gensym 'env))
         (define fvs (freevars e))
         ; convert free-vars in body by using reference to $env
         (if (set-empty? fvs)
             `(prim cons (lambda (,x* ... ,$env) ,body)
                    (prim vector))
             `(prim cons (lambda (,x* ... ,$env)
                           ,(replace-free body $env fvs))
                    (prim vector ,(set->list fvs) ...)))]))

(define-pass closure-call : L4 (e) -> L4 ()
  (Expr : Expr (e) -> Expr ()
        [(,[e] ,[e*] ...)
         `((prim car ,e) ,e* ... (prim cdr  ,e))]))

(define-parser parse-L0 L0)
(define-parser parse-L4 L4)
(define (all e)
  ((compose unparse-L4
            closure-call
            closure-conversion
            explicit-prim-call
            begin-wrapping
            remove-define-lambda
            parse-L0)
   e))

(define target
  '(begin
     (define (make-adder n)
       (lambda (m)
         (+ m n)))
     ((make-adder 2) 3)))

(all target)
(define ev (make-evaluator 'racket
                           '(require syntax/parse/define)
                           '(struct closure (l e))
                           '(define (prim f . a) (apply f a))
                           '(define-syntax-parser lifted-lambda
                              [(_ name (x ...) b ...) #'(lambda (x ...) b ...)])
                           ))
(ev (all target))
