#lang racket
(require nanopass
         racket/sandbox)

(define (primitive? x)
  (member x '(; arithmetic
              +
              -
              *
              /
              ; pair
              cons car cdr
              ; vector
              vector
              vector-ref)))
(define-language L0
  (terminals
   (primitive (p))
   (symbol (x))
   (number (n)))
  (Expr (e body)
        x
        n
        p
        (begin body* ... body)
        (lambda (x* ...) body* ... body)
        (let ([x* e*] ...)
          body* ... body)
        (define (x x* ...)
          body* ... body)
        (define x e)
        (e e* ...)))

(define-language L1
  (extends L0)
  (Expr (e body)
        (- (define (x x* ...) body* ... body))))
(define-pass remove-define-procedure-form : L0 (e) -> L1 ()
  (Expr : Expr (e) -> Expr ()
        [(define (,x ,x* ...) ,[body*] ... ,[body])
         `(define ,x
            (lambda (,x* ...)
              ,body* ... ,body))]))

(define-language L2
  (extends L1)
  (Expr (e body)
        (- (lambda (x* ...) body* ... body)
           (let ([x* e*] ...)
             body* ... body))
        (+ (lambda (x* ...) body)
           (let ([x* e*] ...) body))))
(define-pass begin-wrapping : L1 (e) -> L2 ()
  (definitions
    (define (wrap body* body)
      (if (empty? body*)
          body
          `(begin ,body* ... ,body))))
  (Expr : Expr (e) -> Expr ()
        [(lambda (,x* ...) ,[body*] ... ,[body])
         `(lambda (,x* ...) ,(wrap body* body))]
        [(let ([,x* ,[e*]] ...) ,[body*] ... ,[body])
         `(let ([,x* ,e*] ...) ,(wrap body* body))]))

(define-pass freevars : L2 (e) -> * ()
  (Expr : Expr (e) -> * ()
        [,x (set x)]
        [(lambda (,x* ...) ,body)
         (set-subtract (freevars body) (list->set x*))]
        [(let ([,x* ,e*] ...) ,body)
         (apply set-union
                (cons (set-subtract (freevars body) (list->set x*))
                      (map freevars e*)))]
        [(begin ,body* ... ,body) (apply set-union (map freevars (cons body body*)))]
        [(,p ,e* ...) (apply set-union (map freevars e*))]
        [(,e ,e* ...) (apply set-union (map freevars (cons e e*)))]
        [(define ,x ,e) (freevars e)]
        [else (set)]))

(define-pass replace-free : L2 (e $env fvs) -> L2 ()
  (Expr : Expr (e) -> Expr ()
        [,x (guard (set-member? fvs x))
            `(vector-ref ,$env ,(index-of (set->list fvs) x))]))
(define-pass closure-conversion : L2 (e) -> L2 ()
  (Expr : Expr (e) -> Expr ()
        [(lambda (,x* ...) ,[body])
         (define $env (gensym '$env))
         (define fvs (freevars e))
         ; convert free-vars in body by using reference to $env
         `(cons (lambda (,x* ... ,$env) ,(replace-free body $env fvs))
                (vector ,(set->list fvs) ...))]))

(define-pass closure-call : L2 (e) -> L2 ()
  (Expr : Expr (e) -> Expr ()
        [(,p ,[e*] ...)
         `(,p ,e* ...)]
        [(,[e] ,[e*] ...)
         (if (symbol? e)
             `((car ,e) ,e* ... (cdr ,e))
             `(let ([clos ,e])
                ((car clos) ,e* ... (cdr clos))))]))

(define (all e)
  (define-parser parse-L0 L0)
  (define-parser parse-L1 L1)
  (define-parser parse-L2 L2)

  (define (!debug-L1 e)
    (println (unparse-L1 e))
    e)
  (define (!debug-L2 e)
    (println (unparse-L2 e))
    e)

  ((compose (lambda (e)
              (displayln "gen code:")
              (pretty-display e)
              (define ev (make-evaluator 'racket))
              (displayln "result:")
              (ev e))
            unparse-L2
            closure-call
            closure-conversion
            begin-wrapping
            remove-define-procedure-form
            parse-L0)
   e))

(all '(begin
        (define (make-adder n)
          (lambda (m)
            (+ m n)))
        ((make-adder 2) 3)))
