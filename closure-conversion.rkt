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
(define-pass remove-define-lambda : L0 (e) -> L1 ()
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
        (+ (lambda (x* ...) e)
           (let ([x* e*] ...) e))))
(define-pass begin-wrapping : L1 (e) -> L2 ()
  (Expr : Expr (e) -> Expr ()
        [(lambda (,x* ...) ,[body*] ... ,[body])
         (if (empty? body*)
             `(lambda (,x* ...) ,body)
             `(lambda (,x* ...)
                (begin ,body* ... ,body)))]
        [(let ([,x* ,e*] ...)
           ,body* ... ,body)
         (if (empty? body*)
             `(let ([,x* ,e*] ...) ,body)
             `(let ([,x* ,e*] ...)
                (begin ,body* ... ,body)))]))

(define (prim? e)
  (member e '(+ - * / cons car cdr vector vector-ref)))
(define-pass freevars : L2 (e) -> * ()
  (Expr : Expr (e) -> * ()
        [,x (set x)]
        [(lambda (,x* ...) ,e)
         (set-subtract (freevars e)
                       (list->set x*))]
        [(begin ,body* ... ,body)
         (apply set-union (map freevars (cons body body*)))]
        [(,e ,e* ...)
         (if (prim? e)
             (apply set-union (map freevars e*))
             (apply set-union (map freevars (cons e e*))))]
        [(define ,x ,e)
         (freevars e)]
        [else (set)]))

(define-pass replace-free : L2 (e $env fvs) -> L2 ()
  (Expr : Expr (e) -> Expr ()
        [,x (guard (set-member? fvs x))
            `(vector-ref ,$env ,(index-of (set->list fvs) x))]))
(define-pass closure-conversion : L2 (e) -> L2 ()
  (Expr : Expr (e) -> Expr ()
        [(lambda (,x* ...) ,[body])
         (define $lifted-function-name (gensym 'lifted))
         (define $env (gensym 'env))
         (define fvs (freevars e))
         ; convert free-vars in body by using reference to $env
         (if (set-empty? fvs)
             `(cons (lambda (,x* ... ,$env) ,body)
                    (vector))
             `(cons (lambda (,x* ... ,$env)
                      ,(replace-free body $env fvs))
                    (vector ,(set->list fvs) ...)))]))

(define-pass closure-call : L2 (e) -> L2 ()
  (Expr : Expr (e) -> Expr ()
        [(,[e] ,[e*] ...)
         (if (prim? e)
             `(,e ,e* ...)
             (let ([clos (gensym 'clos)])
               `(let ([,clos ,e])
                  ((car ,clos) ,e* ... (cdr ,clos)))))]))

(define-parser parse-L0 L0)
(define-parser parse-L2 L2)
(define (all e)
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
            remove-define-lambda
            parse-L0)
   e))

(all '(begin
        (define (make-adder n)
          (lambda (m)
            (+ m n)))
        ((make-adder 2) 3)))
