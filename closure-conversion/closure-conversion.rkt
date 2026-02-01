#lang racket
(provide L3
         transform)
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
        (call/cc e)
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

;;; L3: CPS removes call/cc
(define-language L3
  (extends L2)
  (Expr (e body)
        (- (call/cc e))))

;;; CPS Transformation (L2 -> L3)
;;; Converts to continuation-passing style and eliminates call/cc.
(define (cps-convert e)
  (cps e
       ; meta-continuation k is a Racket identity function
       (lambda (v) v)))

(define (cps e k)
  (with-output-language (L3 Expr)
    (nanopass-case (L2 Expr) e
                   [,x (k x)]
                   [,n (k n)]
                   [(lambda (,x* ...) ,body)
                    (let* ([kp (gensym 'k)]
                           [params (append x* (list kp))])
                      (k `(lambda (,params ...)
                            ,(cps body (lambda (v) `(,kp ,(list v) ...))))))]
                   [(let ([,x* ,e*] ...) ,body)
                    (cps-let x* e* body k)]
                   [(begin ,body* ... ,body)
                    (cps-seq (append body* (list body)) k)]
                   [(define ,x ,e)
                    (cps e (lambda (v)
                             `(begin ,(list `(define ,x ,v)) ... ,(k 0))))]
                   [(call/cc ,e)
                    (cps-callcc e k)]
                   [(,p ,e* ...)
                    (cps-prim-args p e* k)]
                   [(,e ,e* ...)
                    (cps e (lambda (fv)
                             (cps-app-args fv e* k)))])))

(define (cps-prim-args p args k)
  (with-output-language (L3 Expr)
    ((for/fold ([cont (lambda (acc)
                        (k `(,p ,(reverse acc) ...)))])
               ([arg (reverse args)])
       (lambda (acc)
         (cps arg (lambda (v) (cont (cons v acc))))))
     '())))

(define (cps-app-args fv args k)
  (with-output-language (L3 Expr)
    ((for/fold ([cont (lambda (acc)
                        (define rv (gensym 'rv))
                        (define c `(lambda (,(list rv) ...) ,(k rv)))
                        (define all-args (append (reverse acc) (list c)))
                        `(,fv ,all-args ...))])
               ([arg (reverse args)])
       (lambda (acc)
         (cps arg (lambda (v) (cont (cons v acc))))))
     '())))

(define (cps-callcc f-expr k)
  (with-output-language (L3 Expr)
    (cps f-expr
         (lambda (fv)
           (define v-cap (gensym 'v))
           (define dk (gensym 'dk))
           (define rv (gensym 'rv))
           (define k-captured `(lambda (,(list v-cap dk) ...) ,(k v-cap)))
           (define k-return `(lambda (,(list rv) ...) ,(k rv)))
           `(,fv ,(list k-captured k-return) ...)))))

(define (cps-seq exprs k)
  (if (null? (cdr exprs))
      (cps (car exprs) k)
      (cps (car exprs)
           (lambda (_v)
             (cps-seq (cdr exprs) k)))))

(define (cps-let xs es body k)
  (with-output-language (L3 Expr)
    (if (null? xs)
        (cps body k)
        (cps (car es)
             (lambda (v)
               `(let ([,(list (car xs)) ,(list v)] ...)
                  ,(cps-let (cdr xs) (cdr es) body k)))))))

(define-pass freevars : L3 (e) -> * ()
  (Expr : Expr (e) -> * ()
        [,x (set x)]
        [(lambda (,x* ...) ,body)
         (set-subtract (freevars body) (list->set x*))]
        [(let ([,x* ,e*] ...) ,body)
         (apply set-union (set-subtract (freevars body) (list->set x*)) (map freevars e*))]
        [(begin ,body* ... ,body) (apply set-union (map freevars (cons body body*)))]
        [(,p ,e* ...) (apply set-union (map freevars e*))]
        [(,e ,e* ...) (apply set-union (map freevars (cons e e*)))]
        [(define ,x ,e) (freevars e)]
        [else (set)]))

(define-pass replace-free : L3 (e $env fvs) -> L3 ()
  (Expr : Expr (e) -> Expr ()
        [,x (guard (set-member? fvs x))
            `(vector-ref ,$env ,(index-of (set->list fvs) x))]))
(define-pass closure-conversion : L3 (e) -> L3 ()
  (Expr : Expr (e) -> Expr ()
        [(lambda (,x* ...) ,[body])
         (define $env (gensym '$env))
         (define fvs (freevars e))
         ; convert free-vars in body by using reference to $env
         `(cons (lambda (,x* ... ,$env) ,(replace-free body $env fvs))
                (vector ,(set->list fvs) ...))]))

(define-pass closure-call : L3 (e) -> L3 ()
  (Expr : Expr (e) -> Expr ()
        [(,p ,[e*] ...)
         `(,p ,e* ...)]
        [(,[e] ,[e*] ...)
         (if (symbol? e)
             `((car ,e) ,e* ... (cdr ,e))
             `(let ([clos ,e])
                ((car clos) ,e* ... (cdr clos))))]))

(define (transform e)
  (define-parser parse-L0 L0)
  ((compose closure-call
            closure-conversion
            cps-convert
            begin-wrapping
            remove-define-procedure-form
            parse-L0)
   e))

(module+ main
  (define (all e)
    (define-parser parse-L2 L2)
    (define-parser parse-L3 L3)

    (define (!debug-L2 e)
      (println (unparse-L2 e))
      e)
    (define (!debug-L3 e)
      (println (unparse-L3 e))
      e)

    ((compose (lambda (e)
                (displayln "gen code:")
                (pretty-display e)
                (define ev (make-evaluator 'racket))
                (displayln "result:")
                (ev e))
              unparse-L3
              transform)
     e))

  (all '(begin
          (define (make-adder n)
            (lambda (m) (+ m n)))
          (+ 1 (call/cc (lambda (k) (k ((make-adder 2) 3))))))))
