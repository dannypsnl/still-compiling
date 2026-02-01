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

;;; L2-CPS: adds with-cont form for CPS conversion
(define-language L2-CPS
  (extends L2)
  (Expr (e body)
        (+ (with-cont e body))))

;;; L3: CPS removes call/cc and with-cont
(define-language L3
  (extends L2-CPS)
  (Expr (e body)
        (- (call/cc e)
           (with-cont e body))))

(define-pass l2->l2-cps : L2 (e) -> L2-CPS ()
  (Expr : Expr (e) -> Expr ()))

(define-pass cps-step : L2-CPS (e) -> L2-CPS ()
  (Expr : Expr (e) -> Expr ()
        [(with-cont ,x ,body0) `(,body0 ,x)]
        [(with-cont ,n ,body0) `(,body0 ,n)]
        [(with-cont ,p ,body0) `(,body0 ,p)]
        [(with-cont (lambda (,x* ...) ,body) ,body0)
         (define $k (gensym 'k))
         `(,body0 (lambda (,x* ... ,$k)
                    (with-cont ,body ,$k)))]
        [(with-cont (let ([,x* ,e*] ...) ,body) ,body0)
         (foldr (lambda (x e acc)
                  `(with-cont ,e
                     (lambda (,x) ,acc)))
                `(with-cont ,body ,body0)
                x*
                e*)]
        [(with-cont (begin ,body* ... ,body) ,body0)
         (foldr (lambda (e acc)
                  (define r (gensym 'r))
                  `(with-cont ,e
                     (lambda (,r) ,acc)))
                `(with-cont ,body ,body0)
                body*)]
        [(with-cont (define ,x ,e0) ,body0)
         (define r (gensym 'r))
         `(with-cont ,e0
            (lambda (,r)
              (begin ,(list `(define ,x ,r)) ... (,body0 0))))]
        [(with-cont (call/cc ,e0) ,body0)
         (define fv (gensym 'fv))
         (define v (gensym 'v))
         (define dk (gensym 'dk))
         `(with-cont ,e0
            (lambda (,fv)
              (,fv (lambda (,v ,dk) (,body0 ,v)) ,body0)))]
        [(with-cont (,p ,e* ...) ,body0)
         (define r* (map (lambda (_) (gensym 'r)) e*))
         (foldr (lambda (e r acc)
                  `(with-cont ,e
                     (lambda (,r) ,acc)))
                `(,body0 (,p ,r* ...))
                e*
                r*)]
        [(with-cont (,e0 ,e* ...) ,body0)
         (define r (gensym 'r))
         (define r* (map (lambda (_) (gensym 'r)) e*))
         (foldr (lambda (e r acc)
                  `(with-cont ,e
                     (lambda (,r) ,acc)))
                `(,r ,r* ... ,body0)
                (cons e0 e*)
                (cons r r*))]))

(define-pass ensure-cps-eliminated : L2-CPS (e) -> L3 ()
  (Expr : Expr (e) -> Expr ()))

(define (cps-convert e)
  (define e-cps (l2->l2-cps e))
  (with-output-language (L2-CPS Expr)
    (let loop ([e `(with-cont ,e-cps (lambda (x) x))])
      (define e* (cps-step e))
      (if (equal? e* e)
          (ensure-cps-eliminated e)
          (loop e*)))))

(define-pass freevars : L3 (e) -> * ()
  (definitions
    (define (defined-names exprs)
      (for/fold ([names (set)])
                ([e exprs])
        (nanopass-case (L3 Expr) e
                       [(define ,x ,e) (set-add names x)]
                       [else names]))))
  (Expr : Expr (e) -> * ()
        [,x (set x)]
        [(lambda (,x* ...) ,body)
         (set-subtract (freevars body) (list->set x*))]
        [(let ([,x* ,e*] ...) ,body)
         (apply set-union (set-subtract (freevars body) (list->set x*)) (map freevars e*))]
        [(begin ,body* ... ,body)
         (define all (cons body body*))
         (set-subtract (apply set-union (map freevars all))
                       (defined-names all))]
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
