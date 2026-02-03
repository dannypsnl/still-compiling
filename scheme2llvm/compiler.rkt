#lang racket
(provide L3-clos L0
         compile/main
         debug-after)
(require nanopass)

(define (primitive? x)
  (member x '(; arithmetic
              + - * /
                ; comparison
                = < > >= <=
                ; pair
                cons car cdr
                ; vector
                vector vector-ref vector-set! vector-length
                ; string
                string-ref string-set! string-length string-append
                ; predicates
                null? pair? number? boolean? vector? string? procedure?
                ; logic
                not
                ; io
                display displayln)))

(define (flonum? x) (and (number? x) (inexact? x)))
(define (fixnum? x) (and (number? x) (exact? x) (integer? x)))
(define (scm-string? x) (string? x))

(define-language L-surface
  (terminals
   (primitive (p))
   (symbol (x))
   (fixnum (n))
   (flonum (f))
   (boolean (b))
   (scm-string (s)))
  (Expr (e body)
        x
        n
        f
        b
        s
        p
        (null)
        (void)
        (if e0 e1 e2)
        (and e* ...)
        (or e* ...)
        (list e* ...)
        (begin body* ... body)
        (lambda (x* ...) body* ... body)
        (let ([x* e*] ...)
          body* ... body)
        (define (x x* ...)
          body* ... body)
        (define x e)
        (call/cc e)
        (e e* ...)))

(define-language L0
  (extends L-surface)
  (Expr (e body)
        (- (and e* ...)
           (or e* ...)
           (list e* ...))))

(define-pass desugar : L-surface (e) -> L0 ()
  (Expr : Expr (e) -> Expr ()
        [(and) #t]
        [(and ,[e]) e]
        [(and ,[e0] ,e* ...)
         `(if ,e0 ,(Expr (with-output-language (L-surface Expr) `(and ,e* ...))) #f)]
        [(or) #f]
        [(or ,[e]) e]
        [(or ,e0 ,e* ...)
         (define t (gensym 'or-tmp))
         `(let ([,t ,(Expr e0)])
            (if ,t ,t ,(Expr (with-output-language (L-surface Expr) `(or ,e* ...)))))]
        [(list) `(null)]
        [(list ,[e*] ...)
         (foldr (lambda (el acc)
                  (with-output-language (L0 Expr) `(cons ,el ,acc)))
                `(null)
                e*)]))

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
        [(with-cont ,f ,body0) `(,body0 ,f)]
        [(with-cont ,b ,body0) `(,body0 ,b)]
        [(with-cont ,s ,body0) `(,body0 ,s)]
        [(with-cont ,p ,body0) `(,body0 ,p)]
        [(with-cont (null) ,body0) `(,body0 (null))]
        [(with-cont (void) ,body0) `(,body0 (void))]
        [(with-cont (if ,e0 ,e1 ,e2) ,body0)
         (define r (gensym 'r))
         `(with-cont ,e0
            (lambda (,r)
              (if ,r
                  (with-cont ,e1 ,body0)
                  (with-cont ,e2 ,body0))))]
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

(define-pass l2-cps->l3 : L2-CPS (e) -> L3 ()
  (Expr : Expr (e) -> Expr ()))

(define (cps-conversion e)
  (with-output-language (L2-CPS Expr)
    (define e-cps (l2->l2-cps e))
    (let loop ([e `(with-cont ,e-cps (lambda (x) x))])
      (define e* (cps-step e))
      (cond
        [(equal? e* e)  (l2-cps->l3 e*)]
        [else (loop e*)]))))

(define-pass beta-reduce : L3 (e) -> L3 ()
  (Expr : Expr (e) -> Expr ()
        [(,[e0] ,[e*] ...)
         (nanopass-case (L3 Expr) e0
                        [(lambda (,x* ...) ,body)
                         (guard (= (length x*) (length e*)))
                         `(let ([,x* ,e*] ...) ,body)]
                        [else `(,e0 ,e* ...)])]))

(define (propagatable? e)
  (nanopass-case (L3 Expr) e
                 [,x #t]
                 [,n #t]
                 [,f #t]
                 [,b #t]
                 [,s #t]
                 [,p #t]
                 [(null) #t]
                 [(void) #t]
                 [else #f]))

(define-pass constant-propagate : L3 (e) -> L3 ()
  (definitions
    (define subst (make-hash))
    (define (replace/propagated x)
      (if (hash-has-key? subst x)
          (hash-ref subst x)
          x)))
  (Expr : Expr (e) -> Expr ()
        [,x (replace/propagated x)]
        [(let ([,x* ,e*] ...) ,body)
         ;; Process bindings: propagate if value is constant/variable
         (define-values (kept-x kept-e)
           (for/fold ([kx '()] [ke '()])
                     ([x x*] [e e*])
             (define ne (Expr e))
             (cond
               ;; Propagatable: substitute directly
               [(propagatable? ne)
                (hash-set! subst x ne)
                (values kx ke)]
               [(= (use-times x body) 1)
                (hash-set! subst x ne)
                (values kx ke)]
               ;; Otherwise keep the binding
               [else (values (cons x kx) (cons ne ke))])))
         (define new-body (Expr body))
         ;; Clean up substitutions
         (for ([x x*]) (hash-remove! subst x))
         ;; Build result
         (if (empty? kept-x)
             new-body
             `(let ([,kept-x ,kept-e] ...) ,new-body))]))

(define (use-times x body)
  (define counter (make-hash))

  (let/ec return
    (define (T e)
      (nanopass-case (L3 Expr) e
                     [,x0
                      (when (eq? x x0)
                        (hash-update! counter x
                                      (lambda (v) (add1 v))
                                      0))]
                     [(if ,e0 ,e1 ,e2)
                      (T e0)
                      (T e1)
                      (T e2)]
                     [(begin ,body* ... ,body)
                      (for ([body body*]) (T body))
                      (T body)]
                     [(,p ,e* ...)
                      (for ([e e*]) (T e))]
                     [(,e ,e* ...)
                      (T e)
                      (for ([e e*]) (T e))]
                     [(define ,x0 ,e)
                      (T e)
                      (when (eq? x x0)
                        (return (hash-ref counter x 0)))]
                     [(let ([,x* ,e*] ...) ,body)
                      (for ([e e*]) (T e))
                      (unless (for/or ([x0 x*]) (eq? x x0))
                        (T body))]
                     [(lambda (,x* ...) ,body)
                      (unless (for/or ([x0 x*]) (eq? x x0))
                        (T body))]
                     [else (void)]))

    (T body)
    (hash-ref counter x 0)))

;;; Remove unused let bindings
(define-pass eliminate-deadcode : L3 (e) -> L3 ()
  (Expr : Expr (e) -> Expr ()
        [(let ([,x* ,e*] ...) ,[body])
         (define used (freevars body))
         (define-values (kept-x kept-e effects)
           (for/fold ([kx '()]
                      [ke '()]
                      [effects '()])
                     ([x x*] [e e*])
             (define ne (Expr e))
             (cond
               [(set-member? used x)
                (values (cons x kx) (cons ne ke) effects)]
               [else
                (values kx ke (cons ne effects))])))
         (cond
           [(and (empty? kept-x) (empty? effects)) body]
           [(empty? kept-x)
            `(begin
               ,(reverse effects) ...
               ,body)]
           [else
            `(let ([,(reverse kept-x) ,(reverse kept-e)] ...)
               (begin
                 ,(reverse effects) ...
                 ,body))])]))

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
        [,n (set)]
        [,f (set)]
        [,b (set)]
        [,s (set)]
        [(null) (set)]
        [(void) (set)]
        [(if ,e0 ,e1 ,e2) (set-union (freevars e0) (freevars e1) (freevars e2))]
        [(lambda (,x* ...) ,body)
         (set-subtract (freevars body) (list->set x*))]
        [(let ([,x* ,e*] ...) ,body)
         (apply set-union (set-subtract (freevars body) (list->set x*)) (map freevars e*))]
        [(begin ,body* ... ,body)
         (define all (cons body body*))
         (set-subtract (apply set-union (map freevars all))
                       (defined-names all))]
        [(,p ,e* ...) (apply set-union (set) (map freevars e*))]
        [(,e ,e* ...) (apply set-union (set) (map freevars (cons e e*)))]
        [(define ,x ,e) (freevars e)]
        [else (set)]))

;;; L3-clos: closure-converted form
(define-language L3-clos
  (extends L3)
  (Expr (e body)
        (+ (make-closure e0 e1)
           (closure-code e)
           (closure-env e))))

(define-pass replace-free : L3-clos (e $env fvs) -> L3-clos ()
  (Expr : Expr (e) -> Expr ()
        [,x (guard (set-member? fvs x))
            `(vector-ref ,$env ,(index-of (set->list fvs) x))]))
(define-pass closure-conversion : L3 (e) -> L3-clos ()
  (Expr : Expr (e) -> Expr ()
        [(lambda (,x* ...) ,[body])
         (define $env (gensym '$env))
         (define fvs (freevars e))
         `(make-closure (lambda (,x* ... ,$env) ,(replace-free body $env fvs))
                        (vector ,(set->list fvs) ...))]))

(define-pass closure-call : L3-clos (e) -> L3-clos ()
  (Expr : Expr (e) -> Expr ()
        [(if ,[e0] ,[e1] ,[e2])
         `(if ,e0 ,e1 ,e2)]
        [(,p ,[e*] ...)
         `(,p ,e* ...)]
        [(,[e] ,[e*] ...)
         (if (symbol? e)
             `((closure-code ,e) ,e* ... (closure-env ,e))
             `(let ([clos ,e])
                ((closure-code clos) ,e* ... (closure-env clos))))]))

;;; Debug machinery
(define debug-after (make-parameter #f))

(define ((make-debug-pass pass name) x)
  (define result (pass x))
  (when (equal? (debug-after) name)
    (printf "=== after ~a ===~n" name)
    (pretty-print result)
    (newline))
  result)

(define (compile/main e)
  (define-parser parse-L-surface L-surface)
  (define (pass p name)
    (if (debug-after)
        (make-debug-pass p name)
        p))
  ((compose (pass closure-call 'closure-call)
            (pass closure-conversion 'closure-conversion)
            (pass eliminate-deadcode 'eliminate-deadcode)
            (pass constant-propagate 'constant-propagate)
            (pass beta-reduce 'beta-reduce)
            (pass cps-conversion 'cps-conversion)
            (pass begin-wrapping 'begin-wrapping)
            (pass remove-define-procedure-form 'remove-define-procedure-form)
            (pass desugar 'desugar)
            parse-L-surface)
   e))
