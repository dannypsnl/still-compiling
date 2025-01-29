#lang racket
(require nanopass)

(define (primitive? sym)
  (set-member?
    (set 'cons 'car 'cdr)
    sym))

(define-language L0
  (terminals 
    (primitive [p])
    (symbol [x name])
    (integer [n])
    (char [c])
    (boolean [b]))
  (Expr (e body)
        n
        c
        b
        name
        (begin e* ... e)
        (let ([x* e*] ...) body* ... body)
        (if e0 e1 e2)
        ; (p e* ...)
        (e0 e1 ...)
        ))
(define-parser parse-scm L0)

(define-language L1
  (extends L0)
  (Expr (e body)
    (- (let ([x* e*] ...) body* ... body))
    (+ (set! x e))))
(define-pass low-level-let : L0 (e) -> L1 ()
  (E : Expr (e) -> Expr ()
    [(let ([,x* ,[e*]] ...) ,body* ... ,body)
      (define binds
        (for/list ([x x*] [e e*])
          `(set! ,x ,e)))
      `(begin ,binds ...
              ,body* ... ,body)]))

(define-language L2
  (extends L1)
  (Simple (s)
    (+ n c b name
      (s s* ...)))
  (Expr (e body)
    (- n
       c
       b
       name)
    (+ s)))
(define-pass remove-complex-operands : L1 (e) -> L2 ()
  (Atom : Expr (e) -> * ()
    [,x (values x #f)]
    [,n (values n #f)]
    [,c (values c #f)]
    [,b (values b #f)]
    [else
      (define new-var (gensym 'complex))
      (define bind
        (with-output-language (L2 Expr)
          `(set! ,new-var ,(E e))))
      (values new-var bind)])
  (E : Expr (e) -> Expr ()
    [(if ,e ,[e1] ,[e2])
      (define-values (a bind) (Atom e))
      (if bind
        `(begin
          ,bind
          (if ,a ,e1 ,e2))
        `(if ,a ,e1 ,e2))]
    [(,e0 ,e* ...)
      (define v (mutable-set))
      (define-values (a a-binding) (Atom e0))
      (set-add! v a-binding)
      (define b*
        (for/list ([e e*])
          (define-values (b b-binding) (Atom e))
          (set-add! v b-binding)
          b))
      `(begin
        ,(set->list v) ...
        (,a ,b* ...))])
  (E e))

(define-language Final
  (extends L2)
  (Expr (e body)
    (+ (return e))))
(define-pass wrap-return : L2 (e) -> Final ()
  (E : Expr (e) -> Expr ())
  `(return ,(E e)))
(define-pass explicate-tail : Final (e) -> Final ()
  (E : Expr (e) -> Expr ()
    [(return (begin ,e* ... ,e))
      `(begin ,e* ... ,(E e))]
    [(return (if ,e0 ,e1 ,e2))
      `(if ,e0 ,(E e1) ,(E e2))])
  (E e))

(define used-variables (mutable-set))
(define-pass 2c : Final (e) -> * ()
  (S : Simple (s) -> * ()
    [,x (printf "~a" x)]
    [,n (printf "make_int(~a)" n)]
    [,c (printf "make_char('~a');" c)]
    [,b (if b (printf "make_bool(true)") (printf "make_bool(false)"))]
    [else (error 'missing)])
  (E : Expr (e) -> * ()
    [,s (S s)]
    [(return ,e)
      (printf "return ")
      (E e)
      (printf ";")]
    [(begin ,e* ... ,e)
      (for-each E e*)
      (E e)]
    [(set! ,x ,e)
      (set-add! used-variables x)
      (printf "~a = " x)
      (E e)
      (printf ";\n")]
    [(if ,e ,e1 ,e2)
      (printf "if (to_bool(")
      (E e)
      (printf ")) {")
      (E e1)
      (printf "} else {")
      (E e2)
      (printf "}")]
    [else (error 'missing "handling ~a" e)])
  (E e))

(define (all-passes form)
  (printf "#include \"scm.h\"\n\n")
  (printf "scm_t scheme_entry() {\n")
  (define expr (open-output-string ""))
  (parameterize ([current-output-port expr])
    ((compose
      2c
      explicate-tail
      wrap-return
      remove-complex-operands
      low-level-let
      parse-scm) form))
  (unless (set-empty? used-variables)
    (printf "scm_t "))
  (for ([x (in-set used-variables)]
        [k (in-naturals)])
    (if (= k 0)
      (printf "~a" x)
      (printf ", ~a" x)))
  (unless (set-empty? used-variables)
    (printf ";\n"))

  (printf (get-output-string expr))

  (printf "}\n"))

(define out (open-output-file "tmp.c"
  #:mode 'text
  #:exists 'truncate/replace))
(parameterize ([current-output-port out])
  (all-passes '(if (let ([y #t]) y) 1 2)))
