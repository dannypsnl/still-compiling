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
    [(let ([,x* ,e*] ...) ,body* ... ,body)
      (define binds
        (for/list ([x x*] [e e*])
          `(set! ,x ,e)))
      `(begin
        ,binds ...
        ,body* ... ,body)]))

(define-language Final
  (extends L1)
  (Expr (e body)
    (+ (return e))))
(define-pass auto : L1 (e) -> Final ()
  (E : Expr (e) -> Expr ()))
(define-pass wrap-return : L1 (e) -> Final ()
  `(return ,(auto e)))

(define-pass proper-return : Final (e) -> Final ()
  (E : Expr (e) -> Expr ()
    [(return (begin ,e* ... ,e))
      `(begin ,e* ... (return ,e))]
    [(return (if ,e0 ,e1 ,e2))
      `(if ,e0 (return ,e1) (return ,e2))]
    [else e]))

(define used-variables (mutable-set))
(define-pass 2c : Final (e) -> * ()
  (E : Expr (e) -> * ()
    [(return ,e)
      (printf "return ")
      (E e)
      (printf ";")]
    [,x (printf "~a" x)]
    [,n (printf "make_int(~a)" n)]
    [,c (printf "make_char('~a');" c)]
    [,b (if b (printf "make_bool(true)") (printf "make_bool(false)"))]
    [(begin ,e* ... ,e)
      (for-each E e*)
      (E e)]
    [(set! ,x ,e)
      (set-add! used-variables x)
      (printf "~a = " x)
      (E e)
      (printf ";\n")]
    [else (void)])
  (E e))

(define (all-passes form)
  (printf "#include \"scm.h\"\n\n")
  (printf "scm_t scheme_entry() {\n")
  (define expr (open-output-string ""))
  (parameterize ([current-output-port expr])
    ((compose
      2c
      proper-return
      wrap-return
      low-level-let
      parse-scm) form))
  (printf "scm_t ")
  (for ([x (in-set used-variables)])
    (printf " ~a " x))
  (printf ";\n")
  (printf (get-output-string expr))
  (printf "}\n"))

(all-passes '(let ([x 1]) x))
