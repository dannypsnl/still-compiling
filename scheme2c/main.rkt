#lang racket
(require nanopass)

(define-language scm
  (terminals 
    (symbol [x name])
    (integer [n])
    (char [c])
    (boolean [b])
    (vector [v])
    (string [s]))
  (Expr [e body]
        n
        c
        b
        v
        s
        name
        (define name e)
        (define (name x* ...) body* ... body)
        (begin e* ... e)
        (lambda (x* ...) body* ... body)
        (let ([x* e*] ...) body* ... body)
        (if e0 e1 e2)
        (e0 e1 ...)))

(define-parser parse-scm scm)

(define-pass 2c : scm (e) -> * ()
  (E : Expr (e) -> * ()
    [,n (printf "make_int(~a)" n)]
    [else (void)]))

(define (all-passes form)
  (printf "#include \"scm.h\"\n\n")
  (printf "scm_t scheme_entry() {\n")
  (printf "  return ")
  ((compose
    2c
    parse-scm) form)
  (printf ";\n")
  (printf "}\n")
  )

(all-passes '1)
