#lang racket
(require nanopass
         racket-llvm
         "closure-conversion.rkt")

(define-language L3
  (extends L2)
  (Expr (e body)
        (- (lambda (x* ...) body))
        (+ (lambda-lifted x (x* ...) body))))
(define-pass lift : L2 (e) -> L3 ()
  (Expr : Expr (e) -> Expr ()
        [(lambda (,x* ...) ,[body])
         (define gen-name (gensym 'lambda))
         `(lambda-lifted ,gen-name (,x* ...) ,body)]))

(define mod (llvm-module "scheme"))
(define builder (llvm-builder-create))
(define llvm-printf-type (llvm-function-type (llvm-int64-type)
                                             (list (llvm-pointer-type (llvm-int8-type)))
                                             #t))
(define llvm-printf (llvm-add-function mod "printf"
                                       llvm-printf-type))
(define pair-type (llvm-array-type (llvm-int64-type) 2))
(define (wrap-pair-GEP pair index)
  (llvm-build-gep2 builder pair-type pair (list (llvm-const-int (llvm-int64-type) 0) (llvm-const-int (llvm-int64-type) index))))
(define llvm-cons-type (llvm-function-type (llvm-int64-type)
                                           (list (llvm-int64-type) (llvm-int64-type))))
(define llvm-cons (llvm-add-function mod "cons" llvm-cons-type))
(define llvm-car-type (llvm-function-type (llvm-int64-type)
                                          (list (llvm-int64-type))))
(define llvm-car (llvm-add-function mod "car"
                                    llvm-car-type))
(define llvm-cdr-type (llvm-function-type (llvm-int64-type)
                                          (list (llvm-int64-type))))
(define llvm-cdr (llvm-add-function mod "cdr"
                                    llvm-cdr-type))
(define llvm-vector-ref-type (llvm-function-type (llvm-int64-type)
                                                 (list (llvm-int64-type) (llvm-int64-type))))
(define llvm-vector-ref (llvm-add-function mod "vector_ref"
                                           llvm-vector-ref-type))

(define-pass compile-lambda : L3 (e) -> L3 ()
  (Expr : Expr (e) -> Expr ()
        [(lambda-lifted ,x (,x* ...) ,[body])
         (define lam
           (llvm-add-function mod (symbol->string x)
                              (llvm-function-type (llvm-int64-type)
                                                  (make-list (length x*) (llvm-int64-type)))))
         (define entry (llvm-append-basic-block lam))
         (llvm-builder-position-at-end builder entry)
         (define vars (make-hash))
         (for ([x x*]
               [i (length x*)])
           (hash-set! vars x (llvm-get-param lam i)))
         (llvm-build-ret builder ((compile-with vars) body))
         e]))

(define (compile-with [vars (make-hash)])
  (define (compile-expr e)
    (nanopass-case
     (L3 Expr) e
     [,x (hash-ref vars x)]
     [,n (llvm-const-int (llvm-int64-type) n)]
     [(,p ,e* ...)
      (define ne* (map compile-expr e*))
      (case p
        [(+) (foldl (lambda (e acc)
                      (llvm-build-add builder acc e))
                    (car ne*)
                    (cdr ne*))]
        [(-) (foldl (lambda (e acc)
                      (llvm-build-sub builder acc e))
                    (car ne*)
                    (cdr ne*))]
        [(*) (foldl (lambda (e acc)
                      (llvm-build-mul builder acc e))
                    (car ne*)
                    (cdr ne*))]
        [(/) (foldl (lambda (e acc)
                      (llvm-build-sdiv builder acc e))
                    (car ne*)
                    (cdr ne*))]
        [(cons) (llvm-build-call2 builder llvm-cons-type llvm-cons ne*)]
        [(car) (llvm-build-call2 builder llvm-car-type llvm-car ne*)]
        [(cdr) (llvm-build-call2 builder llvm-cdr-type llvm-cdr ne*)]
        [(vector) (define vec (llvm-build-array-malloc builder pair-type (llvm-const-int (llvm-int64-type) (length ne*))))
                  (for ([ne ne*]
                        [i (length ne*)])
                    (llvm-build-store builder ne (wrap-pair-GEP vec i)))
                  (llvm-build-ptr->int builder
                                       vec
                                       (llvm-int64-type))]
        [(vector-ref) (llvm-build-call2 builder llvm-vector-ref-type llvm-vector-ref ne*)])]
     [(,e ,e* ...)
      (define ft (llvm-function-type (llvm-int64-type)
                                     (make-list (length e*) (llvm-int64-type))))
      (define f (llvm-build-int->ptr builder
                                     (compile-expr e)
                                     (llvm-pointer-type ft)))
      (llvm-build-call2 builder ft f (map compile-expr e*))]
     [(begin ,body* ... ,body)
      (for-each compile-expr body*)
      (compile-expr body)]
     [(define ,x ,e)
      (define ne (compile-expr e))
      (hash-set! vars x ne)
      (void)]
     [(lambda-lifted ,x (,x* ...) ,body)
      (llvm-build-ptr->int builder
                           (llvm-get-named-function mod (symbol->string x))
                           (llvm-int64-type))]
     [(let ([,x* ,e*] ...) ,body)
      (define new-vars (hash-copy vars))
      (for ([x x*] [e e*])
        (hash-set! new-vars x (compile-expr e)))
      ((compile-with new-vars) body)]
     [else (error 'should-not-go-into-here)]))
  compile-expr)

(module+ main
  ((compose (lambda (e)
              (define main-func (llvm-add-function mod "main" (llvm-function-type (llvm-int32-type))))
              (define entry (llvm-append-basic-block main-func))
              (llvm-builder-position-at-end builder entry)
              ; ==========
              (define str (llvm-build-string-ptr builder "Compile to LLVM. Answer is %d!\n"))
              (llvm-build-call2 builder
                                llvm-printf-type
                                llvm-printf
                                (list str ((compile-with) e)))
              ; ==========
              (llvm-build-ret builder (llvm-const-int (llvm-int32-type) 0))
              (void))
            compile-lambda
            lift
            transform)
   '(begin
      (define (make-adder n)
        (lambda (m)
          (+ 1 n m)))
      ((make-adder 2) 3)))
  (displayln (llvm-module->string mod)))
