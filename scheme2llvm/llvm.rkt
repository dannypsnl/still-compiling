#lang racket
(provide compile-to-bitcode
         compile-and-run)
(require nanopass
         (only-in ffi/unsafe ffi-lib)
         racket-llvm
         racket/runtime-path
         "compiler.rkt"
         "environment.rkt")

;;; Tag constants
(define TAG_INT    0)
(define TAG_PAIR   1)
(define TAG_VEC    2)
(define TAG_CLOS   3)
(define TAG_FLOAT  4)
(define TAG_BOOL   5)
(define TAG_NULL   6)
(define TAG_VOID   7)

(define SCM_FALSE    5)  ; (0 << 3) | TAG_BOOL
(define SCM_TRUE     13) ; (1 << 3) | TAG_BOOL
(define SCM_NULL_VAL 6)  ; TAG_NULL
(define SCM_VOID_VAL 7)  ; TAG_VOID

;;; L4: lambda-lifted version of L3-clos
(define-language L4
  (extends L3-clos)
  (Expr (e body)
        (- (lambda (x* ...) body))
        (+ (lambda-lifted x (x* ...) body))))
(define-pass lift : L3-clos (e) -> L4 ()
  (Expr : Expr (e) -> Expr ()
        [(lambda (,x* ...) ,[body])
         (define gen-name (gensym 'lambda))
         `(lambda-lifted ,gen-name (,x* ...) ,body)]))

;;; LLVM types
(define i64 (llvm-int64-type))
(define i32 (llvm-int32-type))
(define dbl (llvm-double-type))

;; Helper to build type of n-ary function
(define (fn-type-i64 n [varargs #f])
  (llvm-function-type i64 (make-list n i64) varargs))

;;; Mutable compiler state
(define mod #f)
(define builder #f)
(define fns (make-hash))

;; Runtime function type specs: (name . arg-count-or-type-spec)
(define runtime-fn-specs
  `((scm_cons          . 2)
    (scm_car           . 1)
    (scm_cdr           . 1)
    (scm_make_closure  . 2)
    (scm_closure_code  . 1)
    (scm_closure_env   . 1)
    (scm_make_vector   . 1)
    (scm_vector_ref    . 2)
    (scm_vector_set    . 3)
    (scm_vector_length . 1)
    (scm_make_float    . float)
    (scm_add           . 2)
    (scm_sub           . 2)
    (scm_mul           . 2)
    (scm_div           . 2)
    (scm_eq            . 2)
    (scm_lt            . 2)
    (scm_gt            . 2)
    (scm_le            . 2)
    (scm_ge            . 2)
    (scm_is_null       . 1)
    (scm_is_pair       . 1)
    (scm_is_number     . 1)
    (scm_is_boolean    . 1)
    (scm_is_vector     . 1)
    (scm_not           . 1)
    (scm_display       . 1)
    (scm_displayln     . 1)))

(define (make-fn-type spec)
  (cond
    [(eq? spec 'float) (llvm-function-type i64 (list dbl))]
    [(number? spec) (fn-type-i64 spec)]
    [else (error 'make-fn-type "unknown spec: ~a" spec)]))

(define (init-compiler!)
  (set! mod (llvm-module "scheme"))
  (set! builder (llvm-builder-create))
  (set! fns (make-hash))
  (for ([pair runtime-fn-specs])
    (define name (car pair))
    (define spec (cdr pair))
    (define ft (make-fn-type spec))
    (hash-set! fns name
               (cons ft (llvm-add-function mod (symbol->string name) ft)))))

;; Look up a runtime function: returns (cons type fn-ref)
(define (rt name)
  (hash-ref fns name))
(define (rt-type name) (car (rt name)))
(define (rt-fn name)   (cdr (rt name)))

;;; Compile lambdas pass
(define-pass compile-lambda : L4 (e) -> L4 ()
  (Expr : Expr (e) -> Expr ()
        [(lambda-lifted ,x (,x* ...) ,[body])
         (define lam
           (llvm-add-function mod (symbol->string x)
                              (llvm-function-type i64
                                                  (make-list (length x*) i64))))
         (define entry (llvm-append-basic-block lam))
         (llvm-builder-position-at-end builder entry)
         (define env (make-env))
         (for ([x x*]
               [i (length x*)])
           (bind! env x (llvm-get-param lam i)))
         ((compile-with env) body #:tail? #t)

         ; At this point all data are emitted to LLVM, this is a junked value, we will ignore this output
         e]))

;;; Main expression compiler
(define (compile-with [env (make-env)])
  (define (compile-expr e #:tail? [tail? #f])
    ;; Helper: emit ret if in tail position, return val either way
    (define (maybe-ret val)
      (when tail? (llvm-build-ret builder val))
      val)
    (nanopass-case
     (L4 Expr) e
     ;; Variable lookup
     [,x (maybe-ret (lookup env x))]
     ;; Integer literal: tag it (n << 3)
     [,n (maybe-ret (llvm-const-int i64 (arithmetic-shift n 3)))]
     ;; Float literal: call scm_make_float
     [,f (maybe-ret (llvm-build-call2 builder (rt-type 'scm_make_float) (rt-fn 'scm_make_float)
                                      (list (llvm-const-real dbl f))))]
     ;; Boolean literal
     [,b (maybe-ret (llvm-const-int i64 (if b SCM_TRUE SCM_FALSE)))]
     ;; Null
     [(null) (maybe-ret (llvm-const-int i64 SCM_NULL_VAL))]
     ;; Void
     [(void) (maybe-ret (llvm-const-int i64 SCM_VOID_VAL))]
     ;; If expression
     [(if ,e0 ,e1 ,e2)
      (define cond-val (compile-expr e0))
      ;; Check if cond == #f (value 5)
      (define is-false (llvm-build-icmp builder 'int-eq cond-val
                                        (llvm-const-int i64 SCM_FALSE)))
      (define current-fn (llvm-get-basic-block-parent
                          (llvm-get-insert-block builder)))
      (define then-bb (llvm-append-basic-block current-fn))
      (define else-bb (llvm-append-basic-block current-fn))
      (cond
        [tail?
         ;; In tail position: each branch emits its own ret, no merge needed
         (llvm-build-cond-br builder is-false else-bb then-bb)
         (llvm-builder-position-at-end builder then-bb)
         (compile-expr e1 #:tail? #t)
         (llvm-builder-position-at-end builder else-bb)
         (compile-expr e2 #:tail? #t)
         ;; Return dummy — caller ignores this since ret was already emitted
         (llvm-const-int i64 0)]
        [else
         (define merge-bb (llvm-append-basic-block current-fn))
         ;; Branch: if false → else, otherwise → then
         (llvm-build-cond-br builder is-false else-bb then-bb)
         ;; Then branch
         (llvm-builder-position-at-end builder then-bb)
         (define then-val (compile-expr e1))
         (define then-end-bb (llvm-get-insert-block builder))
         (llvm-build-br builder merge-bb)
         ;; Else branch
         (llvm-builder-position-at-end builder else-bb)
         (define else-val (compile-expr e2))
         (define else-end-bb (llvm-get-insert-block builder))
         (llvm-build-br builder merge-bb)
         ;; Merge with phi
         (llvm-builder-position-at-end builder merge-bb)
         (define phi (llvm-build-phi builder i64))
         (llvm-add-incoming phi (list then-val else-val)
                            (list then-end-bb else-end-bb))
         phi])]
     ;; Closure forms
     [(make-closure ,e0 ,e1)
      (maybe-ret (llvm-build-call2 builder (rt-type 'scm_make_closure) (rt-fn 'scm_make_closure)
                                   (list (compile-expr e0) (compile-expr e1))))]
     [(closure-code ,e)
      (maybe-ret (llvm-build-call2 builder (rt-type 'scm_closure_code) (rt-fn 'scm_closure_code)
                                   (list (compile-expr e))))]
     [(closure-env ,e)
      (maybe-ret (llvm-build-call2 builder (rt-type 'scm_closure_env) (rt-fn 'scm_closure_env)
                                   (list (compile-expr e))))]
     ;; Primitive applications
     [(,p ,e* ...)
      (define ne* (map compile-expr e*))
      (maybe-ret
       (case p
         [(+)  (foldl (lambda (e acc)
                        (llvm-build-call2 builder (rt-type 'scm_add) (rt-fn 'scm_add) (list acc e)))
                      (car ne*) (cdr ne*))]
         [(-)  (foldl (lambda (e acc)
                        (llvm-build-call2 builder (rt-type 'scm_sub) (rt-fn 'scm_sub) (list acc e)))
                      (car ne*) (cdr ne*))]
         [(*)  (foldl (lambda (e acc)
                        (llvm-build-call2 builder (rt-type 'scm_mul) (rt-fn 'scm_mul) (list acc e)))
                      (car ne*) (cdr ne*))]
         [(/)  (foldl (lambda (e acc)
                        (llvm-build-call2 builder (rt-type 'scm_div) (rt-fn 'scm_div) (list acc e)))
                      (car ne*) (cdr ne*))]
         [(=)  (llvm-build-call2 builder (rt-type 'scm_eq) (rt-fn 'scm_eq) ne*)]
         [(<)  (llvm-build-call2 builder (rt-type 'scm_lt) (rt-fn 'scm_lt) ne*)]
         [(>)  (llvm-build-call2 builder (rt-type 'scm_gt) (rt-fn 'scm_gt) ne*)]
         [(>=) (llvm-build-call2 builder (rt-type 'scm_ge) (rt-fn 'scm_ge) ne*)]
         [(<=) (llvm-build-call2 builder (rt-type 'scm_le) (rt-fn 'scm_le) ne*)]
         [(cons) (llvm-build-call2 builder (rt-type 'scm_cons) (rt-fn 'scm_cons) ne*)]
         [(car)  (llvm-build-call2 builder (rt-type 'scm_car) (rt-fn 'scm_car) ne*)]
         [(cdr)  (llvm-build-call2 builder (rt-type 'scm_cdr) (rt-fn 'scm_cdr) ne*)]
         [(vector)
          (define n (length ne*))
          (define vec (llvm-build-call2 builder (rt-type 'scm_make_vector) (rt-fn 'scm_make_vector)
                                        (list (llvm-const-int i64 (arithmetic-shift n 3)))))
          (for ([ne ne*]
                [i n])
            (llvm-build-call2 builder (rt-type 'scm_vector_set) (rt-fn 'scm_vector_set)
                              (list vec (llvm-const-int i64 (arithmetic-shift i 3)) ne)))
          vec]
         [(vector-ref)    (llvm-build-call2 builder (rt-type 'scm_vector_ref) (rt-fn 'scm_vector_ref) ne*)]
         [(vector-set!)   (llvm-build-call2 builder (rt-type 'scm_vector_set) (rt-fn 'scm_vector_set) ne*)]
         [(vector-length) (llvm-build-call2 builder (rt-type 'scm_vector_length) (rt-fn 'scm_vector_length) ne*)]
         [(null?)    (llvm-build-call2 builder (rt-type 'scm_is_null) (rt-fn 'scm_is_null) ne*)]
         [(pair?)    (llvm-build-call2 builder (rt-type 'scm_is_pair) (rt-fn 'scm_is_pair) ne*)]
         [(number?)  (llvm-build-call2 builder (rt-type 'scm_is_number) (rt-fn 'scm_is_number) ne*)]
         [(boolean?) (llvm-build-call2 builder (rt-type 'scm_is_boolean) (rt-fn 'scm_is_boolean) ne*)]
         [(vector?)  (llvm-build-call2 builder (rt-type 'scm_is_vector) (rt-fn 'scm_is_vector) ne*)]
         [(not)      (llvm-build-call2 builder (rt-type 'scm_not) (rt-fn 'scm_not) ne*)]
         [(display)  (llvm-build-call2 builder (rt-type 'scm_display) (rt-fn 'scm_display) ne*)]
         [(displayln) (llvm-build-call2 builder (rt-type 'scm_displayln) (rt-fn 'scm_displayln) ne*)]))]
     ;; Closure / indirect function call
     [(,e ,e* ...)
      (define ft (llvm-function-type i64 (make-list (length e*) i64)))
      (define f (llvm-build-int->ptr builder
                                     (compile-expr e)
                                     (llvm-pointer-type ft)))
      (define call (llvm-build-call2 builder ft f (map compile-expr e*)))
      (when tail?
        (llvm-set-tail-call-kind call 2)  ; 2 = musttail
        (llvm-build-ret builder call))
      call]
     ;; Begin
     [(begin ,body* ... ,body)
      (for-each compile-expr body*)
      (compile-expr body #:tail? tail?)]
     ;; Define
     [(define ,x ,e)
      (define ne (compile-expr e))
      (bind! env x ne)
      (maybe-ret ne)]
     ;; Lambda-lifted reference: convert function pointer to i64
     [(lambda-lifted ,x (,x* ...) ,body)
      (maybe-ret (llvm-build-ptr->int builder
                                      (llvm-get-named-function mod (symbol->string x))
                                      i64))]
     ;; Let
     [(let ([,x* ,e*] ...) ,body)
      (define new-env (extend env))
      (for ([x x*] [e e*])
        (bind! new-env x (compile-expr e)))
      ((compile-with new-env) body #:tail? tail?)]
     [else (error 'compile-with "unhandled expression: ~a" e)]))
  compile-expr)

;;; Compile a given S-expression, write LLVM bitcode to file
(define (compile-to-bitcode expr bc-path)
  (init-compiler!)
  (define lifted (lift (compile/main expr)))
  (compile-lambda lifted)
  (define main-func (llvm-add-function mod "main" (llvm-function-type i32)))
  (define entry (llvm-append-basic-block main-func))
  (llvm-builder-position-at-end builder entry)
  ((compile-with) lifted)
  (llvm-build-ret builder (llvm-const-int i32 0))
  (llvm-write-bitcode-to-file mod bc-path))

;;; Untag a raw i64 tagged value into a Racket value
(define (untag-value v rt-lib)
  (local-require ffi/unsafe)
  (define tag (bitwise-and v 7))
  (cond
    [(= tag TAG_INT)  (arithmetic-shift v -3)]
    [(= tag TAG_BOOL) (= v SCM_TRUE)]
    [(= tag TAG_NULL) '()]
    [(= tag TAG_VOID) (void)]
    [(= tag TAG_FLOAT)
     (define scm-get-float
       (get-ffi-obj "scm_get_float" rt-lib (_fun _int64 -> _double)))
     (scm-get-float v)]
    [(= tag TAG_PAIR)
     (define scm-car
       (get-ffi-obj "scm_car" rt-lib (_fun _int64 -> _int64)))
     (define scm-cdr
       (get-ffi-obj "scm_cdr" rt-lib (_fun _int64 -> _int64)))
     (cons (untag-value (scm-car v) rt-lib)
           (untag-value (scm-cdr v) rt-lib))]
    [(= tag TAG_VEC)
     (define scm-vector-length
       (get-ffi-obj "scm_vector_length" rt-lib (_fun _int64 -> _int64)))
     (define scm-vector-ref
       (get-ffi-obj "scm_vector_ref" rt-lib (_fun _int64 _int64 -> _int64)))
     (define len (arithmetic-shift (scm-vector-length v) -3))
     (for/vector ([i len])
       (untag-value (scm-vector-ref v (arithmetic-shift i 3)) rt-lib))]
    [else v]))

(define-runtime-path runtime-lib "runtime")
;;; Compile and run (use LLVM JIT), return untagged Racket value
(define (compile-and-run forms)
  (init-compiler!)
  (define expr `(begin ,@forms))
  (define lifted (lift (compile/main expr)))
  (compile-lambda lifted)
  ;; Create __entry returning i64 (tagged value)
  (define entry-func (llvm-add-function mod "__entry" (llvm-function-type i64)))
  (define entry-bb (llvm-append-basic-block entry-func))
  (llvm-builder-position-at-end builder entry-bb)
  (define result ((compile-with) lifted))
  (llvm-build-ret builder result)
  ;; Load runtime and JIT
  (define rt-lib (ffi-lib runtime-lib #:global? #t))
  (llvm-link-in-mcjit)
  (define engine (llvm-create-execution-engine-for-module mod))
  (define raw (llvm-generic-value->int (llvm-run-function engine entry-func '()) #t))
  (untag-value raw rt-lib))
