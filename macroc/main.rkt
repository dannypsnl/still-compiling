#lang racket
(require syntax/parse/define
         (for-syntax syntax/stx
                     racket/string
                     racket/system)
         ffi/unsafe)

(begin-for-syntax
  (define-syntax-class c/ty
    #:datum-literals (double)
    (pattern double #:attr ffi-type #'_double))
  (define-syntax-class c/expr
    (pattern n:number #:attr to-c (format "~a" (syntax->datum #'n)))
    (pattern x:id #:attr to-c (format "~a" (syntax->datum #'x)))
    (pattern (+ a:c/expr ...)
      #:attr to-c (string-join (attribute a.to-c) "+")))

  (define-syntax-class c/func
    #:datum-literals (: -> =>)
    (pattern (define (name:id p:id ...) : p-ty:c/ty ... -> r-ty:c/ty =>
               e:c/expr)
      #:attr ffi-type #'(_fun p-ty.ffi-type ... -> r-ty.ffi-type)
      #:attr to-c
      (format "EXPORT~n~a ~a(~a) { return ~a; }"
              (syntax->datum #'r-ty)
              (syntax->datum #'name)
              (string-join (stx-map (λ (ty p-name)
                                      (format "~a ~a" (syntax->datum ty) (syntax->datum p-name)))
                                    #'(p-ty ...)
                                    #'(p ...))
                           ", ")
              (attribute e.to-c)))))

(define-syntax-parser define-c/calc
  [(_ form:c/func ...+)
   (define filename "_build/tmp.c")
   (define out (open-output-file filename #:mode 'text #:exists 'replace))
   (displayln "#define EXPORT __attribute__((visibility(\"default\")))" out)
   (for ([c-def (attribute form.to-c)])
     (display c-def out))
   (close-output-port out)
   (system "clang -c _build/tmp.c -o _build/tmp.o")
   (system "clang -dynamiclib _build/tmp.o -o _build/libtmp.dylib")
   #'(module c racket
       (provide define-tmp)
       (require ffi/unsafe
                ffi/unsafe/define)
       (define-ffi-definer define-tmp (ffi-lib "libtmp"))

       (define-tmp form.name form.ffi-type)
       ...

       (provide form.name ...))])

(define-c/calc
  (define (add_double_in_C a b) : double double -> double =>
    (+ a b 3)))

(require 'c)
(println (add_double_in_C 1. 2.))
