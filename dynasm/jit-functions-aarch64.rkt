#lang racket/base

;;; Shared JIT functions used by tests and benchmarks

(require ffi/unsafe
         "dynasm.rkt")

(provide jit-factorial
         jit-fibonacci
         jit-sum
         jit-gcd
         jit-add
         jit-mul)

;; Factorial: n!
(define jit-factorial
  (let ([buf (dynasm-create 4096)])
    (emit! buf (aarch64-movz X1 1 0))       ; result = 1
    (emit! buf (aarch64-cmp-imm X0 1))      ; if n <= 1
    (emit! buf (aarch64-b-cond COND-LE 4))  ;   goto end
    (emit! buf (aarch64-mul X1 X1 X0))      ; result *= n
    (emit! buf (aarch64-sub-imm X0 X0 1))   ; n--
    (emit! buf (aarch64-b -4))              ; loop
    (emit! buf (aarch64-add-reg X0 X1 XZR)) ; return result
    (emit! buf (aarch64-ret))
    (make-jit-function buf (_fun _int64 -> _int64))))

;; Fibonacci: fib(n)
(define jit-fibonacci
  (let ([buf (dynasm-create 4096)])
    (emit! buf (aarch64-cmp-imm X0 1))       ; if n <= 1
    (emit! buf (aarch64-b-cond COND-LE 11))  ;   return n
    (emit! buf (aarch64-movz X1 0 0))        ; prev2 = 0
    (emit! buf (aarch64-movz X2 1 0))        ; prev1 = 1
    (emit! buf (aarch64-sub-imm X3 X0 1))    ; count = n - 1
    (emit! buf (aarch64-add-reg X4 X1 X2))   ; loop: tmp = prev2 + prev1
    (emit! buf (aarch64-add-reg X1 X2 XZR))  ; prev2 = prev1
    (emit! buf (aarch64-add-reg X2 X4 XZR))  ; prev1 = tmp
    (emit! buf (aarch64-sub-imm X3 X3 1))    ; count--
    (emit! buf (aarch64-cmp-imm X3 0))       ; if count > 0
    (emit! buf (aarch64-b-cond COND-GT -5))  ;   goto loop
    (emit! buf (aarch64-add-reg X0 X2 XZR))  ; return prev1
    (emit! buf (aarch64-ret))
    (make-jit-function buf (_fun _int64 -> _int64))))

;; Sum 1..n
(define jit-sum
  (let ([buf (dynasm-create 4096)])
    (emit! buf (aarch64-movz X1 0 0))       ; sum = 0
    (emit! buf (aarch64-movz X2 1 0))       ; i = 1
    (emit! buf (aarch64-cmp-reg X2 X0))     ; loop: if i > n
    (emit! buf (aarch64-b-cond COND-GT 4))  ;   goto end
    (emit! buf (aarch64-add-reg X1 X1 X2))  ; sum += i
    (emit! buf (aarch64-add-imm X2 X2 1))   ; i++
    (emit! buf (aarch64-b -4))              ; goto loop
    (emit! buf (aarch64-add-reg X0 X1 XZR)) ; return sum
    (emit! buf (aarch64-ret))
    (make-jit-function buf (_fun _int64 -> _int64))))

;; GCD using Euclidean algorithm
(define jit-gcd
  (let ([buf (dynasm-create 4096)])
    (emit! buf (aarch64-cmp-imm X1 0))       ; loop: if b == 0
    (emit! buf (aarch64-b-cond COND-EQ 7))   ;   return a
    (emit! buf (aarch64-sdiv X2 X0 X1))      ; tmp = a / b
    (emit! buf (aarch64-mul X2 X2 X1))       ; tmp = tmp * b
    (emit! buf (aarch64-sub-reg X2 X0 X2))   ; tmp = a - tmp (a % b)
    (emit! buf (aarch64-add-reg X0 X1 XZR))  ; a = b
    (emit! buf (aarch64-add-reg X1 X2 XZR))  ; b = tmp
    (emit! buf (aarch64-b -7))               ; goto loop
    (emit! buf (aarch64-ret))
    (make-jit-function buf (_fun _int64 _int64 -> _int64))))

;; Simple add
(define jit-add
  (let ([buf (dynasm-create 4096)])
    (emit! buf (aarch64-add-reg X0 X0 X1))
    (emit! buf (aarch64-ret))
    (make-jit-function buf (_fun _int64 _int64 -> _int64))))

;; Simple multiply
(define jit-mul
  (let ([buf (dynasm-create 4096)])
    (emit! buf (aarch64-mul X0 X0 X1))
    (emit! buf (aarch64-ret))
    (make-jit-function buf (_fun _int64 _int64 -> _int64))))
