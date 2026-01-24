#lang racket/base

;;; Shared JIT functions for x64 used by tests and benchmarks
;;; x64 System V AMD64 ABI calling convention:
;;; - Arguments: RDI, RSI, RDX, RCX, R8, R9
;;; - Return: RAX

(require ffi/unsafe
         "dynasm.rkt")

(provide jit-factorial
         jit-fibonacci
         jit-sum
         jit-gcd
         jit-add
         jit-mul)

;; Factorial: n!
;; int64_t factorial(int64_t n)
;; Uses: RDI=n, RAX=result, RCX=counter
(define jit-factorial
  (let ([buf (dynasm-create 4096)])
    ;; RAX = 1 (result)
    (x64-mov-imm32 buf RAX 1)
    ;; if n <= 1, goto end
    (x64-cmp-imm8 buf RDI 1)
    (x64-jcc-rel8 buf CC-LE 15)  ; skip to end (after loop)
    ;; loop:
    ;;   RAX = RAX * RDI
    (x64-imul-reg buf RAX RDI)
    ;;   RDI--
    (x64-dec buf RDI)
    ;;   if RDI > 1, goto loop
    (x64-cmp-imm8 buf RDI 1)
    (x64-jcc-rel8 buf CC-G -13) ; back to imul
    ;; end: return RAX
    (x64-ret buf)
    (make-jit-function buf (_fun _int64 -> _int64))))

;; Fibonacci: fib(n)
;; Uses: RDI=n, RAX=prev2, RCX=prev1, RDX=tmp, RSI=counter
(define jit-fibonacci
  (let ([buf (dynasm-create 4096)])
    ;; if n <= 1, return n
    (x64-cmp-imm8 buf RDI 1)
    (x64-jcc-rel8 buf CC-G 5)   ; if n > 1, skip to main code
    (x64-mov-reg buf RAX RDI)   ; return n
    (x64-ret buf)
    ;; n > 1:
    ;; RAX = 0 (prev2)
    (x64-xor-reg buf RAX RAX)
    ;; RCX = 1 (prev1)
    (x64-mov-imm32 buf RCX 1)
    ;; RSI = n - 1 (counter)
    (x64-mov-reg buf RSI RDI)
    (x64-dec buf RSI)
    ;; loop:
    ;;   RDX = RAX + RCX (tmp = prev2 + prev1)
    (x64-mov-reg buf RDX RAX)
    (x64-add-reg buf RDX RCX)
    ;;   RAX = RCX (prev2 = prev1)
    (x64-mov-reg buf RAX RCX)
    ;;   RCX = RDX (prev1 = tmp)
    (x64-mov-reg buf RCX RDX)
    ;;   RSI-- (counter--)
    (x64-dec buf RSI)
    ;;   if RSI > 0, goto loop
    (x64-test-reg buf RSI RSI)
    (x64-jcc-rel8 buf CC-NZ -22) ; back to mov rdx, rax
    ;; return RCX (prev1)
    (x64-mov-reg buf RAX RCX)
    (x64-ret buf)
    (make-jit-function buf (_fun _int64 -> _int64))))

;; Sum 1..n
;; Uses: RDI=n, RAX=sum, RCX=i
(define jit-sum
  (let ([buf (dynasm-create 4096)])
    ;; RAX = 0 (sum)
    (x64-xor-reg buf RAX RAX)
    ;; RCX = 1 (i)
    (x64-mov-imm32 buf RCX 1)
    ;; loop:
    ;;   if RCX > RDI, goto end
    (x64-cmp-reg buf RCX RDI)
    (x64-jcc-rel8 buf CC-G 10)  ; jump to ret
    ;;   RAX += RCX
    (x64-add-reg buf RAX RCX)
    ;;   RCX++
    (x64-inc buf RCX)
    ;;   goto loop
    (x64-jmp-rel8 buf -14)      ; back to cmp
    ;; end: return RAX
    (x64-ret buf)
    (make-jit-function buf (_fun _int64 -> _int64))))

;; GCD using Euclidean algorithm
;; int64_t gcd(int64_t a, int64_t b)
;; Uses: RDI=a, RSI=b, RAX/RDX for division
(define jit-gcd
  (let ([buf (dynasm-create 4096)])
    ;; loop:
    ;;   if RSI == 0, return RDI
    (x64-test-reg buf RSI RSI)
    (x64-jcc-rel8 buf CC-NZ 5)  ; if b != 0, continue
    (x64-mov-reg buf RAX RDI)   ; return a
    (x64-ret buf)
    ;;   RAX = RDI (a)
    (x64-mov-reg buf RAX RDI)
    ;;   CQO (sign-extend RAX to RDX:RAX)
    (x64-cqo buf)
    ;;   IDIV RSI (RAX = RDX:RAX / RSI, RDX = RDX:RAX % RSI)
    (x64-idiv-reg buf RSI)
    ;;   RDI = RSI (a = b)
    (x64-mov-reg buf RDI RSI)
    ;;   RSI = RDX (b = a % b)
    (x64-mov-reg buf RSI RDX)
    ;;   goto loop
    (x64-jmp-rel8 buf -26)      ; back to test
    (make-jit-function buf (_fun _int64 _int64 -> _int64))))

;; Simple add
;; int64_t add(int64_t a, int64_t b)
(define jit-add
  (let ([buf (dynasm-create 4096)])
    ;; RAX = RDI + RSI
    (x64-mov-reg buf RAX RDI)
    (x64-add-reg buf RAX RSI)
    (x64-ret buf)
    (make-jit-function buf (_fun _int64 _int64 -> _int64))))

;; Simple multiply
;; int64_t mul(int64_t a, int64_t b)
(define jit-mul
  (let ([buf (dynasm-create 4096)])
    ;; RAX = RDI * RSI
    (x64-mov-reg buf RAX RDI)
    (x64-imul-reg buf RAX RSI)
    (x64-ret buf)
    (make-jit-function buf (_fun _int64 _int64 -> _int64))))
