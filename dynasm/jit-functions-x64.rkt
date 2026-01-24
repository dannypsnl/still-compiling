#lang racket/base

;;; Shared JIT functions for x64 used by tests and benchmarks
;;; x64 System V AMD64 ABI calling convention:
;;; - Arguments: RDI, RSI, RDX, RCX, R8, R9
;;; - Return: RAX

(require ffi/unsafe
         "dynasm.rkt")

(provide jit-add
         jit-mul
         jit-factorial)

;; Simple add
;; int64_t add(int64_t a, int64_t b)
(define jit-add
  (let ([buf (dynasm-create 4096)])
    ;; RAX = RDI + RSI
    (emit-x64! buf (x64-mov-reg RAX RDI))
    (emit-x64! buf (x64-add-reg RAX RSI))
    (emit-x64! buf (x64-ret))
    (make-jit-function buf (_fun _int64 _int64 -> _int64))))

;; Simple multiply
;; int64_t mul(int64_t a, int64_t b)
(define jit-mul
  (let ([buf (dynasm-create 4096)])
    ;; RAX = RDI * RSI
    (emit-x64! buf (x64-mov-reg RAX RDI))
    (emit-x64! buf (x64-imul-reg RAX RSI))
    (emit-x64! buf (x64-ret))
    (make-jit-function buf (_fun _int64 _int64 -> _int64))))

(define jit-factorial
  (let ([buf (dynasm-create 4096)])
    ;; Factorial: n in RDI
    ;; result in RAX = 1
    (emit-x64! buf (x64-mov-imm64 RAX 1))  ; RAX = 1 (accumulator)
    ;; if n <= 1, return 1
    (emit-x64! buf (x64-cmp-imm32 RDI 1))  ; compare n with 1
    (emit-x64! buf (x64-jcc-rel8 CC-LE 8)) ; jump to exit if n <= 1
    ;; loop:
    (define loop-start (dynasm-pos buf))
    (emit-x64! buf (x64-imul-reg RAX RDI)) ; RAX *= n
    (emit-x64! buf (x64-dec RDI))          ; n--
    (emit-x64! buf (x64-cmp-imm32 RDI 1))  ; compare n with 1
    (emit-x64! buf (x64-jcc-rel8 CC-G (- loop-start (+ (dynasm-pos buf) 2))))
    ;; return result in RAX
    (emit-x64! buf (x64-ret))
    (make-jit-function buf (_fun _int64 -> _int64))))
