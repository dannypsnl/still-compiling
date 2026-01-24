#lang racket/base

;;; Shared JIT functions for x64 used by tests and benchmarks
;;; x64 System V AMD64 ABI calling convention:
;;; - Arguments: RDI, RSI, RDX, RCX, R8, R9
;;; - Return: RAX

(require ffi/unsafe
         "dynasm.rkt")

(provide jit-add
         jit-mul)

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
