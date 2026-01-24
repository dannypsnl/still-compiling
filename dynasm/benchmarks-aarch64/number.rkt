#lang racket/base

;;; Scalar JIT vs Native Racket Benchmarks

(require "common.rkt"
         "../jit-functions-aarch64.rkt")
(require racket/format
         math/number-theory)

(provide run-numeric-benchmarks)

;; ============================================
;; Native Racket functions
;; ============================================

(define (racket-sum n)
  (for/sum ([i (in-range 1 (+ n 1))]) i))

;; ============================================
;; Run benchmarks
;; ============================================

(define (run-numeric-benchmarks)
  (printf "============================================\n")
  (printf "Scalar JIT vs Native Racket\n")
  (printf "============================================\n")

  ;; Verify correctness
  (printf "\nVerifying correctness...\n")
  (printf "  factorial(10): JIT=~a, Racket=~a\n" (jit-factorial 10) (factorial 10))
  (printf "  fibonacci(20): JIT=~a, Racket=~a\n" (jit-fibonacci 20) (fibonacci 20))
  (printf "  sum(100):      JIT=~a, Racket=~a\n" (jit-sum 100) (racket-sum 100))
  (printf "  gcd(48, 18):   JIT=~a, Racket=~a\n" (jit-gcd 48 18) (gcd 48 18))

  (define iterations 10000000)

  (benchmark "Simple add" iterations jit-add + 10 32)
  (benchmark "Simple multiply" iterations jit-mul * 6 7)
  (benchmark "Factorial(10)" iterations jit-factorial factorial 10)
  (benchmark "Fibonacci(20)" iterations jit-fibonacci fibonacci 20)
  (benchmark "Sum(100)" iterations jit-sum racket-sum 100)
  (benchmark "GCD(48, 18)" iterations jit-gcd gcd 48 18)

  (printf "\n--- Larger inputs (1000000 iterations) ---\n")
  (benchmark "Factorial(30)" 1000000 jit-factorial factorial 30)
  (benchmark "Fibonacci(40)" 1000000 jit-fibonacci fibonacci 40)
  (benchmark "Sum(1000)" 1000000 jit-sum racket-sum 1000)
  (benchmark "GCD(123456, 7890)" 1000000 jit-gcd gcd 123456 7890))

;; Run if executed directly
(module+ main
  (run-numeric-benchmarks))
