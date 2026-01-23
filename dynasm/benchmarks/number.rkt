#lang racket/base

;;; Scalar JIT vs Native Racket Benchmarks

(require "common.rkt"
         "../jit-functions.rkt")

(provide run-scalar-benchmarks)

;; ============================================
;; Native Racket functions
;; ============================================

(define (racket-factorial n)
  (for/fold ([result 1]) ([i (in-range 2 (+ n 1))])
    (* result i)))

(define (racket-fibonacci n)
  (if (<= n 1)
      n
      (for/fold ([prev2 0] [prev1 1] #:result prev1)
                ([_ (in-range (- n 1))])
        (values prev1 (+ prev2 prev1)))))

(define (racket-sum n)
  (for/sum ([i (in-range 1 (+ n 1))]) i))

(define racket-gcd gcd)
(define racket-add +)
(define racket-mul *)

;; ============================================
;; Run benchmarks
;; ============================================

(define (run-scalar-benchmarks)
  (printf "============================================\n")
  (printf "Scalar JIT vs Native Racket\n")
  (printf "============================================\n")

  ;; Verify correctness
  (printf "\nVerifying correctness...\n")
  (printf "  factorial(10): JIT=~a, Racket=~a\n" (jit-factorial 10) (racket-factorial 10))
  (printf "  fibonacci(20): JIT=~a, Racket=~a\n" (jit-fibonacci 20) (racket-fibonacci 20))
  (printf "  sum(100):      JIT=~a, Racket=~a\n" (jit-sum 100) (racket-sum 100))
  (printf "  gcd(48, 18):   JIT=~a, Racket=~a\n" (jit-gcd 48 18) (racket-gcd 48 18))

  (define iterations 10000000)

  (benchmark "Simple add" iterations jit-add racket-add 10 32)
  (benchmark "Simple multiply" iterations jit-mul racket-mul 6 7)
  (benchmark "Factorial(10)" iterations jit-factorial racket-factorial 10)
  (benchmark "Fibonacci(20)" iterations jit-fibonacci racket-fibonacci 20)
  (benchmark "Sum(100)" iterations jit-sum racket-sum 100)
  (benchmark "GCD(48, 18)" iterations jit-gcd racket-gcd 48 18)

  (printf "\n--- Larger inputs (100000 iterations) ---\n")
  (benchmark "Factorial(20)" 100000 jit-factorial racket-factorial 20)
  (benchmark "Fibonacci(40)" 100000 jit-fibonacci racket-fibonacci 40)
  (benchmark "Sum(1000)" 100000 jit-sum racket-sum 1000)
  (benchmark "GCD(123456, 7890)" 100000 jit-gcd racket-gcd 123456 7890))

;; Run if executed directly
(module+ main
  (run-scalar-benchmarks))
