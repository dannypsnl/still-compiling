#lang racket/base

;;; SIMD Benchmarks - SIMD JIT vs Native Racket vectors

(require "common.rkt")

(provide run-simd-benchmarks)

;; ============================================
;; SIMD JIT functions
;; ============================================

;; SIMD sum: process 2 int64s at a time
(define jit-sum-array-simd
  (let ([buf (dynasm-create 4096)])
    (emit! buf (aarch64-eor-simd V0 V0 V0 SIMD-16B))  ; V0 = 0
    (emit! buf (aarch64-movz X2 0 0))                  ; i = 0
    (emit! buf (aarch64-cmp-reg X2 X1))               ; loop:
    (emit! buf (aarch64-b-cond COND-GE 6))            ; if i >= len, reduce
    (emit! buf (aarch64-ldr-simd V1 X0 0))            ; V1 = load 2 int64s
    (emit! buf (aarch64-add-simd V0 V0 V1 SIMD-2D))   ; V0 += V1
    (emit! buf (aarch64-add-imm X0 X0 16))            ; ptr += 16
    (emit! buf (aarch64-add-imm X2 X2 2))             ; i += 2
    (emit! buf (aarch64-b -6))                        ; goto loop
    ;; reduce
    (emit! buf (aarch64-fmov-vec-to-gp X2 V0))
    (emit! buf (aarch64-dup-element V1 V0 SIMD-2D 1))
    (emit! buf (aarch64-fmov-vec-to-gp X3 V1))
    (emit! buf (aarch64-add-reg X0 X2 X3))
    (emit! buf (aarch64-ret))
    (make-jit-function buf (_fun _pointer _int64 -> _int64))))

;; SIMD dot product: load 2 elements at a time
;; (No 64-bit integer MUL in SIMD, so we extract and multiply scalarly)
(define jit-dot-product-simd
  (let ([buf (dynasm-create 4096)])
    (emit! buf (aarch64-eor-simd V0 V0 V0 SIMD-16B))  ; V0 = 0
    (emit! buf (aarch64-movz X3 0 0))                  ; i = 0
    (emit! buf (aarch64-cmp-reg X3 X2))               ; loop:
    (emit! buf (aarch64-b-cond COND-GE 18))           ; if i >= len, reduce
    (emit! buf (aarch64-ldr-simd V1 X0 0))            ; V1 = ptr1[i:i+1]
    (emit! buf (aarch64-ldr-simd V2 X1 0))            ; V2 = ptr2[i:i+1]
    ;; Extract and multiply
    (emit! buf (aarch64-fmov-vec-to-gp X4 V1))
    (emit! buf (aarch64-fmov-vec-to-gp X5 V2))
    (emit! buf (aarch64-mul X4 X4 X5))
    (emit! buf (aarch64-dup-element V3 V1 SIMD-2D 1))
    (emit! buf (aarch64-dup-element V4 V2 SIMD-2D 1))
    (emit! buf (aarch64-fmov-vec-to-gp X6 V3))
    (emit! buf (aarch64-fmov-vec-to-gp X7 V4))
    (emit! buf (aarch64-mul X6 X6 X7))
    ;; Accumulate
    (emit! buf (aarch64-fmov-gp-to-vec V5 X4))
    (emit! buf (aarch64-ins-general V5 X6 SIMD-2D 1))
    (emit! buf (aarch64-add-simd V0 V0 V5 SIMD-2D))
    (emit! buf (aarch64-add-imm X0 X0 16))
    (emit! buf (aarch64-add-imm X1 X1 16))
    (emit! buf (aarch64-add-imm X3 X3 2))
    (emit! buf (aarch64-b -18))
    ;; reduce
    (emit! buf (aarch64-fmov-vec-to-gp X4 V0))
    (emit! buf (aarch64-dup-element V1 V0 SIMD-2D 1))
    (emit! buf (aarch64-fmov-vec-to-gp X5 V1))
    (emit! buf (aarch64-add-reg X0 X4 X5))
    (emit! buf (aarch64-ret))
    (make-jit-function buf (_fun _pointer _pointer _int64 -> _int64))))

;; ============================================
;; Native Racket functions (using vectors)
;; ============================================

(define (racket-sum-vector vec)
  (for/sum ([x (in-vector vec)]) x))

(define (racket-dot-product-vector vec1 vec2)
  (for/sum ([a (in-vector vec1)] [b (in-vector vec2)])
    (* a b)))

;; ============================================
;; Run benchmarks
;; ============================================

(define (run-simd-benchmarks)
  (printf "\n============================================\n")
  (printf "SIMD Benchmarks\n")
  (printf "============================================\n")

  (define array-size 10000)  ; Must be even
  (define iterations 10000)

  ;; Create aligned memory for SIMD (16-byte alignment required)
  (define aligned-array (aligned-malloc (* array-size 8)))
  (define aligned-array2 (aligned-malloc (* array-size 8)))

  ;; Initialize aligned arrays
  (for ([i (in-range array-size)])
    (ptr-set! aligned-array _int64 i (+ i 1)))
  (for ([i (in-range array-size)])
    (ptr-set! aligned-array2 _int64 i (+ i 1)))

  ;; Create native Racket vectors for comparison
  (define test-vector (for/vector ([i (in-range array-size)]) (+ i 1)))
  (define test-vector2 (for/vector ([i (in-range array-size)]) (+ i 1)))

  ;; Expected results
  (define expected-sum (for/sum ([i (in-range array-size)]) (+ i 1)))
  (define expected-dot (for/sum ([i (in-range array-size)]) (* (+ i 1) (+ i 1))))

  ;; Verify SIMD correctness
  (printf "\nVerifying SIMD operations...\n")
  (define simd-sum (jit-sum-array-simd aligned-array array-size))
  (printf "  Sum: SIMD=~a, Expected=~a ~a\n"
          simd-sum expected-sum
          (if (= simd-sum expected-sum) "OK" (format "DIFF=~a" (- expected-sum simd-sum))))

  (define simd-dot (jit-dot-product-simd aligned-array aligned-array2 array-size))
  (printf "  Dot: SIMD=~a, Expected=~a ~a\n"
          simd-dot expected-dot
          (if (= simd-dot expected-dot) "OK" (format "DIFF=~a" (- expected-dot simd-dot))))

  ;; Benchmark: SIMD vs Native Racket
  (printf "\nSIMD JIT vs Native Racket (~a iterations, ~a elements):\n" iterations array-size)

  ;; Warmup
  (for ([_ (in-range 100)])
    (jit-sum-array-simd aligned-array array-size)
    (racket-sum-vector test-vector))

  ;; Sum benchmark
  (printf "\n  Sum array:\n")
  (collect-garbage)
  (collect-garbage)
  (define simd-sum-start (current-inexact-milliseconds))
  (for ([_ (in-range iterations)])
    (jit-sum-array-simd aligned-array array-size))
  (define simd-sum-time (- (current-inexact-milliseconds) simd-sum-start))

  (collect-garbage)
  (collect-garbage)
  (define racket-sum-start (current-inexact-milliseconds))
  (for ([_ (in-range iterations)])
    (racket-sum-vector test-vector))
  (define racket-sum-time (- (current-inexact-milliseconds) racket-sum-start))

  (printf "    SIMD JIT:      ~a ms\n" (~r simd-sum-time #:precision 2))
  (printf "    Native Racket: ~a ms\n" (~r racket-sum-time #:precision 2))
  (printf "    Speedup: ~ax (SIMD faster)\n"
          (~r (/ racket-sum-time simd-sum-time) #:precision 2))

  ;; Dot product benchmark
  (printf "\n  Dot product:\n")
  (collect-garbage)
  (collect-garbage)
  (define simd-dot-start (current-inexact-milliseconds))
  (for ([_ (in-range iterations)])
    (jit-dot-product-simd aligned-array aligned-array2 array-size))
  (define simd-dot-time (- (current-inexact-milliseconds) simd-dot-start))

  (collect-garbage)
  (collect-garbage)
  (define racket-dot-start (current-inexact-milliseconds))
  (for ([_ (in-range iterations)])
    (racket-dot-product-vector test-vector test-vector2))
  (define racket-dot-time (- (current-inexact-milliseconds) racket-dot-start))

  (printf "    SIMD JIT:      ~a ms\n" (~r simd-dot-time #:precision 2))
  (printf "    Native Racket: ~a ms\n" (~r racket-dot-time #:precision 2))
  (printf "    Speedup: ~ax (SIMD faster)\n"
          (~r (/ racket-dot-time simd-dot-time) #:precision 2))

  ;; Cleanup
  (libc-free aligned-array)
  (libc-free aligned-array2))

;; Run if executed directly
(module+ main
  (run-simd-benchmarks))
