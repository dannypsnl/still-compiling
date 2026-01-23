#lang racket/base

;;; Array Operation Benchmarks (Scalar JIT vs Racket lists/vectors)

(require "common.rkt")

(provide run-array-benchmarks)

;; ============================================
;; JIT-compiled array functions
;; ============================================

(define jit-sum-array
  (let ([buf (dynasm-create 4096)])
    (emit! buf (aarch64-movz X2 0 0))       ; sum = 0
    (emit! buf (aarch64-movz X3 0 0))       ; i = 0
    (emit! buf (aarch64-cmp-reg X3 X1))     ; loop:
    (emit! buf (aarch64-b-cond COND-GE 6))  ; if i >= len, done
    (emit! buf (aarch64-ldr-imm X4 X0 0))   ; tmp = ptr[i]
    (emit! buf (aarch64-add-reg X2 X2 X4))  ; sum += tmp
    (emit! buf (aarch64-add-imm X0 X0 8))   ; ptr++
    (emit! buf (aarch64-add-imm X3 X3 1))   ; i++
    (emit! buf (aarch64-b -6))              ; goto loop
    (emit! buf (aarch64-add-reg X0 X2 XZR)) ; return sum
    (emit! buf (aarch64-ret))
    (make-jit-function buf (_fun _pointer _int64 -> _int64))))

(define jit-max-array
  (let ([buf (dynasm-create 4096)])
    (emit! buf (aarch64-ldr-imm X2 X0 0))   ; max = ptr[0]
    (emit! buf (aarch64-add-imm X0 X0 8))   ; ptr++
    (emit! buf (aarch64-movz X3 1 0))       ; i = 1
    (emit! buf (aarch64-cmp-reg X3 X1))     ; loop:
    (emit! buf (aarch64-b-cond COND-GE 8))  ; if i >= len, done
    (emit! buf (aarch64-ldr-imm X4 X0 0))   ; tmp = ptr[i]
    (emit! buf (aarch64-cmp-reg X4 X2))     ; if tmp > max
    (emit! buf (aarch64-b-cond COND-LE 2))  ; skip if tmp <= max
    (emit! buf (aarch64-add-reg X2 X4 XZR)) ; max = tmp
    (emit! buf (aarch64-add-imm X0 X0 8))   ; ptr++
    (emit! buf (aarch64-add-imm X3 X3 1))   ; i++
    (emit! buf (aarch64-b -8))              ; goto loop
    (emit! buf (aarch64-add-reg X0 X2 XZR)) ; return max
    (emit! buf (aarch64-ret))
    (make-jit-function buf (_fun _pointer _int64 -> _int64))))

(define jit-dot-product
  (let ([buf (dynasm-create 4096)])
    (emit! buf (aarch64-movz X3 0 0))       ; sum = 0
    (emit! buf (aarch64-movz X4 0 0))       ; i = 0
    (emit! buf (aarch64-cmp-reg X4 X2))     ; loop:
    (emit! buf (aarch64-b-cond COND-GE 9))  ; if i >= len, done
    (emit! buf (aarch64-ldr-imm X5 X0 0))   ; a = ptr1[i]
    (emit! buf (aarch64-ldr-imm X6 X1 0))   ; b = ptr2[i]
    (emit! buf (aarch64-mul X5 X5 X6))      ; a * b
    (emit! buf (aarch64-add-reg X3 X3 X5))  ; sum += a * b
    (emit! buf (aarch64-add-imm X0 X0 8))   ; ptr1++
    (emit! buf (aarch64-add-imm X1 X1 8))   ; ptr2++
    (emit! buf (aarch64-add-imm X4 X4 1))   ; i++
    (emit! buf (aarch64-b -9))              ; goto loop
    (emit! buf (aarch64-add-reg X0 X3 XZR)) ; return sum
    (emit! buf (aarch64-ret))
    (make-jit-function buf (_fun _pointer _pointer _int64 -> _int64))))

(define jit-count-gt
  (let ([buf (dynasm-create 4096)])
    (emit! buf (aarch64-movz X3 0 0))       ; count = 0
    (emit! buf (aarch64-movz X4 0 0))       ; i = 0
    (emit! buf (aarch64-cmp-reg X4 X1))     ; loop:
    (emit! buf (aarch64-b-cond COND-GE 8))  ; if i >= len, done
    (emit! buf (aarch64-ldr-imm X5 X0 0))   ; tmp = ptr[i]
    (emit! buf (aarch64-cmp-reg X5 X2))     ; if tmp > threshold
    (emit! buf (aarch64-b-cond COND-LE 2))  ; skip if tmp <= threshold
    (emit! buf (aarch64-add-imm X3 X3 1))   ; count++
    (emit! buf (aarch64-add-imm X0 X0 8))   ; ptr++
    (emit! buf (aarch64-add-imm X4 X4 1))   ; i++
    (emit! buf (aarch64-b -8))              ; goto loop
    (emit! buf (aarch64-add-reg X0 X3 XZR)) ; return count
    (emit! buf (aarch64-ret))
    (make-jit-function buf (_fun _pointer _int64 _int64 -> _int64))))

;; ============================================
;; Native Racket functions
;; ============================================

(define (racket-sum-list lst)
  (for/sum ([x (in-list lst)]) x))

(define (racket-max-list lst)
  (for/fold ([m (car lst)]) ([x (in-list (cdr lst))])
    (if (> x m) x m)))

(define (racket-dot-product-list lst1 lst2)
  (for/sum ([a (in-list lst1)] [b (in-list lst2)])
    (* a b)))

(define (racket-count-gt-list lst threshold)
  (for/sum ([x (in-list lst)])
    (if (> x threshold) 1 0)))

(define (racket-sum-vector vec)
  (for/sum ([x (in-vector vec)]) x))

(define (racket-max-vector vec)
  (for/fold ([m (vector-ref vec 0)]) ([x (in-vector vec 1)])
    (if (> x m) x m)))

(define (racket-dot-product-vector vec1 vec2)
  (for/sum ([a (in-vector vec1)] [b (in-vector vec2)])
    (* a b)))

(define (racket-count-gt-vector vec threshold)
  (for/sum ([x (in-vector vec)])
    (if (> x threshold) 1 0)))

;; ============================================
;; Benchmark runner for arrays
;; ============================================

(define (benchmark-array name iterations array-size
                         jit-fn jit-args
                         racket-list-fn list-args
                         racket-vec-fn vec-args)
  (printf "\n~a (~a iterations, ~a elements):\n" name iterations array-size)

  ;; Warmup
  (for ([_ (in-range 100)])
    (apply jit-fn jit-args)
    (apply racket-list-fn list-args)
    (apply racket-vec-fn vec-args))

  ;; JIT benchmark
  (collect-garbage)
  (collect-garbage)
  (define jit-start (current-inexact-milliseconds))
  (for ([_ (in-range iterations)])
    (apply jit-fn jit-args))
  (define jit-time (- (current-inexact-milliseconds) jit-start))

  ;; Racket list benchmark
  (collect-garbage)
  (collect-garbage)
  (define list-start (current-inexact-milliseconds))
  (for ([_ (in-range iterations)])
    (apply racket-list-fn list-args))
  (define list-time (- (current-inexact-milliseconds) list-start))

  ;; Racket vector benchmark
  (collect-garbage)
  (collect-garbage)
  (define vec-start (current-inexact-milliseconds))
  (for ([_ (in-range iterations)])
    (apply racket-vec-fn vec-args))
  (define vec-time (- (current-inexact-milliseconds) vec-start))

  (printf "  JIT:           ~a ms\n" (~r jit-time #:precision 2))
  (printf "  Racket list:   ~a ms (~ax vs JIT)\n"
          (~r list-time #:precision 2)
          (~r (/ list-time jit-time) #:precision 2))
  (printf "  Racket vector: ~a ms (~ax vs JIT)\n"
          (~r vec-time #:precision 2)
          (~r (/ vec-time jit-time) #:precision 2)))

;; ============================================
;; Run benchmarks
;; ============================================

(define (run-array-benchmarks)
  (printf "\n============================================\n")
  (printf "Array Operation Benchmarks\n")
  (printf "============================================\n")

  (define array-size 10000)
  (define array-iterations 10000)

  ;; Create test data
  (define test-s64vec (make-s64vector array-size))
  (for ([i (in-range array-size)])
    (s64vector-set! test-s64vec i (+ i 1)))

  (define test-s64vec2 (make-s64vector array-size))
  (for ([i (in-range array-size)])
    (s64vector-set! test-s64vec2 i (+ i 1)))

  (define test-list (for/list ([i (in-range array-size)]) (+ i 1)))
  (define test-vector (for/vector ([i (in-range array-size)]) (+ i 1)))
  (define test-vector2 (for/vector ([i (in-range array-size)]) (+ i 1)))

  (define (s64vector-ptr vec) (s64vector->cpointer vec))

  ;; Verify correctness
  (printf "\nVerifying correctness (size=~a)...\n" array-size)
  (define jit-sum-result (jit-sum-array (s64vector-ptr test-s64vec) array-size))
  (define racket-sum-result (racket-sum-list test-list))
  (printf "  sum:     JIT=~a, Racket=~a ~a\n"
          jit-sum-result racket-sum-result
          (if (= jit-sum-result racket-sum-result) "OK" "MISMATCH"))

  (define jit-max-result (jit-max-array (s64vector-ptr test-s64vec) array-size))
  (define racket-max-result (racket-max-list test-list))
  (printf "  max:     JIT=~a, Racket=~a ~a\n"
          jit-max-result racket-max-result
          (if (= jit-max-result racket-max-result) "OK" "MISMATCH"))

  (define threshold (quotient array-size 2))

  ;; Run benchmarks
  (benchmark-array "Sum array" array-iterations array-size
                   jit-sum-array (list (s64vector-ptr test-s64vec) array-size)
                   racket-sum-list (list test-list)
                   racket-sum-vector (list test-vector))

  (benchmark-array "Max array" array-iterations array-size
                   jit-max-array (list (s64vector-ptr test-s64vec) array-size)
                   racket-max-list (list test-list)
                   racket-max-vector (list test-vector))

  (benchmark-array "Dot product" array-iterations array-size
                   jit-dot-product (list (s64vector-ptr test-s64vec) (s64vector-ptr test-s64vec2) array-size)
                   racket-dot-product-list (list test-list test-list)
                   racket-dot-product-vector (list test-vector test-vector2))

  (benchmark-array "Count > threshold" array-iterations array-size
                   jit-count-gt (list (s64vector-ptr test-s64vec) array-size threshold)
                   racket-count-gt-list (list test-list threshold)
                   racket-count-gt-vector (list test-vector threshold)))

;; Run if executed directly
(module+ main
  (run-array-benchmarks))
