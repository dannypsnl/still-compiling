#lang racket/base

;;; Benchmark: JIT-compiled code vs native Racket
;;; Run: racket benchmark.rkt

(require ffi/unsafe
         ffi/vector
         "dynasm.rkt")

(define iterations 10000000)

;; ============================================
;; JIT-compiled functions
;; ============================================

;; Factorial (iterative)
(define jit-factorial
  (let ([buf (dynasm-create 4096)])
    (emit! buf (aarch64-movz X1 1 0))       ; result = 1
    (emit! buf (aarch64-cmp-imm X0 1))      ; loop:
    (emit! buf (aarch64-b-cond COND-LE 4))  ; if n <= 1, done
    (emit! buf (aarch64-mul X1 X1 X0))      ; result *= n
    (emit! buf (aarch64-sub-imm X0 X0 1))   ; n--
    (emit! buf (aarch64-b -4))              ; goto loop
    (emit! buf (aarch64-add-reg X0 X1 XZR)) ; return result
    (emit! buf (aarch64-ret))
    (make-jit-function buf (_fun _int64 -> _int64))))

;; Fibonacci (iterative)
(define jit-fibonacci
  (let ([buf (dynasm-create 4096)])
    (emit! buf (aarch64-cmp-imm X0 1))
    (emit! buf (aarch64-b-cond COND-LE 11))
    (emit! buf (aarch64-movz X1 0 0))       ; prev2 = 0
    (emit! buf (aarch64-movz X2 1 0))       ; prev1 = 1
    (emit! buf (aarch64-sub-imm X3 X0 1))   ; counter = n - 1
    (emit! buf (aarch64-add-reg X4 X1 X2))  ; loop: curr = prev2 + prev1
    (emit! buf (aarch64-add-reg X1 X2 XZR)) ; prev2 = prev1
    (emit! buf (aarch64-add-reg X2 X4 XZR)) ; prev1 = curr
    (emit! buf (aarch64-sub-imm X3 X3 1))   ; counter--
    (emit! buf (aarch64-cmp-imm X3 0))
    (emit! buf (aarch64-b-cond COND-GT -5)) ; if counter > 0, loop
    (emit! buf (aarch64-add-reg X0 X2 XZR)) ; return prev1
    (emit! buf (aarch64-ret))
    (make-jit-function buf (_fun _int64 -> _int64))))

;; Sum 1 to n
(define jit-sum
  (let ([buf (dynasm-create 4096)])
    (emit! buf (aarch64-movz X1 0 0))       ; sum = 0
    (emit! buf (aarch64-movz X2 1 0))       ; i = 1
    (emit! buf (aarch64-cmp-reg X2 X0))     ; loop:
    (emit! buf (aarch64-b-cond COND-GT 4))  ; if i > n, done
    (emit! buf (aarch64-add-reg X1 X1 X2))  ; sum += i
    (emit! buf (aarch64-add-imm X2 X2 1))   ; i++
    (emit! buf (aarch64-b -4))              ; goto loop
    (emit! buf (aarch64-add-reg X0 X1 XZR)) ; return sum
    (emit! buf (aarch64-ret))
    (make-jit-function buf (_fun _int64 -> _int64))))

;; GCD
(define jit-gcd
  (let ([buf (dynasm-create 4096)])
    (emit! buf (aarch64-cmp-imm X1 0))      ; loop:
    (emit! buf (aarch64-b-cond COND-EQ 7))  ; if b == 0, done
    (emit! buf (aarch64-sdiv X2 X0 X1))     ; t = a / b
    (emit! buf (aarch64-mul X2 X2 X1))      ; t = t * b
    (emit! buf (aarch64-sub-reg X2 X0 X2))  ; t = a % b
    (emit! buf (aarch64-add-reg X0 X1 XZR)) ; a = b
    (emit! buf (aarch64-add-reg X1 X2 XZR)) ; b = t
    (emit! buf (aarch64-b -7))              ; goto loop
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

;; ============================================
;; JIT Array functions
;; ============================================

;; Sum array elements: sum_array(ptr, len)
(define jit-sum-array
  (let ([buf (dynasm-create 4096)])
    ;; x0 = ptr, x1 = len
    (emit! buf (aarch64-movz X2 0 0))       ; sum = 0
    (emit! buf (aarch64-movz X3 0 0))       ; i = 0
    ;; loop:
    (emit! buf (aarch64-cmp-reg X3 X1))     ; if i >= len
    (emit! buf (aarch64-b-cond COND-GE 6))  ; goto done
    (emit! buf (aarch64-ldr-imm X4 X0 0))   ; tmp = ptr[i]
    (emit! buf (aarch64-add-reg X2 X2 X4))  ; sum += tmp
    (emit! buf (aarch64-add-imm X0 X0 8))   ; ptr++
    (emit! buf (aarch64-add-imm X3 X3 1))   ; i++
    (emit! buf (aarch64-b -6))              ; goto loop
    ;; done:
    (emit! buf (aarch64-add-reg X0 X2 XZR)) ; return sum
    (emit! buf (aarch64-ret))
    (make-jit-function buf (_fun _pointer _int64 -> _int64))))

;; Find max in array: max_array(ptr, len)
(define jit-max-array
  (let ([buf (dynasm-create 4096)])
    ;; x0 = ptr, x1 = len
    (emit! buf (aarch64-ldr-imm X2 X0 0))   ; max = ptr[0]
    (emit! buf (aarch64-add-imm X0 X0 8))   ; ptr++
    (emit! buf (aarch64-movz X3 1 0))       ; i = 1
    ;; loop:
    (emit! buf (aarch64-cmp-reg X3 X1))     ; if i >= len
    (emit! buf (aarch64-b-cond COND-GE 8))  ; goto done (skip 7 instructions)
    (emit! buf (aarch64-ldr-imm X4 X0 0))   ; tmp = ptr[i]
    (emit! buf (aarch64-cmp-reg X4 X2))     ; if tmp > max
    (emit! buf (aarch64-b-cond COND-LE 2))  ; skip max update if tmp <= max
    (emit! buf (aarch64-add-reg X2 X4 XZR)) ; max = tmp
    (emit! buf (aarch64-add-imm X0 X0 8))   ; ptr++
    (emit! buf (aarch64-add-imm X3 X3 1))   ; i++
    (emit! buf (aarch64-b -8))              ; goto loop
    ;; done:
    (emit! buf (aarch64-add-reg X0 X2 XZR)) ; return max
    (emit! buf (aarch64-ret))
    (make-jit-function buf (_fun _pointer _int64 -> _int64))))

;; Dot product: dot(ptr1, ptr2, len)
(define jit-dot-product
  (let ([buf (dynasm-create 4096)])
    ;; x0 = ptr1, x1 = ptr2, x2 = len
    (emit! buf (aarch64-movz X3 0 0))       ; sum = 0
    (emit! buf (aarch64-movz X4 0 0))       ; i = 0
    ;; loop:
    (emit! buf (aarch64-cmp-reg X4 X2))     ; if i >= len
    (emit! buf (aarch64-b-cond COND-GE 9))  ; goto done (skip 8 instructions)
    (emit! buf (aarch64-ldr-imm X5 X0 0))   ; a = ptr1[i]
    (emit! buf (aarch64-ldr-imm X6 X1 0))   ; b = ptr2[i]
    (emit! buf (aarch64-mul X5 X5 X6))      ; a * b
    (emit! buf (aarch64-add-reg X3 X3 X5))  ; sum += a * b
    (emit! buf (aarch64-add-imm X0 X0 8))   ; ptr1++
    (emit! buf (aarch64-add-imm X1 X1 8))   ; ptr2++
    (emit! buf (aarch64-add-imm X4 X4 1))   ; i++
    (emit! buf (aarch64-b -9))              ; goto loop
    ;; done:
    (emit! buf (aarch64-add-reg X0 X3 XZR)) ; return sum
    (emit! buf (aarch64-ret))
    (make-jit-function buf (_fun _pointer _pointer _int64 -> _int64))))

;; Count elements > threshold: count_gt(ptr, len, threshold)
(define jit-count-gt
  (let ([buf (dynasm-create 4096)])
    ;; x0 = ptr, x1 = len, x2 = threshold
    (emit! buf (aarch64-movz X3 0 0))       ; count = 0
    (emit! buf (aarch64-movz X4 0 0))       ; i = 0
    ;; loop:
    (emit! buf (aarch64-cmp-reg X4 X1))     ; if i >= len
    (emit! buf (aarch64-b-cond COND-GE 8))  ; goto done (skip 7 instructions)
    (emit! buf (aarch64-ldr-imm X5 X0 0))   ; tmp = ptr[i]
    (emit! buf (aarch64-cmp-reg X5 X2))     ; if tmp > threshold
    (emit! buf (aarch64-b-cond COND-LE 2))  ; skip count++ if tmp <= threshold
    (emit! buf (aarch64-add-imm X3 X3 1))   ; count++
    (emit! buf (aarch64-add-imm X0 X0 8))   ; ptr++
    (emit! buf (aarch64-add-imm X4 X4 1))   ; i++
    (emit! buf (aarch64-b -8))              ; goto loop
    ;; done:
    (emit! buf (aarch64-add-reg X0 X3 XZR)) ; return count
    (emit! buf (aarch64-ret))
    (make-jit-function buf (_fun _pointer _int64 _int64 -> _int64))))

;; ============================================
;; Native Racket functions (using built-ins)
;; ============================================

;; Use built-in factorial from racket/math would be unfair (uses lookup table)
;; So we use a simple loop that mirrors the JIT version
(define (racket-factorial n)
  (for/fold ([result 1]) ([i (in-range 2 (+ n 1))])
    (* result i)))

;; Iterative fibonacci matching JIT structure
(define (racket-fibonacci n)
  (if (<= n 1)
      n
      (for/fold ([prev2 0] [prev1 1] #:result prev1)
                ([_ (in-range (- n 1))])
        (values prev1 (+ prev2 prev1)))))

;; Use built-in for/sum
(define (racket-sum n)
  (for/sum ([i (in-range 1 (+ n 1))]) i))

;; Use built-in gcd
(define racket-gcd gcd)

;; Use built-in + and *
(define racket-add +)
(define racket-mul *)

;; Array functions using Racket lists
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

;; Array functions using vectors (faster)
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
;; Benchmark runner
;; ============================================

(define (benchmark name iterations jit-fn racket-fn . args)
  (printf "\n~a (~a iterations):\n" name iterations)

  ;; Warmup
  (for ([_ (in-range 1000)])
    (apply jit-fn args)
    (apply racket-fn args))

  ;; JIT benchmark
  (collect-garbage)
  (collect-garbage)
  (define jit-start (current-inexact-milliseconds))
  (for ([_ (in-range iterations)])
    (apply jit-fn args))
  (define jit-end (current-inexact-milliseconds))
  (define jit-time (- jit-end jit-start))

  ;; Racket benchmark
  (collect-garbage)
  (collect-garbage)
  (define racket-start (current-inexact-milliseconds))
  (for ([_ (in-range iterations)])
    (apply racket-fn args))
  (define racket-end (current-inexact-milliseconds))
  (define racket-time (- racket-end racket-start))

  ;; Results
  (printf "  JIT:    ~a ms\n" (~r jit-time #:precision 2))
  (printf "  Racket: ~a ms\n" (~r racket-time #:precision 2))
  (define speedup (/ racket-time jit-time))
  (printf "  Speedup: ~ax ~a\n"
          (~r speedup #:precision 2)
          (if (> speedup 1) "(JIT faster)" "(Racket faster)")))

(define (~r n #:precision [p 2])
  (real->decimal-string n p))

;; ============================================
;; Run benchmarks
;; ============================================

(printf "============================================\n")
(printf "Dynasm JIT vs Native Racket Benchmark\n")
(printf "============================================\n")

;; Verify correctness first
(printf "\nVerifying correctness...\n")
(printf "  factorial(10): JIT=~a, Racket=~a\n" (jit-factorial 10) (racket-factorial 10))
(printf "  fibonacci(20): JIT=~a, Racket=~a\n" (jit-fibonacci 20) (racket-fibonacci 20))
(printf "  sum(100):      JIT=~a, Racket=~a\n" (jit-sum 100) (racket-sum 100))
(printf "  gcd(48, 18):   JIT=~a, Racket=~a\n" (jit-gcd 48 18) (racket-gcd 48 18))
(printf "  add(10, 32):   JIT=~a, Racket=~a\n" (jit-add 10 32) (racket-add 10 32))
(printf "  mul(6, 7):     JIT=~a, Racket=~a\n" (jit-mul 6 7) (racket-mul 6 7))

;; Run benchmarks
(benchmark "Simple add" iterations jit-add racket-add 10 32)
(benchmark "Simple multiply" iterations jit-mul racket-mul 6 7)
(benchmark "Factorial(10)" iterations jit-factorial racket-factorial 10)
(benchmark "Fibonacci(20)" iterations jit-fibonacci racket-fibonacci 20)
(benchmark "Sum(100)" iterations jit-sum racket-sum 100)
(benchmark "GCD(48, 18)" iterations jit-gcd racket-gcd 48 18)

;; Larger inputs with fewer iterations
(printf "\n--- Larger inputs (100000 iterations) ---\n")
(benchmark "Factorial(20)" 100000 jit-factorial racket-factorial 20)
(benchmark "Fibonacci(40)" 100000 jit-fibonacci racket-fibonacci 40)
(benchmark "Sum(1000)" 100000 jit-sum racket-sum 1000)
(benchmark "GCD(123456, 7890)" 100000 jit-gcd racket-gcd 123456 7890)

;; ============================================
;; Array benchmarks
;; ============================================

(printf "\n============================================\n")
(printf "Array/List Benchmarks\n")
(printf "============================================\n")

;; Create test data
(define array-size 10000)
(define array-iterations 10000)

;; Create s64vector for JIT (contiguous memory)
(define test-s64vec (make-s64vector array-size))
(for ([i (in-range array-size)])
  (s64vector-set! test-s64vec i (+ i 1)))

(define test-s64vec2 (make-s64vector array-size))
(for ([i (in-range array-size)])
  (s64vector-set! test-s64vec2 i (+ i 1)))

;; Create Racket list and vector
(define test-list (for/list ([i (in-range array-size)]) (+ i 1)))
(define test-vector (for/vector ([i (in-range array-size)]) (+ i 1)))
(define test-vector2 (for/vector ([i (in-range array-size)]) (+ i 1)))

;; Helper to get pointer from s64vector
(define (s64vector-ptr vec)
  (s64vector->cpointer vec))

;; Verify correctness
(printf "\nVerifying array operations (size=~a)...\n" array-size)

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

(define jit-dot-result (jit-dot-product (s64vector-ptr test-s64vec)
                                         (s64vector-ptr test-s64vec2)
                                         array-size))
(define racket-dot-result (racket-dot-product-list test-list test-list))
(printf "  dot:     JIT=~a, Racket=~a ~a\n"
        jit-dot-result racket-dot-result
        (if (= jit-dot-result racket-dot-result) "OK" "MISMATCH"))

(define threshold (quotient array-size 2))
(define jit-count-result (jit-count-gt (s64vector-ptr test-s64vec) array-size threshold))
(define racket-count-result (racket-count-gt-list test-list threshold))
(printf "  count>~a: JIT=~a, Racket=~a ~a\n"
        threshold jit-count-result racket-count-result
        (if (= jit-count-result racket-count-result) "OK" "MISMATCH"))

;; Array benchmark helper
(define (benchmark-array name iterations jit-fn jit-args racket-list-fn list-args racket-vec-fn vec-args)
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

  ;; Results
  (printf "  JIT:           ~a ms\n" (~r jit-time #:precision 2))
  (printf "  Racket list:   ~a ms (~ax vs JIT)\n"
          (~r list-time #:precision 2)
          (~r (/ list-time jit-time) #:precision 2))
  (printf "  Racket vector: ~a ms (~ax vs JIT)\n"
          (~r vec-time #:precision 2)
          (~r (/ vec-time jit-time) #:precision 2)))

;; Run array benchmarks
(benchmark-array "Sum array"
                 array-iterations
                 jit-sum-array (list (s64vector-ptr test-s64vec) array-size)
                 racket-sum-list (list test-list)
                 racket-sum-vector (list test-vector))

(benchmark-array "Max array"
                 array-iterations
                 jit-max-array (list (s64vector-ptr test-s64vec) array-size)
                 racket-max-list (list test-list)
                 racket-max-vector (list test-vector))

(benchmark-array "Dot product"
                 array-iterations
                 jit-dot-product (list (s64vector-ptr test-s64vec) (s64vector-ptr test-s64vec2) array-size)
                 racket-dot-product-list (list test-list test-list)
                 racket-dot-product-vector (list test-vector test-vector2))

(benchmark-array "Count > threshold"
                 array-iterations
                 jit-count-gt (list (s64vector-ptr test-s64vec) array-size threshold)
                 racket-count-gt-list (list test-list threshold)
                 racket-count-gt-vector (list test-vector threshold))

(printf "\n============================================\n")
(printf "Benchmark complete!\n")
(printf "============================================\n")
