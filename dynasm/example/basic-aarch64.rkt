#lang racket/base

;;; Elementary examples of using dynasm with Racket FFI
;;; Run from the dynasm directory: racket examples.rkt

(require ffi/unsafe
         "../dynasm.rkt")

(displayln "=== Dynasm + Racket FFI Examples ===\n")

;; ============================================
;; Example 1: Return a constant
;; ============================================

(displayln "--- Example 1: Return constant 42 ---")

(let ([buf (dynasm-create 4096)])
  ;; mov x0, #42
  (emit! buf (aarch64-movz X0 42 0))
  ;; ret
  (emit! buf (aarch64-ret))

  ;; Create callable function
  (define return-42
    (make-jit-function buf (_fun -> _int64)))

  (printf "Result: ~a (expected 42)\n\n" (return-42))

  (dynasm-free buf))

;; ============================================
;; Example 2: Add two numbers
;; ============================================

(displayln "--- Example 2: Add two numbers ---")

(let ([buf (dynasm-create 4096)])
  ;; add x0, x0, x1
  (emit! buf (aarch64-add-reg X0 X0 X1))
  ;; ret
  (emit! buf (aarch64-ret))

  (define add-numbers
    (make-jit-function buf (_fun _int64 _int64 -> _int64)))

  (printf "10 + 32 = ~a (expected 42)\n" (add-numbers 10 32))
  (printf "100 + 200 = ~a (expected 300)\n\n" (add-numbers 100 200))

  (dynasm-free buf))

;; ============================================
;; Example 3: Multiply two numbers
;; ============================================

(displayln "--- Example 3: Multiply two numbers ---")

(let ([buf (dynasm-create 4096)])
  ;; mul x0, x0, x1
  (emit! buf (aarch64-mul X0 X0 X1))
  ;; ret
  (emit! buf (aarch64-ret))

  (define multiply
    (make-jit-function buf (_fun _int64 _int64 -> _int64)))

  (printf "6 * 7 = ~a (expected 42)\n" (multiply 6 7))
  (printf "12 * 12 = ~a (expected 144)\n\n" (multiply 12 12))

  (dynasm-free buf))

;; ============================================
;; Example 4: Compute (a + b) * c
;; ============================================

(displayln "--- Example 4: Compute (a + b) * c ---")

(let ([buf (dynasm-create 4096)])
  ;; add x0, x0, x1   ; x0 = a + b
  (emit! buf (aarch64-add-reg X0 X0 X1))
  ;; mul x0, x0, x2   ; x0 = (a + b) * c
  (emit! buf (aarch64-mul X0 X0 X2))
  ;; ret
  (emit! buf (aarch64-ret))

  (define expr
    (make-jit-function buf (_fun _int64 _int64 _int64 -> _int64)))

  (printf "(3 + 4) * 6 = ~a (expected 42)\n" (expr 3 4 6))
  (printf "(10 + 5) * 2 = ~a (expected 30)\n\n" (expr 10 5 2))

  (dynasm-free buf))

;; ============================================
;; Example 5: Absolute value (conditional)
;; ============================================

(displayln "--- Example 5: Absolute value ---")

(let ([buf (dynasm-create 4096)])
  ;; cmp x0, #0
  (emit! buf (aarch64-cmp-imm X0 0))
  ;; b.ge positive (skip 2 instructions forward)
  (emit! buf (aarch64-b-cond COND-GE 2))
  ;; sub x0, xzr, x0  (negate: x0 = 0 - x0)
  (emit! buf (aarch64-sub-reg X0 XZR X0))
  ;; ret
  (emit! buf (aarch64-ret))

  (define abs-value
    (make-jit-function buf (_fun _int64 -> _int64)))

  (printf "abs(-42) = ~a (expected 42)\n" (abs-value -42))
  (printf "abs(42) = ~a (expected 42)\n" (abs-value 42))
  (printf "abs(0) = ~a (expected 0)\n\n" (abs-value 0))

  (dynasm-free buf))

;; ============================================
;; Example 6: Max of two numbers
;; ============================================

(displayln "--- Example 6: Max of two numbers ---")

(let ([buf (dynasm-create 4096)])
  ;; cmp x0, x1
  (emit! buf (aarch64-cmp-reg X0 X1))
  ;; b.ge done (x0 >= x1, return x0)
  (emit! buf (aarch64-b-cond COND-GE 2))
  ;; mov x0, x1 (x0 < x1, return x1)
  (emit! buf (aarch64-add-imm X0 X1 0))  ; mov via add x0, x1, #0
  ;; ret
  (emit! buf (aarch64-ret))

  (define max-of
    (make-jit-function buf (_fun _int64 _int64 -> _int64)))

  (printf "max(10, 20) = ~a (expected 20)\n" (max-of 10 20))
  (printf "max(30, 15) = ~a (expected 30)\n" (max-of 30 15))
  (printf "max(5, 5) = ~a (expected 5)\n\n" (max-of 5 5))

  (dynasm-free buf))

;; ============================================
;; Example 7: Simple loop - sum 1 to n
;; ============================================

(displayln "--- Example 7: Sum 1 to n ---")

;; sum(n) = 1 + 2 + ... + n
;; Using formula: n*(n+1)/2 to verify

(let ([buf (dynasm-create 4096)])
  ;; x0 = n (input)
  ;; x1 = sum = 0
  ;; x2 = i = 1

  (emit! buf (aarch64-movz X1 0 0))        ; sum = 0
  (emit! buf (aarch64-movz X2 1 0))        ; i = 1

  ;; loop: (position 2)
  ;; if i > n, goto done
  (emit! buf (aarch64-cmp-reg X2 X0))      ; cmp i, n
  (emit! buf (aarch64-b-cond COND-GT 4))   ; if i > n, skip 4 instrs to done

  ;; sum += i
  (emit! buf (aarch64-add-reg X1 X1 X2))   ; sum = sum + i
  ;; i++
  (emit! buf (aarch64-add-imm X2 X2 1))    ; i = i + 1
  ;; goto loop (back 4 instructions)
  (emit! buf (aarch64-b -4))               ; b loop

  ;; done:
  (emit! buf (aarch64-add-imm X0 X1 0))    ; return sum
  (emit! buf (aarch64-ret))

  (define sum-to-n
    (make-jit-function buf (_fun _int64 -> _int64)))

  (printf "sum(10) = ~a (expected 55)\n" (sum-to-n 10))
  (printf "sum(100) = ~a (expected 5050)\n\n" (sum-to-n 100))

  (dynasm-free buf))

;; ============================================
;; Example 8: Factorial
;; ============================================

(displayln "--- Example 8: Factorial ---")

(let ([buf (dynasm-create 4096)])
  ;; x0 = n (input)
  ;; x1 = result = 1

  (emit! buf (aarch64-movz X1 1 0))        ; result = 1

  ;; loop: (position 1)
  ;; if n <= 1, goto done
  (emit! buf (aarch64-cmp-imm X0 1))       ; cmp n, 1
  (emit! buf (aarch64-b-cond COND-LE 4))   ; if n <= 1, skip 4 instrs

  ;; result *= n
  (emit! buf (aarch64-mul X1 X1 X0))       ; result = result * n
  ;; n--
  (emit! buf (aarch64-sub-imm X0 X0 1))    ; n = n - 1
  ;; goto loop (back 4 instructions)
  (emit! buf (aarch64-b -4))               ; b loop

  ;; done:
  (emit! buf (aarch64-add-imm X0 X1 0))    ; return result
  (emit! buf (aarch64-ret))

  (define factorial
    (make-jit-function buf (_fun _int64 -> _int64)))

  (printf "5! = ~a (expected 120)\n" (factorial 5))
  (printf "10! = ~a (expected 3628800)\n\n" (factorial 10))

  (dynasm-free buf))

;; ============================================
;; Example 9: Build large constant (64-bit)
;; ============================================

(displayln "--- Example 9: Large 64-bit constant ---")

(let ([buf (dynasm-create 4096)])
  ;; Build 0x123456789ABCDEF0
  ;; movz x0, #0xDEF0, lsl #0
  (emit! buf (aarch64-movz X0 #xDEF0 0))
  ;; movk x0, #0x9ABC, lsl #16
  (emit! buf (aarch64-movk X0 #x9ABC 1))
  ;; movk x0, #0x5678, lsl #32
  (emit! buf (aarch64-movk X0 #x5678 2))
  ;; movk x0, #0x1234, lsl #48
  (emit! buf (aarch64-movk X0 #x1234 3))
  ;; ret
  (emit! buf (aarch64-ret))

  (define get-large-const
    (make-jit-function buf (_fun -> _int64)))

  (printf "Result: #x~x (expected #x123456789ABCDEF0)\n\n" (get-large-const))

  (dynasm-free buf))

(displayln "=== All examples completed! ===")
