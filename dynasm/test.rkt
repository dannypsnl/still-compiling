#lang racket/base

;;; Comprehensive tests for dynasm instructions
;;; Run: racket test.rkt

(require rackunit
         rackunit/text-ui
         ffi/unsafe
         "dynasm.rkt")

;; ============================================
;; Test helpers
;; ============================================

(define (run-jit-void thunk)
  (let ([buf (dynasm-create 4096)])
    (thunk buf)
    (let* ([fn (make-jit-function buf (_fun -> _int64))]
           [result (fn)])
      (dynasm-free buf)
      result)))

(define (run-jit-1arg thunk arg)
  (let ([buf (dynasm-create 4096)])
    (thunk buf)
    (let* ([fn (make-jit-function buf (_fun _int64 -> _int64))]
           [result (fn arg)])
      (dynasm-free buf)
      result)))

(define (run-jit-2arg thunk arg1 arg2)
  (let ([buf (dynasm-create 4096)])
    (thunk buf)
    (let* ([fn (make-jit-function buf (_fun _int64 _int64 -> _int64))]
           [result (fn arg1 arg2)])
      (dynasm-free buf)
      result)))

(define (run-jit-3arg thunk arg1 arg2 arg3)
  (let ([buf (dynasm-create 4096)])
    (thunk buf)
    (let* ([fn (make-jit-function buf (_fun _int64 _int64 _int64 -> _int64))]
           [result (fn arg1 arg2 arg3)])
      (dynasm-free buf)
      result)))

(define (run-jit-5arg thunk a1 a2 a3 a4 a5)
  (let ([buf (dynasm-create 4096)])
    (thunk buf)
    (let* ([fn (make-jit-function buf (_fun _int64 _int64 _int64 _int64 _int64 -> _int64))]
           [result (fn a1 a2 a3 a4 a5)])
      (dynasm-free buf)
      result)))

(define (run-jit-ptr thunk)
  (let ([buf (dynasm-create 4096)])
    (thunk buf)
    (let* ([fn (make-jit-function buf (_fun _pointer -> _int64))]
           [arr (malloc (_array _int64 3))]
           [result (fn arr)])
      (dynasm-free buf)
      (free arr)
      result)))

;; ============================================
;; MOVZ tests
;; ============================================

(define movz-tests
  (test-suite
   "MOVZ instruction tests"

   (test-case "movz - basic immediate"
     (check-equal?
      (run-jit-void
       (lambda (buf)
         (emit! buf (aarch64-movz X0 42 0))
         (emit! buf (aarch64-ret))))
      42))

   (test-case "movz - zero value"
     (check-equal?
      (run-jit-void
       (lambda (buf)
         (emit! buf (aarch64-movz X0 0 0))
         (emit! buf (aarch64-ret))))
      0))

   (test-case "movz - max 16-bit value"
     (check-equal?
      (run-jit-void
       (lambda (buf)
         (emit! buf (aarch64-movz X0 #xFFFF 0))
         (emit! buf (aarch64-ret))))
      #xFFFF))

   (test-case "movz - shift 16 bits"
     (check-equal?
      (run-jit-void
       (lambda (buf)
         (emit! buf (aarch64-movz X0 1 1))
         (emit! buf (aarch64-ret))))
      #x10000))

   (test-case "movz - shift 32 bits"
     (check-equal?
      (run-jit-void
       (lambda (buf)
         (emit! buf (aarch64-movz X0 1 2))
         (emit! buf (aarch64-ret))))
      #x100000000))

   (test-case "movz - shift 48 bits"
     (check-equal?
      (run-jit-void
       (lambda (buf)
         (emit! buf (aarch64-movz X0 1 3))
         (emit! buf (aarch64-ret))))
      #x1000000000000))

   (test-case "movz - value 255"
     (check-equal?
      (run-jit-void
       (lambda (buf)
         (emit! buf (aarch64-movz X0 255 0))
         (emit! buf (aarch64-ret))))
      255))

   (test-case "movz - value 256"
     (check-equal?
      (run-jit-void
       (lambda (buf)
         (emit! buf (aarch64-movz X0 256 0))
         (emit! buf (aarch64-ret))))
      256))))

;; ============================================
;; MOVK tests
;; ============================================

(define movk-tests
  (test-suite
   "MOVK instruction tests"

   (test-case "movk - build 32-bit constant"
     (check-equal?
      (run-jit-void
       (lambda (buf)
         (emit! buf (aarch64-movz X0 #x5678 0))
         (emit! buf (aarch64-movk X0 #x1234 1))
         (emit! buf (aarch64-ret))))
      #x12345678))

   (test-case "movk - build 64-bit constant"
     ;; 0xDEADBEEFCAFEBABE as signed int64 is negative
     (check-equal?
      (run-jit-void
       (lambda (buf)
         (emit! buf (aarch64-movz X0 #xBABE 0))
         (emit! buf (aarch64-movk X0 #xCAFE 1))
         (emit! buf (aarch64-movk X0 #xBEEF 2))
         (emit! buf (aarch64-movk X0 #xDEAD 3))
         (emit! buf (aarch64-ret))))
      -2401053089206453570))

   (test-case "movk - build 48-bit constant"
     (check-equal?
      (run-jit-void
       (lambda (buf)
         (emit! buf (aarch64-movz X0 #x1111 0))
         (emit! buf (aarch64-movk X0 #x2222 1))
         (emit! buf (aarch64-movk X0 #x3333 2))
         (emit! buf (aarch64-ret))))
      #x333322221111))))

;; ============================================
;; ADD immediate tests
;; ============================================

(define add-imm-tests
  (test-suite
   "ADD immediate instruction tests"

   (test-case "add_imm - basic addition"
     (check-equal?
      (run-jit-void
       (lambda (buf)
         (emit! buf (aarch64-movz X0 10 0))
         (emit! buf (aarch64-add-imm X0 X0 32))
         (emit! buf (aarch64-ret))))
      42))

   (test-case "add_imm - add zero"
     (check-equal?
      (run-jit-void
       (lambda (buf)
         (emit! buf (aarch64-movz X0 100 0))
         (emit! buf (aarch64-add-imm X0 X0 0))
         (emit! buf (aarch64-ret))))
      100))

   (test-case "add_imm - max 12-bit immediate"
     (check-equal?
      (run-jit-void
       (lambda (buf)
         (emit! buf (aarch64-movz X0 1 0))
         (emit! buf (aarch64-add-imm X0 X0 4095))
         (emit! buf (aarch64-ret))))
      4096))

   (test-case "add_imm - different registers"
     (check-equal?
      (run-jit-1arg
       (lambda (buf)
         (emit! buf (aarch64-add-imm X1 X0 10))
         (emit! buf (aarch64-movz X0 0 0))
         (emit! buf (aarch64-add-reg X0 X0 X1))
         (emit! buf (aarch64-ret)))
       32)
      42))

   (test-case "add_imm - chain additions"
     (check-equal?
      (run-jit-void
       (lambda (buf)
         (emit! buf (aarch64-movz X0 0 0))
         (emit! buf (aarch64-add-imm X0 X0 10))
         (emit! buf (aarch64-add-imm X0 X0 20))
         (emit! buf (aarch64-add-imm X0 X0 12))
         (emit! buf (aarch64-ret))))
      42))))

;; ============================================
;; SUB immediate tests
;; ============================================

(define sub-imm-tests
  (test-suite
   "SUB immediate instruction tests"

   (test-case "sub_imm - basic subtraction"
     (check-equal?
      (run-jit-void
       (lambda (buf)
         (emit! buf (aarch64-movz X0 100 0))
         (emit! buf (aarch64-sub-imm X0 X0 58))
         (emit! buf (aarch64-ret))))
      42))

   (test-case "sub_imm - subtract zero"
     (check-equal?
      (run-jit-void
       (lambda (buf)
         (emit! buf (aarch64-movz X0 42 0))
         (emit! buf (aarch64-sub-imm X0 X0 0))
         (emit! buf (aarch64-ret))))
      42))

   (test-case "sub_imm - subtract to zero"
     (check-equal?
      (run-jit-void
       (lambda (buf)
         (emit! buf (aarch64-movz X0 100 0))
         (emit! buf (aarch64-sub-imm X0 X0 100))
         (emit! buf (aarch64-ret))))
      0))

   (test-case "sub_imm - chain subtractions"
     (check-equal?
      (run-jit-void
       (lambda (buf)
         (emit! buf (aarch64-movz X0 100 0))
         (emit! buf (aarch64-sub-imm X0 X0 20))
         (emit! buf (aarch64-sub-imm X0 X0 30))
         (emit! buf (aarch64-sub-imm X0 X0 8))
         (emit! buf (aarch64-ret))))
      42))))

;; ============================================
;; ADD register tests
;; ============================================

(define add-reg-tests
  (test-suite
   "ADD register instruction tests"

   (test-case "add_reg - basic addition"
     (check-equal?
      (run-jit-2arg
       (lambda (buf)
         (emit! buf (aarch64-add-reg X0 X0 X1))
         (emit! buf (aarch64-ret)))
       10 32)
      42))

   (test-case "add_reg - same register (double)"
     (check-equal?
      (run-jit-1arg
       (lambda (buf)
         (emit! buf (aarch64-add-reg X0 X0 X0))
         (emit! buf (aarch64-ret)))
       21)
      42))

   (test-case "add_reg - add with xzr"
     (check-equal?
      (run-jit-1arg
       (lambda (buf)
         (emit! buf (aarch64-add-reg X0 X0 XZR))
         (emit! buf (aarch64-ret)))
       42)
      42))

   (test-case "add_reg - negative numbers"
     (check-equal?
      (run-jit-2arg
       (lambda (buf)
         (emit! buf (aarch64-add-reg X0 X0 X1))
         (emit! buf (aarch64-ret)))
       -10 2)
      -8))

   (test-case "add_reg - both negative"
     (check-equal?
      (run-jit-2arg
       (lambda (buf)
         (emit! buf (aarch64-add-reg X0 X0 X1))
         (emit! buf (aarch64-ret)))
       -20 -22)
      -42))

   (test-case "add_reg - three registers"
     (check-equal?
      (run-jit-3arg
       (lambda (buf)
         (emit! buf (aarch64-add-reg X0 X0 X1))
         (emit! buf (aarch64-add-reg X0 X0 X2))
         (emit! buf (aarch64-ret)))
       10 20 12)
      42))))

;; ============================================
;; SUB register tests
;; ============================================

(define sub-reg-tests
  (test-suite
   "SUB register instruction tests"

   (test-case "sub_reg - basic subtraction"
     (check-equal?
      (run-jit-2arg
       (lambda (buf)
         (emit! buf (aarch64-sub-reg X0 X0 X1))
         (emit! buf (aarch64-ret)))
       100 58)
      42))

   (test-case "sub_reg - same register (zero)"
     (check-equal?
      (run-jit-1arg
       (lambda (buf)
         (emit! buf (aarch64-sub-reg X0 X0 X0))
         (emit! buf (aarch64-ret)))
       42)
      0))

   (test-case "sub_reg - negate (0 - x)"
     (check-equal?
      (run-jit-1arg
       (lambda (buf)
         (emit! buf (aarch64-sub-reg X0 XZR X0))
         (emit! buf (aarch64-ret)))
       42)
      -42))

   (test-case "sub_reg - negate negative"
     (check-equal?
      (run-jit-1arg
       (lambda (buf)
         (emit! buf (aarch64-sub-reg X0 XZR X0))
         (emit! buf (aarch64-ret)))
       -42)
      42))

   (test-case "sub_reg - result negative"
     (check-equal?
      (run-jit-2arg
       (lambda (buf)
         (emit! buf (aarch64-sub-reg X0 X0 X1))
         (emit! buf (aarch64-ret)))
       10 52)
      -42))))

;; ============================================
;; MUL tests
;; ============================================

(define mul-tests
  (test-suite
   "MUL instruction tests"

   (test-case "mul - basic multiplication"
     (check-equal?
      (run-jit-2arg
       (lambda (buf)
         (emit! buf (aarch64-mul X0 X0 X1))
         (emit! buf (aarch64-ret)))
       6 7)
      42))

   (test-case "mul - multiply by zero"
     (check-equal?
      (run-jit-2arg
       (lambda (buf)
         (emit! buf (aarch64-mul X0 X0 X1))
         (emit! buf (aarch64-ret)))
       42 0)
      0))

   (test-case "mul - multiply by one"
     (check-equal?
      (run-jit-2arg
       (lambda (buf)
         (emit! buf (aarch64-mul X0 X0 X1))
         (emit! buf (aarch64-ret)))
       42 1)
      42))

   (test-case "mul - negative number"
     (check-equal?
      (run-jit-2arg
       (lambda (buf)
         (emit! buf (aarch64-mul X0 X0 X1))
         (emit! buf (aarch64-ret)))
       -6 7)
      -42))

   (test-case "mul - both negative"
     (check-equal?
      (run-jit-2arg
       (lambda (buf)
         (emit! buf (aarch64-mul X0 X0 X1))
         (emit! buf (aarch64-ret)))
       -6 -7)
      42))

   (test-case "mul - large numbers"
     (check-equal?
      (run-jit-2arg
       (lambda (buf)
         (emit! buf (aarch64-mul X0 X0 X1))
         (emit! buf (aarch64-ret)))
       1000000 1000000)
      1000000000000))

   (test-case "mul - square"
     (check-equal?
      (run-jit-1arg
       (lambda (buf)
         (emit! buf (aarch64-mul X0 X0 X0))
         (emit! buf (aarch64-ret)))
       7)
      49))))

;; ============================================
;; SDIV tests
;; ============================================

(define sdiv-tests
  (test-suite
   "SDIV instruction tests"

   (test-case "sdiv - basic division"
     (check-equal?
      (run-jit-2arg
       (lambda (buf)
         (emit! buf (aarch64-sdiv X0 X0 X1))
         (emit! buf (aarch64-ret)))
       42 6)
      7))

   (test-case "sdiv - exact division"
     (check-equal?
      (run-jit-2arg
       (lambda (buf)
         (emit! buf (aarch64-sdiv X0 X0 X1))
         (emit! buf (aarch64-ret)))
       126 3)
      42))

   (test-case "sdiv - negative dividend"
     (check-equal?
      (run-jit-2arg
       (lambda (buf)
         (emit! buf (aarch64-sdiv X0 X0 X1))
         (emit! buf (aarch64-ret)))
       -42 6)
      -7))

   (test-case "sdiv - negative divisor"
     (check-equal?
      (run-jit-2arg
       (lambda (buf)
         (emit! buf (aarch64-sdiv X0 X0 X1))
         (emit! buf (aarch64-ret)))
       42 -6)
      -7))

   (test-case "sdiv - both negative"
     (check-equal?
      (run-jit-2arg
       (lambda (buf)
         (emit! buf (aarch64-sdiv X0 X0 X1))
         (emit! buf (aarch64-ret)))
       -42 -6)
      7))

   (test-case "sdiv - zero dividend"
     (check-equal?
      (run-jit-2arg
       (lambda (buf)
         (emit! buf (aarch64-sdiv X0 X0 X1))
         (emit! buf (aarch64-ret)))
       0 42)
      0))

   (test-case "sdiv - divide by 1"
     (check-equal?
      (run-jit-2arg
       (lambda (buf)
         (emit! buf (aarch64-sdiv X0 X0 X1))
         (emit! buf (aarch64-ret)))
       42 1)
      42))

   (test-case "sdiv - truncation"
     (check-equal?
      (run-jit-2arg
       (lambda (buf)
         (emit! buf (aarch64-sdiv X0 X0 X1))
         (emit! buf (aarch64-ret)))
       10 3)
      3))))

;; ============================================
;; RET tests
;; ============================================

(define ret-tests
  (test-suite
   "RET instruction tests"

   (test-case "ret - basic return"
     (check-equal?
      (run-jit-void
       (lambda (buf)
         (emit! buf (aarch64-movz X0 42 0))
         (emit! buf (aarch64-ret))))
      42))

   (test-case "ret_reg - return via LR"
     (check-equal?
      (run-jit-void
       (lambda (buf)
         (emit! buf (aarch64-movz X0 42 0))
         (emit! buf (aarch64-ret-reg LR))))
      42))))

;; ============================================
;; B (unconditional branch) tests
;; ============================================

(define b-tests
  (test-suite
   "B instruction tests"

   (test-case "b - forward branch skip 1"
     (check-equal?
      (run-jit-void
       (lambda (buf)
         (emit! buf (aarch64-b 2))              ; skip next instruction
         (emit! buf (aarch64-movz X0 999 0))    ; skipped
         (emit! buf (aarch64-movz X0 42 0))     ; target
         (emit! buf (aarch64-ret))))
      42))

   (test-case "b - forward branch skip 2"
     (check-equal?
      (run-jit-void
       (lambda (buf)
         (emit! buf (aarch64-b 3))              ; skip 2 instructions
         (emit! buf (aarch64-movz X0 111 0))    ; skipped
         (emit! buf (aarch64-movz X0 222 0))    ; skipped
         (emit! buf (aarch64-movz X0 42 0))     ; target
         (emit! buf (aarch64-ret))))
      42))

   (test-case "b - backward branch (loop sum 1..10)"
     (check-equal?
      (run-jit-void
       (lambda (buf)
         (emit! buf (aarch64-movz X0 0 0))      ; sum = 0
         (emit! buf (aarch64-movz X1 10 0))     ; counter = 10
         ;; loop:
         (emit! buf (aarch64-add-reg X0 X0 X1)) ; sum += counter
         (emit! buf (aarch64-sub-imm X1 X1 1))  ; counter--
         (emit! buf (aarch64-cmp-imm X1 0))     ; if counter > 0
         (emit! buf (aarch64-b-cond COND-GT -3)); goto loop
         (emit! buf (aarch64-ret))))
      55))   ; 1+2+...+10 = 55

   (test-case "b - backward branch (loop sum 1..100)"
     (check-equal?
      (run-jit-void
       (lambda (buf)
         (emit! buf (aarch64-movz X0 0 0))
         (emit! buf (aarch64-movz X1 100 0))
         (emit! buf (aarch64-add-reg X0 X0 X1))
         (emit! buf (aarch64-sub-imm X1 X1 1))
         (emit! buf (aarch64-cmp-imm X1 0))
         (emit! buf (aarch64-b-cond COND-GT -3))
         (emit! buf (aarch64-ret))))
      5050))))

;; ============================================
;; B.cond tests
;; ============================================

(define b-cond-tests
  (test-suite
   "B.cond instruction tests"

   (test-case "b.cond EQ - equal (true)"
     (check-equal?
      (run-jit-2arg
       (lambda (buf)
         (emit! buf (aarch64-cmp-reg X0 X1))
         (emit! buf (aarch64-b-cond COND-EQ 3))
         (emit! buf (aarch64-movz X0 0 0))
         (emit! buf (aarch64-ret))
         (emit! buf (aarch64-movz X0 42 0))
         (emit! buf (aarch64-ret)))
       5 5)
      42))

   (test-case "b.cond EQ - equal (false)"
     (check-equal?
      (run-jit-2arg
       (lambda (buf)
         (emit! buf (aarch64-cmp-reg X0 X1))
         (emit! buf (aarch64-b-cond COND-EQ 3))
         (emit! buf (aarch64-movz X0 0 0))
         (emit! buf (aarch64-ret))
         (emit! buf (aarch64-movz X0 42 0))
         (emit! buf (aarch64-ret)))
       5 6)
      0))

   (test-case "b.cond NE - not equal (true)"
     (check-equal?
      (run-jit-2arg
       (lambda (buf)
         (emit! buf (aarch64-cmp-reg X0 X1))
         (emit! buf (aarch64-b-cond COND-NE 3))
         (emit! buf (aarch64-movz X0 0 0))
         (emit! buf (aarch64-ret))
         (emit! buf (aarch64-movz X0 42 0))
         (emit! buf (aarch64-ret)))
       5 6)
      42))

   (test-case "b.cond NE - not equal (false)"
     (check-equal?
      (run-jit-2arg
       (lambda (buf)
         (emit! buf (aarch64-cmp-reg X0 X1))
         (emit! buf (aarch64-b-cond COND-NE 3))
         (emit! buf (aarch64-movz X0 0 0))
         (emit! buf (aarch64-ret))
         (emit! buf (aarch64-movz X0 42 0))
         (emit! buf (aarch64-ret)))
       5 5)
      0))

   (test-case "b.cond GT - greater than (true)"
     (check-equal?
      (run-jit-2arg
       (lambda (buf)
         (emit! buf (aarch64-cmp-reg X0 X1))
         (emit! buf (aarch64-b-cond COND-GT 3))
         (emit! buf (aarch64-movz X0 0 0))
         (emit! buf (aarch64-ret))
         (emit! buf (aarch64-movz X0 42 0))
         (emit! buf (aarch64-ret)))
       10 5)
      42))

   (test-case "b.cond GT - greater than (equal)"
     (check-equal?
      (run-jit-2arg
       (lambda (buf)
         (emit! buf (aarch64-cmp-reg X0 X1))
         (emit! buf (aarch64-b-cond COND-GT 3))
         (emit! buf (aarch64-movz X0 0 0))
         (emit! buf (aarch64-ret))
         (emit! buf (aarch64-movz X0 42 0))
         (emit! buf (aarch64-ret)))
       5 5)
      0))

   (test-case "b.cond GT - greater than (false)"
     (check-equal?
      (run-jit-2arg
       (lambda (buf)
         (emit! buf (aarch64-cmp-reg X0 X1))
         (emit! buf (aarch64-b-cond COND-GT 3))
         (emit! buf (aarch64-movz X0 0 0))
         (emit! buf (aarch64-ret))
         (emit! buf (aarch64-movz X0 42 0))
         (emit! buf (aarch64-ret)))
       3 5)
      0))

   (test-case "b.cond LT - less than (true)"
     (check-equal?
      (run-jit-2arg
       (lambda (buf)
         (emit! buf (aarch64-cmp-reg X0 X1))
         (emit! buf (aarch64-b-cond COND-LT 3))
         (emit! buf (aarch64-movz X0 0 0))
         (emit! buf (aarch64-ret))
         (emit! buf (aarch64-movz X0 42 0))
         (emit! buf (aarch64-ret)))
       3 5)
      42))

   (test-case "b.cond LT - less than (equal)"
     (check-equal?
      (run-jit-2arg
       (lambda (buf)
         (emit! buf (aarch64-cmp-reg X0 X1))
         (emit! buf (aarch64-b-cond COND-LT 3))
         (emit! buf (aarch64-movz X0 0 0))
         (emit! buf (aarch64-ret))
         (emit! buf (aarch64-movz X0 42 0))
         (emit! buf (aarch64-ret)))
       5 5)
      0))

   (test-case "b.cond LT - less than (false)"
     (check-equal?
      (run-jit-2arg
       (lambda (buf)
         (emit! buf (aarch64-cmp-reg X0 X1))
         (emit! buf (aarch64-b-cond COND-LT 3))
         (emit! buf (aarch64-movz X0 0 0))
         (emit! buf (aarch64-ret))
         (emit! buf (aarch64-movz X0 42 0))
         (emit! buf (aarch64-ret)))
       10 5)
      0))

   (test-case "b.cond GE - greater or equal (greater)"
     (check-equal?
      (run-jit-2arg
       (lambda (buf)
         (emit! buf (aarch64-cmp-reg X0 X1))
         (emit! buf (aarch64-b-cond COND-GE 3))
         (emit! buf (aarch64-movz X0 0 0))
         (emit! buf (aarch64-ret))
         (emit! buf (aarch64-movz X0 42 0))
         (emit! buf (aarch64-ret)))
       10 5)
      42))

   (test-case "b.cond GE - greater or equal (equal)"
     (check-equal?
      (run-jit-2arg
       (lambda (buf)
         (emit! buf (aarch64-cmp-reg X0 X1))
         (emit! buf (aarch64-b-cond COND-GE 3))
         (emit! buf (aarch64-movz X0 0 0))
         (emit! buf (aarch64-ret))
         (emit! buf (aarch64-movz X0 42 0))
         (emit! buf (aarch64-ret)))
       5 5)
      42))

   (test-case "b.cond GE - greater or equal (less)"
     (check-equal?
      (run-jit-2arg
       (lambda (buf)
         (emit! buf (aarch64-cmp-reg X0 X1))
         (emit! buf (aarch64-b-cond COND-GE 3))
         (emit! buf (aarch64-movz X0 0 0))
         (emit! buf (aarch64-ret))
         (emit! buf (aarch64-movz X0 42 0))
         (emit! buf (aarch64-ret)))
       3 5)
      0))

   (test-case "b.cond LE - less or equal (less)"
     (check-equal?
      (run-jit-2arg
       (lambda (buf)
         (emit! buf (aarch64-cmp-reg X0 X1))
         (emit! buf (aarch64-b-cond COND-LE 3))
         (emit! buf (aarch64-movz X0 0 0))
         (emit! buf (aarch64-ret))
         (emit! buf (aarch64-movz X0 42 0))
         (emit! buf (aarch64-ret)))
       3 5)
      42))

   (test-case "b.cond LE - less or equal (equal)"
     (check-equal?
      (run-jit-2arg
       (lambda (buf)
         (emit! buf (aarch64-cmp-reg X0 X1))
         (emit! buf (aarch64-b-cond COND-LE 3))
         (emit! buf (aarch64-movz X0 0 0))
         (emit! buf (aarch64-ret))
         (emit! buf (aarch64-movz X0 42 0))
         (emit! buf (aarch64-ret)))
       5 5)
      42))

   (test-case "b.cond LE - less or equal (greater)"
     (check-equal?
      (run-jit-2arg
       (lambda (buf)
         (emit! buf (aarch64-cmp-reg X0 X1))
         (emit! buf (aarch64-b-cond COND-LE 3))
         (emit! buf (aarch64-movz X0 0 0))
         (emit! buf (aarch64-ret))
         (emit! buf (aarch64-movz X0 42 0))
         (emit! buf (aarch64-ret)))
       10 5)
      0))))

;; ============================================
;; CMP tests
;; ============================================

(define cmp-tests
  (test-suite
   "CMP instruction tests"

   (test-case "cmp_reg - equal values"
     (check-equal?
      (run-jit-2arg
       (lambda (buf)
         (emit! buf (aarch64-cmp-reg X0 X1))
         (emit! buf (aarch64-b-cond COND-EQ 3))
         (emit! buf (aarch64-movz X0 0 0))
         (emit! buf (aarch64-ret))
         (emit! buf (aarch64-movz X0 1 0))
         (emit! buf (aarch64-ret)))
       42 42)
      1))

   (test-case "cmp_reg - not equal"
     (check-equal?
      (run-jit-2arg
       (lambda (buf)
         (emit! buf (aarch64-cmp-reg X0 X1))
         (emit! buf (aarch64-b-cond COND-EQ 3))
         (emit! buf (aarch64-movz X0 0 0))
         (emit! buf (aarch64-ret))
         (emit! buf (aarch64-movz X0 1 0))
         (emit! buf (aarch64-ret)))
       42 43)
      0))

   (test-case "cmp_reg - negative values"
     (check-equal?
      (run-jit-2arg
       (lambda (buf)
         (emit! buf (aarch64-cmp-reg X0 X1))
         (emit! buf (aarch64-b-cond COND-LT 3))
         (emit! buf (aarch64-movz X0 0 0))
         (emit! buf (aarch64-ret))
         (emit! buf (aarch64-movz X0 1 0))
         (emit! buf (aarch64-ret)))
       -10 5)
      1))

   (test-case "cmp_imm - equal to immediate"
     (check-equal?
      (run-jit-1arg
       (lambda (buf)
         (emit! buf (aarch64-cmp-imm X0 42))
         (emit! buf (aarch64-b-cond COND-EQ 3))
         (emit! buf (aarch64-movz X0 0 0))
         (emit! buf (aarch64-ret))
         (emit! buf (aarch64-movz X0 1 0))
         (emit! buf (aarch64-ret)))
       42)
      1))

   (test-case "cmp_imm - not equal to immediate"
     (check-equal?
      (run-jit-1arg
       (lambda (buf)
         (emit! buf (aarch64-cmp-imm X0 42))
         (emit! buf (aarch64-b-cond COND-EQ 3))
         (emit! buf (aarch64-movz X0 0 0))
         (emit! buf (aarch64-ret))
         (emit! buf (aarch64-movz X0 1 0))
         (emit! buf (aarch64-ret)))
       43)
      0))

   (test-case "cmp_imm - compare with zero"
     (check-equal?
      (run-jit-1arg
       (lambda (buf)
         (emit! buf (aarch64-cmp-imm X0 0))
         (emit! buf (aarch64-b-cond COND-EQ 3))
         (emit! buf (aarch64-movz X0 0 0))
         (emit! buf (aarch64-ret))
         (emit! buf (aarch64-movz X0 1 0))
         (emit! buf (aarch64-ret)))
       0)
      1))

   (test-case "cmp_imm - greater than immediate"
     (check-equal?
      (run-jit-1arg
       (lambda (buf)
         (emit! buf (aarch64-cmp-imm X0 10))
         (emit! buf (aarch64-b-cond COND-GT 3))
         (emit! buf (aarch64-movz X0 0 0))
         (emit! buf (aarch64-ret))
         (emit! buf (aarch64-movz X0 1 0))
         (emit! buf (aarch64-ret)))
       15)
      1))))

;; ============================================
;; NOP tests
;; ============================================

(define nop-tests
  (test-suite
   "NOP instruction tests"

   (test-case "nop - no operation"
     (check-equal?
      (run-jit-void
       (lambda (buf)
         (emit! buf (aarch64-nop))
         (emit! buf (aarch64-nop))
         (emit! buf (aarch64-nop))
         (emit! buf (aarch64-movz X0 42 0))
         (emit! buf (aarch64-ret))))
      42))

   (test-case "nop - as branch padding"
     (check-equal?
      (run-jit-void
       (lambda (buf)
         (emit! buf (aarch64-b 3))              ; skip 3 instructions
         (emit! buf (aarch64-nop))
         (emit! buf (aarch64-nop))
         (emit! buf (aarch64-movz X0 999 0))    ; skipped
         (emit! buf (aarch64-movz X0 42 0))     ; target
         (emit! buf (aarch64-ret))))
      42))

   (test-case "nop - encoding value"
     (check-equal? (aarch64-nop) #xD503201F))))

;; ============================================
;; Integration tests
;; ============================================

(define integration-tests
  (test-suite
   "Integration tests"

   (test-case "absolute value - negative"
     (check-equal?
      (run-jit-1arg
       (lambda (buf)
         (emit! buf (aarch64-cmp-imm X0 0))
         (emit! buf (aarch64-b-cond COND-GE 2))
         (emit! buf (aarch64-sub-reg X0 XZR X0))
         (emit! buf (aarch64-ret)))
       -42)
      42))

   (test-case "absolute value - positive"
     (check-equal?
      (run-jit-1arg
       (lambda (buf)
         (emit! buf (aarch64-cmp-imm X0 0))
         (emit! buf (aarch64-b-cond COND-GE 2))
         (emit! buf (aarch64-sub-reg X0 XZR X0))
         (emit! buf (aarch64-ret)))
       42)
      42))

   (test-case "absolute value - zero"
     (check-equal?
      (run-jit-1arg
       (lambda (buf)
         (emit! buf (aarch64-cmp-imm X0 0))
         (emit! buf (aarch64-b-cond COND-GE 2))
         (emit! buf (aarch64-sub-reg X0 XZR X0))
         (emit! buf (aarch64-ret)))
       0)
      0))

   (test-case "max of two - first larger"
     (check-equal?
      (run-jit-2arg
       (lambda (buf)
         (emit! buf (aarch64-cmp-reg X0 X1))
         (emit! buf (aarch64-b-cond COND-GE 2))
         (emit! buf (aarch64-add-reg X0 X1 XZR))
         (emit! buf (aarch64-ret)))
       42 10)
      42))

   (test-case "max of two - second larger"
     (check-equal?
      (run-jit-2arg
       (lambda (buf)
         (emit! buf (aarch64-cmp-reg X0 X1))
         (emit! buf (aarch64-b-cond COND-GE 2))
         (emit! buf (aarch64-add-reg X0 X1 XZR))
         (emit! buf (aarch64-ret)))
       10 42)
      42))

   (test-case "max of two - equal"
     (check-equal?
      (run-jit-2arg
       (lambda (buf)
         (emit! buf (aarch64-cmp-reg X0 X1))
         (emit! buf (aarch64-b-cond COND-GE 2))
         (emit! buf (aarch64-add-reg X0 X1 XZR))
         (emit! buf (aarch64-ret)))
       42 42)
      42))

   (test-case "min of two - first smaller"
     (check-equal?
      (run-jit-2arg
       (lambda (buf)
         (emit! buf (aarch64-cmp-reg X0 X1))
         (emit! buf (aarch64-b-cond COND-LE 2))
         (emit! buf (aarch64-add-reg X0 X1 XZR))
         (emit! buf (aarch64-ret)))
       10 42)
      10))

   (test-case "min of two - second smaller"
     (check-equal?
      (run-jit-2arg
       (lambda (buf)
         (emit! buf (aarch64-cmp-reg X0 X1))
         (emit! buf (aarch64-b-cond COND-LE 2))
         (emit! buf (aarch64-add-reg X0 X1 XZR))
         (emit! buf (aarch64-ret)))
       42 10)
      10))

   (test-case "min of two - equal"
     (check-equal?
      (run-jit-2arg
       (lambda (buf)
         (emit! buf (aarch64-cmp-reg X0 X1))
         (emit! buf (aarch64-b-cond COND-LE 2))
         (emit! buf (aarch64-add-reg X0 X1 XZR))
         (emit! buf (aarch64-ret)))
       42 42)
      42))

   (test-case "factorial 0"
     (check-equal?
      (run-jit-1arg
       (lambda (buf)
         (emit! buf (aarch64-movz X1 1 0))
         (emit! buf (aarch64-cmp-imm X0 1))
         (emit! buf (aarch64-b-cond COND-LE 4))
         (emit! buf (aarch64-mul X1 X1 X0))
         (emit! buf (aarch64-sub-imm X0 X0 1))
         (emit! buf (aarch64-b -4))
         (emit! buf (aarch64-add-reg X0 X1 XZR))
         (emit! buf (aarch64-ret)))
       0)
      1))

   (test-case "factorial 1"
     (check-equal?
      (run-jit-1arg
       (lambda (buf)
         (emit! buf (aarch64-movz X1 1 0))
         (emit! buf (aarch64-cmp-imm X0 1))
         (emit! buf (aarch64-b-cond COND-LE 4))
         (emit! buf (aarch64-mul X1 X1 X0))
         (emit! buf (aarch64-sub-imm X0 X0 1))
         (emit! buf (aarch64-b -4))
         (emit! buf (aarch64-add-reg X0 X1 XZR))
         (emit! buf (aarch64-ret)))
       1)
      1))

   (test-case "factorial 5"
     (check-equal?
      (run-jit-1arg
       (lambda (buf)
         (emit! buf (aarch64-movz X1 1 0))
         (emit! buf (aarch64-cmp-imm X0 1))
         (emit! buf (aarch64-b-cond COND-LE 4))
         (emit! buf (aarch64-mul X1 X1 X0))
         (emit! buf (aarch64-sub-imm X0 X0 1))
         (emit! buf (aarch64-b -4))
         (emit! buf (aarch64-add-reg X0 X1 XZR))
         (emit! buf (aarch64-ret)))
       5)
      120))

   (test-case "factorial 10"
     (check-equal?
      (run-jit-1arg
       (lambda (buf)
         (emit! buf (aarch64-movz X1 1 0))
         (emit! buf (aarch64-cmp-imm X0 1))
         (emit! buf (aarch64-b-cond COND-LE 4))
         (emit! buf (aarch64-mul X1 X1 X0))
         (emit! buf (aarch64-sub-imm X0 X0 1))
         (emit! buf (aarch64-b -4))
         (emit! buf (aarch64-add-reg X0 X1 XZR))
         (emit! buf (aarch64-ret)))
       10)
      3628800))

   (test-case "fibonacci 0"
     (check-equal?
      (run-jit-1arg
       (lambda (buf)
         (emit! buf (aarch64-cmp-imm X0 1))
         (emit! buf (aarch64-b-cond COND-LE 11))
         (emit! buf (aarch64-movz X1 0 0))
         (emit! buf (aarch64-movz X2 1 0))
         (emit! buf (aarch64-sub-imm X3 X0 1))
         (emit! buf (aarch64-add-reg X4 X1 X2))
         (emit! buf (aarch64-add-reg X1 X2 XZR))
         (emit! buf (aarch64-add-reg X2 X4 XZR))
         (emit! buf (aarch64-sub-imm X3 X3 1))
         (emit! buf (aarch64-cmp-imm X3 0))
         (emit! buf (aarch64-b-cond COND-GT -5))
         (emit! buf (aarch64-add-reg X0 X2 XZR))
         (emit! buf (aarch64-ret)))
       0)
      0))

   (test-case "fibonacci 1"
     (check-equal?
      (run-jit-1arg
       (lambda (buf)
         (emit! buf (aarch64-cmp-imm X0 1))
         (emit! buf (aarch64-b-cond COND-LE 11))
         (emit! buf (aarch64-movz X1 0 0))
         (emit! buf (aarch64-movz X2 1 0))
         (emit! buf (aarch64-sub-imm X3 X0 1))
         (emit! buf (aarch64-add-reg X4 X1 X2))
         (emit! buf (aarch64-add-reg X1 X2 XZR))
         (emit! buf (aarch64-add-reg X2 X4 XZR))
         (emit! buf (aarch64-sub-imm X3 X3 1))
         (emit! buf (aarch64-cmp-imm X3 0))
         (emit! buf (aarch64-b-cond COND-GT -5))
         (emit! buf (aarch64-add-reg X0 X2 XZR))
         (emit! buf (aarch64-ret)))
       1)
      1))

   (test-case "fibonacci 10"
     (check-equal?
      (run-jit-1arg
       (lambda (buf)
         (emit! buf (aarch64-cmp-imm X0 1))
         (emit! buf (aarch64-b-cond COND-LE 11))
         (emit! buf (aarch64-movz X1 0 0))
         (emit! buf (aarch64-movz X2 1 0))
         (emit! buf (aarch64-sub-imm X3 X0 1))
         (emit! buf (aarch64-add-reg X4 X1 X2))
         (emit! buf (aarch64-add-reg X1 X2 XZR))
         (emit! buf (aarch64-add-reg X2 X4 XZR))
         (emit! buf (aarch64-sub-imm X3 X3 1))
         (emit! buf (aarch64-cmp-imm X3 0))
         (emit! buf (aarch64-b-cond COND-GT -5))
         (emit! buf (aarch64-add-reg X0 X2 XZR))
         (emit! buf (aarch64-ret)))
       10)
      55))

   (test-case "gcd(48, 18)"
     (check-equal?
      (run-jit-2arg
       (lambda (buf)
         ;; gcd using euclidean algorithm
         (emit! buf (aarch64-cmp-imm X1 0))
         (emit! buf (aarch64-b-cond COND-EQ 7))
         (emit! buf (aarch64-sdiv X2 X0 X1))
         (emit! buf (aarch64-mul X2 X2 X1))
         (emit! buf (aarch64-sub-reg X2 X0 X2))
         (emit! buf (aarch64-add-reg X0 X1 XZR))
         (emit! buf (aarch64-add-reg X1 X2 XZR))
         (emit! buf (aarch64-b -7))
         (emit! buf (aarch64-ret)))
       48 18)
      6))

   (test-case "gcd(17, 13)"
     (check-equal?
      (run-jit-2arg
       (lambda (buf)
         (emit! buf (aarch64-cmp-imm X1 0))
         (emit! buf (aarch64-b-cond COND-EQ 7))
         (emit! buf (aarch64-sdiv X2 X0 X1))
         (emit! buf (aarch64-mul X2 X2 X1))
         (emit! buf (aarch64-sub-reg X2 X0 X2))
         (emit! buf (aarch64-add-reg X0 X1 XZR))
         (emit! buf (aarch64-add-reg X1 X2 XZR))
         (emit! buf (aarch64-b -7))
         (emit! buf (aarch64-ret)))
       17 13)
      1))

   (test-case "complex expression (a+b)*(c-d)/e = 42"
     (check-equal?
      (run-jit-5arg
       (lambda (buf)
         (emit! buf (aarch64-add-reg X5 X0 X1))
         (emit! buf (aarch64-sub-reg X6 X2 X3))
         (emit! buf (aarch64-mul X5 X5 X6))
         (emit! buf (aarch64-sdiv X0 X5 X4))
         (emit! buf (aarch64-ret)))
       3 4 10 4 1)  ; (3+4)*(10-4)/1 = 7*6 = 42
      42))

   (test-case "complex expression (a+b)*(c-d)/e = 50"
     (check-equal?
      (run-jit-5arg
       (lambda (buf)
         (emit! buf (aarch64-add-reg X5 X0 X1))
         (emit! buf (aarch64-sub-reg X6 X2 X3))
         (emit! buf (aarch64-mul X5 X5 X6))
         (emit! buf (aarch64-sdiv X0 X5 X4))
         (emit! buf (aarch64-ret)))
       10 10 15 5 4)  ; (10+10)*(15-5)/4 = 20*10/4 = 50
      50))))

;; ============================================
;; Encoding verification tests
;; ============================================

(define encoding-tests
  (test-suite
   "Encoding verification tests"

   (test-case "stp_pre encoding has correct prefix"
     (let ([inst (aarch64-stp-pre X19 X20 SP -16)])
       (check-equal? (arithmetic-shift inst -27) #b10101)))

   (test-case "ldp_post encoding has correct prefix"
     (let ([inst (aarch64-ldp-post X19 X20 SP 16)])
       (check-equal? (arithmetic-shift inst -27) #b10101)))

   (test-case "br encoding has correct prefix"
     (let ([inst (aarch64-br X9)])
       (check-equal? (arithmetic-shift inst -25) #b1101011)))

   (test-case "blr encoding has correct prefix"
     (let ([inst (aarch64-blr X9)])
       (check-equal? (arithmetic-shift inst -25) #b1101011)))

   (test-case "ret encoding has correct value"
     (check-equal? (aarch64-ret) (aarch64-ret-reg LR)))))

;; ============================================
;; Run all tests
;; ============================================

(define all-tests
  (test-suite
   "Dynasm Comprehensive Tests"
   movz-tests
   movk-tests
   add-imm-tests
   sub-imm-tests
   add-reg-tests
   sub-reg-tests
   mul-tests
   sdiv-tests
   ret-tests
   b-tests
   b-cond-tests
   cmp-tests
   nop-tests
   integration-tests
   encoding-tests))

(module+ main
  (run-tests all-tests 'verbose))

(module+ test
  (require rackunit/text-ui)
  (run-tests all-tests))
