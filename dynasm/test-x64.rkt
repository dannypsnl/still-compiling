#lang racket/base

;;; Comprehensive tests for x64 dynasm instructions
;;; Run: racket test-x64.rkt

(require rackunit
         rackunit/text-ui
         ffi/unsafe
         "dynasm.rkt"
         "jit-functions-x64.rkt")

;; ============================================
;; Architecture check - skip tests if not x86_64
;; ============================================

(unless (eq? (system-type 'arch) 'x86_64)
  (printf "Skipping x64 tests: system architecture is ~a\n" (system-type 'arch))
  (exit 0))

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

;; ============================================
;; MOV immediate tests
;; ============================================

(define mov-imm-tests
  (test-suite
   "MOV immediate instruction tests"

   (test-case "mov_imm32 - basic immediate"
     (check-equal?
      (run-jit-void
       (lambda (buf)
         (x64-mov-imm32 buf RAX 42)
         (x64-ret buf)))
      42))

   (test-case "mov_imm32 - zero value"
     (check-equal?
      (run-jit-void
       (lambda (buf)
         (x64-mov-imm32 buf RAX 0)
         (x64-ret buf)))
      0))

   (test-case "mov_imm32 - negative value"
     (check-equal?
      (run-jit-void
       (lambda (buf)
         (x64-mov-imm32 buf RAX -42)
         (x64-ret buf)))
      -42))

   (test-case "mov_imm64 - large positive value"
     (check-equal?
      (run-jit-void
       (lambda (buf)
         (x64-mov-imm64 buf RAX #x123456789ABCDEF0)
         (x64-ret buf)))
      #x123456789ABCDEF0))

   (test-case "mov_imm32 - max 32-bit value"
     (check-equal?
      (run-jit-void
       (lambda (buf)
         (x64-mov-imm32 buf RAX #x7FFFFFFF)
         (x64-ret buf)))
      #x7FFFFFFF))

   (test-case "mov_imm32 - using different register"
     (check-equal?
      (run-jit-void
       (lambda (buf)
         (x64-mov-imm32 buf RCX 42)
         (x64-mov-reg buf RAX RCX)
         (x64-ret buf)))
      42))

   (test-case "mov_imm32 - using R8-R15"
     (check-equal?
      (run-jit-void
       (lambda (buf)
         (x64-mov-imm32 buf R8 42)
         (x64-mov-reg buf RAX R8)
         (x64-ret buf)))
      42))))

;; ============================================
;; MOV register tests
;; ============================================

(define mov-reg-tests
  (test-suite
   "MOV register instruction tests"

   (test-case "mov_reg - basic copy"
     (check-equal?
      (run-jit-1arg
       (lambda (buf)
         ;; RDI contains arg, copy to RAX
         (x64-mov-reg buf RAX RDI)
         (x64-ret buf))
       42)
      42))

   (test-case "mov_reg - copy between extended registers"
     (check-equal?
      (run-jit-void
       (lambda (buf)
         (x64-mov-imm32 buf R8 42)
         (x64-mov-reg buf R9 R8)
         (x64-mov-reg buf RAX R9)
         (x64-ret buf)))
      42))))

;; ============================================
;; ADD instruction tests
;; ============================================

(define add-tests
  (test-suite
   "ADD instruction tests"

   (test-case "add_reg - basic addition"
     (check-equal?
      (run-jit-2arg
       (lambda (buf)
         ;; RDI=arg1, RSI=arg2
         (x64-mov-reg buf RAX RDI)
         (x64-add-reg buf RAX RSI)
         (x64-ret buf))
       10 32)
      42))

   (test-case "add_imm32 - add immediate"
     (check-equal?
      (run-jit-1arg
       (lambda (buf)
         (x64-mov-reg buf RAX RDI)
         (x64-add-imm32 buf RAX 10)
         (x64-ret buf))
       32)
      42))

   (test-case "add_imm8 - add small immediate"
     (check-equal?
      (run-jit-1arg
       (lambda (buf)
         (x64-mov-reg buf RAX RDI)
         (x64-add-imm8 buf RAX 10)
         (x64-ret buf))
       32)
      42))

   (test-case "add_reg - same register (double)"
     (check-equal?
      (run-jit-1arg
       (lambda (buf)
         (x64-mov-reg buf RAX RDI)
         (x64-add-reg buf RAX RAX)
         (x64-ret buf))
       21)
      42))

   (test-case "add_reg - negative numbers"
     (check-equal?
      (run-jit-2arg
       (lambda (buf)
         (x64-mov-reg buf RAX RDI)
         (x64-add-reg buf RAX RSI)
         (x64-ret buf))
       -10 52)
      42))))

;; ============================================
;; SUB instruction tests
;; ============================================

(define sub-tests
  (test-suite
   "SUB instruction tests"

   (test-case "sub_reg - basic subtraction"
     (check-equal?
      (run-jit-2arg
       (lambda (buf)
         (x64-mov-reg buf RAX RDI)
         (x64-sub-reg buf RAX RSI)
         (x64-ret buf))
       100 58)
      42))

   (test-case "sub_imm32 - subtract immediate"
     (check-equal?
      (run-jit-1arg
       (lambda (buf)
         (x64-mov-reg buf RAX RDI)
         (x64-sub-imm32 buf RAX 58)
         (x64-ret buf))
       100)
      42))

   (test-case "sub_imm8 - subtract small immediate"
     (check-equal?
      (run-jit-1arg
       (lambda (buf)
         (x64-mov-reg buf RAX RDI)
         (x64-sub-imm8 buf RAX 58)
         (x64-ret buf))
       100)
      42))

   (test-case "sub_reg - same register (zero)"
     (check-equal?
      (run-jit-1arg
       (lambda (buf)
         (x64-mov-reg buf RAX RDI)
         (x64-sub-reg buf RAX RAX)
         (x64-ret buf))
       42)
      0))))

;; ============================================
;; IMUL instruction tests
;; ============================================

(define imul-tests
  (test-suite
   "IMUL instruction tests"

   (test-case "imul_reg - basic multiplication"
     (check-equal?
      (run-jit-2arg
       (lambda (buf)
         (x64-mov-reg buf RAX RDI)
         (x64-imul-reg buf RAX RSI)
         (x64-ret buf))
       6 7)
      42))

   (test-case "imul_reg - multiply by zero"
     (check-equal?
      (run-jit-2arg
       (lambda (buf)
         (x64-mov-reg buf RAX RDI)
         (x64-imul-reg buf RAX RSI)
         (x64-ret buf))
       42 0)
      0))

   (test-case "imul_reg - negative number"
     (check-equal?
      (run-jit-2arg
       (lambda (buf)
         (x64-mov-reg buf RAX RDI)
         (x64-imul-reg buf RAX RSI)
         (x64-ret buf))
       -6 7)
      -42))

   (test-case "imul_reg - both negative"
     (check-equal?
      (run-jit-2arg
       (lambda (buf)
         (x64-mov-reg buf RAX RDI)
         (x64-imul-reg buf RAX RSI)
         (x64-ret buf))
       -6 -7)
      42))

   (test-case "imul_imm32 - multiply with immediate"
     (check-equal?
      (run-jit-1arg
       (lambda (buf)
         (x64-imul-imm32 buf RAX RDI 7)
         (x64-ret buf))
       6)
      42))))

;; ============================================
;; IDIV instruction tests
;; ============================================

(define idiv-tests
  (test-suite
   "IDIV instruction tests"

   (test-case "idiv_reg - basic division"
     (check-equal?
      (run-jit-2arg
       (lambda (buf)
         ;; RDI=dividend, RSI=divisor
         (x64-mov-reg buf RAX RDI)
         (x64-cqo buf)
         (x64-idiv-reg buf RSI)
         (x64-ret buf))
       42 6)
      7))

   (test-case "idiv_reg - negative dividend"
     (check-equal?
      (run-jit-2arg
       (lambda (buf)
         (x64-mov-reg buf RAX RDI)
         (x64-cqo buf)
         (x64-idiv-reg buf RSI)
         (x64-ret buf))
       -42 6)
      -7))

   (test-case "idiv_reg - both negative"
     (check-equal?
      (run-jit-2arg
       (lambda (buf)
         (x64-mov-reg buf RAX RDI)
         (x64-cqo buf)
         (x64-idiv-reg buf RSI)
         (x64-ret buf))
       -42 -6)
      7))

   (test-case "idiv_reg - truncation"
     (check-equal?
      (run-jit-2arg
       (lambda (buf)
         (x64-mov-reg buf RAX RDI)
         (x64-cqo buf)
         (x64-idiv-reg buf RSI)
         (x64-ret buf))
       10 3)
      3))))

;; ============================================
;; Shift instruction tests
;; ============================================

(define shift-tests
  (test-suite
   "Shift instruction tests"

   (test-case "shl_imm - shift left"
     (check-equal?
      (run-jit-1arg
       (lambda (buf)
         (x64-mov-reg buf RAX RDI)
         (x64-shl-imm buf RAX 2)
         (x64-ret buf))
       10)
      40))

   (test-case "shr_imm - shift right"
     (check-equal?
      (run-jit-1arg
       (lambda (buf)
         (x64-mov-reg buf RAX RDI)
         (x64-shr-imm buf RAX 2)
         (x64-ret buf))
       40)
      10))

   (test-case "sar_imm - arithmetic shift right"
     (check-equal?
      (run-jit-1arg
       (lambda (buf)
         (x64-mov-reg buf RAX RDI)
         (x64-sar-imm buf RAX 2)
         (x64-ret buf))
       -40)
      -10))))

;; ============================================
;; Compare and branch tests
;; ============================================

(define cmp-jcc-tests
  (test-suite
   "CMP and Jcc instruction tests"

   (test-case "cmp_reg + je - equal (true)"
     (check-equal?
      (run-jit-2arg
       (lambda (buf)
         (x64-cmp-reg buf RDI RSI)
         (x64-jcc-rel8 buf CC-E 7)   ; jump to mov 42
         (x64-mov-imm32 buf RAX 0)
         (x64-ret buf)
         (x64-mov-imm32 buf RAX 42)
         (x64-ret buf))
       5 5)
      42))

   (test-case "cmp_reg + je - equal (false)"
     (check-equal?
      (run-jit-2arg
       (lambda (buf)
         (x64-cmp-reg buf RDI RSI)
         (x64-jcc-rel8 buf CC-E 7)
         (x64-mov-imm32 buf RAX 0)
         (x64-ret buf)
         (x64-mov-imm32 buf RAX 42)
         (x64-ret buf))
       5 6)
      0))

   (test-case "cmp_reg + jne - not equal (true)"
     (check-equal?
      (run-jit-2arg
       (lambda (buf)
         (x64-cmp-reg buf RDI RSI)
         (x64-jcc-rel8 buf CC-NE 7)
         (x64-mov-imm32 buf RAX 0)
         (x64-ret buf)
         (x64-mov-imm32 buf RAX 42)
         (x64-ret buf))
       5 6)
      42))

   (test-case "cmp_reg + jg - greater (true)"
     (check-equal?
      (run-jit-2arg
       (lambda (buf)
         (x64-cmp-reg buf RDI RSI)
         (x64-jcc-rel8 buf CC-G 7)
         (x64-mov-imm32 buf RAX 0)
         (x64-ret buf)
         (x64-mov-imm32 buf RAX 42)
         (x64-ret buf))
       10 5)
      42))

   (test-case "cmp_reg + jl - less (true)"
     (check-equal?
      (run-jit-2arg
       (lambda (buf)
         (x64-cmp-reg buf RDI RSI)
         (x64-jcc-rel8 buf CC-L 7)
         (x64-mov-imm32 buf RAX 0)
         (x64-ret buf)
         (x64-mov-imm32 buf RAX 42)
         (x64-ret buf))
       3 5)
      42))

   (test-case "cmp_imm8 + jge - greater or equal"
     (check-equal?
      (run-jit-1arg
       (lambda (buf)
         (x64-cmp-imm8 buf RDI 10)
         (x64-jcc-rel8 buf CC-GE 7)
         (x64-mov-imm32 buf RAX 0)
         (x64-ret buf)
         (x64-mov-imm32 buf RAX 42)
         (x64-ret buf))
       10)
      42))

   (test-case "cmp_imm8 + jle - less or equal"
     (check-equal?
      (run-jit-1arg
       (lambda (buf)
         (x64-cmp-imm8 buf RDI 10)
         (x64-jcc-rel8 buf CC-LE 7)
         (x64-mov-imm32 buf RAX 0)
         (x64-ret buf)
         (x64-mov-imm32 buf RAX 42)
         (x64-ret buf))
       5)
      42))))

;; ============================================
;; Jump instruction tests
;; ============================================

(define jmp-tests
  (test-suite
   "JMP instruction tests"

   (test-case "jmp_rel8 - forward jump"
     (check-equal?
      (run-jit-void
       (lambda (buf)
         (x64-jmp-rel8 buf 7)           ; skip next instruction
         (x64-mov-imm32 buf RAX 999)    ; skipped
         (x64-ret buf)                  ; skipped
         (x64-mov-imm32 buf RAX 42)     ; target
         (x64-ret buf)))
      42))

   (test-case "jmp_rel8 - backward jump (loop sum 1..10)"
     (check-equal?
      (run-jit-void
       (lambda (buf)
         ;; RAX = 0 (sum), RCX = 10 (counter)
         (x64-xor-reg buf RAX RAX)
         (x64-mov-imm32 buf RCX 10)
         ;; loop: RAX += RCX
         (x64-add-reg buf RAX RCX)
         ;; RCX--
         (x64-dec buf RCX)
         ;; if RCX > 0, goto loop
         (x64-test-reg buf RCX RCX)
         (x64-jcc-rel8 buf CC-NZ -9)
         ;; return RAX
         (x64-ret buf)))
      55))))

;; ============================================
;; NEG instruction tests
;; ============================================

(define neg-tests
  (test-suite
   "NEG instruction tests"

   (test-case "neg - negate positive"
     (check-equal?
      (run-jit-1arg
       (lambda (buf)
         (x64-mov-reg buf RAX RDI)
         (x64-neg buf RAX)
         (x64-ret buf))
       42)
      -42))

   (test-case "neg - negate negative"
     (check-equal?
      (run-jit-1arg
       (lambda (buf)
         (x64-mov-reg buf RAX RDI)
         (x64-neg buf RAX)
         (x64-ret buf))
       -42)
      42))))

;; ============================================
;; Logical instruction tests
;; ============================================

(define logical-tests
  (test-suite
   "Logical instruction tests"

   (test-case "and_reg - bitwise AND"
     (check-equal?
      (run-jit-2arg
       (lambda (buf)
         (x64-mov-reg buf RAX RDI)
         (x64-and-reg buf RAX RSI)
         (x64-ret buf))
       #xFF00FF00 #xFFFF0000)
      #xFF000000))

   (test-case "or_reg - bitwise OR"
     (check-equal?
      (run-jit-2arg
       (lambda (buf)
         (x64-mov-reg buf RAX RDI)
         (x64-or-reg buf RAX RSI)
         (x64-ret buf))
       #xFF00FF00 #x00FF00FF)
      #xFFFFFFFF))

   (test-case "xor_reg - bitwise XOR"
     (check-equal?
      (run-jit-2arg
       (lambda (buf)
         (x64-mov-reg buf RAX RDI)
         (x64-xor-reg buf RAX RSI)
         (x64-ret buf))
       #xFF00FF00 #xFFFFFFFF)
      #x00FF00FF))

   (test-case "xor_reg - zero register"
     (check-equal?
      (run-jit-void
       (lambda (buf)
         (x64-mov-imm32 buf RAX 42)
         (x64-xor-reg buf RAX RAX)  ; RAX = 0
         (x64-ret buf)))
      0))))

;; ============================================
;; INC/DEC instruction tests
;; ============================================

(define inc-dec-tests
  (test-suite
   "INC and DEC instruction tests"

   (test-case "inc - increment"
     (check-equal?
      (run-jit-1arg
       (lambda (buf)
         (x64-mov-reg buf RAX RDI)
         (x64-inc buf RAX)
         (x64-ret buf))
       41)
      42))

   (test-case "dec - decrement"
     (check-equal?
      (run-jit-1arg
       (lambda (buf)
         (x64-mov-reg buf RAX RDI)
         (x64-dec buf RAX)
         (x64-ret buf))
       43)
      42))))

;; ============================================
;; NOP instruction tests
;; ============================================

(define nop-tests
  (test-suite
   "NOP instruction tests"

   (test-case "nop - no operation"
     (check-equal?
      (run-jit-void
       (lambda (buf)
         (x64-nop buf)
         (x64-nop buf)
         (x64-nop buf)
         (x64-mov-imm32 buf RAX 42)
         (x64-ret buf)))
      42))))

;; ============================================
;; Integration tests using jit-functions
;; ============================================

(define integration-tests
  (test-suite
   "Integration tests"

   (test-case "factorial"
     (check-equal? (jit-factorial 0) 1)
     (check-equal? (jit-factorial 1) 1)
     (check-equal? (jit-factorial 5) 120)
     (check-equal? (jit-factorial 10) 3628800))

   (test-case "fibonacci"
     (check-equal? (jit-fibonacci 0) 0)
     (check-equal? (jit-fibonacci 1) 1)
     (check-equal? (jit-fibonacci 2) 1)
     (check-equal? (jit-fibonacci 3) 2)
     (check-equal? (jit-fibonacci 4) 3)
     (check-equal? (jit-fibonacci 5) 5)
     (check-equal? (jit-fibonacci 6) 8)
     (check-equal? (jit-fibonacci 7) 13)
     (check-equal? (jit-fibonacci 8) 21)
     (check-equal? (jit-fibonacci 9) 34)
     (check-equal? (jit-fibonacci 10) 55)
     (check-equal? (jit-fibonacci 20) 6765))

   (test-case "sum"
     (check-equal? (jit-sum 0) 0)
     (check-equal? (jit-sum 1) 1)
     (check-equal? (jit-sum 10) 55)
     (check-equal? (jit-sum 100) 5050))

   (test-case "gcd"
     (check-equal? (jit-gcd 48 18) 6)
     (check-equal? (jit-gcd 17 13) 1)
     (check-equal? (jit-gcd 100 25) 25))

   (test-case "add"
     (check-equal? (jit-add 10 32) 42)
     (check-equal? (jit-add -10 52) 42))

   (test-case "mul"
     (check-equal? (jit-mul 6 7) 42)
     (check-equal? (jit-mul -6 -7) 42))))

;; ============================================
;; Error handling tests
;; ============================================

(define error-handling-tests
  (test-suite
   "Error handling tests"

   (test-case "make-jit-function raises error on NULL buffer"
     (check-exn
      exn:fail?
      (lambda () (make-jit-function #f (_fun -> _int64)))))))

;; ============================================
;; Run all tests
;; ============================================

(define all-tests
  (test-suite
   "x64 Dynasm Comprehensive Tests"
   mov-imm-tests
   mov-reg-tests
   add-tests
   sub-tests
   imul-tests
   idiv-tests
   shift-tests
   cmp-jcc-tests
   jmp-tests
   neg-tests
   logical-tests
   inc-dec-tests
   nop-tests
   integration-tests
   error-handling-tests))

(module+ main
  (run-tests all-tests 'verbose))

(module+ test
  (require rackunit/text-ui)
  (run-tests all-tests))
