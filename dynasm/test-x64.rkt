#lang racket/base

;;; Basic tests for x64 dynasm instructions
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

;; ============================================
;; Basic tests
;; ============================================

(define basic-tests
  (test-suite
   "Basic x64 instruction tests"

   (test-case "mov_imm32 and ret"
     (check-equal?
      (run-jit-void
       (lambda (buf)
         (emit-x64! buf (x64-mov-imm32 RAX 42))
         (emit-x64! buf (x64-ret))))
      42))

   (test-case "mov_imm64 and ret"
     (check-equal?
      (run-jit-void
       (lambda (buf)
         (emit-x64! buf (x64-mov-imm64 RAX #x123456789ABCDEF0))
         (emit-x64! buf (x64-ret))))
      #x123456789ABCDEF0))

   (test-case "mov_reg - copy argument"
     (check-equal?
      (run-jit-1arg
       (lambda (buf)
         (emit-x64! buf (x64-mov-reg RAX RDI))
         (emit-x64! buf (x64-ret)))
       42)
      42))

   (test-case "add_reg"
     (check-equal?
      (run-jit-2arg
       (lambda (buf)
         (emit-x64! buf (x64-mov-reg RAX RDI))
         (emit-x64! buf (x64-add-reg RAX RSI))
         (emit-x64! buf (x64-ret)))
       10 32)
      42))

   (test-case "sub_reg"
     (check-equal?
      (run-jit-2arg
       (lambda (buf)
         (emit-x64! buf (x64-mov-reg RAX RDI))
         (emit-x64! buf (x64-sub-reg RAX RSI))
         (emit-x64! buf (x64-ret)))
       100 58)
      42))

   (test-case "imul_reg"
     (check-equal?
      (run-jit-2arg
       (lambda (buf)
         (emit-x64! buf (x64-mov-reg RAX RDI))
         (emit-x64! buf (x64-imul-reg RAX RSI))
         (emit-x64! buf (x64-ret)))
       6 7)
      42))

   (test-case "xor_reg - zero register"
     (check-equal?
      (run-jit-void
       (lambda (buf)
         (emit-x64! buf (x64-mov-imm32 RAX 999))
         (emit-x64! buf (x64-xor-reg RAX RAX))
         (emit-x64! buf (x64-ret))))
      0))

   (test-case "inc"
     (check-equal?
      (run-jit-1arg
       (lambda (buf)
         (emit-x64! buf (x64-mov-reg RAX RDI))
         (emit-x64! buf (x64-inc RAX))
         (emit-x64! buf (x64-ret)))
       41)
      42))

   (test-case "dec"
     (check-equal?
      (run-jit-1arg
       (lambda (buf)
         (emit-x64! buf (x64-mov-reg RAX RDI))
         (emit-x64! buf (x64-dec RAX))
         (emit-x64! buf (x64-ret)))
       43)
      42))

   (test-case "neg"
     (check-equal?
      (run-jit-1arg
       (lambda (buf)
         (emit-x64! buf (x64-mov-reg RAX RDI))
         (emit-x64! buf (x64-neg RAX))
         (emit-x64! buf (x64-ret)))
       -42)
      42))

   (test-case "add_imm32"
     (check-equal?
      (run-jit-1arg
       (lambda (buf)
         (emit-x64! buf (x64-mov-reg RAX RDI))
         (emit-x64! buf (x64-add-imm32 RAX 100))
         (emit-x64! buf (x64-ret)))
       42)
      142))

   (test-case "add_imm8"
     (check-equal?
      (run-jit-1arg
       (lambda (buf)
         (emit-x64! buf (x64-mov-reg RAX RDI))
         (emit-x64! buf (x64-add-imm8 RAX 10))
         (emit-x64! buf (x64-ret)))
       32)
      42))

   (test-case "sub_imm32"
     (check-equal?
      (run-jit-1arg
       (lambda (buf)
         (emit-x64! buf (x64-mov-reg RAX RDI))
         (emit-x64! buf (x64-sub-imm32 RAX 58))
         (emit-x64! buf (x64-ret)))
       100)
      42))

   (test-case "sub_imm8"
     (check-equal?
      (run-jit-1arg
       (lambda (buf)
         (emit-x64! buf (x64-mov-reg RAX RDI))
         (emit-x64! buf (x64-sub-imm8 RAX 8))
         (emit-x64! buf (x64-ret)))
       50)
      42))

   (test-case "imul_imm32"
     (check-equal?
      (run-jit-1arg
       (lambda (buf)
         (emit-x64! buf (x64-imul-imm32 RAX RDI 7))
         (emit-x64! buf (x64-ret)))
       6)
      42))

   (test-case "and_reg"
     (check-equal?
      (run-jit-2arg
       (lambda (buf)
         (emit-x64! buf (x64-mov-reg RAX RDI))
         (emit-x64! buf (x64-and-reg RAX RSI))
         (emit-x64! buf (x64-ret)))
       #xFF #x2A)
      #x2A))

   (test-case "and_imm32"
     (check-equal?
      (run-jit-1arg
       (lambda (buf)
         (emit-x64! buf (x64-mov-reg RAX RDI))
         (emit-x64! buf (x64-and-imm32 RAX #xFF))
         (emit-x64! buf (x64-ret)))
       #x12A)
      #x2A))

   (test-case "or_reg"
     (check-equal?
      (run-jit-2arg
       (lambda (buf)
         (emit-x64! buf (x64-mov-reg RAX RDI))
         (emit-x64! buf (x64-or-reg RAX RSI))
         (emit-x64! buf (x64-ret)))
       #x20 #x0A)
      #x2A))

   (test-case "shl_imm - shift left by 1"
     (check-equal?
      (run-jit-1arg
       (lambda (buf)
         (emit-x64! buf (x64-mov-reg RAX RDI))
         (emit-x64! buf (x64-shl-imm RAX 1))
         (emit-x64! buf (x64-ret)))
       21)
      42))

   (test-case "shr_imm - shift right by 1"
     (check-equal?
      (run-jit-1arg
       (lambda (buf)
         (emit-x64! buf (x64-mov-reg RAX RDI))
         (emit-x64! buf (x64-shr-imm RAX 1))
         (emit-x64! buf (x64-ret)))
       84)
      42))

   (test-case "sar_imm - arithmetic shift right by 2"
     (check-equal?
      (run-jit-1arg
       (lambda (buf)
         (emit-x64! buf (x64-mov-reg RAX RDI))
         (emit-x64! buf (x64-sar-imm RAX 2))
         (emit-x64! buf (x64-ret)))
       168)
      42))

   (test-case "shl_cl - shift left by CL"
     (check-equal?
      (run-jit-2arg
       (lambda (buf)
         (emit-x64! buf (x64-mov-reg RAX RDI))
         (emit-x64! buf (x64-mov-reg RCX RSI))
         (emit-x64! buf (x64-shl-cl RAX))
         (emit-x64! buf (x64-ret)))
       21 1)
      42))

   (test-case "shr_cl - shift right by CL"
     (check-equal?
      (run-jit-2arg
       (lambda (buf)
         (emit-x64! buf (x64-mov-reg RAX RDI))
         (emit-x64! buf (x64-mov-reg RCX RSI))
         (emit-x64! buf (x64-shr-cl RAX))
         (emit-x64! buf (x64-ret)))
       84 1)
      42))

   (test-case "cmp_reg and mov"
     (check-equal?
      (run-jit-2arg
       (lambda (buf)
         (emit-x64! buf (x64-cmp-reg RDI RSI))
         (emit-x64! buf (x64-mov-imm32 RAX 42))
         (emit-x64! buf (x64-ret)))
       10 20)
      42))

   (test-case "cmp_imm32"
     (check-equal?
      (run-jit-1arg
       (lambda (buf)
         (emit-x64! buf (x64-cmp-imm32 RDI 100))
         (emit-x64! buf (x64-mov-imm32 RAX 42))
         (emit-x64! buf (x64-ret)))
       50)
      42))

   (test-case "test_reg"
     (check-equal?
      (run-jit-1arg
       (lambda (buf)
         (emit-x64! buf (x64-test-reg RDI RDI))
         (emit-x64! buf (x64-mov-imm32 RAX 42))
         (emit-x64! buf (x64-ret)))
       #xFF)
      42))

   (test-case "push and pop"
     (check-equal?
      (run-jit-1arg
       (lambda (buf)
         (emit-x64! buf (x64-mov-reg RAX RDI))
         (emit-x64! buf (x64-push RAX))
         (emit-x64! buf (x64-mov-imm32 RAX 0))
         (emit-x64! buf (x64-pop RAX))
         (emit-x64! buf (x64-ret)))
       42)
      42))

   (test-case "nop - does nothing"
     (check-equal?
      (run-jit-1arg
       (lambda (buf)
         (emit-x64! buf (x64-mov-reg RAX RDI))
         (emit-x64! buf (x64-nop))
         (emit-x64! buf (x64-nop))
         (emit-x64! buf (x64-ret)))
       42)
      42))

   (test-case "idiv_reg - division"
     (check-equal?
      (run-jit-2arg
       (lambda (buf)
         (emit-x64! buf (x64-mov-reg RAX RDI))  ; RAX = dividend
         (emit-x64! buf (x64-cqo))              ; sign-extend RAX to RDX:RAX
         (emit-x64! buf (x64-idiv-reg RSI))     ; RAX = RAX / RSI
         (emit-x64! buf (x64-ret)))
       84 2)
      42))

   (test-case "idiv_reg - remainder in RDX"
     (check-equal?
      (run-jit-2arg
       (lambda (buf)
         (emit-x64! buf (x64-mov-reg RAX RDI))  ; RAX = dividend
         (emit-x64! buf (x64-cqo))              ; sign-extend RAX to RDX:RAX
         (emit-x64! buf (x64-idiv-reg RSI))     ; RAX = quotient, RDX = remainder
         (emit-x64! buf (x64-mov-reg RAX RDX))  ; return remainder
         (emit-x64! buf (x64-ret)))
       45 3)
      0))

   (test-case "mov_mr and mov_rm - store and load"
     (check-equal?
      (run-jit-1arg
       (lambda (buf)
         (emit-x64! buf (x64-sub-imm8 RSP 8))   ; allocate 8 bytes on stack
         (emit-x64! buf (x64-mov-mr RSP RDI))   ; store arg to [RSP]
         (emit-x64! buf (x64-mov-rm RAX RSP))   ; load [RSP] to RAX
         (emit-x64! buf (x64-add-imm8 RSP 8))   ; deallocate stack
         (emit-x64! buf (x64-ret)))
       42)
      42))

   (test-case "mov_mr_disp32 and mov_rm_disp32 - store and load with displacement"
     (check-equal?
      (run-jit-2arg
       (lambda (buf)
         (emit-x64! buf (x64-sub-imm8 RSP 16))         ; allocate 16 bytes
         (emit-x64! buf (x64-mov-mr-disp32 RSP 8 RDI)) ; store arg1 at [RSP+8]
         (emit-x64! buf (x64-mov-mr RSP RSI))          ; store arg2 at [RSP]
         (emit-x64! buf (x64-mov-rm-disp32 RAX RSP 8)) ; load [RSP+8] to RAX
         (emit-x64! buf (x64-add-imm8 RSP 16))         ; deallocate stack
         (emit-x64! buf (x64-ret)))
       42 100)
      42))

   (test-case "jmp_rel8 - unconditional jump"
     (check-equal?
      (run-jit-void
       (lambda (buf)
         (emit-x64! buf (x64-jmp-rel8 7))       ; jump over the next mov instruction
         (emit-x64! buf (x64-mov-imm32 RAX 99)) ; this should be skipped (7 bytes)
         (emit-x64! buf (x64-mov-imm32 RAX 42)) ; land here
         (emit-x64! buf (x64-ret))))
      42))

   (test-case "jcc_rel8 - conditional jump (je/CC-E)"
     (check-equal?
      (run-jit-2arg
       (lambda (buf)
         (emit-x64! buf (x64-cmp-reg RDI RSI))  ; compare args
         (emit-x64! buf (x64-jcc-rel8 CC-E 8))  ; if equal, jump over next mov+ret (7+1=8 bytes)
         (emit-x64! buf (x64-mov-imm32 RAX 99)) ; set RAX=99 if not equal (7 bytes)
         (emit-x64! buf (x64-ret))              ; 1 byte
         (emit-x64! buf (x64-mov-imm32 RAX 42)) ; set RAX=42 if equal
         (emit-x64! buf (x64-ret)))
       10 10)
      42))

   (test-case "jcc_rel8 - conditional jump not taken"
     (check-equal?
      (run-jit-2arg
       (lambda (buf)
         (emit-x64! buf (x64-cmp-reg RDI RSI))  ; compare args
         (emit-x64! buf (x64-jcc-rel8 CC-E 8))  ; if equal, jump over next mov+ret
         (emit-x64! buf (x64-mov-imm32 RAX 42)) ; set RAX=42 if not equal
         (emit-x64! buf (x64-ret))
         (emit-x64! buf (x64-mov-imm32 RAX 99)) ; set RAX=99 if equal
         (emit-x64! buf (x64-ret)))
       10 20)
      42))

   (test-case "jcc_rel8 - less than (CC-L)"
     (check-equal?
      (run-jit-2arg
       (lambda (buf)
         (emit-x64! buf (x64-cmp-reg RDI RSI))  ; compare args
         (emit-x64! buf (x64-jcc-rel8 CC-L 8))  ; if arg1 < arg2, jump
         (emit-x64! buf (x64-mov-imm32 RAX 99)) ; set RAX=99 if not less
         (emit-x64! buf (x64-ret))
         (emit-x64! buf (x64-mov-imm32 RAX 42)) ; set RAX=42 if less
         (emit-x64! buf (x64-ret)))
       10 20)
      42))

   (test-case "jcc_rel8 - greater or equal (CC-GE)"
     (check-equal?
      (run-jit-2arg
       (lambda (buf)
         (emit-x64! buf (x64-cmp-reg RDI RSI))  ; compare args
         (emit-x64! buf (x64-jcc-rel8 CC-GE 8)) ; if arg1 >= arg2, jump
         (emit-x64! buf (x64-mov-imm32 RAX 99)) ; set RAX=99 if less
         (emit-x64! buf (x64-ret))
         (emit-x64! buf (x64-mov-imm32 RAX 42)) ; set RAX=42 if >=
         (emit-x64! buf (x64-ret)))
       50 20)
      42))

   (test-case "sar_cl - arithmetic shift right by CL"
     (check-equal?
      (run-jit-2arg
       (lambda (buf)
         (emit-x64! buf (x64-mov-reg RAX RDI))
         (emit-x64! buf (x64-mov-reg RCX RSI))
         (emit-x64! buf (x64-sar-cl RAX))
         (emit-x64! buf (x64-ret)))
       168 2)
      42))

   (test-case "cmp_imm8"
     (check-equal?
      (run-jit-1arg
       (lambda (buf)
         (emit-x64! buf (x64-cmp-imm8 RDI 50))
         (emit-x64! buf (x64-mov-imm32 RAX 42))
         (emit-x64! buf (x64-ret)))
       10)
      42))

   (test-case "jmp_rel32 - longer jump"
     (check-equal?
      (run-jit-void
       (lambda (buf)
         (emit-x64! buf (x64-jmp-rel32 7))      ; jump over the next mov instruction
         (emit-x64! buf (x64-mov-imm32 RAX 99)) ; this should be skipped (7 bytes)
         (emit-x64! buf (x64-mov-imm32 RAX 42)) ; land here
         (emit-x64! buf (x64-ret))))
      42))

   (test-case "jcc_rel32 - longer conditional jump (CC-NE)"
     (check-equal?
      (run-jit-2arg
       (lambda (buf)
         (emit-x64! buf (x64-cmp-reg RDI RSI))   ; compare args
         (emit-x64! buf (x64-jcc-rel32 CC-NE 8)) ; if not equal, jump over next mov+ret
         (emit-x64! buf (x64-mov-imm32 RAX 99))  ; set RAX=99 if equal
         (emit-x64! buf (x64-ret))
         (emit-x64! buf (x64-mov-imm32 RAX 42))  ; set RAX=42 if not equal
         (emit-x64! buf (x64-ret)))
       10 20)
      42))))

(define fac-tests
  (test-suite "factorial tests"
    (test-case "fac(1)" (check-equal? (jit-factorial 1) 1))
    (test-case "fac(2)" (check-equal? (jit-factorial 2) 2))
    (test-case "fac(3)" (check-equal? (jit-factorial 3) 6))
    (test-case "fac(4)" (check-equal? (jit-factorial 4) 24))
    (test-case "fac(5)" (check-equal? (jit-factorial 5) 120))
    (test-case "fac(6)" (check-equal? (jit-factorial 6) 720))
    ))

(define all-tests
  (test-suite "All tests"
    basic-tests
    fac-tests))

;; ============================================
;; Run all tests
;; ============================================
(module+ main
  (run-tests all-tests 'verbose))

(module+ test
  (require rackunit/text-ui)
  (run-tests all-tests))
