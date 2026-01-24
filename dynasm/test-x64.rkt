#lang racket/base

;;; Basic tests for x64 dynasm instructions
;;; Run: racket test-x64.rkt

(require rackunit
         rackunit/text-ui
         ffi/unsafe
         "dynasm.rkt")

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
      42))))

;; ============================================
;; Run all tests
;; ============================================

(module+ main
  (run-tests basic-tests 'verbose))

(module+ test
  (require rackunit/text-ui)
  (run-tests basic-tests))
