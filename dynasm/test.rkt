#lang racket/base

(require rackunit
         rackunit/text-ui
         ffi/unsafe
         "dynasm.rkt"
         "unicorn.rkt")

(displayln "Starting test...")

(define CODE-ADDRESS #x10000)
(define RETURN-ADDRESS #x20000)

(define (get-code-bytes buf)
  (define code-ptr (dynasm-finalize buf))
  (define size (dynasm-pos buf))
  (define code-bytes (make-bytes size))
  (memcpy code-bytes code-ptr size)
  code-bytes)

(define (setup-uc code-bytes)
  (define uc (uc-create-arm64))
  (uc-map-memory uc CODE-ADDRESS #x1000)
  (uc-map-memory uc RETURN-ADDRESS #x1000)
  (uc-write-code uc CODE-ADDRESS code-bytes)
  (uc-reg-write-u64 uc UC_ARM64_REG_X30 RETURN-ADDRESS)
  uc)

(define (run-uc-void thunk)
  (let ([buf (dynasm-create 4096)])
    (thunk buf)
    (define code-bytes (get-code-bytes buf))
    (define uc (setup-uc code-bytes))
    (uc-emulate uc CODE-ADDRESS RETURN-ADDRESS)
    (define result (uc-reg-read-u64 uc UC_ARM64_REG_X0))
    (uc-close uc)
    (dynasm-free buf)
    result))

(define (run-uc-1arg thunk arg0)
  (let ([buf (dynasm-create 4096)])
    (thunk buf)
    (define code-bytes (get-code-bytes buf))
    (define uc (setup-uc code-bytes))
    (uc-reg-write-u64 uc UC_ARM64_REG_X0 arg0)
    (uc-emulate uc CODE-ADDRESS RETURN-ADDRESS)
    (define result (uc-reg-read-u64 uc UC_ARM64_REG_X0))
    (uc-close uc)
    (dynasm-free buf)
    result))

(define (run-uc-2arg thunk arg0 arg1)
  (let ([buf (dynasm-create 4096)])
    (thunk buf)
    (define code-bytes (get-code-bytes buf))

    (define uc (setup-uc code-bytes))
    (uc-reg-write-u64 uc UC_ARM64_REG_X0 arg0)
    (uc-reg-write-u64 uc UC_ARM64_REG_X1 arg1)

    (uc-emulate uc CODE-ADDRESS RETURN-ADDRESS)

    (define result (uc-reg-read-u64 uc UC_ARM64_REG_X0))

    (uc-close uc)
    (dynasm-free buf)
    result))

(define tests
  (test-suite
   "Tests"

   (test-case "movz - basic"
     (displayln "  movz basic...")
     (check-equal?
      (run-uc-void
       (lambda (buf)
         (emit! buf (aarch64-movz X0 42 0))
         (emit! buf (aarch64-ret))))
      42))

   (test-case "movz - zero value"
     (displayln "  movz zero...")
     (check-equal?
      (run-uc-void
       (lambda (buf)
         (emit! buf (aarch64-movz X0 0 0))
         (emit! buf (aarch64-ret))))
      0))

   (test-case "movz - max 16-bit"
     (displayln "  movz max...")
     (check-equal?
      (run-uc-void
       (lambda (buf)
         (emit! buf (aarch64-movz X0 #xFFFF 0))
         (emit! buf (aarch64-ret))))
      #xFFFF))

   (test-case "movz - shift 16"
     (displayln "  movz shift 16...")
     (check-equal?
      (run-uc-void
       (lambda (buf)
         (emit! buf (aarch64-movz X0 1 1))
         (emit! buf (aarch64-ret))))
      #x10000))

   (test-case "movz - shift 32"
     (displayln "  movz shift 32...")
     (check-equal?
      (run-uc-void
       (lambda (buf)
         (emit! buf (aarch64-movz X0 1 2))
         (emit! buf (aarch64-ret))))
      #x100000000))

   (test-case "movz - shift 48"
     (displayln "  movz shift 48...")
     (check-equal?
      (run-uc-void
       (lambda (buf)
         (emit! buf (aarch64-movz X0 1 3))
         (emit! buf (aarch64-ret))))
      #x1000000000000))

   (test-case "movk - build 32-bit"
     (displayln "  movk 32-bit...")
     (check-equal?
      (run-uc-void
       (lambda (buf)
         (emit! buf (aarch64-movz X0 #x5678 0))
         (emit! buf (aarch64-movk X0 #x1234 1))
         (emit! buf (aarch64-ret))))
      #x12345678))

   (test-case "movk - build 64-bit"
     (displayln "  movk 64-bit...")
     (check-equal?
      (run-uc-void
       (lambda (buf)
         (emit! buf (aarch64-movz X0 #xBABE 0))
         (emit! buf (aarch64-movk X0 #xCAFE 1))
         (emit! buf (aarch64-movk X0 #xBEEF 2))
         (emit! buf (aarch64-movk X0 #xDEAD 3))
         (emit! buf (aarch64-ret))))
      #xDEADBEEFCAFEBABE))

   (test-case "add_imm - basic"
     (displayln "  add_imm...")
     (check-equal?
      (run-uc-void
       (lambda (buf)
         (emit! buf (aarch64-movz X0 10 0))
         (emit! buf (aarch64-add-imm X0 X0 32))
         (emit! buf (aarch64-ret))))
      42))

   (test-case "sub_imm - basic"
     (displayln "  sub_imm...")
     (check-equal?
      (run-uc-void
       (lambda (buf)
         (emit! buf (aarch64-movz X0 100 0))
         (emit! buf (aarch64-sub-imm X0 X0 58))
         (emit! buf (aarch64-ret))))
      42))

   (test-case "add_reg - basic"
     (displayln "  add_reg...")
     (check-equal?
      (run-uc-2arg
       (lambda (buf)
         (emit! buf (aarch64-add-reg X0 X0 X1))
         (emit! buf (aarch64-ret)))
       10 32)
      42))

   (test-case "sub_reg - basic"
     (displayln "  sub_reg...")
     (check-equal?
      (run-uc-2arg
       (lambda (buf)
         (emit! buf (aarch64-sub-reg X0 X0 X1))
         (emit! buf (aarch64-ret)))
       100 58)
      42))

   (test-case "mul - basic"
     (displayln "  mul...")
     (check-equal?
      (run-uc-2arg
       (lambda (buf)
         (emit! buf (aarch64-mul X0 X0 X1))
         (emit! buf (aarch64-ret)))
       6 7)
      42))

   (test-case "sdiv - basic"
     (displayln "  sdiv...")
     (check-equal?
      (run-uc-2arg
       (lambda (buf)
         (emit! buf (aarch64-sdiv X0 X0 X1))
         (emit! buf (aarch64-ret)))
       42 6)
      7))

   (test-case "sum 1 to 10"
     (displayln "  sum loop...")
     (check-equal?
      (run-uc-void
       (lambda (buf)
         (emit! buf (aarch64-movz X0 0 0))
         (emit! buf (aarch64-movz X1 10 0))
         (emit! buf (aarch64-add-reg X0 X0 X1))
         (emit! buf (aarch64-sub-imm X1 X1 1))
         (emit! buf (aarch64-cmp-imm X1 0))
         (emit! buf (aarch64-b-cond COND-GT -3))
         (emit! buf (aarch64-ret))))
      55))

   (test-case "factorial 5"
     (displayln "  factorial...")
     (check-equal?
      (run-uc-1arg
       (lambda (buf)
         (emit! buf (aarch64-movz X1 1 0))
         (emit! buf (aarch64-cmp-imm X0 1))
         (emit! buf (aarch64-b-cond COND-LE 4))
         (emit! buf (aarch64-mul X1 X1 X0))
         (emit! buf (aarch64-sub-imm X0 X0 1))
         (emit! buf (aarch64-b -4))
         (emit! buf (aarch64-add-imm X0 X1 0))
         (emit! buf (aarch64-ret)))
       5)
      120))))

(displayln "Running tests...")
(run-tests tests 'verbose)
(displayln "Done!")
