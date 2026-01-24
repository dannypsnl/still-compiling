#lang racket/base

;;; Example: JIT compile factorial with dynasm and run it in Unicorn emulator
;;; Run from the dynasm directory: racket unicorn-example.rkt

(require ffi/unsafe
         racket/format
         "../dynasm.rkt"
         "../unicorn.rkt")

(displayln "=== Dynasm + Unicorn Example: Factorial ===\n")

;; Memory layout for Unicorn
(define CODE-ADDRESS #x10000)
(define CODE-SIZE #x1000)

;; Generate factorial code using dynasm
;; Input: X0 = n
;; Output: X0 = n!
(define (generate-factorial-code)
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
    (emit! buf (aarch64-add-imm X0 X1 0))    ; return result (mov x0, x1)
    (emit! buf (aarch64-ret))                ; ret (won't execute in unicorn, we stop before)

    buf))

;; Extract code bytes from dynasm buffer
(define (dynasm-get-code-bytes buf)
  (define code-ptr (dynasm-finalize buf))
  (unless code-ptr
    (error 'dynasm-get-code-bytes "finalize failed"))
  (define size (dynasm-pos buf))
  (define code-bytes (make-bytes size))
  (memcpy code-bytes code-ptr size)
  code-bytes)

;; Run factorial in Unicorn and return result
(define (run-factorial-in-unicorn code-bytes n)
  ;; Create ARM64 emulator
  (define uc (uc-create-arm64))

  ;; Map memory for code
  (uc-map-memory uc CODE-ADDRESS CODE-SIZE)

  ;; Write code to memory
  (uc-write-code uc CODE-ADDRESS code-bytes)

  ;; Set X0 = n (input argument)
  (uc-reg-write-u64 uc UC_ARM64_REG_X0 n)

  ;; Calculate end address (before the RET instruction)
  ;; 8 instructions * 4 bytes = 32 bytes, RET is at offset 28
  (define end-address (+ CODE-ADDRESS 28))

  ;; Emulate from start to just before RET
  (uc-emulate uc CODE-ADDRESS end-address)

  ;; Read result from X0
  (define result (uc-reg-read-u64 uc UC_ARM64_REG_X0))

  ;; Clean up
  (uc-close uc)

  result)

;; Main
(displayln "Generating factorial code with dynasm...")
(define buf (generate-factorial-code))
(define code-bytes (dynasm-get-code-bytes buf))
(printf "Generated ~a bytes of ARM64 code\n\n" (bytes-length code-bytes))

;; Show the generated code (hex dump)
(displayln "Generated machine code:")
(for ([i (in-range 0 (bytes-length code-bytes) 4)])
  (define inst (integer-bytes->integer (subbytes code-bytes i (+ i 4)) #f #f))
  (printf "  ~a: ~a\n"
          (~a #:width 4 #:align 'right (format "+~a" i))
          (~a #:width 8 #:pad-string "0" #:align 'right (format "~x" inst))))
(newline)

;; Test factorial with various inputs
(displayln "Running in Unicorn emulator:")
(for ([n '(0 1 5 10 12)])
  (define result (run-factorial-in-unicorn code-bytes n))
  (printf "  ~a! = ~a\n" n result))

(newline)
(dynasm-free buf)
(displayln "=== Done ===")
